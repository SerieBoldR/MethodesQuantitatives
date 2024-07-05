
require(dplyr)

split_group_effect <- function(x, which = 1) {
  init = strsplit(x, split = ",")
  purrr::map_chr(init, function(x) x[min(which, length(x))])
}

extract_random_effects.gam <- function(
  model,
  re = NULL,
  # component,
  ci_level = .95,
  digits = 3,
  add_group_N = FALSE,
  ...
) {
  # get the re variables and their levels
  re_terms <- purrr::map_lgl(model$smooth,
                             function(x)
                               inherits(x, "random.effect"))

  re_smooths <- model$smooth[re_terms]

  re_names <- purrr::map_chr(model$smooth[re_terms],
                             function(x)
                               ifelse(length(x$vn) == 1,
                                      x$vn,
                                      x$vn[length(x$vn)]))

  re_levels <- vector("list", length(re_names))


  # add check on re name/type
  # check that re is factor as re smooth can be applied to continuous
  for (i in seq_along(re_names)) {
    if (!inherits(model$model[, re_names[i]], "factor")) {
      warning(
        paste0(re_names[i], ' is not a factor. No results provided for it.')
      )
      re_levels[[i]] <- NULL
    }
    else {
      re_levels[[i]] <- levels(model$model[, re_names[i]])
    }
  }

  if (purrr::is_empty(re_levels) | all(purrr::map_lgl(re_levels, is.null))) {
    stop('No factor random effects.')
  }

  # can put an re smooth on a continuous covariate for penalization, but don't
  # want that in output
  non_factors <- purrr::map_lgl(re_levels, is.null)
  re_idx  <- which(!non_factors)

  # this test is covered but covr ignores for some reason
  if (any(non_factors)) {
    re_terms[non_factors] <- FALSE
    re_names  <- re_names[re_idx]
  }

  if (!is.null(re) && !re %in% re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(re_names, collapse = ' ')
      )
    )

  re_labels <- purrr::map(re_smooths[re_idx], function(x) x$label)

  gam_coef <- stats::coef(model)

  # issue, parenthesis in the names means problematic regex matching, so remove
  # all but key part of pattern
  re_label_base <- gsub(re_labels, pattern = "s\\(", replacement = '') # remove first s
  re_label_base <- gsub(re_label_base, pattern = "\\(|\\)", replacement = '') # remove parenthesis

  re_coef  <- vector('list', length = length(re_idx))
  coef_idx <- vector('list', length = length(re_idx))

  for (i in re_idx) {
    first_para <- re_smooths[[i]]$first.para
    last_para  <- re_smooths[[i]]$last.para

    coef_idx[[i]] <- first_para:last_para
    re_coef[[i]]  <- gam_coef[coef_idx[[i]]]
  }

  # extract coefs and se
  re0 <- unlist(re_coef)
  gam_se <- sqrt(diag(model$Vp))[unlist(coef_idx)]

  # clean up and gather names
  names(re0) <- gsub(names(re0), pattern = "s\\(|\\)", replacement = '')
  names(re0) <- gsub(names(re0), pattern = "\\.[0-9]+", replacement = '')

  re_names   <- names(re0)
  re_effects <- purrr::map_chr(re_smooths, function(x) x$term[1])
  re_effects <- rep(re_effects, times = purrr::map_int(re_coef, length))

  # check to see if factors are the smooth terms (i.e. random cat slope), and
  # repeat levels of grouping variable the number of levels in the factor
  for (i in re_idx) {
    smooth_vars <-  re_smooths[[i]]$term
    smooth_term <- model$model[[smooth_vars[1]]] # it will be the first term, 2nd term is the RE var

    if (length(smooth_vars) > 1 & (is.factor(smooth_term) | is.character(smooth_term))) {
      n_levs <- dplyr::n_distinct(smooth_term)
      re_levels[[i]] <- rep(re_levels[[i]], each = n_levs) # note the each, this is how mgcv orders and confirmed via lme4
    }
  }

  random_effects <- dplyr::tibble(group_var = re_names) %>%
    dplyr::mutate(
      group_var = split_group_effect(group_var, which = 2),
      effect = re_effects,
      effect = ifelse(effect ==  group_var, 'Intercept', effect),
      group  = unlist(re_levels),
      value  = re0,
      se     = gam_se
    )

  if (add_group_N) {
    grp_vars <- unique(random_effects$group_var)

    ns <- count_grps(model, grp_vars)

    # suppress warning of char vs. factor
    random_effects <- suppressWarnings({
      dplyr::left_join(random_effects, ns, by = c("group_var", "group"))
    })
  }

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects <- random_effects %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  if (add_group_N) {
    random_effects <- random_effects %>% dplyr::select(-n, dplyr::everything())
  }

  random_effects
}
