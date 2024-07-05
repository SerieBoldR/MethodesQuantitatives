corr_0 <- FALSE
actual_angle <- 1
df_lines <- data.frame(
  x = c(-3,3),
  y = c(-3,3)
)

while (corr_0 == FALSE){

  ## calculer la rotation des donnees
  print(paste0("angle is : ", actual_angle))
  data2 <- data.frame(Rotation(data1[,c(1,2)], deg2rad(-1*actual_angle)))
  line2 <- data.frame(Rotation(df_lines, deg2rad(-1*actual_angle)))
  names(data2) <- c("V1","V2")
  names(line2) <- c("V1","V2")
  data2$angle <- actual_angle
  line2$angle <- actual_angle
  actual_cor <- cor(data2$V1, data2$V2)
  actual_angle <<- actual_angle + 1

  string <- paste0("Corrélation = ", tofr(round(actual_cor, 2)),
                   ", variance axe 1 = ", tofr(round(var(data2$V1), 2)),
                   ", variance axe 2 = ", tofr(round(var(data2$V2), 2))
  )
  ## créer le graphique
  this_plot <- ggplot(data2, aes(x=V1,y=V2))+
    xlim(-3,3)+
    ylim(-3,3)+
    geom_point(size = 1, color="steelblue")+
    #geom_hline(yintercept=0, size = .2, color="black")+
    #geom_vline(xintercept=0, size = .2, color="black") +
    geom_line(data = line2, mapping = aes(x = V1, y = V2), color = "black") +
    xlab("Axe 1")+ylab("Axe 2")+
    stat_ellipse( size = 1, color="black")+
    geom_smooth(method = lm, color = "red",  se=FALSE)+
    coord_fixed() +
    ggtitle("Rotation des axes",
            subtitle = )

  ggsave(paste0("images/analysesfactorielles/animation1/angle",actual_angle,".png"),
         plot = this_plot,
         device = "png", dpi = 300, width = 10, height = 10, units = "cm")

  if(actual_cor < 0.05){
    corr_0 <<- TRUE
  }
  if(actual_angle >= 180){
    corr_0 <<- TRUE
  }
}

## créer une animation
library(magick)

imgs <- list.files("images/analysesfactorielles/animation1", full.names = TRUE)
val <- sapply(imgs, function(x){
  as.numeric(gsub(".png","",strsplit(x, "angle")[[1]][[2]], fixed = TRUE))
})
img_list <- lapply(imgs[order(val)], image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 4)
image_write(image = img_animated,
            path = "images/analysesfactorielles/animation.gif")

```
