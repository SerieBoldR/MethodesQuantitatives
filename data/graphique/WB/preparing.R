file_urb <- "C:/Users/gelbj/OneDrive/Bureau/TEMP/WB_URB_POP.csv"
file_co2 <- "C:/Users/gelbj/OneDrive/Bureau/TEMP/WB_CO2.csv"
file_pop <- "C:/Users/gelbj/OneDrive/Bureau/TEMP/WB_pop.csv"

library(data.table)

df_urb <- fread(file_urb, sep = ',', stringsAsFactors = F, header = T, integer64 = 'numeric')
df_co2 <- fread(file_co2, sep = ',', stringsAsFactors = F, header = T, integer64 = 'numeric')
df_pop <- fread(file_pop, sep = ',', stringsAsFactors = F, header = T, integer64 = 'numeric')


df_urb1 <- df_urb[,c(2,5:64)]
df_co21 <- df_co2[,c(2,5:64)]
df_pop1 <- df_pop[,c(2,5:64)]


library(reshape2)

df_pop_melt <- melt(df_pop1,id.vars = "Country Code")
df_co2_melt <- melt(df_co21,id.vars = "Country Code")
df_urb_melt <- melt(df_urb1,id.vars = "Country Code")


names(df_pop_melt) <- c("County","Year","Population")
names(df_co2_melt) <- c("County","Year","CO2_kt")
names(df_urb_melt) <- c("County","Year","Urbanisation")

df_pop_melt$join_code <- paste(df_pop_melt$County,df_pop_melt$Year, sep = "_")
df_co2_melt$join_code <- paste(df_co2_melt$County,df_co2_melt$Year, sep = "_")
df_urb_melt$join_code <- paste(df_urb_melt$County,df_urb_melt$Year, sep = "_")


comb1 <- merge(df_pop_melt, df_urb_melt, by = "join_code", all.x = TRUE, all.y = TRUE)
comb2 <- merge(comb1, df_co2_melt, by = "join_code", all.x = TRUE, all.y = TRUE)

tot_df <- comb2[,c(1,4,7,10)]

library(tidyr)

tot_df <- separate(tot_df, col ="join_code", sep = "_", into = c("country_code", "year"))

tot_df2 <- merge(tot_df,df_pop[,c("Country Name","Country Code")], by.x = "country_code", by.y = "Country Code")

tot_df2$CO2t_hab <- (tot_df2$CO2_kt / tot_df2$Population) * 1000

ggplot(data = tot_df2) +
  geom_point(aes(y = CO2t_hab, x =  Urbanisation))+
  ylim(0,110)


tot_df2$region7 <- countrycode(sourcevar = tot_df2$country_code,
                                 origin = 'wb',
                                 destination = 'region')

tot_df2$region23 <- countrycode(sourcevar = tot_df2$country_code,
                              origin = 'wb',
                              destination = 'region23')

final_df <- subset(tot_df2, is.na(tot_df2$region7) == F)

setwd("E:/Projets/Philippe_apparicio/Livre Statistique/VersionGIT/livre_statistique_Phil_Jere/data/graphique")

write.csv(tot_df2,file = 'world_urb_co2.csv', row.names = F)

