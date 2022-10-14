# # library(plotly)
# # library(sf)
# # library(tidyverse)
# # library(haven)
# # 
# # Reading shp file containing geometry for canada provinces----
# data <- st_read("lpr_000b21a_e.shp")%>%
#   select(c("PRENAME","geometry"))
# #View(data)
#
# #Reading the csv file containing our data----
# rate_69 <- read.csv("Book2.csv")%>%
#   select(c("PRENAME","VALUE", "Indicators", "Visible.minority.status"))
#
# View(rate_69)
# new_rate<- rate_69%>% filter(!is.na(rate_69$VALUE))
# View(new_rate)
# #
# # #Merging the files using PRENAME as the unique variable in both files----
# # provinces <- merge(rate_69,data,by = "PRENAME")
# # #View(provinces)
# # #names(provinces)
# #
# # #Removing the empty cells in the VALUE variable----
# # provinces_1 <- provinces %>% filter(!is.na(provinces$VALUE))%>%
# #   select(c("PRENAME","VALUE","geometry"))
# # #View(provinces_1)
#
# #plot(st_geometry(provinces))
# fig <- ggplotly(
#   data %>%
#   left_join(.,new_rate, by = "PRENAME") %>%
#   ggplot() +
#   geom_sf(aes(fill = PRENAME)) +
#   ggtitle("Labour Force Status by Groups Designated as Visible Minorities")
# )
# fig
# # #Plotting the map-----
# # fig <- ggplotly(
# #   ggplot(provinces_1) +
# #     geom_sf(mapping = aes(fill= VALUE, geometry = geometry)) +
# #     ggtitle("Labour Force Status by Groups Designated as Visible Minorities")
# # )
# # # 
# # # fig
# # 
# # # rate_69<- rename(rate_69,c(VALUE="PERCENT"))
# # # View(rate_69)
# 
# library(maps)
# library(mapproj)
# source("helpers(1)(1).R")
# provinces <- st_read("lpr_000b21a_e.shp")
# percent_map(provinces$white, "darkgreen", "% White")
# 
# 
# 
# 
# 
# 
# 
# 
