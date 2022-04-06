library(rgdal)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(rgeos)
library(maptools)
library(ggmap)
library(tidyr)
library(dplyr)

setwd("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/")

world_spdf <- readOGR(paste0(getwd(),"/","world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp",sep="") , 
  verbose=FALSE
)

emptymap <- st_read(paste0(getwd(),"/","world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp",sep=""))

mean_area_loss_df <- read.csv("Country_level_mean_area_loss_forest.csv")
mean_area_loss_df <- mean_area_loss_df %>% mutate(ISO3 = iso3)
mean_area_loss_df$ISO3 <- as.factor(mean_area_loss_df$ISO3)

data_merged <- left_join(emptymap,mean_area_loss_df, by= "ISO3")


ggplot(data_merged)+
  geom_sf(aes(fill = mean_area_loss_diff)) + #aes(fill = cut(mean_area_loss_diff, zCuts))) +
  coord_sf(datum = NA) + 
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="blue", space ="Lab",aesthetics = "fill",na.value = "grey50", name = "Control - PA loss (sq. km.)") +
  labs(
    title = "PA effectiveness in reducing forest cover loss",
    subtitle = "") 


