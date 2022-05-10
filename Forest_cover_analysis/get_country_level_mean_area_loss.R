library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"

loss.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/", sep="")

cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair1.csv")

country_list <- cont_country_list$iso3

#country_list <- c("BRA","COD")

iso3_mean_pa_vs_ctrl_loss <- data.frame()

iso3_mean_pa_vs_ctrl_loss_cols <- c("iso3", "mean_area_loss_diff", "sd_area_loss_diff","var_area_loss_diff")

for(i in 1:length(country_list)){
  print(country_list[i])
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_for_pa_vs_ctrl_area_diff.csv", sep=""))
  mean_area_diff <- mean(area_diff_df$area_loss_diff)
  sd_area_diff <- sd(area_diff_df$area_loss_diff)
  var_area_diff <- var(area_diff_df$area_loss_diff)
  
  curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(country_list[i],sep=""),mean_area_diff,sd_area_diff,var_area_diff)
  colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
  iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_level_mean_area_loss_forest.csv",sep=""))
