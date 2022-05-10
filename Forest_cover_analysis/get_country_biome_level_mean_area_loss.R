library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"

loss.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/", sep="")

cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair1.csv")

country_list <- cont_country_list$iso3

iso3_mean_pa_vs_ctrl_loss <- data.frame()

iso3_mean_pa_vs_ctrl_loss_cols <- c("Continent", "iso3","wwfbiome", "mean_area_loss_diff", "sd_area_loss_diff","var_area_loss_diff")

for(i in 1:length(country_list)){
  
  curr_conti_list <- cont_country_list %>% filter(iso3 == country_list[i])
  curr_conti <- unique(curr_conti_list$continent)
  #print(paste("Processing ",  country_list[i], " in ", curr_conti,sep=""))
  
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_pa_vs_ctrl_area_diff.csv", sep=""))
  for (j in 1:14){
    #print(paste("Processing the biome",j, " in country ",  country_list[i], " in continent ", curr_conti,sep=""))
    area_diff_df_biome <- area_diff_df %>% filter(wwfbiome == j)
    #print(area_diff_df_biome)
    if(dim(area_diff_df_biome)[1] == 0){
      print(paste("Biome ",j," does not exist in ",country_list[i]))
      next
    }
    print(paste("Processing the biome",j, " in country ",  country_list[i], " in continent ", curr_conti,sep=""))
    mean_area_diff <- mean(area_diff_df_biome$area_loss_diff)
    sd_area_diff <- sd(area_diff_df_biome$area_loss_diff)
    var_area_diff <- var(area_diff_df_biome$area_loss_diff)
  
    curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(curr_conti),paste(country_list[i]),j,mean_area_diff,sd_area_diff,var_area_diff)
    colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
    iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
  }
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_biome_level_mean_area_loss.csv",sep=""))
