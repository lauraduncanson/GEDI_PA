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

iso3_mean_pa_vs_ctrl_loss_cols <- c("Continent", "iso3","wwfbiome", "p_val", "if_diff_significant")

for(i in 1:length(country_list)){
  curr_conti_list <- cont_country_list %>% filter(iso3 == country_list[i])
  curr_conti <- unique(curr_conti_list$continent)
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_pa_vs_ctrl_abs_area_loss.csv", sep=""))
  for (j in 1:14){
    
    area_diff_df_biome <- area_diff_df %>% filter(wwfbiome == j)
    if(dim(area_diff_df_biome)[1] == 0){
      print(paste("Biome ",j," does not exist in the country"))
      next
    }
    
    pa_area_loss <- area_diff_df_biome %>% filter(status == 1)
    ctrl_area_loss <- area_diff_df_biome %>% filter(status == 0)
    
    print(paste("PA : ", dim(pa_area_loss)[1], " and control : ", dim(ctrl_area_loss)[1]))
    res <- wilcox.test(pa_area_loss$area_loss, ctrl_area_loss$area_loss)
    
    if(is.na(res$p.value)) {
      significant = "no"
    }
    else if(res$p.value <= 0.05){
      significant = "yes"
    } else {
      significant = "no"
    }
    
    
    curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(curr_conti),paste(country_list[i]),j,res$p.value,significant)
    colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
    iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
  }
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_biome_level_loss_significance.csv", sep=""))
