library(rgdal)
library(sp)
library(raster)
library(plyr)
library(dplyr)
library(parallel)

writeToIso3Folders <- function(country, gediwk=18){
  
  print(paste("Processing ",country,sep =""))

  iso3 <- country
  country_biome_result_path <- paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved/",sep="")
  matched_pa <- list.files(country_biome_result_path, pattern = paste(iso3,"_",sep=""))  #matched_PAs
  
  
  if(length(matched_pa) > 0){
    for(i in 1:length(matched_pa)){
      dest_folder <- paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved/", iso3, sep="")
      dir.create(dest_folder, showWarnings = FALSE)
      file.copy(paste(country_biome_result_path, matched_pa[i],sep=""), dest_folder)
    }
  }
}

cont_country_list <- read.csv("/gpfs/data1/duncansongp/sruthikp/GEDI_PA/all_count_conti_pair.csv")

country_list <- unique(cont_country_list$iso3)

mcmapply(FUN = writeToIso3Folders, country_list, mc.cores = 6)

