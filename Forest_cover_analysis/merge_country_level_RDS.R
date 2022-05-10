library(raster)
library(rgdal)
#library(gfcanalysis)
library(tidyr)
library(dplyr)
library(rgeos)
library(sp)
library(parallel)

mergeCountryLevelRDS <- function(iso3)
{
gediwk <- 18
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
wkpath <- paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/",iso3,"_wk", gediwk,sep="")
write_f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/Country_level_RDS/"

print(paste("Processing ",iso3,sep=""))
#if (file.exists(paste(write_f.path, iso3,"_PA.RDS",sep=""))){
#   print(paste("file for ",iso3, " exists",sep=""))
#} else{
    
#gediwk <- 18
#f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
#wkpath <- paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/",iso3,"_wk", gediwk,sep="")
#write_f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/Country_level_RDS/"
all_pa <- data.frame()
all_ctrl <- data.frame()

matched_pa <- list.files(wkpath,".RDS")  #matched_PAs

ecoreg_key <- read.csv(paste(f.path,"wwf_ecoregions_key.csv",sep=""))
#print(matched_pa)

if(length(matched_pa) > 0){
  
  
  for(i in 1:length(matched_pa)){
    #i=1
    print(matched_pa[i])
    ###read in the matched file and prepare it as a spdf
    match_results <- readRDS(paste(wkpath,matched_pa[i],sep="/"))  #testing with the the first pa file from iso3
    if(is.null(match_results)){
      next
    }
    
    if(dim(match_results)[1] == 0){
      next
    }
    
    
    
    #unique_match_results <- match_results %>% group_by(lat, lon) %>% filter(row_number() == 1)
    unique_match_results <- match_results 
    unique_match_results_pa <- unique_match_results %>% filter(status == TRUE)
    unique_match_results_ctrl <- unique_match_results %>% filter(status == FALSE)
    print(dim(all_pa))
    
    if(dim(all_pa)[1] == 0){
	all_pa <- unique_match_results_pa
    }
    if(dim(all_ctrl)[1] == 0){
       all_ctrl <- unique_match_results_ctrl
    }

 
    all_pa <- rbind(all_pa, unique_match_results_pa)
    all_ctrl <- rbind(all_ctrl, unique_match_results_ctrl)
  }
}

saveRDS(all_pa, file= paste(write_f.path,iso3,"_PA.RDS", sep=""))
saveRDS(all_ctrl, file= paste(write_f.path,iso3,"_Ctrl.RDS", sep=""))
#}
}

country_list <- c("NIC", "ZWE")
#cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv")

#country_list <- cont_country_list$iso3
#print(dim(country_list))
mcmapply(FUN = mergeCountryLevelRDS,country_list, mc.cores = 5)
