library(tidyr)
library(rgdal)
library(sp)
library(raster)
library(plyr)
library(dplyr)
library(doParallel)
library(parallel)


getCountryAndBiomeLevelPAs <- function(country, gediwk=18){
  ecoregion_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/wwf_ecoregions_key.csv")
  
  biome_list <- unique(ecoregion_list$BIOME_NAME)
  source("/gpfs/data1/duncansongp/sruthikp/GEDI_PA/getBiomeLevelPAsPerCountryFunction.R")
  registerDoParallel(10)
  cat("Parallel processing",getDoParWorkers(),"PAs \n")
  foreach(i=1:length(biome_list), .packages=c('tidyr', 'raster', 'plyr', 'dplyr', 'sp','rgdal','doParallel','bpgmm')) %dopar% {
    biome_id <- ecoregion_list %>% filter(BIOME_NAME == biome_list[i])
    biome_id <- unique(biome_id$BIOME)
    getBiomeLevelPAsPerCountry(biome_list[i],biome_id, country)
  }
  
}

cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv")

#country_list <- unique(cont_country_list$iso3)

country_list <- c("ALB","BGD","BHS", "BLR", "BRA", "CHL", "DOM", "ESP", "GTM", "IND", "ITA", "JPN", "KWT", "MEX", "MKD", "NZL", "PAN", "PER", "PRI", "ROU", "USA_west", "VEN", "WSM")

#country_list <- c("ALB")
mcmapply(FUN = getCountryAndBiomeLevelPAs,country_list, mc.cores = 10)



