library(raster)
library(sp)
library(sf)
library(rgdal)
library(doParallel)

dir.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/"
write.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved/"

all_shp_files <- list.files(dir.path,".shp")
shpfiles_to_process <- vector()
for(i in 1:length(all_shp_files)){
    if (file.exists(paste(write.path,all_shp_files[i],sep=""))){
       #print(paste("file ", write.path,all_shp_files[i] ," exists",sep=""))
    } else{
        shpfiles_to_process <- c(shpfiles_to_process, all_shp_files[i])
    }

  }
print(length(shpfiles_to_process))
registerDoParallel(20)  #parallelize across 15 tiles at a time
#startTime <- Sys.time()
foreach(this_shpfile=seq(length(shpfiles_to_process)), .combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
  #i<-1
  ##add the GEDI l4a model prediction for AGB here :
  cat("Readng in no. ", this_shpfile, " shp file of ", length(shpfiles_to_process),"\n")
  
  shp_file <- readOGR(paste(dir.path,shpfiles_to_process[this_shpfile],sep=""))
  shp_file <- aggregate(shp_file, dissolve = TRUE)
  shp_file <- SpatialPolygonsDataFrame(shp_file, data = data.frame(dt = c("Status"), row.names = c(1)))
  
  writeOGR(shp_file,paste(write.path,shpfiles_to_process[this_shpfile],sep=""),layer=shpfiles_to_process[this_shpfile], driver = "ESRI Shapefile")
  return(NULL)
}
stopImplicitCluster()
