library(doParallel)
library(raster)
library(rgdal)
#library(gfcanalysis)
library(tidyr)
library(plyr)
library(dplyr)
library(rgeos)
library(sp)
library(parallel)
library(exactextractr)

getmode <- function(v,na.rm=TRUE) {
  uniqv <- unique(v)
  if(na.rm==TRUE){
    uniqv <- unique(na.omit(v))
  }
  return(uniqv[which.max((tabulate(match(v,uniqv))))])
}  #function to get mode 

getLossAreaPerCtrl <- function(iso3) {
  
  gediwk <- 18
  f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
  hansen.loss.f.path <- "/gpfs/data1/duncansongp/leitoldv/gfc_data/"
  rds.f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/Country_level_RDS/"
  
  write_f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/"
  
  ###read in the control file for every match and prepare it as a spdf
  match_results <- readRDS( paste(rds.f.path,iso3,"_Ctrl.RDS",sep="")) 
  if(is.null(match_results)){
    next
  }
  
  if(dim(match_results)[1] == 0){
    next
  }
  
  #unique_match_results <- match_results %>% group_by(lat, lon) %>% filter(row_number() == 1)
  
  unique_match_results <- match_results
  unique_match_results$wwfbiomnum <- factor(unique_match_results$wwfbiom, levels=c("Tropical & Subtropical Moist Broadleaf Forests",
                                                                                   "Tropical & Subtropical Dry Broadleaf Forests",            
                                                                                   "Tropical & Subtropical Coniferous Forests",
                                                                                   "Temperate Broadleaf & Mixed Forests",                     
                                                                                   "Temperate Conifer Forests",
                                                                                   "Boreal Forests/Taiga",                                 
                                                                                   "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                                                                   "Temperate Grasslands, Savannas & Shrublands",
                                                                                   "Flooded Grasslands & Savannas",
                                                                                   "Montane Grasslands & Shrublands",
                                                                                   "Tundra",
                                                                                   "Mediterranean Forests, Woodlands & Scrub",  
                                                                                   "Deserts & Xeric Shrublands",
                                                                                   "Mangroves"))
  
  
  unique_match_results$wwfbiomnum <- as.numeric(unique_match_results$wwfbiomnum)
  
  
  match_results_with_tileinfo <- unique_match_results %>% mutate(tileLat=round_any(lat, 10, f = ceiling), tileLon=round_any(lon, 10,f=floor)) %>%   #get the tile name that each cell is in
    mutate(tileLat2 =ifelse(tileLat>=0,"N","S"), tileLon2=ifelse(tileLon>=0,"E","W")) %>% 
    mutate(tileLon=abs(tileLon)) %>% 
    mutate(tileLat=abs(tileLat)) %>% 
    mutate(tileLat=ifelse(nchar(tileLat)<2, paste("0",tileLat,sep=""),tileLat)) %>% 
    mutate(tileLon=ifelse(nchar(tileLon)<3, paste("0",tileLon,sep=""),tileLon)) %>% 
    mutate(tileLon=ifelse(nchar(tileLon)<3, paste("0",tileLon,sep=""),tileLon)) %>% 
    mutate(tile=paste("Hansen_GFC-2020-v1.8_lossmask_", tileLat, tileLat2, "_",tileLon, tileLon2,".tif",sep=""))
  
  match_results_with_tileinfo_SP <- match_results_with_tileinfo %>% SpatialPointsDataFrame(data=., coords=.[,c("lon","lat")],
                                                                                           proj4string=CRS("+init=epsg:4326")) %>% 
    spTransform(., "+init=epsg:6933")
  
  world_region <- raster(paste(f.path,"GEDI_ANCI_PFT_r1000m_EASE2.0_UMD_v1_projection_defined_6933.tif",sep=""))  #1km raster template
  projection(world_region) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
  
  tiles_processed <- unique(match_results_with_tileinfo_SP$tile)
  tiles_to_process <- vector()
  print(tiles_processed)
  
  #Double-check to prevent the processing of already processed tiles
  for(i in 1:length(tiles_processed)){
    if (file.exists(paste(write_f.path, iso3,"/",iso3,"_Ctrl_with_dup_", tiles_processed[i],".rds",sep=""))){
       print(paste("file ", write_f.path, iso3,"/",iso3,"_Ctrl_with_dup_", tiles_processed[i],".rds", " exists",sep=""))
    } else{
      tiles_to_process <- c(tiles_to_process, tiles_processed[i])
    }
    
  }
  print(tiles_to_process)
  registerDoParallel(10)  #parallelize across 10 tiles at a time
  startTime <- Sys.time()
  foreach(this_r=unique(tiles_to_process),.combine = rbind, .packages=c('raster','sp','magrittr', 'dplyr','tidyr','optmatch','doParallel')) %dopar% {
    cat(this_r,match(this_r, unique(tiles_to_process)),"-- out of",length(unique(tiles_to_process)),"\n")
    
    contiAGB <- data.frame()
    sampleSub <- match_results_with_tileinfo_SP[match_results_with_tileinfo_SP$tile==this_r,] #getting the SPD with all the cells within a single forest cover loss tile
    cat("rasterizing...\n")
    world_region_sub <- crop(world_region,extent(buffer(sampleSub,10000))) # cropping the 1 km raster to the extent of the SPD to process within this tile
    
    ### Rasterizing the SPD using the cropped 1 km raster template and adding the status, biome and id of the matched PA as raster fields
    rasOutST <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$status), world_region_sub) 
    rasOutBIOM <- rasterize(sampleSub@coords, field=sampleSub$wwfbiomnum, world_region_sub,fun=getmode)
    rasOutPAID <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$pa_id), world_region_sub,fun=getmode)
    rasOut <- stack(rasOutST,rasOutPAID, rasOutBIOM)#rasOutST
    
    ### Converting the raster to gridpolygon
    gridPolygon <- rasterToPolygons(rasOut)
    names(gridPolygon) <-c("status","pa_id","wwfbiom") #names(sampleTile)[4:18]
    
    gridPolygonExPrj <- gridPolygon %>% spTransform(paste("+init=epsg:",4326,sep="")) %>% extent() # getting the spatial extent of gridpolygon
    
    hansan_loss <- raster(paste(hansen.loss.f.path,this_r, sep="")) # Reading the forest cover loss tile from the source folder to process
    
    
    cat("Projetcing...\n")
    ### Cropping the tile to the spatial extent of the PA/match gridpolygon and then reprojecting the cropped raster to 6933 projection
    hansan_loss_prj <- hansan_loss %>%crop(gridPolygonExPrj)%>% projectRaster(.,crs="+init=epsg:6933 +proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ", method="ngb")  
    cat("aggregating...\n")
    ### Converting the resolution of 30 cm forest cover loss raster tile to 1 km raster using the aggregate method  
   
    ini_res <- res(hansan_loss_prj)
    
    fact_res <- res(rasOut)
    fact_res_x <- fact_res[1]/ini_res[1]
    fact_res_y <- fact_res[2]/ini_res[2]
    
    hansan_loss_prj2 <- tryCatch(raster::aggregate(hansan_loss_prj,c(fact_res_x, fact_res_y),fun=mean), error=function(cond){return(NULL)})
    cat("resampling...\n")
    
    ### Resampling the 1 km aggregated forest cover loss tile to match the resolution of the PA/match 1 km raster 
    hansan_loss_prj3 <- tryCatch(raster::resample(hansan_loss_prj2,rasOut,method='ngb'), error=function(cond){return(NULL)})
    cat("extracting...\n")
    
    ### Extracting the corresponding forest cover loss values for the PA/matched raster from the resampled forest cover loss tile
    sampleloss_df <- exact_extract(hansan_loss_prj3, gridPolygon)
    sampleloss_df_bound <- bind_rows(sampleloss_df, .id = "coverage_fraction")  
    sampleloss_end_df <- sampleloss_df_bound  %>% group_by(coverage_fraction) %>% filter(row_number() == max(row_number()))
    sampleloss <- sampleloss_end_df$value
    cat("extracting done...\n")
    if(!is.null(sampleloss)){
      gridPolygon$loss <- unlist(sampleloss)
      gridPolygon$area <- area(gridPolygon)
      gridPolygonDF <- data.frame(gridPolygon)
      contiAGB <- rbind(contiAGB, gridPolygonDF)
      dir.create(file.path(write_f.path, iso3), showWarnings = FALSE)
      saveRDS(gridPolygonDF,paste(write_f.path, iso3,"/",iso3,"_Ctrl_with_dup_", this_r,".rds",sep=""))

      cat("done\n")
      
    } else{
      cat("Null in sample AGB")
    }
    
    return(NULL)
  }
  tElapsed <- Sys.time()-startTime
  # cat(tElapsed, "for matching all PAs in", iso3,"\n")
  stopImplicitCluster()
  
}


#Parallely processing the control region forest cover loss for multiple countries at a time
cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") #Reading the list of countries to process 

country_list <- cont_country_list$iso3

mcmapply(FUN = getLossAreaPerCtrl,country_list, mc.cores = 10) #mc.cores = 10 -> processing 10 countries at a time, which can be adapted based on the system used to process
