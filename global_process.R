#!/usr/bin/env Rscript

# This global processing script is derived from the global processing notebook 
#the input can be the iso3 code (3-character) for one or multiple countries 

options(warn=-1)
options(dplyr.summarise.inform = FALSE)

packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","spatialEco","bit64")
package.check <- lapply(packages, FUN = function(x) {
    suppressPackageStartupMessages(library(x, character.only = TRUE))
})


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)>=1) {
  
  iso3 <- args[1]  #country to process
  gediwk <- args[2]   #the # of weeks GEDI data to use
  mproc <- as.integer(args[3])#the number of cores to use for macthing 
}

cat("Step 0: Loading global variables to process country", iso3,"with GEDI data until week", gediwk, "\n")

f.path <- "/gpfs/data1/duncansongp/leitoldv/"
exclude <- c("mean_gHM","pop_cnt_2000")
tifs <- list.files(paste(f.path,"WDPA_input_vars_iso3/",iso3,"/",sep=""),full.names=T) %>% .[str_detect(., exclude, negate = TRUE)]
tifs.name <- str_match(tifs, "//\\s*(.*?)\\s*.tif")[,2]
ecoreg_key <- read.csv(paste(f.path,"wwf_ecoregions_key.csv",sep=""))
allPAs <- readRDS(paste(f.path,"WDPA_shapefiles/WDPA_polygons/",iso3,"_PA_poly.rds",sep=""))
MCD12Q1 <- raster(paste(f.path,"GEDI_ANCI_PFT_r1000m_EASE2.0_UMD_v1_projection_defined_6933.tif",sep=""))
projection(MCD12Q1) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
world_region <- raster(paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_6933.tif",sep=""))
projection(world_region) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
adm <- readOGR(paste(f.path,"WDPA_countries/shp/",iso3,".shp",sep=""),verbose=F)
adm_prj <- spTransform(adm, "+init=epsg:6933") 
source("/gpfs/data1/duncansongp/amberliang/trends.Earth/git/GEDI_PA/matching_func.R")

# STEP1. Create 1km sampling grid with points only where GEDI data is available; first check if grid file exist to avoid reprocessing 
if(!file.exists(paste(f.path,"WDPA_grids/",iso3,"_grid_wk",gediwk,".RDS", sep=""))){
  cat("Step 1: Creating 1km sampling grid fiilter with GEDI data for", iso3,"\n")
  GRID.lats <- raster(file.path(f.path,"EASE2_M01km_lats.tif"))
  GRID.lons <- raster(file.path(f.path,"EASE2_M01km_lons.tif"))
  GRID.lats.adm   <- crop(GRID.lats, adm_prj)
  GRID.lats.adm.m <- raster::mask(GRID.lats.adm, adm_prj)
  GRID.lons.adm   <- crop(GRID.lons, adm_prj)
  GRID.lons.adm.m <- raster::mask(GRID.lons.adm, adm_prj)
  rm(GRID.lats, GRID.lons, GRID.lats.adm, GRID.lons.adm)
  
  #1.3) extract coordinates of raster cells with valid GEDI data in them
  gedi_folder <- paste(f.path,"WDPA_gedi_l2a+l2b_clean/",iso3,"/",sep="")
  
  GRID.coords <- data.frame()
  for(i in 1:length(dir(gedi_folder))){
    # print(list.files(gedi_folder)[i])
    gedi_data <- read.csv(list.files(gedi_folder,full.names=T)[i]) %>%
      dplyr::select(lon_lowestmode,lat_lowestmode)
    gedi_pts  <- SpatialPoints(coords=gedi_data[,c("lon_lowestmode","lat_lowestmode")],
                               proj4string=CRS("+init=epsg:4326"))
    gedi_pts_prj <- spTransform(gedi_pts, "+init=epsg:6933")
    
    GRID.lons.overlap <- GRID.lons.adm.m[gedi_pts_prj]
    GRID.lats.overlap <- GRID.lats.adm.m[gedi_pts_prj]
    
    x.overlap <- GRID.lons.overlap[!is.na(GRID.lons.overlap)]
    y.overlap <- GRID.lats.overlap[!is.na(GRID.lats.overlap)]
    
    xy.overlap <- cbind(x.overlap,y.overlap)
    xy.overlap.clean <- unique(xy.overlap)
    
    GRID.coords <- rbind(GRID.coords, xy.overlap.clean) 
    
  }
  GRID.for.matching <- SpatialPoints(coords = GRID.coords, proj4string=CRS("+init=epsg:4326"))
  saveRDS(GRID.for.matching, file = paste(f.path,"WDPA_grids/",iso3,"_grid_wk",gediwk,".RDS", sep=""))
} else if (file.exists(paste(f.path,"WDPA_grids/",iso3,"_grid_wk",gediwk,".RDS", sep=""))) {
  cat(paste("STEP 1: Grid file exists, no need to process grids for ",iso3, "\n"))
}
  
# STEP2. Clip sampling grid to nonPA areas within country & sample raster layers on nonPA grid
cat("Step 2.0: Reading 1k GRID from RDS for " ,iso3, "\n")
GRID.for.matching <- readRDS(paste(f.path,"WDPA_grids/",iso3,"_grid_wk",gediwk,".RDS", sep="")) 

if(!file.exists(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_prepped_control_wk",gediwk,".RDS",sep=""))){
  cat("Step 2.1: Preparing control dataset for", iso3, "\n")
  GRID.pts.nonPA <- GRID.for.matching
  for(i in 1:length(allPAs)){
    PA          <- allPAs[i,]
    PA_prj      <- spTransform(PA, "+init=epsg:6933")
    PA_prj_buff <- gBuffer(PA_prj, width = 10000) #10km buffer
    PA2         <- spTransform(PA_prj_buff, "+init=epsg:4326")
    overlap     <- GRID.pts.nonPA[PA2]
    if(length(overlap)>0){
      GRID.pts.nonPA <- erase.point(GRID.pts.nonPA, PA2, inside = T) ##remove pts inside poly
    } 
    # print(length(GRID.pts.nonPA))
  }
  nonPA_xy  <- coordinates(GRID.pts.nonPA)
  colnames(nonPA_xy)  <- c("x","y")
  nonPA_spdf  <- tryCatch(SpatialPointsDataFrame(nonPA_xy, data=data.frame(nonPA_xy),
                                        proj4string=CRS("+init=epsg:4326")),
                          error=function(cond){
                            cat("Country too samll, after buffer no grid left, so quit processing country", iso3, dim(nonPA_xy),"\n")
                            writeLines("Country too samll, after buffer no grid left", paste(f.path,"WDPA_log/",iso3,"_log_control.txt", sep=""))
                            return(quit(save="no"))})
    
  for (j in 1:length(tifs)){
    ras <- raster(tifs[j])
    ras_ex <- raster::extract(ras, nonPA_spdf@coords, method="simple", factors=F)
    nm <- names(ras)
    nonPA_spdf <- cbind(nonPA_spdf, ras_ex)
    names(nonPA_spdf)[j+2] <- tifs.name[j]
    
  }
  d_control <- nonPA_spdf
  d_control$status <- as.logical("FALSE")
  d_control <- data.frame(d_control) %>%
    dplyr::rename(
      land_cover = lc2000,
      slope = slope,
      elevation = dem,
      popden = pop_den_2000,
      mean_temp = annual_mean_temp,
      prec = annual_prec,
      wwfbiom = wwf_biomes,
      wwfecoreg = wwf_ecoreg,
      d2city = dcities,
      tt2city= tt2cities,
      d2road = d2roads,
      lon = x,
      lat = y)
  d_control$land_cover <- factor(d_control$land_cover, levels=sequence(7),
                                 labels = c("l1_forest",
                                            "l2_grassland",
                                            "l3_agriculture",
                                            "l4_wetlands",
                                            "l5_artificial",
                                            "l6_other land/bare",
                                            "l7_water"))
  d_control$wwfbiom <- factor(d_control$wwfbiom,
                           levels = as.vector(unique(ecoreg_key[,"BIOME"])),
                           labels = as.vector(unique(ecoreg_key[,"BIOME_NAME"])))
  d_control$wwfecoreg <- factor(d_control$wwfecoreg,
                             levels = as.vector(ecoreg_key[,"ECO_ID"]),
                             labels = as.vector(ecoreg_key[,"ECO_NAME"]))
  
  
  d_control$UID <-  seq.int(nrow(d_control))
  
  saveRDS(d_control, file=paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_prepped_control_wk",gediwk,".RDS",sep="")) 
  
} else if (file.exists(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_prepped_control_wk",gediwk,".RDS",sep=""))){
  cat("Step 2.1: preppred control dataset is already exist for", iso3, "no need for reprocessing\n")
}

#STEP3. Loop through all PAs in iso3 country:
# - clip sampling grid to each PA
# - sample raster layers on each PA grid
# - save each PA sample into prepped_pa_##.RDS file

cat("Step 3.0: Reading 1k GRID from RDS for " ,iso3, "\n")
GRID.for.matching <- readRDS(paste(f.path,"WDPA_grids/",iso3,"_grid_wk",gediwk,".RDS", sep="")) 

if(length(dir(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs","/",sep=""),pattern = paste(gediwk,".RDS",sep="")))==0){
  cat("Step 3.1: Processing prepped PA treatment dataset for ", iso3, "\n")
  for(i in 1:length(allPAs)){
    cat(iso3, i, "out of ", length(allPAs), "\n")
    testPA <- allPAs[i,]
    testPA <- spTransform(testPA, "+init=epsg:4326")
    GRID.pts.testPA <- GRID.for.matching[testPA]
    
    if(length(GRID.pts.testPA)>0){
      testPA_xy <- coordinates(GRID.pts.testPA)
      colnames(testPA_xy) <- c("x","y")
      testPA_spdf <- SpatialPointsDataFrame(testPA_xy, data=data.frame(testPA_xy),
                                            proj4string=CRS("+init=epsg:4326"))
      for (j in 1:length(tifs)){
        ras <- raster(tifs[j])
        ras <- crop(ras, testPA)
        #ras_prj <- projectRaster(ras, crs="+init=epsg:4326", method="ngb")
        ras_ex <- raster::extract(ras, testPA_spdf@coords, method="simple", factors=F)
        nm <- names(ras)
        
        testPA_spdf <- cbind(testPA_spdf, ras_ex)
        names(testPA_spdf)[j+2] <- tifs.name[j]
        
      }
      d_pa <- testPA_spdf
      d_pa$status <- as.logical("TRUE")
      d_pa$DESIG_ENG <- testPA$DESIG_ENG
      d_pa$REP_AREA <- testPA$REP_AREA
      d_pa$PA_STATUS <- testPA$STATUS
      d_pa$PA_STATUSYR <- testPA$STATUS_YR
      d_pa$GOV_TYPE <- testPA$GOV_TYPE
      d_pa$OWN_TYPE <- testPA$OWN_TYPE
      d_pa$MANG_AUTH <- testPA$MANG_AUTH
      d_pa <- data.frame(d_pa) %>%
        dplyr::rename(
          land_cover = lc2000,
          slope = slope,
          elevation = dem,
          popden = pop_den_2000,
          mean_temp = annual_mean_temp,
          prec = annual_prec,
          wwfbiom = wwf_biomes,
          wwfecoreg = wwf_ecoreg,
          d2city = dcities,
          tt2city= tt2cities,
          d2road = d2roads,
          lon = x,
          lat = y)
      d_pa$land_cover <- factor(d_pa$land_cover, levels=sequence(7),
                                labels = c("l1_forest",
                                           "l2_grassland",
                                           "l3_agriculture",
                                           "l4_wetlands",
                                           "l5_artificial",
                                           "l6_other land/bare",
                                           "l7_water"))
      d_pa$wwfbiom <- factor(d_pa$wwfbiom,
                          levels = as.vector(unique(ecoreg_key[,"BIOME"])),
                          labels = as.vector(unique(ecoreg_key[,"BIOME_NAME"])))
      d_pa$wwfecoreg <- factor(d_pa$wwfecoreg,
                            levels = as.vector(ecoreg_key[,"ECO_ID"]),
                            labels = as.vector(ecoreg_key[,"ECO_NAME"]))
      
      d_pa$UID <- seq.int(nrow(d_pa))
      saveRDS(d_pa, file = paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs","/","prepped_pa_",
                                 testPA$WDPAID,"_wk",gediwk,".RDS", sep="")) 
    }
  }
} else if (length(dir(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs","/",sep=""),pattern = paste(gediwk,".RDS",sep="")))>0){
  cat("Step 3.1: prepped PA treatment dataset is already exist for ", iso3, "no need for reprocessing\n")
}

#STEP4. Set up spatial points data frames (control + each PA) for point matching
# if (file.exists(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_matching_output_wk",gediwk,".RDS", sep=""))){
cat("Step 4: Performing matching for", iso3,"\n")
d_control_local <- readRDS(file=paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_prepped_control_wk",gediwk,".RDS",sep=""))
d_control_local <-d_control_local[complete.cases(d_control_local), ]  #filter away non-complete cases w/ NA in control set
d_PAs <- list.files(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs/", sep=""),pattern=gediwk,full.names=F)

registerDoParallel(mproc)
cat("using number of cores:",getDoParWorkers(),"\n")
startTime <- Sys.time()
results <-  foreach(this_pa=d_PAs,.combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','optmatch','doParallel')) %dopar% {
  pa <- this_pa
  id_pa <-pa %>%str_split("_") %>% unlist %>% .[3]
  # cat(id_pa, "in",iso3,"\n")
  cat("No.", match(pa,d_PAs),"of total",length(d_PAs),"PAs in ", iso3, "\n" )
  d_pa <- readRDS(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs/",pa, sep=""))
  cat(iso3, "pa no. ",id_pa, "dim of treatment is",dim(d_pa),"\n")
  d_filtered_prop <- tryCatch(propensity_filter(d_pa, d_control_local), error=function(e) return(NA))  #return a df of control and treatment after complete cases and propensity filters are applied 
  # print(dim(d_filtered_prop))
  d_wocat_all <- tryCatch(filter(d_filtered_prop, status),error=function(e) return(NA))
  d_control_all <- tryCatch(filter(d_filtered_prop, !status),error=function(e) return(NA))
  
  #sample the control dataset to the size of the sample dataset, keep unsampled ids to iterate until full number of matches found
  n_treatment <- dim(d_wocat_all)[1]
  n_control <- dim(d_control_all)[1]
  t <- ifelse(floor(n_control/n_treatment)<=7, ifelse(floor(n_control/n_treatment)<1, 1,floor(n_control/n_treatment)),7)
  # t=3
  n_sample <- round(n_treatment*t)    #now the n_control is 1.4 times the number of n_treatment, 7 will set the if ststament below to flase
  ids_all <- tryCatch(seq(1,n_control),error=function(e) return(NULL))
  m_all2_out <- data.frame()
  Bscore <- data.frame()
  n_matches <- 0
  
  tryCatch(
    while(n_matches < n_treatment){
      n_ids <- length(ids_all)
      if(n_ids >= n_sample){
        sample_ids <- sample(ids_all, n_sample)
        cat(iso3, "current sampling",n_sample,"of",n_control,"controls to match\n")
        d_control_sample <- d_control_all[sample_ids,]
        ids_all <- ids_all[-sample_ids]
        new_d <- tryCatch(rbind(d_wocat_all,d_control_sample),error=function(e) return(NULL))
        
        #outside of the match_wocat function check the balance
        f <- status ~ mean_temp + prec + elevation + slope+ d2road + d2city + popden + tt2city
        #do a glm here to throw out definitely not control data based on low propensity score (they weren't going to be matching anyways)
        
        m_all <- tryCatch(match_wocat(new_d),error=function(e) return(NULL))
        m_all2 <- tryCatch(m_all%>%unique(),error=function(e) return(NULL))
        n_matches_temp <- tryCatch(nrow(m_all2),error=function(e) return(NULL))
        cat(iso3, "Matched\n")
        # n_matches_temp <- nrow(m_all2$df)
        if(!is.null(n_matches_temp)){
          n_matches <- n_matches + nrow(m_all2)
          m_all2$pa_id <- rep(id_pa,n_matches_temp)
          # m_all2_out <- rbind(m_all2,m_all2_out)
          
        } else {
          n_treatment <- 0  #if not macthes ae found in this sampling 
        }
      } else {n_treatment <- n_matches}
    }, error=function(e) return(NA))
  
  reps <- tryCatch(nrow(m_all2), error=function(e) return(NULL))
  
  Bscore <-tryCatch(m_all2 %>% cbind(id=rep(id_pa,ifelse(is.null(reps),0, reps)),
                                     DESIG_ENG=rep(unique(d_pa$DESIG_ENG),ifelse(is.null(reps),0, reps)),
                                     REP_AREA=rep(unique(d_pa$REP_AREA),ifelse(is.null(reps),0, reps)),
                                     PA_STATUS=rep(unique(d_pa$PA_STATUS),ifelse(is.null(reps),0, reps)),
                                     PA_STATUSYR=rep(unique(d_pa$PA_STATUSYR),ifelse(is.null(reps),0, reps)),
                                     GOV_TYPE=rep(unique(d_pa$GOV_TYPE),ifelse(is.null(reps),0, reps)),
                                     OWN_TYPE=rep(unique(d_pa$OWN_TYPE),ifelse(is.null(reps),0, reps)),
                                     MANG_AUTH=rep(unique(d_pa$MANG_AUTH),ifelse(is.null(reps),0, reps))), error=function(e) return(NULL))
  
  m_all2_out <- rbind(m_all2_out,Bscore)
  return(m_all2_out)
}

tElapsed <- Sys.time()-startTime
cat(tElapsed, "for matching all PAs in", iso3,"\n")
cat("Done matching for", iso3, "\n")
stopImplicitCluster()
cat(paste("Dimension of Results: ", dim(results),"\n"))
# cat("Below is to check if certain controls have been used more than once: \n")
# cat(table(table(results[results$status==F,]$UID)),"\n")

dir.create(file.path(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",sep="")))
saveRDS(results, file=paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_matching_output_wk",gediwk,".RDS", sep=""))
cat("Exported matching results for",iso3,"\n")

# } 

# else if (file.exists(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_matching_output_wk",gediwk,".RDS", sep=""))){
#   cat("Step 4: Matching has been done for", iso3, "with GEDI data until week", gediwk,"\n")
# }


#STEP5. GEDI PROCESSING - using GEDI shots to extract the treatment/control status, also extract the MODIS PFT for AGB prediction 
# if (file.exists(paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",iso3,"_gedi_extracted_matching_wk",gediwk,".RDS", sep=""))){
cat(paste("Step 5: Performing WK ",gediwk,"GEDI extraction for", iso3,"\n"))
matched <- readRDS(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_matching_output_wk",gediwk,".RDS", sep=""))

mras  <- tryCatch(matched2ras(matched),
                  error=function(cond){
                    cat("Matched results is likely null for country", iso3, dim(matched),"quiting GEDI extraction \n")
                    writeLines("Matched results is likely null for country", paste(f.path,"WDPA_log/",iso3,"_log_matching.txt", sep=""))
                    return(quit(save="no"))}) #convert the macthed df to a raster stack 

mras_grid <- as(mras,"SpatialPolygonsDataFrame")

gedil2_f <- list.files(file.path(f.path,"WDPA_gedi_l2a+l2b_clean",iso3), full.names = T)

registerDoParallel(cores=round(mproc*0.5))
getDoParWorkers()
startTime <- Sys.time()
iso_matched_gedi <- foreach(this_csv=gedil2_f, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
  ##add the GEDI l4a model prediction for AGB here :
  gedi_l2  <- read.csv(this_csv) %>%
    dplyr::select(shot_number,lon_lowestmode, lat_lowestmode, starts_with("rh_"),cover, pai)%>%
    SpatialPointsDataFrame(coords=.[,c("lon_lowestmode","lat_lowestmode")],
                           proj4string=CRS("+init=epsg:4326"), data=.) %>%spTransform(., CRS("+init=epsg:6933"))
  
  iso_matched_gedi_df <- data.frame()
  matched_gedi <- raster::extract(mras,gedi_l2, df=T)
  matched_gedi_metrics <- cbind(matched_gedi,gedi_l2@data)
  
  matched_gedi_metrics_filtered <-matched_gedi_metrics %>% dplyr::filter(!is.na(status))
  iso_matched_gedi_df <- rbind(iso_matched_gedi_df,matched_gedi_metrics_filtered)
  
  return(iso_matched_gedi_df)
}

tElapsed <- Sys.time()-startTime
cat(tElapsed,"for extracting GEDI data for", iso3,"\n")
cat("Done GEDI extraction for",iso3,"\n")
stopImplicitCluster()

iso_matched_gedi_spdf <- SpatialPointsDataFrame(coords=iso_matched_gedi[,c("lon_lowestmode","lat_lowestmode")],
                                                proj4string=CRS("+init=epsg:4326"), data=iso_matched_gedi) %>%spTransform(., CRS("+init=epsg:6933"))

#rasterize the GEDI results 
gcount_ras <- rasterize(coordinates(iso_matched_gedi_spdf),mras, fun="count",background=NA)
names(gcount_ras) <- "gshot_counts"
gattr_ras <- rasterize(iso_matched_gedi_spdf@coords, mras, fun=getmode, 
                       field=iso_matched_gedi_spdf@data[,c("shot_number","status","pa_id","REP_AREA","PA_STATUSYR","DESIG_ENG","OWN_TYPE","GOV_TYPE",
                                                           "wwfbiom","wwfecoreg","pft","region")],background=NA)
gmetric_ras <- rasterize(iso_matched_gedi_spdf@coords, mras, fun=mean, 
                         field=iso_matched_gedi_spdf@data[,c("rh_050","rh_098","cover","pai")],background=NA)  #add AGBD here 
gstack <- stack(gcount_ras,gattr_ras,gmetric_ras)
g1km_sp <- as(gstack, 'SpatialPointsDataFrame')
g1km <- cbind(g1km_sp@coords, g1km_sp@data)
g1km_unfact <- convertFactor(matched0 = matched,exgedi = g1km)

dir.create(file.path(paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",sep="")))
#pass the df to function to compare treatment& control to filter out extra and export per pa df, then return entire contry result 
bala_g1km <- subdfExport(g1km_unfact)

saveRDS(bala_g1km, file=paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",iso3,"_gedi_extracted_matching_wk",gediwk,".RDS", sep=""))
write.csv(bala_g1km, file=paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",iso3,"_gedi_extracted_matching_wk",gediwk,".csv", sep=""))
writeLines(paste("Full data balanced and exported GEDI extracts using GEDI data until week", gediwk, sep=""), paste(f.path,"WDPA_log/",iso3,"_log_success.txt", sep=""))
cat(paste("Full data balanced and exported GEDI extracts for",iso3, "\n"))

  
# } 

# else if (file.exists(paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",iso3,"_gedi_extracted_matching_wk",gediwk,".RDS", sep=""))){
#   cat(paste("Step 5: WK ",gediwk,"GEDI extraction has been done for", iso3,"\n"))
#   
# }

