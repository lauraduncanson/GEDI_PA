#!/usr/bin/env Rscript

# This global processing script is derived from the global processing notebook 
#the input can be the iso3 code (3-character) for one or multiple countries 

options(warn=-1)
options(dplyr.summarise.inform = FALSE)

packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","spatialEco","bit64","randomForest", "modelr")
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

cat("Step 0: Loading global variables for", iso3,"with wk", gediwk, "data \n")

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
matching_tifs <- c("wwf_biomes","wwf_ecoreg","lc2000","d2roads", "dcities","dem","pop_cnt_2000","pop_den_2000","slope", "tt2cities_2000", "wc_prec_1990-1999",
                   "wc_tmax_1990-1999","wc_tavg_1990-1999","wc_tmin_1990-1999" )
ecoreg_key <- read.csv(paste(f.path,"wwf_ecoregions_key.csv",sep=""))
allPAs <- readRDS(paste(f.path,"WDPA_shapefiles/WDPA_polygons/",iso3,"_PA_poly.rds",sep=""))
MCD12Q1 <- raster(paste(f.path,"GEDI_ANCI_PFT_r1000m_EASE2.0_UMD_v1_projection_defined_6933.tif",sep=""))
projection(MCD12Q1) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
world_region <- raster(paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_6933.tif",sep=""))
projection(world_region) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
adm <- readOGR(paste(f.path,"WDPA_countries/shp/",iso3,".shp",sep=""),verbose=F)
adm_prj <- spTransform(adm, "+init=epsg:6933") 
load("/gpfs/data1/duncansongp/amberliang/trends.Earth/rf_noclimate.RData")
source("/gpfs/data1/duncansongp/amberliang/trends.Earth/git/GEDI_PA/matching_func.R")

# STEP1. Create 1km sampling grid with points only where GEDI data is available; first check if grid file exist to avoid reprocessing 
if(!file.exists(paste(f.path,"WDPA_grids/",iso3,"_grid_wk",gediwk,".RDS", sep=""))){
  cat("Step 1: Creating 1km sampling grid filter GEDI data for", iso3,"\n")
  GRID.lats <- raster(file.path(f.path,"EASE2_M01km_lats.tif"))
  GRID.lons <- raster(file.path(f.path,"EASE2_M01km_lons.tif"))
  GRID.lats.adm   <- crop(GRID.lats, adm_prj)
  GRID.lats.adm.m <- raster::mask(GRID.lats.adm, adm_prj)
  GRID.lons.adm   <- crop(GRID.lons, adm_prj)
  GRID.lons.adm.m <- raster::mask(GRID.lons.adm, adm_prj)
  rm(GRID.lats, GRID.lons, GRID.lats.adm, GRID.lons.adm)
  
  #1.3) extract coordinates of raster cells with valid GEDI data in them
  gedi_folder <- paste(f.path,"WDPA_gedi_l2a+l2b_clean2/",iso3,"/",sep="")
  
  GRID.coords <- data.frame()
  for(i in 1:length(dir(gedi_folder))){
    # print(list.files(gedi_folder)[i])
    gedi_data <- read.csv(list.files(gedi_folder,full.names=TRUE)[i]) %>%
      dplyr::select(lon_lowestmode,lat_lowestmode)
    gedi_pts  <- SpatialPoints(coords=gedi_data[,c("lon_lowestmode","lat_lowestmode")],
                               proj4string=CRS("+init=epsg:4326"))
    gedi_pts_prj <- spTransform(gedi_pts, "+init=epsg:6933")
    
    gcount_ras <- rasterize(coordinates(gedi_pts_prj),GRID.lons.adm.m , fun="count",background=NA)
    names(gcount_ras) <- "gshot_counts"
    pxid <- raster::extract(gcount_ras,  gedi_pts_prj)
    gedi_pts_prj %>% 
      SpatialPointsDataFrame(., data=data.frame(pxid)) ->gedi_pts_prj_sp
    gedi_pts_prj_sp$pxid[is.na(gedi_pts_prj_sp$pxid)] <- 0
    gedi_pts_prj_sp[gedi_pts_prj_sp$pxid>5,]->gedi_pts_prj_filtered  #change the numeric threshold to filter with a different min # of GEDI shots in each 1km cell
    
    GRID.lons.overlap <- GRID.lons.adm.m[gedi_pts_prj_filtered]
    GRID.lats.overlap <- GRID.lats.adm.m[gedi_pts_prj_filtered]
    
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
      GRID.pts.nonPA <- erase.point(GRID.pts.nonPA, PA2, inside = TRUE) ##remove pts inside poly
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
    
  for (j in 1:length(matching_tifs)){
    ras <- raster(paste(f.path, "WDPA_input_vars_iso3_v2/",iso3,"/",matching_tifs[j],".tif", sep=""))
    print(matching_tifs[j])
    ras_ex <- raster::extract(ras, nonPA_spdf@coords, method="simple", factors=FALSE)
    nm <- names(ras)
    nonPA_spdf <- cbind(nonPA_spdf, ras_ex)
    names(nonPA_spdf)[j+2] <- matching_tifs[j]
    
  }
  d_control <- nonPA_spdf
  d_control$status <- as.logical("FALSE")
  names(d_control) <- make.names(names(d_control), allow_ = FALSE)
  d_control <- data.frame(d_control) %>%
    dplyr::rename(
      land_cover = lc2000,
      slope = slope,
      elevation = dem,
      popden = pop.den.2000,
      popcnt=pop.cnt.2000,
      min_temp=wc.tmin.1990.1999,
      max_temp=wc.tmax.1990.1999,
      mean_temp = wc.tavg.1990.1999,
      prec = wc.prec.1990.1999,
      tt2city= tt2cities.2000,
      wwfbiom = wwf.biomes,
      wwfecoreg = wwf.ecoreg,
      d2city = dcities,
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
      for (j in 1:length(matching_tifs)){
        ras <- raster(paste(f.path, "WDPA_input_vars_iso3_v2/",iso3,"/",matching_tifs[j],".tif", sep=""))
        ras <- crop(ras, testPA)
        ras_ex <- raster::extract(ras, testPA_spdf@coords, method="simple", factors=F)
        nm <- names(ras)
        testPA_spdf <- cbind(testPA_spdf, ras_ex)
        names(testPA_spdf)[j+2] <- matching_tifs[j]
        
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
      names(d_pa) <- make.names(names(d_pa), allow_ = FALSE)
      d_pa <- data.frame(d_pa) %>%
        dplyr::rename(
          land_cover = lc2000,
          slope = slope,
          elevation = dem,
          popden = pop.den.2000,
          popcnt=pop.cnt.2000,
          min_temp=wc.tmin.1990.1999,
          max_temp=wc.tmax.1990.1999,
          mean_temp = wc.tavg.1990.1999,
          prec = wc.prec.1990.1999,
          tt2city= tt2cities.2000,
          wwfbiom = wwf.biomes,
          wwfecoreg = wwf.ecoreg,
          d2city = dcities,
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

if(!dir.exists(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",sep=""))){
  # cat("Matching result dir does not EXISTS\n")
  dir.create(file.path(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",sep="")))
  d_PAs <- list.files(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs/", sep=""), pattern=paste("wk",gediwk,sep=""), full.names=FALSE)
} else if (dir.exists(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",sep=""))){   #if matching result folder exists, check for any PAs w/o matched results
  pattern1 = c(paste("wk",gediwk,sep=""),"RDS")
  matched_PAid <- list.files(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",sep=""), full.names = FALSE, pattern=paste0(pattern1, collapse="|"))%>%
    readr::parse_number() %>% unique()
  d_PAs<- list.files(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs/", sep=""), pattern=paste("wk",gediwk,sep=""), full.names=FALSE)
  d_PA_id <- d_PAs %>% readr::parse_number()
  runPA_id <- d_PA_id[!(d_PA_id %in% matched_PAid)]
  if (length(runPA_id)>0){
    Pattern2 <-  paste(runPA_id, collapse="|")
    runPA <-  d_PAs[grepl(Pattern2,d_PAs)]
    d_PAs <- runPA
  } else {
    d_PAs <- NULL
  }
}

registerDoParallel(mproc)
# cat("Parallel processing",getDoParWorkers(),"PAs \n")
startTime <- Sys.time()
foreach(this_pa=d_PAs,.combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','optmatch','doParallel')) %dopar% {
  pa <- this_pa
  id_pa <-pa %>%str_split("_") %>% unlist %>% .[3]
  # cat(id_pa, "in",iso3,"\n")
  cat("No.", match(pa,d_PAs),"of total",length(d_PAs),"PAs in ", iso3, "\n" )
  d_pa <- readRDS(paste(f.path,"WDPA_matching_points/",iso3,"/",iso3,"_testPAs/",pa, sep=""))
  # cat(iso3, "pa no.",id_pa, "has",nrow(d_pa)," of treatment \n")
  d_filtered_prop <- tryCatch(propensity_filter(d_pa, d_control_local), error=function(e) return(NA))  #return a df of control and treatment after complete cases and propensity filters are applied
  # cat("Propensity score filtered DF dimension is",dim(d_filtered_prop),"\n")
  d_wocat_all <- tryCatch(filter(d_filtered_prop, status),error=function(e) return(NA))
  d_control_all <- tryCatch(filter(d_filtered_prop, !status),error=function(e) return(NA))
  
  n_control <- dim(d_control_all)[1]
  # ids_all <- d_control_all$UID   #seq(1,n_control)
  ids_all0 <- tryCatch(d_control_all$UID, error=function(e) return(NA))
  set.seed(125)
  # cat("Using number of cores:",getDoParWorkers(),"\n")
  N <- ceiling(nrow(d_wocat_all)/300)
  l <- tryCatch(split(d_wocat_all, sample(1:N, nrow(d_wocat_all), replace=TRUE)),error=function(e) return(NULL))
  # l <- tryCatch(split(d_wocat_all, (as.numeric(rownames(d_wocat_all))-1) %/% 300),error=function(e) return(0))
  
  if (length(l)<50 && length(l)>0 ){
    pa_match <- data.frame()
    for (pa_c in 1:length(l)){
      ids_all <- d_control_all$UID
      cat("chunk",pa_c,"of PA", id_pa,"\n")
      
      d_wocat_chunk <- l[[pa_c]]
      # #sample the control dataset to the size of the sample dataset, keep unsampled ids to iterate until full number of matches found
      n_treatment <- dim(d_wocat_chunk)[1]
      
      t <- ifelse(floor(n_control/n_treatment)<=7, ifelse(floor(n_control/n_treatment)<1, 1,floor(n_control/n_treatment)),7)   #floor(n_control/n_treatment))
      n_sample <- round(n_treatment*t)    #now the n_control is 1.4 times the number of n_treatment, 7 will set the if ststament below to flase
      m_all2_out <- data.frame()
      Bscore <- data.frame()
      n_matches <- 0
      tryCatch(
        while(n_matches < n_treatment){
          n_ids <- length(ids_all)
          # cat("n ids",n_ids,"\n")
          if(n_ids > n_sample){
            set.seed(125)
            sample_ids_bar <- sample(ids_all, n_sample)
            sample_ids <- sample(ids_all0, n_sample)
            d_control_sample <- d_control_all[d_control_all$UID %in% sample_ids,]
            ids_all <-setdiff(ids_all, sample_ids_bar)    #ids_all[-sample_ids]
            # cat("protected uid", head(d_wocat_chunk$UID),"\n")
            # All approaches
            new_d <- tryCatch(rbind(d_wocat_chunk,d_control_sample),error=function(e) return(NULL))
            # new_d <- tryCatch(rbind(d_wocat_chunk,d_control_all),error=function(e) return(NULL))
            #create a smaller distance matrix
            m_all <- tryCatch(match_wocat(new_d, pid=id_pa),error=function(e) return(NULL))
            # m_all <- match_wocat(new_d)
            m_all2 <- tryCatch(m_all[1,],error=function(e) return(NULL))
            # m_all2 <- m_all[1,]
            n_matches_temp <- tryCatch(nrow(m_all2$df),error=function(e) return(NULL))
            # n_matches_temp <- nrow(m_all2$df)
            if(!is.null(n_matches_temp)){
              # n_matches <- n_matches + nrow(m_all2$df)
              m_all2$df$pa_id <- rep(id_pa,n_matches_temp)
              m_all2_out <- rbind(m_all2_out, m_all2$df)
              matched_protected <- m_all2$df %>% dplyr::filter(status==TRUE)
              matched_control <- m_all2$df %>% dplyr::filter(status==FALSE)
              cat("matched_protected", nrow(matched_protected),"\n")
              n_matches <- n_matches + nrow(matched_protected)
              d_wocat_chunk <- d_wocat_chunk[-(match(matched_protected$UID,d_wocat_chunk$UID)),]
              # d_control_all <- d_control_all[-(match(matched_control$UID,d_control$UID)),]
            } 
            # ids_all <-setdiff(ids_all, sample_ids)
            ids_all0 <-setdiff(ids_all0, matched_control$UID)
            # else {
            #   n_treatment <- 0  #if not macthes are found in this sampling
            # }
          } else {n_treatment <- n_matches}
        }, error=function(e) return(NULL))
      # ids_all0 <-setdiff(ids_all0, matched_control$UID)
      match_score <- m_all2_out
      cat(table(match_score$status),"\n")
      pa_match <- rbind(pa_match,match_score)
    }
  } else if (length(l)>=50){
    registerDoParallel(3)
    pa_match <- foreach(pa_c=length(l), .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','optmatch','doParallel'))%dopar%{
      # cat("Matching treatment chunk", pa_c, "out of", length(l), "for PA", id_pa,"\n")
      # cat("chunk",pa_c,"\n")
      # cat("head control",head(ids_all0),"\n")
      d_wocat_chunk <- l[[pa_c]]
      # #sample the control dataset to the size of the sample dataset, keep unsampled ids to iterate until full number of matches found
      n_treatment <- dim(d_wocat_chunk)[1]
      # cat( "n control", length(ids_all0),"\n")
      t <- ifelse(floor(n_control/n_treatment)<=7, ifelse(floor(n_control/n_treatment)<1, 1,floor(n_control/n_treatment)),7)   #floor(n_control/n_treatment))
      n_sample <- round(n_treatment*t)    #now the n_control is 1.4 times the number of n_treatment, 7 will set the if ststament below to flase
      m_all2_out <- data.frame()
      Bscore <- data.frame()
      n_matches <- 0
      
      tryCatch(
        while(n_matches < n_treatment){
          
          n_ids <- length(ids_all0)
          cat("n ids",n_ids,"\n")
          if(n_ids > n_sample){
            set.seed(125)
            sample_ids_bar <- sample(ids_all, n_sample)
            sample_ids <- sample(ids_all0, n_sample)
            d_control_sample <- d_control_all[d_control_all$UID %in% sample_ids,]
            ids_all <-setdiff(ids_all, sample_ids_bar)    #ids_all[-sample_ids]
            cat("protected uid", head(d_wocat_chunk$UID),"\n")
            # All approaches
            new_d <- tryCatch(rbind(d_wocat_chunk,d_control_sample),error=function(e) return(NULL))
            # new_d <- tryCatch(rbind(d_wocat_chunk,d_control_all),error=function(e) return(NULL))
            
            #create a smaller distance matrix
            m_all <- tryCatch(match_wocat(new_d, pid=id_pa),error=function(e) return(NULL))
            # m_all <- match_wocat(new_d)
            m_all2 <- tryCatch(m_all[1,],error=function(e) return(NULL))
            # m_all2 <- m_all[1,]
            n_matches_temp <- tryCatch(nrow(m_all2$df),error=function(e) return(NULL))
            # n_matches_temp <- nrow(m_all2$df)
            if(!is.null(n_matches_temp)){
              # n_matches <- n_matches + nrow(m_all2$df)
              m_all2$df$pa_id <- rep(id_pa,n_matches_temp)
              m_all2_out <- rbind(m_all2_out, m_all2$df)
              matched_protected <- m_all2$df %>% dplyr::filter(status==TRUE)
              matched_control <- m_all2$df %>% dplyr::filter(status==FALSE)
              cat("matched_protected", nrow(matched_protected),"\n")
              n_matches <- n_matches + nrow(matched_protected)
              d_wocat_chunk <- d_wocat_chunk[-(match(matched_protected$UID,d_wocat_chunk$UID)),]
              # d_control_all <- d_control_all[-(match(matched_control$UID,d_control$UID)),]
              # 
            } 
            ids_all0 <-setdiff(ids_all0, matched_control$UID)
            cat( "n control", length(ids_all0),"\n")
            
            # else {
            #   n_treatment <- 0  #if not macthes are found in this sampling
            # }
          } else {n_treatment <- n_matches}
        }, error=function(e) return(NULL))
      # ids_all0 <-setdiff(ids_all0, matched_control$UID)
      match_score <- m_all2_out
      cat(table(match_score$status),"\n")
      return(match_score)
    }
    stopImplicitCluster()
  } else{
    pa_match <- NULL
  }
    saveRDS(pa_match, file=paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_matching_results_wk",gediwk,".RDS", sep=""))
  # cat("Results exported for PA", id_pa,"\n")
  rm(pa_match)
  return(NULL)
}

tElapsed <- Sys.time()-startTime
# cat(tElapsed, "for matching all PAs in", iso3,"\n")
stopImplicitCluster()
cat("Done matching for",iso3,". Finishing...\n")
 

 # writeLines(paste("Full data balanced and exported GEDI extracts using GEDI data until week", gediwk, sep=""), paste(f.path,"WDPA_log/",iso3,"_log_success_wk",gediwk,".txt", sep=""))
