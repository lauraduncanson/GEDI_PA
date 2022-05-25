getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getBiomeLevelPAsPerCountry <- function(biome, biome_id, country, gediwk=18){
  
  biomegridPolygon <- NULL
  unmatchedbiomepolygon <- NULL
  print(paste("Processing ",biome, " in ", country, sep =""))
  f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
  #wkpath <- paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/",iso3,"_wk", gediwk,sep="")
  write_f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/missed_list/"
  
  #f.path <- "C:/Users/sruth/OneDrive - UGent/UMDPostdoc/PA_analysis/"
  #write_f.path <- "C:/Users/sruth/OneDrive - UGent/UMDPostdoc/PA_analysis/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/"
  
  #conti_list <- read.csv("C:/Users/sruth/OneDrive - UGent/UMDPostdoc/PA_analysis/iso3_region_pair.csv")
  #iso3_list <- unique(conti_list$iso3)

    all_pas_and_ctrls <- NULL
    iso3 <- country
    #print(iso3)

    #allpapath <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/WDPA_polygons_clipped/"
    
    wkpath <- paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/",iso3,"_wk", gediwk,sep="")
    #print(wkpath)
    matched_pa <- list.files(wkpath,".RDS")  #matched_PAs
    print(paste("Matched pa length: ",length(matched_pa), sep=""))
    allpapath <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/WDPA_polygons_clipped/"
    allpaRDS <- readRDS(paste(allpapath,iso3,"_PA_poly.rds",sep=""))
    allpaRDS_data <- allpaRDS@data
    #print(allpaRDS_data)
    matched_pa_ids <- vector()
    
    if(length(matched_pa) > 0){
      
      #print("pas available")
      for(i in 1:length(matched_pa)){
       
        pa_id <- unlist(strsplit(matched_pa[i], '_'))[3]
        
        
        #print(matched_pa[i])
        ###read in the matched file and prepare it as a spdf
        match_results <- readRDS(paste(wkpath,matched_pa[i],sep="/"))  #testing with the the first pa file from iso3
        #print(dim(match_results)) 
        if(is.null(match_results)){
          next
        }
        
        if(dim(match_results)[1] == 0){
          next
        }
        
        #print("not empty pas available")
        match_results <- match_results %>% drop_na()
        #matched_pa_ids = rbind(matched_pa_ids,pa_id)
        matched_pa_ids <- c(matched_pa_ids, pa_id) 
        
        if(dim(match_results)[1] == 0){
          next
        }
        
        dominant_biome <- getMode(match_results$wwfbiom)
        
        
        if(dominant_biome != biome){
          next
        }
        
        unique_match_results <- match_results %>% group_by(lat, lon) %>% filter(row_number() == 1)
        
        
        
        match_results2 <- SpatialPointsDataFrame(coords=unique_match_results[,c("lon","lat")],
                                                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs "), data=unique_match_results) %>% 
          spTransform(., CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "))  #espg:6933
        
        ###filter for unique controls, a discussed before, some matched cells duplciate with themselevs
        controls <- match_results2[match_results2$status==FALSE,]
        pas <- match_results2[match_results2$status==TRUE,]
        
        #loc <- cbind(match_results2$lon, match_results2$lat)
        #(unique(loc))%>% dim()  #unique points
        #dim(loc)   #non-unique/dup points
        # unique(match_results2) ->upa
        # write.csv(match_results,"/gpfs/data1/duncansongp/amberliang/cod_testPA3.csv")
        
        ###get th 1km raster we will use to get the grids
        
        # plot(r)
        
        ###rasterize matched pa
        match_results3 <- rbind(controls, pas) #[1:12439,]
        
        match_results3$status <- as.numeric(match_results3$status)
        match_results3$pa_id <- as.numeric(match_results3$pa_id)
        #match_results3$wwfbiom <- as.numeric(match_results3$wwfbiom)
        
        if(is.null(all_pas_and_ctrls)){
          all_pas_and_ctrls <- match_results3
        }
        else{
          
          all_pas_and_ctrls <- rbind(all_pas_and_ctrls, match_results3)
        }
      }
      
    }
    adm <- readOGR(paste(f.path,"WDPA_countries/shp/",iso3,".shp",sep=""),verbose=F)
    adm_prj <- spTransform(adm, "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ") 
    
    if(!(is.null(all_pas_and_ctrls))){
      #print("creating matched biome polygon for PA and control")
      pftr <- raster(paste(f.path,"GEDI_ANCI_PFT_r1000m_EASE2.0_UMD_v1_projection_defined_6933.tif",sep="/"))
      
      r <- raster::crop(pftr, adm_prj)
      rasOut <- rasterize(all_pas_and_ctrls@coords, field=all_pas_and_ctrls@data[,c("status", "pa_id")], r)
      #rasOut
      rasOut[] %>% table()
      biomegridPolygon <- rasterToPolygons(rasOut)
      #plot(gridPolygon)
      names(biomegridPolygon) <- c("status","pa_id")
      
      print("writing matched biome polygon for PA and control")
      biomegridPolygon0 <- biomegridPolygon[biomegridPolygon$status==0,]
      biomegridPolygon1 <-biomegridPolygon[biomegridPolygon$status==1,]
      
      pa_layer_name <- paste(country,"_",biome,"_PA_samples", sep="")
      ctrl_layer_name <- paste(country,"_",biome,"_Ctrl_samples", sep="")
      pa_write_file_name <-  paste(write_f.path, country,"_",biome,"_PA",".shp", sep="")
      ctrl_write_file_name <-  paste(write_f.path,country,"_",biome,"_Ctrl",".shp", sep="")
      
      writeOGR(biomegridPolygon0,pa_write_file_name,layer=pa_layer_name, driver = "ESRI Shapefile")
      writeOGR(biomegridPolygon1,ctrl_write_file_name,layer=ctrl_layer_name, driver = "ESRI Shapefile")
    }
    
    biome_tif <- raster('/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_input_vars_GLOBAL/wwf_biomes_1km_global_EPSG4326.tif')
    
    #crs(biome_tif) <- crs(adm_prj)
    
     print(paste("All matched pa ids: ", matched_pa_ids, sep=""))
      allpa_ids <- unique(allpaRDS$WDPAID)
      
      for(id in 1:length(allpa_ids)){
        
        id_to_search <- as.character(allpa_ids[id])
        #print(paste("Current unmatched pa id: ",allpa_ids[id], sep=""))
        if(id_to_search %in% matched_pa_ids){
          #print("ID matched")
          next
        }
        #print(paste("Current unmatched pa id: ",allpa_ids[id], sep=""))
        

        unmatched_pa <- allpaRDS[allpaRDS@data$WDPAID == allpa_ids[id],]
        #unmatched_pa <- spTransform(unmatched_pa, "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ") 
        
        ext <- extent(unmatched_pa)
        
        
        biome_tif_cpd <- raster::crop(biome_tif, ext) 
        #print(biome_tif_cpd)
        dominant_biome_id <- getMode(values(biome_tif_cpd))
        #print(dominant_biome_id)
        if(is.na(dominant_biome_id)){
          next
        }
        if(dominant_biome_id != biome_id){
          next
        }
        #print(paste("Biome match success: ", dominant_biome_id,sep=""))
        unmatched_pa <- spTransform(unmatched_pa, "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ") 
        #unmatched_pa <-  allpaRDS %>%  filter(paid == allpa_ids[id])
        
        #unmatched_pa$status <- as.numeric(unmatched_pa$status)
        #unmatched_pa$pa_id <- as.numeric(unmatched_pa$pa_id)
        #unmatched_pa$wwfbiom <- as.numeric(unmatched_pa$wwfbiom)
        if(is.null(unmatchedbiomepolygon)){
          unmatchedbiomepolygon <- unmatched_pa
        }
        else{
          
          unmatchedbiomepolygon <- rbind.SpatialPolygonsDataFrame(unmatchedbiomepolygon, unmatched_pa)
        }
      }
      if(!is.null(unmatchedbiomepolygon)) {
      print(paste("Writing unmatched biomes in ",iso3,sep=""))
      unmapa_layer_name <- paste(country,"_",biome,"_unmatched_PA_samples", sep="")
      unmapa_write_file_name <-  paste(write_f.path, country,"_",biome,"_unmatched_PA",".shp", sep="")
      writeOGR(unmatchedbiomepolygon,unmapa_write_file_name,layer=unmapa_layer_name, driver = "ESRI Shapefile")
      }
}

