################################################################################
################################################################################
## 2. COUNTRY level shapefiles from Country x Biome files --- check the "hole issue"
################################################################################
################################################################################
library(tidyr)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(sf)
library(maptools)

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
#f.path <- "/Users/veronika/GEDI_global_PA/"
#write_f.path <- paste(f.path,"/WDPA_AGB_Input_Polygons/Country_PAs_and_Ctrls_test/",sep="")
write_f.path <- paste(f.path,"/WDPA_AGB_Input_Polygons/Country_PAs_and_Ctrls_V2/",sep="")
country_list <- dir(paste(f.path,"/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/",sep=""))
country_list

for(i in 1:length(country_list)){
#for(i in 200:length(country_list)){
  
  print(paste("i = ",i,sep=""))
  country <- as.character(country_list[i])
  
  all_pas <- NULL
  all_unmatched_pas <- NULL
  all_ctrls <- NULL
  
  if(!dir.exists(paste(write_f.path,country,sep=""))){
    dir.create(paste(write_f.path,country,sep=""))
  }
  
  country_biome_result_path <- paste(f.path,"/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/",country,"/",sep="")
  country_biome_files <- list.files(country_biome_result_path, pattern = country)
  country_biome_shp <- country_biome_files[grep(country_biome_files, pattern=".shp")]
  country_biome_shp
  
  if(length(country_biome_shp) > 0){
    
    unmatched_PA <- country_biome_shp[grep(country_biome_shp, pattern="unmatched_PA_V2.shp")]
    matched <- setdiff(country_biome_shp, unmatched_PA)
    matched_PA <- matched[grep(matched, pattern="_PA_V2.shp")]
    matched_Ctrl <- matched[grep(matched, pattern="_Ctrl_V2.shp")]
    
    if(length(matched_PA) > 0){
      for(a in 1:length(matched_PA)){
        print(matched_PA[a]) 
        ###read in the matched file and prepare it as a spdf
        pa_shp <- readShapePoly(paste(country_biome_result_path,matched_PA[a],sep=""),verbose=F)
        if(is.null(all_pas)){
          all_pas <- pa_shp
        }
        else{
          all_pas <- rbind.SpatialPolygonsDataFrame(all_pas, pa_shp)
        }
      }
    }
    
    if(length(matched_Ctrl) > 0){
      for(b in 1:length(matched_Ctrl)){
        print(matched_Ctrl[b])
        ctrl_shp <- readShapePoly(paste(country_biome_result_path,matched_Ctrl[b],sep=""),verbose=F)
        if(is.null(all_ctrls)){
          all_ctrls <- ctrl_shp
        }
        else{
          all_ctrls <- rbind.SpatialPolygonsDataFrame(all_ctrls, ctrl_shp)
        }
      }
    }
    
    if(length(unmatched_PA) > 0){
      for(c in 1:length(unmatched_PA)){
        print(unmatched_PA[c])
        unmatch_pa_shp <- readShapePoly(paste(country_biome_result_path,unmatched_PA[c],sep=""),verbose=F)
        if(is.null(all_unmatched_pas)){
          all_unmatched_pas <- unmatch_pa_shp
        }
        else{
          all_unmatched_pas <- rbind.SpatialPolygonsDataFrame(all_unmatched_pas, unmatch_pa_shp)
          #all_unmatched_pas <- bind(all_unmatched_pas, unmatch_pa_shp)
        }
      }
    }
    
    if(!is.null(all_pas)){
      pa_layer_name <- paste(country,"_PA_samples", sep="")
      pa_write_file_name <- paste(write_f.path, country, "/", country,"_PA_V2.shp", sep="")
      #all_pas2 <- aggregate(buffer(all_pas, width=0, dissolve=F), dissolve=T)
      all_pas2 <- aggregate(all_pas, dissolve=T)
      shapefile(all_pas2, file=pa_write_file_name, overwrite=TRUE)
    }
    if(!is.null(all_ctrls)){
      ctrl_layer_name <- paste(country,"_Ctrl_samples", sep="")
      ctrl_write_file_name <- paste(write_f.path, country, "/", country,"_Ctrl_V2.shp", sep="")
      #all_ctrls2 <- aggregate(buffer(all_ctrls, width=0, dissolve=F), dissolve=T)
      all_ctrls2 <- aggregate(all_ctrls, dissolve=T)
      shapefile(all_ctrls2, file=ctrl_write_file_name, overwrite=TRUE)
    }
    if(!is.null(all_unmatched_pas)){
      unmatch_pa_layer_name <- paste(country,"_unmatched_PA_samples", sep="")
      unmatch_pa_write_file_name <- paste(write_f.path, country, "/", country,"_unmatched_PA_V2.shp", sep="")
      #all_unmatched_pas2 <- aggregate(buffer(all_unmatched_pas, width=0, dissolve=F), dissolve=T)
      all_unmatched_pas2 <- aggregate(all_unmatched_pas, dissolve=T)
      shapefile(all_unmatched_pas2, file=unmatch_pa_write_file_name, overwrite=TRUE)
    }
  }
  rm(all_pas, all_pas2, all_ctrls, all_ctrls2, all_unmatched_pas, all_unmatched_pas2)
}



################################################################################
################################################################################
################################################################################
## OLD version with the "holes problem" not yet resolved (readOGR function)
################################################################################
################################################################################
## 2. COUNTRY level shapefiles from Country x Biome files
################################################################################
################################################################################
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
#f.path <- "/Users/veronika/GEDI_global_PA/"
write_f.path <- paste(f.path,"/WDPA_AGB_Input_Polygons/Country_PAs_and_Ctrls_V2_hole-issue-unresolved/",sep="")
country_list <- dir(paste(f.path,"/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/",sep=""))
country_list

#for(i in 1:52){
for(i in 200:length(country_list)){
  #for(i in 196:198){
  
  print(paste("i = ",i,sep=""))
  country <- as.character(country_list[i])
  
  all_pas <- NULL
  all_unmatched_pas <- NULL
  all_ctrls <- NULL
  
  if(!dir.exists(paste(write_f.path,country,sep=""))){
    dir.create(paste(write_f.path,country,sep=""))
  }
  
  country_biome_result_path <- paste(f.path,"/WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/",country,"/",sep="")
  country_biome_files <- list.files(country_biome_result_path, pattern = country)
  country_biome_shp <- country_biome_files[grep(country_biome_files, pattern=".shp")]
  country_biome_shp
  
  if(length(country_biome_shp) > 0){
    
    unmatched_PA <- country_biome_shp[grep(country_biome_shp, pattern="unmatched_PA_V2.shp")]
    matched <- setdiff(country_biome_shp, unmatched_PA)
    matched_PA <- matched[grep(matched, pattern="_PA_V2.shp")]
    matched_Ctrl <- matched[grep(matched, pattern="_Ctrl_V2.shp")]
    
    if(length(matched_PA) > 0){
      for(a in 1:length(matched_PA)){
        print(matched_PA[a]) 
        ###read in the matched file and prepare it as a spdf
        pa_shp <- readOGR(paste(country_biome_result_path,matched_PA[a],sep=""),verbose=F)
        if(is.null(all_pas)){
          all_pas <- pa_shp
        }
        else{
          all_pas <- rbind.SpatialPolygonsDataFrame(all_pas, pa_shp)
        }
      }
    }
    
    if(length(matched_Ctrl) > 0){
      for(b in 1:length(matched_Ctrl)){
        print(matched_Ctrl[b])
        ctrl_shp <- readOGR(paste(country_biome_result_path,matched_Ctrl[b],sep=""),verbose=F)
        if(is.null(all_ctrls)){
          all_ctrls <- ctrl_shp
        }
        else{
          all_ctrls <- rbind.SpatialPolygonsDataFrame(all_ctrls, ctrl_shp)
        }
      }
    }
    
    if(length(unmatched_PA) > 0){
      for(c in 1:length(unmatched_PA)){
        print(unmatched_PA[c])
        unmatch_pa_shp <- readOGR(paste(country_biome_result_path,unmatched_PA[c],sep=""),verbose=F)
        if(is.null(all_unmatched_pas)){
          all_unmatched_pas <- unmatch_pa_shp
        }
        else{
          all_unmatched_pas <- rbind.SpatialPolygonsDataFrame(all_unmatched_pas, unmatch_pa_shp)
          #all_unmatched_pas <- bind(all_unmatched_pas, unmatch_pa_shp)
        }
      }
    }
    
    if(!is.null(all_pas)){
      pa_layer_name <- paste(country,"_PA_samples", sep="")
      pa_write_file_name <- paste(write_f.path, country, "/", country,"_PA_V2.shp", sep="")
      all_pas2 <- aggregate(buffer(all_pas, width=0, dissolve=F), dissolve=T)
      shapefile(all_pas2, file=pa_write_file_name, overwrite=TRUE)
    }
    if(!is.null(all_ctrls)){
      ctrl_layer_name <- paste(country,"_Ctrl_samples", sep="")
      ctrl_write_file_name <- paste(write_f.path, country, "/", country,"_Ctrl_V2.shp", sep="")
      all_ctrls2 <- aggregate(buffer(all_ctrls, width=0, dissolve=F), dissolve=T)
      shapefile(all_ctrls2, file=ctrl_write_file_name, overwrite=TRUE)
    }
    if(!is.null(all_unmatched_pas)){
      unmatch_pa_layer_name <- paste(country,"_unmatched_PA_samples", sep="")
      unmatch_pa_write_file_name <- paste(write_f.path, country, "/", country,"_unmatched_PA_V2.shp", sep="")
      all_unmatched_pas2 <- aggregate(buffer(all_unmatched_pas, width=0, dissolve=F), dissolve=T)
      shapefile(all_unmatched_pas2, file=unmatch_pa_write_file_name, overwrite=TRUE)
    }
  }
  rm(all_pas, all_pas2, all_ctrls, all_ctrls2, all_unmatched_pas, all_unmatched_pas2)
}

