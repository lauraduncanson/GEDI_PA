################################################################################
################################################################################
## 3. CONTINENT x BIOME level shapefiles from Country x Biome files V2
################################################################################
################################################################################
library(tidyr)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(sf)
library(maptools)

#f.path <- "/Users/veronika/GEDI_global_PA/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
write_f.path <- paste(f.path,"/WDPA_AGB_Input_Polygons/Continent_Biome_PAs_and_Ctrls_V2/",sep="")
country_biome_path <- paste(f.path,"WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/",sep="")
continent_list <- read.csv(paste(f.path,"iso3_region_pair_ALL.csv",sep=""))
countries <- unique(continent_list$iso3)
continent_ids <- unique(continent_list$continent)
continent_names <- unique(continent_list$continent_name)
continent_names

for(i in 1:length(continent_names)){
  print(continent_names[i])
  dir.create(paste(write_f.path,continent_names[i],"_files",sep=""))
  conti_countries <- continent_list[continent_list$continent_name == continent_names[i], "iso3"]
  for(c in 1:length(conti_countries)){
    files_list <- list.files(paste(country_biome_path,conti_countries[c],sep=""), full.names=T)
    file.copy(from=files_list, to=paste(write_f.path,continent_names[i],"_files",sep=""))
  }
}

biomes_list <- read.csv(paste(f.path,"/wwf_biomes_key.csv",sep=""))
biome.names <- unique(biomes_list$BIOME_NAME)
biome.names

for(i in 1:length(continent_names)){
  
  print(continent_names[i])
  
  for(j in length(biome.names):1){
    
    print(biome.names[j])
    
    all_pas <- NULL
    all_ctrls <- NULL
    all_unmatched_pas <- NULL
    
    continent_biome_result_path <- paste(f.path,"WDPA_AGB_Input_Polygons/Continent_Biome_PAs_and_Ctrls_V2/",continent_names[i],"_files/",sep="")
    continent_biome_files <- list.files(continent_biome_result_path, pattern=".shp")
    continent_biome_shp <- continent_biome_files[grep(continent_biome_files, pattern=biome.names[j])]
    print(continent_biome_shp)
    
    if(length(continent_biome_shp) > 0){
      
      unmatched_PA <- continent_biome_shp[grep(continent_biome_shp, pattern="unmatched_PA_V2.shp")]
      matched <- setdiff(continent_biome_shp, unmatched_PA)
      matched_PA <- matched[grep(matched, pattern="_PA_V2.shp")]
      matched_Ctrl <- matched[grep(matched, pattern="_Ctrl_V2.shp")]
      
      if(length(matched_PA) > 0){
        for(a in 1:length(matched_PA)){
          print(matched_PA[a]) 
          ###read in the matched file and prepare it as a spdf
          pa_shp <- readShapePoly(paste(continent_biome_result_path,matched_PA[a],sep=""),verbose=F)
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
          ctrl_shp <- readShapePoly(paste(continent_biome_result_path,matched_Ctrl[b],sep=""),verbose=F)
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
          unmatch_pa_shp <- readShapePoly(paste(continent_biome_result_path,unmatched_PA[c],sep=""),verbose=F)
          if(is.null(all_unmatched_pas)){
            all_unmatched_pas <- unmatch_pa_shp
          }
          else{
            all_unmatched_pas <- bind(all_unmatched_pas, unmatch_pa_shp)
            #all_unmatched_pas <- rbind.SpatialPolygonsDataFrame(all_unmatched_pas, unmatch_pa_shp)
          }
        }
      }
      
      dir.create(paste(write_f.path,continent_ids[i],sep=""))
      
      if(!is.null(all_pas)){
        pa_layer_name <- paste(biome.names[j],"_PA_samples", sep="")
        pa_write_file_name <- paste(write_f.path,continent_ids[i],"/",continent_ids[i],"_",biome.names[j],"_PA_V2.shp", sep="")
        #all_pas2 <- aggregate(buffer(all_pas, width=0, dissolve=F), dissolve=T)
        all_pas2 <- aggregate(all_pas, dissolve=T)
        shapefile(all_pas2, file=pa_write_file_name, overwrite=TRUE)
      }
      if(!is.null(all_ctrls)){
        ctrl_layer_name <- paste(biome.names[j],"_Ctrl_samples", sep="")
        ctrl_write_file_name <- paste(write_f.path,continent_ids[i],"/",continent_ids[i],"_",biome.names[j],"_Ctrl_V2.shp", sep="")
        #all_ctrls2 <- aggregate(buffer(all_ctrls, width=0, dissolve=F), dissolve=T)
        all_ctrls2 <- aggregate(all_ctrls, dissolve=T)
        shapefile(all_ctrls2, file=ctrl_write_file_name, overwrite=TRUE)
      }
      if(!is.null(all_unmatched_pas)){
        unmatch_pa_layer_name <- paste(biome.names[j],"_unmatched_PA_samples", sep="")
        unmatch_pa_write_file_name <- paste(write_f.path,continent_ids[i],"/",continent_ids[i],"_",biome.names[j],"_unmatched_PA_V2.shp", sep="")
        #all_unmatched_pas2 <- aggregate(buffer(all_unmatched_pas, width=0, dissolve=F), dissolve=T)
        all_unmatched_pas2 <- aggregate(all_unmatched_pas, dissolve=T)
        shapefile(all_unmatched_pas2, file=unmatch_pa_write_file_name, overwrite=TRUE)
      }
    }
  }
}

