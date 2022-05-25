library(tidyr)
library(rgdal)
library(sp)
library(raster)
library(plyr)
library(dplyr)
library(parallel)

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
#f.path <- "/Users/veronika/GEDI_global_PA/"
read_f.path <- paste(f.path,"WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new/",sep="")
write_f.path <- paste(f.path,"WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/",sep="")
country_list <- dir(read_f.path)
country_list <- country_list[-c(which(country_list=="BGR_0"),which(country_list=="ROU_0"),
                                which(country_list=="USA"))]
country_list

################################################################################
## 1.a. correct unmatched_PA shapefiles with buffer = 0 around them
################################################################################
for(i in 1:length(country_list)){
  
  country <- country_list[i]
  print(country)
  
  if(!dir.exists(paste(write_f.path,country,sep=""))){
    dir.create(paste(write_f.path,country,sep=""))
  }

##read in unmatchedPA shapefiles and save the modified versions to Dissolved_new_V2 folder
  unmatchedPAs <- list.files(paste(read_f.path,country,sep=""), pattern="unmatched_PA.shp")
  
  if(length(unmatchedPAs) > 0){
    for(s in 1:length(unmatchedPAs)){
      print(unmatchedPAs[s])
      file.name <- paste(read_f.path,country,"/",unmatchedPAs[s],sep="")
      shp1 <- readOGR(file.name)
      shp2 <- buffer(shp1, width=0, dissolve=F)
      
      sa1 <- area(shp1)
      sa2 <- area(shp2)
      if((sa2-sa1) > 0){print("SHP2 area > SHP1 area")}
      if((sa2-sa1) == 0){print("SHP1 and SHP2 areas EQUAL")}
      if((sa2-sa1) < 0){print("SHP1 area > SHP2 area")}
      
      file.name.out2 <- paste(write_f.path,country,"/",strsplit(unmatchedPAs[s],".shp")[[1]],"_V2.shp",sep="")
      shapefile(shp2, file=file.name.out2, overwrite=TRUE)
      
      shp3 <- aggregate(shp2, dissolve=TRUE)
      sa3 <- area(shp3)
      if((sa3-sa2) > 0){print("SHP3 area > SHP2 area")}
      if((sa3-sa2) < 0){print("SHP2 area > SHP3 area")}
      if((sa3-sa2) == 0){print("SHP2 and SHP3 areas EQUAL")}else{
        file.name.out3 <- paste(write_f.path,country,"/",strsplit(unmatchedPAs[s],".shp")[[1]],"_V3.shp",sep="")
        shapefile(shp3, file=file.name.out3, overwrite=TRUE)
      }
    }
  }
} 

##copy over unmodified PA and Ctrl shapefiles from Dissolved_new folder  
for(i in 1:length(country_list)){
  
  country <- country_list[i]
  print(country)
  
  country.files <- list.files(paste(read_f.path,country,sep=""))
  unmatched_PAs <- list.files(paste(read_f.path,country,sep=""), pattern="unmatched_PA")
  copy.files <- setdiff(country.files, unmatched_PAs)
  paste.files <- paste(write_f.path, country, "/",
                       substr(copy.files, 1, nchar(copy.files)-4), "_V2",
                       substr(copy.files, nchar(copy.files)-3, nchar(copy.files)), sep="")
  file.copy(paste(read_f.path,country,"/",copy.files,sep=""), paste.files)
} 

##check if number of files match in old vs new directories
for(i in 1:length(country_list)){
  country <- country_list[i]
  files.old <- length(list.files(paste(read_f.path,country,sep="")))
  files.new <- length(list.files(paste(write_f.path,country,sep="")))
  if(files.old != files.new){print(paste("CHECK",country,sep=" "))}
}

################################################################################
## 1.b. create COUNTRY x Biome for USA (from USA east, west, pcfc)
################################################################################
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
#f.path <- "/Users/veronika/GEDI_global_PA/"

country_biome_result_path <- paste(f.path,"WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new/",sep="")
write_f.path <- paste(f.path,"WDPA_AGB_Input_Polygons/Country_Biome_PAs_and_Ctrls/Dissolved_new_V2/USA/",sep="")
dir.create(write_f.path)

biomes_list <- read.csv(paste(f.path,"wwf_biomes_key.csv",sep=""))
biome.names <- unique(biomes_list$BIOME_NAME)
biome.names

#read in list of files from USA_east, USA_pcfc, USA_west
files_list <- c(list.files(paste(country_biome_result_path,"USA_east/",sep=""), pattern=".shp", full.names=T),
                list.files(paste(country_biome_result_path,"USA_pcfc/",sep=""), pattern=".shp", full.names=T),
                list.files(paste(country_biome_result_path,"USA_west/",sep=""), pattern=".shp", full.names=T))

for(j in 1:length(biome.names)){
  
  biome <- as.character(biome.names[j])
  print(biome)
  
  all_pas <- NULL
  all_ctrls <- NULL
  all_unmatched_pas <- NULL
  
  biome_shape <- files_list[grep(files_list, pattern=biome)]
  print(biome_shape)
  
  if(length(biome_shape) > 0){
    
    unmatched_PA <- biome_shape[grep(biome_shape, pattern="unmatched_PA.shp")]
    matched <- setdiff(biome_shape, unmatched_PA)
    matched_PA <- matched[grep(matched, pattern="_PA.shp")]
    matched_Ctrl <- matched[grep(matched, pattern="_Ctrl.shp")]
    
    if(length(matched_PA) > 0){
      for(a in 1:length(matched_PA)){
        print(matched_PA[a]) 
        ###read in the matched file and prepare it as a spdf
        pa_shp <- readShapePoly(matched_PA[a], verbose=F)
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
        ctrl_shp <- readShapePoly(matched_Ctrl[b], verbose=F)
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
        unmatch_pa_shp <- readShapePoly(unmatched_PA[c], verbose=F)
        if(is.null(all_unmatched_pas)){
          all_unmatched_pas <- unmatch_pa_shp
        }
        else{
          #all_unmatched_pas <- rbind.SpatialPolygonsDataFrame(all_unmatched_pas, unmatch_pa_shp)
          all_unmatched_pas <- bind(all_unmatched_pas, unmatch_pa_shp)
        }
      }
    }
    
    if(!is.null(all_pas)){
      pa_layer_name <- paste(biome.names[j],"_PA_samples", sep="")
      pa_write_file_name <- paste(write_f.path, "USA_", biome.names[j],"_PA_V2.shp", sep="")
      #all_pas2 <- buffer(all_pas, width=0, dissolve=FALSE)
      all_pas2 <- aggregate(all_pas, dissolve=T)
      shapefile(all_pas, file=pa_write_file_name, overwrite=TRUE)
      }
    if(!is.null(all_ctrls)){
      ctrl_layer_name <- paste(biome.names[j],"_Ctrl_samples", sep="")
      ctrl_write_file_name <- paste(write_f.path, "USA_", biome.names[j],"_Ctrl_V2.shp", sep="")
      #all_ctrls2 <- buffer(all_ctrls, width=0, dissolve=FALSE)
      all_ctrls2 <- aggregate(all_ctrls, dissolve=T)
      shapefile(all_ctrls, file=ctrl_write_file_name, overwrite=TRUE)
      }
    if(!is.null(all_unmatched_pas)){
      unmatch_pa_layer_name <- paste(biome.names[j],"_unmatched_PA_samples", sep="")
      unmatch_pa_write_file_name <- paste(write_f.path, "USA_", biome.names[j],"_unmatched_PA_V2.shp", sep="")
      #all_unmatched_pas2 <- buffer(all_unmatched_pas, width=0, dissolve=FALSE)
      all_unmatched_pas2 <- aggregate(all_unmatched_pas, dissolve=TRUE)
      shapefile(all_unmatched_pas2, file=unmatch_pa_write_file_name, overwrite=TRUE)
      }
  }
}

