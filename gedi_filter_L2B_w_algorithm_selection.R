################################################################################
### match L2B shots to L2A filtered shots and keep only "good quality data"
### + calculate cover and pai based on algorithm selection referenced from L4A
################################################################################
library(sf)
library(sp)
library(rgeos)
library(rgdal)

#f.path <- "/Users/veronika/leitoldv/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
countries.folder <- paste(f.path,"WDPA_countries/shp/", sep="")
WDPA_countries <- read.csv(paste(f.path,"WDPA_countries_table3.csv",sep=""))
IDs <- as.character(WDPA_countries[,"ISO3"])

#IDs <- c("AUS")

for(v in 1:length(IDs)){
  id <- IDs[v]
  print(id)
  #
  path.l2a  <- paste(f.path,"WDPA_gedi_l2a_clean/",id,"/",sep="")
  path.l2b  <- paste(f.path,"WDPA_gedi_l2b/",id,"/",sep="")
  path.out  <- paste(f.path,"WDPA_gedi_l2a+l2b_clean2/",id,"/",sep="")
  f.l2a  <- length(list.files(path.l2a))
  f.l2b  <- length(list.files(path.l2b))
  #
  list.files.l2a <- list.files(path.l2a)
  setwd(path.out)
  #####################################
  # read in filtered l2a files (1º tiles)
  #####################################
  if(length(list.files.l2a) == 0){
    print(paste(id,": empty directory", sep=""))
  } else {
    for(i in 1:length(list.files.l2a))
    {
      file.l2a <- list.files.l2a[i]
      east <- strsplit(file.l2a,"_")[[1]][3]
      nort <- strsplit(strsplit(file.l2a,"_")[[1]][4],".csv")
      file.l2b <- paste(id,"_l2b_",east,"_",nort,".csv",sep="")
      print(file.l2a)
      print(file.l2b)
      #
      data.l2a.clean <- read.csv(paste(path.l2a,file.l2a,sep=""))
      if(nrow(data.l2a.clean) > 0){
      ####################################
      # bring in L2B data for same 1º tile and match to l2a by shot number
      ####################################
      data.l2b <- read.csv(paste(path.l2b, file.l2b, sep=""))
      nrow(data.l2b)
      clean.l2a.l2b <- merge(data.l2a.clean, data.l2b, by="shot_number", all.x=T)
      nrow(clean.l2a.l2b)
      if(nrow(clean.l2a.l2b) == nrow(data.l2a.clean)) { print("row numbers match")}
      #####################################
      # loop through shots and select pgap_theta_<n> based on the selected_algorithm value
      #####################################
      pgap_theta <- c()
      for(k in 1:nrow(clean.l2a.l2b)){
        algorithm <- clean.l2a.l2b[k,"selected_algorithm"]
        pgap_theta <- c(pgap_theta, clean.l2a.l2b[k, paste("pgap_theta_a",algorithm,sep="")])
      }
      clean.l2a.l2b <- cbind(clean.l2a.l2b, pgap_theta)
      cols.to.keep <- c("shot_number", "lon_lowestmode.x", "lat_lowestmode.x",
                        "rh_025", "rh_050", "rh_075", "rh_090", "rh_098", 
                        "quality_flag", "l2b_quality_flag", "sensitivity.x",
                        "selected_algorithm", "local_beam_elevation",
                        "pgap_theta", "fhd_normal", "cover", "pai")
      clean.l2a.l2b <- clean.l2a.l2b[,cols.to.keep]
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "lon_lowestmode.x")] <- "lon_lowestmode"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "lat_lowestmode.x")] <- "lat_lowestmode"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "quality_flag")]     <- "l2a_quality_flag"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "sensitivity.x")]    <- "sensitivity"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "cover")]            <- "cover_default"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "pai")]              <- "pai_default"
      colnames(clean.l2a.l2b)
      #
      rm(data.l2a.clean, data.l2b)
      ##############################################
      ## calculate cover and pai values based on algorithm selected pgap_theta_<n>
      ##############################################
      rossg = 0.5
      omega = 1
      cos_zenith = abs(sin(clean.l2a.l2b[,"local_beam_elevation"]))
      cover = cos_zenith * (1.0 - clean.l2a.l2b[,"pgap_theta"])
      pai = -(1.0 / (rossg * omega)) * log(clean.l2a.l2b[,"pgap_theta"]) * cos_zenith
      clean.l2a.l2b <- cbind(clean.l2a.l2b, cover, pai)
      ##############################################
      ## save clean.l2a.l2b data to .csv file
      ##############################################
      file.out <- paste(path.out,id,"_l2a+l2b_",east,"_",nort,"_clean_q1sen95_rev.csv",sep="")
      write.csv(clean.l2a.l2b, file = file.out, row.names = F)
      }
      unlink(paste(path.l2a, file.l2a, sep="")) ##delete l2a file already processed
      unlink(paste(path.l2b, file.l2b, sep="")) ##delete l2b file already processed
    }
    l2b.nomatch <- length(list.files(path.l2b))
    l2b.matched <- f.l2b - l2b.nomatch
    setwd(path.l2b)
    sink(paste("matched",l2b.matched,"of",f.l2b,".txt",sep=""))
  }
  sink()
}

