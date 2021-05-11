################################################################################
################################################################################
################################################################################
#######################################################
# set up directories where GEDI L4A subsets will be saved
#######################################################
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
WDPA_table <- read.csv(paste(f.path,"WDPA_countries_table3.csv",sep=""))
IDs <- as.character(WDPA_table[,"ISO3"])
for(i in 1:length(IDs)){dir.create(paste(f.path,"WDPA_gedi_l4a_clean/",IDs[i],sep=""))}
################################################################################
### filter L4A data 
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
#length(IDs)

for(v in 1:length(IDs)){
  id <- IDs[v]
  print(id)
  #
  boundary  <- readOGR(paste(countries.folder, id, ".shp", sep=""))
  #plot(boundary, main=id)
  path.l4a  <- paste(f.path,"WDPA_gedi_l4a/",id,"/",sep="")
  path.out  <- paste(f.path,"WDPA_gedi_l4a_clean/",id,"/",sep="")
  f.l4a  <- length(list.files(path.l4a))
  #
  list.files.l4a <- list.files(path.l4a)
  setwd(path.out)
  #####################################
  # filter l4a data subsets (1º tiles)
  #####################################
  if(length(list.files.l4a) == 0){
    print(paste(id,": empty directory", sep=""))
  } else {
    for(i in 1:length(list.files.l4a))
    {
      file.l4a <- list.files.l4a[i]
      east <- strsplit(file.l4a,"_")[[1]][3]
      nort <- strsplit(strsplit(file.l4a,"_")[[1]][4],".csv")
      print(file.l4a) #print(file.l2b)
      #
      data.l4a <- read.csv(paste(path.l4a,file.l4a,sep=""))
      if(nrow(data.l4a) > 0){
        ####################################
        # check if GEDI shots are within country boundary
        ####################################
        gedi_coords <- data.l4a[,c("lon_lowestmode","lat_lowestmode")]
        gedi_pts <- SpatialPointsDataFrame(coords=gedi_coords, data=data.l4a, proj4string=CRS(proj4string(boundary)))
        #plot(gedi_pts, add=T, col="black", pch=".")
        overlap <- gedi_pts[boundary,]
        #plot(overlap, add=T, col="blue", pch=".")
        if(nrow(overlap) == 0){
          print("NO OVERLAPPING SHOTS FOUND")
        } else {
          data.good2 <- overlap[overlap$l2_quality_flag == 1 | overlap$l4_quality_flag == 1,]
          #data.good2 <- data.good1[data.good1$sensitivity >= 0.95,]
          #plot(data.good2, add=T, col="red", pch=".")
          if(nrow(data.good2) > 0){
            data.l4a.clean <- as.data.frame(data.good2)
            ############
            cols.to.keep <- c("shot_number","lat_lowestmode","lon_lowestmode","agbd","agbd_se",
                              "agbd_t","agbd_t_se","agbd_pi_upper","agbd_pi_lower",
                              "l2_quality_flag", "l4_quality_flag", "sensitivity",
                              "predict_stratum","predictor_limit_flag","response_limit_flag",
                              "selected_algorithm","selected_mode","selected_mode_flag")
            data.l4a.clean <- data.l4a.clean[,cols.to.keep]
            ############
            #save filtered data to new .csv file
            file.out <- paste(path.out,id,"_l4a_",east,"_",nort,"_clean_q1.csv",sep="")
            print(file.out)
            write.csv(data.l4a.clean, file = file.out, row.names = F)
          }
          unlink(paste(path.l4a, file.l4a, sep="")) ##delete l4a file already processed
        }
      }
    }
    f.l4a_clean  <- length(list.files(path.out))
    setwd(path.l4a)
    sink(paste("filtered",f.l4a_clean,"of",f.l4a,".txt",sep=""))
  }
  sink()
}

