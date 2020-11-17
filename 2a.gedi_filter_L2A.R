###############################################################################
####### filtering subsets of GEDI L2A for quality (=1) and sensitivity (>0.95)
###############################################################################
library(sf)
library(sp)
library(rgeos)
library(rgdal)

#f.path <- "/Users/veronika/leitoldv/"
f.path <- "/gpfs/data1/duncansongp/leitoldv/"
countries.folder <- paste(f.path,"WDPA_countries/shp/", sep="")
WDPA_table <- read.csv(paste(f.path,"WDPA_countries_table3.csv",sep=""))
IDs <- as.character(WDPA_table[,"ISO3"])
##
# create a folder for each country in WDPA_gedi_l2a_clean directory
#dir.create(paste(f.path,"WDPA_gedi_l2a_clean",sep=""))
#for(i in 1:length(IDs)){
#  dir.create(paste(f.path,"WDPA_gedi_l2a_clean/",IDs[i],sep=""))
#}
#
#
for(v in 1:length(IDs)){
  id <- IDs[v]
  #
  path.in  <- paste(f.path,"WDPA_gedi_l2a/",id,"/",sep="")
  path.out <- paste(f.path,"WDPA_gedi_l2a_clean/",id,"/",sep="")
  f.in  <- length(list.files(path.in)); f.in
  #
  setwd(path.in)
  if(f.in == 0){
    sink("emptyDIR.txt")
  } else {
    boundary <- readOGR(paste(countries.folder, id, ".shp", sep=""))
    plot(boundary, main=id)
    for(i in 1:length(dir()))
    {
      file.in <- dir()[i]
      print(file.in)
      data.in <- read.csv(file.in)
      if(nrow(data.in) > 0){
        ### check which GEDI shots are within country boundary
        gedi_coords <- data.in[,c("lon_lowestmode","lat_lowestmode")]
        gedi_pts <- SpatialPointsDataFrame(coords=gedi_coords, data=data.in, proj4string=CRS(proj4string(boundary)))
          plot(gedi_pts, add=T, col="black", pch=".")
        overlap <- gedi_pts[boundary,]
          plot(overlap, add=T, col="blue", pch=".")
        if(nrow(overlap) > 0){
          #overlap <- data.in
          data.good1 <- overlap[overlap$quality_flag == 1,]
          data.good2 <- data.good1[data.good1$sensitivity >= 0.95,]
            plot(data.good2, add=T, col="red", pch=".")
          if(nrow(data.good2) > 0){
            #file.out1 <- paste(path.out, strsplit(file.in, ".csv"), "_clean.csv", sep="")
            file.out2 <- paste(path.out, strsplit(file.in, ".csv"), "_clean_newdata1.csv", sep="")
            write.csv(data.good2, file = file.out2, row.names = F)
          }
        }
      }
    }
  }
  
  f.out <- length(list.files(path.out)); f.out
  unlink(dir())
  sink(paste("filtered",f.out,"of",f.in,".txt",sep=""))
  
  sink()
}
