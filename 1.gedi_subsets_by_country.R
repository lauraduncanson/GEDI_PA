###############################################################################
## GEDI data subsetting country-by-country in 1º tiles w/ start & end dates
##############################################################################
## outputs .py files country-by-country to be run w/ issgedi_umd on UMD cluster
###############################################################################
library(sf)
library(sp)
library(rgeos)
library(rgdal)
##
#f.path <- "/Users/veronika/leitoldv/"
f.path <- "/gpfs/data1/duncansongp/leitoldv/"
countries.folder <- paste(f.path,"WDPA_countries/shp/", sep="")
WDPA_table <- read.csv(paste(f.path,"WDPA_countries_table3.csv",sep=""))
IDs <- as.character(WDPA_table[,"ISO3"])
##
#for(i in 1:length(IDs)){
#  dir.create(paste(f.path,"WDPA_gedi_",GEDI_data,"/",IDs[i],sep=""))
#}
##
################################################################################
## l2a
################################################################################
startT <- "--start_time 2019-10-03"  ### day of year 275
endT <- "--end_time 2020-02-19"  ### day of year 050
GEDI_data <- "l2a"
GEDI_folder <- paste(f.path,"WDPA_gedi_",GEDI_data,"/",sep="")
#
for(i in 1:length(IDs)){
  #
  iso3     <- IDs[i]
  boundary <- readOGR(paste(countries.folder, iso3, ".shp", sep=""))
  plot(boundary, main=iso3)
  # set the corners of ROI in geographic coordinates
  lon_min <-   floor(bbox(boundary)["x","min"])
  lon_max <- ceiling(bbox(boundary)["x","max"])
  lat_min <-   floor(bbox(boundary)["y","min"])
  lat_max <- ceiling(bbox(boundary)["y","max"])
  # set your ROI name and GEDI data type
  roi_name <- iso3
  outfilePY <- paste(f.path,"WDPA_countries/py_",GEDI_data,"_newdata1/",iso3,".py",sep="")
  sink(outfilePY)
  # loop through latitudes
  for(lat in seq(lat_min, lat_max, 1)){
    # loop through longitudes
    for(lon in seq(lon_min, lon_max, 1)){
      # make tile name using bottom left corner coordinate, removing negative sign and adding cardinal direction
      if(lon >= 0){lon_label <- "E_"} else {lon_label <- "W_"}
      if(lat >= 0){lat_label <- "N.csv"} else {lat_label <- "S.csv"}
      tile <- paste(abs(lon),lon_label,abs(lat),lat_label,sep="")
      # make file name (and set a path if desired) based on GEDI data product and tile
      file_out <- paste(roi_name,"_",GEDI_data,"_",tile,sep="")
      # make bbox coordinates from lon and lat
      xmin <- lon
      xmax <- lon + 1
      ymin <- lat
      ymax <- lat + 1
      ##########
      ## check if latitude is outside GEDI data span (<55ºS or >55ºN)
      ## only use tiles that are within GEDI data range
      ##########
      if(abs(lat)<55){
        ##########
        ## check if at least one tile corner falls within country boundary
        ## OR if the entire country falls within the tile polygon
        ## only use tiles that have overlapping area with country boundary
        ##########
        tile.x  <- c(xmin, xmax, xmax, xmin, xmin)
        tile.y  <- c(ymin, ymin, ymax, ymax, ymin)
        tile.xy <- cbind(tile.x, tile.y)
        tile.p  <- Polygon(tile.xy)
        tile.ps <- Polygons(list(tile.p),1)
        tile.sps<- SpatialPolygons(list(tile.ps))
        proj4string(tile.sps) <- CRS(proj4string(boundary))
        plot(tile.sps, add=T)
        #
        test <- over(tile.sps, boundary)
        #
        if( !is.na(test[,1]) ){ #### if tile and boundary overlap
          plot(tile.sps, add=T, col="red")
          plot(boundary, main=iso3, add=T)
          py <- "issgedi_export_subset.py"
          r <- "--root_path /gpfs/data1/duncansongp/gedi_data/"
          d <- paste("-d /gpfs/data1/duncansongp/leitoldv/txt/",GEDI_data,"_vars.txt", sep="")
          b <- paste("-b", xmin, ymax, xmax, ymin, sep=" ")
          output <- paste(f.path,"WDPA_gedi_",GEDI_data,"/",iso3,"/",file_out,sep="")
          cat(paste(py, r, d, b, startT, endT, output, sep=" "))
          cat("\n")
        }
      }
    }
  }
  sink()
}
##
################################################################################
## l2b
################################################################################
startT <- "--start_time 2019-10-03"  ### day of year 275
endT <- "--end_time 2020-02-19"  ### day of year 050
GEDI_data <- "l2b"
GEDI_folder <- paste(f.path,"WDPA_gedi_",GEDI_data,"/",sep="")
#
for(i in 1:length(IDs)){
  #i <- 190 ##"USA"
  iso3     <- IDs[i]
  boundary <- readOGR(paste(countries.folder, iso3, ".shp", sep=""))
  plot(boundary, main=iso3)
  # set the corners of ROI in geographic coordinates
  lon_min <-   floor(bbox(boundary)["x","min"])
  lon_max <- ceiling(bbox(boundary)["x","max"])
  lat_min <-   floor(bbox(boundary)["y","min"])
  lat_max <- ceiling(bbox(boundary)["y","max"])
  # set your ROI name and GEDI data type
  roi_name <- iso3
  outfilePY <- paste(f.path,"WDPA_countries/py_",GEDI_data,"_newdata1/",iso3,".py",sep="")
  sink(outfilePY)
  # loop through latitudes
  for(lat in seq(lat_min, lat_max, 1)){
    # loop through longitudes
    for(lon in seq(lon_min, lon_max, 1)){
      # make tile name using bottom left corner coordinate, removing negative sign and adding cardinal direction
      if(lon >= 0){lon_label <- "E_"} else {lon_label <- "W_"}
      if(lat >= 0){lat_label <- "N.csv"} else {lat_label <- "S.csv"}
      tile <- paste(abs(lon),lon_label,abs(lat),lat_label,sep="")
      # make file name (and set a path if desired) based on GEDI data product and tile
      file_out <- paste(roi_name,"_",GEDI_data,"_",tile,sep="")
      # make bbox coordinates from lon and lat
      xmin <- lon
      xmax <- lon + 1
      ymin <- lat
      ymax <- lat + 1
      ##########
      ## check if latitude is outside GEDI data span (<55ºS or >55ºN)
      ## only use tiles that are within GEDI data range
      ##########
      if(abs(lat)<55){
        ##########
        ## check if at least one tile corner falls within country boundary
        ## OR if the entire country falls within the tile polygon
        ## only use tiles that have overlapping area with country boundary
        ##########
        tile.x  <- c(xmin, xmax, xmax, xmin, xmin)
        tile.y  <- c(ymin, ymin, ymax, ymax, ymin)
        tile.xy <- cbind(tile.x, tile.y)
        tile.p  <- Polygon(tile.xy)
        tile.ps <- Polygons(list(tile.p),1)
        tile.sps<- SpatialPolygons(list(tile.ps))
        proj4string(tile.sps) <- CRS(proj4string(boundary))
        plot(tile.sps, add=T)
        #
        test <- over(tile.sps, boundary)
        #
        if( !is.na(test[,1]) ){ #### if tile and boundary overlap
          plot(tile.sps, add=T, col="red")
          plot(boundary, main=iso3, add=T)
          py <- "issgedi_export_subset.py"
          r <- "--root_path /gpfs/data1/duncansongp/gedi_data/"
          d <- paste("-d /gpfs/data1/duncansongp/leitoldv/txt/",GEDI_data,"_vars.txt", sep="")
          b <- paste("-b", xmin, ymax, xmax, ymin, sep=" ")
          etc <- "--product GEDI02 --level B --pgeversion 1 --type 02"
          output <- paste(f.path,"WDPA_gedi_",GEDI_data,"/",iso3,"/",file_out,sep="")
          cat(paste(py, r, d, b, etc, startT, endT, output, sep=" "))
          cat("\n")
        }
      }
    }
  }
  sink()
}




