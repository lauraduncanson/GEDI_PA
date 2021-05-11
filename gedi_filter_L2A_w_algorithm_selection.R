################################################################################
### filter L2A data with correct algorithm setting selection from L4A
### then match L2B shots and keep only "good quality data"
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
length(IDs)
#
#IDs <- c("FSM","MCO","KNA","LIE","SYC","VCT","GRD","DMA","SGP","KIR","CPV","COM","MUS","BHR","BRN","DJI","TTO","WSM")
#IDs <- c("AND","LCA","TON","STP","PLW","ATG","VUT","MLT","BRB","TLS","BDI","JAM","GMB","GNQ","CYP","SLV","LBN","MNE")
#IDs <- c("QAT","BLZ","SLB","TGO","SLE","TWN","BTN","LBR","HTI","KWT","BEN","LSO","IRQ","BHS","RWA","FJI","GNB","SWZ")
#IDs <- c("SVN","PAN","ISR","DOM","ARM","BGD","ALB","MDA","MAR","ZMB","CRI","MKD","IRL","LKA","GAB","GHA","ERI","POL")
#IDs <- c("TKM","BWA","AFG","GTM","ECU","NIC","TZA","CHL","PER","UZB","MLI","BOL","AGO","ESP","CHE","MRT","DEU","NAM")
#IDs <- c("NLD","ARE","HND","KOR","HRV","MWI","MMR","ROU","VEN","ZWE","TUR","NER","TCD","ZAF","SDN","EGY","GIN","SUR")
#IDs <- c("BIH","GUY","GEO","NPL","KHM","AZE","ITA","IRN","PRY","YEM","PNG","CUB","COG","BFA","UGA","SRB","JOR","KEN")
#IDs <- c("BEL","BLR","SEN","CIV","PRT","TUN","UKR","SVK","MYS","CMR","AUT","TJK","LAO","HUN","BGR","NGA","VNM","CAF")
#IDs <- c("POL","BWA","TKM","BOL","AGO","AFG","VNM","URY","OMN","GBR","JPN","CAF","DEU","NAM","IND","CHL","PER","FRA")
#IDs <- c("ESP","MRT","ZMB","TJK","LAO","HUN","BGR","NGA","KGZ","CZE","GRC","SSD","UZB","MLI","PHL","THA","SAU","NZL")
#IDs <- c("COL","MEX",DZA","BRA","ARG","AUS","MNG") 
IDs <- c("KAZ")
#
for(v in 1:length(IDs)){
  id <- IDs[v]
  print(id)
  #
  boundary  <- readOGR(paste(countries.folder, id, ".shp", sep=""))
  #plot(boundary, main=id)
  path.l2a  <- paste(f.path,"WDPA_gedi_l2a/",id,"/",sep="")
  path.l4a  <- paste(f.path,"WDPA_gedi_l4a_clean/",id,"/",sep="")
  path.out  <- paste(f.path,"WDPA_gedi_l2a_clean/",id,"/",sep="")
  f.l2a  <- length(list.files(path.l2a))
  f.l4a  <- length(list.files(path.l4a))
  #
  list.files.l2a <- list.files(path.l2a)
  list.files.l4a <- list.files(path.l4a)
  print(list.files.l2a)
  setwd(path.out)
  #####################################
  #
  #####################################
  if(length(list.files.l2a) == 0){
    print(paste(id,": empty directory", sep=""))
  } else {
    for(i in 1:length(list.files.l2a))
    {
      file.l2a <- list.files.l2a[i]
      east <- strsplit(file.l2a,"_")[[1]][3]
      nort <- strsplit(strsplit(file.l2a,"_")[[1]][4],".csv")
      file.l4a <- paste(id,"_l4a_",east,"_",nort,"_clean_q1.csv",sep="")
      print(file.l2a)
      if(!file.exists(paste(path.l4a,file.l4a,sep=""))){
        print(paste("no corresponding L4A file found"))
        #####################################
        # read in L2A data (1º tiles)
        #####################################
        data.l2a <- read.csv(paste(path.l2a,file.l2a,sep=""))
        if(nrow(data.l2a) > 0){
          data.l2a.select <- data.l2a[,c("shot_number","lat_lowestmode_a1","lon_lowestmode_a1",
                                         "rh_a1_025","rh_a1_050","rh_a1_075","rh_a1_090","rh_a1_098",
                                         "quality_flag_a1","sensitivity_a1")]
          selected_algorithm <- rep(1,nrow(data.l2a.select))
          data.l2a.select <- cbind(data.l2a.select, selected_algorithm)
          colnames(data.l2a.select) <- c("shot_number","lat_lowestmode","lon_lowestmode",
                                         "rh_025","rh_050","rh_075","rh_090","rh_098",
                                         "quality_flag","sensitivity","selected_algorithm")
          print(paste("algorithm",unique(data.l2a.select[,"selected_algorithm"]),sep=" "))
          print(paste("#shots input:",nrow(data.l2a.select),sep=" "))
          rm(data.l2a)
          ####################################
          # filter on quality==1 and sensitivity>0.95
          ####################################
          data.good1 <- data.l2a.select[data.l2a.select$quality_flag == 1,]
          data.good2 <- data.good1[data.good1$sensitivity >= 0.95,]
          if(nrow(data.good2) > 0){
            ####################################
            # check if GEDI shots are within country boundary
            ####################################
            gedi_coords <- data.good2[,c("lon_lowestmode","lat_lowestmode")]
            gedi_pts <- SpatialPointsDataFrame(coords=gedi_coords, data=data.good2, proj4string=CRS(proj4string(boundary)))
            overlap <- gedi_pts[boundary,]
            print(paste("#shots output:",nrow(overlap),sep=" "))
            if(nrow(overlap) == 0){
              print("NO OVERLAPPING SHOTS FOUND")
            } else {
              data.l2a.clean <- as.data.frame(overlap)
              data.l2a.clean <- data.l2a.clean[,c("shot_number","lat_lowestmode","lon_lowestmode",
                                                  "rh_025","rh_050","rh_075","rh_090","rh_098",
                                                  "quality_flag","sensitivity","selected_algorithm")]
              file.out2 <- paste(path.out, strsplit(file.l2a, ".csv"), "_clean_q1sen95_rev.csv", sep="")
              write.csv(data.l2a.clean, file = file.out2, row.names = F)
              rm(data.l2a.select, data.good1, data.good2, gedi_coords, gedi_pts, overlap, data.l2a.clean)
            }
          }
        }
      } else {
        #####################################
        # read in algorithm setting selection from L4A (1º tiles)
        #####################################
        print(paste("matching shots from:",file.l4a,sep=" "))
        data.l4a <- read.csv(paste(path.l4a,file.l4a,sep=""))
        if(nrow(data.l4a) > 0){
          data.l4a.algrtm <- data.l4a[,c("shot_number","selected_algorithm")]
        }
        rm(data.l4a)
        #####################################
        # read in L2A data (1º tiles) and merge with L4A algorithm information
        #####################################
        data.l2a <- read.csv(paste(path.l2a,file.l2a,sep=""))
        if(nrow(data.l2a) > 0){
          data.l2a.select <- data.l2a[,c("shot_number","lat_lowestmode_a1","lon_lowestmode_a1",
                                         "rh_a1_025","rh_a1_050","rh_a1_075","rh_a1_090","rh_a1_098",
                                         "quality_flag_a1","sensitivity_a1",
                                         "lat_lowestmode_a2","lon_lowestmode_a2",
                                         "rh_a2_025","rh_a2_050","rh_a2_075","rh_a2_090","rh_a2_098",
                                         "quality_flag_a2","sensitivity_a2",
                                         "lat_lowestmode_a3","lon_lowestmode_a3",
                                         "rh_a3_025","rh_a3_050","rh_a3_075","rh_a3_090","rh_a3_098",
                                         "quality_flag_a3","sensitivity_a3",
                                         "lat_lowestmode_a4","lon_lowestmode_a4",
                                         "rh_a4_025","rh_a4_050","rh_a4_075","rh_a4_090","rh_a4_098",
                                         "quality_flag_a4","sensitivity_a4",
                                         "lat_lowestmode_a5","lon_lowestmode_a5",
                                         "rh_a5_025","rh_a5_050","rh_a5_075","rh_a5_090","rh_a5_098",
                                         "quality_flag_a5","sensitivity_a5",
                                         "lat_lowestmode_a6","lon_lowestmode_a6",
                                         "rh_a6_025","rh_a6_050","rh_a6_075","rh_a6_090","rh_a6_098",
                                         "quality_flag_a6","sensitivity_a6")]
          rm(data.l2a)
          #####################################
          # merge shots in L4A with L2A shots (replace "selected_algorithm" NAs with default 1)
          #####################################
          l2a.l4a.merge <- merge(data.l2a.select, data.l4a.algrtm, by="shot_number", all.x=T)
          l2a.l4a.merge[which(is.na(l2a.l4a.merge[,"selected_algorithm"])),"selected_algorithm"] <- 1
          l2a.l4a.merge[which(l2a.l4a.merge[,"selected_algorithm"] > 6),"selected_algorithm"] <- 1
          #
          shot_number    <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          lat_lowestmode <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          lon_lowestmode <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          rh_025         <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          rh_050         <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          rh_075         <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          rh_090         <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          rh_098         <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          quality_flag   <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          sensitivity    <- vector(mode="numeric", length=nrow(l2a.l4a.merge))
          selected_algorithm <- l2a.l4a.merge[,"selected_algorithm"]
          #
          for(k in 1:length(selected_algorithm)){
            a <- selected_algorithm[k]
            shot_number[k]    <- l2a.l4a.merge[k, "shot_number"]
            lat_lowestmode[k] <- l2a.l4a.merge[k, paste("lat_lowestmode_a", a, sep="")]
            lon_lowestmode[k] <- l2a.l4a.merge[k, paste("lon_lowestmode_a", a, sep="")]
            rh_025[k]         <- l2a.l4a.merge[k, paste("rh_a", a, "_025",sep="")]
            rh_050[k]         <- l2a.l4a.merge[k, paste("rh_a", a, "_050",sep="")]
            rh_075[k]         <- l2a.l4a.merge[k, paste("rh_a", a, "_075",sep="")]
            rh_090[k]         <- l2a.l4a.merge[k, paste("rh_a", a, "_090",sep="")]
            rh_098[k]         <- l2a.l4a.merge[k, paste("rh_a", a, "_098",sep="")]
            quality_flag[k]   <- l2a.l4a.merge[k, paste("quality_flag_a", a, sep="")]
            sensitivity[k]    <- l2a.l4a.merge[k, paste("sensitivity_a", a, sep="")]
          }
          data.l2a.clean <- cbind(shot_number, lat_lowestmode, lon_lowestmode,
                                  rh_025, rh_050, rh_075, rh_090, rh_098,
                                  quality_flag, sensitivity, selected_algorithm)
          print(paste("#shots input:", nrow(data.l2a.clean), sep=" "))
          rm(l2a.l4a.merge, shot_number, lat_lowestmode, lon_lowestmode,
             rh_025, rh_050, rh_075, rh_090, rh_098, quality_flag, sensitivity, selected_algorithm)
          ####################################
          # filter on quality==1 and sensitivity>0.95
          ####################################
          data.good1 <- data.l2a.clean[data.l2a.clean[,"quality_flag"] == 1, ]
          data.good2 <- data.good1[data.good1[,"sensitivity"] >= 0.95, ]
          if(nrow(data.good2) > 0){
            ####################################
            # check if GEDI shots are within country boundary
            ####################################
            gedi_coords <- data.good2[,c("lon_lowestmode","lat_lowestmode")]
            gedi_pts <- SpatialPointsDataFrame(coords=gedi_coords, data=as.data.frame(data.good2), proj4string=CRS(proj4string(boundary)))
            overlap <- gedi_pts[boundary,]
            print(paste("#shots output:",nrow(overlap),sep=" "))
            if(nrow(overlap) == 0){
              print("NO OVERLAPPING SHOTS FOUND")
            } else {
              data.l2a.out <- as.data.frame(overlap)
              data.l2a.out <- data.l2a.out[,c("shot_number","lat_lowestmode","lon_lowestmode",
                                              "rh_025","rh_050","rh_075","rh_090","rh_098",
                                              "quality_flag","sensitivity","selected_algorithm")]
              file.out2 <- paste(path.out, strsplit(file.l2a, ".csv"), "_clean_q1sen95_rev.csv", sep="")
              write.csv(data.l2a.out, file = file.out2, row.names = F)
              rm(data.l2a.clean, data.good1, data.good2, gedi_coords, gedi_pts, overlap, data.l2a.out)
            }
          }
        }
      }
      unlink(paste(path.l2a, file.l2a, sep="")) ##delete l2a file already processed
    }
    f.l2a_clean  <- length(list.files(path.out))
    setwd(paste(f.path,"WDPA_gedi_l2a/",id,"/",sep=""))
    sink(paste("filtered",f.l2a_clean,"of",f.l2a,".txt",sep=""))
  }
  sink()
} 

