#This script explores the relationship between protected age and the mean difference in AGBD (b/t PA and control) by forest/non-forest types.
# essentially the significnce by PA age is tested by forest/non-forest dominant PA types, and the plot is for Fig S3 in the manuscript. 


packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","spatialEco","bit64","randomForest", "modelr")
package.check <- lapply(packages, FUN = function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/csv"
# world_region_prj <- world_region %>%  
#   projectRaster(.,crs="+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", method="bilinear")  
# writeRaster(world_region_prj,paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_4326.tif",sep=""))
# nras <- t <- raster(xmn=-180, ymn=-90, xmx=180, ymx=90, resolution=1000,
#                     crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')


# matchedFolder <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results"
# matchedISO <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv")
# matchedISO <- matchedISO[-142,]
# # Matched cells compilation by continent 
# rm(contiMatched)
# contiDF <- matchedISO %>% dplyr::filter(continent=="As")
# contiMatched <- data.frame()
# for (iso3 in contiDF$iso3){
#   cat(iso3,match(iso3,unique(contiDF$iso3)),"out of ",length(unique(contiDF$iso3)),"\n")
#   iso3Folder <- list.files(paste(matchedFolder,"/",iso3,"_wk18",sep=""), full.names=TRUE)
#   for (f in iso3Folder){
#     print(f)
#     m <- readRDS(f)
#     if(nrow(m)>0 && !is.null(m)){
#       ms <- m %>% dplyr::select(-c(propensity_score,matched)) %>% mutate(iso3=iso3, continent=unique(contiDF$continent))
#       contiMatched <- rbind(contiMatched,ms)
#       print(dim(contiMatched))
#     }
#   }
# }
# 
# saveRDS(contiMatched,paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/",contiMatched$continent[1],"_results.rds",sep=""))

# Sampling and dowload tif files 
packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","bit64","randomForest", "modelr")
package.check <- lapply(packages, FUN = function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
# tzaDF <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/Af_results.rds") %>% dplyr::filter(iso3=="TZA")
eurDF <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/US_results.rds") %>%
  rbind(readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/SA_results.rds")) %>%
  rbind(readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/Eu_results.rds")) %>%
  rbind(readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/Af_results.rds")) %>%
  rbind(readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/Au_results.rds")) %>%
  rbind(readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/As_results.rds"))
tileDF <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/Aboveground_live_woody_biomass_density.csv") %>%
  dplyr::select(tile_id, download)

# isofiles <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/WDPA_polygons", full.names = TRUE)
# wdpadf <- data.frame()
# for (f in isofiles){
#   print(f)
#   tf <- readRDS(f)@data %>% dplyr::select(WDPAID, STATUS_YR, ISO3)
#   wdpadf <- rbind(tf, wdpadf)
# }
# #wdpfdf2 <- readOGR("/gpfs/data1/duncansongp/amberliang/trends.Earth/WDPA_Apr2020-shapefile/WDPA_Apr2020-shapefile-polygons.shp")
# analyzedPAs <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/analyzedPAs.rds")
# countryContinent <-read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>%
#   rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
# paAge <- wdpadf %>%
#   dplyr::mutate(analyzedpas=ifelse(WDPAID %in% analyzedPAs, 1, 0)) %>% 
#   left_join(countryContinent,by=c("ISO3"="iso3")) %>%
#   # dplyr::select(WDPAID, STATUS_YR,continent,ISO3) %>% 
#   dplyr::filter(!is.na(STATUS_YR)) %>% dplyr::filter(STATUS_YR!=0) %>%
#   mutate(paAge=2021-STATUS_YR) %>% mutate(before2000=ifelse(STATUS_YR<=2000, 1, 0)) %>% 
#   mutate(before2000_2=ifelse(STATUS_YR<2000, 1, 0)) %>% 
#   mutate(after2000=ifelse(paAge<=20, 1, 0))
# # saveRDS(paAge,"/gpfs/data1/duncansongp/GEDI_global_PA/csv/paAge3.rds")
paAge <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/csv/paAge3.rds")


set.seed(1231)
sample <- eurDF %>%
  group_by(continent, wwfbiom,pa_id, status) %>% #dplyr::summarise(count=length(UID)) %>% data.frame()
  sample_n(.,ifelse(length(min_temp)>10,10, length(min_temp))) %>% ungroup()
sample <- tzaDF %>%
  group_by(continent, wwfbiom, status) %>% #dplyr::summarise(count=length(UID)) %>%
  sample_n(.,ifelse(length(min_temp)>500,500, length(min_temp))) %>%
  ungroup()
# sample[sample$status==FALSE,]$pa_id %>% unique() %>% length()
# dplyr::summarise(count=length(UID))
sampleTile <- sample %>% mutate(tileLat=round_any(lat, 10, f = ceiling), tileLon=round_any(lon, 10,f=floor)) %>% 
  mutate(tileLat2 =ifelse(tileLat>=0,"N","S"), tileLon2=ifelse(tileLon>=0,"E","W")) %>% 
  mutate(tileLon=abs(tileLon)) %>% 
  mutate(tileLat=abs(tileLat)) %>% 
  mutate(tileLat=ifelse(nchar(tileLat)<2, paste("0",tileLat,sep=""),tileLat)) %>% 
  mutate(tileLon=ifelse(nchar(tileLon)<3, paste("0",tileLon,sep=""),tileLon)) %>% 
  mutate(tileLon=ifelse(nchar(tileLon)<3, paste("0",tileLon,sep=""),tileLon)) %>% 
  mutate(tile=paste(tileLat, tileLat2, "_",tileLon, tileLon2,sep="")) %>% 
  left_join(tileDF,by=c("tile"="tile_id")) %>%mutate(pa_id=as.numeric(pa_id)) %>% 
  left_join(paAge, by=c("pa_id"="WDPAID"))
sampleTileSP <- sampleTile %>% SpatialPointsDataFrame(data=., coords=.[,c("lon","lat")],
                                                      proj4string=CRS("+init=epsg:4326")) %>% 
  spTransform(., "+init=epsg:6933")
# sampleTile$tile %>% unique() #%>% length()
# sampleTile$download %>% is.na() %>% sum
# world_region_prj <- raster(paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_4326.tif",sep=""))
world_region <- raster(paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_6933.tif",sep=""))
projection(world_region) <- sp::CRS(paste("+init=epsg:",6933,sep=""))

getmode <- function(v,na.rm=TRUE) {
  uniqv <- unique(v)
  if(na.rm==TRUE){
    uniqv <- unique(na.omit(v))
  }
  return(uniqv[which.max((tabulate(match(v,uniqv))))])
}

wwfbiomConvert <- function(numbiom){
  factorbiom <- factor(numbiom, levels=c(1:14),labels=c("Tropical and subtropical moist broadleaf forests",
                                                        "Tropical and subtropical dry broadleaf forests",
                                                        "Tropical and subtropical coniferous forests",
                                                        "Temperate broadleaf and mixed forests",
                                                        "Temperate Coniferous Forest",
                                                        "Boreal forests / Taiga",
                                                        "Tropical and subtropical grasslands, savannas and shrublands",
                                                        "Temperate grasslands, savannas and shrublands",
                                                        "Flooded grasslands and savannas",
                                                        "Montane grasslands and shrublands",
                                                        "Tundra",
                                                        "Mediterranean Forests, woodlands and scrubs",
                                                        "Deserts and xeric shrublands",
                                                        "Mangroves"))
  return(factorbiom)
}

# for (r in unique(sampleTileSP$tile)){}
# source("/gpfs/data1/duncansongp/amberliang/trends.Earth/git/GEDI_PA/matching_func.R")

ranTile <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv", pattern="_covar_prj_v3.rds") %>% 
  substr(4,11)
norunTile <- unique(sampleTileSP$tile)[unique(sampleTileSP$tile)%notin%ranTile]  

registerDoParallel(15)
startTime <- Sys.time()
foreach(this_r=norunTile,.combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','optmatch','doParallel')) %dopar% {
  cat(this_r,match(this_r, unique(sampleTile$tile)),"-- out of",length(unique(sampleTile$tile)), as.character(sampleTileSP$continent[1]) ,"\n")
  contiAGB <- data.frame()
  sampleSub <- sampleTileSP[sampleTileSP$tile==this_r,]
  cat(nrow(sampleSub),"in this tile\n")
  cat("cropping...\n")
  world_region_sub <- crop(world_region,extent(sampleSub))
  cat("rasterizing...\n")
  rasOutST <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$status), world_region_sub)
  rasOutBIOM <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$wwfbiom), world_region_sub,fun=getmode)
  rasOutPAID <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$pa_id), world_region_sub,fun=getmode)
  rasOutAge <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$paAge), world_region_sub,fun=getmode)
  # rasOutB2000 <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$before2000), world_region_sub,fun=getmode)
  # rasOutLC <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$land_cover), world_region_sub, fun=getmode)
  # rasOutEREG<- rasterize(sampleSub@coords, field=as.numeric(sampleSub$wwfecoreg), world_region_sub,fun=getmode)
  # rasOutELE <- rasterize(sampleSub@coords, field=sampleSub$elevation, world_region_sub, fun=mean)
  # rasOutSLP<- rasterize(sampleSub@coords, field=sampleSub$slope, world_region_sub,fun=mean)
  # rasOutMTMP <- rasterize(sampleSub@coords, field=sampleSub$mean_temp, world_region_sub, fun=mean)
  # rasOutXTMP<- rasterize(sampleSub@coords, field=sampleSub$max_temp, world_region_sub,fun=mean)
  # rasOutITEMP <- rasterize(sampleSub@coords, field=sampleSub$min_temp, world_region_sub, fun=mean)
  # rasOutPRE<- rasterize(sampleSub@coords, field=sampleSub$prec, world_region_sub,fun=mean)
  # rasOutDTR <- rasterize(sampleSub@coords, field=sampleSub$d2road, world_region_sub, fun=mean)
  # rasOutDTC<- rasterize(sampleSub@coords, field=sampleSub$d2city, world_region_sub,fun=mean)
  # rasOutTTC<- rasterize(sampleSub@coords, field=sampleSub$tt2city, world_region_sub,fun=mean)
  # rasOutPPD <- rasterize(sampleSub@coords, field=sampleSub$popden, world_region_sub, fun=mean)
  # rasOutPPC<- rasterize(sampleSub@coords, field=sampleSub$popcnt, world_region_sub,fun=mean)
  # rasOut <- stack(rasOutST, rasOutLC, rasOutBIOM, rasOutEREG, rasOutELE, rasOutSLP,rasOutMTMP,rasOutXTMP,rasOutITEMP,
  #                 rasOutPRE, rasOutDTR, rasOutDTC, rasOutPPD, rasOutTTC, rasOutPPC)
  cat("stacking..\n")
  rasOut <- stack(rasOutST,rasOutPAID, rasOutBIOM, rasOutAge)#rasOutST
  cat("raster to polygon..\n")
  gridPolygon <- rasterToPolygons(rasOut)
  names(gridPolygon) <-c("status","pa_id","wwfbiom","pa_Age") #names(sampleTile)[4:18]
  gridPolygonExPrj <- gridPolygon %>% spTransform(paste("+init=epsg:",4326,sep=""))  %>% extent()   #%>%  buffer(0.1) %>% 
  # gridPolygonExPrj[1] <- floor(gridPolygonExPrj[1])
  # gridPolygonExPrj[2] <- ceiling(gridPolygonExPrj[2])
  # gridPolygonExPrj[3] <- floor(gridPolygonExPrj[3])
  # gridPolygonExPrj[4] <- ceil(gridPolygonExPrj[4])
  gridPolygonExPrj_poly <- as(gridPolygonExPrj, 'SpatialPolygons') %>%  buffer(0.1)
  agbtif <- tryCatch(raster(as.character(sampleSub$download[1])), error=function(cond){ 
    if (file.exists(paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/","alt_ras_",this_r,".tif",sep=""))){
      cat("this tif has been downloaded before\n")
      tr <- raster(paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/","alt_ras_",this_r,".tif",sep=""))
      
    } else{
      cat(this_r,"has error in raster reading \n")
      download.file(url=as.character(sampleSub$download[1]), destfile=paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/","alt_ras_",this_r,".tif",sep=""))
      tr <- raster(paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/","alt_ras_",this_r,".tif",sep=""))
    }
    return(tr)
  })
  cat("Projetcing...\n")
  agbtifprj <- agbtif %>%  crop(extent(gridPolygonExPrj_poly))%>% 
    projectRaster(.,crs="+init=epsg:6933 +proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ", method="bilinear")  
  cat("Resampling...\n")
  agbtif2 <- tryCatch(raster::resample(agbtifprj,rasOut,method='bilinear'), error=function(cond){return(NULL)})
  sampleAGB <- tryCatch(raster::extract(agbtif2, gridPolygon, df=FALSE, method="simple"), error=function(cond){return(NULL)})
  # names(sampleAGB)[2] <- "AGB2000"
  if(!is.null(sampleAGB)){
    gridPolygon$AGB2000 <- unlist(sampleAGB)
    gridPolygonDF <- data.frame(gridPolygon)
    contiAGB <- rbind(contiAGB, gridPolygonDF)
    saveRDS(gridPolygonDF,paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/", as.character(sampleSub$continent.x[1]), "_", this_r,"_covar_prj_v3.rds",sep=""))
    cat("done\n")
    
  } else{
    cat("Null in sample AGB")
  }
  
  return(NULL)
}
tElapsed <- Sys.time()-startTime
# cat(tElapsed, "for matching all PAs in", iso3,"\n")
stopImplicitCluster()
# saveRDS(contiAGB,paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/", as.character(sampleTileSP$continent[1]), "_AGB2000_covar_all_prj2.rds",sep=""))

#read in the compiled AGB2000, resample to equalize control and protected, and compare the distribution 
f1files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern = "covar_prj_v3.rds",full.names = TRUE) %>% 
  intersect(list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern="SA", full.names = TRUE))
f1 <- data.frame()
for (f in f1files){
  print(f)
  tf <- readRDS(f)
  f1 <- rbind(tf,f1)
}
f2files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern = "covar_prj_v3.rds",full.names = TRUE) %>% 
  intersect(list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern="Au", full.names = TRUE))
f2 <- data.frame()
for (f in f2files){
  print(f)
  tf <- readRDS(f)
  f2 <- rbind(tf,f2)
}
f3files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern = "covar_prj_v3.rds",full.names = TRUE) %>% 
  intersect(list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern="As", full.names = TRUE))
f3 <- data.frame()
for (f in f3files){
  print(f)
  tf <- readRDS(f)
  f3 <- rbind(tf,f3)
}
f4files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern = "covar_prj_v3.rds",full.names = TRUE) %>% 
  intersect(list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern="US", full.names = TRUE))
f4 <- data.frame()
for (f in f4files){
  print(f)
  tf <- readRDS(f)
  f4 <- rbind(tf,f4)
}
f5files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern = "covar_prj_v3.rds",full.names = TRUE) %>% 
  intersect(list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern="Af", full.names = TRUE))
f5 <- data.frame()
for (f in f5files){
  print(f)
  tf <- readRDS(f)
  f5 <- rbind(tf,f5)
}
f6files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern = "covar_prj_v3.rds",full.names = TRUE) %>% 
  intersect(list.files("/gpfs/data1/duncansongp/GEDI_global_PA/csv/",pattern="Eu", full.names = TRUE))
f6 <- data.frame()
for (f in f6files){
  print(f)
  tf <- readRDS(f)
  f6 <- rbind(tf,f6)
}

f1 <- f1%>% mutate(conti="S.America")
f2 <- f2%>% mutate(conti="Oceania")
f3 <- f3%>% mutate(conti="Asia")
f4 <- f4%>% mutate(conti="N.America")
f5 <- f5%>% mutate(conti="Africa")
f6 <- f6%>% mutate(conti="Europe")

# agbDF <- f6
# statusCount <- agbDF %>% dplyr::rename(status=layer) %>% dplyr::filter(!is.na(AGB2000)) %>% group_by(conti,status) %>% 
#   dplyr::summarise(count=length(AGB2000)) 
# agbdDF_Samp <- agbDF %>% dplyr::rename(status=layer) %>% dplyr::filter(!is.na(AGB2000)) %>%
#   group_by(conti,status) %>% 
#   sample_n(min(statusCount$count))%>%
#   mutate(status=as.factor(status)) %>% 
#   ungroup()  %>% 
#   mutate(fstats=0, pval=0)
# one.way <- aov(AGB2000 ~ status, data=agbdDF_Samp)
# summary(one.way)

#resample to get rid of NA in AGB2000 and leave only 100 samples per biome per continent
`%notin%` <- Negate(`%in%`)
nonforest=c("Montane Grasslands & Shrublands", "Deserts & Xeric Shrublands", "Tundra", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
            "Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas")


allSub <- eurDF[,c("pa_id","wwfbiom")] %>% group_by(pa_id) %>% dplyr::summarise(wwfbiom=getmode(wwfbiom))
paAge$WDPAID <- as.character.Date(paAge$WDPAID)
agbDF <- f1 %>% rbind(f2) %>% rbind(f3) %>% rbind(f4) %>% rbind(f5) %>% rbind(f6)
statusCount <- agbDF %>% dplyr::filter(!is.na(AGB2000)) %>% group_by(conti,status) %>%
  dplyr::summarise(count=length(AGB2000)) %>% ungroup
statusCount %>% 
  tidyr::pivot_wider(names_from=status, values_from=count, names_prefix="status") %>%
  mutate(minCount=ifelse(status1>status0, status0, status1)) %>% #mutate(contiBiom=paste(conti, wwfbiom,sep="_")) %>% 
  drop_na()->countMin
set.seed(1234)
agbdDF_Samp <- agbDF %>% #mutate(contiBiom=paste(conti, wwfbiom,sep="_")) %>% 
  #left_join(countMin,by="conti")  %>% 
  dplyr::filter(!is.na(AGB2000)) %>%
  left_join(countMin, by="conti") %>% 
  # dplyr::filter(minCount>0) %>%
  group_by(conti,status) %>%data.frame() %>% 
  # sample_n(ifelse(minCount<10000, minCount,10000))%>%
  # mutate(status=as.factor(status)) %>%
  ungroup()  %>%
  mutate(pa_id=as.character(pa_id)) %>% 
  left_join(allSub, by="pa_id") %>% 
  mutate(fstats=0, pval=0) %>%
  # mutate(wwfbiom=wwfbiomConvert(wwfbiom.y)) %>%
  mutate(forest=ifelse(wwfbiom.y %notin% nonforest, "forest", "non-forest")) %>%
  # mutate(status_yr= ifelse(!is.na(pa_Age), 2021-pa_Age, NA)) %>% 
  left_join(paAge, by=c("pa_id"="WDPAID")) %>% 
  dplyr::filter(!is.na(STATUS_YR)) %>% dplyr::mutate(statYr=as.numeric(STATUS_YR)) %>% 
  mutate(newOld= ifelse(statYr>2000, "new","old" )) #2000 is old PA
  # mutate(before2000_2=ifelse(!is.na(paAge), ifelse(paAge>21,"oldPA","newPA"), NA)) %>%  #before2000_2 differ from before2000 in regards to the exclusion of year 2000 in older PA categories
  # mutate(after2001=ifelse(!is.na(paAge), ifelse(paAge<=20,"newPA","oldPA"), NA))
agbdDF_Samp %>% 
  ggplot( aes(x=AGB2000, fill=as.character(status))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#C3820C", "#0CC316")) +
  theme_ipsum() +
  labs(fill="")+
  facet_wrap(vars(newOld))

#one way anova by attribute
agbdDF_Samp2 <- agbdDF_Samp %>% dplyr::filter(!is.na(paAge)) %>% 
  mutate(after2001_forest=paste(newOld,sep="_"))
x <- unique(agbdDF_Samp2$after2001_forest)
x
models <- sapply(x, function(my) {
  lm(AGB2000 ~ status, data=agbdDF_Samp2, after2001_forest==my)
}, simplify=FALSE)

ANOVA.tables <- sapply(models, anova, simplify=FALSE)
ANOVA.tables
# saveRDS(ANOVA.tables,"/gpfs/data1/duncansongp/GEDI_global_PA/csv/forestNonforest_newOldPA_anova_mar16_samp10000.rds")

#overall results
# t <- agbdDF_Samp %>% filter(after2001==0) %>% filter(forest==0)
one.way <- aov(AGB2000 ~ status, data =agbdDF_Samp)
summary(one.way)

agbdDF_Samp %>% dplyr::filter(!is.na(paAge)) %>% 
  group_by(newOld, status) %>% 
  dplyr::summarise(count_ttl=length(AGB2000),
                   meanagbd=mean(AGB2000, na.rm=TRUE), sdagbd=sd(AGB2000, na.rm=TRUE), medagbd=median(AGB2000, na.rm=TRUE), msagbd=sum(is.na(AGB2000)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c( "status","newOld"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% as.data.frame()
  

#absolute difference between protected and control by biome, and continent x biome
agbdDF_Samp %>% dplyr::filter(!is.na(paAge)) %>% 
  # dplyr::mutate(bins=cut(statYr, breaks =seq(1789,2020,21))) %>% 
  dplyr::mutate(bins=cut(paAge, breaks =seq(0,221,21))) %>% 
  group_by(forest, bins,status) %>% 
  dplyr::summarise(count_ttl=length(AGB2000),
                   meanagbd=mean(AGB2000, na.rm=TRUE), sdagbd=sd(AGB2000, na.rm=TRUE), medagbd=median(AGB2000, na.rm=TRUE), msagbd=sum(is.na(AGB2000)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("forest","bins", "status","newOld"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% as.data.frame() %>% 
  mutate(forest_bin=paste(forest, bins, sep="_")) %>% 
  dplyr::select(forest_bin, absolute_diff_AGBD)->tf
tf_add <- data.frame(forest_bin="non-forest_(126,147]", absolute_diff_AGBD=NA)
tf <- tf%>%rbind(tf_add) %>% filter(forest_bin!="forest_NA")

agbdDF_Samp %>% dplyr::filter(!is.na(paAge)) %>% 
  dplyr::mutate(bins=cut(statYr, breaks =seq(1800,2020,20))) %>% 
  group_by(bins,status) %>% 
  dplyr::summarise(count_ttl=length(AGB2000),
                   meanagbd=mean(AGB2000, na.rm=TRUE), sdagbd=sd(AGB2000, na.rm=TRUE), medagbd=median(AGB2000, na.rm=TRUE), msagbd=sum(is.na(AGB2000)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c( "status","bins"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% as.data.frame()->tb


agbdDF_Samp %>% dplyr::filter(!is.na(paAge)) %>% 
  dplyr::mutate(bins=cut(statYr, breaks =seq(1800,2020,20))) %>% filter(forest=="forest")%>%
  group_by(conti,bins,status) %>% 
  dplyr::summarise(count_ttl=length(AGB2000),
                   meanagbd=mean(AGB2000, na.rm=TRUE), sdagbd=sd(AGB2000, na.rm=TRUE), medagbd=median(AGB2000, na.rm=TRUE), msagbd=sum(is.na(AGB2000)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c( "conti","status","bins"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% as.data.frame()->ta


agbdDF_Samp %>% dplyr::filter(!is.na(paAge)) %>% 
  dplyr::mutate(bins=cut(paAge, breaks =seq(0,221,21))) %>% 
  filter(!is.na(bins)) %>% 
  group_by(forest, bins,status) %>% 
  dplyr::summarise(mm=mean(AGB2000*0.49,na.rm=TRUE), sdm=sd(AGB2000*0.49, na.rm = TRUE),npa=as.integer(length(unique(pa_id)))) %>% 
  as.data.frame() %>% 
  mutate(forest_bin=paste(forest, bins,sep="_")) %>% 
  full_join(tf, by="forest_bin") %>% 
  mutate(diff2000=round(absolute_diff_AGBD*0.49, 2)) %>% 
  mutate(diff2000=ifelse(status==0, NA, paste("Mean difference\n",diff2000," (Mg/ha)\nn=",formatC(npa, format="d", big.mark=","), sep="")))->tf_org

tf_org$forest[27] <- "non-forest"
tf_org$bins[27] <- "(126,147]"
tf_org$status[27] <- 0

tf_org$bins <- factor(tf_org$bins, levels=c("(0,21]","(21,42]",'(42,63]', "(63,84]","(84,105]",'(105,126]',"(126,147]"),
                      labels=c("0 ~ 21", "22 ~ 42", "43 ~ 63" ,"64 ~ 84", "85 ~ 105" ," 106 ~ 126", "127 ~ 147"))

tf_org$forest <- factor(tf_org$forest, levels=c("forest","non-forest"), labels = c("PAs in forest-domiant biomes", "PAs in non-forest-domiant biomes"))

pa_age_plot <- ggplot(tf_org, aes(x = bins, y = mm,color=as.character(status), shape=as.character(status))) + 
  facet_wrap(vars(forest), scales = "free_y")+
  scale_shape_manual("Protected vs. Controls\n(Data presented as mean values +/- SD)", values=c(18,17),labels=c("Control","Protected/treated"))+
  scale_color_manual("Protected vs. Controls\n(Data presented as mean values +/- SD)", values=c("#e39244","#89d149"), labels=c("Control","Protected/treated"))+
  geom_point(position = position_dodge(.5)) +
  geom_pointrange(aes(ymin  = mm - sdm,
                      ymax  = mm + sdm),size  = 0.7,# width = 0.9,
                  position = position_dodge(.5)) +
  geom_text(
    mapping = aes(x = bins, y = Inf, label = diff2000),hjust   = 1.04,label.padding=0.5,
    vjust   = 0.04,size=4, color="dark green",
    color = "black")+
  theme_bw() +
  coord_flip()+
  labs(y="Year 2000 AGCD (Mg/ha)", x="PA Age (years)")+ 
  theme(strip.background = element_rect(fill="transparent", colour = "transparent"), strip.text = element_text(size = 14)) +
  theme(text=element_text(family="Helvetica", size=11))+ theme(axis.text = element_text(size = 12, color="black")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme(legend.position = c(0.837, 0.93),legend.text=element_text(size=10),
        legend.background = element_rect(fill = "white", color = "black"))

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_paAge_effect_forest_C_v3.png", 
                plot=pa_age_plot, width=10, height=8.2,
                units = "in", dpi="print",bg = "transparent")


#calculate mean abs_diff per PA

agbdDF_Samp %>%dplyr::filter(!is.na(AGB2000)) %>% group_by(status) %>% dplyr::summarise(count=length(unique(pa_id)),
                                                                                        unique_pa=list(unique(pa_id)))->tt
overlapPA <-intersect(unlist(tt$unique_pa[1]), unlist(tt$unique_pa[2])) 
stat1 <- agbdDF_Samp %>%
  dplyr::filter(status==1) %>%
  dplyr::filter(pa_id%in% overlapPA)
stat0 <- agbdDF_Samp %>%
  dplyr::filter(status==0) %>%
  dplyr::filter(pa_id%in% overlapPA)

agbdDF_Samp <- stat1 %>% rbind(stat0)

# set.seed(1234)
AGB_diff_pa <- agbdDF_Samp %>% dplyr::filter(!is.na(STATUS_YR)) %>% 
  # left_join(countMin, by="conti") %>% 
  # dplyr::filter(minCount>0) %>%
  # group_by(conti,status) %>%data.frame() %>% 
  # sample_n(ifelse(minCount<10000, minCount,10000))%>%
  # dplyr::mutate(bins=cut(statYr, breaks =seq(1800,2020,21))) %>% 
  group_by(pa_id,status) %>% 
  dplyr::summarise(meanagbd=mean(AGB2000, na.rm=TRUE), sdagbd=sd(AGB2000, na.rm=TRUE), medagbd=median(AGB2000, na.rm=TRUE), 
                   msagbd=sum(is.na(AGB2000)),
                   statYr=getmode(statYr),newOld=getmode(newOld),forest=getmode(forest),conti=getmode(conti))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id","conti","statYr","newOld","forest"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% 
  dplyr::filter(!is.na(absolute_diff_AGBD)) %>% as.data.frame()

# plot the relatinship between PA age and difference in 200 AGB 
AGB_diff_pa %>% dplyr::mutate(bins=cut(statYr, breaks =seq(1789,2020,21))) %>% 
  dplyr::group_by(conti, bins) %>% 
  dplyr::summarise(meanDiff=mean(absolute_diff_AGBD))
                   
  
contiPAage <- ggplot(AGB_diff_pa, aes(x = statYr, y = absolute_diff_AGBD,fill=bins)) + 
  geom_point(varwidth = TRUE, alpha=0.5) +
  theme_bw() +
  facet_wrap(vars(conti))+ylim(-300,300)+
  scale_fill_viridis(name="PA Age (Years)", discrete = TRUE, option = "plasma")+
  labs(y="Absolute Difference in 2000 AGBD (Mg/ha)", x="")+ 
  theme(strip.background = element_rect(fill="transparent", colour = "transparent")) +
  theme(text=element_text(family="Helvetica", size=14))+ theme(axis.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

allPAage <- ggplot(AGB_diff_pa, aes(x = bins, y = absolute_diff_AGBD,fill=bins)) + 
  geom_boxplot(varwidth = TRUE, alpha=0.5) +
  theme_bw() +ylim(-200, 200)+
  scale_fill_viridis(name="PA Age (Years)", discrete = TRUE, option="plasma")+
  labs(y="Absolute Difference in 2000 AGBD (Mg/ha)", x="")+ 
  theme(text=element_text(family="Helvetica", size=14)) 

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_paAge_effect_all_apr04.png", 
                plot=allPAage, width=8, height=6,
                units = "in", dpi="print",bg = "transparent")

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_paAge_effect_conti_apr04.png", 
                plot=contiPAage, width=10, height=6,
                units = "in", dpi="print",bg = "transparent")


#bin pas by their age to explore the difference in 2000 AGB as related to pa age 
pa_age_effect2000 <- agbdDF_Samp %>%  dplyr::filter(!is.na(paAge)) %>% 
  left_join(countMin, by="conti") %>% 
  # dplyr::filter(minCount>0) %>%
  # group_by(conti,pa_id,status) %>%data.frame() %>% 
  # sample_n(ifelse(minCount<10000, minCount,10000))%>%
  mutate(bins=cut(paAge, breaks = seq(0,240,21))) %>% 
  group_by(conti, bins, pa_id,status) %>%
  dplyr::summarise(meanagbd=mean(AGB2000, na.rm=TRUE), sdagbd=sd(AGB2000, na.rm=TRUE), medagbd=median(AGB2000, na.rm=TRUE), 
                   msagbd=sum(is.na(AGB2000)), count=length(unique(pa_id)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status","bins","conti"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% 
  dplyr::filter(!is.na(absolute_diff_AGBD)) %>% as.data.frame()
#   mutate(forest=factor(forest, level=c(0,1), labels = c("non-forest biomes", "forest biomes")))
# dplyr::summarise(m=mean(absolute_diff_AGBD,na.rm=TRUE),sd=sd(absolute_diff_AGBD,na.rm=TRUE),count=length(absolute_diff_AGBD) )

# plot the relatinship between PA age and difference in 200 AGB 
ggplot(pa_age_effect2000, aes(x = bins, y = absolute_diff_AGBD,fill=bins)) + 
  geom_boxplot(varwidth = TRUE) +
  theme_bw() +
  facet_wrap(vars(conti))+
  theme(legend.position="none") +
  # scale_x_discrete(labels=bin)+
  labs(title = "Absolute Difference in 2000 AGB by PA Age Group", x="Binned PA Age", y="Absolute Difference in AGB")+
  ylim(-300,300)


#historgram the analyzedPA age distributions
analyzedPAs <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/analyzedPAs.rds")
paAge <- paAge %>% mutate(analyzedpas=ifelse(WDPAID %in% analyzedPAs, 1, 0))
# paAge %>% ggplot(aes(x=STATUS_YR)) + 
#   geom_histogram(color="black", fill="white",binwidth = 5)
# 
# paAge %>% dplyr::filter(analyzedpas==1) %>%   #.$STATUS_YR %>% quantile()
#   ggplot(aes(x=STATUS_YR)) + 
#   geom_histogram(color="black", fill="white",binwidth = 5)
# 
# agbdDF_Samp %>%    #.$STATUS_YR %>% quantile()
#   ggplot(aes(x=status_yr)) + 
#   geom_histogram(color="black", fill="white",binwidth = 5)

#% sampled PAs younger than 2000 
agbdDF_Samp_uniq <- agbdDF_Samp %>% group_by(pa_id) %>% dplyr::summarise(STATUS_YR=getmode(STATUS_YR))
sum(agbdDF_Samp_uniq[!is.na(agbdDF_Samp_uniq$STATUS_YR),]$STATUS_YR>=2000) #1530
sum(agbdDF_Samp_uniq[!is.na(agbdDF_Samp_uniq$STATUS_YR),]$STATUS_YR<2000)  #620
dim(agbdDF_Samp_uniq[!is.na(agbdDF_Samp_uniq$STATUS_YR),])  #2150
1530/2150*100  #71.16% younger than 2000

#% analyzed PAs young3er than 2000 
paAge_ana <- paAge %>% dplyr::filter(analyzedpas==1) %>% group_by(WDPAID) %>% dplyr::summarise(STATUS_YR=unique(STATUS_YR))
sum(paAge_ana[!is.na(paAge_ana$STATUS_YR),]$STATUS_YR>=2000)->a
sum(paAge_ana[!is.na(paAge_ana$STATUS_YR),]$STATUS_YR<2000) ->b
dim(paAge_ana[!is.na(paAge_ana$STATUS_YR),]) ->c
a/c*100  #72.08% younger than 2000

#% all PAs young3er than 2000 
paAge2 <- paAge %>% group_by(WDPAID) %>% dplyr::summarise(STATUS_YR2=unique(STATUS_YR))
sum(paAge2[!is.na(paAge2$STATUS_YR2),]$STATUS_YR2>=2000)->a
sum(paAge2[!is.na(paAge2$STATUS_YR2),]$STATUS_YR2<2000) ->b
dim(paAge2[!is.na(paAge2$STATUS_YR2),]) ->c
a/c*100  #60.00% younger than 2000


hgAll <- hist(paAge$STATUS_YR, breaks = seq(1800,2025,5), plot = FALSE) # Save first histogram data
hgAnalyzed <- hist(paAge[paAge$analyzedpas==1,]$STATUS_YR, breaks =seq(1800, 2025,5), plot = FALSE) # Save 2nd histogram data
hgSampled <- hist(agbdDF_Samp$STATUS_YR, breaks = seq(1800,2025,5), plot = FALSE) # Save 3rd histogram data

plot(hgAll, col = "#ebb475", main="PA status year historgram", 
     sub="Orange: all PAs;  Green: analyzed PAs;  Blue: sampled PAs", xlab="PA status year", ylab="count") # Plot 1st histogram using a transparent color
plot(hgAnalyzed, col ="#bcd4a3", add = TRUE) 
plot(hgSampled, col="#91bfdb", add=TRUE)
abline(v=median(paAge$STATUS_YR),col="#fc8d59",lwd=4)
text(x=1980, y=42000, median(paAge$STATUS_YR),col="#fc8d59" )
abline(v=median(paAge[paAge$analyzedpas==1,]$STATUS_YR, na.rm=TRUE),col="#648f36",lwd=4)
text(x=1990, y=46000,median(paAge[paAge$analyzedpas==1,]$STATUS_YR), col="#648f36")
abline(v=median(agbdDF_Samp$STATUS_YR,na.rm=TRUE),col="#4f78ab",lwd=4)
text(x=2020, y=42000, median(agbdDF_Samp$STATUS_YR,na.rm=TRUE), col="#4f78ab")




#---------old codes more complex----------------------
one.way <- aov(AGB2000 ~ status, data = agbdDF_Samp[agbdDF_Samp$conti=="S.America",])
summary(one.way)
agbdDF_Samp[agbdDF_Samp$conti=="S.America",]$fstats <- summary(one.way)[[1]][1,4]
agbdDF_Samp[agbdDF_Samp$conti=="S.America",]$pval <- summary(one.way)[[1]][1,5]

one.way <- aov(AGB2000 ~status, data = agbdDF_Samp[agbdDF_Samp$conti=="N.America",])
summary(one.way)
agbdDF_Samp[agbdDF_Samp$conti=="N.America",]$fstats <- summary(one.way)[[1]][1,4]
agbdDF_Samp[agbdDF_Samp$conti=="N.America",]$pval <- summary(one.way)[[1]][1,5]

one.way <- aov(AGB2000 ~ status, data = agbdDF_Samp[agbdDF_Samp$conti=="Europe",])
summary(one.way)
agbdDF_Samp[agbdDF_Samp$conti=="Europe",]$fstats <- summary(one.way)[[1]][1,4]
agbdDF_Samp[agbdDF_Samp$conti=="Europe",]$pval <- summary(one.way)[[1]][1,5]

one.way <- aov(AGB2000 ~ status, data = agbdDF_Samp[agbdDF_Samp$conti=="Africa",])
summary(one.way)
agbdDF_Samp[agbdDF_Samp$conti=="Africa",]$fstats <- summary(one.way)[[1]][1,4]
agbdDF_Samp[agbdDF_Samp$conti=="Africa",]$pval <- summary(one.way)[[1]][1,5]

one.way <- aov(AGB2000 ~ status, data = agbdDF_Samp[agbdDF_Samp$conti=="Asia",])
summary(one.way)
agbdDF_Samp[agbdDF_Samp$conti=="Asia",]$fstats <- summary(one.way)[[1]][1,4]
agbdDF_Samp[agbdDF_Samp$conti=="Asia",]$pval <- summary(one.way)[[1]][1,5]

one.way <- aov(AGB2000 ~ status, data = agbdDF_Samp[agbdDF_Samp$conti=="Oceania",])
summary(one.way)
agbdDF_Samp[agbdDF_Samp$conti=="Oceania",]$fstats <- summary(one.way)[[1]][1,4]
agbdDF_Samp[agbdDF_Samp$conti=="Oceania",]$pval <- summary(one.way)[[1]][1,5]

one.way <- aov(AGB2000 ~ status, data=agbdDF_Samp)
summary(one.way)

agbDF_summ <-agbdDF_Samp %>% 
  mutate(contiStats=paste(conti, " f(1)=",round(fstats,2), " p-value=",round(pval,5),sep=""))
agbDF_summ %>% 
  ggplot( aes(x=AGB2000, fill=as.character(status))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#C3820C", "#0CC316")) +
  theme_ipsum() +
  labs(fill="")+
  facet_wrap(vars(contiStats))+
  labs(title="Overall ANOVA summary f(1)=31.61 Pr=1.91e-08 n=20926 ")+
  theme_minimal()+
  theme( strip.text = element_text(size = 12), plot.title=element_text(size=14))