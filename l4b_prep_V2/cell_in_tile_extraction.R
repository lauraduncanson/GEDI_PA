#This code snippet shows how to get the tile that each grid cell is in, and parallelize across tiles to extract tile values for the extent of each 1km grid cell
eurDF <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/Eu_results.rds")
tileDF <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/Aboveground_live_woody_biomass_density.csv") %>%
  dplyr::select(name, download)
set.seed(1231)
sample <- eurDF %>% group_by(continent, wwfbiom, status) %>%   #eurDF has all the matched grid cells for Europe
  sample_n(.,ifelse(length(min_temp)>400,400, length(min_temp)))  #stratified sampling
sampleTile <- sample %>% mutate(tileLat=round_any(lat, 10, f = ceiling), tileLon=round_any(lon, 10,f=floor)) %>%   #get the tile name that each cell is in
  mutate(tileLat2 =ifelse(tileLat>=0,"N","S"), tileLon2=ifelse(tileLon>=0,"E","W")) %>% 
  mutate(tileLon=abs(tileLon)) %>% 
  mutate(tileLat=abs(tileLat)) %>% 
  mutate(tileLat=ifelse(nchar(tileLat)<2, paste("0",tileLat,sep=""),tileLat)) %>% 
  mutate(tileLon=ifelse(nchar(tileLon)<3, paste("0",tileLon,sep=""),tileLon)) %>% 
  mutate(tileLon=ifelse(nchar(tileLon)<3, paste("0",tileLon,sep=""),tileLon)) %>% 
  mutate(tile=paste(tileLat, tileLat2, "_",tileLon, tileLon2,"_t_aboveground_biomass_ha_2000",sep="")) %>% 
  left_join(tileDF,by=c("tile"="name")) %>%mutate(pa_id=as.numeric(pa_id)) %>% 
  left_join(paAge, by=c("pa_id"="WDPAID"))
sampleTileSP <- sampleTile %>% SpatialPointsDataFrame(data=., coords=.[,c("lon","lat")],
                                                      proj4string=CRS("+init=epsg:4326")) %>% 
  spTransform(., "+init=epsg:6933")
# sampleTile$tile %>% unique() #%>% length()
world_region <- raster(paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_6933.tif",sep=""))  #1km raster template
projection(world_region) <- sp::CRS(paste("+init=epsg:",6933,sep=""))

getmode <- function(v,na.rm) {
  uniqv <- unique(v)
  if(na.rm==TRUE){
    uniqv <- unique(na.omit(v))
  }
  return(uniqv[which.max((tabulate(match(v,uniqv))))])
}  #function to get mode 

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
}  #function to convert wwfbiome 

registerDoParallel(15)  #parallelize across 15 tiles at a time
startTime <- Sys.time()
foreach(this_r=unique(sampleTileSP$tile),.combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','optmatch','doParallel')) %dopar% {
  cat(this_r,match(this_r, unique(sampleTile$tile)),"-- out of",length(unique(sampleTile$tile)), as.character(sampleTileSP$continent[1]) ,"\n")
  contiAGB <- data.frame()
  sampleSub <- sampleTileSP[sampleTileSP$tile==this_r,]
  cat("rasterizing...\n")
  world_region_sub <- crop(world_region,extent(buffer(sampleSub,10000)))
  rasOutST <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$status), world_region_sub)
  rasOutBIOM <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$wwfbiom), world_region_sub,fun=getmode)
  rasOutPAID <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$pa_id), world_region_sub,fun=getmode)
  rasOutAge <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$paAge), world_region_sub,fun=getmode)
  rasOutB2000 <- rasterize(sampleSub@coords, field=as.numeric(sampleSub$before2000), world_region_sub,fun=getmode)
  rasOut <- stack(rasOutST,rasOutPAID, rasOutBIOM, rasOutAge, rasOutB2000)#rasOutST
  gridPolygon <- rasterToPolygons(rasOut)
  names(gridPolygon) <-c("status","wwfbiom","pa_Age","before2000") #names(sampleTile)[4:18]
  gridPolygonExPrj <- gridPolygon %>% spTransform(paste("+init=epsg:",4326,sep="")) %>%union() %>% buffer(0.1) %>% extent()
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
  agbtifprj <- agbtif %>%crop(gridPolygonExPrj)%>% projectRaster(.,crs="+init=epsg:6933 +proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ", method="bilinear")  
  cat("Resampling...\n")
  agbtif2 <- tryCatch(raster::resample(agbtifprj,rasOut,method='bilinear'), error=function(cond){return(NULL)})
  sampleAGB <- tryCatch(raster::extract(agbtif2, gridPolygon, df=FALSE, method="simple"), error=function(cond){return(NULL)})
  if(!is.null(sampleAGB)){
    gridPolygon$AGB2000 <- unlist(sampleAGB)
    gridPolygonDF <- data.frame(gridPolygon)
    contiAGB <- rbind(contiAGB, gridPolygonDF)
    saveRDS(gridPolygonDF,paste("/gpfs/data1/duncansongp/GEDI_global_PA/csv/", as.character(sampleSub$continent[1]), "_", this_r,"_covar_prj_v3.rds",sep=""))
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
