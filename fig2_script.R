#fig2
#2b map of all GEDI 
packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools","gdalUtils",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","spatialEco","bit64","randomForest", "modelr",
              "ranger","caret","scattermore")
package.check <- lapply(packages, FUN = function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})

fpath <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
dd <- list.files(path = paste(fpath, "iso_full_nodup",sep=""), pattern=".csv",full.names = TRUE, recursive=FALSE)
bbox=matrix(c(-179.9,-53,179.9,53), nrow = 2, dimnames = list(c("x","y"), c("min", "max")))  #get a worldwide bbox to run globally 
iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") 
oPAs <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/balanced_paids.csv") %>% .$x
# bc_big <- get_map(location = bbox,maptype="roadmap", source="osm", crop=TRUE)
# top=%quantile(0.999)%>%round_any(10, f=ceiling)


#------------rh98[scattermore method]----------------------
dd <- list.files(path = paste(fpath, "WDPA_gedi_l2a+l2b_clean2/",sep=""), pattern="",full.names = TRUE, recursive=TRUE)

b=c(seq(0,50,10),60)
lab=format(b)
lab[length(b)]="60+ (~top 0.1%)"
lab

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(3,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) 

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "transparent",
                               size = 0.15)
country_border <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "transparent", color = "#515151",
                               size = 0.15)

mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-51.6, 51.6))

mbase<- ggplot() +  mapcoords + maptheme+ country_shapes+
  scale_color_gradientn(
    limits = c(0,60),
    colours=c("#bfd3e6",
              "#9ebcda",
              "#8c96c6",
              "#8c6bb1",
              "#88419d",
              "#253494", "#004529"),
    na.value="#f5f5f5",
    breaks=b, labels=lab,name=paste("Max Canopy Height (m)")) 


t=dd[1] %>% read.csv() %>% .[1,]
t[1,]$rh_098 <- NA

mbase=mbase+ geom_point(data=t,
                        aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098, fill=""), size=0)



set.seed(1234)
index <- c(seq(1, length(dd), 105), length(dd))
for (ind in 2:length(index)){
  print(ind)
  start <- index[ind-1]
  end <- index[ind]
  sub_dd <- dd[start:end]
  
  registerDoParallel(cores=10)
  sub_dd_all <- foreach(d=sub_dd,.combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
    sub_dd_all <- data.frame()
    print(d)
    rm(dsub)
    dsub=read.csv(d)%>% #dplyr::filter(pa_id %in% oPAs) %>% 
      dplyr::select(lon_lowestmode, lat_lowestmode,rh_098) %>% dplyr::mutate(rh_098=rh_098/100)
    print("alldata")
    if(nrow(dsub)>5000){
      dsub <- dsub %>% sample_n(5000)
    }
    sub_dd_all <- rbind(sub_dd_all, dsub)
    return(sub_dd_all)
    
  }
  stopImplicitCluster()
  print("sampledata")
  # mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), size=0.001, alpha=0.3)
  mbase=mbase+ geom_scattermore(data=sub_dd_all,
                                aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), 
                                pointsize=1,
                                pixels =c(10000,10000),
                                interpolate=TRUE)
  print("layer added")

}

mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-51.6, 51.6))


mbase1=mbase+country_border+  #+labs(title = "Distribution of GEDI shots")
  scale_fill_manual(values=NA) +  mapcoords+            
  guides(fill=guide_legend("No valid data", override.aes=list(colour="#f5f5f5"), title.position="bottom"))+
  guides(fill = FALSE)

mbase1=mbase1+labs(tag = "C")

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_gedishots_dist_rh98_mar26_2.png", 
                plot=mbase1, width=12, height=4,
                units = "in", dpi="print",bg = "transparent")



#-----------------------rh98-------------------------------------------------
dd <- list.files(path = paste(fpath, "WDPA_gedi_l2a+l2b_clean2/",sep=""), pattern="",full.names = TRUE, recursive=TRUE)

b=c(seq(0,50,10),60)
lab=format(b)
lab[length(b)]="60+ (~top 0.1%)"
lab

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(3,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) 

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "transparent",
                               size = 0.15)
country_border <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "transparent", color = "#515151",
                               size = 0.15)

mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-51.6, 51.6))

mbase<- ggplot() +  mapcoords + maptheme+ country_shapes+
  scale_color_gradientn(
                        limits = c(0,60),
                        colours=c("#bfd3e6",
                          "#9ebcda",
                          "#8c96c6",
                          "#8c6bb1",
                          "#88419d",
                          "#6e016b", "#004529"),
                        na.value="#f5f5f5",
                        breaks=b, labels=lab,name=paste("Max Canopy Height (m)")) 

for (d in dd){
  print(d)
  dsub=read.csv(d)%>% #dplyr::filter(pa_id %in% oPAs) %>% 
    dplyr::select(lon_lowestmode, lat_lowestmode,rh_098) %>% dplyr::mutate(rh_098=rh_098/100)
  mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098, fill=""), size=0.001, alpha=0.3)
}


mbase1=mbase+country_border+  #+labs(title = "Distribution of GEDI shots")
  scale_fill_manual(values=NA) +              
  guides(fill=guide_legend("No valid data", override.aes=list(colour="#f5f5f5"), title.position="bottom"))+
  guides(fill = FALSE)

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_gedishots_dist_rh98_mar26.png", plot=mbase1, width=12, height=8,
                units = "in", dpi="print",bg = "transparent")


#--------------------by AGBD---------------------------------
dd <- list.files(path = paste(fpath, "WDPA_gedi_l4a_clean/",sep=""), pattern="",full.names = TRUE, recursive=TRUE)

b=c(seq(0,340,40),345)
lab=format(b)
lab[length(b)]="345+\n(~top 1%)"
# lab=str_wrap(lab, width = 5)

nonforest=c("Montane Grasslands & Shrublands", "Deserts & Xeric Shrublands", "Tundra", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
            "Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas")
mapcoords <-coord_fixed(xlim = c(-178, 180), ylim = c(-55, 55)) #coord_fixed(xlim =c(60, 75) , ylim =c(29, 39) )   #
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "transparent",
                               size = 0.15)
country_border <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "transparent", color = "#515151",
                               size = 0.15)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(3,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) 

mbase<- ggplot() +  mapcoords +  country_shapes+
  scale_color_gradientn(
    limits = c(0,345),
    colours=c("olivedrab1",
              "olivedrab4", "darkgreen","midnightblue", "red4"),
    na.value="#f5f5f5",
    breaks=b, labels=lab,name="AGBD (Mg/ha)")

t=dd[1] %>% read.csv() %>% .[1,]
t[1,]$agbd <- NA
mbase=mbase+ geom_point(data=t,
                        aes(x = lon_lowestmode, y = lat_lowestmode,color = agbd, fill=""), size=0.00001)

 
set.seed(1234)
index <- c(seq(1, length(dd), 110), length(dd))
for (ind in 2:length(index)){
  print(ind)
  start <- index[ind-1]
  end <- index[ind]
  sub_dd <- dd[start:end]
  
  registerDoParallel(cores=10)
  sub_dd_all <- foreach(d=sub_dd,.combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
    sub_dd_all <- data.frame()
    print(d)
    rm(dsub)
    dsub <- read.csv(d) %>% #dplyr::filter(pa_id%in% oPAs) %>% 
      dplyr::select(lon_lowestmode, lat_lowestmode,agbd)
    print("alldata")
    if(nrow(dsub)>5000){
      dsub <- dsub %>% sample_n(5000)
    }
    sub_dd_all <- rbind(sub_dd_all, dsub)
    return(sub_dd_all)
    
  }
  stopImplicitCluster()
  print("sampledata")
  # mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), size=0.001, alpha=0.3)
  mbase=mbase+ geom_scattermore(data=sub_dd_all,
                                aes(x = lon_lowestmode, y = lat_lowestmode,color = agbd), 
                                pointsize=1,
                                pixels=c(10000,10000),
                                interpolate=TRUE)
  print("layer added")
  
}

mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-51.6, 51.6))

mbase2 <- mbase+country_border+maptheme+mapcoords+
  scale_fill_manual(values=NA) +
  guides(fill=guide_legend("  No valid data", override.aes=list(colour="#f5f5f5"), title.position="bottom"))

mbase2=mbase2+labs(tag = "B")

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_gedishots_dist_agbd_mar26_2.png",
                plot=mbase2, width=12, height=4,
                units = "in", dpi="print",bg = "transparent")



#--------canopy cover----------------------------------------

dd <- list.files(path = paste(fpath, "WDPA_gedi_l2a+l2b_clean2/",sep=""), pattern="",full.names = TRUE, recursive=TRUE)

b=seq(0,1, 0.1)
lab=format(b)
# lab[length(b)]="345+\n(~top 1%)"
# lab=str_wrap(lab, width = 5)

nonforest=c("Montane Grasslands & Shrublands", "Deserts & Xeric Shrublands", "Tundra", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
            "Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas")
mapcoords <-coord_fixed(xlim = c(-178, 180), ylim = c(-55, 55)) #coord_fixed(xlim =c(60, 75) , ylim =c(29, 39) )   #
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "transparent",
                               size = 0.15)
country_border <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "transparent", color = "#515151",
                               size = 0.15)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(3,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) 

mbase<- ggplot() +  mapcoords +  country_shapes+
  scale_color_gradientn(
    limits = c(0,1),
    colours=c("#ffffe5",
      "#f7fcb9",
      '#d9f0a3',
      "#addd8e",
      "#78c679",
      "#41ab5d",
      '#238443',
      "#006837",
      "#004529","#253494"),
    na.value="#f5f5f5",
    breaks=b, labels=lab,name="Canopy cover  ")

t=dd[1] %>% read.csv() %>% .[1,]
t[1,]$cover <- NA

mbase=mbase+ geom_point(data=t,
                        aes(x = lon_lowestmode, y = lat_lowestmode,color = cover, fill=""), size=0)


set.seed(1234)
index <- c(seq(1, length(dd), 110), length(dd))
for (ind in 132:length(index)){
  print(ind)
  start <- index[ind-1]
  end <- index[ind]
  sub_dd <- dd[start:end]
  
  registerDoParallel(cores=10)
  sub_dd_all <- foreach(d=sub_dd,.combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
    sub_dd_all <- data.frame()
    print(d)
    rm(dsub)
    dsub <- read.csv(d) %>% #dplyr::filter(pa_id%in% oPAs) %>% 
      dplyr::select(lon_lowestmode, lat_lowestmode,cover)
    print("alldata")
    if(nrow(dsub)>5000){
      dsub <- dsub %>% sample_n(5000)
    }
    sub_dd_all <- rbind(sub_dd_all, dsub)
    return(sub_dd_all)
    
  }
  stopImplicitCluster()
  print("sampledata")
  # mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), size=0.001, alpha=0.3)
  mbase=mbase+ geom_scattermore(data=sub_dd_all,
                                aes(x = lon_lowestmode, y = lat_lowestmode,color = cover), 
                                pointsize=1,
                                pixels=c(10000,10000),
                                interpolate=TRUE)
  print("layer added")
  
}


mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-51.6, 51.6))

mbase3 <- mbase+country_border+maptheme+
  scale_fill_manual(values=NA) +  mapcoords+        
  guides(fill=guide_legend("Data quality not suitable", override.aes=list(colour="#f5f5f5"), title.position="bottom"))


mbase3=mbase3+labs(tag="E")
ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_gedishots_dist_cover_mar26_3.png", 
                plot=mbase3, width=12, height=4,
                units = "in", dpi="print",bg = "transparent")

#-----PAI--------------------

dd <- list.files(path = paste(fpath, "WDPA_gedi_l2a+l2b_clean2/",sep=""), pattern="",full.names = TRUE, recursive=TRUE)[1:100]
b=c(seq(0, 9, 1), 10)
lab=format(b)
lab[length(b)]="10+"
lab=str_wrap(lab, width = 5)
mapcoords <-coord_fixed(xlim = c(-178, 180), ylim = c(-55, 55)) #coord_fixed(xlim =c(60, 75) , ylim =c(29, 39) )   #
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "transparent",
                               size = 0.15)
country_border <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "transparent", color = "#515151",
                               size = 0.15)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(3,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) 

mbase<- ggplot() +  mapcoords +  country_shapes+
  scale_color_gradientn(
    limits = c(0,10),
    colours=c("#fff7ec",
      "#fdd49e",
      "#fdbb84",
      "#fc8d59",
      "#ef6548",
      '#d7301f',
      '#b30000',"#49006a"),
    na.value="#f5f5f5",
    breaks=b, labels=lab,name="PAI  ")


t=dd[1] %>% read.csv() %>% .[1,]
t[1,]$pai <- NA

mbase=mbase+ geom_point(data=t,
                        aes(x = lon_lowestmode, y = lat_lowestmode,color = pai, fill=""), size=0)


set.seed(1234)
index <- c(seq(1, length(dd), 110), length(dd))
for (ind in 2:length(index)){
  print(ind)
  start <- index[ind-1]
  end <- index[ind]
  sub_dd <- dd[start:end]
  
  registerDoParallel(cores=10)
  sub_dd_all <- foreach(d=sub_dd,.combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
    sub_dd_all <- data.frame()
    print(d)
    rm(dsub)
    dsub <- read.csv(d) %>% #dplyr::filter(pa_id%in% oPAs) %>% 
      dplyr::select(lon_lowestmode, lat_lowestmode,pai)
    print("alldata")
    if(nrow(dsub)>5000){
      dsub <- dsub %>% sample_n(5000)
    }
    sub_dd_all <- rbind(sub_dd_all, dsub)
    return(sub_dd_all)
    
  }
  stopImplicitCluster()
  print("sampledata")
  # mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), size=0.001, alpha=0.3)
  mbase=mbase+ geom_scattermore(data=sub_dd_all,
                                aes(x = lon_lowestmode, y = lat_lowestmode,color = pai), 
                                pointsize=1,
                                pixels=c(10000,10000),
                                interpolate=TRUE)
  print("layer added")
  
}


mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-51.6, 51.6))

mbase4 <- mbase+country_border+maptheme+
  scale_fill_manual(values=NA) +  mapcoords+        
  guides(fill=guide_legend(" Valid data not available", override.aes=list(colour="#f5f5f5"), title.position="bottom", order = 5))+
  guides(fill = FALSE)

mbase4=mbase4+labs(tag = "D") 

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_gedishots_dist_cover_mar26_2.png", 
                plot=mbase4, width=12, height=4,
                units = "in", dpi="print",bg = "transparent")







#------------pa polygons--------------------
# b=c(seq(0,340,40),345)
# lab=format(b)
# lab[length(b)]="345+\n(~top 1%)"
# lab=str_wrap(lab, width = 5)

dd <- list.files(path = paste(fpath, "WDPA_GEDI_extract4/iso_full_nodup",sep=""), pattern=".csv",full.names = TRUE, recursive=FALSE)




nonforest=c("Montane Grasslands & Shrublands", "Deserts & Xeric Shrublands", "Tundra", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
            "Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas")
forestBiomKey <- c(1:6, 12,14)
biomRas <- raster()

mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-52, 52)) 
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "transparent",
                               size = 0.1)
country_border <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "transparent", color = "#515151",
                               size = 0.1)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(1.5,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10),
        rect = element_rect(fill = "transparent")) 


f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
ecoregion_list <- read.csv(paste(f.path,"wwf_ecoregions_key.csv",sep=""))
biomKey <- unique(ecoregion_list[,1:2])

mbase<- ggplot() +  mapcoords +  country_shapes

`%notin%` <- Negate(`%in%`)
countryContinent <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/progress_l4bAGB_countryxBiome_byCountry_jan13.csv") %>% 
  dplyr::filter(iso3 %notin% c("COK","BHR","FSM","SYR","SOM","SPM","PSE","MDA","LBY","PRK","ATF","ABW","AIA","AND","ASM","USA")) 

for (d in countryContinent$iso3){
  print(d)
  country <- d %>% gsub(".*full_nodup/(.+)_country_full.*", "\\1", .)
  if (countryContinent$iso3Status[match(d, countryContinent$iso3)]=="analyzed"){
    country_all_pa <- read_sf(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "all_PAs_",country,"_v3.shp",sep="")) %>%
      st_transform("+proj=longlat +datum=WGS84 +no_defs")
    country_ana_pa <- read_sf(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "analyzed_PAs_",country,"_v3.shp",sep="")) %>%
      st_transform("+proj=longlat +datum=WGS84 +no_defs")
    # diff <- gDifference(country_all_pa, country_ana_pa)
    foreBiomRas <- raster(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_input_vars_iso3_v2/", country,"/wwf_biomes.tif",sep=""))
    foreBiomdf <- as.data.frame(foreBiomRas, xy = TRUE) %>% mutate(wwf_biomes=ifelse(wwf_biomes %in% forestBiomKey, wwf_biomes,"NA"))
    
    # dsub <- read.csv(d) %>% dplyr::filter(pa_id%in% oPAs) %>% dplyr::select(lon_lowestmode, lat_lowestmode,agbd, wwfbiom)
    # dsub=dsub[which(dsub$wwfbiom %notin% nonforest),]
    # if (!is.null(diff)){
    country_ana_pa$level <- "Analyzed PAs"
    country_all_pa$level <- "Other PAs"
    
    # country_ana_pa_df <- broom::tidy(country_ana_pa, region = "level")
    # 
    # country_all_pa_df <- broom::tidy(country_ana_pa, region = "level")
    mbase <- mbase+ #geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = agbd, fill=""), size=0.0001, alpha=1)+
      geom_tile(data = foreBiomdf, aes(x = x, y = y, fill = as.character(wwf_biomes)))+
      # geom_sf(aes(colour=level),
      #         data =country_all_pa,size=0.15,
      #         fill = "transparent")+
      # geom_sf(aes(colour=level),
      #         data =country_ana_pa,size=0.15,
      #         fill = "transparent")+
      coord_sf(xlim = c(-178, 180), ylim = c(-53, 53),expand = FALSE)
    
  } else{
    country_all_pa <- read_sf(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "all_PAs_",country,"_v3.shp",sep="")) %>%
      st_transform("+proj=longlat +datum=WGS84 +no_defs")
   
    # diff <- gDifference(country_all_pa, country_ana_pa)
    foreBiomRas <- raster(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_input_vars_iso3_v2/", country,"/wwf_biomes.tif",sep=""))
    foreBiomdf <- as.data.frame(foreBiomRas, xy = TRUE) %>% mutate(wwf_biomes=ifelse(wwf_biomes %in% forestBiomKey, wwf_biomes,"NA"))
    
    # dsub <- read.csv(d) %>% dplyr::filter(pa_id%in% oPAs) %>% dplyr::select(lon_lowestmode, lat_lowestmode,agbd, wwfbiom)
    # dsub=dsub[which(dsub$wwfbiom %notin% nonforest),]
    # if (!is.null(diff)){
    
    country_all_pa$level <- "Other PAs"
    
    # country_ana_pa_df <- broom::tidy(country_ana_pa, region = "level")
    # 
    # country_all_pa_df <- broom::tidy(country_ana_pa, region = "level")
    # mbase <- mbase+ #geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = agbd, fill=""), size=0.0001, alpha=1)+
    #   geom_tile(data = foreBiomdf, aes(x = x, y = y, fill = as.character(wwf_biomes)))+
    #   geom_sf(aes(colour=level),
    #           data =country_all_pa,size=0.2,
    #           fill = "transparent")
    # 
  }
  
}


for (d in countryContinent$iso3){
  print(d)
  country <- d %>% gsub(".*full_nodup/(.+)_country_full.*", "\\1", .)
  if (countryContinent$iso3Status[match(d, countryContinent$iso3)]=="analyzed"){
    country_all_pa <- read_sf(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "all_PAs_",country,"_v3.shp",sep="")) %>%
      st_transform("+proj=longlat +datum=WGS84 +no_defs")
    country_ana_pa <- read_sf(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "analyzed_PAs_",country,"_v3.shp",sep="")) %>%
      st_transform("+proj=longlat +datum=WGS84 +no_defs")
    # diff <- gDifference(country_all_pa, country_ana_pa)
    # foreBiomRas <- raster(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_input_vars_iso3_v2/", country,"/wwf_biomes.tif",sep=""))
    # foreBiomdf <- as.data.frame(foreBiomRas, xy = TRUE) %>% mutate(wwf_biomes=ifelse(wwf_biomes %in% forestBiomKey, wwf_biomes,"NA"))
    # 
    # dsub <- read.csv(d) %>% dplyr::filter(pa_id%in% oPAs) %>% dplyr::select(lon_lowestmode, lat_lowestmode,agbd, wwfbiom)
    # dsub=dsub[which(dsub$wwfbiom %notin% nonforest),]
    # if (!is.null(diff)){
    country_ana_pa$level <- "Analyzed PAs"
    country_all_pa$level <- "Other PAs"
    
    # country_ana_pa_df <- broom::tidy(country_ana_pa, region = "level")
    # 
    # country_all_pa_df <- broom::tidy(country_ana_pa, region = "level")
    mbase <- mbase+ #geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = agbd, fill=""), size=0.0001, alpha=1)+
      # geom_tile(data = foreBiomdf, aes(x = x, y = y, fill = as.character(wwf_biomes)))+
      geom_sf(aes(colour=level),
              data =country_all_pa,size=0.1,
              fill = "#FF00FF")+
      geom_sf(aes(colour=level),
              data =country_ana_pa,size=0.1,
              fill = "blue")+
      coord_sf(xlim = c(-178, 180), ylim = c(-53, 53),expand = FALSE)
    
  } else{
    country_all_pa <- read_sf(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "all_PAs_",country,"_v3.shp",sep="")) %>%
      st_transform("+proj=longlat +datum=WGS84 +no_defs")
    
    country_all_pa$level <- "Other PAs"
    
    mbase <- mbase+ #geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = agbd, fill=""), size=0.0001, alpha=1)+
      geom_tile(data = foreBiomdf, aes(x = x, y = y, fill = as.character(wwf_biomes)))+
      geom_sf(aes(colour=level),
              data =country_all_pa,size=0.2,
              fill = "transparent")
    # 
  }
  
}
mbase5 <- mbase+country_border+
  scale_fill_manual(values = c("1" = "#B2FF64","2"="#CCFE64", "3"="#86CC64","4"="#ABAB66","5"="#CBCB60", 
                               "6"="#6A9C6E","12"="#FFCD61", "14"="#7CAC60","NA"="transparent"),
                    labels=c(as.character(biomKey$BIOME_NAME[forestBiomKey]),"Non-forest biomes"),name=str_wrap("WWF Biomes", width = 5), drop = FALSE)+
  scale_colour_manual(values=c("Analyzed PAs"="blue", "Other PAs"="#FF00FF"), name="PA\nTypes") +
  coord_sf(xlim = c(-178, 180), ylim = c(-53, 53),expand = FALSE)+
  # guides(colour=guide_legend("No data", override.aes=list(colour="#f5f5f5"),title.position="bottom"))+
  guides(fill=guide_legend(ncol=2))+ guides(colour=guide_legend(ncol=1, override.aes = list(fill = c("blue", "#FF00FF"))))+
  maptheme

source("/gpfs/data1/duncansongp/amberliang/scripts/scale_bar_north_arrow.R")
mbase5 <- mbase5+ scale_bar(lon = -160, lat = -45, 
                  distance_lon = 2000, distance_lat = 300, distance_legend = 620, 
                  dist_unit = "km", orientation = FALSE, legend_size=3.5)+labs(tag = "A")

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_pa_locations_apr05_v2.png", 
                plot=mbase5, width=12, height=8,
                units = "in", dpi="print",bg = "transparent")




######arranging the five figures into one figure##########

mbase2=readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_agbd_final_22.rds")
mbase4=readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_pai_final_22.rds")
mbase1=readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_rh98_final_22.rds")
mbase3=readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_cover_final.rds")
mbase5=readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/mbase5_scale.rds")


maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(1.5,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10),
        rect = element_rect(fill = "transparent")) 
mbase5=mbase5+labs(tag = "A")+maptheme

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_pa_locations_mar26_4.png", 
                plot=mbase5, width=12, height=6,
                units = "in", dpi="print",bg = "transparent")



ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig2_3panels.pdf", 
                plot=ggpubr::ggarrange( mbase5,mbase2, mbase1, mbase4, mbase3,
                                        labels = c("A", "B", "C","D","E"),
                                        ncol = 1, nrow = 5), width=12, height=18,
                units = "in", dpi="print",bg = "transparent")

# +
#   scale_colour_manual(values=NA) +
#   guides(fill=guide_legend(ncol=2))

# +
#   guides(fill=guide_legend("Non-forest biomes", override.aes=list(colour=rgb(0,0,0,0)), title.position="bottom", ncol=2))
# mbase2

# global_pa <- readOGR(paste("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/dissolved_by_region/", "all_PAs_",iso3,"_v3.shp",sep=""))


# mbase_papoly <- mapbase+geom_polygon(data = global_pa, aes(x = long, y = lat, group = group), colour = "red", fill = NA)


##previous version
# mbase=ggmap(bc_big) + 
#   scale_color_gradientn(limits = c(0,60),
#                         colours=c("olivedrab1",
#                                   "olivedrab4", "darkgreen","midnightblue", "red4"),
#                         na.value="grey50",
#                         breaks=b, labels=lab,name=paste("Max Canopy Height, n=150,465,105")) +
#   labs(title = "Distribution of GEDI shots")+
#   theme_bw()+
#   theme(text=element_text(family="Times", face="bold", size=14),
#         legend.title=element_text(size=12), 
#         legend.text=element_text( size=12),
#         rect = element_rect(fill = "transparent")) 
# for (d in dd){
#   print(d)
#   dsub=read.csv(d)%>% dplyr::select(lon_lowestmode, lat_lowestmode,rh_098)
#   mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), size=0.001, alpha=0.3)
# }

#2a map of all matched
# matched_PAs <- Sys.glob(paste(f.path,"WDPA_matching_results/*_wk",gediwk,"/*.RDS", sep=""))
# matched_PAs <- list.files(paste(f.path,"WDPA_matching_results/",sep=""), pattern="wk106", full.names = TRUE, recursive = TRUE)
# 
# bbox=matrix(c(-179.9,-52,179.9,83), nrow = 2, dimnames = list(c("x","y"), c("min", "max")))  #get a worldwide bbox to run globally 
# bc_big <- get_map(location = bbox, source="stamen", maptype="terrain", crop=FALSE)
# b=c(seq(0,100,10),550)
# lab=format(b)
# lab[length(b)]="100+"
# lab
# 
# mbase=ggmap(bc_big) + 
#   theme(legend.text = element_text(color = "black", size = 8),plot.title = element_text(size=15, face="bold"),
#         rect = element_rect(fill = "transparent"))
# 
# for (f in matched_PAs){
#   print(f)
#   dsub=readRDS(f) 
#   # %>% dplyr::select(lon, lat,status)
#   mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon, y = lat,colour =as.factor(as.numeric(status))), size=0.01, alpha=0.3)+
#     scale_colour_manual(values=c("#bf812d","#35978f", rgb(0,0,0,0)))
# }
# 

