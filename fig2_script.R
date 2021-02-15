#fig2
#2b map of all GEDI 
fpath <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_GEDI_extract3/"
dd <- list.files(path = paste(fpath, "iso_full_nodup",sep=""), pattern=".csv",full.names = TRUE, recursive=FALSE)
bbox=matrix(c(-179.9,-53,179.9,53), nrow = 2, dimnames = list(c("x","y"), c("min", "max")))  #get a worldwide bbox to run globally 
# bc_big <- get_map(location = bbox,maptype="roadmap", source="osm", crop=TRUE)
# top=%quantile(0.999)%>%round_any(10, f=ceiling)
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
  theme(text=element_text(family="Times", face="bold", size=14),
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

mapcoords <- coord_fixed(xlim = c(-178, 180), ylim = c(-55, 55))

mbase<- ggplot() +  mapcoords + maptheme+ country_shapes+
  scale_color_gradientn(
                        limits = c(0,60),
                        colours=c("olivedrab1",
                                  "olivedrab4", "darkgreen","midnightblue", "red4"),
                        na.value="grey50",
                        breaks=b, labels=lab,name=paste("Max Canopy Height, n=150,465,105")) 

for (d in dd){
  print(d)
  dsub=read.csv(d)%>% dplyr::select(lon_lowestmode, lat_lowestmode,rh_098)
  mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = rh_098), size=0.001, alpha=0.3)
}


mbase=mbase+country_border+labs(title = "Distribution of GEDI shots")

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig2_gedishots_dist.png", plot=mbase, width=12, height=11,
                units = "in", bg = "transparent")


#by AGBD
b=c(seq(0,340,40),345)
lab=format(b)
lab[length(b)]="345+\n(~top 1%)"
# lab=str_wrap(lab, width = 5)

nonforest=c("Montane Grasslands & Shrublands", "Deserts & Xeric Shrublands", "Tundra", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
            "Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas")


maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key.width=unit(3,"cm"))+
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Times", face="bold", size=14),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) 

mbase<- ggplot() +  mapcoords +  country_shapes+
  scale_color_gradientn(
    limits = c(0,345),
    colours=c("olivedrab1",
              "olivedrab4", "darkgreen","midnightblue", "red4"),
    na.value="#f5f5f5",
    breaks=b, labels=lab,name=paste("AGBD Mg/ha,\nn=60,985,021"))
 

for (d in dd){
  print(d)
  dsub=read.csv(d) %>% dplyr::select(lon_lowestmode, lat_lowestmode,AGBD, wwfbiom)
  # dsub=dsub[which(dsub$wwfbiom %notin% nonforest),]
  if(nrow(dsub)>0){
    mbase=mbase+ geom_point(data = dsub, mapping = aes(x = lon_lowestmode, y = lat_lowestmode,color = AGBD, fill=""), size=0.001, alpha=0.3)
  } else{
    mbase=mbase
  }
}
mbase=mbase+country_border+labs(title = "Distribution of GEDI shots")+maptheme+
  scale_fill_manual(values=NA) +              
  guides(fill=guide_legend("No data", override.aes=list(colour="#f5f5f5")))
# mbase
 

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig2_gedishots_agbddist_2.png", plot=mbase, width=12, height=12,
                units = "in", bg = "transparent")

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

