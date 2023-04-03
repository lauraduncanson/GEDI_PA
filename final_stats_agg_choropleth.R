packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools","gdalUtils",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","spatialEco","bit64","randomForest", "modelr","ranger","caret",
              "cowplot", "googleway",  "ggrepel", "ggspatial",  "sf", "rnaturalearth", "rnaturalearthdata","BAMMtools")
package.check <- lapply(packages, FUN = function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})

iso_agbd_diff <- sub_for_iso %>%
  # dplyr::filter(is.finite(pai)) %>% 
  group_by(iso3, status) %>% 
  # group_by(status) %>%  
  dplyr::summarise(
    count_ttl=length(agbd),npas=length(unique(pa_id)),
    meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
    meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
    meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
    meanagbd=mean(agbd, na.rm=TRUE), sdagbd=sd(agbd, na.rm=TRUE), medagbd=median(agbd, na.rm=TRUE), msagbd=sum(is.na(agbd)))%>% 
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id","iso3", "status","wwfbiom"))) %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  # dplyr::select(iso3, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control, count_ttl_1, msagbd_1, count_ttl_0,msagbd_0) %>%
  # dplyr::mutate(n_PA= format(count_ttl_1-msagbd_1, big.mark=",")) %>%
  # dplyr::mutate(n_control= format(count_ttl_0-msagbd_0, big.mark=",")) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>%
  dplyr::mutate(absolute_diff_rh98=(meanrh98_1-meanrh98_0)/100) %>%
  dplyr::mutate(percent_diff_rh98=100*(meanrh98_1-meanrh98_0)/meanrh98_0) %>%
  dplyr::mutate(absolute_diff_cover=meancov_1-meancov_0) %>%
  dplyr::mutate(percent_diff_cover=100*(meancov_1-meancov_0)/meancov_0) %>%
  dplyr::mutate(absolute_diff_pai=meanpai_1-meanpai_0) %>%
  dplyr::mutate(percent_diff_pai=100*(meanpai_1-meanpai_0)/meanpai_0) %>%
  # dplyr::mutate(percent_diff_AGBD_normalized=percent_diff_AGBD/iso_mean_agbd_0) %>%
  # dplyr::select(-c(count_ttl_0, count_ttl_1, msagbd_0, msagbd_1)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
iso_level_agb <- iso_agbd_diff%>%
  left_join(iso_pa_area,by="iso3") %>% 
  dplyr::mutate(sem_1=sdAGBD_PA/sqrt(count_ttl_1-msagbd_1), sem_0=sdAGBD_control/sqrt(count_ttl_0-msagbd_0)) %>% 
  dplyr::mutate(sedm=sqrt(sem_1^2+sem_0^2)) %>% dplyr::mutate(extra_AGB_in_PA=absolute_diff_AGBD*totalPAArea/1000000000, SE_in_AGB=sedm*totalPAArea/1000000000) %>% 
  dplyr::mutate(extra_AGB_in_PA=format(round(extra_AGB_in_PA,digits=5), nsmall = 5),SE_in_AGB=format(round(SE_in_AGB,digits=5), nsmall = 5) ) %>% 
  dplyr::arrange(desc(as.numeric(extra_AGB_in_PA))) 


iso_level_agb_sub <- iso_level_agb %>% filter(iso3 %notin% c("USA_east","USA_west","USA_pcfc")) %>% dplyr::select(iso3, totalPAArea, extra_AGB_in_PA, SE_in_AGB)
iso_level_agb_usa <-  iso_level_agb %>% filter(iso3 %in% c("USA_east","USA_west","USA_pcfc")) %>% dplyr::select(iso3, totalPAArea, extra_AGB_in_PA, SE_in_AGB)
iso_level_agb_usa2 <- data.frame(iso3="USA", totalPAArea=sum(as.numeric(iso_level_agb_usa$totalPAArea)),
                                         extra_AGB_in_PA=sum(as.numeric(iso_level_agb_usa$extra_AGB_in_PA)), 
                                                             SE_in_AGB=sum(as.numeric(iso_level_agb_usa$SE_in_AGB)))
iso_level_agb_mod <- iso_level_agb_sub %>% rbind(iso_level_agb_usa2)

iso_sub <- iso_agbd_diff %>% left_join(iso_region, by="iso3") %>% dplyr::select(names(temp)) %>%  filter(iso3 %in% c("USA_east","USA_west","USA_pcfc")) 
ctgroup=iso_sub2 %>% group_by(continent) %>% group_split()
ctmain=data.frame()
for (g in 1:length(ctgroup)){
  
  main=ctgroup[[g]][1, ] %>% as.data.frame()
  while(complete.cases(main)!=TRUE){
    rows <- sample(nrow(ctgroup[[g]]))
    ctgroup[[g]]<-ctgroup[[g]][rows, ]
    main=ctgroup[[g]][1,] %>% as.data.frame()
    print(main) 
  }
  
  for (r in 2:nrow(ctgroup[[g]])){
    tmp=ctgroup[[g]][r,] %>% as.data.frame()
    print(tmp)
    if(is.na(tmp$meanagbd_1)|| is.na(tmp$meanagbd_0)){
      main=main
    } else{
      main= format_agg_stats(main, tmp)
    }
  }
  ctmain=rbind(main, ctmain)
}
iso_sub_usa <- ctmain
iso_sub_usa$iso3 <- "USA"
iso_sub_mod <- iso_sub %>% filter(iso3 %notin% c("USA_east","USA_west","USA_pcfc")) %>% rbind(iso_sub_usa) %>% left_join(iso_level_agb_mod,by="iso3") %>% 
  mutate(SE_in_AGB=as.numeric(SE_in_AGB), extra_AGB_in_PA=as.numeric(extra_AGB_in_PA))


#------calculating with the country level stats----------------------------
iso_region_usa_r <- data.frame(X=0, iso3="USA", continent="US")
iso_region <-rbind(iso_region_usa_r, iso_region)
iso_pa_area_usa_r <- data.frame(iso3="USA", pa_area=529565.6 , totalPAArea=52956556)
iso_pa_area<-rbind(iso_pa_area_usa_r, iso_pa_area)
iso_total_Area <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/allpa_areas_dup.csv") %>% mutate(totalPAArea_all=area*100)
all0=read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/country_level_sum_usa.csv")


all2 <- all0 %>% left_join(iso_region, by="iso3") %>% dplyr::rename(continent=continent.y) %>% 
  left_join(iso_pa_area, by=c("iso3")) %>% 
  left_join(iso_total_Area, by="iso3") %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  # dplyr::select(continent, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control, count_ttl_0, count_ttl_1) %>% 
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>% 
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% 
  dplyr::mutate(absolute_diff_rh98=(meanrh98_1-meanrh98_0)/100) %>%
  dplyr::mutate(percent_diff_rh98=100*(meanrh98_1-meanrh98_0)/meanrh98_0) %>%
  dplyr::mutate(absolute_diff_cover=meancov_1-meancov_0) %>%
  dplyr::mutate(percent_diff_cover=100*(meancov_1-meancov_0)/meancov_0) %>%
  dplyr::mutate(absolute_diff_pai=meanpai_1-meanpai_0) %>%
  dplyr::mutate(percent_diff_pai=100*(meanpai_1-meanpai_0)/meanpai_0) %>% 
  dplyr::mutate(sem_1=sdAGBD_PA/sqrt(count_ttl_1-msagbd_1), sem_0=sdAGBD_control/sqrt(count_ttl_0-msagbd_0)) %>% 
  dplyr::mutate(sedm=sqrt(sem_1^2+sem_0^2)) %>% dplyr::mutate(extra_AGB_in_PA=absolute_diff_AGBD*totalPAArea/1000000000, SE_in_AGB=sedm*totalPAArea/1000000000) %>% 
  dplyr::mutate(extra_AGB_in_PA=format(round(extra_AGB_in_PA,digits=5), nsmall = 5),SE_in_AGB=format(round(SE_in_AGB,digits=5), nsmall = 5) ) %>% 
  dplyr::arrange(desc(as.numeric(extra_AGB_in_PA)))
write.csv(all2, "/gpfs/data1/duncansongp/GEDI_global_PA/csv/country_level_stats_all.csv")


#-------prepare the data by joing the Atotal AGB values and the ancillary information by country ---------------------
totalAGB_anc <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_ancillary_v2.csv")
totalAGB <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_usa.csv") %>% 
  dplyr::select(totalAGB ,   AGB_stderr   ,       AGB_type, ISO3, continent, iso3Status) %>% 
  pivot_wider(names_from=AGB_type, values_from=c(totalAGB,AGB_stderr)) %>% 
  left_join(totalAGB_anc, by=c("ISO3"="iso3"))


#-----plotting the choropleth maps-----
library(rnaturalearth)
library(BAMMtools)
library(sf)
# all2_mod <- all2 %>% mutate(extra_AGB_in_PA=as.numeric(extra_AGB_in_PA)) %>% mutate(totalPAArea_all=totalPAArea_all/1000000)
all2_mod <- totalAGB

sf_use_s2(FALSE)
world <-ne_countries(scale = "medium", returnclass = "sf")
class(world)
dim(world)
world2=world %>% left_join(all2_mod, by=c("iso_a3"="ISO3")) 
# %>% 
#   mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0)%>%
#   dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
#   dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>%
#   dplyr::mutate(absolute_diff_rh98=(meanrh98_1-meanrh98_0)/100) %>%
#   dplyr::mutate(percent_diff_rh98=100*(meanrh98_1-meanrh98_0)/meanrh98_0) %>%
#   dplyr::mutate(absolute_diff_cover=meancov_1-meancov_0) %>%
#   dplyr::mutate(percent_diff_cover=100*(meancov_1-meancov_0)/meancov_0) %>%
#   dplyr::mutate(absolute_diff_pai=meanpai_1-meanpai_0) %>%
#   dplyr::mutate(percent_diff_pai=100*(meanpai_1-meanpai_0)/meanpai_0) 

br <- append(getJenksBreaks(world2$totalAGB_allPAExtraAGB,7), 1.5) %>% unique() %>% sort() %>% round(digits = 1)
t1=ggplot(data = world2) +
  geom_sf(aes(fill = cut(round(totalAGB_allPAExtraAGB,1), breaks=br, include.lowest=TRUE, right=FALSE)), size=0.4, colour="#494a4a") +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#dfc27d", "#eef5dc","#d9f0a3","#addd8e", "#41ab5d", "#1b7038",  "#004529"), na.translate = F) +
  # scale_colour_manual(values=NA) +              
  # guides(colour=guide_legend("No PAs/No matches", override.aes=list(colour="black", fill="white")))   +       
  labs(title = "Additional preserved AGB in PAs by country",
       fill = "Preserved AGB (GT)")+
  theme_bw() +
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12), axis.text=element_text(size=12)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
  labs(tag = "A")

t11=t1+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
t11


br2 <- getJenksBreaks(world2$totalAGB_allPAAGB,8) %>% unique() %>% sort() %>% round(digits = 1)
t2=ggplot(data = world2) +
  geom_sf(aes(fill = cut(round(totalAGB_allPAAGB,1), breaks=br2, include.lowest=TRUE, right=FALSE)), size=0.4, colour="#494a4a") +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#ffffcc", "#c7e9b4","#7fcdbb", "#41b6c4", "#1d91c0",'#225ea8','#0c2c84'),na.translate = F) +
  # scale_colour_manual(values=NA) +              
  # guides(colour=guide_legend("No PAs/No matches", override.aes=list(colour="black", fill="white")))   +       
  labs(title = "Total AGB in PAs by country",
       fill = str_wrap(" Total PA AGB (GT)", width = 20))+
  theme_bw() +
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12), axis.text=element_text(size=12)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
  labs(tag = "C")

t22=t2+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
t22


br3 <-append(seq(-65, 75, len=7), 0) %>% sort() %>% round(digits = 1) #append(getJenksBreaks(world2$weighted_agbd_diff,7), 0) %>% unique() %>% sort() %>% round(digits = 2)
t3=ggplot(data = world2) +
  geom_sf(aes(fill = cut(weighted_agbd_diff, breaks=br3, include.lowest=TRUE, right=FALSE)), size=0.4, colour="#494a4a") +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  scale_fill_manual(values=c("#bf812d", "#dfc27d", "#f6e8c3","#c7eae5",  "#80cdc1",  "#35978f", "#0a4a45"),na.translate = F) +
  # scale_colour_manual(values=NA) +         
  # guides(colour=guide_legend("No PAs/No matches", override.aes=list(colour="black", fill="white")))   +       
  labs(title = "Average additional AGBD preserved in PAs by country",
       fill = str_wrap("Average additional AGBD (Mg/ha)", width = 20))+
  theme_bw() +
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12),
        axis.text=element_text(size=12)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
  labs(tag = "B")

t33=t3+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
t33


world2[world2$adm0_a3=="BRA",]$total_pa_area <- 2.52*1000000
br4 <- getJenksBreaks(world2$total_pa_area/1000000,8) %>%round(digits = 2)
br4[8] <- 2.52
t4=ggplot(data = world2) +
  geom_sf(aes(fill = cut(total_pa_area/1000000, breaks=br4, include.lowest=TRUE, right=FALSE), colour=""), size=0.4)+
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#fffad6", "#fff7bc","#fee391", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#993404","#662506"), na.translate = F) +
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("No PAs/No matches", override.aes=list(colour="black", fill="#f0f0f0")))   +       
  labs(title = "PA areas by country",
       fill = "PA area (Million ha)")+
  theme_bw() +
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12),
        axis.text=element_text(size=12)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(legend.spacing.y = unit(0.1, "cm"))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
  labs(tag = "D")

t44=t4+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
  # scale_y_continuous(breaks = c(-50, -25, 0, 25,50))
t44


ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/iso_level_biomass_results_apr22.png', sep=''), 
                plot= grid.arrange(t11, t33,t22, t44, ncol=1), 
                width=8, height=10, units = "in", bg = "transparent")

#--------panel for other three metrics-----------------
p1=ggplot(data = world2) +
  geom_sf(aes(fill = cut(absolute_diff_rh98, breaks=append(getJenksBreaks(all2$absolute_diff_rh98,9), 0, 2), include.lowest=TRUE, right=FALSE))) +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#dfc27d", "#f6e8c3","#d9f0a3","#addd8e", "#78c679","#41ab5d", "#238443", "#006837", "#004529"),na.value="#f5f5f5") +
  labs(title = "Absolute Difference in Max Canopy Height by country",
       fill = "Absolute differnce in max height (m)")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "A")

p11=p1+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
# t11
br=getJenksBreaks(world2$percent_diff_rh98,9)
br[2]=-15
br=append(br, 0, 3)
p2=ggplot(data = world2) +
  geom_sf(aes(fill = cut(percent_diff_rh98, breaks=br, include.lowest=TRUE, right=FALSE))) +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#bf812d",
                             "#dfc27d",
                             "#f6e8c3",
                             '#d9f0a3',
                             '#addd8e',
                             '#78c679',
                             '#31a354',
                             '#006837',"#005a32"),na.value="#f5f5f5") +
  labs(title = "Percent Difference in Max Canopy Height by country",
       fill = "Percent differnce in max height (%)")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "B")

p22=p2+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
p22

p3=ggplot(data = world2) +
  geom_sf(aes(fill = cut(absolute_diff_cover, breaks=append(getJenksBreaks(world2$absolute_diff_cover,8),0,2), include.lowest=TRUE, right=FALSE))) +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#fee0b6", "#edf8b1","#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"),na.value="#f5f5f5") +
  labs(title = "Absolute Difference in Canopy Cover by country",
       fill = " Absolute difference in cover")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "C")

p33=p3+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)

p4=ggplot(data = world2) +
  geom_sf(aes(fill = cut(percent_diff_cover, breaks=append(getJenksBreaks(world2$percent_diff_cover,8),0,2), include.lowest=TRUE, right=FALSE))) +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#fee0b6", "#edf8b1", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"),na.value="#f5f5f5") +
  labs(title = "Percent Difference in Canopy Cover by country",
       fill = "Percent difference in cover (%)")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "D")

p44=p4+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)

p5=ggplot(data = world2) +
  geom_sf(aes(fill = cut(absolute_diff_pai, breaks=append(getJenksBreaks(world2$absolute_diff_pai,8),0,3), include.lowest=TRUE, right=FALSE))) +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#fec44f","#fee391","#fff7bc",'#edf8fb',
                             '#ccece6',
                             '#99d8c9',
                             '#66c2a4',
                             '#2ca25f',
                             '#006d2c'),na.value="#f5f5f5") +
  labs(title = "Absolute Difference in PAI by country",
       fill = "Absolute difference in PAI")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "E")

p55=p5+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)

p6=ggplot(data = world2) +
  geom_sf(aes(fill = cut(percent_diff_pai, breaks=append(getJenksBreaks(world2$percent_diff_pai,8),0,2), include.lowest=TRUE, right=FALSE))) +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#fec44f","#fff7bc",'#edf8fb',
                             '#ccece6',
                             '#99d8c9',
                             '#66c2a4',
                             '#41ae76',
                             '#238b45',
                             '#005824'),na.value="#f5f5f5") +
  labs(title = "Percent Difference in PAI by country",
       fill = "Percent difference in PAI (%)")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(axis.ticks.x = element_blank()) +
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "F")

p66=p6+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)

ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/iso_level_metrics_results_v3.png', sep=''), 
                plot= do.call("grid.arrange", c(list(p11, p22, p33, p44, p55, p66), ncol = 2)), 
                width=18, height=10, units = "in", bg = "transparent")






#old method of choropleth map that can't be projected and cropped but kept
library(choroplethr)
data(country.map, package = "choroplethrMaps")
nameC <- country.map %>%  mutate(iso3=adm0_a3) %>% dplyr::select(iso3,region) %>%  unique()
plotAGBD <- iso_sub_mod %>% right_join(nameC, by="iso3") %>% mutate(value=extra_AGB_in_PA)%>% dplyr::select(region, value)
plotAGBD <- plotAGBD[!duplicated(plotAGBD[,c('region')]),] 
p1 <- country_choropleth(plotAGBD,
                         num_colors=9) +
  scale_fill_manual(values=c("#dfc27d", "#f6e8c3","#d9f0a3","#addd8e", "#78c679","#41ab5d", "#238443", "#006837", "#004529")) +
  labs(title = "AGBD percent difference between PA sites and control sites by country",
       fill = "Percent Difference in AGBD")+
  theme_bw() +
  
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "A")

plotAGBD_abs <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=absolute_diff_AGBD)%>% dplyr::select(region, value)
plotAGBD_abs <- plotAGBD_abs[!duplicated(plotAGBD_abs[,c('region')]),] 
p2 <- country_choropleth(plotAGBD_abs,
                         num_colors=9) +
  scale_fill_manual(values=c("#dfc27d", "#f6e8c3","#d9f0a3","#addd8e", "#78c679","#41ab5d", "#238443", "#006837", "#004529")) +
  labs(title = "AGBD absolute difference between PA sites and control sites by country",
       fill = "Absolute Difference in AGBD")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "B")

plotRH98 <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=percent_diff_rh98)%>% dplyr::select(region, value)
plotRH98 <- plotRH98[!duplicated(plotRH98[,c('region')]),] 
p3 <- country_choropleth(plotRH98,
                         num_colors=9) +
  scale_fill_manual(values=c("#fec44f", "#fee391", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b")) +
  labs(title = "Max height percent difference between PA sites and control sites by country",
       fill = "Percent Difference in max height")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "C")

plotRH98_abs <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=absolute_diff_rh98)%>% dplyr::select(region, value)
plotRH98_abs <- plotRH98_abs[!duplicated(plotRH98_abs[,c('region')]),] 
p4 <- country_choropleth(plotRH98_abs,
                         num_colors=9) +
  scale_fill_manual(values=c("#fec44f", "#fee391", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b")) +
  labs(title = "Max height absolute difference between PA sites and control sites by country",
       fill = "Absolute Difference in Max Height")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "D")

plotCover <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=percent_diff_rh98)%>% dplyr::select(region, value)
plotCover <- plotCover[!duplicated(plotCover[,c('region')]),] 
p5 <- country_choropleth(plotCover,
                         num_colors=9) +
  scale_fill_manual(values=c("#fee391","#fff7bc", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#02818a", "#016c59", "#014636")) +
  labs(title = "Cover percent difference between PA sites and control sites by country",
       fill = "Percent Difference in Cover")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "E")

plotCover_abs <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=absolute_diff_rh98)%>% dplyr::select(region, value)
plotCover_abs <- plotCover_abs[!duplicated(plotCover_abs[,c('region')]),] 
p6 <- country_choropleth(plotCover_abs,
                         num_colors=9) +
  scale_fill_manual(values=c("#fee391","#fff7bc", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#02818a", "#016c59", "#014636")) +
  labs(title = "Cover absolute difference between PA sites and control sites by country",
       fill = "Absolute Difference in Cover")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "F")

plotPAI <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=percent_diff_rh98)%>% dplyr::select(region, value)
plotPAI <- plotPAI[!duplicated(plotPAI[,c('region')]),] 
p7 <- country_choropleth(plotPAI,
                         num_colors=9) +
  scale_fill_manual(values=c("#fd8d3c","#fed976","#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")) +
  labs(title = "PAI percent difference between PA sites and control sites by country",
       fill = "Percent Difference in PAI")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "G")

plotPAI_abs <- iso_agbd_diff %>% right_join(nameC, by="iso3") %>% mutate(value=absolute_diff_rh98)%>% dplyr::select(region, value)
plotPAI_abs <- plotPAI_abs[!duplicated(plotPAI_abs[,c('region')]),] 
p8 <- country_choropleth(plotPAI_abs,
                         num_colors=9) +
  scale_fill_manual(values=c("#fd8d3c","#fed976","#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")) +
  labs(title = "PAI absolute difference between PA sites and control sites by country",
       fill = "Absolute Difference in PAI")+
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=15),
        legend.title=element_text(size=10), 
        legend.text=element_text( size=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.key.size = unit(0.5, 'cm'))+
  labs(tag = "H")

ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/iso_level_results_fore_biomes_v2_3.png', sep=''), 
                plot= do.call("grid.arrange", c(list(p1,p2,p3,p4,p5,p6,p7,p8), ncol = 2)), 
                width=18, height=14, units = "in", bg = "transparent")
# ggsave(plot=grid.table(ranks), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_top_fore.png", 
#        height =0.7* nrow(ranks), width = 2*ncol(ranks), bg = "transparent")
