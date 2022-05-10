#This script is for making the four panel choropleth maps shwoing country level PA effectiveness as well as other stats at the country level 

#-------prepare the data by joing the Atotal AGB values and the ancillary information by country ---------------------
totalAGB_anc <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_mar08_ancillary_v2.csv")
totalAGB <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_mar08_usa.csv") %>% 
  dplyr::select(totalAGB ,   AGB_stderr   ,       AGB_type, ISO3, continent, iso3Status) %>% 
  pivot_wider(names_from=AGB_type, values_from=c(totalAGB,AGB_stderr)) %>% 
  left_join(totalAGB_anc, by=c("ISO3"="iso3"))


#-----plotting the choropleth maps-----
library(rnaturalearth)
library(BAMMtools)
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
  geom_sf(aes(fill = cut(totalAGB_allPAExtraAGB, breaks=br, include.lowest=TRUE, right=FALSE)), size=0.4, colour="#494a4a") +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  # coord_sf(crs = "+proj=moll")+
  scale_fill_manual(values=c("#dfc27d", "#eef5dc","#d9f0a3","#addd8e", "#41ab5d", "#1b7038",  "#004529"), na.translate = F) +
  # scale_colour_manual(values=NA) +              
  # guides(colour=guide_legend("No PAs/No matches", override.aes=list(colour="black", fill="white")))   +       
  labs(title = "Additional Preserved AGB in PAs by country",
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
  geom_sf(aes(fill = cut(totalAGB_allPAAGB, breaks=br2, include.lowest=TRUE, right=FALSE)), size=0.4, colour="#494a4a") +
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
  labs(tag = "B")

t22=t2+coord_sf(xlim = c(-179, 180), ylim = c(-55, 55), expand = FALSE)
t22


br3 <-append(seq(-65, 75, len=7), 0) %>% sort() %>% round(digits = 1) #append(getJenksBreaks(world2$weighted_agbd_diff,7), 0) %>% unique() %>% sort() %>% round(digits = 2)
t3=ggplot(data = world2) +
  geom_sf(aes(fill = cut(weighted_agbd_diff, breaks=br3, include.lowest=TRUE, right=FALSE)), size=0.4, colour="#494a4a") +
  coord_sf(crs = "+proj=cea +lon_0=Central Meridian +lat_ts=Standard Parallel +x_0=False Easting +y_0=False Northing +ellps=WGS84")+
  scale_fill_manual(values=c("#bf812d", "#dfc27d", "#f6e8c3","#c7eae5",  "#80cdc1",  "#35978f", "#0a4a45"),na.translate = F) +
  # scale_colour_manual(values=NA) +         
  # guides(colour=guide_legend("No PAs/No matches", override.aes=list(colour="black", fill="white")))   +       
  labs(title = "Average Additional AGBD by country",
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
  labs(tag = "C")

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


ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/iso_level_biomass_results_apr04.png', sep=''), 
                plot= grid.arrange(t11, t22,t33, t44, ncol=1), 
                width=8, height=10, units = "in", bg = "transparent")