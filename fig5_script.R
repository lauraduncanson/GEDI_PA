library(dplyr)
library(tidyr)
library(gridExtra)
library(cowplot)

fpath <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_GEDI_extract3/"
dir(fpath)
# sub <- data.frame()
# dd <- list.files(path = paste(fpath, "iso_full_nodup",sep=""), pattern=".csv",full.names = TRUE, recursive=FALSE)
# #don't run this unless need to reconpile the pull dataset
# registerDoParallel(cores=round(mproc))
# getDoParWorkers()
# startTime <- Sys.time()
# sub =foreach(this_d=dd, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
#   tmp <- read.csv(this_d)%>%
#     # dplyr::filter(!is.na(pa_id)) %>%
#     dplyr::select(iso3, region, pft, wwfbiom,pa_id,status, AGBD, rh_098, cover, pai)
#   print(this_d)
#   sub <- sub %>% rbind(tmp, stringsAsFactors=FALSE)
#   return(sub)
# }
# stopImplicitCluster()
# tElapsed <- Sys.time()-startTime
saveRDS(sub_for_iso, "/gpfs/data1/duncansongp/GEDI_global_PA/figures/all_fullds_2_9_fore_iso.rds")

sub_for_iso <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/all_fullds_2_9_fore_iso.rds")


getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#source in the custome panel plot scaling script
source("/gpfs/data1/duncansongp/amberliang/scripts/individule_facet_scale.R")

ps=sub_for %>% dplyr::select(status, AGBD) %>% mutate(status=factor(status, levels=c(0, 1)))
psp=ggplot(data=ps, aes(x=AGBD, group=status, color=status)) +
  geom_density(adjust=1.5,  alpha=.4, size=1.1) +
  theme_bw()+theme(plot.title = element_text(size=15, face="bold"))+
  scale_color_manual(values=c("#bf812d","#35978f"), labels=c( "Control", "Protected Area"))+
  # scale_fill_manual(values=c("#35978f","#bf812d"), labels=c( "Protected Area","Control"))+
  labs(title = "Distributions of GEDI L4A AGBD in PA vs Control")+
  theme(text=element_text(family="Times", face="bold", size=14),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12)) 
# ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5.png", plot=psp, width=8, height=6, units = "in", bg = "transparent")

psz=sub_for %>% dplyr::select(status, AGBD, region) %>% mutate(status=factor(status, levels = c(0,1)))  #zoomed in on the AGBD density plot 

pspz1=ggplot(data=ps, aes(x=AGBD, group=status, color=status)) +
  geom_density(adjust=1.5,  alpha=.4,size=1.1) +
  coord_cartesian(xlim=c(0, 80), ylim=c(0, 0.022))+theme_bw()+theme(legend.position="none")+
  scale_color_manual(values=c("#bf812d","#35978f"), labels=c("Control", "Protected Area"))
# ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5_zoom1.png', sep=''), plot=pspz, width=10, height=12, units = "in", bg = "transparent")

options(scipen=999)
pspz2=ggplot(data=ps, aes(x=AGBD, group=status, color=status)) +
  geom_density(adjust=1.5,  alpha=.4, size=1.1) +
  coord_cartesian(xlim=c(80,500), ylim=c(0, 0.0075))+theme_bw()+theme(legend.position="none")+
  scale_color_manual(values=c("#bf812d","#35978f"), labels=c( "Control", "Protected Area"))
# ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5_zoom2.png', sep=''), plot=pspz2, width=10, height=12, units = "in", bg = "transparent")

# pspz3=ggplot(data=ps, aes(x=AGBD, group=status, color=status)) +
#   geom_density(adjust=1.5,  alpha=.4, size=1.1) +
#   coord_cartesian(xlim=c(500,1500), ylim=c(0, 0.00005))+theme_bw()+
#   scale_color_manual(values=c("#bf812d","#35978f"), labels=c("Control", "Protected Area"))+theme(legend.position="none")
# ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5_zoom3.png', sep=''), plot=pspz, width=10, height=12, units = "in", bg = "transparent")

psp_z <- psp + annotation_custom(ggplotGrob(pspz1), xmin = 100, xmax = 650, ymin = 0.005, ymax =0.021)+
  annotation_custom(ggplotGrob(pspz2), xmin = 650, xmax = 1300,ymin = 0.005, ymax =0.015)
# +annotation_custom(ggplotGrob(pspz3), xmin = 900, xmax = 1300, ymin = 0.025, ymax =0.05)

ggplot2::ggsave(filename=paste('/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5_zoom_fore2.png', sep=''), plot=psp_z, width=10, height=8, units = "in", bg = "transparent")


#break down by continent 
psp_c <- sub_for_iso %>% dplyr::select(status, AGBD, region) %>% 
  mutate(status=factor(status, level=c(1,0))) %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(AGBD)) %>% 
  ggplot(data=., aes(x=AGBD, group=status, color=status)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()+theme(plot.title = element_text(size=15, face="bold"))+
  scale_color_manual(values=c("#bf812d","#35978f"), labels=c( "Control","Protected Area"))+
  labs(title = "Distributions of GEDI L4A AGBD in PA vs Control by Continent")+
  # guides(color = FALSE)+
  facet_wrap_custom(~region, scales = "free", ncol = 3, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits = c(0,0.01))),
      scale_override(1, scale_x_continuous(limits = c(0,300))),
      scale_override(2, scale_y_continuous(limits = c(0,0.03))),
      scale_override(2, scale_x_continuous(limits = c(0,300))),
      scale_override(3, scale_y_continuous(limits = c(0,0.03))),
      scale_override(3, scale_x_continuous(limits = c(0,300))),
      scale_override(4, scale_y_continuous(limits = c(0,0.03))),
      scale_override(4, scale_x_continuous(limits = c(0,300))),
      scale_override(5, scale_y_continuous(limits = c(0,0.1))),
      scale_override(5, scale_x_continuous(limits = c(0,300))),
      scale_override(6, scale_y_continuous(limits = c(0,0.03))),
      scale_override(6, scale_x_continuous(limits = c(0,300)))))+
  theme(text=element_text(family="Times", face="bold", size=14),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12)) 

ggplot2::ggsave(filename=paste("/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5b_by_continent_fore.png", sep=''), plot=psp_c, width=8, height=6, units = "in", bg = "transparent")

#break down by biome
psp_b <- sub_for_iso %>% dplyr::select(status, AGBD, region, wwfbiom) %>% 
  mutate(status=factor(status, level=c(0,1))) %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(AGBD)) %>% 
  filter(!is.na(wwfbiom)) %>% 
  ggplot(data=., aes(x=AGBD, group=status, color=status)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()+theme(plot.title = element_text(size=15, face="bold"),
                   strip.text = element_text(size = 6))+
  scale_color_manual(values=c("#bf812d","#35978f"), labels=c( "Control","Protected Area"))+
  labs(title = "Distributions of GEDI L4A AGBD in PA vs Control by Biome")+
  scale_x_continuous(limits = c(0,300))+
  facet_wrap_custom(~wwfbiom, scales = "free", ncol = 3, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits = c(0,0.025))),
    scale_override(2, scale_y_continuous(limits = c(0,0.1))),
    scale_override(3, scale_y_continuous(limits = c(0,0.025))),
    scale_override(4, scale_y_continuous(limits = c(0,0.02))),
    scale_override(5, scale_y_continuous(limits = c(0,0.025))),
    scale_override(6, scale_y_continuous(limits = c(0,0.025))),
    scale_override(7, scale_y_continuous(limits = c(0,0.025))),
    scale_override(8, scale_y_continuous(limits = c(0,0.03)))))+
  theme(text=element_text(family="Times", face="bold", size=14),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12)) 

ggplot2::ggsave(filename=paste("/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5b_by_biome_fore.png", sep=''), plot=psp_b, width=8, height=6, units = "in", bg = "transparent")

#by PFT
psp_p <- sub_for_iso%>% dplyr::select(status, AGBD, region, wwfbiom,pft) %>% 
  mutate(status=factor(status, level=c(0,1))) %>% 
  # filter(!is.na(region)) %>% 
  filter(!is.na(AGBD)) %>% 
  # filter(!is.na(wwfbiom)) %>% 
  filter(!is.na(pft)) %>% 
  ggplot(data=., aes(x=AGBD, group=status, color=status)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()+theme(plot.title = element_text(size=15, face="bold"),
                   strip.text = element_text(size = 6))+
  scale_color_manual(values=c("#bf812d","#35978f"), labels=c( "Control","Protected Area"))+
  labs(title = "Distributions of GEDI L4A AGBD in PA vs Control by PFT")+
  scale_y_continuous(limits = c(0,300))+
  facet_wrap_custom(~pft, scales = "free", ncol = 2, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits = c(0,0.025))),
    scale_override(2, scale_y_continuous(limits = c(0,0.004))),
    scale_override(3, scale_y_continuous(limits = c(0,0.025))),
    scale_override(4, scale_y_continuous(limits = c(0,0.025)))))+
  theme(text=element_text(family="Times", face="bold", size=14),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12)) 

ggplot2::ggsave(filename=paste("/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5b_by_pft_fore.png", sep=''), plot=psp_p, width=8, height=6, units = "in", bg = "transparent")


#-----------------------violin plot for all obsevrtaions----------------------
sub_for_iso%>%
  filter(!is.na(status)) %>% 
  mutate(pa_stat=factor(status)) %>% 
  # mutate(biome=factor(wwfbiom)) %>%
  # mutate(pft=factor(pft)) %>%
  mutate(continent=factor(region)) %>%
  dplyr::select(pa_stat, continent,wwfbiom, AGBD, rh_098, cover, pai)%>%
  gather(GEDI_metrics, values, AGBD, rh_098, cover, pai)%>%
  mutate(values=round(values,2))->metric_sub
dim(metric_sub)
metric_sub$pa_stat=factor(metric_sub$pa_stat, levels=c(0, 1),
                           labels = c("Control", "Protected Area"))
metric_sub$GEDI_metrics=factor(metric_sub$GEDI_metrics, levels=c("AGBD","rh_098","cover","pai"),
                          labels = c("AGBD", "Max Height", "Canopy Cover", "PAI"))
t=table(metric_sub$pa_stat)  #total metrics for each dist_bin; for legend
t=t[t!=0]
names(t)=c("control","Protected/treated")
metric_sub$GEDI_metrics%>%unique()%>%length()->n  #how many metrics used

p <- ggplot(metric_sub, aes(GEDI_metrics, values, fill=pa_stat))
p2=p + geom_violin(position = position_dodge(1),width=0.8, trim=TRUE,na.rm=TRUE,color="#3b3b3b", size=0.3) + 
  facet_wrap(~GEDI_metrics, scale="free",ncol=2)+
  scale_fill_manual(name = "Protected vs. Controls", 
                    labels = c(paste(names(t), "n=", format(t/n, big.mark=","),sep=" ")),
                    values =c("#bf812d","#35978f"))+
  labs(title = "GEDI metrics and AGBD for PA and Control")+
  stat_summary(
    fun.ymin = function(z) { mean(z, na.rm=TRUE)- sd(z, na.rm=TRUE) },
    fun.ymax = function(z) { mean(z, na.rm=TRUE) + sd(z, na.rm=TRUE) },
    fun.y = function(z) {mean(z, na.rm=TRUE)},
    size=0.3, width=0.3,
    geom = "pointrange", color="#e41a1c",position = position_dodge(1))+
  facet_wrap_custom(~GEDI_metrics, scales = "free", ncol = 2, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits = c(0,400))),
    scale_override(2, scale_y_continuous(limits = c(0,35))),
    scale_override(3, scale_y_continuous(limits = c(0,1))),
    scale_override(4, scale_y_continuous(limits = c(0,3)))))+
  xlab("")+ylab("")+theme( strip.background = element_blank(),
                           strip.text.x = element_blank())+
  theme(text=element_text(family="Times", face="bold", size=14),
        legend.title=element_text(size=12), 
        legend.text=element_text( size=12)) 
  
ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/fig5c_violin_fore.png", plot=p2, width=8, height=10, units = "in", bg = "transparent")


