matchResdir <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/"
countryResFiles <- list.files(matchResdir, pattern = "_wk18", recursive = TRUE, full.names = TRUE)
countryResFiles %>% head()
mr_all <- data.frame()
for (f in countryResFiles){
  mr <- readRDS(f) 
  if(!is.null(mr) && nrow(mr)!=0){
    mr_sub <- mr %>% dplyr::select(status, propensity_score, UID)
  } else{
    mr_sub <- mr
  }
  
  mr_all <- rbind(mr_sub, mr_all)
}

p <- mr_all %>%
  ggplot( aes(x=propensity_score, colour=status)) +
  geom_density(size=1)+
  scale_color_manual(values=c("#de9b16", "#17a63d"), labels=c("Control","Protected/Treated"), name="Matched status")+
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))+
  theme(text=element_text(family="Helvetica", face="bold", size=16),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16),axis.title = element_text(family="Helvetica", face="bold", size=16),
        rect = element_rect(fill = "transparent"))+theme_bw()+xlab("Propensity score")+ylab("Density")+xlim(-0.01,1)

p

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig_propensity_score_distribution_2.png", 
                plot=p, width=6, height=6,
                units = "in", dpi="print",bg = "transparent")
