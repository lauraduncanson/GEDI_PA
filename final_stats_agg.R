
fpath <- "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_GEDI_extract3/"
iso_sum=list.files(paste(fpath,"iso_stats", sep=""), pattern="2.csv",full.names = TRUE)  
set.seed(42)

#------load in functions for stats aggregation------------------------------------
aggregate_stats <- function(metric, status, d1, d2){
  
  mean1=d1[,paste("mean",metric,"_",status,sep="")]
  mean2=d2[,paste("mean",metric,"_",status,sep="")]
  var1=(d1[,paste("sd",metric,"_",status,sep="")])^2
  var2=(d2[,paste("sd",metric,"_",status,sep="")])^2
  count1=as.numeric(d1[,paste("count_ttl","_",status,sep="")]- d1[,paste("ms",metric,"_",status,sep="")])
  count2=as.numeric(d2[,paste("count_ttl","_",status,sep="")] - d2[,paste("ms",metric,"_",status,sep="")])
  delta2=(mean1-mean2)^2
  new_count = count1 + count2
  new_mean = (count1 * mean1 + count2 * mean2) / new_count
  q1 = (count1-1)*var1 + count1*mean1^2
  q2 = (count2-1)*var2 + count2*mean2^2
  qc = q1 + q2
  new_sd = sqrt((qc - (count1+count2)*new_mean^2)/(count1+count2-1) )
  new_ms=as.numeric(d1[,paste("ms",metric,"_",status,sep="")]+d2[,paste("ms",metric,"_",status,sep="")])
  
  return(c(new_count, new_mean, new_sd, new_ms))
}

format_agg_stats <- function(d1, d2){
  t1=aggregate_stats("rh98","0", d1, d2)
  main$count_ttl_0=t1[1]
  main$meanrh98_0=t1[2]
  main$sdrh98_0=t1[3]
  main$msrh98_0=t1[4]
  t2=aggregate_stats("rh98","1", d1, d2)
  main$count_ttl_1=t2[1]
  main$meanrh98_1=t2[2]
  main$sdrh98_1=t2[3]
  main$msrh98_1=t2[4]
  t3=aggregate_stats("pai","0", d1, d2)
  main$meanpai_0=t3[2]
  main$sdpai_0=t3[3]
  main$mspai_0=t3[4]
  t4=aggregate_stats("pai","1", d1, d2)
  main$meanpai_1=t4[2]
  main$sdpai_1=t4[3]
  main$mspai_1=t4[4]
  t5=aggregate_stats("cov","0", d1, d2)
  main$meancov_0=t5[2]
  main$sdcov_0=t5[3]
  main$mscov_0=t5[4]
  t6=aggregate_stats("cov","1", d1, d2)
  main$meancov_1=t6[2]
  main$sdcov_1=t6[3]
  t7=aggregate_stats("agbd","0", d1, d2)
  main$meanagbd_0=t7[2]
  main$sdagbd_0=t7[3]
  main$msagbd_0=t7[4]
  t8=aggregate_stats("agbd","1", d1, d2)
  main$meanagbd_1=t8[2]
  main$sdagbd_1=t8[3]
  main$msagbd_1=t8[4]
  return(main)
}

no_region=c()
for (i in 1:length(iso_sum)){
  tmp=read.csv(iso_sum[i]) %>% dplyr::select(-c("continent", "medrh98_1","medrh98_0", "medpai_1","medpai_0", "medcov_1","medcov_0", 
                                                "medagbd_1","medagbd_0"))
  if(is.na(tmp$meanagbd_1)||is.na(tmp$meanagbd_0)){  
    print(tmp$iso3)
    no_region=c(no_region, as.character(tmp$iso3))
  }
}
# no_region
# no_region=c( "ISR", "KWT", "SLB")
# 
# se=c("IDN","KHM","LAO","MMR","MYS","PHL","THA","TLS","VNM")
# oc=c("ISR","KWT","SLB","VUT","WSM")

nonforest=c("Montane Grasslands & Shrublands", "Deserts & Xeric Shrublands", "Tundra", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
            "Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas")

#------world-wide summary------------------------------------------------------------- 
main=read.csv(iso_sum[1]) %>% dplyr::select(-c("iso3","continent", "medrh98_1","medrh98_0", "medpai_1","medpai_0", "medcov_1","medcov_0", 
                                               "medagbd_1","medagbd_0"))
for (i in 2:length(iso_sum)){
  print(iso_sum[i])
  tmp=read.csv(iso_sum[i]) %>% dplyr::select(-c("iso3","continent", "medrh98_1","medrh98_0", "medpai_1","medpai_0", "medcov_1","medcov_0", 
                                                "medagbd_1","medagbd_0"))
  if(is.na(tmp$meanagbd_1)||is.na(tmp$meanagbd_0)){
    main=main
  } else {
    main=format_agg_stats(main, tmp)
  }
}

main %>% 
  dplyr::select(-c(count_ttl_0,count_ttl_1, msrh98_0, msrh98_1, mscov_0, mscov_1, mspai_0, mspai_1, msagbd_0, msagbd_1)) %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  dplyr::mutate(percent_diff_agbd=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_PA) %>% 
  dplyr::mutate(percent_diff_rh=100*(meanrh98_1-meanrh98_0)/meanrh98_1) %>% 
  dplyr::mutate(percent_diff_cov=100*(meancov_1-meancov_0)/meancov_1) %>% 
  dplyr::mutate(percent_diff_pai=100*(meanpai_1-meanpai_0)/meanpai_1) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame()

#check results
sub_for_iso%>% group_by(status) %>%  
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)))%>% 
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status"))) %>% 
  dplyr::select(-c(count_ttl_0,count_ttl_1, msrh98_0, msrh98_1, mscov_0, mscov_1, mspai_0, mspai_1, msagbd_0, msagbd_1)) %>% 
  # mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  dplyr::mutate(percent_diff_agbd=100*(meanAGBD_1-meanAGBD_0)/meanAGBD_0) %>% 
  dplyr::mutate(percent_diff_rh=100*(meanrh98_1-meanrh98_0)/meanrh98_0) %>% 
  dplyr::mutate(percent_diff_cov=100*(meancov_1-meancov_0)/meancov_0) %>% 
  dplyr::mutate(percent_diff_pai=100*(meanpai_1-meanpai_0)/meanpai_0) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame()

#-------by continent summary----------------------------
all=data.frame()
for (f in iso_sum){
  # print(f)
  tmp=read.csv(f) %>%dplyr::select(-c("medrh98_1","medrh98_0", "medpai_1","medpai_0", "medcov_1","medcov_0", 
                                               "medagbd_1","medagbd_0"))
  if(is.na(tmp$continent)){
    print(tmp$iso3)
  }
  all=rbind(tmp,all)
}

all <- all %>% dplyr::filter(iso3%notin%no_region)

ctgroup=all %>% group_by(continent) %>% group_split()
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

ctmain %>% mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  dplyr::select(continent, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control, count_ttl_0, count_ttl_1, msagbd_0, msagbd_1) %>% 
  dplyr::mutate(percent_diff_agbd=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_PA) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame() ->ctmain_rfmt_AGBD

ctmain %>% mutate(meanMaxHeight_PA= meanrh98_1,meanMaxHeight_control=meanrh98_0, sdMaxHeight_PA=sdrh98_1, sdMaxHeight_control=sdrh98_0) %>% 
  dplyr::select(continent, meanMaxHeight_PA, meanMaxHeight_control, sdMaxHeight_PA, sdMaxHeight_control) %>% 
  dplyr::mutate(percent_diff_MaxHeight=100*(meanMaxHeight_PA-meanMaxHeight_control)/meanMaxHeight_PA) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame() ->ctmain_rfmt_ht

rownames(ctmain_rfmt) <- NULL
ctmain_rfmt$continent <- factor(ctmain_rfmt$continent, levels=c("Eu","US","SA","As","Au","Af"),
                                                             labels=c("Europe","North America", "South America", "Asia", "Oceania", "Africa"))

ggsave(plot=grid.table(ctmain_rfmt), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_by_continent.png", 
       height =1* nrow(ctmain_rfmt), width = 1.5*ncol(biom_sum), bg = "transparent")


# by continent summary AGBD
# sub <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/all_iso_fullds.rds")
# sub_for <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/all_fullds_2_9_fore_iso.rds")
conti_sum <- sub_for%>%
  group_by(iso3) %>% 
  dplyr::mutate(region2=getmode(region)) %>% 
  ungroup() %>% 
  group_by(region2, status) %>%
  # group_by(status) %>%
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status","region2","region"))) %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::select(region2, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control,count_ttl_0, count_ttl_1, msagbd_0, msagbd_1 ) %>%
  # dplyr::mutate(n_PA= format(count_ttl_1-msagbd_1, big.mark=",")) %>%
  # dplyr::mutate(n_control= format(count_ttl_0-msagbd_0, big.mark=",")) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>%
  dplyr::select(-c(count_ttl_0, count_ttl_1, msagbd_0, msagbd_1)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
conti_sum$region2 <- factor(ctmain_rfmt$continent, levels=c("Eu","US","SA","As","Au","Af"),
                                labels=c("Europe","North America", "South America", "Asia", "Oceania", "Africa"))
colnames(conti_sum)[1]="region"

ggsave(plot=grid.table(conti_sum), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_by_conti_fore.png", 
       height =1* nrow(conti_sum), width = 2.1*ncol(conti_sum), bg = "transparent")

#rh098 by continent
conti_sum_ht <- sub_for%>%
  group_by(iso3) %>% 
  dplyr::mutate(region2=getmode(region)) %>% 
  ungroup() %>% 
  group_by(region2, status) %>%
  # group_by(status) %>%
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)))%>%
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status","region2","region"))) %>%
  mutate(meanMaxHeight_PA= meanrh98_1,meanMaxHeight_control=meanrh98_0, sdMaxHeight_PA=sdrh98_1, sdMaxHeight_control=sdrh98_0) %>%
  dplyr::select(region2, meanMaxHeight_PA, meanMaxHeight_control, sdMaxHeight_PA, sdMaxHeight_control,count_ttl_0, count_ttl_1, msrh98_0, msrh98_1 ) %>%
  dplyr::mutate(n_PA= format(count_ttl_1-msrh98_1, big.mark=",")) %>%
  dplyr::mutate(n_control= format(count_ttl_0-msrh98_0, big.mark=",")) %>%
  dplyr::mutate(percent_diff_MaxHeight=100*(meanMaxHeight_PA-meanMaxHeight_control)/meanMaxHeight_control) %>%
  dplyr::select(-c(count_ttl_0, count_ttl_1, msrh98_0, msrh98_1)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()
ggsave(plot=grid.table(conti_sum_ht), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_by_conti_ht_fore.png", 
       height =1* nrow(conti_sum_ht), width = 2.1*ncol(conti_sum_ht), bg = "transparent")


# by biome summary 
# sub <- readRDS("/gpfs/data1/duncansongp/GEDI_global_PA/figures/all_iso_fullds.rds")
biom_sum <- sub_for%>%
  group_by(wwfbiom, status) %>% 
  # group_by(status) %>%  
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)))%>% 
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status","wwfbiom"))) %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  dplyr::select(wwfbiom, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control, count_ttl_0, count_ttl_1, msagbd_0, msagbd_1 ) %>% 
  # dplyr::mutate(n_PA= format(count_ttl_1-msagbd_1, big.mark=",")) %>%
  # dplyr::mutate(n_control= format(count_ttl_0-msagbd_0, big.mark=",")) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>%
  dplyr::select(-c(count_ttl_0, count_ttl_1, msagbd_0, msagbd_1)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame()
ggsave(plot=grid.table(biom_sum), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_by_biom_fore.png", 
       height =1* nrow(biom_sum), width = 2.1*ncol(biom_sum), bg = "transparent")

#by regional biome
biom_reg_sum <- sub_for%>%
  group_by(iso3) %>% 
  dplyr::mutate(region2=getmode(region)) %>% 
  ungroup() %>%
  # dplyr::filter(!is.na(region)) %>% 
  # dplyr::filter(!is.na(wwfbiom)) %>% 
  # dplyr::filter(!is.na(status)) %>% 
  group_by(region2, wwfbiom, status) %>% 
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)))%>% 
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("region","status","wwfbiom","region2"))) %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  dplyr::select(region2, wwfbiom, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control, count_ttl_0, count_ttl_1, msagbd_0, msagbd_1 ) %>% 
  dplyr::mutate(n_PA= format(count_ttl_1-msagbd_1, big.mark=",")) %>%
  dplyr::mutate(n_control= format(count_ttl_0-msagbd_0, big.mark=",")) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% 
  dplyr::select(-c(count_ttl_0, count_ttl_1, msagbd_0, msagbd_1 )) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame()

ggsave(plot=grid.table(biom_reg_sum), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_by_biom_reg_fore.png", 
       height =20, width = 18, bg = "transparent")


#ranking countries by AGBD
#read in the csv contains full names
names=read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/iso32fullnames.csv") %>% dplyr::mutate(iso3=as.character(iso3))

sub_for%>%
  dplyr::group_by(iso3) %>% 
  dplyr::mutate(region2=getmode(region)) %>% 
  dplyr::mutate(iso_mean_agbd=mean(AGBD, na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(iso3 = ifelse(as.character(iso3) %in% c("USA_east", "USA_pcfc", "USA_west"), "USA", as.character(iso3))) %>% 
  group_by(iso3, status) %>% 
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)),
                   iso_mean_agbd=getmode(iso_mean_agbd))%>% 
  dplyr::select(iso3,status, count_ttl, meanagbd, sdagbd, meanrh98, sdrh98, iso_mean_agbd, msagbd) %>% 
  tidyr::pivot_wider(names_from=status, values_from= c(count_ttl, meanagbd, sdagbd, meanrh98, sdrh98,msagbd, iso_mean_agbd)) %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  # dplyr::select(region2, wwfbiom, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control) %>% 
  # dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame()->sub_for_iso3

iso_agbd_diff <- sub_for_iso3 %>%
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>%
  dplyr::select(iso3, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control, count_ttl_1, msagbd_1, count_ttl_0,msagbd_0, iso_mean_agbd_0) %>%
  dplyr::mutate(n_PA= format(count_ttl_1-msagbd_1, big.mark=",")) %>%
  dplyr::mutate(n_control= format(count_ttl_0-msagbd_0, big.mark=",")) %>%
  dplyr::mutate(absolute_diff_AGBD=meanAGBD_PA-meanAGBD_control) %>%
  dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>%
  # dplyr::mutate(percent_diff_AGBD_normalized=percent_diff_AGBD/iso_mean_agbd_0) %>%
  dplyr::select(-c(count_ttl_0, count_ttl_1, msagbd_0, msagbd_1, iso_mean_agbd_0)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()

top_iso <- iso_agbd_diff[order(iso_agbd_diff$absolute_diff_AGBD, decreasing = TRUE),] %>% head(20) 
top_iso$iso3 <- names$name[match(top_iso$iso3, names$iso3)]
top_iso_rank1=top_iso %>% dplyr::select(iso3, absolute_diff_AGBD)
colnames(top_iso_rank1)[1]="Rank 1 country"
top_iso2 <- iso_agbd_diff[order(iso_agbd_diff$percent_diff_AGBD, decreasing = TRUE),] %>% head(20) 
top_iso2$iso3 <- names$name[match(top_iso2$iso3, names$iso3)]
top_iso_rank2=top_iso2 %>% dplyr::select(iso3, percent_diff_AGBD)
colnames(top_iso_rank2)[1]="Rank 2 country"
country3=rep(NA, 20)
normalized_diff_AGBD=rep(NA, 20)
ranks=cbind(top_iso_rank1, top_iso_rank2,country3,normalized_diff_AGBD)
ranks

ggsave(plot=grid.table(ranks), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_top_fore.png", 
       height =0.7* nrow(ranks), width = 2*ncol(ranks), bg = "transparent")

#ranked by rh98
sub_for%>%
  dplyr::group_by(iso3) %>% 
  dplyr::mutate(region2=getmode(region)) %>% 
  dplyr::mutate(iso_mean_rh98=mean(rh_098, na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(iso3 = ifelse(as.character(iso3) %in% c("USA_east", "USA_pcfc", "USA_west"), "USA", as.character(iso3))) %>% 
  group_by(iso3, status) %>% 
  dplyr::summarise(count_ttl=length(rh_098),
                   meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),msrh98=sum(is.na(rh_098)),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),mspai=sum(is.na(pai)),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),mscov=sum(is.na(cov)),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE), msagbd=sum(is.na(AGBD)),
                   iso_mean_rh98=getmode(iso_mean_rh98))%>% 
  dplyr::select(iso3,status, count_ttl, meanagbd, sdagbd, meanrh98, sdrh98, iso_mean_rh98, msrh98) %>% 
  tidyr::pivot_wider(names_from=status, values_from= c(count_ttl, meanagbd, sdagbd, meanrh98, sdrh98,msrh98, iso_mean_rh98)) %>% 
  mutate(meanAGBD_PA= meanagbd_1,meanAGBD_control=meanagbd_0, sdAGBD_PA=sdagbd_1, sdAGBD_control=sdagbd_0) %>% 
  # dplyr::select(region2, wwfbiom, meanAGBD_PA, meanAGBD_control, sdAGBD_PA, sdAGBD_control) %>% 
  # dplyr::mutate(percent_diff_AGBD=100*(meanAGBD_PA-meanAGBD_control)/meanAGBD_control) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as.data.frame()->sub_for_iso3

iso_ht_diff <- sub_for_iso3 %>%
  mutate(meanMaxHeight_PA= meanrh98_1,meanMaxHeight_control=meanrh98_0, sdMaxHeight_PA=sdrh98_1, sdMaxHeight_control=sdrh98_0) %>%
  dplyr::select(iso3, meanMaxHeight_PA, meanMaxHeight_control, sdMaxHeight_PA, sdMaxHeight_control, msrh98_1, count_ttl_0,count_ttl_1, msrh98_0, iso_mean_rh98_0) %>%
  dplyr::mutate(n_PA= format(count_ttl_1-msrh98_1, big.mark=",")) %>%
  dplyr::mutate(n_control= format(count_ttl_0-msrh98_0, big.mark=",")) %>%
  dplyr::mutate(percent_diff_MaxHeight=100*(meanMaxHeight_PA-meanMaxHeight_control)/meanMaxHeight_control) %>%
  dplyr::mutate(percent_diff_MaxHeight_normalized=percent_diff_MaxHeight/iso_mean_rh98_0) %>%
  dplyr::select(-c(count_ttl_0, count_ttl_1, msrh98_0, msrh98_1, iso_mean_rh98_0)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame()


top_iso_ht <- iso_ht_diff[order(iso_ht_diff$percent_diff_MaxHeight, decreasing = TRUE),] %>% head(20)
top_iso_ht$iso3 <- names$name[match(top_iso_ht$iso3, names$iso3)]
colnames(top_iso_ht)[1] <- "country"

ggsave(plot=grid.table(top_iso_ht), file = "/gpfs/data1/duncansongp/GEDI_global_PA/figures/summary_table_top_ht_fore.png", 
       height =0.7* nrow(top_iso_ht), width = 2*ncol(top_iso_ht), bg = "transparent")


