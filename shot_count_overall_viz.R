#post proxessing script
# read in the iso3 level output to get the shot count per 1km pixel to plot a hist 

fpath <- "/gpfs/data1/duncansongp/leitoldv/WDPA_GEDI_extract"
dir(fpath)

shotInCell <- data.frame()

for (d in dir(fpath)){
  dpath <- paste(fpath, d,sep="/")
  coutf <- list.files(dpath, pattern = "GEDI|.RDS", full.names = T)
  cout <- readRDS(coutf)
  iso3 <-  d %>% strsplit("_") %>% unlist() %>% .[1]
  print(iso3)
  tmp <-  data.frame(iso3=rep(iso3, nrow(cout)), status=as.factor(cout$status), gshot=cout$gshot_counts, stringsAsFactors=F)
  shotInCell <- shotInCell %>% rbind(tmp, stringsAsFactors=F)
}

print(dim(shotInCell))

# plot
p <- shotInCell %>%
  ggplot( aes(x=gshot)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Number of GEDI shots in 1km pixel (N=442,116 ISO3=86)") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )+
  labs(x = "Number of GEDI shots in 1km Pixel")
p

# calculate the mean, variance, counts b/t control and treatment from each country, and calculate the cumulative mean and sd

metrics <- data.frame()

for (d in dir(fpath)){
  dpath <- paste(fpath, d,sep="/")
  coutf <- list.files(dpath, pattern = "GEDI|.RDS", full.names = T)
  cout <- readRDS(coutf)
  iso3 <-  d %>% strsplit("_") %>% unlist() %>% .[1]
  print(iso3)
  tmp <-  data.frame(iso3=rep(iso3, nrow(cout)), status=as.factor(cout$status), rh50=cout$rh_050, rh98=cout$rh_098, 
                     cover=cout$cover, pai=cout$pai,desig=cout$DESIG_ENG, biome=cout$wwfbiom,
                     stringsAsFactors=F)
  metrics <- metrics %>% rbind(tmp, stringsAsFactors=F)
}

print(dim(metrics))


metrics %>%
  ggplot(aes(fill=status, x=status, y=pai)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
  scale_fill_viridis(discrete=T, name="") +
  theme_ipsum()  +
  xlab("") +
  ylab("PAI") +
  ylim(0,10)+
  geom_boxplot(width=0.1)



metrics%>%
  filter(!is.na(status)) %>% 
  dplyr::select(status, rh98, rh50, cover, pai)%>%
  gather(CH_metrics, values, rh98, rh50, cover, pai)%>%
  mutate(values=round(values,2))->metric_sub
dim(metric_sub)
metric_sub$CH_metrics=factor(metric_sub$CH_metrics,levels=c("rh98","rh50","cover", "pai"))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
p <- ggplot(metric_sub, aes(CH_metrics, values, fill=status))
t=table(metric_sub$status)  #total metrics for each dist_bin; for legend
t=t[t!=0]
names(t)=c("control","treatment")
metric_sub$CH_metrics%>%unique()%>%length()->n  #how many metrics used
p + geom_violin(position = position_dodge(1),width=0.8, trim=F,na.rm=F) + 
  facet_wrap(~CH_metrics, scale="free",ncol=2)+
  scale_fill_manual(name = "PA (treatments) vs. Controls", 
                    labels = c(paste(names(t), "n=", t/n,sep=" ")),
                    values = viridis(length(t)))+
  labs(title = "GEDI metrics for test PA1 and controls")+
  stat_summary(fun.data="mean_sdl",  
               geom="crossbar",width=0.05,alpha=0.6,color="white", size=0.01,
               position = position_dodge(1))


#stat_summary(fun.y=median, geom="point", size=2, color="red",position = position_dodge(1))
p + geom_violin(position = position_dodge(1),width=0.8, trim=F,na.rm=F) + 
  # facet_wrap(~CH_metrics, scale="free",ncol=2)+
  scale_fill_manual(name = "PA (treatments) vs. Controls", 
                    labels = c(paste(names(t), "n=", t/n,sep=" ")),
                    values = viridis(length(t)))+
  labs(title = "GEDI metrics for test PA and controls",subtitle = "Overplot: mean and SD")+
  stat_summary(
    fun.ymin = function(z) { quantile(z,0.25) },
    fun.ymax = function(z) { quantile(z,0.75) },
    fun.y = median,
    size=0.3, width=0.3,
    geom = "pointrange", color="red",position = position_dodge(1))+
  facet_wrap_custom(~CH_metrics, scales = "free", ncol = 2, scale_overrides = list(
    scale_override(1, scale_y_continuous(limits = c(0,50))),
    scale_override(2, scale_y_continuous(limits = c(0,20))),
    scale_override(3, scale_y_continuous(limits = c(0,1))),
    scale_override(4, scale_y_continuous(limits = c(0,4)))
  ))






