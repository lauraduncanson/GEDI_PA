library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)
library(parallel)

analyze_PA_vs_Ctrl_area_loss <- function(iso3) {
  
getmode <- function(v,na.rm=TRUE) {
  uniqv <- unique(v)
  if(na.rm==TRUE){
    uniqv <- unique(na.omit(v))
  }
  return(uniqv[which.max((tabulate(match(v,uniqv))))])
} 

print(paste("Currently processing ", iso3,sep=""))

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
lossrds.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso3,"/", sep="")
write.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso3,"/", sep="")
#iso3 <- "BRA"
pa_loss_files <- list.files(lossrds.f.path,paste(iso3,"_PA",sep=""))
ctrl_loss_files <- list.files(lossrds.f.path, paste(iso3,"_Ctrl",sep=""))

country_pa <- data.frame()
country_ctrl <- data.frame()

for(i in 1:length(pa_loss_files)){
  
  current_pa_tile_data <- readRDS(paste(lossrds.f.path,pa_loss_files[i],sep=""))
  if(dim(country_pa)[1] == 0){
    country_pa <- current_pa_tile_data
  }
  else {
    country_pa <- rbind(country_pa, current_pa_tile_data)
  }
  
}

for(i in 1:length(ctrl_loss_files)){
  
  current_ctrl_tile_data <- readRDS(paste(lossrds.f.path,ctrl_loss_files[i],sep=""))
  if(dim(country_ctrl)[1] == 0){
    country_ctrl <- current_ctrl_tile_data
  }
  else {
    country_ctrl <- rbind(country_ctrl, current_ctrl_tile_data)
  }
  
}

country_pa <- country_pa %>% drop_na()
country_ctrl <- country_ctrl %>% drop_na()

unique_pa_paid <- unique(country_pa$pa_id)
unique_ctrl_paid <- unique(country_ctrl$pa_id)

all_pa_id <- unique(c(unique_pa_paid, unique_ctrl_paid))

all_lost_pa_vs_ctrl <- data.frame()

lost_pa_vs_ctrl_columns <- c("pa_id", "status", "wwfbiome","area_loss")

ctrl_pa_loss_area_diff <- data.frame()
ctrl_pa_loss_area_diff_cols <- c("pa_id", "wwfbiome","area_loss_diff")

for(i in 1:length(all_pa_id)){
  
  lost_pa_vs_ctrl <- as.data.frame(matrix(ncol = length(lost_pa_vs_ctrl_columns), nrow=0))
  colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  
  current_pa_data <- country_pa %>% filter(pa_id == all_pa_id[i])
  current_ctrl_data <- country_ctrl %>% filter(pa_id == all_pa_id[i])
  
  if(dim(current_pa_data)[1] == 0){
    lost_pa_area <- 0
  }
  else{
    lost_pa_area <- sum(current_pa_data$loss)
    wwfbiome <- getmode(current_pa_data$wwfbiom)
  }
  
  if(dim(current_ctrl_data)[1] == 0){
    lost_ctrl_area <- 0
  }
  else{
    lost_ctrl_area <- sum(current_ctrl_data$loss)
    wwfbiome <- getmode(current_ctrl_data$wwfbiom)
  }
  
  
  lost_pa_vs_ctrl <- cbind(all_pa_id[i],1,wwfbiome,lost_pa_area)
  colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
  #colnames(all_lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  
  
  lost_pa_vs_ctrl <- cbind(all_pa_id[i],0, wwfbiome,lost_ctrl_area)
  colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
  
  area_diff <- lost_ctrl_area-lost_pa_area
  
  ctrl_pa_loss_area_diff_row <- cbind(all_pa_id[i],wwfbiome,lost_ctrl_area-lost_pa_area)
  colnames(ctrl_pa_loss_area_diff_row) = ctrl_pa_loss_area_diff_cols
  
  ctrl_pa_loss_area_diff <- rbind(ctrl_pa_loss_area_diff, ctrl_pa_loss_area_diff_row)
}

#print(mean(ctrl_pa_loss_area_diff))

all_lost_pa_vs_ctrl$status <- as.factor(all_lost_pa_vs_ctrl$status)
all_lost_pa_vs_ctrl$wwfbiome <- as.factor(all_lost_pa_vs_ctrl$wwfbiome)

all_lost_pa_vs_ctrl <- all_lost_pa_vs_ctrl %>% mutate(statusname = ifelse(status == 1, "Protected","Control"))
all_lost_pa_vs_ctrl$statusname <- as.factor(all_lost_pa_vs_ctrl$statusname)

all_lost_pa_vs_ctrl_forest <- all_lost_pa_vs_ctrl %>% filter(wwfbiome == 1 | wwfbiome == 2 | wwfbiome == 3 | wwfbiome ==4 | wwfbiome == 5 | wwfbiome == 6 | wwfbiome == 7 | wwfbiome == 14)
write.csv(all_lost_pa_vs_ctrl, paste(write.f.path,iso3,"_pa_vs_ctrl_abs_area_loss.csv",sep=""))
write.csv(ctrl_pa_loss_area_diff, paste(write.f.path,iso3,"_pa_vs_ctrl_area_diff.csv",sep=""))

ctrl_pa_loss_area_diff$wwfbiome <- as.factor(ctrl_pa_loss_area_diff$wwfbiome)
ctrl_pa_loss_area_diff_forest <- ctrl_pa_loss_area_diff %>% filter(wwfbiome == 1 | wwfbiome == 2 | wwfbiome == 3 | wwfbiome ==4 | wwfbiome == 5 | wwfbiome == 6 | wwfbiome == 7 | wwfbiome == 14)

write.csv(all_lost_pa_vs_ctrl_forest, paste(write.f.path,iso3,"_for_pa_vs_ctrl_abs_area_loss.csv",sep=""))
write.csv(ctrl_pa_loss_area_diff_forest, paste(write.f.path,iso3,"_for_pa_vs_ctrl_area_diff.csv",sep=""))


#pa_area <- all_lost_pa_vs_ctrl %>% filter(status == 1)
#ctrl_area <- all_lost_pa_vs_ctrl %>% filter(status == 0)

#res <- wilcox.test(pa_area$area_loss, ctrl_area$area_loss)#, var.equal = TRUE)
#res
}

cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv")

country_list <- cont_country_list$iso3
#country_list <- c("BRA")
mcmapply(FUN = analyze_PA_vs_Ctrl_area_loss,country_list, mc.cores = 10)

