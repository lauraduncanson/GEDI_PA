library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)


getmode <- function(v,na.rm=TRUE) {
  uniqv <- unique(v)
  if(na.rm==TRUE){
    uniqv <- unique(na.omit(v))
  }
  return(uniqv[which.max((tabulate(match(v,uniqv))))])
} 
  
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
#lossrds.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso3,"/", sep="")
write.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/", sep="")
  
cont_country_list <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair1.csv")
  
country_list <- cont_country_list$iso3
#summary(cont_country_list)
all_lost_pa_vs_ctrl <- data.frame()

lost_pa_vs_ctrl_columns <- c("Continent", "iso3", "pa_id", "status", "wwfbiome","area_loss")


for(j in 1:length(country_list)){
  iso <- country_list[j]
  curr_conti_list <- cont_country_list %>% filter(iso3 == iso)
  curr_conti <- unique(curr_conti_list$continent)
  #print(curr_conti)
  print(paste("Processing ", iso, " in ", curr_conti,sep="")) 
  
  lossrds.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso,"/", sep="")

  pa_loss_files <- list.files(lossrds.f.path,paste(iso,"_PA",sep=""))
  ctrl_loss_files <- list.files(lossrds.f.path, paste(iso,"_Ctrl",sep=""))
  
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
    
    
    lost_pa_vs_ctrl <- cbind(paste(curr_conti), paste(iso),all_pa_id[i],1,wwfbiome,lost_pa_area)
    colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
    #colnames(all_lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    
    
    lost_pa_vs_ctrl <- cbind(paste(curr_conti), paste(iso), all_pa_id[i],0, wwfbiome,lost_ctrl_area)
    colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
    
  }
  
  #print(mean(ctrl_pa_loss_area_diff))
} 

all_lost_pa_vs_ctrl$Continent <- as.factor(all_lost_pa_vs_ctrl$Continent)
all_lost_pa_vs_ctrl$iso3 <- as.factor(all_lost_pa_vs_ctrl$iso3)
all_lost_pa_vs_ctrl$status <- as.factor(all_lost_pa_vs_ctrl$status)
all_lost_pa_vs_ctrl$wwfbiome <- as.factor(all_lost_pa_vs_ctrl$wwfbiome)
  
  
all_lost_pa_vs_ctrl <- all_lost_pa_vs_ctrl %>% mutate(statusname = ifelse(status == 1, "Protected","Control"))
all_lost_pa_vs_ctrl$statusname <- as.factor(all_lost_pa_vs_ctrl$statusname)
  
write.csv(all_lost_pa_vs_ctrl, paste(write.f.path,"all_pa_vs_ctrl_abs_area_loss.csv",sep=""))

  #pa_area <- all_lost_pa_vs_ctrl %>% filter(status == 1)
  #ctrl_area <- all_lost_pa_vs_ctrl %>% filter(status == 0)
  
  #res <- wilcox.test(pa_area$area_loss, ctrl_area$area_loss)#, var.equal = TRUE)
  #res
