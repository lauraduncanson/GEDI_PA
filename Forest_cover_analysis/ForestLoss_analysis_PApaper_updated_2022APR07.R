################################################################################
## Sruthi's Forest Cover Loss analysis (PAs vs Ctrls) updated -- APR 07, 2022
################################################################################
library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)
library(parallel)

f.path <- "/Users/veronika/GEDI_global_PA/"
#
#Codes are in: /gpfs/data1/duncansongp/sruthikp/GEDI_PA/final_codes/forest_cover_loss_codes
#
#-------------------------------------------------------------------------------
#STEP1: Run “merge_country_level_RDS.R” to get the PA and Control files merged at the country level
#(i.e. matched grid cells) from "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/iso3"
#Resulting files are stored here: “/gpfs/data1/duncansongp/GEDI_global_PA/Country_level_RDS/”
#-------------------------------------------------------------------------------
#STEP2: Run “processing_PA_loss_mask_layer_cell_by_cell.R” and “processing_Ctrl_loss_mask_layer_cell_by_cell.R” parallelly,
#which in turn parallelly processes country-level merged PA and control files generated from the previous step respectively
#to get the area of forest cover loss at cell-level for each matched PA and control at country-level.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#STEP3: Run “analyze_pa_and_ctrl_area_loss.R” to summarize the mean forest loss area
#and area loss difference per matched PA and control polygon.
##------------------------------------------------------------------------------
f.path <- "/Users/veronika/GEDI_global_PA/"
## read in PA level AGBD difference
diffAGBDall <- readRDS(paste(f.path,"diff_in_l4a_AGBD_by_PA_v2.rds",sep=""))  ##all PAs
#diffAGBDfor <- readRDS(paste(f.path,"diff_in_l4a_AGBD_by_PA_forest_v2.rds",sep=""))  ##forests only
head(diffAGBDall)
length(unique(diffAGBDall$pa_id))
##
##
getmode <- function(v,na.rm=TRUE) {
  uniqv <- unique(v)
  if(na.rm==TRUE){
    uniqv <- unique(na.omit(v))
  }
  return(uniqv[which.max((tabulate(match(v,uniqv))))])
} 

cont_country_list <- read.csv(paste(f.path,"iso3_region_pair1.csv",sep=""))
country_list <- cont_country_list$iso3
country_list

for(c in 1:length(country_list)){
  iso3 <- country_list[c]
  continent <- cont_country_list[cont_country_list$iso3 == iso3, "continent"]
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
    } else {
      country_pa <- rbind(country_pa, current_pa_tile_data)
    }
    
  }
  head(country_pa)  
  
  for(i in 1:length(ctrl_loss_files)){
    
    current_ctrl_tile_data <- readRDS(paste(lossrds.f.path,ctrl_loss_files[i],sep=""))
    if(dim(country_ctrl)[1] == 0){
      country_ctrl <- current_ctrl_tile_data
    } else {
      country_ctrl <- rbind(country_ctrl, current_ctrl_tile_data)
    }
    
  }
  head(country_ctrl)
  
  country_pa <- country_pa %>% drop_na()
  country_ctrl <- country_ctrl %>% drop_na()
  
  unique_pa_paid <- unique(country_pa$pa_id)
  unique_ctrl_paid <- unique(country_ctrl$pa_id)
  
  all_pa_id <- unique(c(unique_pa_paid, unique_ctrl_paid))
  
  all_lost_pa_vs_ctrl <- data.frame()
  
  lost_pa_vs_ctrl_columns <- c("pa_id", "continent", "iso3", "status", "wwfbiome",
                               "mean_area_loss", "cell_count")
  
  ctrl_pa_loss_area_diff <- data.frame()
  ctrl_pa_loss_area_diff_cols <- c("pa_id", "continent", "iso3", "wwfbiome",
                                   "mean_area_loss_pa", "pa_cell_count",
                                   "mean_area_loss_ctrl", "ctrl_cell_count",
                                   "meanAGBD_PA", "meanAGBD_control", "absolute_diff_AGBD", "percent_diff_AGBD")
  
  for(l in 1:length(all_pa_id)){
    
    lost_pa_vs_ctrl <- as.data.frame(matrix(ncol = length(lost_pa_vs_ctrl_columns), nrow=0))
    colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    
    current_pa_data <- country_pa %>% filter(pa_id == all_pa_id[l])
    current_ctrl_data <- country_ctrl %>% filter(pa_id == all_pa_id[l])
    print(paste("pa:",nrow(current_pa_data),sep=""))
    print(paste("ctrl:",nrow(current_ctrl_data),sep=""))
    
    if(dim(current_pa_data)[1] == 0){
      lost_pa_area <- 0
    } else {
      lost_pa_area <- mean(current_pa_data$loss)  ###CHANGED sum() to mean() -- APR 7 2022
      wwfbiome <- getmode(current_pa_data$wwfbiom)
      lost_pa_celltotal <- nrow(current_pa_data)
    }
    
    if(dim(current_ctrl_data)[1] == 0){
      lost_ctrl_area <- 0
    } else {
      lost_ctrl_area <- mean(current_ctrl_data$loss)    ###CHANGED sum() to mean() -- APR 7 2022
      wwfbiome <- getmode(current_ctrl_data$wwfbiom)
      lost_ctrl_celltotal <- nrow(current_ctrl_data)
    }
    
    lost_pa_vs_ctrl <- cbind(all_pa_id[l], as.character(continent), as.character(iso3), 1, wwfbiome, lost_pa_area, lost_pa_celltotal)
    colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
    #colnames(all_lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    
    lost_pa_vs_ctrl <- cbind(all_pa_id[l], as.character(continent), as.character(iso3), 0, wwfbiome, lost_ctrl_area, lost_ctrl_celltotal)
    colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
    all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
    
    #area_diff <- lost_ctrl_area-lost_pa_area
    #percent_area_diff <- 100*((lost_ctrl_area/lost_ctrl_celltotal)-(lost_pa_area/lost_pa_celltotal))
    
    if(length(diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"percent_diff_AGBD"])==1){
      meanAGBD_PA <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"meanAGBD_PA"]
      meanAGBD_control  <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"meanAGBD_control"]
      absolute_diff_AGBD <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"absolute_diff_AGBD"]
      percent_diff_AGBD  <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"percent_diff_AGBD"]
    } else {
      meanAGBD_PA <- NA
      meanAGBD_control <- NA
      absolute_diff_AGBD <- NA
      percent_diff_AGBD <- NA
    }
    
    #for(i in 1:length(all_pa_id)){
    #  if(length(diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[i],"percent_diff_AGBD"]) > 1){
    #    print(paste("PA id:",all_pa_id[i],sep=""))
    #    print(diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[i],"percent_diff_AGBD"])
    #  }
    #}
    
    ctrl_pa_loss_area_diff_row <- cbind(all_pa_id[l], as.character(continent), as.character(iso3), wwfbiome,
                                        lost_pa_area, lost_pa_celltotal, lost_ctrl_area, lost_ctrl_celltotal,
                                        meanAGBD_PA, meanAGBD_control, absolute_diff_AGBD, percent_diff_AGBD)
    colnames(ctrl_pa_loss_area_diff_row) = ctrl_pa_loss_area_diff_cols
    
    ctrl_pa_loss_area_diff <- rbind(ctrl_pa_loss_area_diff, ctrl_pa_loss_area_diff_row)
  }
  
  #print(mean(ctrl_pa_loss_area_diff))
  
  all_lost_pa_vs_ctrl$status <- as.factor(all_lost_pa_vs_ctrl$status)
  all_lost_pa_vs_ctrl$wwfbiome <- as.factor(all_lost_pa_vs_ctrl$wwfbiome)
  
  all_lost_pa_vs_ctrl <- all_lost_pa_vs_ctrl %>% mutate(statusname = ifelse(status == 1, "Protected","Control"))
  all_lost_pa_vs_ctrl$statusname <- as.factor(all_lost_pa_vs_ctrl$statusname)
  
  all_lost_pa_vs_ctrl_forest <- all_lost_pa_vs_ctrl %>% filter(wwfbiome == 1 | wwfbiome == 2 | wwfbiome == 3 | wwfbiome ==4 | wwfbiome == 5 | wwfbiome == 6 | wwfbiome == 7 | wwfbiome == 12 | wwfbiome == 14)
  
  write.csv(all_lost_pa_vs_ctrl, paste(write.f.path,iso3,"_pa_vs_ctrl_abs_area_loss_2022APR07.csv",sep=""))
  write.csv(ctrl_pa_loss_area_diff, paste(write.f.path,iso3,"_pa_vs_ctrl_area_diff_2022APR07.csv",sep=""))
  
  ctrl_pa_loss_area_diff$wwfbiome <- as.factor(ctrl_pa_loss_area_diff$wwfbiome)
  ctrl_pa_loss_area_diff_forest <- ctrl_pa_loss_area_diff %>% filter(wwfbiome == 1 | wwfbiome == 2 | wwfbiome == 3 | wwfbiome ==4 | wwfbiome == 5 | wwfbiome == 6 | wwfbiome == 7 | wwfbiome == 12 | wwfbiome == 14)
  
  write.csv(all_lost_pa_vs_ctrl_forest, paste(write.f.path,iso3,"_for_pa_vs_ctrl_abs_area_loss_2022APR07.csv",sep=""))
  write.csv(ctrl_pa_loss_area_diff_forest, paste(write.f.path,iso3,"_for_pa_vs_ctrl_area_diff_2022APR07.csv",sep=""))
  
}
#---------------------------------------------------------------------------------
#for(c in 1:length(country_list)){
iso3 <- "USA"
f.path <- "/Users/veronika/GEDI_global_PA/"
lossrds.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso3,"/", sep="")
write.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso3,"/", sep="")
#iso3 <- "BRA"
pa_loss_files <- c(list.files(lossrds.f.path,paste(iso3,"_east_PA",sep="")),
                   list.files(lossrds.f.path,paste(iso3,"_west_PA",sep="")),
                   list.files(lossrds.f.path,paste(iso3,"_pcfc_PA",sep="")))
ctrl_loss_files <- c(list.files(lossrds.f.path, paste(iso3,"_east_Ctrl",sep="")),
                     list.files(lossrds.f.path, paste(iso3,"_west_Ctrl",sep="")),
                     list.files(lossrds.f.path, paste(iso3,"_pcfc_Ctrl",sep="")))

country_pa <- data.frame()
country_ctrl <- data.frame()

for(i in 1:length(pa_loss_files)){
  
  current_pa_tile_data <- readRDS(paste(lossrds.f.path,pa_loss_files[i],sep=""))
  if(dim(country_pa)[1] == 0){
    country_pa <- current_pa_tile_data
  } else {
    country_pa <- rbind(country_pa, current_pa_tile_data)
  }
  
}
head(country_pa)  

for(i in 1:length(ctrl_loss_files)){
  
  current_ctrl_tile_data <- readRDS(paste(lossrds.f.path,ctrl_loss_files[i],sep=""))
  if(dim(country_ctrl)[1] == 0){
    country_ctrl <- current_ctrl_tile_data
  } else {
    country_ctrl <- rbind(country_ctrl, current_ctrl_tile_data)
  }
  
}
head(country_ctrl)

country_pa <- country_pa %>% drop_na()
country_ctrl <- country_ctrl %>% drop_na()

unique_pa_paid <- unique(country_pa$pa_id)
unique_ctrl_paid <- unique(country_ctrl$pa_id)

all_pa_id <- unique(c(unique_pa_paid, unique_ctrl_paid))

all_lost_pa_vs_ctrl <- data.frame()

lost_pa_vs_ctrl_columns <- c("pa_id", "continent", "iso3", "status", "wwfbiome",
                             "mean_area_loss", "cell_count")

ctrl_pa_loss_area_diff <- data.frame()
ctrl_pa_loss_area_diff_cols <- c("pa_id", "continent", "iso3", "wwfbiome",
                                 "mean_area_loss_pa", "pa_cell_count",
                                 "mean_area_loss_ctrl", "ctrl_cell_count",
                                 "meanAGBD_PA", "meanAGBD_control", "absolute_diff_AGBD", "percent_diff_AGBD")

for(l in 1:length(all_pa_id)){
  
  lost_pa_vs_ctrl <- as.data.frame(matrix(ncol = length(lost_pa_vs_ctrl_columns), nrow=0))
  colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  
  current_pa_data <- country_pa %>% filter(pa_id == all_pa_id[l])
  current_ctrl_data <- country_ctrl %>% filter(pa_id == all_pa_id[l])
  print(paste("pa:",nrow(current_pa_data),sep=""))
  print(paste("ctrl:",nrow(current_ctrl_data),sep=""))
  
  if(dim(current_pa_data)[1] == 0){
    lost_pa_area <- 0
  } else {
    lost_pa_area <- mean(current_pa_data$loss)  ###CHANGED sum() to mean() -- APR 7 2022
    wwfbiome <- getmode(current_pa_data$wwfbiom)
    lost_pa_celltotal <- nrow(current_pa_data)
  }
  
  if(dim(current_ctrl_data)[1] == 0){
    lost_ctrl_area <- 0
  } else {
    lost_ctrl_area <- mean(current_ctrl_data$loss)    ###CHANGED sum() to mean() -- APR 7 2022
    wwfbiome <- getmode(current_ctrl_data$wwfbiom)
    lost_ctrl_celltotal <- nrow(current_ctrl_data)
  }
  
  lost_pa_vs_ctrl <- cbind(all_pa_id[l], as.character(continent), as.character(iso3), 1, wwfbiome, lost_pa_area, lost_pa_celltotal)
  colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
  #colnames(all_lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  
  lost_pa_vs_ctrl <- cbind(all_pa_id[l], as.character(continent), as.character(iso3), 0, wwfbiome, lost_ctrl_area, lost_ctrl_celltotal)
  colnames(lost_pa_vs_ctrl) = lost_pa_vs_ctrl_columns
  all_lost_pa_vs_ctrl <- rbind(all_lost_pa_vs_ctrl, lost_pa_vs_ctrl)
  
  #area_diff <- lost_ctrl_area-lost_pa_area
  #percent_area_diff <- 100*((lost_ctrl_area/lost_ctrl_celltotal)-(lost_pa_area/lost_pa_celltotal))
  
  if(length(diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"percent_diff_AGBD"])==1){
    meanAGBD_PA <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"meanAGBD_PA"]
    meanAGBD_control  <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"meanAGBD_control"]
    absolute_diff_AGBD <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"absolute_diff_AGBD"]
    percent_diff_AGBD  <- diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[l],"percent_diff_AGBD"]
  } else {
    meanAGBD_PA <- NA
    meanAGBD_control <- NA
    absolute_diff_AGBD <- NA
    percent_diff_AGBD <- NA
  }
  
  #for(i in 1:length(all_pa_id)){
  #  if(length(diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[i],"percent_diff_AGBD"]) > 1){
  #    print(paste("PA id:",all_pa_id[i],sep=""))
  #    print(diffAGBDall[diffAGBDall[,"pa_id"]==all_pa_id[i],"percent_diff_AGBD"])
  #  }
  #}
  
  ctrl_pa_loss_area_diff_row <- cbind(all_pa_id[l], as.character(continent), as.character(iso3), wwfbiome,
                                      lost_pa_area, lost_pa_celltotal, lost_ctrl_area, lost_ctrl_celltotal,
                                      meanAGBD_PA, meanAGBD_control, absolute_diff_AGBD, percent_diff_AGBD)
  colnames(ctrl_pa_loss_area_diff_row) = ctrl_pa_loss_area_diff_cols
  
  ctrl_pa_loss_area_diff <- rbind(ctrl_pa_loss_area_diff, ctrl_pa_loss_area_diff_row)
}

#print(mean(ctrl_pa_loss_area_diff))

all_lost_pa_vs_ctrl$status <- as.factor(all_lost_pa_vs_ctrl$status)
all_lost_pa_vs_ctrl$wwfbiome <- as.factor(all_lost_pa_vs_ctrl$wwfbiome)

all_lost_pa_vs_ctrl <- all_lost_pa_vs_ctrl %>% mutate(statusname = ifelse(status == 1, "Protected","Control"))
all_lost_pa_vs_ctrl$statusname <- as.factor(all_lost_pa_vs_ctrl$statusname)

all_lost_pa_vs_ctrl_forest <- all_lost_pa_vs_ctrl %>% filter(wwfbiome == 1 | wwfbiome == 2 | wwfbiome == 3 | wwfbiome ==4 | wwfbiome == 5 | wwfbiome == 6 | wwfbiome == 7 | wwfbiome == 12 | wwfbiome == 14)

write.csv(all_lost_pa_vs_ctrl, paste(write.f.path,iso3,"_pa_vs_ctrl_abs_area_loss_2022APR07.csv",sep=""))
write.csv(ctrl_pa_loss_area_diff, paste(write.f.path,iso3,"_pa_vs_ctrl_area_diff_2022APR07.csv",sep=""))

ctrl_pa_loss_area_diff$wwfbiome <- as.factor(ctrl_pa_loss_area_diff$wwfbiome)
ctrl_pa_loss_area_diff_forest <- ctrl_pa_loss_area_diff %>% filter(wwfbiome == 1 | wwfbiome == 2 | wwfbiome == 3 | wwfbiome ==4 | wwfbiome == 5 | wwfbiome == 6 | wwfbiome == 7 | wwfbiome == 12 | wwfbiome == 14)

write.csv(all_lost_pa_vs_ctrl_forest, paste(write.f.path,iso3,"_for_pa_vs_ctrl_abs_area_loss_2022APR07.csv",sep=""))
write.csv(ctrl_pa_loss_area_diff_forest, paste(write.f.path,iso3,"_for_pa_vs_ctrl_area_diff_2022APR07.csv",sep=""))

#}

################################################################################
## join all the country PAs together into one database
################################################################################
f.path <- "/Users/veronika/GEDI_global_PA/"
#
cont_country_list <- read.csv(paste(f.path,"iso3_region_pair1.csv",sep=""))
country_list <- as.character(cont_country_list$iso3)
country_list <- country_list[-c(which(country_list=="USA_west"),which(country_list=="USA_pcfc"),which(country_list=="USA_east"))]
country_list <- c(country_list, "USA")
length(country_list)

all_countries_joined <- data.frame()
all_countries_joined_colnames <- c("X", "pa_id", "continent", "iso3", "wwfbiome",
                                   "mean_area_loss_pa", "pa_cell_count",
                                   "mean_area_loss_ctrl", "ctrl_cell_count",
                                   "meanAGBD_PA", "meanAGBD_control", "absolute_diff_AGBD", "percent_diff_AGBD")

for(c in 1:length(country_list)){
  iso3 <- country_list[c]
  print(iso3)
  write.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/",iso3,"/", sep="")
  iso3_data <- read.csv(paste(write.f.path,iso3,"_pa_vs_ctrl_area_diff_2022APR07.csv",sep=""))
  all_countries_joined <- rbind(all_countries_joined, iso3_data)
  print(nrow(all_countries_joined))
}

write.csv(all_countries_joined, paste(f.path,"/WDPA_lost_PA_polygons/New_method/all_PAs_forest_loss_VS_agbd_diff_2022APR07.csv",sep=""))

################################################################################
## area_loss_vs_extra_agb_Analysis
################################################################################
library(ggplot2)
library(ggpubr)
library(dplyr)

#setwd('C:/Users/sruth/OneDrive - UGent/UMDPostdoc/PA_analysis/')
setwd('/Users/veronika/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/')

all_pa_data <- read.csv('all_PAs_forest_loss_VS_agbd_diff_2022APR07.csv')
all_pa_data$mean_area_loss_diff <- 100*(all_pa_data$mean_area_loss_ctrl - all_pa_data$mean_area_loss_pa)
head(all_pa_data)

all_pa_data$wwfbiome <- as.factor(all_pa_data$wwfbiome)
all_pa_data$Continent <- as.factor(all_pa_data$continent)

trop_moist <- all_pa_data %>% filter(wwfbiome == 1)

biome.labs <- c("Trop. Moist", "Trop. Dry", "Trop. Conif.", "Temp. Broadleaf", "Temp. Conif." ,"Boreal", "Trop. Savannah", "Temp. Savannah", "Flooded Savannah", "Montane Savannah", "Tundra", "Mediterranian" ,"Desert","Mangrove")
names(biome.labs) <- c("1",
                       "2",            
                       "3",
                       "4",                     
                       "5",
                       "6",                                 
                       "7",
                       "8",
                       "9",
                       "10",
                       "11",
                       "12",  
                       "13",
                       "14")

ggscatter(all_pa_data, x = "mean_area_loss_diff", y = "absolute_diff_AGBD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "mean area loss difference (Ctrl - PA)", ylab = "mean AGBD difference (PA - Ctrl)") +
  facet_wrap(~wwfbiome, scales = "free", labeller = labeller(wwfbiome = biome.labs ))


conti.labs <- c("Africa", "S. America", "N. America", "Europe", "Asia", "Australia")
names(conti.labs) <- c("Af",
                       "SA",            
                       "US",
                       "Eu",                     
                       "As",
                       "Au")

ggscatter(trop_moist, x = "mean_area_loss_diff", y = "absolute_diff_AGBD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "mean area loss difference (Ctrl - PA)", ylab = "mean AGBD difference (PA - Ctrl)") +
  facet_wrap(~Continent, scales = "free", labeller = labeller(Continent = conti.labs ))


##--------------
