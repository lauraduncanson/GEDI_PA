################################################################################
## Sruthi's Forest Cover Loss analysis (PAs vs Ctrls) updated -- APR 07, 2022 v2
################################################################################
library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)
library(parallel)

#f.path <- "/Users/veronika/GEDI_global_PA/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
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
#f.path <- "/Users/veronika/GEDI_global_PA/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
## read in PA level AGBD difference
diffAGBDall <- readRDS(paste(f.path,"WDPA_L4b_output/diff_in_l4a_AGBD_by_PA_v2.rds",sep=""))  ##all PAs
#diffAGBDfor <- readRDS(paste(f.path,"WDPA_L4b_output/diff_in_l4a_AGBD_by_PA_forest_v2.rds",sep=""))  ##forests only
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
#f.path <- "/Users/veronika/GEDI_global_PA/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
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
#f.path <- "/Users/veronika/GEDI_global_PA/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
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
#setwd('/Users/veronika/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/')
setwd("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/")

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


#-------------------------------------------------------------------------------
#Step 4: Run “get_country_biome_level_mean_area_loss.R” and “get_country_biome_level_area_loss_significance.R”
#to get summarized average area of forest cover loss and its associated statistical significance values at country-biome-level.
#-------------------------------------------------------------------------------
#f.path <- "/Users/veronika/GEDI_global_PA/"
f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
loss.f.path <- paste(f.path,"/WDPA_lost_PA_polygons/New_method/", sep="")
cont_country_list <- read.csv(paste(f.path,"/iso3_region_pair2.csv", sep=""))
country_list <- cont_country_list$iso3
iso3_mean_pa_vs_ctrl_loss <- data.frame()
iso3_mean_pa_vs_ctrl_loss_cols <- c("Continent","iso3","wwfbiome",
                                    "mean_area_loss_diff","sd_area_loss_diff","var_area_loss_diff",
                                    "mean_percent_area_loss_diff","sd_percent_area_loss_diff","var_percent_area_loss_diff")

for(i in 1:length(country_list)){
  
  #i=which(country_list=="AFG") ##MRT, LSO, EGY, DJI, UZB, TKM, TJK, SAU, QAT, OMN, LBN, KWT, KAZ, JOR, ISR, ARE, AFG
  
  curr_conti_list <- cont_country_list %>% filter(iso3 == country_list[i])
  curr_conti <- unique(curr_conti_list$continent)
  print(paste("Processing ",  country_list[i], " in ", curr_conti,sep=""))
  
  #area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_pa_vs_ctrl_area_diff.csv", sep=""))
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_pa_vs_ctrl_area_diff_V2.csv", sep=""))
  area_diff_df
  
  head(area_diff_df)
  for (j in 1:14){
    #print(paste("Processing the biome",j, " in country ",  country_list[i], " in continent ", curr_conti,sep=""))
    area_diff_df_biome <- area_diff_df %>% filter(wwfbiome == j)
    #print(area_diff_df_biome)
    if(dim(area_diff_df_biome)[1] == 0){
      print(paste("Biome ",j," does not exist in ",country_list[i]))
      next
    }
    print(paste("Processing the biome",j, " in country ",  country_list[i], " in continent ", curr_conti,sep=""))
    mean_area_diff <- mean(area_diff_df_biome$area_loss_diff)
    sd_area_diff <- sd(area_diff_df_biome$area_loss_diff)
    var_area_diff <- var(area_diff_df_biome$area_loss_diff)
    mean_percent_area_diff <- mean(area_diff_df_biome$percent_area_loss_diff)
    sd_percent_area_diff <- sd(area_diff_df_biome$percent_area_loss_diff)
    var_percent_area_diff <- var(area_diff_df_biome$percent_area_loss_diff)
    
    curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(curr_conti),paste(country_list[i]),j,
                                            mean_area_diff,sd_area_diff,var_area_diff,
                                            mean_percent_area_diff,sd_percent_area_diff,var_percent_area_diff)
    colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
    iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
  }
  iso3_mean_pa_vs_ctrl_loss
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_biome_level_mean_area_loss_V2.csv",sep=""))
##
##-----------------------
##
iso3_mean_pa_vs_ctrl_loss <- data.frame()
iso3_mean_pa_vs_ctrl_loss_cols <- c("Continent", "iso3","wwfbiome", "p_val", "if_diff_significant","p_val2", "if_diff_significant2")

for(i in 1:length(country_list)){
  curr_conti_list <- cont_country_list %>% filter(iso3 == country_list[i])
  curr_conti <- unique(curr_conti_list$continent)
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_pa_vs_ctrl_abs_area_loss_V2.csv", sep=""))
  head(area_diff_df)
  for (j in 1:14){
    
    area_diff_df_biome <- area_diff_df %>% filter(wwfbiome == j)
    if(dim(area_diff_df_biome)[1] == 0){
      print(paste("Biome ",j," does not exist in the country"))
      next
    }
    
    pa_area_loss <- area_diff_df_biome %>% filter(status == 1)
    ctrl_area_loss <- area_diff_df_biome %>% filter(status == 0)
    
    print(paste("PA : ", dim(pa_area_loss)[1], " and control : ", dim(ctrl_area_loss)[1]))
    res <- wilcox.test(pa_area_loss$area_loss, ctrl_area_loss$area_loss)
    if(is.na(res$p.value)) {
      significant = "no"
    }
    else if(res$p.value <= 0.05){
      significant = "yes"
    } else {
      significant = "no"
    }
    
    res2 <- wilcox.test(100*(pa_area_loss$area_loss/pa_area_loss$numberOFcells), 
                        100*(ctrl_area_loss$area_loss/ctrl_area_loss$numberOFcells))
    if(is.na(res2$p.value)) {
      significant2 = "no"
    }
    else if(res2$p.value <= 0.05){
      significant2 = "yes"
    } else {
      significant2 = "no"
    }
    
    
    curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(curr_conti),paste(country_list[i]),j,res$p.value,significant,
                                            res2$p.value,significant2)
    colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
    iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
  }
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_biome_level_loss_significance_V2.csv", sep=""))

#-------------------------------------------------------------------------------
#Step 5: Run “get_country_level_mean_area_loss.R” and “get_country_level_area_loss_significance.R”
#to get summarized average area of forest cover loss and its associated statistical significance values at country-level.
#-------------------------------------------------------------------------------
iso3_mean_pa_vs_ctrl_loss <- data.frame()
iso3_mean_pa_vs_ctrl_loss_cols <- c("iso3","mean_area_loss_diff","sd_area_loss_diff","var_area_loss_diff",
                                    "mean_percent_area_loss_diff","sd_percent_area_loss_diff","var_percent_area_loss_diff")

for(i in 1:length(country_list)){
  
  #i=which(country_list=="MRT") ##MRT, LSO, EGY, DJI, UZB, TKM, TJK, SAU, QAT, OMN, LBN, KWT, KAZ, JOR, ISR, ARE, AFG
  
  print(country_list[i])
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_for_pa_vs_ctrl_area_diff_V2.csv", sep=""))
  #area_diff_df
  mean_area_diff <- mean(area_diff_df$area_loss_diff)
  sd_area_diff <- sd(area_diff_df$area_loss_diff)
  var_area_diff <- var(area_diff_df$area_loss_diff)
  mean_percent_area_diff <- mean(area_diff_df$percent_area_loss_diff)
  sd_percent_area_diff <- sd(area_diff_df$percent_area_loss_diff)
  var_percent_area_diff <- var(area_diff_df$percent_area_loss_diff)
  
  curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(country_list[i],sep=""),mean_area_diff,sd_area_diff,var_area_diff,
                                          mean_percent_area_diff,sd_percent_area_diff,var_percent_area_diff)
  colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
  iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_level_mean_area_loss_forest_V2.csv",sep=""))
##
##----------------------------------
##
iso3_mean_pa_vs_ctrl_loss <- data.frame()
iso3_mean_pa_vs_ctrl_loss_cols <- c("iso3","p_val","if_diff_significant","p_val2","if_diff_significant2")

for(i in 1:length(country_list)){
  area_diff_df <- read.csv(paste(loss.f.path,country_list[i],"/",country_list[i],"_pa_vs_ctrl_abs_area_loss_V2.csv", sep=""))
  
  pa_area_loss <- area_diff_df %>% filter(status == 1)
  ctrl_area_loss <- area_diff_df %>% filter(status == 0)
  print(paste("PA : ", dim(pa_area_loss)[1], " and control : ", dim(ctrl_area_loss)[1])) 
  res <- wilcox.test(pa_area_loss$area_loss, ctrl_area_loss$area_loss, exact = FALSE)
  print(res)
  
  if(is.na(res$p.value)) {
    significant = "no"
  }
  else if(res$p.value <= 0.05){
    significant = "yes"
  } else {
    significant = "no"
  }
  
  res2 <- wilcox.test(100*(pa_area_loss$area_loss/pa_area_loss$numberOFcells), 
                      100*(ctrl_area_loss$area_loss/ctrl_area_loss$numberOFcells))
  if(is.na(res2$p.value)) {
    significant2 = "no"
  }
  else if(res2$p.value <= 0.05){
    significant2 = "yes"
  } else {
    significant2 = "no"
  }
  
  
  curr_iso3_mean_pa_vs_ctrl_loss <- cbind(paste(country_list[i]),res$p.value,significant,res2$p.value,significant2)
  colnames(curr_iso3_mean_pa_vs_ctrl_loss) = iso3_mean_pa_vs_ctrl_loss_cols
  iso3_mean_pa_vs_ctrl_loss <- rbind(iso3_mean_pa_vs_ctrl_loss, curr_iso3_mean_pa_vs_ctrl_loss)
}

write.csv(iso3_mean_pa_vs_ctrl_loss,paste(loss.f.path,"Country_level_loss_significance_V2.csv", sep =""))

#-------------------------------------------------------------------------------
#Step 6: Run “figure_chloropeth.R” to generate the choropleth map to visualize
#country-level area loss between PAs and controls.
#-------------------------------------------------------------------------------
library(rgdal)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(rgeos)
library(maptools)
library(ggmap)
library(tidyr)
library(dplyr)

#setwd("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/")
#setwd("/Users/veronika/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/")
setwd("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/")

world_spdf <- readOGR(paste0(getwd(),"/","world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp",sep=""),verbose=FALSE)

emptymap <- st_read(paste0(getwd(),"/","world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp",sep=""))

mean_area_loss_df <- read.csv("Country_level_mean_area_loss_forest_V2.csv"); head(mean_area_loss_df)
mean_area_loss_df <- mean_area_loss_df %>% mutate(ISO3 = iso3)
mean_area_loss_df$ISO3 <- as.factor(mean_area_loss_df$ISO3)
#----------------------
sort(emptymap$ISO3)
sort((mean_area_loss_df$ISO3))

mean_area_loss_df[which(is.na(mean_area_loss_df[,"mean_percent_area_loss_diff"])),"ISO3"]
#----------------------

data_merged <- left_join(emptymap, mean_area_loss_df, by= "ISO3")

#jpeg("/home/leitoldv/choropleth1.jpg")
ggplot(data_merged)+
  geom_sf(aes(fill = mean_area_loss_diff)) + #aes(fill = cut(mean_area_loss_diff, zCuts))) +
  coord_sf(datum = NA) + 
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="blue", space ="Lab",aesthetics = "fill",na.value = "grey50", name = "Control - PA loss (sq. km.)") +
  labs(
    title = "PA effectiveness in reducing forest cover loss",
    subtitle = "") 
#dev.off()

#jpeg("/home/leitoldv/choropleth2.jpg")
ggplot(data_merged)+
  geom_sf(aes(fill = mean_percent_area_loss_diff)) + #aes(fill = cut(mean_area_loss_diff, zCuts))) +
  coord_sf(datum = NA) + 
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="blue", space ="Lab",
                       aesthetics = "fill",na.value = "grey50", name = "Control - PA loss (%area)") +
  labs(
    title = "PA effectiveness in reducing forest cover loss V2",
    subtitle = "") 
#dev.off()
