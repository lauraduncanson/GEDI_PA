#!/usr/bin/env Rscript

# This global processing script PART II is derived from the global processing notebook 
#the input can be the iso3 code (3-character) for one or multiple countries 

options(warn=-1)
options(dplyr.summarise.inform = FALSE)

packages <- c("sp","rgdal","sf","rgeos","dplyr","plyr","ggplot2","raster","mapview","stringr",
              "maptools","gridExtra","lattice","MASS","foreach","optmatch","doParallel","RItools",
              "rlang","tidyr","magrittr","viridis","ggmap","Hmisc","hrbrthemes","spatialEco","bit64","randomForest", "modelr")
package.check <- lapply(packages, FUN = function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)>=1) {
  
  iso3 <- args[1]  #country to process
  gediwk <- args[2]   #the # of weeks GEDI data to use
  mproc <- as.integer(args[3])#the number of cores to use for macthing 
}

cat("Step 0: Loading global variables to process country", iso3,"with GEDI data until week", gediwk, "\n")

f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/"
exclude <- c("mean_gHM","pop_cnt_2000")
tifs <- list.files(paste(f.path,"WDPA_input_vars_iso3/",iso3,"/",sep=""),full.names=T) %>% .[str_detect(., exclude, negate = TRUE)]
tifs.name <- str_match(tifs, "//\\s*(.*?)\\s*.tif")[,2]
ecoreg_key <- read.csv(paste(f.path,"wwf_ecoregions_key.csv",sep=""))
allPAs <- readRDS(paste(f.path,"WDPA_shapefiles/WDPA_polygons/",iso3,"_PA_poly.rds",sep=""))
MCD12Q1 <- raster(paste(f.path,"GEDI_ANCI_PFT_r1000m_EASE2.0_UMD_v1_projection_defined_6933.tif",sep=""))
projection(MCD12Q1) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
world_region <- raster(paste(f.path,"GEDI_ANCI_CONTINENT_r1000m_EASE2.0_UMD_v1_revised_projection_defined_6933.tif",sep=""))
projection(world_region) <- sp::CRS(paste("+init=epsg:",6933,sep=""))
adm <- readOGR(paste(f.path,"WDPA_countries/shp/",iso3,".shp",sep=""),verbose=FALSE)
adm_prj <- spTransform(adm, "+init=epsg:6933") 
load("/gpfs/data1/duncansongp/amberliang/trends.Earth/rf_noclimate.RData")
source("/gpfs/data1/duncansongp/amberliang/trends.Earth/git/GEDI_PA/matching_func.R")

#---------------STEP5. GEDI PROCESSING - using GEDI shots to extract the treatment/control status, also extract the MODIS PFT for AGB prediction---------------- 
# if (file.exists(paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",iso3,"_gedi_extracted_matching_wk",gediwk,".RDS", sep=""))){
cat(paste("Step 5: Performing WK ",gediwk,"GEDI extraction for", iso3,"\n"))


if(!dir.exists(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,"/",sep=""))){
  matched_PAs <- list.files(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,sep=""), pattern=".RDS", full.names = FALSE)
  
} else if (dir.exists(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,"/",sep=""))){
  pattern1 = c(paste("wk",gediwk,sep=""),"RDS")
  extracted_PAid <- list.files(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,"/",sep=""), full.names = F, pattern=paste0(pattern1, collapse="|"))%>% 
    readr::parse_number() %>% unique()
  matched_PAs<- list.files(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk, sep=""), pattern=paste("wk",gediwk,sep=""), full.names=F) 
  matched_PA_id <- matched_PAs %>% readr::parse_number()
  runPA_id <- matched_PA_id[!(matched_PA_id %in% extracted_PAid)]
  if (length(runPA_id)>0){
    Pattern2 <-  paste(runPA_id, collapse="|")
    runPA <-  matched_PAs[grepl(Pattern2,matched_PAs)]
    # runPA_ind <- str_detect(matched_PAs, paste(runPA_id, collapse = "|"))
    matched_PAs <-runPA
  } else {
    matched_PAs <- NULL
    cat("Step 5 already done for", iso3, "\n")
  }
}


registerDoParallel(cores=round(mproc))
getDoParWorkers()
startTime <- Sys.time()
foreach(this_rds=matched_PAs, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
  cat("Extracting for no. ", match(this_rds,matched_PAs),"pa out of", length(matched_PAs),"\n")
  id_pa <- this_rds %>% str_split("_") %>% unlist %>% .[3]
  matched <- readRDS(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_matching_results_wk",gediwk,".RDS", sep=""))
  if (is.null(matched)==TRUE) {
    cat("Matched result is null for PA", id_pa, "quitting...\n")
  } else if (!is.null(matched)==TRUE){
    mras  <- tryCatch(matched2ras(matched),
                      error=function(cond){
                        cat("Matched result is likely null for country", iso3,"pa", id_pa, "dimension of the match is", dim(matched),"\n")
                        # writeLines("Matched results is likely null for country", paste(f.path,"WDPA_log/",iso3,"_log_matching.txt", sep=""))
                        return(NULL)}) #convert the macthed df to a raster stack 
    if(table(mras$status[])[2]==0 | table(mras$status[])[1]==0){
      cat("Rasterized results unbalanced for PA", id_pa, "quitting...\n")
    } else {
      gedil2_f <- list.files(file.path(f.path,"WDPA_gedi_l2a+l2b_clean",iso3), full.names = TRUE)
      registerDoParallel(cores=round(mproc*0.5))
      iso_matched_gedi <- foreach(this_csv=gedil2_f, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
        ##add the GEDI l4a model prediction for AGB here :
        cat("Readng in no. ", match(this_csv, gedil2_f),"csv of ", length(gedil2_f),"csvs \n")
        gedi_l2  <- read.csv(this_csv) %>%
          dplyr::select(shot_number,lon_lowestmode, lat_lowestmode, starts_with("rh_"),cover, pai)%>%
          SpatialPointsDataFrame(coords=.[,c("lon_lowestmode","lat_lowestmode")],
                                 proj4string=CRS("+init=epsg:4326"), data=.) %>%spTransform(., CRS("+init=epsg:6933"))
        
        iso_matched_gedi_df <- data.frame()
        matched_gedi <- raster::extract(mras,gedi_l2, df=TRUE)
        matched_gedi_metrics <- cbind(matched_gedi,gedi_l2@data)
        
        matched_gedi_metrics_filtered <- matched_gedi_metrics %>% dplyr::filter(!is.na(status)) %>% 
          convertFactor(matched0 = matched,exgedi = .) 
        
        matched_gedi_l4a <-matched_gedi_metrics_filtered %>% 
          dplyr::mutate(
            LAT=lat_lowestmode,
            LON=lon_lowestmode,
            REGION=region,
            PFT=pft,
            RH_10=rh_010+100,
            RH_20=rh_020+100,
            RH_30=rh_030+100,
            RH_40=rh_040+100,
            RH_50=rh_050+100,
            RH_60=rh_060+100,
            RH_70=rh_070+100,
            RH_80=rh_080+100,
            RH_90=rh_090+100,
            RH_98=rh_098+100) %>% 
          modelr::add_predictions(model2, "AGBD")
        iso_matched_gedi_df <- rbind(matched_gedi_l4a,iso_matched_gedi_df)
        return(iso_matched_gedi_df)
      }
      stopImplicitCluster()
      cat("Done GEDI extraction for pa", id_pa,"in ",iso3,"\n")
      
      iso_matched_gedi_sub <- iso_matched_gedi %>% 
        dplyr::select("pa_id","shot_number","status","DESIG_ENG.x","wwfbiom","wwfecoreg","PADDD","pft","region","lon_lowestmode","lat_lowestmode",
                      "rh_010","rh_020", "rh_030", "rh_040",  "rh_050",  "rh_060" , "rh_070","rh_075",  "rh_080",  "rh_090",  "rh_098", "AGBD","cover","pai","REGION","PFT")  #write to individual country folder
      
      if (length(unique(iso_matched_gedi_sub$wwfbiom)) >1){
        pabiome <- iso_matched_gedi_sub$wwfbiom %>% unique() %>% gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',.,perl = TRUE)%>% str_c( collapse = "+")
      } else if (length(unique(iso_matched_gedi_sub$wwfbiom))==1){
        pabiome <- iso_matched_gedi_sub$wwfbiom %>% unique() %>% gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',.,perl = TRUE)
      } else {
        pabiome <- iso_matched_gedi_sub$wwfbiom %>% unique()
      }
      papaddd <- unique(iso_matched_gedi_sub$PADDD) %>% getmode()
      continent <- unique(iso_matched_gedi_sub$REGION) %>% getmode()
      
      dir.create(file.path(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,"/",sep="")))
      saveRDS(iso_matched_gedi_sub, file=paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_gedi_wk_",gediwk,"_conti_", continent,"_biome_",pabiome,"_paddd_",papaddd,".RDS", sep=""))
      cat(id_pa,"results is written to dir\n")
      # write.csv(iso_matched_gedi_sub, file=paste(f.path,"WDPA_GEDI_extract2/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_iso_matched_gedi_sub_wk_",gediwk,".csv", sep=""))
    }
  }
  return(NULL)
}
stopImplicitCluster()
tElapsed <- Sys.time()-startTime
cat(tElapsed, "for extracting all PAs in", iso3,"\n")
cat("Done GEDI extraction for pa in ",iso3,"\n")    

#---------------STEP6: [FIGURE 4B] Calculating per pa summary stats, 1 pa per row, contain shot#/PA---------------------------- 
gedi_paf <-list.files(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,sep=""), pattern=".RDS", full.names = TRUE)
cat(paste("Step 6: calculating per pa summary stats for", iso3,"\n"))
if (file.exists(paste(f.path,"WDPA_GEDI_extract3/pa_stats/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""))) {
  #Delete existing files in exists to avoid duplicate appending
  cat("old version for", iso3,"exists, removing...\n")
  file.remove(paste(f.path,"WDPA_GEDI_extract3/pa_stats/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""))
}

for (this_paf in gedi_paf){
  pa_metrics <- readRDS(this_paf) %>% unique()
  if (length(table(pa_metrics$status))<2) {
    cat(iso3, this_paf, "has 0 protected or treatment \n")
  } else if (table(pa_metrics$status)[1]!=0 && table(pa_metrics$status)[2]!=0) {
    pa_stats_summary <- pa_metrics %>%
      group_by(status) %>% 
      dplyr::mutate(pa_id=as.character(pa_id)) %>%
      dplyr::summarise(pa_id=na.omit(unique(pa_id)),
                       count=length(rh_098),meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE),medrh98=median(rh_098, na.rm = TRUE),
                       meanrh75=mean(rh_075,na.rm=TRUE), sdrh75=sd(rh_075,na.rm=TRUE), medrh75=median(rh_075,na.rm=TRUE),
                       meanrh50=mean(rh_050,na.rm=TRUE), sdrh50=sd(rh_050,na.rm=TRUE), medrh50=median(rh_050,na.rm=TRUE),
                       meanrh10=mean(rh_010,na.rm=TRUE ), sdrh10=sd(rh_010, na.rm=TRUE),medrh10=median(rh_010, na.rm=TRUE),
                       meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE), medpai=median(pai, na.rm=TRUE),
                       meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),
                       meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE),medagbd=median(AGBD, na.rm=TRUE),
                       wwfecoreg=getmode(wwfecoreg),REGION=getmode(REGION),
                       PADDD=getmode(PADDD))%>% 
      tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status"))) #writeLine to a large txt file where world pas stats are
    
    pa_stats_summary$iso3=iso3
    if(!file.exists(paste(f.path,"WDPA_GEDI_extract3/pa_stats/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""))){
      print("not exists")
      write.csv(pa_stats_summary, file=paste(f.path,"WDPA_GEDI_extract3/pa_stats/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""), row.names = F)
    } else if (file.exists(paste(f.path,"WDPA_GEDI_extract3/pa_stats/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""))){
      print("exists so appending to existing file")
      write.table(pa_stats_summary, file=paste(f.path,"WDPA_GEDI_extract3/pa_stats/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""),
                  sep=",", append=TRUE , row.names=FALSE, col.names=FALSE)   #will not overwrite but append to existing files
    }
  }
}
cat("Done summarizing pa-level stats for country",iso3,"\n")  

#---------------STEP7: [Figure 4A] Removing dup gedi shots in overlapping region, count shot#/PA w/o dups-------------------
gedi_paf <-list.files(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,sep=""), pattern=".RDS", full.names = TRUE)
fullds <- data.frame()
cat ("Step7a: Compiling country wide dataframe for",iso3, "and removing duplicates...\n")
for (c in gedi_paf){  #removing dups based on shot#
  print(c)
  tmpds=readRDS(c)
  tmpds$shot_number=as.character(tmpds$shot_number)
  fullds=rbind(fullds, tmpds)
  print(dim(fullds))
  fullds=fullds[!duplicated(fullds$shot_number), ]
  print(dim(fullds))
}
fullds$iso3=iso3
write.csv(fullds, file=paste(f.path,"WDPA_GEDI_extract3/iso_full_nodup/",iso3,"_country_full_nodup_wk",gediwk,".csv", sep=""), row.names = F)
cat(iso3,"Dup removed df is exported to /iso_full_nodup/ \n")

#rasterize the non-dup GEDI results to output the shots_per_1km results
cat("Step 7b: Summarizing #of GEDI shots per 1km pixel\n ")
# fullds <- read.csv(paste(f.path,"WDPA_GEDI_extract3/iso_full_nodup/",iso3,"_country_full_nodup_wk",gediwk,".csv", sep=""))
iso_gedi_spdf <- SpatialPointsDataFrame(coords=fullds[,c("lon_lowestmode","lat_lowestmode")],
                                                proj4string=CRS("+init=epsg:4326"), data=fullds) %>%spTransform(., CRS("+init=epsg:6933"))
ras <- crop(MCD12Q1, extent(iso_gedi_spdf)) #a little slow with buffer 
gcount_ras <- rasterize(coordinates(iso_gedi_spdf),ras, fun="count",background=NA)
names(gcount_ras) <- "gshot_counts"
gpid_ras <- rasterize(coordinates(iso_gedi_spdf),ras, fun=getmode,field=iso_gedi_spdf$pa_id,background=NA)
names(gpid_ras) <- "pid"
gattr_ras <- rasterize(iso_gedi_spdf@coords, ras, fun=getmode, field=iso_gedi_spdf$status, background=NA)
names(gattr_ras) <- "status"
gstack <- stack(gcount_ras,gpid_ras,gattr_ras)
g1km_sp <- as(gstack, 'SpatialPointsDataFrame')
g1km <- cbind(g1km_sp@coords, g1km_sp@data, country=iso3)
# dir.create(paste(f.path,"WDPA_GEDI_extract3/cell_stats/",sep=""))
write.csv(g1km, file=paste(f.path,"WDPA_GEDI_extract3/cell_stats/",iso3,"_cell_shots_wk",gediwk,".csv", sep=""))
cat(iso3,"1km pixel level shot count df is exported to /cell_stats/ \n")
rm(ras, gstack)

#---------------STEP8: Calculating per country summary stats, 1 country per row, summarize key stats for the country ---------------------    
cat("Step 8: Calculating country level summary stats for ", iso3,"\n ")
# fullds <- read.csv(paste(f.path,"WDPA_GEDI_extract3/iso_full_nodup/",iso3,"_country_full_nodup_wk",gediwk,".csv", sep=""))
iso_sum <- fullds %>%
  group_by(status) %>%  
  dplyr::summarise(count=length(rh_098),meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE), medrh98=median(rh_098, na.rm = TRUE),
                   meanrh75=mean(rh_075,na.rm=TRUE), sdrh75=sd(rh_075,na.rm=TRUE), medrh75=median(rh_075,na.rm=TRUE),
                   meanrh50=mean(rh_050,na.rm=TRUE), sdrh50=sd(rh_050,na.rm=TRUE),  medrh50=median(rh_050,na.rm=TRUE),
                   meanrh10=mean(rh_010,na.rm=TRUE ), sdrh10=sd(rh_010, na.rm=TRUE), medrh10=median(rh_010, na.rm=TRUE),
                   meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),  medpai=median(pai, na.rm=TRUE),
                   meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),  medcov=median(cover,na.rm=TRUE),
                   meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE), medagbd=median(AGBD, na.rm=TRUE))%>% 
  tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status"))) #writeLine to a large txt file where world pas stats are
iso_sum$iso3 <- iso3
continent <- fullds$region %>% unique() %>% getmode()
iso_sum$continent <- continent

write.csv(iso_sum, file=paste(f.path,"WDPA_GEDI_extract3/iso_stats/",iso3,"_country_stats_summary_wk",gediwk,".csv", sep=""), row.names = F)
cat(iso3,"country level summary stats is exported to /iso_stats/ \n")

#---------------[NOT NEEDED FOR NOW] STEP9: Calculate per biome summary stats, 1 biome per row, removing dup gedi shots in overlapping region------------------------
# gedi_paf <-list.files(paste(f.path,"WDPA_GEDI_extract3/",iso3,"_wk",gediwk,sep=""), pattern=".RDS", full.names = FALSE)
# iso_biome <- gedi_paf %>%str_match( "biome_\\s*(.*?)\\s*_paddd") %>% .[,2] %>% strsplit(string,split='+', fixed=TRUE) %>% unlist() %>% unique()
# 
# registerDoParallel(cores=round(mproc*0.5))
# foreach(this_biome=iso_biome, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
#  gedifs <- gedi_paf[grepl(this_biome, gedi_paf)]
#  fullds <- data.frame()
#  for (gedif in gedifs){
#    print(this_biome)
#    print(gedif)
#    tmpds <- readRDS(paste(f.path,"WDPA_GEDI_extract2/",iso3,"_wk",gediwk,"/",gedif, sep = "")) %>% 
#      dplyr::mutate(wwfbiom=gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',wwfbiom,perl = TRUE))%>% 
#      dplyr::filter(wwfbiom==this_biome)
#    tmpds$shot_number=as.character(tmpds$shot_number)
#    fullds=rbind(fullds, tmpds)
#    print(dim(fullds))
#    fullds=fullds[!duplicated(fullds$shot_number), ]
#    print(dim(fullds))
#  }
#  biome_sum <- fullds %>%
#    group_by(status) %>%  
#    dplyr::summarise(count=length(rh_098),meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE),
#                     meanrh75=mean(rh_075,na.rm=TRUE), sdrh75=sd(rh_075,na.rm=TRUE),
#                     meanrh50=mean(rh_050,na.rm=TRUE), sdrh50=sd(rh_050,na.rm=TRUE),
#                     meanrh10=mean(rh_010,na.rm=TRUE ), sdrh10=sd(rh_010, na.rm=TRUE),
#                     meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),
#                     meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),
#                     meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE))%>% 
#    tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status"))) 
#  biome_sum$biome <- this_biome
#  biome_sum$iso3 <- iso3
#  write.csv(biome_sum,paste(f.path,"WDPA_GEDI_extract2/biome_stats/",iso3,"_", this_biome,"_sumamry_stats_wk",gediwk,".csv", sep=""), row.names = F)
#  
# }  
# stopImplicitCluster()
# cat("Done summarizing by biomeions for country",iso3,"\n")    

#---------------Other old stuff don't use ----------------------
# for (i in matchedpaf) {
# 
#   id_pa <- i %>% str_split("_") %>% unlist %>% .[3]
#   matched <- readRDS(paste(f.path,"WDPA_matching_results/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_matching_results_wk",gediwk,".RDS", sep=""))
#   # cat("Extracting for no. ",match(id_pa,matchedpaf) ,"oa out of", length(matchedpaf))
#   if (is.null(matched)==TRUE) {
#     # stop(message("Matched result is null for PA", id_pa, " quitting...\n"))
#     # quit(save="no")} 
#   mras  <- tryCatch(matched2ras(matched),
#                     error=function(cond){
#                       cat("Matched result is likely null for country", iso3,"pa", id_pa, "dimension of the match is", dim(matched),"\n")
#                       # writeLines("Matched results is likely null for country", paste(f.path,"WDPA_log/",iso3,"_log_matching.txt", sep=""))
#                       return(NULL)}) #convert the macthed df to a raster stack 
#   if(table(mras$status[])[2]==0 | table(mras$status[])[1]==0){
#     cat("Rasterized results unbalanced for PA", id_pa, "quitting...\n")
#   }
#   
#   gedil2_f <- list.files(file.path(f.path,"WDPA_gedi_l2a+l2b_clean",iso3), full.names = TRUE)
#   registerDoParallel(cores=round(mproc*0.5))
#   iso_matched_gedi <- foreach(this_csv=gedil2_f, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
#     ##add the GEDI l4a model prediction for AGB here :
#     gedi_l2  <- read.csv(this_csv) %>%
#       dplyr::select(shot_number,lon_lowestmode, lat_lowestmode, starts_with("rh_"),cover, pai)%>%
#       SpatialPointsDataFrame(coords=.[,c("lon_lowestmode","lat_lowestmode")],
#                              proj4string=CRS("+init=epsg:4326"), data=.) %>%spTransform(., CRS("+init=epsg:6933"))
#     
#     iso_matched_gedi_df <- data.frame()
#     matched_gedi <- raster::extract(mras,gedi_l2, df=TRUE)
#     matched_gedi_metrics <- cbind(matched_gedi,gedi_l2@data)
#     
#     matched_gedi_metrics_filtered <- matched_gedi_metrics %>% dplyr::filter(!is.na(status)) %>% 
#       convertFactor(matched0 = matched,exgedi = .) 
#     
#     matched_gedi_l4a <-matched_gedi_metrics_filtered %>% 
#       dplyr::mutate(
#         LAT=lat_lowestmode,
#         LON=lon_lowestmode,
#         REGION=region,
#         PFT=pft,
#         RH_10=rh_010+100,
#         RH_20=rh_020+100,
#         RH_30=rh_030+100,
#         RH_40=rh_040+100,
#         RH_50=rh_050+100,
#         RH_60=rh_060+100,
#         RH_70=rh_070+100,
#         RH_80=rh_080+100,
#         RH_90=rh_090+100,
#         RH_98=rh_098+100) %>% 
#       modelr::add_predictions(model2, "AGBD")
#     
#     iso_matched_gedi_df <- rbind(matched_gedi_l4a,iso_matched_gedi_df)
#     
#     return(iso_matched_gedi_df)
#   }
#   stopImplicitCluster()
#   cat("Done GEDI extraction for pa", id_pa,"in ",iso3,"\n")
#   
#   iso_pa_stats_summary <- iso_matched_gedi %>%
#     group_by(status) %>% 
#     dplyr::mutate(pa_id=as.character(pa_id)) %>%
#     dplyr::summarise(count=length(rh_098),meanrh98=mean(rh_098, na.rm=TRUE), sdrh98=sd(rh_098, na.rm = TRUE),
#                      meanrh75=mean(rh_075,na.rm=TRUE), sdrh75=sd(rh_075,na.rm=TRUE),
#                      meanrh50=mean(rh_050,na.rm=TRUE), sdrh50=sd(rh_050,na.rm=TRUE),
#                      meanrh25=mean(rh_025,na.rm=TRUE ), sdrh25=sd(rh_025, na.rm=TRUE),
#                      meanpai=mean(pai, na.rm=TRUE), sdpai=sd(pai, na.rm=TRUE),
#                      meancov=mean(cover, na.rm=TRUE), sdcov=sd(cover,na.rm=TRUE),
#                      meanagbd=mean(AGBD, na.rm=TRUE), sdagbd=sd(AGBD, na.rm=TRUE),
#                      wwfecoreg=getmode(wwfecoreg),REGION=getmode(REGION),
#                      PADDD=getmode(PADDD))%>% 
#     tidyr::pivot_wider(names_from=status, values_from= setdiff(names(.),c("pa_id", "status"))) #writeLine to a large txt file where world pas stats are
#   
#   iso_pa_stats_summary$iso3=iso3
#   iso_pa_stats_summary$pa_id=id_pa
#   # write.csv(iso_pa_stats_summary, file=paste(f.path,"WDPA_GEDI_extract2/","iso_pa_stats_summary_wk",gediwk,".csv", sep=""), row.names = F)
#   write.table(iso_pa_stats_summary, file=paste(f.path,"WDPA_GEDI_extract2/",iso3,"_pa_stats_summary_wk",gediwk,".csv", sep=""),
#               sep=",", append=TRUE , row.names=FALSE, col.names=FALSE)
#   
#   iso_matched_gedi_sub <- iso_matched_gedi %>%
#     dplyr::select("pa_id","status","REP_AREA", "PA_STATUSYR","DESIG_ENG.x","GOV_TYPE","OWN_TYPE","wwfbiom","wwfecoreg","PADDD","pft","region","shot_number","lon_lowestmode","lat_lowestmode",
#                   "rh_010","rh_020", "rh_030", "rh_040",  "rh_050",  "rh_060" , "rh_070","rh_075",  "rh_080",  "rh_090",  "rh_098", "AGBD","cover","pai","REGION","PFT")  #write to individual country folder
#   
#   dir.create(file.path(paste(f.path,"WDPA_GEDI_extract2/",iso3,"_wk",gediwk,"/",sep="")))
#   saveRDS(iso_matched_gedi_sub, file=paste(f.path,"WDPA_GEDI_extract2/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_iso_matched_gedi_sub_wk_",gediwk,".RDS", sep=""))
#   write.csv(iso_matched_gedi_sub, file=paste(f.path,"WDPA_GEDI_extract2/",iso3,"_wk",gediwk,"/",iso3,"_pa_", id_pa,"_iso_matched_gedi_sub_wk_",gediwk,".csv", sep=""))
#   
#   
#   iso_matched_gedi_spdf <- SpatialPointsDataFrame(coords=iso_matched_gedi[,c("lon_lowestmode","lat_lowestmode")],
#                                                   proj4string=CRS("+init=epsg:4326"), data=iso_matched_gedi) %>%spTransform(., CRS("+init=epsg:6933"))
#   #rasterize the GEDI results 
#   gcount_ras <- rasterize(coordinates(iso_matched_gedi_spdf),mras, fun="count",background=NA)
#   names(gcount_ras) <- "gshot_counts"
#   gattr_ras <- rasterize(iso_matched_gedi_spdf@coords, mras, fun=getmode, field=iso_matched_gedi_spdf$status, background=NA)
#   names(gattr_ras) <- "status"
#   gstack <- stack(gcount_ras,gattr_ras)
#   g1km_sp <- as(gstack, 'SpatialPointsDataFrame')
#   g1km <- cbind(g1km_sp@coords, g1km_sp@data, pa_id=id_pa, country=iso3)
#   
#   write.table(g1km, file=paste(f.path,"WDPA_GEDI_extract2/",iso3,"_gedi_shots_per_1km_wk",gediwk,".csv", sep=""),
#               sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
#   
#   rm(mras,iso_matched_gedi, iso_matched_gedi_df, iso_matched_gedi_sub, iso_matched_gedi_spdf, g1km)
# }