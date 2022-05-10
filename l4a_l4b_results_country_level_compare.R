#This scripts compares the l4b output to l4a output computed in previous iterations, using l4b outputs to calculate the three sets oftotal AGB and 
  #the associated uncertainities at country level in step 1-4
  #step 10 makes the summary table for the paper, and 
  #step 8 plots the total AGB for the top 20 countries 
packages <- c("dplyr","plyr","ggplot2","randomForest","raster","mapview","sp","maptools","gridExtra",
              "lattice","rgdal","MASS","ggpubr","stringr","readr","tidyr","sp","maptools","gridExtra",
              "lattice","MASS","ggpubr","viridis","forcats", "Hmisc","corrr","ranger","rsample", "Metrics",
              "leaps","rgeos","data.table")
package.check <- lapply(packages, FUN = function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
})
`%notin%` <- Negate(`%in%`)
colnames <- data.frame("totalAGB",	"AGB_stderr",	"AGB_type",	"ISO3")

countryContinent <-read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
  mutate(iso3Status="analyzed")
otherISO <- rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv")) %>% 
  mutate(iso3Status="unmatched")
countryContinent <- rbind(countryContinent, otherISO)
write.csv(countryContinent,"/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/progress_l4bAGB_countryxBiome_byCountry_jan13.csv")


write.table(colnames,"/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_feb1.csv",  sep = ",",
            quote = FALSE,
            col.names =FALSE, row.names = FALSE)

reformatBiom <- function(lowercaseBiomeName){
  if(lowercaseBiomeName=="Tropical and subtropical moist broadleaf forests"){
    outName <- "Tropical_Subtropical_Moist_Broadleaf_Forests"
  } else if(lowercaseBiomeName=="Montane grasslands and shrublands"){
    outName <- "Montane_Grasslands_Shrublands"
  } else if (lowercaseBiomeName=="Tropical and subtropical grasslands, savannas and shrublands"){
    outName <- "Tropical_Subtropical_Grasslands_Savannas_Shrublands"
  } else if (lowercaseBiomeName=="Flooded grasslands and savannas"){
    outName <- "Flooded_Grasslands_Savannas"
  } else if (lowercaseBiomeName=="Tropical and subtropical dry broadleaf forests"){
    outName <- "Tropical_Subtropical_Dry_Broadleaf_Forests"
  }  else if (lowercaseBiomeName=="Deserts and xeric shrublands"){
    outName <- "Deserts_Xeric_Shrublands"
  } else if (lowercaseBiomeName=="Temperate broadleaf and mixed forests"){
    outName <- "Temperate_Broadleaf_Mixed_Forests"
  } else if (lowercaseBiomeName=="Temperate Coniferous Forest"){
    outName <- "Temperate_Conifer_Forests"
  } else if (lowercaseBiomeName=="Temperate grasslands, savannas and shrublands"){
    outName <- "Temperate_Grasslands_Savannas_Shrublands"
  } else if (lowercaseBiomeName=="Tropical and subtropical coniferous forests"){
    outName <- "Tropical_Subtropical_Coniferous_Forests"
  } else if (lowercaseBiomeName=="Mediterranean Forests, woodlands and scrubs"){
    outName <- "Mediterranean_Forests_Woodlands_Scrub"
  } else if (lowercaseBiomeName=="Tundra"){
    outName <-"Tundra"
  } else if (lowercaseBiomeName=="Boreal forests / Taiga"){
    outName <-"Boreal_Forests_Taiga"
  }
  
  else {
    outName <- lowercaseBiomeName
  }
  return(outName)
}
getMode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

notmatch <- c()
###check the influence of full join and left join#####
countryContinent <- countryContinent %>% dplyr::filter(iso3 %notin% c("COK","BHR","FSM","SYR","SOM","SPM","PSE","MDA","LBY","PRK"))
for (country in countryContinent$iso3){
  print(country)
  # country <- "AND"
  #------------loading-------------------
  l4aResult <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/country_level_stats_all.csv")
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  
  l4bResult0 <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022MAR07_CountryBiome_V2.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid0))
  
  l4bResult <-  l4bResult0 %>% 
    filter(grepl("V2", rid, fixed = TRUE)) %>%   #only grab the verson 2 results
    mutate(rid=sub("_V2","",rid)) %>% 
    mutate(iso3 =sub("_.*", "", rid)) %>% 
    mutate(status=gsub("^.*_", "", rid)) %>% 
    mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    left_join(iso_region, by="iso3")
  
  l4b_conti_biom <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022JAN19_ContinentBiome.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid)) %>% 
    mutate(conti =sub("_.*", "", rid)) %>% 
    mutate(status=gsub("^.*_", "", rid)) %>% 
    mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    mutate(conti_biom=paste(conti,biom,sep="_"))
  
  isoL4b <- l4bResult %>% 
    # mutate(rid=sub("_V2","",rid0)) %>% 
    # mutate(iso3 =sub("_.*", "", rid)) %>% 
    # mutate(status=gsub("^.*_", "", rid)) %>%
    # mutate(status=sub(" .*_V2", "", rid)) %>% 
    dplyr::filter(iso3 %like% substr(country,1,3)) %>% 
    dplyr::filter(status %notin% c("test", "test2"))
  #   dplyr::filter(grepl(country, rid, fixed = TRUE)) %>% #for hanlding regional processing for USA
  #   mutate(iso3=country) %>% 
  #   mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%mutate(biom=sub("_[^_]+$", "", biom))
  # # mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid)))
  # dplyr::filter(!grepl("east", rid, fixed = TRUE)) %>% 
  # dplyr::filter(!grepl("west", rid, fixed = TRUE))
  isoL4b
  
  continentBiome <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated.csv")
  
  rm(isol4a_output)         
 
  # write.table(isol4a_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_dec29.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  
  #------calculate AREAs for a given country using l4b-------------------------------
  #three sets of areas to be caluclated 
  #a) analyzed PAs extra AGB
  #b) all PAs extra AGB
  #c) all PAs total AGB
  
  #STEP1: load in dissolved PA areas i)for all anlayzed PAs ii) for all PAs
  analyzedPAAreas <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/analyzed_pa_areas_by_country", 
                                pattern="v2.csv", full.names = TRUE)
  allPAAreas <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/all_pa_areas_by_country", 
                           pattern="v2.csv", full.names = TRUE)
  
  rm(isoAnalyzedPA)
  
  isoAnalyzedPA <- tryCatch(
    {read.csv(analyzedPAAreas[grepl(analyzedPAAreas, pattern=country)][1])%>% rowwise() %>% 
        mutate(biome=reformatBiom(biome))
    
    },
    error = function(e){ 
      return(NA)
    })
  
  
  isoAllPA <- read.csv(allPAAreas[grepl(allPAAreas, pattern=country)][1]) %>% 
    rowwise() %>% 
    mutate(biome=reformatBiom(biome))
  if (exists("isoAnalyzedPA") && !is.na(isoAnalyzedPA)){
    isoNonAnalyzedPAs <- isoAllPA %>%dplyr::rename(biome_all=biome, pa_area_all=pa_area) %>% 
      left_join(isoAnalyzedPA,by=c("biome_all"="biome")) %>% 
      mutate(pa_area=ifelse(is.na(pa_area),0,pa_area )) %>% 
      mutate(nonMatchPAs=ifelse(is.na(pa_area), pa_area_all, (pa_area_all- pa_area))) %>% 
      mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
      mutate(biome=biome_all)
  } else {
    isoNonAnalyzedPAs <- isoAllPA %>% 
      mutate(nonMatchPAs=pa_area) %>% 
      mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
      mutate(pa_area=NA)
  }
  isoNonAnalyzedPAs %>% data.frame()
  
  #---STEP2: calculate a) extra AGB in analyzed PAs by (PA_mean-Control_mean)*PA_areas for each biome in the country ----
  stat_area <- isoL4b %>% dplyr::select(iso3, status, biom, region_mean,region_var,region_area, region_stderr, region_cell_count, nshots) %>% 
    dplyr::group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area),sum_cell_count=sum(region_cell_count), sum_shot_count=sum(nshots))
  stat_area
  extraAGB_in <- isoL4b %>% 
    left_join(stat_area,by="status") %>% 
    mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
    dplyr::select(iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, region_cell_count,nshots,weighted_region_var) %>% 
    # dplyr::filter(status!="unmatchedPA") %>%
    left_join(isoNonAnalyzedPAs, by=c("biom"="biome")) %>% #data.frame()
    pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var,region_cell_count,nshots,  weighted_region_var)) %>% data.frame()
  if ("region_mean_PA" %notin% colnames(extraAGB_in)){
    print("TRUE")
    extraAGB_in$region_mean_PA <- NA
    extraAGB_in$region_mean_Ctrl <- NA
    extraAGB_in$region_area_PA <- NA
    extraAGB_in$region_area_Ctrl <- NA
    extraAGB_in$region_stderr_PA <- NA
    extraAGB_in$region_stderr_Ctrl <- NA
    extraAGB_in$region_var_PA <- NA
    extraAGB_in$region_var_Ctrl <- NA
    extraAGB_in$weighted_region_var_PA  <- NA
    extraAGB_in$weighted_region_var_Ctrl  <- NA
  } 
  
  if ("region_mean_unmatchedPA" %notin% colnames(extraAGB_in)){
    print("TRUE")
    extraAGB_in$region_mean_unmatchedPA <- NA
    extraAGB_in$region_area_unmatchedPA <- NA
    extraAGB_in$region_var_unmatchedPA <- NA
    extraAGB_in$weighted_region_var_unmatchedPA  <- NA
  } 
  extraAGB_in <- tryCatch(mutate(extraAGB_in,AGBD_diff=region_mean_PA-region_mean_Ctrl), error = function(e) e, print("Hello"))
  extraAGB_in
  rm(extraAGB_only)
  
  #------add-------------------
  
  areaBiom <- unique(isoNonAnalyzedPAs$biome)
  l4bBiom <- unique(extraAGB_in$biom)
  
  if(length(areaBiom)!=length(l4bBiom)){
    print("not equal")
    notmatch = c(country, notmatch)
  }
  
}


#---STEP 1-4:  for a given country ISO3, retrive it's L4B value and L4A value----

countryList <-countryContinent%>% dplyr::filter(iso3 %notin% c("COK","BHR","FSM","SYR","SOM","SPM","PSE","MDA","LBY","PRK","ATF")) %>% .$iso3
for (country in countryList){

# countryAgg <- function(country=iso3){
  print(country)
 
  #------------loading-------------------
  l4aResult <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/country_level_stats_all.csv")
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  continentBiome <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_apr22.csv")
  l4bResult0 <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022APR22_CountryBiome_V2_modified.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid0))
  
  l4bResult <-  l4bResult0 %>% 
    filter(grepl("V2", rid, fixed = TRUE)) %>%   #only grab the verson 2 results
    mutate(rid=sub("_V2","",rid)) %>% 
    mutate(iso3 =sub("_.*", "", rid)) %>% 
    mutate(status=gsub("^.*_", "", rid)) %>% 
    mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    left_join(iso_region, by="iso3")
  
  l4b_conti_biom <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022JAN19_ContinentBiome.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid)) %>% 
    mutate(conti =sub("_.*", "", rid)) %>% 
    mutate(status=gsub("^.*_", "", rid)) %>% 
    mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    mutate(conti_biom=paste(conti,biom,sep="_"))
  
  library(data.table)
  # isoL4a <- l4aResult %>% dplyr::filter(iso3==country) 
  if(country !="USA"){
    isoL4b <- l4bResult %>% 
      # mutate(rid=sub("_V2","",rid0)) %>% 
      # mutate(iso3 =sub("_.*", "", rid)) %>% 
      # mutate(status=gsub("^.*_", "", rid)) %>%
      # mutate(status=sub(" .*_V2", "", rid)) %>% 
      dplyr::filter(iso3 %like% substr(country,1,3)) %>% 
      dplyr::filter(status %notin% c("test", "test2")) %>% 
      dplyr::filter(grepl(country, rid, fixed = TRUE)) %>% #for hanlding regional processing for USA
      mutate(iso3=country) %>%
      mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%mutate(biom=sub("_[^_]+$", "", biom))
    # # mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid)))
    # dplyr::filter(!grepl("east", rid, fixed = TRUE)) %>% 
    # dplyr::filter(!grepl("west", rid, fixed = TRUE))
    # isoL4b
  } else{
    isoL4b <- l4bResult %>% 
      # mutate(rid=sub("_V2","",rid0)) %>% 
      # mutate(iso3 =sub("_.*", "", rid)) %>% 
      # mutate(status=gsub("^.*_", "", rid)) %>%
      # mutate(status=sub(" .*_V2", "", rid)) %>% 
      dplyr::filter(iso3 %like% substr(country,1,3)) %>% 
      dplyr::filter(status %notin% c("test", "test2")) %>% 
      dplyr::filter(grepl(country, rid, fixed = TRUE)) %>% #for hanlding regional processing for USA
      filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) %>% 
      mutate(iso3=country) %>%
      mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%
      mutate(biome=sub(".*?_", "", biom))
      
    #   mutate(biom=sub("_[^_]+$", "", biom)) %>% 
    #   mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) #%>% 
    #   d#plyr::filter(!grepl("east", rid, fixed = TRUE)) 
    # 
    # isoL4b <- isoL4b %>% filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) 
    # mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    # dplyr::filter(!grepl("east", rid, fixed = TRUE)) %>%
    # dplyr::filter(!grepl("west", rid, fixed = TRUE))
    # isoL4b
    
  }
  
  # rm(isol4a_output)         
  # isol4a_output <- try(data.frame(totalExtraAGBallPAs=isoL4a$extra_AGB_in_PA, totalExtraAGBallPAs_err=isoL4a$SE_in_AGB, type="L4aAGB", iso3=country))
  # if(class(isol4a_output)=="try-error"){
  #   isol4a_output <- data.frame(totalExtraAGBallPAs=NA, totalExtraAGBallPAs_err=NA, type="L4aAGB", iso3=country)
  #   
  # }
  # isol4a_output
  # write.table(isol4a_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_dec29.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  
  #------calculate AREAs for a given country using l4b-------------------------------
  #three sets of areas to be caluclated 
  #a) analyzed PAs extra AGB
  #b) all PAs extra AGB
  #c) all PAs total AGB
  
  #STEP1: load in dissolved PA areas i)for all anlayzed PAs ii) for all PAs
  analyzedPAAreas <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/analyzed_pa_areas_by_country", 
                                pattern="v2.csv", full.names = TRUE)
  allPAAreas <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/all_pa_areas_by_country", 
                           pattern="v2.csv", full.names = TRUE)
  
  rm(isoAnalyzedPA)
  isoAnalyzedPA <- tryCatch(
    {read.csv(analyzedPAAreas[grepl(analyzedPAAreas, pattern=country)][1])%>% rowwise() %>% 
        mutate(biome=reformatBiom(biome))
      
    },
    error = function(e){ 
      return(NA)
    })
  
  isoAllPA <- read.csv(allPAAreas[grepl(allPAAreas, pattern=country)][1]) %>% 
    rowwise() %>% 
    mutate(biome=reformatBiom(biome))
  if (exists("isoAnalyzedPA") && !is.na(isoAnalyzedPA)){
    isoNonAnalyzedPAs <- isoAllPA %>%dplyr::rename(biome_all=biome, pa_area_all=pa_area) %>% 
      left_join(isoAnalyzedPA,by=c("biome_all"="biome")) %>% 
      mutate(pa_area=ifelse(is.na(pa_area),0,pa_area )) %>% 
      mutate(nonMatchPAs=ifelse(is.na(pa_area), pa_area_all, (pa_area_all- pa_area))) %>% 
      mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
      mutate(biome=biome_all)
  } else {
    isoNonAnalyzedPAs <- isoAllPA %>% 
      mutate(nonMatchPAs=pa_area) %>% 
      mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
      mutate(pa_area=NA)
  }
  # isoNonAnalyzedPAs %>% data.frame()
  if(country =="USA"){
    anaFiles <- analyzedPAAreas[grepl(analyzedPAAreas, pattern="USA_west|USA_east_|USA_pcfc")]
    rm(isoAnalyzedPA)
    isoAnalyzedPA <- data.frame()
    for(f in anaFiles){
      f1 <- read.csv(f)%>% rowwise() %>% mutate(biome=reformatBiom(biome))
      isoAnalyzedPA <- isoAnalyzedPA %>% rbind(f1)
    }
    
    allFiles <- allPAAreas[grepl(allPAAreas, pattern="USA_west|USA_east_|USA_pcfc")]
    rm(isoAllPA)
    isoAllPA <- data.frame()
    for (ff in allFiles){
      f2 <- read.csv(ff) %>% rowwise() %>% mutate(biome=reformatBiom(biome))
      isoAllPA <- isoAllPA %>% rbind(f2)
    }
    isoAllPA$iso3_biome <- paste(isoAllPA$iso3, isoAllPA$biome)
    isoAnalyzedPA$iso3_biome <- paste(isoAnalyzedPA$iso3, isoAnalyzedPA$biome)
    isoNonAnalyzedPAs <- isoAllPA %>%dplyr::rename(biome_all=biome, pa_area_all=pa_area) %>% 
      left_join(isoAnalyzedPA,by="iso3_biome") %>% 
      mutate(pa_area=ifelse(is.na(pa_area),0,pa_area )) %>% 
      mutate(nonMatchPAs=ifelse(is.na(pa_area), pa_area_all, (pa_area_all- pa_area))) %>% 
      mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
      mutate(biome=biome_all)
    
    
  }
  
  #---STEP2: calculate a) extra AGB in analyzed PAs by (PA_mean-Control_mean)*PA_areas for each biome in the country ----
  stat_area <- isoL4b %>% dplyr::select(iso3, status, biom, region_mean,region_var,region_area, region_stderr, region_cell_count, nshots) %>% 
    dplyr::group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area),sum_cell_count=sum(region_cell_count), sum_shot_count=sum(nshots))
  stat_area
  if (country %in% c("GNQ","KEN","LBR","MEX","MMR","SSD","SUR")){
    print("full join")
    extraAGB_in <- isoL4b %>% 
      left_join(stat_area,by="status") %>% 
      mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
      dplyr::select(iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, region_cell_count,nshots,weighted_region_var) %>% 
      # dplyr::filter(status!="unmatchedPA") %>%
      full_join(isoNonAnalyzedPAs, by=c("biom"="biome")) %>% #data.frame()
      pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var,region_cell_count,nshots,  weighted_region_var)) %>% data.frame()
    
  } else{
    extraAGB_in <- isoL4b %>% 
      left_join(stat_area,by="status") %>% 
      mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
      dplyr::select(iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, region_cell_count,nshots,weighted_region_var) %>% 
      # dplyr::filter(status!="unmatchedPA") %>%
      left_join(isoNonAnalyzedPAs, by=c("biom"="biome")) %>% #data.frame()
      pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var,region_cell_count,nshots,  weighted_region_var)) %>% data.frame()
    
  }
  if ("region_mean_PA" %notin% colnames(extraAGB_in)){
    print("TRUE")
    extraAGB_in$region_mean_PA <- NA
    extraAGB_in$region_mean_Ctrl <- NA
    extraAGB_in$region_area_PA <- NA
    extraAGB_in$region_area_Ctrl <- NA
    extraAGB_in$region_stderr_PA <- NA
    extraAGB_in$region_stderr_Ctrl <- NA
    extraAGB_in$region_var_PA <- NA
    extraAGB_in$region_var_Ctrl <- NA
    extraAGB_in$weighted_region_var_PA  <- NA
    extraAGB_in$weighted_region_var_Ctrl  <- NA
  } 
  
  if ("region_mean_unmatchedPA" %notin% colnames(extraAGB_in)){
    print("TRUE")
    extraAGB_in$region_mean_unmatchedPA <- NA
    extraAGB_in$region_area_unmatchedPA <- NA
    extraAGB_in$region_var_unmatchedPA <- NA
    extraAGB_in$weighted_region_var_unmatchedPA  <- NA
  } 
  extraAGB_in <- extraAGB_in %>% mutate(AGBD_diff=region_mean_PA-region_mean_Ctrl)
  # extraAGB_in
  rm(extraAGB_only)
  
  extraAGB_only <- extraAGB_in%>% 
    # mutate(AGBD_diff_err=sqrt((region_stderr_PA**2)+(region_stderr_Ctrl**2))) %>% 
    # mutate(extraAGB= AGBD_diff*pa_area*100) %>%  # data.frame() %>%  #x100 because areas are in km2
    # mutate(extraAGB_err= AGBD_diff_err*pa_area*100) %>% data.frame() 
    mutate(weighted_mean_PA=sum(region_mean_PA*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
           weighted_mean_Ctrl=sum(region_mean_Ctrl*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE),
           weighted_mean_unmatchedPA=sum(region_mean_unmatchedPA*region_area_unmatchedPA, na.rm=TRUE)/sum(region_area_unmatchedPA, na.rm=TRUE))%>% 
    mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA, na.rm=TRUE), 
           country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
           country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
    mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
           country_PA_se=round(sqrt(country_PA_var),5),
           country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
    mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl) %>%
    mutate(total_pa_area=sum(pa_area*100))
  # extraAGB_only
  
  # dplyr::select( AGBD_diff, extraAGB) %>% 
  extraAGB_only_output <-  extraAGB_only %>%
    dplyr::summarise(totalExtraAGB_weighted=weighted_agbd_diff * total_pa_area/1000000000,
                     totalExtraAGB=sum(AGBD_diff *pa_area*100,na.rm=TRUE)/1000000000,
                     totalExtraAGB_uncer=sqrt(((country_PA_se*total_pa_area)**2)+((country_Ctrl_se*total_pa_area)**2))/1000000000) %>%
                     # totalExtraAGB_err=sqrt(sum(extraAGB_err**2,na.rm=TRUE))/1000000000) %>%
    mutate(type="matchedPAExtraAGB", iso3=country) %>% unique()
  # extraAGB_only_output
  
  #-----STEP3: calculate b) extra AGB in all PAs by (PA_mean-Control_mean)*PA_areas + unmatched_PA_areas * AGBD_diff for each biome in the country ----
  #!!compute contientn x biome AGBD diff for PAs don't have country x biome level mean estimates, e.g. FGS PAs in TZA, and 
  #the continent xbiome mean needs to be recalculated with John's code
  if(any(is.na(extraAGB_in$AGBD_diff))){
    print("needs extrpolation")
    if("region.x" %in% colnames(extraAGB_in)){
      extraAGB_in <- extraAGB_in %>% mutate(region=region.x)
    }
    rm(replacement)
  
    toreplace <- extraAGB_in %>% dplyr::filter(is.na(AGBD_diff))
    replacement <- continentBiome %>%  dplyr::filter(biom %in% toreplace$biom) %>% dplyr::filter(continent %in% toreplace$region) %>% unique()#%>% mutate(contiBiom=paste(continent, biom, sep="_"))
    toreplace <- toreplace[order(toreplace$biom),]
    replacement <- replacement[order(replacement$biom),]
    if(nrow(toreplace)==nrow(replacement)){
      toreplace$weighted_region_var_Ctrl <- replacement$country_Ctrl_var
      toreplace$weighted_region_var_PA <- replacement$country_PA_var
      toreplace$region_mean_Ctrl <- replacement$weighted_mean_Ctrl
      toreplace$region_mean_PA <- replacement$weighted_mean_PA
      toreplace$AGBD_diff <- replacement$weighted_agbd_diff
      # toreplace$region_area_Ctrl <- replacement$region_area_Ctrl_sum
      # toreplace$region_area_PA <- replacement$region_area_PA_sum
      extraAGB_in[extraAGB_in$biom %in% toreplace$biom,] <- toreplace
      
      extraAGB_only2 <- extraAGB_in %>% 
        # mutate(weighted_mean_PA=sum(region_mean_PA*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
        #        weighted_mean_Ctrl=sum(region_mean_Ctrl*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE),
        #        weighted_mean_unmatchedPA=sum(region_mean_unmatchedPA*region_area_unmatchedPA, na.rm=TRUE)/sum(region_area_unmatchedPA, na.rm=TRUE))%>% 
        mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA, na.rm=TRUE), 
               country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
               country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
        mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
               country_PA_se=round(sqrt(country_PA_var),5),
               country_Ctrl_se=round(sqrt(country_Ctrl_var),5))# %>% 
      # mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl) %>%
      # mutate(total_pa_area=sum(pa_area*100))
      
    } else {
      print("replacment lacking")
      print(toreplace$biom)
      print(replacement$biom)
      extraAGB_only2 <- NA
      write.table(country, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/noExtrapolation_apr22.txt", sep = ",",
                  append = TRUE, quote = FALSE,
                  col.names =FALSE, row.names = FALSE)
      next
    }
   
      
  } else{
    extraAGB_only2<- extraAGB_only
  }
  # extraAGB_only2
  rm(allExtra_sum)
  allExtra_sum <- extraAGB_only2 %>% 
    mutate(total_pa_area=sum(pa_area*100, na.rm=TRUE)) %>% 
    mutate(total_unmatchedpa_area=sum(nonMatchPAs*100,na.rm=TRUE)) %>% 
    mutate(weighted_agbd_diff=tryCatch(weighted_mean_PA-weighted_mean_Ctrl,error = function(e) NA )) %>% 
    
    mutate(extraPaAGB_weighted=weighted_agbd_diff * total_pa_area) %>% 
    mutate(extraPaAGB=sum(AGBD_diff *pa_area*100,na.rm=TRUE)) %>%
    mutate(extraNonMatchPaAGB_weighted=weighted_agbd_diff*total_unmatchedpa_area) %>% 
    mutate(extraNonMatchPaAGB=sum(AGBD_diff *nonMatchPAs*100,na.rm=TRUE)) %>%
    
    mutate(extraPaAGB=ifelse(is.na(extraPaAGB), 0,extraPaAGB)) %>% 
    mutate(extraNonMatchPaAGB=ifelse(is.na(extraNonMatchPaAGB), 0,extraNonMatchPaAGB)) %>% 
    mutate(extraPaAGB_weighted=ifelse(is.na(extraPaAGB_weighted), 0,extraPaAGB_weighted)) %>% 
    mutate(extraNonMatchPaAGB_weighted=ifelse(is.na(extraNonMatchPaAGB_weighted), 0,extraNonMatchPaAGB_weighted)) %>% 
    
    mutate(allExtraAGB=extraNonMatchPaAGB+ extraPaAGB) %>% 
    mutate(allExtraAGB_weighted=extraNonMatchPaAGB_weighted + extraPaAGB_weighted) %>% 
    mutate(weighted_AGBD_diff_err=sqrt((country_PA_se**2)+(country_Ctrl_se**2))) %>% 
    mutate(extraPaAGB_err= weighted_AGBD_diff_err*total_pa_area) %>% 
    mutate(extraNonMatchPaAGB_err=weighted_AGBD_diff_err*total_unmatchedpa_area) %>% 
    mutate(allExtraAGB_err=sqrt((extraPaAGB_err**2)+(extraNonMatchPaAGB_err**2)))
  # allExtra_sum
  rm(allExtra_output)
  allExtra_output <- allExtra_sum%>% 
    dplyr::summarise(allExtraAGB_weighted=allExtraAGB_weighted/1000000000,
                     allExtraAGB=allExtraAGB/1000000000,
                     totalExtraAGBallPAs_err=sqrt(allExtraAGB_err**2)/1000000000,
                     totalExtraAGBallPAs_err2=sqrt(((country_PA_se*total_pa_area)**2)+((country_Ctrl_se*total_pa_area)**2)+
                      ((country_PA_se*total_unmatchedpa_area)**2)+((country_Ctrl_se*total_unmatchedpa_area)**2))/1000000000) %>%
    # mutate(totalExtraAGB_err=sqrt(extraAGB_err**2)) %>%
    mutate(type="allPAExtraAGB", iso3=country) %>% unique()  
    
  # allExtra_output
  
  #----step 4: calculate c) all PAs AGB by analyzed_PA_areas x PA_mean_AGB + non_analyzed_PA_areas x nonMatched_PA_mean----
  #!!caveat- for biome that has an analyzed PA size but no PA_mean_AGB, the total analyzed PAs are estimated to be 0, 
  #but the un-analyzed PA areas are used, so the results likely underestimated the analyzed PAs AGB
  #(essentially a mismacth between grid cell level biome charcterization and PA level biom coding) !!
  
  allAGB <- extraAGB_only2 %>% 
    mutate(total_pa_area=sum(pa_area*100, na.rm=TRUE)) %>% 
    mutate(total_unmatchedpa_area=sum(nonMatchPAs*100,na.rm=TRUE)) %>% 
    # mutate(totalAnalyzedAGB_weighted=tryCatch(weighted_mean_PA*total_pa_area,error = function(e) NA), 
    #        totalNonAnalyzedAGB_weighted=tryCatch(total_unmatchedpa_area*weighted_mean_unmatchedPA, error = function(e) NA) ) %>% 
    # mutate(totalAnalyzedAGB_weighted=ifelse(is.na(totalAnalyzedAGB_weighted), 0, totalAnalyzedAGB_weighted)) %>% 
    # mutate(totalNonAnalyzedAGB_weighted=ifelse(is.na(totalNonAnalyzedAGB_weighted), 0, totalNonAnalyzedAGB_weighted)) %>% 
    
    mutate(totalAnalyzedAGB=region_mean_PA*pa_area, totalNonAnalyzedAGB=region_mean_unmatchedPA * nonMatchPAs) %>% 
    mutate(totalAnalyzedAGB=ifelse(is.na(totalAnalyzedAGB), 0, totalAnalyzedAGB)) %>% 
    mutate(totalNonAnalyzedAGB=ifelse(is.na(totalNonAnalyzedAGB), 0, totalNonAnalyzedAGB)) %>% 
    # mutate(allPAAGB_weighted=totalAnalyzedAGB_weighted+ totalNonAnalyzedAGB_weighted) %>% 
    mutate(allPAAGB=totalAnalyzedAGB+ totalNonAnalyzedAGB) %>% 
    data.frame()
  # allAGB
  allAGB_output <- allAGB %>%
    dplyr::summarise(sumPAAGB=sum(allPAAGB)*100/1000000000, 
                     allExtraAGB_uncer=sqrt((country_PA_se * total_pa_area)**2+(country_unmatched_se *total_unmatchedpa_area)**2 )/1000000000) %>% 
    mutate(type="allPAAGB", iso3=country) %>% unique()
  # allAGB_output
  
  ######--------------writing values to table-------------------
  rm(r1,r2,r3, output)
  r1 <- extraAGB_only_output[,-c(1)] %>% dplyr::rename(totalAGB=totalExtraAGB,AGB_stderr=totalExtraAGB_uncer) 
  r2 <- allExtra_output[,-c(1, 4)] %>% dplyr::rename(totalAGB=allExtraAGB ,AGB_stderr=totalExtraAGBallPAs_err ) 
  r3 <- allAGB_output %>% dplyr::rename(totalAGB=sumPAAGB,AGB_stderr=allExtraAGB_uncer) 
  # r4 <- countryExtraAGBPA_ouput[,-c(3)]%>% dplyr::rename(totalAGB=totalExtraAGB, AGB_stderr=totalExtraAGB_err ) 
  # r5 <- allcountryExtraAGBPA_output %>% dplyr::rename(totalAGB=totalExtraAGB, AGB_stderr=totalExtraAGBallPAs_err ) 
  # r6 <- countryAllPAAGB_output %>% dplyr::rename(totalAGB=sumPAAGB, AGB_stderr=sumPAAGB_err )
  output <- r1 %>% rbind(r2) %>% rbind(r3) #%>% rbind(r4) %>% rbind(r5) %>% rbind(r6)
  output
  write.table(output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22.csv", sep = ",",
              append = TRUE, quote = FALSE,
              col.names =FALSE, row.names = FALSE)
  
  total_analyzed_pa_area <- sum(isoNonAnalyzedPAs$pa_area, na.rm=TRUE)
  total_pa_area <- sum(isoNonAnalyzedPAs$pa_area,isoNonAnalyzedPAs$nonMatchPAs, na.rm=TRUE)
  weighted_agbd_diff <- extraAGB_only$weighted_agbd_diff %>% unique()
  weighted_perc_diff <- unique((extraAGB_only$weighted_mean_PA - extraAGB_only$weighted_mean_Ctrl)/extraAGB_only$weighted_mean_Ctrl  *100)
  output_anc <- data.frame(total_analyzed_pa_area, total_pa_area, weighted_agbd_diff, weighted_perc_diff, iso3=country)
  output_anc
  # write.table(output_anc, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_ancillary_v2.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  
  
  
  
  
  # return(lacking)
}

# stat_area
# countryl4b
# 
# extraAGB_only
# countryExtraAGBPA1

# write.table(output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_feb15.csv", sep = ",",
#             append = TRUE, quote = FALSE,
#             col.names =FALSE, row.names = FALSE)
# 
# write.table(percentdiff,"/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/percent_diff_propagated.csv", sep = ",",
#             append = TRUE, quote = FALSE,
#             col.names =FALSE, row.names = FALSE)

#----step 5: compare with the country level processing results-----
rm(countryl4b)
rm(countryArea)
countryl4b <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022MAR07_Country_V2.csv") %>% 
  mutate(rid0=rid) %>% 
  filter(grepl("V2", rid, fixed = TRUE)) %>%   #only grab the verson 2 results
  mutate(rid=sub("_V2","",rid)) %>% 
  mutate(rid=sub("unmatched_PA","unmatchedPA",rid)) %>% 
  mutate(iso3 =sub("_.*", "", rid)) %>% 
  mutate(status=gsub("^.*_", "", rid)) %>% 
  dplyr::filter(iso3 %like% substr(country,1,3)) %>% 
  dplyr::filter(status %notin% c("test", "test2"))

countryl4b
if (exists("isoAnalyzedPA")){
  countryArea <- isoNonAnalyzedPAs %>% group_by(iso3.x) %>% 
    summarise(iso3=getMode(iso3.x), region=getMode(region.x),analyzedISO=getMode(analyzedISO), 
              pa_area_all=sum(pa_area_all,na.rm=TRUE),pa_area=sum(pa_area,na.rm=TRUE), nonMatchPAs=sum(nonMatchPAs,na.rm=TRUE))
} else {
  countryArea <- isoNonAnalyzedPAs %>% group_by(iso3) %>% 
    summarise(iso3=getMode(iso3), region=getMode(region),analyzedISO=getMode(analyzedISO), 
              pa_area=sum(pa_area,na.rm=TRUE), nonMatchPAs=sum(nonMatchPAs,na.rm=TRUE))
}
countryl4b

#--------------a) country level extra agb in analyzed PAs-------------------------------
rm(countryExtraAGBPA)
rm(countryExtraAGBPA_ouput)
sum_region_area <- sum(countryl4b$region_area, na.rm=TRUE)
countryExtraAGBPA1 <- countryl4b %>% 
  mutate(w=region_area/region_area, weighted_region_var=w**2*region_var) %>% 
  dplyr::select(iso3, status, region_mean, region_stderr, region_var, region_area, weighted_region_var) %>% 
  full_join(countryArea, by="iso3") %>% 
  pivot_wider(names_from=status, values_from=c(region_mean, region_stderr, region_var, region_area, weighted_region_var)) %>% 
  data.frame()
if ("region_mean_unmatchedPA" %notin% colnames(countryExtraAGBPA1)){
  print("TRUE")
  countryExtraAGBPA1$region_mean_unmatchedPA <- NA
  countryExtraAGBPA1$region_area_unmatchedPA <- NA
  countryExtraAGBPA1$region_stderr_unmatchedPA <- NA
  countryExtraAGBPA1$region_var_unmatchedPA <- NA
  countryExtraAGBPA1$weighted_region_var_unmatchedPA  <- NA
}
countryExtraAGBPA1 
countryExtraAGBPA<- countryExtraAGBPA1 %>% 
  mutate(AGBD_diff=region_mean_PA-region_mean_Ctrl) %>%
  mutate(extraAGB= AGBD_diff*pa_area*100) %>%  
  mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA, na.rm=TRUE), 
         country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
         country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
  mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
         country_PA_se=round(sqrt(country_PA_var),5),
         country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
  data.frame() #%>%  #x100 because areas are in km2
  # mutate(extraAGB_err= AGBD_diff_err*pa_area*100) %>% data.frame()
countryExtraAGBPA
countryExtraAGBPA_ouput <- countryExtraAGBPA %>% 
  dplyr::summarise(totalExtraAGB=sum(extraAGB,na.rm=TRUE)/1000000000,
                   totalExtraAGB_err=sqrt((region_stderr_PA * pa_area*100)**2+(region_stderr_Ctrl * pa_area*100)**2)/1000000000,
                   totalExtraAGB_err2=sqrt(((country_PA_se*pa_area*100)**2)+((country_Ctrl_se*pa_area*100)**2))/1000000000) %>% 
  mutate(type="C-matchedPAExtraAGB", iso3=country)
countryExtraAGBPA_ouput


#--------------b) country level extra agb in all PAs--------------------------------
rm(allcountryExtraAGBPA)
rm(allcountryExtraAGBPA_output)
allcountryExtraAGBPA <- countryExtraAGBPA1 %>% 
  mutate(AGBD_diff=region_mean_PA-region_mean_Ctrl) %>%
  mutate(extraPaAGB= AGBD_diff*pa_area*100) %>%  
  mutate(extraNonMatchPaAGB= AGBD_diff*nonMatchPAs*100) %>% 
  mutate(allExtraAGB=extraNonMatchPaAGB+ extraPaAGB) %>% 
  mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA, na.rm=TRUE), 
         country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
         country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
  mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
         country_PA_se=round(sqrt(country_PA_var),5),
         country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
  mutate(AGBD_diff_err=sqrt((region_stderr_PA**2)+(region_stderr_Ctrl**2))) %>% 
  mutate(extraPaAGB_err= AGBD_diff_err*pa_area*100) %>% 
  mutate(extraNonMatchPaAGB_err=AGBD_diff_err*nonMatchPAs*100) %>% 
  mutate(allExtraAGB_err=sqrt((extraPaAGB_err**2)+(extraNonMatchPaAGB_err**2)))
  
allcountryExtraAGBPA   
  
allcountryExtraAGBPA_output <- allcountryExtraAGBPA %>% 
  dplyr::summarise(totalExtraAGB=sum(allExtraAGB,na.rm=TRUE)/1000000000,
                   totalExtraAGBallPAs_err=sqrt(sum(allExtraAGB_err**2,na.rm=TRUE))/1000000000) %>% #old way to aggregating errors 
  mutate(type="C-allPAExtraAGB", iso3=country)                 
allcountryExtraAGBPA_output  

#--------------c) country level AGB in all PAs------------------
rm(countryAllPAAGB_in)
rm(countryAllPAAGB)
rm(countryAllPAAGB_output)
# countryAllPAAGB_in <- countryl4b %>%  dplyr::select(iso3, status, region_mean, region_stderr) %>% 
#   left_join(countryArea, by="iso3")  %>% 
#   pivot_wider(names_from=status, values_from=c(region_mean, region_stderr)) %>% data.frame()
# if ("region_mean_unmatchedPA" %notin% colnames(countryAllPAAGB_in)){
#   print("TRUE")
#   countryAllPAAGB_in$region_mean_unmatchedPA <- NA
#   countryAllPAAGB_in$region_stderr_unmatchedPA <- NA
# }
# if ("region_mean_PA" %notin% colnames(countryAllPAAGB_in)){
#   print("TRUE")
#   countryAllPAAGB_in$region_mean_PA <- NA
#   countryAllPAAGB_in$region_mean_Ctrl <- NA
#   countryAllPAAGB_in$region_stderr_PA <- NA
#   countryAllPAAGB_in$region_stderr_Ctrl <- NA
# }

countryAllPAAGB <-countryExtraAGBPA1   %>% 
  mutate(totalAnalyzedAGB=pa_area*region_mean_PA*100, totalNonAnalyzedAGB=nonMatchPAs*region_mean_unmatchedPA*100 ) %>% 
  mutate(totalAnalyzedAGB=ifelse(is.na(totalAnalyzedAGB), 0, totalAnalyzedAGB)) %>% 
  mutate(totalNonAnalyzedAGB=ifelse(is.na(totalNonAnalyzedAGB), 0, totalNonAnalyzedAGB)) %>% 
  mutate(allPAAGB=totalAnalyzedAGB+ totalNonAnalyzedAGB) %>%
  mutate(totalAnalyzedAGB_err=pa_area*region_stderr_PA*100, totalNonAnalyzedAGB_err=nonMatchPAs*region_stderr_unmatchedPA*100 ) %>% 
  mutate(totalAnalyzedAGB_err=ifelse(is.na(totalAnalyzedAGB_err), 0, totalAnalyzedAGB_err)) %>% 
  mutate(totalNonAnalyzedAGB_err=ifelse(is.na(totalNonAnalyzedAGB_err), 0, totalNonAnalyzedAGB_err)) %>% 
  mutate(allPAAGB_err=sqrt((totalAnalyzedAGB_err**2)+(totalNonAnalyzedAGB_err**2))) %>% 
  mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA, na.rm=TRUE),   #sanity check these area-weighted sumemd var are the same as the country-level var
         country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
         country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
  mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
         country_PA_se=round(sqrt(country_PA_var),5),
         country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) 
  data.frame()
countryAllPAAGB
countryAllPAAGB_output <- countryAllPAAGB %>%
  dplyr::summarise(sumPAAGB=sum(allPAAGB,na.rm=TRUE)/1000000000, sumPAAGB_err=sqrt(sum(allPAAGB_err**2,na.rm=TRUE))/1000000000 ) %>% 
  mutate(type="C-allPAAGB", iso3=country)
countryAllPAAGB_output

#---------------step 6: comparing %differnce b/t country x biome level aggregated mean and stderr to country level mean and stderr-----------------------
##differnce between country level and country x biome level mean and variance
p1 <- (extraAGB_only$weighted_mean_PA - countryExtraAGBPA$region_mean_PA)/(0.5*(extraAGB_only$weighted_mean_PA + countryExtraAGBPA$region_mean_PA))*100
p2 <- (extraAGB_only$weighted_mean_Ctrl - countryExtraAGBPA$region_mean_Ctrl)/ (0.5* (extraAGB_only$weighted_mean_Ctrl +countryExtraAGBPA$region_mean_Ctrl))*100
p3 <- (extraAGB_only$weighted_mean_unmatchedPA - countryExtraAGBPA$region_mean_unmatchedPA)/countryExtraAGBPA$region_mean_unmatchedPA *100

p4 <- (extraAGB_only$country_PA_se - countryExtraAGBPA$country_PA_se)/(0.5* (extraAGB_only$country_PA_se + countryExtraAGBPA$country_PA_se))*100
p5 <- (extraAGB_only$country_Ctrl_se - countryExtraAGBPA$country_Ctrl_se)/(0.5*(extraAGB_only$country_Ctrl_se + countryExtraAGBPA$country_Ctrl_se))*100
p6 <- (extraAGB_only$country_unmatched_se - countryExtraAGBPA$country_unmatched_se)/ (0.5*(extraAGB_only$country_unmatched_se +countryExtraAGBPA$country_unmatched_se)) *100

q1 <- (extraAGB_only_output$totalExtraAGB_weighted- countryExtraAGBPA_ouput$totalExtraAGB)/ (0.5*(countryExtraAGBPA_ouput$totalExtraAGB+extraAGB_only_output$totalExtraAGB_weighted))*100
q2 <- (extraAGB_only_output$totalExtraAGB_uncer - countryExtraAGBPA_ouput$totalExtraAGB_err2) / (0.5 * (countryExtraAGBPA_ouput$totalExtraAGB_err2 + extraAGB_only_output$totalExtraAGB_uncer))*100

q3 <-(allExtra_output$allExtraAGB_weighted - allcountryExtraAGBPA_output$totalExtraAGB)/(0.5*(allExtra_output$allExtraAGB_weighted + allcountryExtraAGBPA_output$totalExtraAGB))*100 #unmatched + matched extra 
q4 <-(round(allExtra_output$totalExtraAGBallPAs_err,5) - round(allcountryExtraAGBPA_output$totalExtraAGBallPAs_err,5))/(0.5*(allExtra_output$totalExtraAGBallPAs_err + allcountryExtraAGBPA_output$totalExtraAGBallPAs_err))*100 #unmatched + matched extra 

q5 <- (allAGB_output$sumPAAGB- countryAllPAAGB_output$sumPAAGB)/(0.5*(allAGB_output$sumPAAGB + countryAllPAAGB_output$sumPAAGB))*100  #total PA agb 
q6 <- (allAGB_output$allExtraAGB_uncer  - countryAllPAAGB_output$sumPAAGB_err) /(0.5*  (allAGB_output$allExtraAGB_uncer +countryAllPAAGB_output$sumPAAGB_err) )*100

percentdiff <- data.frame(iso3=country, agg_pa_mean_diff=p1, agg_ctrl_mean_diff= p2, agg_unmatched_mean_diff= p3,
                          agg_pa_se_diff=p4, agg_ctrl_se_diff= p5, agg_unmatched_se_diff= p6,
                          extra_matched_agb=q1, extra_matched_uncer= q2, 
                          all_extra_agb= q3, all_extra_uncer=q4, 
                          all_pa_agb= q5, all_pa_uncer= q6)

percentdiff



######--------------writing values to table-------------------
r1 <- extraAGB_only_output[,-c(1)] %>% dplyr::rename(totalAGB=totalExtraAGB,AGB_stderr=totalExtraAGB_uncer) 
r2 <- allExtra_output[,-c(1, 4)] %>% dplyr::rename(totalAGB=allExtraAGB ,AGB_stderr=totalExtraAGBallPAs_err ) 
r3 <- allAGB_output %>% dplyr::rename(totalAGB=sumPAAGB,AGB_stderr=allExtraAGB_uncer) 
r4 <- countryExtraAGBPA_ouput[,-c(3)]%>% dplyr::rename(totalAGB=totalExtraAGB, AGB_stderr=totalExtraAGB_err )
r5 <- allcountryExtraAGBPA_output %>% dplyr::rename(totalAGB=totalExtraAGB, AGB_stderr=totalExtraAGBallPAs_err )
r6 <- countryAllPAAGB_output %>% dplyr::rename(totalAGB=sumPAAGB, AGB_stderr=sumPAAGB_err )

output <- r1 %>% rbind(r2) %>% rbind(r3) %>% rbind(r4) %>% rbind(r5) %>% rbind(r6)

output
percentdiff

# stat_area
# countryl4b
# 
# extraAGB_only
# countryExtraAGBPA1

write.table(output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_mar08.csv", sep = ",",
            append = TRUE, quote = FALSE,
            col.names =FALSE, row.names = FALSE)
# 
write.table(percentdiff,"/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/percent_diff_propagated.csv", sep = ",",
            append = TRUE, quote = FALSE,
            col.names =FALSE, row.names = FALSE)


#---------------potting the mean and distribution of the 6 percent diff values---------------

percentdiff_df <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/percent_diff_propagated.csv")  #total of 97 countries included for tthe comparision 
modi_names <- names(percentdiff_df)[2:14]
percentdiff_df <- percentdiff_df[,-c(14)]
names(percentdiff_df) <- modi_names

niso3 <- unique(percentdiff_df$iso3) %>% length()
niso3

#check the average of each column
percentdiff_df <- unique(percentdiff_df)
summary <- round(colMeans(percentdiff_df[,-1], na.rm=TRUE), 3) %>% data.frame() %>% tibble::rownames_to_column(., "type")
names(summary) <- c("Type_of_percent_difference","lab")
summary$lab_per <- paste(summary$lab, "%",sep="")

#MEAN AND SISTRIBTUION OF THE AGGREGATED MEAN DIFF FOR THE PA class
percentdiff_tr <- percentdiff_df %>% pivot_longer(cols=2:13, names_to="Type_of_percent_difference",values_to="Percent_difference" ) %>% 
  left_join(summary, by="Type_of_percent_difference")

percentdiff_tr$Type_of_percent_difference <- factor(percentdiff_tr$Type_of_percent_difference,
                                                    levels = c("agg_pa_mean_diff","agg_ctrl_mean_diff", "agg_unmatched_mean_diff", "agg_pa_se_diff", "agg_ctrl_se_diff",       
                                                               "agg_unmatched_se_diff","extra_matched_agb", "all_extra_agb", "all_pa_agb","extra_matched_uncer","all_extra_uncer", "all_pa_uncer"),
                                                    labels=c("%diff aggregated and processed PA mean", "%diff aggregated and processed Ctrl mean", "%diff aggregated and processed unmatched mean",
                                                             "%diff aggregated and processed PA SE", "%diff aggregated and processed Ctrl SE", "%diff aggregated and processed unmacthed SE",
                                                             "%diff aggregated and processed analyzed PA extra AGB", "%diff aggregated and processed all PA extra AGB", "%diff aggregated and processed all PA AGB",
                                                             "%diff aggregated and processed analyzed PA extra AGB uncertainties", "%diff aggregated and processed all PA extra AGB uncertainties", "%diff aggregated and processed all PA AGB uncertainties"))
p<-ggplot(percentdiff_tr, aes(x=Percent_difference)) + 
  facet_wrap(vars(Type_of_percent_difference), scales = "free", nrow = 4)+
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept=lab),
             color="blue", linetype="dashed", size=1)+theme_light()
# geom_text(aes(x=-3, label=paste(round(mean(Percent_difference, na.rm=TRUE),3), "%", sep=""), y=200), colour="blue", angle=0, text=element_text(size=11)) 
p2 <- p+ geom_text(
  mapping = aes(x = -Inf, y = -Inf, label = lab_per), hjust   = -1.5,
  vjust   = -10, colour="blue")+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black", size=12))+
  xlab("")+ylab("")
p2
ggsave("/gpfs/data1/duncansongp/GEDI_global_PA/figures/percent_diff_aggregated_vs_processed_corrected.png",
       p2,width = 17, height=8, units = "in", device = "png")



#----step 7: compute total extra AGB (extrapolated for all countries biomes)------------------------------
allResults <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22.csv") %>% 
  left_join(countryContinent[,2:4],by=c("ISO3"="iso3"))

country2Filter <- c("USA","SYR","SOM","SPM","PSE","PRK","MDA","LBY","FSM","COK","BHR","AIA","ABW","ATF")

allResults2 <- allResults %>% filter(ISO3 %notin% country2Filter) %>% 
  mutate(type_iso=paste(AGB_type, ISO3,sep="_")) %>% 
  distinct(type_iso, .keep_all=TRUE)

###a) analyzed extra total
analyzedExtra <- allResults2 %>%  filter(AGB_type=="matchedPAExtraAGB") %>% filter(iso3Status!="unmatched")
analyzedExtra_entry <- analyzedExtra %>% 
  dplyr::summarise(sumPAAGB=sum(totalAGB, na.rm=TRUE), sumPAAGB_err=sqrt(sum(AGB_stderr**2,na.rm=TRUE))) %>% 
  mutate(type="AnalyzedPAExtra")
analyzedExtra_entry

###b) extrapolated extra total 
extrapolatedExtra <- allResults2 %>%  filter(AGB_type=="allPAExtraAGB") 
extrapolatedExtra_entry <- extrapolatedExtra %>% 
  dplyr::summarise(sumPAAGB=sum(totalAGB, na.rm=TRUE), sumPAAGB_err=sqrt(sum(AGB_stderr**2,na.rm=TRUE))) %>% 
  mutate(type="AllPAExtra")
extrapolatedExtra_entry  

###c) total PA AGB
allPA <- allResults2 %>%  filter(AGB_type=="allPAAGB") 
allPA_entry <- allPA %>% 
  dplyr::summarise(sumPAAGB=sum(totalAGB, na.rm=TRUE), sumPAAGB_err=sqrt(sum(AGB_stderr**2,na.rm=TRUE))) %>% 
  mutate(type="AllPA")
allPA_entry  

worldAgg <- analyzedExtra_entry %>% rbind(extrapolatedExtra_entry) %>% rbind(allPA_entry) %>% mutate_if(is.numeric, round, digits=2)
names(worldAgg) <- c("PA AGB (MT)","PA AGB STDERR (MT)", "AGB TYPE")
worldAgg <- worldAgg[,c(3,1,2)]
stargazer::stargazer(worldAgg, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/world_agg_stats_apr22.html')


#----step 8 : conitnent level aggregated from the country xbiome level results-----
analyzedExtra <- allResults2 %>%  filter(AGB_type=="matchedPAExtraAGB") %>% filter(iso3Status!="unmatched")
analyzedExtra_entry <- analyzedExtra %>% 
  dplyr::group_by(continent) %>% 
  dplyr::summarise(sumPAAGB=sum(totalAGB, na.rm=TRUE), sumPAAGB_err=sqrt(sum(AGB_stderr**2,na.rm=TRUE))) %>% 
  mutate(type="AnalyzedPAExtra") %>% 
  arrange(desc(sumPAAGB)) %>% filter(continent !="At") %>% mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)
analyzedExtra_entry

names(analyzedExtra_entry) <- c("Continent","PA AGB (GT)","PA AGB STDERR (GT)", "AGB TYPE")
analyzedExtra_entry <-analyzedExtra_entry[,c(4,1,2,3)]
# stargazer(analyzedExtra_entry, type = 'html',summary = FALSE, digits = 2,
#           out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/analyzedExtra_entry_byconti_mar08.html')


###b) extrapolated extra total 
extrapolatedExtra <- allResults2 %>%  filter(AGB_type=="allPAExtraAGB") 
extrapolatedExtra_entry <- extrapolatedExtra %>% 
  dplyr::group_by(continent) %>% 
  dplyr::summarise(sumPAAGB=sum(totalAGB, na.rm=TRUE), sumPAAGB_err=sqrt(sum(AGB_stderr**2,na.rm=TRUE))) %>% 
  mutate(type="AllPAExtra")%>% arrange(desc(sumPAAGB)) %>% filter(continent !="At") %>% mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)
extrapolatedExtra_entry  

names(extrapolatedExtra_entry) <- c("Continent","PA AGB (GT)","PA AGB STDERR (GT)", "AGB TYPE")
extrapolatedExtra_entry <-extrapolatedExtra_entry[,c(4,1,2,3)]
extrapolatedExtra_entry
# stargazer(extrapolatedExtra_entry, type = 'html',summary = FALSE, digits = 2,
#           out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/allextra_entry_byconti_mar08.html')


###c) total PA AGB
allPA <- allResults2 %>%  filter(AGB_type=="allPAAGB") 
allPA_entry <- allPA %>% 
  dplyr::group_by(continent) %>% 
  dplyr::summarise(sumPAAGB=sum(totalAGB, na.rm=TRUE), sumPAAGB_err=sqrt(sum(AGB_stderr**2,na.rm=TRUE))) %>% 
  mutate(type="AllPA") %>% arrange(desc(sumPAAGB)) %>% filter(continent !="At") %>% mutate_if(is.numeric, round, digits=2)
allPA_entry  

names(allPA_entry) <- c("Continent","PA AGB (GT)","PA AGB STDERR (GT)", "AGB TYPE")
allPA_entry <- allPA_entry[,c(4,1,2,3)] %>% mutate(Continent=as.character(Continent))
allPA_entry
# stargazer(allPA_entry, type = 'html',summary = FALSE, digits = 2,out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/allPA_entry_bycoonti_mar08.html')


#----step 8: ranking by country  --------------------------

allResults3 <- allResults2[!(allResults2$ISO3 %like% "USA"),]
for(u in unique(allResults2$AGB_type)[1:3]){
  print(u)
  rr <- allResults2[allResults2$ISO3 %like% "USA",] %>% filter(AGB_type==u) %>% 
    summarise(totalAGB=sum(totalAGB, na.rm=TRUE), AGB_stderr=sqrt(sum(AGB_stderr**2,na.rm=TRUE)), AGB_type=u, ISO3="USA", continent="US", 
              iso3Status="analyzed", type_iso=paste(u,"USA",sep="_") ) 
  print(rr)
  allResults3 <- allResults3 %>% rbind(rr)
}

write.csv(allResults3,"/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_usa.csv")



#0) load in the country level results to check
countryExtraResults <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_country_byCountry_jan20.csv") %>% 
  filter(AGB_type=="allPAExtraAGB") %>% dplyr::rename(countryTotalAGB=totalAGB, country_AGB_stderr=AGB_stderr) %>% dplyr::select(-c(AGB_type))
countryTotalResults <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_country_byCountry_jan20.csv") %>% 
  filter(AGB_type=="allPAAGB") %>% dplyr::rename(countryTotalAGB=totalAGB, country_AGB_stderr=AGB_stderr) %>% dplyr::select(-c(AGB_type))


allResults3 <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_usa.csv")

#a) analyzed extra
AnalyzedExtra_top10 <- allResults3 %>%  filter(AGB_type=="matchedPAExtraAGB") %>% filter(iso3Status!="unmatched") %>% arrange(desc(totalAGB)) %>% head(20) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>%
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2) 

AnalyzedExtra_bottom10 <- allResults3 %>%  filter(AGB_type=="matchedPAExtraAGB") %>% filter(iso3Status!="unmatched") %>% arrange(desc(totalAGB)) %>% tail(10) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>%
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)

AnalyzedExtra_top10
AnalyzedExtra_bottom10

###b) extrapolated extra total 
AllExtra_top10 <- allResults3 %>%  filter(AGB_type=="allPAExtraAGB")  %>% arrange(desc(totalAGB)) %>% head(20) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>% left_join(countryExtraResults,by="ISO3") %>% 
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)

AllExtra_bottom10 <- allResults3 %>%  filter(AGB_type=="allPAExtraAGB") %>% arrange(desc(totalAGB)) %>% tail(10) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>% left_join(countryExtraResults,by="ISO3") %>% 
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)
AllExtra_top10
AllExtra_bottom10

###c) total PA AGB
AllPA_top10 <- allResults3 %>%  filter(AGB_type=="allPAAGB")  %>% arrange(desc(totalAGB)) %>% head(20) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>% left_join(countryTotalResults,by="ISO3") %>% 
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)

AllPA_bottom10 <- allResults3 %>%  filter(AGB_type=="allPAAGB") %>% arrange(desc(totalAGB)) %>% tail(10) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>% left_join(countryTotalResults,by="ISO3") %>% 
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=6) 

AllPA_top10
AllPA_bottom10

###d) create a joined data with same top 20 countries from the allextra table, the agb values from the total PA agb table, and an external one with country total
targetISO <- AllExtra_top10$ISO3
table2 <-  allResults3 %>%  filter(AGB_type=="allPAAGB")  %>% arrange(desc(totalAGB)) %>% dplyr::filter(ISO3 %in% targetISO) %>% 
  dplyr::select(AGB_type, ISO3, continent, totalAGB, AGB_stderr) %>% left_join(countryTotalResults,by="ISO3") %>% 
  mutate(continent=as.character(continent)) %>%  mutate_if(is.numeric, round, digits=2)
table3 <- table2
table3$AGB_type <- "countryAGB"
table3$totalAGB <- c(76.26, 39.82, 42.73, 18.82, NA, 8.73, 27.86, 12.06, 7.26, 3.85, 4.41, 2.98, 2.63, 3.49,2.99, 2.53, 5.71,2.01, 
                     1.69, 0.98)

allTables <- AllExtra_top10 %>% rbind(table2) %>% rbind(table3) %>% dplyr::select(-c(countryTotalAGB, country_AGB_stderr) ) %>% 
  dplyr::filter(ISO3!="CAN") %>% dplyr::select(-c(continent, AGB_stderr)) %>% 
  pivot_wider(names_from = c(AGB_type), values_from=c(totalAGB)) %>% 
  mutate(allPAAGB2= allPAAGB- allPAExtraAGB, countryAGB2=countryAGB-allPAAGB-allPAExtraAGB,
         allPAExtraAGB2=allPAExtraAGB) %>% 
  pivot_longer(cols=c(allPAAGB2, countryAGB2, allPAExtraAGB2),names_to = "AGB_type",values_to="totalAGB")

allTables$AGB_type <- factor(allTables$AGB_type, levels=c("allPAExtraAGB2","allPAAGB2", "countryAGB2"),
                             labels=c("Additional preserved AGB by PAs", "Total AGB in PAs", "Non-protected AGB in the country"))

# top19iso3 <- AllExtra_top10 %>% dplyr::filter(ISO3!="CAN")
# allTables$ISO3 <- factor(allTables$ISO3, levels = top19iso3$ISO3,
#                          labels=c("Brazil","Australia","Venezuela","United States","Thailand",
#                                   "Peru","Bolivia","Tanzania","Indonesia","Chile","Zambia","Cambodia",
#                                   "Spain","*Madagascar","*DRC","New Zealand","France", "Mozambique","Malaysia"))

top19iso3 <- table3%>% filter(ISO3!="CAN") %>% arrange(desc(totalAGB)) %>% dplyr::select(ISO3)
allTables$ISO3 <- factor(allTables$ISO3, levels = top19iso3$ISO3,
                         labels=c("Brazil","United States","Indonesia","DRC","Peru","Australia","Venezuela","Bolivia","Malaysia",
                                  "Thailand","Chile","France","Mozambique", "Tanzania","Zambia","New Zealand","Madagascar",  "Spain",
                                 "Cambodia"))

sbar1 <- ggplot(allTables, aes(fill=AGB_type, y=totalAGB, x=ISO3)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(name="AGB Type",values=c("#7CFC00","#2AAA8A", "#C4B454")) +
  theme_classic()+#ylim(0,40)+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) +theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("")+ylab("AGB (Gt)")+
  # labs(caption = "Countries completed covered by GEDI data are included; US does not include Alaska.")+
  theme(legend.position = c(0.7, 0.8),
        legend.background = element_rect(fill = "white", color = "black"),
        plot.caption = element_text( face = "italic"),
        panel.background = element_rect(fill = "#FAFAFA"))+labs(tag = "a)")
  
sbar1


allTables2 <- allTables %>% dplyr::filter(AGB_type!=("Additional preserved AGB by PAs")) %>% 
  mutate(Percentage=allPAAGB/countryAGB*100) %>%
  pivot_longer(cols=c(allPAAGB, countryAGB, allPAExtraAGB),names_to = "AGB_type2",values_to="totalAGB2") %>% 
  group_by(ISO3) %>% arrange(desc(Percentage)) %>% ungroup() %>% filter(AGB_type2!="allPAExtraAGB")
allTables2$ISO3 <- factor(allTables2$ISO3, levels=unique(allTables2$ISO3))
allTables2$AGB_type2 <- factor(allTables2$AGB_type2, levels=c("allPAAGB", "countryAGB"),
                             labels=c("Total AGB in PAs", "Total AGB in the country"))


sbar3 <- ggplot(allTables2, aes( y=Percentage, x=ISO3)) + 
  geom_bar(position="identity", stat="identity",  fill="#9FE2BF") +
  # scale_fill_manual(name="AGB Type",values=c("#2AAA8A", "#C4B454")) +
  theme_classic()+#ylim(0,40)+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) +theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("")+ylab("National AGB Within Protected Areas (%)")+
  labs(caption = "Countries completed covered by GEDI data are included; US does not include Alaska")+
  theme(legend.position = c(0.81, 0.12),
        legend.background = element_rect(fill = "white", color = "black"),
        plot.caption = element_text( face = "italic"),
        panel.background = element_rect(fill = "#FAFAFA"))+labs(tag = "b)")

sbar3

ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig_top20_extratop_percent_vapr22.png", 
                plot=grid.arrange(sbar1, sbar3, ncol=1), width=6, height=10,
                units = "in", dpi="print",bg = "transparent")



ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig_top20_proportion.png", plot=sbar3, width=8, height=8,
                units = "in", dpi="print",bg = "transparent")


###e) export the results
stargazer(AnalyzedExtra_top10, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/AnalyzedExtra_top20_Mar08.html')
stargazer(AnalyzedExtra_bottom10, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/AnalyzedExtra_bottom10_Mar08.html')

stargazer(AllExtra_top10, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/AllExtra_top20_Mar21.html')
stargazer(AllExtra_bottom10, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/AllExtra_bottom10_Mar21.html')
stargazer(AllPA_top10, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/AllAGB_top20_Mar21.html')
stargazer(AllPA_bottom10, type = 'html',summary = FALSE, digits = 6,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/AllGAB_bottom10_Mar21.html')

###f) bar plots for comparison
AllExtra_top10$ISO3 <- as.factor(AllExtra_top10$ISO3)
AllExtra_top10$ISO3_names <- as.factor(c("Brazil","Australia","Venezuela","United States","Thailand",
                                         "Peru","Bolivia","Tanzania","Indonesia","Chile","Zambia","Canada","Cambodia",
                                         "Spain","DRC","Madagascar","New Zealand","France", "Mozambique","Malaysia"))

p <- ggplot(AllExtra_top10, aes(x=reorder(ISO3_names,-totalAGB),totalAGB), y=totalAGB) + 
  geom_bar(stat = "identity", width=0.6, fill="#7ECC49")+
  xlab("")+ylab("Preserved AGB by PAs (Gt)")+theme_classic()+#ylim(0,40)+
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) +theme(axis.text.x = element_text(angle = 45, hjust=1))
p

AllPA_top10$ISO3_names <- as.factor(c("Brazil","Indonesia","United States","Peru","Canada","Venezuela","DRC","Australia", "Colombia","Bolivia", 
                                      "Russia","Chile","Thailand","Tanzania","Republic of Congo","Germany","Mexico","Zambia","Gabon","France"))

p2 <- ggplot(AllPA_top10, aes(x=reorder(ISO3_names,-totalAGB),totalAGB), y=totalAGB) + 
  geom_bar(stat = "identity", width=0.6, fill="#299438")+
  xlab("")+ylab("Total AGB in PAs (Gt)")+theme_classic()+ 
  theme(text=element_text(family="Helvetica", face="bold", size=12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        rect = element_rect(fill = "transparent")) +theme(axis.text.x = element_text(angle = 45, hjust=1))
p2

pp=ggpubr::ggarrange(p, p2, ncol = 1, nrow = 2, labels = c("A","B"))

  ggplot2::ggsave(filename="/gpfs/data1/duncansongp/GEDI_global_PA/figures/JAN21_FIGS/fig_top20_2.png", plot=pp, width=8, height=10,
                  units = "in", dpi="print",bg = "transparent")

  
#----step 9: count the amount of negative agbd_diff among the analyzed countries for each biomes-----------------

extraAGB_files <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/matchedExtraAGB_df/",pattern = ".rds",full.names = TRUE)
extraAGB_sum <- data.frame()
for (f in extraAGB_files){
  print(f)
  t <-readRDS(f)
  if(ncol(t)>14){
    # t$region_mean_unmatchedPA <- NA
    # t$region_stderr_unmatchedPA <- NA
    tt <-try(dplyr::select(t,colnames(extraAGB_sum)))
    if(class(tt)=="try-error"){
      tt <- t %>% dplyr::rename(X=X.x,region=region.x ) %>% dplyr::select(colnames(extraAGB_sum))
    }
    t <- tt
    
  }
  
  # if ("analyzedISO"%notin% colnames(t)){
  #   t <- t %>% left_join(countryContinent[,c(2,4)],by=c("iso3.x"="iso3")) %>% dplyr::rename(analyzedISO=iso3Status)
  # }
  # if ("nonMatchPAs"%notin% colnames(t)){
  #   t$nonMatchPAs <- 0
  # }
  extraAGB_sum <- rbind(extraAGB_sum,t)
}

extraAGB_status <- extraAGB_sum %>% 
  left_join(countryContinent,by=c("iso3.x"="iso3")) %>% 
  dplyr::filter(!is.na(iso3.x)) %>% 
  dplyr::filter(iso3Status=="analyzed") %>% 
  dplyr::filter(!is.na(AGBD_diff)) %>% 
  mutate(iso_biom=paste(iso3.x, biom,sep="_"))

extraAGB_count <- extraAGB_status %>%
  mutate(positive_agbdDiff= ifelse(AGBD_diff>0, 1,0), negative_agbdDiff= ifelse(AGBD_diff<0, 1,0)) %>% 
  group_by(biom) %>% 
  dplyr::summarise(total_valid_agbdDiff=length(AGBD_diff), negative_agbdDiff=sum(negative_agbdDiff),positive_agbdDiff=sum(positive_agbdDiff) ) %>% 
  mutate(percent_negative=negative_agbdDiff/total_valid_agbdDiff*100) %>% 
  dplyr::rename(Biome=biom) %>% 
  mutate_if(is.numeric, round, digits=2)%>% 
  as.data.frame() %>% arrange(desc(percent_negative))
extraAGB_count
stargazer(extraAGB_count, type = 'html',summary = FALSE, digits = 2,
          out = '/gpfs/data1/duncansongp/GEDI_global_PA/figures/negative_agbd_diff_count.html')

#----step 10: create a merged table with contientnet, global, and biome level results all in one --------------
library(Gmisc, verbose=FALSE)
totalAGB_anc <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_mar08_ancillary_v2.csv")

#global
totalAGB_anc %>% filter(iso3%notin% c("USA_west","USA_east","USA_pcfc")) %>% .$total_pa_area %>% sum->total_pa_area

names(worldAgg) <- c("type","total","error")
worldAgg_rfmt <- worldAgg[2:3,]%>% dplyr::mutate(conti="Globe") %>%  mutate(paagb=paste(total, error, sep="+/-")) %>% 
  dplyr::select(conti,type, paagb) %>% 
  pivot_wider(names_from=type, values_from=paagb) %>% mutate(pa_area=total_pa_area/1000000)

#continent 
totalAGB_anc %>% filter(iso3%notin% c("USA_west","USA_east","USA_pcfc")) %>% left_join(countryContinent, by="iso3") %>%
  group_by(continent) %>%  dplyr::summarise(pa_area=sum(total_pa_area)/1000000)->conti_total_pa_area

contiagg <- extrapolatedExtra_entry %>% rbind(allPA_entry) 
names(contiagg) <-  c("type","conti","total","error")
contiagg <- contiagg%>% left_join(conti_total_pa_area, by=c("conti"="continent"))
contiagg$conti <-factor(as.factor(contiagg$conti), levels=c("SA","As","Af","US","Au","Eu"), 
                                         labels=c("South America (SA)","Asia (AS)", "Africa (AF)",
                                                  "North America (NA)", "Oceania (OC)", "Europe (EU)"))
contiAgg_rfmt <- contiagg %>% mutate(paagb=paste(total, error, sep="+/-")) %>% dplyr::select(conti, type, paagb, pa_area) %>% 
  pivot_wider(names_from=type, values_from=c(paagb, pa_area)) %>%  mutate(pa_area=pa_area_AllPA) %>% 
  dplyr::select(-c(pa_area_AllPAExtra,pa_area_AllPA))
names(contiAgg_rfmt) <- names(worldAgg_rfmt)

#biome

countryBiomAreas <- countryBiomAreasGenerate(countryContinent = countryContinent)
countryBiomAreas$iso3.x=as.character(countryBiomAreas$iso3.x)

bname <- data.frame(spacename=countryBiomAreas$X.x, dashname=countryBiomAreas$biome) %>% unique()

biomeAreas <- countryBiomAreas %>% #mutate(iso3=as.character(iso3.x)) %>% 
  group_by(biome) %>% 
  dplyr::summarise(pa_Area=sum(pa_area_all)) 

biomeAll <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_biome_allAGB_err_vapr22.csv")
biomeExtra <- read.csv( "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_biome_extraAGB_err_vapr22.csv")

biomeExtra$type <- "AllPAExtra"
biomeAll$type <- "AllPA"
names(biomeAll) <-names(biomeExtra)

biome_results <- rbind(biomeExtra, biomeAll) %>% left_join(biomeAreas,by="biome")
names(biome_results) <- c("X","biome","total","error","type","pa_area")

biomAgg_rfmt <- biome_results %>%mutate_if(is.numeric, round, digits=2) %>%  mutate(paagb=paste(total, error, sep="+/-")) %>% 
  dplyr::select(biome, type, paagb) %>% 
  pivot_wider(names_from=type, values_from=paagb) %>% left_join(biomeAreas,by="biome") %>% dplyr::rename(pa_area=pa_Area) %>% 
  mutate(pa_area=pa_area/1000000) %>% left_join(bname, by=c("biome"="dashname")) %>% mutate(biome=spacename, dashname=biome) %>% 
  dplyr::select(biome, AllPAExtra, AllPA,pa_area) %>% dplyr::rename(conti=biome)


#organize into a amtric for the html table
worldAgg_rfmt %>% rbind(contiAgg_rfmt) %>% rbind(biomAgg_rfmt) %>% 
  mutate(pa_area=round(pa_area, digits=2)) ->allrows

allrw_x <- allrows %>% as.matrix()
rownames(allrw_x) <- allrw_x[,1]
allrw_x <- allrw_x[,2:4]
colnames(allrw_x) <- c("Preserved AGB in PAs (Gt) ","All AGB in PAs (Gt) ", "All PA areas (Mkm\u00B2) ")

t <- htmlTable(allrw_x, 
          tspanner =c("Globe","Continent","Biome"),
          n.tspanner = c(1,6,nrow(allrw_x) - 7))

write_tableHTML(t, file = 'myfile.html')


#function for getting all the biome level pa areas
countryBiomAreasGenerate <- function(countryContinent){
  countryBiomAreas <- data.frame()
  countryList <- countryContinent%>% dplyr::filter(iso3 %notin% c("COK","BHR","FSM","SYR","SOM","SPM","PSE","MDA","USA","LBY","PRK","ATF")) %>% .$iso3
  for (country in countryList){
    
    # countryAgg <- function(country=iso3){
    print(country)
    
    #------------loading-------------------
    l4aResult <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/country_level_stats_all.csv")
    iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
      rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
    continentBiome <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_apr22.csv")
    l4bResult0 <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022APR22_CountryBiome_V2_modified.csv") %>% 
      mutate(rid0=rid) %>% 
      mutate(rid=sub("unmatched_PA","unmatchedPA",rid0))
    
    l4bResult <-  l4bResult0 %>% 
      filter(grepl("V2", rid, fixed = TRUE)) %>%   #only grab the verson 2 results
      mutate(rid=sub("_V2","",rid)) %>% 
      mutate(iso3 =sub("_.*", "", rid)) %>% 
      mutate(status=gsub("^.*_", "", rid)) %>% 
      mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
      left_join(iso_region, by="iso3")
    
    l4b_conti_biom <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022JAN19_ContinentBiome.csv") %>% 
      mutate(rid0=rid) %>% 
      mutate(rid=sub("unmatched_PA","unmatchedPA",rid)) %>% 
      mutate(conti =sub("_.*", "", rid)) %>% 
      mutate(status=gsub("^.*_", "", rid)) %>% 
      mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
      mutate(conti_biom=paste(conti,biom,sep="_"))
    
    library(data.table)
    # isoL4a <- l4aResult %>% dplyr::filter(iso3==country) 
    if(country !="USA"){
      isoL4b <- l4bResult %>% 
        # mutate(rid=sub("_V2","",rid0)) %>% 
        # mutate(iso3 =sub("_.*", "", rid)) %>% 
        # mutate(status=gsub("^.*_", "", rid)) %>%
        # mutate(status=sub(" .*_V2", "", rid)) %>% 
        dplyr::filter(iso3 %like% substr(country,1,3)) %>% 
        dplyr::filter(status %notin% c("test", "test2")) %>% 
        dplyr::filter(grepl(country, rid, fixed = TRUE)) %>% #for hanlding regional processing for USA
        mutate(iso3=country) %>%
        mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%mutate(biom=sub("_[^_]+$", "", biom))
      # # mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid)))
      # dplyr::filter(!grepl("east", rid, fixed = TRUE)) %>% 
      # dplyr::filter(!grepl("west", rid, fixed = TRUE))
      # isoL4b
    } else{
      isoL4b <- l4bResult %>% 
        # mutate(rid=sub("_V2","",rid0)) %>% 
        # mutate(iso3 =sub("_.*", "", rid)) %>% 
        # mutate(status=gsub("^.*_", "", rid)) %>%
        # mutate(status=sub(" .*_V2", "", rid)) %>% 
        dplyr::filter(iso3 %like% substr(country,1,3)) %>% 
        dplyr::filter(status %notin% c("test", "test2")) %>% 
        dplyr::filter(grepl(country, rid, fixed = TRUE)) %>% #for hanlding regional processing for USA
        filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) %>% 
        mutate(iso3=country) %>%
        mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%
        mutate(biome=sub(".*?_", "", biom))
      
      #   mutate(biom=sub("_[^_]+$", "", biom)) %>% 
      #   mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) #%>% 
      #   d#plyr::filter(!grepl("east", rid, fixed = TRUE)) 
      # 
      # isoL4b <- isoL4b %>% filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) 
      # mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
      # dplyr::filter(!grepl("east", rid, fixed = TRUE)) %>%
      # dplyr::filter(!grepl("west", rid, fixed = TRUE))
      # isoL4b
      
    }
    
    # rm(isol4a_output)         
    # isol4a_output <- try(data.frame(totalExtraAGBallPAs=isoL4a$extra_AGB_in_PA, totalExtraAGBallPAs_err=isoL4a$SE_in_AGB, type="L4aAGB", iso3=country))
    # if(class(isol4a_output)=="try-error"){
    #   isol4a_output <- data.frame(totalExtraAGBallPAs=NA, totalExtraAGBallPAs_err=NA, type="L4aAGB", iso3=country)
    #   
    # }
    # isol4a_output
    # write.table(isol4a_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_dec29.csv", sep = ",",
    #             append = TRUE, quote = FALSE,
    #             col.names =FALSE, row.names = FALSE)
    
    #------calculate AREAs for a given country using l4b-------------------------------
    #three sets of areas to be caluclated 
    #a) analyzed PAs extra AGB
    #b) all PAs extra AGB
    #c) all PAs total AGB
    
    #STEP1: load in dissolved PA areas i)for all anlayzed PAs ii) for all PAs
    analyzedPAAreas <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/analyzed_pa_areas_by_country", 
                                  pattern="v2.csv", full.names = TRUE)
    allPAAreas <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/all_pa_areas_by_country", 
                             pattern="v2.csv", full.names = TRUE)
    
    rm(isoAnalyzedPA)
    isoAnalyzedPA <- tryCatch(
      {read.csv(analyzedPAAreas[grepl(analyzedPAAreas, pattern=country)][1])%>% rowwise() %>% 
          mutate(biome=reformatBiom(biome))
        
      },
      error = function(e){ 
        t= data.frame(X=NA, iso3=country, region=unique(isoL4b$continent),pa_area=0, biome=NA )
        return(t)
      })
    
    isoAllPA <- read.csv(allPAAreas[grepl(allPAAreas, pattern=country)][1]) %>% 
      rowwise() %>% 
      mutate(biome=reformatBiom(biome))
    if (exists("isoAnalyzedPA") ){
      isoNonAnalyzedPAs <- isoAllPA %>%dplyr::rename(biome_all=biome, pa_area_all=pa_area) %>% 
        left_join(isoAnalyzedPA,by=c("biome_all"="biome")) %>% 
        mutate(pa_area=ifelse(is.na(pa_area),0,pa_area )) %>% 
        mutate(nonMatchPAs=ifelse(is.na(pa_area), pa_area_all, (pa_area_all- pa_area))) %>% 
        mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
        mutate(biome=biome_all)
    } else {
      isoNonAnalyzedPAs <- isoAllPA %>% 
        mutate(nonMatchPAs=pa_area) %>% 
        mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
        mutate(pa_area=NA)
    }
    # isoNonAnalyzedPAs %>% data.frame()
    if(country =="USA"){
      anaFiles <- analyzedPAAreas[grepl(analyzedPAAreas, pattern="USA_west|USA_east_|USA_pcfc")]
      rm(isoAnalyzedPA)
      isoAnalyzedPA <- data.frame()
      for(f in anaFiles){
        f1 <- read.csv(f)%>% rowwise() %>% mutate(biome=reformatBiom(biome))
        isoAnalyzedPA <- isoAnalyzedPA %>% rbind(f1)
      }
      
      allFiles <- allPAAreas[grepl(allPAAreas, pattern="USA_west|USA_east_|USA_pcfc")]
      rm(isoAllPA)
      isoAllPA <- data.frame()
      for (ff in allFiles){
        f2 <- read.csv(ff) %>% rowwise() %>% mutate(biome=reformatBiom(biome))
        isoAllPA <- isoAllPA %>% rbind(f2)
      }
      isoAllPA$iso3_biome <- paste(isoAllPA$iso3, isoAllPA$biome)
      isoAnalyzedPA$iso3_biome <- paste(isoAnalyzedPA$iso3, isoAnalyzedPA$biome)
      isoNonAnalyzedPAs <- isoAllPA %>%dplyr::rename(biome_all=biome, pa_area_all=pa_area) %>% 
        left_join(isoAnalyzedPA,by="iso3_biome") %>% 
        mutate(pa_area=ifelse(is.na(pa_area),0,pa_area )) %>% 
        mutate(nonMatchPAs=ifelse(is.na(pa_area), pa_area_all, (pa_area_all- pa_area))) %>% 
        mutate(nonMatchPAs=round(nonMatchPAs, 5)) %>% 
        mutate(biome=biome_all)
      
      
    }
    countryBiomAreas <- rbind(isoNonAnalyzedPAs, countryBiomAreas)
    
  }
  
  return(countryBiomAreas)
}



####checking the matching outcome for TZA to see if any FGS sites are matched###
matchingOutput <- data.frame("/gpfs/data1/duncansongp/GEDI_global_PA/")
matchedfiles <- list.files("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_matching_results/TZA_wk18", full.names = TRUE)
for (f in matchedfiles){
  print(f)
  ft <- readRDS(f)
  head(ft, 5)
  matchingOutput <- rbind(ft, matchingOutput)
}


###combining areas for USA
isoAnalyzedPA1 <- read.csv(analyzedPAAreas[grepl(analyzedPAAreas, pattern="USA_pcfc")]) %>% 
  rowwise() %>% 
  mutate(biome=reformatBiom(biome))
isoAnalyzedPA2 <- read.csv(analyzedPAAreas[grepl(analyzedPAAreas, pattern="USA_east")]) %>% 
  rowwise() %>% 
  mutate(biome=reformatBiom(biome))
isoAnalyzedPA3 <- read.csv(analyzedPAAreas[grepl(analyzedPAAreas, pattern="USA_west")]) %>% 
  rowwise() %>% 
  mutate(biome=reformatBiom(biome))
isoAnalyzedPA <- isoAnalyzedPA1 %>% full_join(isoAnalyzedPA2,by="biome") %>% full_join(isoAnalyzedPA3,by="biome") %>% 
  mutate(pa_area1=ifelse(is.na(pa_area.x), 0, pa_area.x)) %>% 
  mutate(pa_area2=ifelse(is.na(pa_area.y), 0, pa_area.y)) %>% 
  mutate(pa_area.z=pa_area) %>% 
  mutate(pa_area3=ifelse(is.na(pa_area.z), 0, pa_area.z)) %>% 
  mutate(pa_area=pa_area1+pa_area2+pa_area3) %>% dplyr::select(X, iso3, region, pa_area, biome)
# isoAnalyzedPA %>% dplyr::select(X, iso3, region, pa_area, biome) %>% write.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/analyzed_pa_areas_by_country/USA_analyzedPAbyBiomes_v2.csv")

isoAllPA1 <- read.csv(allPAAreas[grepl(allPAAreas, pattern="USA_pcfc")]) %>% 
  rowwise() %>% 
  mutate(biome=reformatBiom(biome))
isoAllPA2 <- read.csv(allPAAreas[grepl(allPAAreas, pattern="USA_east")]) %>% 
  rowwise() %>% 
  mutate(biome=reformatBiom(biome))
isoAllPA3 <- read.csv(allPAAreas[grepl(allPAAreas, pattern="USA_west")]) %>% 
  rowwise() %>% 
  mutate(biome=reformatBiom(biome))
isoAllPA<- isoAllPA1 %>% full_join(isoAllPA2,by="biome") %>% full_join(isoAllPA3,by="biome") %>% 
  mutate(pa_area1=ifelse(is.na(pa_area.x), 0, pa_area.x)) %>% 
  mutate(pa_area2=ifelse(is.na(pa_area.y), 0, pa_area.y)) %>% 
  mutate(pa_area.z=pa_area) %>% 
  mutate(pa_area3=ifelse(is.na(pa_area.z), 0, pa_area.z)) %>% 
  mutate(pa_area=pa_area1+pa_area2+pa_area3) %>% dplyr::select(X, iso3, region, pa_area, biome)

# write.csv(isoAllPA,"/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_shapefiles/all_pa_areas_by_country/USA_allPAbyBiomes_v2.csv")


#[not used]
#step3 length calc
# stat_area 
# allExtra_in <-  isoL4b %>% 
#   left_join(stat_area,by="status") %>% 
#   mutate(w=region_area/sum_region_area, weighted_region_var=w**2*region_var) %>% 
#   dplyr::select(iso3, status, biom, region_mean,weighted_region_var) %>% 
#   left_join(isoNonAnalyzedPAs, by=c("biom"="biome")) %>% #data.frame()
#   pivot_wider(names_from=status, values_from=c(region_mean,  weighted_region_var)) %>% data.frame()
# 
# if ("region_mean_unmatchedPA" %notin% colnames(allExtra_in)|| any(is.na(allExtra_in$region_mean_unmatchedPA))) {
#   allExtra_in$region_mean_unmatchedPA <- NA
#   allExtra_in$weighted_region_var_unmatchedPA <- NA
#   
# }
# if (("region_mean_PA" %notin% colnames(allExtra_in))|| any(is.na(allExtra_in$region_mean_PA)) ){
#   print("TRUE")
#   allExtra_in0 <- allExtra_in %>% 
#     mutate(region=tryCatch({as.character(region)},error=function(cond){as.character(region.x)})) %>% 
#     mutate(region=ifelse(region=="US","NAm", region)) %>% 
#     mutate(region=ifelse(region=="SA","SAm", region)) %>% 
#     mutate(conti_biom_iso=paste(region,biom,sep="_"))
#   print(allExtra_in0)
#   rm(continent_mean_PA)
#   continent_mean_PA <- l4b_conti_biom %>% 
#     filter(conti_biom %in% unique(allExtra_in0$conti_biom_iso)) %>% 
#     dplyr::select(conti_biom,conti, status, biom, region_mean, region_stderr) %>% 
#     pivot_wider(names_from=status, values_from=c(region_mean, region_stderr)) %>% data.frame()
#   print(continent_mean_PA)
#   if(!exists("continent_mean_PA") || nrow(continent_mean_PA)<1 || "region_mean_PA" %notin% colnames(continent_mean_PA) ){
#     allExtra_in$region_mean_PA <- NA
#     allExtra_in$region_mean_Ctrl <- NA
#     allExtra_in$region_stderr_PA <- NA
#     allExtra_in$region_stderr_Ctrl <- NA
#     cat("no continent x biome mean for extrapolation\n")
#   } else{
#     cat("Using continent x biome mean for extrapolation\n")
#     allExtra_in00 <- try(allExtra_in0[is.na(allExtra_in0$region_mean_PA),])
#     if (class(allExtra_in00)=="try-error" || nrow(allExtra_in00)<1){
#       allExtra_in00 <- allExtra_in0
#       allExtra_in00$region_mean_PA <- NA
#       allExtra_in00$region_mean_Ctrl <- NA
#       allExtra_in00$region_stderr_PA <- NA
#       allExtra_in00$region_stderr_Ctrl <- NA
#       
#       allExtra_in0$region_mean_PA <- NA
#       allExtra_in0$region_mean_Ctrl <- NA
#       allExtra_in0$region_stderr_PA <- NA
#       allExtra_in0$region_stderr_Ctrl <- NA
#       
#     }
#     allExtra_in00 <- allExtra_in00 %>% filter(conti_biom_iso%in% continent_mean_PA$conti_biom)
#     for (x in 1:nrow(allExtra_in00)){
#       print(x)
#       replace <- continent_mean_PA[continent_mean_PA$conti_biom==allExtra_in00[x,]$conti_biom_iso,]
#       toReplace <- allExtra_in00[x,]
#       toReplace$region_mean_PA <- replace$region_mean_PA
#       toReplace$region_mean_Ctrl <- replace$region_mean_Ctrl
#       toReplace$region_stderr_PA <- replace$region_stderr_PA
#       toReplace$region_stderr_Ctrl <- replace$region_stderr_Ctrl
#       allExtra_in0[allExtra_in0$conti_biom_iso==toReplace$conti_biom_iso,] <- toReplace
#       
#     }
#     allExtra_in <- allExtra_in0
#   }
# }
# rm(allExtra)
# allExtra <- allExtra_in %>% 
#   mutate(AGBD_diff=region_mean_PA-region_mean_Ctrl) %>%#names
#   # mutate(AGBD_diff_err=sqrt((region_stderr_PA**2)+(region_stderr_Ctrl**2))) 
#   mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA, na.rm=TRUE), 
#          country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
#          country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
#   mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
#          country_PA_se=round(sqrt(country_PA_var),5),
#          country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
#   mutate(total_pa_area=sum(pa_area*100, na.rm=TRUE)) %>% 
#   mutate(total_unmatchedpa_area=sum(nonMatchPAs*100,na.rm=TRUE))
