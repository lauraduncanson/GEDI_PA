#This scripts using the country x biome outputs to aggregate total AGB and uncertianities at the continent x biome level (part2), 
  #continent (part3), and biome level (part4)
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
      mutate(pa_area_all=pa_area) %>% 
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
      write.table(country, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/noExtrapolation_apr22_v2.txt", sep = ",",
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
    mutate(extraPaAGB_ind=AGBD_diff *pa_area*100) %>% 
    mutate(extraPaAGB=sum(AGBD_diff *pa_area*100,na.rm=TRUE)) %>%
    mutate(extraNonMatchPaAGB_weighted=weighted_agbd_diff*total_unmatchedpa_area) %>% 
    mutate(extraNonMatchPaAGB=sum(AGBD_diff *nonMatchPAs*100,na.rm=TRUE)) %>%
    mutate(extraNonMatchPaAGB_ind=AGBD_diff *nonMatchPAs*100) %>%
    
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
  
  
  allExtra_sum_sub <- allExtra_sum %>% 
    mutate(region.x = ifelse( "region.x" %in% colnames(.), as.character(region.x), as.character(region))) %>% 
    mutate(iso3.x = ifelse( "iso3.x" %in% colnames(.), as.character(iso3.x), as.character(iso3))) %>% 
    mutate(extraPaAGB_ind=ifelse(is.na(extraPaAGB_ind), 0, extraPaAGB_ind)) %>% 
    mutate(extraNonMatchPaAGB_ind= ifelse(is.na(extraNonMatchPaAGB_ind), 0, extraNonMatchPaAGB_ind)) %>% 
    dplyr::select(iso3.x, region.x,biom,  analyzedISO, pa_area_all, pa_area, nonMatchPAs, AGBD_diff,  
                  region_var_Ctrl, region_var_PA, region_var_unmatchedPA,
                  extraPaAGB_ind,  extraNonMatchPaAGB_ind) %>% 
    mutate(iso3=iso3.x, region=as.character(region.x), allExtraAGB=extraPaAGB_ind+extraNonMatchPaAGB_ind)
  allExtra_sum_sub$weighted_region_var_unmatchedPA <- extraAGB_only2$weighted_region_var_unmatchedPA
  allExtra_sum_sub$weighted_region_var_Ctrl <- extraAGB_only2$weighted_region_var_Ctrl
  allExtra_sum_sub$weighted_region_var_PA <- extraAGB_only2$weighted_region_var_PA
  
  # allAGB_sub <- allAGB %>% 
  #   mutate(region.x = ifelse( "region.x" %in% colnames(.), as.character(region.x), as.character(region))) %>% 
  #   mutate(iso3.x = ifelse( "iso3.x" %in% colnames(.), as.character(iso3.x),as.character(iso3))) %>% 
  #   dplyr::select(iso3.x,  region.x, biom,pa_area_all, pa_area, nonMatchPAs, AGBD_diff, totalAnalyzedAGB,region_mean_Ctrl, region_mean_PA, region_mean_unmatchedPA,
  #                 region_var_Ctrl, region_var_PA, region_var_unmatchedPA,totalNonAnalyzedAGB, allPAAGB) %>% 
  #   mutate(iso3=iso3.x, region=as.character(region.x))
  allAGB_sub <- allExtra_sum %>% 
    mutate(region.x = ifelse( "region.x" %in% colnames(.), as.character(region.x), as.character(region))) %>% 
    mutate(iso3.x = ifelse( "iso3.x" %in% colnames(.), as.character(iso3.x), as.character(iso3))) %>% 
    mutate(extraPaAGB_ind=ifelse(is.na(extraPaAGB_ind), 0, extraPaAGB_ind)) %>% 
    mutate(extraNonMatchPaAGB_ind= ifelse(is.na(extraNonMatchPaAGB_ind), 0, extraNonMatchPaAGB_ind)) %>% 
    dplyr::select(iso3.x, region.x,biom,  analyzedISO, pa_area_all, pa_area, nonMatchPAs, AGBD_diff,  
                  region_var_Ctrl, region_var_PA, region_var_unmatchedPA,
                  extraPaAGB_ind,  extraNonMatchPaAGB_ind, region_mean_Ctrl, region_mean_PA, region_mean_unmatchedPA) %>% 
    mutate(iso3=iso3.x, region=as.character(region.x), allExtraAGB=extraPaAGB_ind+extraNonMatchPaAGB_ind)
  allAGB_sub$weighted_region_var_unmatchedPA <- extraAGB_only2$weighted_region_var_unmatchedPA
  allAGB_sub$weighted_region_var_Ctrl <- extraAGB_only2$weighted_region_var_Ctrl
  allAGB_sub$weighted_region_var_PA <- extraAGB_only2$weighted_region_var_PA
  
  # write.table(allExtra_sum_sub, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_extra+area_v2.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  write.table(allAGB_sub, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_all+area_v2.csv", sep = ",",
              append = TRUE, quote = FALSE,
              col.names =FALSE, row.names = FALSE)
}

#sum up the total AGB for conti x biome and biom

#1. extra
extraagb <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_extra+area_v2.csv")
biomExtra <- extraagb %>% group_by(biom) %>% dplyr::summarise(conti_extra=sum(allExtraAGB, na.rm=TRUE)/1000000000) %>% data.frame()
contiBiomExtra <- extraagb %>% group_by(region, biom) %>% dplyr::summarise(conti_extra=sum(allExtraAGB, na.rm=TRUE)/1000000000) %>% data.frame()

#2. total
allagb <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_all+area_v2.csv")
biomAll <- allagb %>% group_by(biom) %>% dplyr::summarise(biom_all=sum(allExtraAGB, na.rm=TRUE)*100/1000000000) %>% data.frame()
contiBiomAll <- allagb %>% group_by(region, biom) %>% dplyr::summarise(biom_all=sum(allExtraAGB, na.rm=TRUE)*100/1000000000) %>% data.frame()


#----1. extra AGB in PAs at contient x biome level------
#handles the uncertianities 
#do the weighting at the continent or continent x biome level - total AGB is calculated 
# conti <- "Af"
# biome <- "Deserts_Xeric_Shrublands"

output <- data.frame()
currentVal <- contiBiomExtra
for (r in 1:nrow(currentVal)){
  if (currentVal[r,]$conti_extra !=0){
    conti <- currentVal[r,]$region
    biome <- currentVal[r,]$biom
    print(conti)
    print(biome)
    conti_extraAGB_err <- contiBiomAggErr(conti, biome)
    conti_extraAGB_err_output <- cbind(conti=conti, biome=biome, conti_extraAGB_err)
    output <- rbind(output, conti_extraAGB_err_output)
  } else{
    print(currentVal[r,])
    
  }
  # write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  
}

contiBiomAggErr <- function(conti=conti, biome=biome){
  
  #------------loading-------------------
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  
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

  
  area <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_extra+area_v2.csv") %>% 
    dplyr::filter(region%in% conti) %>% dplyr::filter(biom %in% biome) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>% 
    dplyr::select(contiBiom, pa_area_all, pa_area, nonMatchPAs,AGBD_diff, weighted_region_var_unmatchedPA,
                  weighted_region_var_PA, weighted_region_var_Ctrl)

  if (conti=="US"){
    l4bResult %>% 
      dplyr::filter(continent %in% conti) %>% 
      mutate(iso3 =sub("_.*", "", rid))->t
    usasub <- t %>% filter(iso3=="USA") %>% filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) %>% 
      mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%
      mutate(biom=stringr::str_remove(biom, paste("_",status, sep=""))) %>% 
      mutate(iso3=paste(iso3, str_extract(biom, "[^_]+"), sep="_")) %>% 
      mutate(biom=sub(".*?_", "", biom)) %>% 
      dplyr::filter(biom %in% biome) %>% dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr) 
    nonusasub <- t %>%filter(iso3!="USA") %>% dplyr::filter(biom %in% biome) %>% 
      dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr)
    contiL4b <- rbind(usasub, nonusasub)
  } else {
    contiL4b <- l4bResult %>% 
      dplyr::filter(continent %in% conti) %>% 
      dplyr::filter(biom %in% biome) %>% dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr) 
    
    
  }
  contiL4b
  
  stat_area <- contiL4b %>% group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area))
    
  contiL4b2 <- contiL4b %>% 
    left_join(stat_area,by="status") %>% 
    mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
    dplyr::select(continent,iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, weighted_region_var) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>% 
    pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var, weighted_region_var)) %>% 
    data.frame() %>%full_join(area, by="contiBiom") 
  
  #fill in NA for the countries with missing variance with continent x biome level extrpolation 
  if ("PA" %notin% unique(contiL4b$status)){
    contiL4b2$weighted_region_var_PA.x <- contiL4b2$weighted_region_var_PA
    contiL4b2$weighted_region_var_Ctrl.x <- contiL4b2$weighted_region_var_Ctrl
    contiL4b2$region_mean_PA <- NA
    contiL4b2$region_mean_Ctrl <- NA
    contiL4b2$region_area_PA <- NA
    contiL4b2$region_area_Ctrl <- NA
    
    
  } else {
    contiL4b2$weighted_region_var_Ctrl.x <- ifelse(is.na(contiL4b2$weighted_region_var_Ctrl.x),contiL4b2$weighted_region_var_Ctrl.y, contiL4b2$weighted_region_var_Ctrl.x)
    contiL4b2$weighted_region_var_PA.x <- ifelse(is.na(contiL4b2$weighted_region_var_PA.x),contiL4b2$weighted_region_var_PA.y, contiL4b2$weighted_region_var_PA.x)

  }
  
  if("region_mean_unmatchedPA" %notin% colnames(contiL4b2)){
    contiL4b2$region_mean_unmatchedPA <- NA
    contiL4b2$region_area_unmatchedPA <- NA
    contiL4b2$region_var_unmatchedPA <- NA
    contiL4b2$weighted_region_var_unmatchedPA.x <- NA
  }
  
  contiL4b3 <- contiL4b2 %>% mutate(weighted_mean_PA=sum(region_mean_PA*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
                       weighted_mean_Ctrl=sum(region_mean_Ctrl*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE),
                       weighted_mean_unmatchedPA=sum(region_mean_unmatchedPA*region_area_unmatchedPA, na.rm=TRUE)/sum(region_area_unmatchedPA, na.rm=TRUE))%>% 
    mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA.x, na.rm=TRUE), 
           country_PA_var=sum(weighted_region_var_PA.x, na.rm=TRUE),
           country_Ctrl_var=sum(weighted_region_var_Ctrl.x, na.rm=TRUE)) %>% 
    mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
           country_PA_se=round(sqrt(country_PA_var),5),
           country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
    mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl)
    
  
    contiBiomextra_output1 <-  contiL4b3 %>%
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
    
    allExtra_output <- contiBiomextra_output1%>% 
      dplyr::summarise(allExtraAGB_weighted=allExtraAGB_weighted/1000000000,
                       allExtraAGB=allExtraAGB/1000000000,
                       totalExtraAGBallPAs_err=sqrt(allExtraAGB_err**2)/1000000000,
                       totalExtraAGBallPAs_err2=sqrt(((country_PA_se*total_pa_area)**2)+((country_Ctrl_se*total_pa_area)**2)+
                                                       ((country_PA_se*total_unmatchedpa_area)**2)+((country_Ctrl_se*total_unmatchedpa_area)**2))/1000000000) %>%
      # mutate(totalExtraAGB_err=sqrt(extraAGB_err**2)) %>%
      mutate(type="allPAExtraAGB") %>% unique()  
    return(allExtra_output)
      
}

#new is table for vero with new grouping
new <- output %>% group_by(conti) %>% 
  mutate(newgroup= ifelse(biome %in% c("Montane_Grasslands_Shrublands","Flooded_Grasslands_Savannas",
                                       "Temperate_Grasslands_Savannas_Shrublands","Tropical_Subtropical_Grasslands_Savannas_Shrublands"), "GS", 
                                       ifelse(biome %in% c("Temperate_Conifer_Forests", "Tropical_Subtropical_Coniferous_Forests"), "CF",as.character(biome)  ))) %>% 
  group_by(conti, newgroup) %>% 
  dplyr::summarise(contiBiomPAAGB=sum(allExtraAGB, na.rm=TRUE), contiBiomPAAGB_err=sqrt(sum(totalExtraAGBallPAs_err2**2,na.rm=TRUE))) %>% 
  arrange(desc(contiBiomPAAGB)) #%>%  mutate_if(is.numeric, round, digits=2)

write.csv(new, "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_contixbiome_extraAGB_err_vapr22.csv")


# this is using the  conti x biome AGB to get biome level results, otherwise uncertainitties huge
output %>% group_by(biome) %>%  
  dplyr::summarise(contiBiomPAAGB=sum(allExtraAGB, na.rm=TRUE), contiBiomPAAGB_err=sqrt(sum(totalExtraAGBallPAs_err2**2,na.rm=TRUE))) %>% 
  arrange(desc(contiBiomPAAGB))->biomeExtra


write.csv(biomeExtra, "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_biome_extraAGB_err_vapr22.csv")


#----2.total AGB in PAs at contient x biome level------

output2 <- data.frame()
currentVal <-contiBiomAll
for (r in 1:nrow(currentVal)){
  if (currentVal[r,]$biom_all !=0){
    conti <- currentVal[r,]$region
    biome <- currentVal[r,]$biom
    print(conti)
    print(biome)
    conti_extraAGB_err2 <- contiBiomAggErr2(conti, biome)
    conti_extraAGB_err_output2 <- cbind(conti=conti, biome=biome, conti_extraAGB_err2)
    output2 <- rbind(output2, conti_extraAGB_err_output2)
   } else{
      print(currentVal[r,])
  }
     # write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv", sep = ",",
      #             append = TRUE, quote = FALSE,
      #             col.names =FALSE, row.names = FALSE)
     
}


contiBiomAggErr2 <- function(conti=conti, biome=biome){
  
  #------------loading-------------------
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  
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
  
  
  area <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_apr22_all+area_v2.csv") %>% 
    dplyr::filter(region%in% conti) %>% dplyr::filter(biom %in% biome) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>% 
    dplyr::select(contiBiom, pa_area_all, pa_area, nonMatchPAs,AGBD_diff, weighted_region_var_unmatchedPA,
                  weighted_region_var_PA, weighted_region_var_Ctrl, region_mean_PA, region_mean_Ctrl, region_mean_unmatchedPA)
  
  
  if (conti=="US"){
    l4bResult %>% 
      dplyr::filter(continent %in% conti) %>% 
      mutate(iso3 =sub("_.*", "", rid))->t
    usasub <- t %>% filter(iso3=="USA") %>% filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) %>% 
      mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%
      mutate(biom=stringr::str_remove(biom, paste("_",status, sep=""))) %>% 
      mutate(iso3=paste(iso3, str_extract(biom, "[^_]+"), sep="_")) %>% 
      mutate(biom=sub(".*?_", "", biom)) %>% 
      dplyr::filter(biom %in% biome) %>% dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr) 
    nonusasub <- t %>%filter(iso3!="USA") %>% dplyr::filter(biom %in% biome) %>% 
      dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr)
    contiL4b <- rbind(usasub, nonusasub)
  } else {
    contiL4b <- l4bResult %>% 
      dplyr::filter(continent %in% conti) %>% 
      dplyr::filter(biom %in% biome) %>% dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr) 
    
    
  }
  contiL4b
  
  stat_area <- contiL4b %>% group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area))
  
  contiL4b2 <- contiL4b %>% 
    left_join(stat_area,by="status") %>% 
    mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
    dplyr::select(continent,iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, weighted_region_var) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>% 
    pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var, weighted_region_var)) %>% 
    data.frame() %>%full_join(area, by="contiBiom") 
  
  #fill in NA for the countries with missing variance with continent x biome level extrpolation 
  if ("PA" %notin% unique(contiL4b$status)){
    contiL4b2$weighted_region_var_PA.x <- contiL4b2$weighted_region_var_PA
    contiL4b2$weighted_region_var_Ctrl.x <- contiL4b2$weighted_region_var_Ctrl
    contiL4b2$region_mean_PA.x <- NA
    contiL4b2$region_mean_Ctrl.x <- NA
    contiL4b2$region_area_PA <- NA
    contiL4b2$region_area_Ctrl <- NA
    
  } else {
    contiL4b2$weighted_region_var_Ctrl.x <- ifelse(is.na(contiL4b2$weighted_region_var_Ctrl.x),contiL4b2$weighted_region_var_Ctrl.y, contiL4b2$weighted_region_var_Ctrl.x)
    contiL4b2$weighted_region_var_PA.x <- ifelse(is.na(contiL4b2$weighted_region_var_PA.x),contiL4b2$weighted_region_var_PA.y, contiL4b2$weighted_region_var_PA.x)
    
    contiL4b2$region_mean_Ctrl.x <- ifelse(is.na(contiL4b2$region_mean_Ctrl.x),contiL4b2$region_mean_Ctrl.y, contiL4b2$region_mean_Ctrl.x)
    contiL4b2$region_mean_PA.x <- ifelse(is.na(contiL4b2$region_mean_PA.x),contiL4b2$region_mean_PA.y, contiL4b2$region_mean_PA.x)
    if("region_mean_unmatchedPA.y" %in% colnames(contiL4b2)){
      contiL4b2$region_mean_unmatchedPA.x <- ifelse(is.na(contiL4b2$region_mean_unmatchedPA.x),contiL4b2$region_mean_unmatchedPA.y, contiL4b2$region_mean_unmatchedPA.x)
    } else{
      contiL4b2$region_mean_unmatchedPA.x <- contiL4b2$region_mean_unmatchedPA
      
    }
    
  }
  
  if("region_area_unmatchedPA" %notin% colnames(contiL4b2) ){
    contiL4b2$region_mean_unmatchedPA.x <- NA
    contiL4b2$region_area_unmatchedPA <- NA
    contiL4b2$region_var_unmatchedPA.x <- NA
    contiL4b2$weighted_region_var_unmatchedPA.x <- NA
    
  }
  

  contiL4b3 <- contiL4b2 %>% mutate(weighted_mean_PA=sum(region_mean_PA.x*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
                                    weighted_mean_Ctrl=sum(region_mean_Ctrl.x*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE),
                                    weighted_mean_unmatchedPA=sum(region_mean_unmatchedPA.x*region_area_unmatchedPA, na.rm=TRUE)/sum(region_area_unmatchedPA, na.rm=TRUE))%>% 
    mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA.x, na.rm=TRUE), 
           country_PA_var=sum(weighted_region_var_PA.x, na.rm=TRUE),
           country_Ctrl_var=sum(weighted_region_var_Ctrl.x, na.rm=TRUE)) %>% 
    mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
           country_PA_se=round(sqrt(country_PA_var),5),
           country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
    mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl)
  
  
  contiBiomextra_output1 <-  contiL4b3 %>%
    mutate(total_pa_area=sum(pa_area*100, na.rm=TRUE)) %>% 
    mutate(total_unmatchedpa_area=sum(nonMatchPAs*100,na.rm=TRUE)) %>% 
    # mutate(totalAnalyzedAGB_weighted=tryCatch(weighted_mean_PA*total_pa_area,error = function(e) NA), 
    #        totalNonAnalyzedAGB_weighted=tryCatch(total_unmatchedpa_area*weighted_mean_unmatchedPA, error = function(e) NA) ) %>% 
    # mutate(totalAnalyzedAGB_weighted=ifelse(is.na(totalAnalyzedAGB_weighted), 0, totalAnalyzedAGB_weighted)) %>% 
    # mutate(totalNonAnalyzedAGB_weighted=ifelse(is.na(totalNonAnalyzedAGB_weighted), 0, totalNonAnalyzedAGB_weighted)) %>% 
    
    mutate(totalAnalyzedAGB=region_mean_PA.x*pa_area, totalNonAnalyzedAGB=region_mean_unmatchedPA.x * nonMatchPAs) %>% 
    mutate(totalAnalyzedAGB=ifelse(is.na(totalAnalyzedAGB), 0, totalAnalyzedAGB)) %>% 
    mutate(totalNonAnalyzedAGB=ifelse(is.na(totalNonAnalyzedAGB), 0, totalNonAnalyzedAGB)) %>% 
    # mutate(allPAAGB_weighted=totalAnalyzedAGB_weighted+ totalNonAnalyzedAGB_weighted) %>% 
    mutate(allPAAGB=totalAnalyzedAGB+ totalNonAnalyzedAGB) %>% 
    data.frame()
  
  allAGB_output <- contiBiomextra_output1%>% 
    dplyr::summarise(sumPAAGB=sum(allPAAGB)*100/1000000000, 
                     allExtraAGB_uncer=sqrt((country_PA_se * total_pa_area)**2+(country_unmatched_se *total_unmatchedpa_area)**2 )/1000000000) %>% 
    mutate(type="allPAAGB") %>% unique()
  return(allAGB_output)
  
}

#new is the table for vero with new grouping 
new2 <- output2 %>% group_by(conti) %>% 
  mutate(newgroup= ifelse(biome %in% c("Montane_Grasslands_Shrublands","Flooded_Grasslands_Savannas",
                                       "Temperate_Grasslands_Savannas_Shrublands","Tropical_Subtropical_Grasslands_Savannas_Shrublands"), "GS", 
                          ifelse(biome %in% c("Temperate_Conifer_Forests", "Tropical_Subtropical_Coniferous_Forests"), "CF",as.character(biome)  ))) %>% 
  group_by(conti, newgroup) %>% 
  dplyr::summarise(contiBiomAllPAAGB=sum( sumPAAGB, na.rm=TRUE), contiBiomPAAGB_err=sqrt(sum(allExtraAGB_uncer**2,na.rm=TRUE))) %>% 
  arrange(desc(contiBiomAllPAAGB)) #%>%  mutate_if(is.numeric, round, digits=2)
write.csv(new2, "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_contixbiome_AllAGB_err_vapr22.csv")

#aggregating to the biome level 
output2 %>% group_by(biome) %>%  
  dplyr::summarise(contiBiomAllPAAGB=sum( sumPAAGB, na.rm=TRUE), contiBiomPAAGB_err=sqrt(sum(allExtraAGB_uncer**2,na.rm=TRUE))) %>% 
  arrange(desc(contiBiomAllPAAGB))-> biomeAll


write.csv(biomeAll, "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_biome_allAGB_err_vapr22.csv")


#----3. total AGB in PAs at contient level------


output3 <- data.frame()
currentVal <-biomAll
for (r in 1:nrow(currentVal)){
  if (currentVal[r,]$biom_all !=0){
    # conti <- currentVal[r,]$region
    biome <- currentVal[r,]$biom
    # print(conti)
    print(biome)
    conti_extraAGB_err3 <- contiBiomAggErr3(biome)
    conti_extraAGB_err_output3 <- cbind( biome=biome, conti_extraAGB_err3)
    output3 <- rbind(output3, conti_extraAGB_err_output3)
  } else{
    print(currentVal[r,])
  }
  # write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  
}


contiBiomAggErr3 <- function(biome=biome){
  
  #------------loading-------------------
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  
  l4bResult0 <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022MAR07_CountryBiome_V2_modified.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid0))
  
  l4bResult <-  l4bResult0 %>% 
    filter(grepl("V2", rid, fixed = TRUE)) %>%   #only grab the verson 2 results
    mutate(rid=sub("_V2","",rid)) %>% 
    mutate(iso3 =sub("_.*", "", rid)) %>% 
    mutate(status=gsub("^.*_", "", rid)) %>% 
    mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    left_join(iso_region, by="iso3")
  
  
  area <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_mar08_all+area_v2.csv") %>% 
    dplyr::filter(biom %in% biome) %>% 
    mutate(contiBiom=paste(iso3, biom, sep="_")) %>% 
    dplyr::select(biom,  pa_area_all, pa_area,contiBiom, nonMatchPAs,AGBD_diff, weighted_region_var_unmatchedPA,
                  weighted_region_var_PA, weighted_region_var_Ctrl, region_mean_PA, region_mean_Ctrl, region_mean_unmatchedPA)
  
  
  # if (conti=="US"){
    l4bResult %>%
      # dplyr::filter(continent %in% conti) %>%
      mutate(iso3 =sub("_.*", "", rid))->t
    usasub <- t %>% filter(iso3=="USA") %>% filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) %>%
      mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%
      mutate(biom=stringr::str_remove(biom, paste("_",status, sep=""))) %>%
      mutate(iso3=paste(iso3, str_extract(biom, "[^_]+"), sep="_")) %>%
      mutate(biom=sub(".*?_", "", biom)) %>%
      dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr)
    nonusasub <- t %>%filter(iso3!="USA") %>% 
      dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr)
    l4bResult <- rbind(usasub, nonusasub)
  # } else {
    contiL4b <- l4bResult %>% 
      # dplyr::filter(continent %in% conti) %>% 
      dplyr::filter(biom %in% biome) %>% dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr) 
    
    
  # }
  contiL4b
  
  stat_area <- contiL4b %>% group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area))
  
  contiL4b2 <- contiL4b %>% 
    left_join(stat_area,by="status") %>% 
    mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
    dplyr::select(continent,iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, weighted_region_var) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>%
    pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var, weighted_region_var)) %>% 
    data.frame() %>%full_join(area, by="contiBiom") 
  
  #fill in NA for the countries with missing variance with continent x biome level extrpolation 
  if ("PA" %notin% unique(contiL4b$status)){
    contiL4b2$weighted_region_var_PA.x <- contiL4b2$weighted_region_var_PA
    contiL4b2$weighted_region_var_Ctrl.x <- contiL4b2$weighted_region_var_Ctrl
    contiL4b2$region_mean_PA.x <- NA
    contiL4b2$region_mean_Ctrl.x <- NA
    contiL4b2$region_area_PA <- NA
    contiL4b2$region_area_Ctrl <- NA
    
  } else {
    contiL4b2$weighted_region_var_Ctrl.x <- ifelse(is.na(contiL4b2$weighted_region_var_Ctrl.x),contiL4b2$weighted_region_var_Ctrl.y, contiL4b2$weighted_region_var_Ctrl.x)
    contiL4b2$weighted_region_var_PA.x <- ifelse(is.na(contiL4b2$weighted_region_var_PA.x),contiL4b2$weighted_region_var_PA.y, contiL4b2$weighted_region_var_PA.x)
    
    contiL4b2$region_mean_Ctrl.x <- ifelse(is.na(contiL4b2$region_mean_Ctrl.x),contiL4b2$region_mean_Ctrl.y, contiL4b2$region_mean_Ctrl.x)
    contiL4b2$region_mean_PA.x <- ifelse(is.na(contiL4b2$region_mean_PA.x),contiL4b2$region_mean_PA.y, contiL4b2$region_mean_PA.x)
    if("region_mean_unmatchedPA.y" %in% colnames(contiL4b2)){
      contiL4b2$region_mean_unmatchedPA.x <- ifelse(is.na(contiL4b2$region_mean_unmatchedPA.x),contiL4b2$region_mean_unmatchedPA.y, contiL4b2$region_mean_unmatchedPA.x)
    } else{
      contiL4b2$region_mean_unmatchedPA.x <- contiL4b2$region_mean_unmatchedPA
      
    }
    
  }
  
  if("region_area_unmatchedPA" %notin% colnames(contiL4b2) ){
    contiL4b2$region_mean_unmatchedPA.x <- NA
    contiL4b2$region_area_unmatchedPA <- NA
    contiL4b2$region_var_unmatchedPA.x <- NA
    contiL4b2$weighted_region_var_unmatchedPA.x <- NA
    
  }
  
  
  contiL4b3 <- contiL4b2 %>% mutate(weighted_mean_PA=sum(region_mean_PA.x*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
                                    weighted_mean_Ctrl=sum(region_mean_Ctrl.x*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE),
                                    weighted_mean_unmatchedPA=sum(region_mean_unmatchedPA.x*region_area_unmatchedPA, na.rm=TRUE)/sum(region_area_unmatchedPA, na.rm=TRUE))%>% 
    mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA.x, na.rm=TRUE), 
           country_PA_var=sum(weighted_region_var_PA.x, na.rm=TRUE),
           country_Ctrl_var=sum(weighted_region_var_Ctrl.x, na.rm=TRUE)) %>% 
    mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
           country_PA_se=round(sqrt(country_PA_var),5),
           country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
    mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl)
  
  
  contiBiomextra_output1 <-  contiL4b3 %>%
    mutate(total_pa_area=sum(pa_area*100, na.rm=TRUE)) %>% 
    mutate(total_unmatchedpa_area=sum(nonMatchPAs*100,na.rm=TRUE)) %>% 
    # mutate(totalAnalyzedAGB_weighted=tryCatch(weighted_mean_PA*total_pa_area,error = function(e) NA), 
    #        totalNonAnalyzedAGB_weighted=tryCatch(total_unmatchedpa_area*weighted_mean_unmatchedPA, error = function(e) NA) ) %>% 
    # mutate(totalAnalyzedAGB_weighted=ifelse(is.na(totalAnalyzedAGB_weighted), 0, totalAnalyzedAGB_weighted)) %>% 
    # mutate(totalNonAnalyzedAGB_weighted=ifelse(is.na(totalNonAnalyzedAGB_weighted), 0, totalNonAnalyzedAGB_weighted)) %>% 
    
    mutate(totalAnalyzedAGB=region_mean_PA.x*pa_area, totalNonAnalyzedAGB=region_mean_unmatchedPA.x * nonMatchPAs) %>% 
    mutate(totalAnalyzedAGB=ifelse(is.na(totalAnalyzedAGB), 0, totalAnalyzedAGB)) %>% 
    mutate(totalNonAnalyzedAGB=ifelse(is.na(totalNonAnalyzedAGB), 0, totalNonAnalyzedAGB)) %>% 
    # mutate(allPAAGB_weighted=totalAnalyzedAGB_weighted+ totalNonAnalyzedAGB_weighted) %>% 
    mutate(allPAAGB=totalAnalyzedAGB+ totalNonAnalyzedAGB) %>% 
    data.frame()
  
  allAGB_output <- contiBiomextra_output1%>% 
    dplyr::summarise(sumPAAGB=sum(allPAAGB)*100/1000000000, 
                     allExtraAGB_uncer=sqrt((country_PA_se * total_pa_area)**2+(country_unmatched_se *total_unmatchedpa_area)**2 )/1000000000) %>% 
    mutate(type="allPAAGB") %>% unique()
  return(allAGB_output)
  
}

new2 <- output2 %>% group_by(conti) %>% 
  mutate(newgroup= ifelse(biome %in% c("Montane_Grasslands_Shrublands","Flooded_Grasslands_Savannas",
                                       "Temperate_Grasslands_Savannas_Shrublands","Tropical_Subtropical_Grasslands_Savannas_Shrublands"), "GS", 
                          ifelse(biome %in% c("Temperate_Conifer_Forests", "Tropical_Subtropical_Coniferous_Forests"), "CF",as.character(biome)  ))) %>% 
  group_by(conti, newgroup) %>% 
  dplyr::summarise(contiBiomAllPAAGB=sum( sumPAAGB, na.rm=TRUE), contiBiomPAAGB_err=sqrt(sum(allExtraAGB_uncer**2,na.rm=TRUE))) %>% 
  arrange(desc(contiBiomAllPAAGB)) #%>%  mutate_if(is.numeric, round, digits=2)
write.csv(new2, "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_contixbiome_AllAGB_err.csv")





#----4. extra AGB in PAs at biome level------
#handles the uncertianities 
#do the weighting at the continent or continent x biome level - total AGB is calculated 
# conti <- "Af"
# biome <- "Deserts_Xeric_Shrublands"

output4 <- data.frame()
currentVal <- biomExtra
for (r in 1:nrow(currentVal)){
  if (currentVal[r,]$conti_extra !=0){

    biome <- currentVal[r,]$biom
    print(biome)
    conti_extraAGB_err4 <- contiBiomAggErr4( biome)
    conti_extraAGB_err_output4<- cbind( biome=biome, conti_extraAGB_err4)
    output4 <- rbind(output4, conti_extraAGB_err_output4)
  } else{
    print(currentVal[r,])
    
  }
  # write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv", sep = ",",
  #             append = TRUE, quote = FALSE,
  #             col.names =FALSE, row.names = FALSE)
  
}

contiBiomAggErr4 <- function(biome=biome){
  
  #------------loading-------------------
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  
  l4bResult0 <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022MAR07_CountryBiome_V2_modified.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid0))
  
  l4bResult <-  l4bResult0 %>% 
    filter(grepl("V2", rid, fixed = TRUE)) %>%   #only grab the verson 2 results
    mutate(rid=sub("_V2","",rid)) %>% 
    mutate(iso3 =sub("_.*", "", rid)) %>% 
    mutate(status=gsub("^.*_", "", rid)) %>% 
    mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
    left_join(iso_region, by="iso3")
  
  
  area <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_countryxBiome_byCountry_mar08_extra+area_v2.csv") %>% 
    dplyr::filter(biom %in% biome) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>% 
    dplyr::select(contiBiom, pa_area_all, pa_area, nonMatchPAs,AGBD_diff, weighted_region_var_unmatchedPA,
                  weighted_region_var_PA, weighted_region_var_Ctrl)
  
    l4bResult %>%
      # dplyr::filter(continent %in% conti) %>%
      mutate(iso3 =sub("_.*", "", rid))->t
    usasub <- t %>% filter(iso3=="USA") %>% filter(rid %in% grep("USA_west_|USA_east_|USA_pcfc_",rid, value=TRUE)) %>%
      mutate(biom=stringr::str_remove(rid, paste(iso3,"_",sep=""))) %>%
      mutate(biom=stringr::str_remove(biom, paste("_",status, sep=""))) %>%
      mutate(iso3=paste(iso3, str_extract(biom, "[^_]+"), sep="_")) %>%
      mutate(biom=sub(".*?_", "", biom)) %>%
      dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr)
    nonusasub <- t %>%filter(iso3!="USA") %>% 
      dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr)
    l4bResult <- rbind(usasub, nonusasub)
    contiL4b <- l4bResult %>% 
      filter(iso3 %notin% c("COK","BHR","FSM","SYR","SOM","SPM","PSE","MDA","LBY","PRK","ATF")) %>% 
      dplyr::filter(biom %in% biome) %>% dplyr::select(continent, biom, iso3, status,region_area, region_mean,region_var, region_stderr) 
    
    contiL4b
  
  stat_area <- contiL4b %>% group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area))
  
  contiL4b2 <- contiL4b %>% 
    left_join(stat_area,by="status") %>% 
    mutate(w=region_area/sum_region_area) %>% mutate(weighted_region_var=w**2*region_var) %>% 
    dplyr::select(continent,iso3, status, biom, w, region_mean,sum_region_area, region_area,region_var, weighted_region_var) %>% 
    mutate(contiBiom=paste(iso3, biom,sep="_")) %>% 
    pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var, weighted_region_var)) %>% 
    data.frame() %>%full_join(area, by="contiBiom") 
  
  #fill in NA for the countries with missing variance with continent x biome level extrpolation 
  if ("PA" %notin% unique(contiL4b$status)){
    contiL4b2$weighted_region_var_PA.x <- contiL4b2$weighted_region_var_PA
    contiL4b2$weighted_region_var_Ctrl.x <- contiL4b2$weighted_region_var_Ctrl
    contiL4b2$region_mean_PA <- NA
    contiL4b2$region_mean_Ctrl <- NA
    contiL4b2$region_area_PA <- NA
    contiL4b2$region_area_Ctrl <- NA
    
    
  } else {
    contiL4b2$weighted_region_var_Ctrl.x <- ifelse(is.na(contiL4b2$weighted_region_var_Ctrl.x),contiL4b2$weighted_region_var_Ctrl.y, contiL4b2$weighted_region_var_Ctrl.x)
    contiL4b2$weighted_region_var_PA.x <- ifelse(is.na(contiL4b2$weighted_region_var_PA.x),contiL4b2$weighted_region_var_PA.y, contiL4b2$weighted_region_var_PA.x)
    
  }
  
  if("region_mean_unmatchedPA" %notin% colnames(contiL4b2)){
    contiL4b2$region_mean_unmatchedPA <- NA
    contiL4b2$region_area_unmatchedPA <- NA
    contiL4b2$region_var_unmatchedPA <- NA
    contiL4b2$weighted_region_var_unmatchedPA.x <- NA
  }
  
  contiL4b3 <- contiL4b2 %>% mutate(weighted_mean_PA=sum(region_mean_PA*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
                                    weighted_mean_Ctrl=sum(region_mean_Ctrl*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE),
                                    weighted_mean_unmatchedPA=sum(region_mean_unmatchedPA*region_area_unmatchedPA, na.rm=TRUE)/sum(region_area_unmatchedPA, na.rm=TRUE))%>% 
    mutate(country_unmatched_var=sum(weighted_region_var_unmatchedPA.x, na.rm=TRUE), 
           country_PA_var=sum(weighted_region_var_PA.x, na.rm=TRUE),
           country_Ctrl_var=sum(weighted_region_var_Ctrl.x, na.rm=TRUE)) %>% 
    mutate(country_unmatched_se=round(sqrt(country_unmatched_var),5),
           country_PA_se=round(sqrt(country_PA_var),5),
           country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
    mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl)
  
  
  contiBiomextra_output1 <-  contiL4b3 %>%
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
  
  allExtra_output <- contiBiomextra_output1%>% 
    dplyr::summarise(allExtraAGB_weighted=allExtraAGB_weighted/1000000000,
                     allExtraAGB=allExtraAGB/1000000000,
                     totalExtraAGBallPAs_err=sqrt(allExtraAGB_err**2)/1000000000,
                     totalExtraAGBallPAs_err2=sqrt(((country_PA_se*total_pa_area)**2)+((country_Ctrl_se*total_pa_area)**2)+
                                                     ((country_PA_se*total_unmatchedpa_area)**2)+((country_Ctrl_se*total_unmatchedpa_area)**2))/1000000000) %>%
    # mutate(totalExtraAGB_err=sqrt(extraAGB_err**2)) %>%
    mutate(type="allPAExtraAGB") %>% unique()  
  return(allExtra_output)
  
}

new <- output %>% group_by(conti) %>% 
  mutate(newgroup= ifelse(biome %in% c("Montane_Grasslands_Shrublands","Flooded_Grasslands_Savannas",
                                       "Temperate_Grasslands_Savannas_Shrublands","Tropical_Subtropical_Grasslands_Savannas_Shrublands"), "GS", 
                          ifelse(biome %in% c("Temperate_Conifer_Forests", "Tropical_Subtropical_Coniferous_Forests"), "CF",as.character(biome)  ))) %>% 
  group_by(conti, newgroup) %>% 
  dplyr::summarise(contiBiomPAAGB=sum(allExtraAGB, na.rm=TRUE), contiBiomPAAGB_err=sqrt(sum(totalExtraAGBallPAs_err2**2,na.rm=TRUE))) %>% 
  arrange(desc(contiBiomPAAGB)) #%>%  mutate_if(is.numeric, round, digits=2)

write.csv(new, "/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_contixbiome_extraAGB_err.csv")


