#######this script works side-by-side with the l4a_l4b_results_Country_level_compare.R, and 
###calculates continent x biome level mean AGBD for PA and controls by aggregating the country x biome level results 

#testing example, Eu_Temperate_Broadleaf_Mixed_Forests_Ctrl and PA, because all Eu countries have finished processing except DEU, which needs extrapolation

currentVal <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv") %>% unique()
for (r in 1:nrow(currentVal)){
  if (currentVal[r,]$weighted_agbd_diff !=0){
    conti <- currentVal[r,]$continent
    biome <- currentVal[r,]$biom
    # print(conti)
    # print(biome)
    conti_extraAGB_output <- contiAgg(conti, biome)
  } else{
    print(currentVal[r,])
    conti_extraAGB_output <- currentVal[r,]
  }
  write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_apr22.csv", sep = ",",
              append = TRUE, quote = FALSE,
              col.names =FALSE, row.names = FALSE)
  
}

new <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv") %>% 
  dplyr::filter(!is.na(region_area_Ctrl_sumAs))

new %>% write.csv("/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_mar08_V2.csv")

#-----------adding new entries-------------------------
conti <- "Eu"
biome <- "Tundra"

contiAgg <- function(conti=conti, biome=biome){

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
        dplyr::mutate(rid = ifelse(grepl("west",rid), gsub("USA_west", "USAwest", rid), rid)) %>% 
        dplyr::mutate(rid = ifelse(grepl("east",rid), gsub("USA_east", "USAeast", rid), rid)) %>% 
        dplyr::mutate(rid = ifelse(grepl("pcfc",rid), gsub("USA_pcfc", "USApcfc", rid), rid)) %>% 
       
        mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
        left_join(iso_region, by="iso3") %>% mutate(iso3 =sub("_.*", "", rid))
      
      # l4b_conti_biom <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022JAN19_ContinentBiome.csv") %>% 
      #   mutate(rid0=rid) %>% 
      #   mutate(rid=sub("unmatched_PA","unmatchedPA",rid)) %>% 
      #   mutate(conti =sub("_.*", "", rid)) %>% 
      #   mutate(status=gsub("^.*_", "", rid)) %>% 
      #   mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
      #   mutate(conti_biom=paste(conti,biom,sep="_"))
      
      
        contiL4b <- l4bResult %>%
          dplyr::filter(continent %in% conti) %>% 
          dplyr::filter(biom %in% biome) %>% 
          dplyr::filter(status!="unmatchedPA")
        
        if ("USA" %in% contiL4b$iso3){
          contiL4b <- contiL4b %>% filter(iso3 %notin%"USA")
        }
        
        contiL4b
        
        
     
      
      
      
      # if (conti=="SA" && biome=="Tropical_Subtropical_Moist_Broadleaf_Forests"){
      #   entryBRA <- data.frame(X.x=c(0,1), rid=c("BRA_Tropical_Subtropical_Moist_Broadleaf_Forests_Ctrl","BRA_Tropical_Subtropical_Moist_Broadleaf_Forests_PA"),
      #                         rid_type=c("user","user"), cell_area=c(0,0), region_area=c(983535000000,1096215000000 ),
      #                         region_cell_count=c(957, 737), region_mean=c(146.3847, 183.2286),sample_var=c(0,0),model_var=c(0,0),
      #                         sample_cov=c(0,0), model_cov=c(0,0), region_var=c(9.757362,  0.4123388), region_stderr=c(3.12368, 0.64214), region_rel_stderr=c(0,0),  nshots=c(0,0), 
      #                         ntracks=c(0,0), nonresponse_count=c(0,0), nmodels=c(0,0), pgeversion=c(0,0), generation=c(0,0),
      #                         rid0=c("BRA_Tropical_Subtropical_Moist_Broadleaf_Forests_Ctrl_V2","BRA_Tropical_Subtropical_Moist_Broadleaf_Forests_PA_V2"), 
      #                         iso3=c("BRA","BRA"), status=c("Ctrl","PA"), biom=c("Tropical_Subtropical_Moist_Broadleaf_Forests","Tropical_Subtropical_Moist_Broadleaf_Forests"),
      #                         X.y=c(0,0), continent=c("SA","SA"))
      #   contiL4b <- contiL4b %>% rbind(entryBRA)
      # }
      
      #-----aggregate the region_mean and region_var from country x biome level to continent level, weighting by region_area-----
      conti_stat_area <- contiL4b %>% dplyr::select(continent, status, biom, region_mean,region_var,region_area, region_stderr, region_cell_count, nshots) %>% 
        dplyr::group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area),sum_cell_count=sum(region_cell_count), sum_shot_count=sum(nshots))
      conti_stat_area
      conti_extraAGB_in <- contiL4b %>% 
        left_join(conti_stat_area,by="status") %>%
        mutate(w=region_area/sum_region_area, w2=w**2) %>% mutate(weighted_region_var=w**2*region_var) %>% 
        dplyr::select(continent, status, biom,iso3, w, region_mean,sum_region_area, region_area,region_var, weighted_region_var) %>% 
        # dplyr::filter(status!="unmatchedPA") %>%
        # left_join(isoNonAnalyzedPAs, by=c("biom"="biome")) %>% #data.frame()
        pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var,weighted_region_var)) %>% 
        data.frame()
      conti_extraAGB_only <- conti_extraAGB_in%>% 
        mutate(agbd_diff=region_mean_PA-region_mean_Ctrl) %>% 
        mutate(weighted_mean_PA=sum(region_mean_PA*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
               weighted_mean_Ctrl=sum(region_mean_Ctrl*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE))%>% 
        mutate(country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
               country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
        mutate(country_PA_se=round(sqrt(country_PA_var),5),
               country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
        mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl) 
      conti_extraAGB_only
      
      conti_extraAGB_output <-  conti_extraAGB_only %>% 
        dplyr::select(continent, biom, weighted_agbd_diff,weighted_mean_PA, weighted_mean_Ctrl, country_PA_var, country_Ctrl_var, country_PA_se, country_Ctrl_se,
                      sum_region_area_PA, sum_region_area_Ctrl) %>% 
        unique()
      return(conti_extraAGB_output)
      
}

conti_extraAGB_output <- contiAgg(conti, biome)
conti_extraAGB_output

write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/l4bAGB_continentxbiome_aggregated_apr22.csv", sep = ",",
            append = TRUE, quote = FALSE,
            col.names =FALSE, row.names = FALSE)




#-----------modifying the above functions to handle subsets from AUS and BRA--------------------------------
country="AUS"
biome="Deserts_Xeric_Shrublands"
pattern = "Desert_sub"   #="TSMBF"  #
countryAgg2 <- function(conti=country, biome=biome){
  
  #------------loading-------------------
  iso_region <- read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/iso3_region_pair.csv") %>% 
    rbind(read.csv("/gpfs/data1/duncansongp/GEDI_global_PA/csv/nonanalyzed_iso3_region.csv"))
  
  l4bResult0 <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022APR22_CountryBiome_V2.csv") %>% 
    mutate(rid0=rid) %>% 
    mutate(rid=sub("unmatched_PA","unmatchedPA",rid0))
  
  if (country !="BRA"){
    l4bResult <-  l4bResult0 %>% 
      filter(grepl(pattern, rid, fixed = TRUE)) %>%   #only grab the verson 2 results
      # mutate(rid=sub("_V2","",rid)) %>% 
      mutate(iso3 =paste(sub("_.*", "", rid), extract_numeric(rid),sep="_")) %>% 
      mutate(status=gsub("^.*_", "", rid)) %>% 
      mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid)))
  }  else {
    l4bResult <-  l4bResult0 %>% 
      filter(grepl(pattern, rid, fixed = TRUE)) %>% mutate(subbiom=gsub(".*_TSMBF_(.+)_small.*", "\\1", rid)) %>%   #only grab the verson 2 results
      # mutate(rid=sub("_V2","",rid)) %>% 
      mutate(iso3 =paste(sub("_.*", "", rid), subbiom,extract_numeric(rid),sep="_")) %>% 
      mutate(status=gsub("^.*_", "", rid)) %>% 
      mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
      # filter(rid %notin% c("BRA_TSMBF_lowerAR_small36_PA","BRA_TSMBF_upperAR_small1_PA","BRA_TSMBF_upperAR_small8_PA")) %>% 
      filter(!grepl("small_sub", rid)) 
    
  }
  
  # l4b_conti_biom <- read.csv("/gpfs/data1/duncansongp/leitoldv/gedi_l4b_AGB_results_2022JAN19_ContinentBiome.csv") %>% 
  #   mutate(rid0=rid) %>% 
  #   mutate(rid=sub("unmatched_PA","unmatchedPA",rid)) %>% 
  #   mutate(conti =sub("_.*", "", rid)) %>% 
  #   mutate(status=gsub("^.*_", "", rid)) %>% 
  #   mutate(biom=sub("_[^_]+$", "",gsub("^.*?_", "", rid))) %>% 
  #   mutate(conti_biom=paste(conti,biom,sep="_"))
  
  contiL4b <- l4bResult %>%mutate(continent=country, biom=biome)
    # dplyr::filter(continent %in% conti) %>%
    # dplyr::filter(biom %in% biome) %>%
  #   dplyr::filter(status!="unmatchedPA")
  
  
  #-----aggregate the region_mean and region_var from country x biome level to continent level, weighting by region_area-----
  conti_stat_area <- contiL4b %>% dplyr::select(continent, status, biom, region_mean,region_var,region_area, region_stderr, region_cell_count, nshots) %>% 
    dplyr::group_by(status) %>% dplyr::summarise(sum_region_area=sum(region_area),sum_cell_count=sum(region_cell_count), sum_shot_count=sum(nshots))
  conti_stat_area
  conti_extraAGB_in <- contiL4b %>% 
    left_join(conti_stat_area,by="status") %>%
    mutate(w=region_area/sum_region_area, w2=w**2) %>% mutate(weighted_region_var=w**2*region_var) %>% 
    dplyr::select(continent, status, biom, w,iso3, region_mean,sum_region_area, region_area,region_var, weighted_region_var) %>% 
    # dplyr::filter(status!="unmatchedPA") %>%
    # left_join(isoNonAnalyzedPAs, by=c("biom"="biome")) %>% #data.frame()
    pivot_wider(names_from=status, values_from=c(w, region_mean, sum_region_area, region_area, region_var,weighted_region_var)) %>% 
    data.frame()
  conti_extraAGB_only <- conti_extraAGB_in%>% 
    mutate(agbd_diff=region_mean_PA-region_mean_Ctrl) %>% 
    mutate(weighted_mean_PA=sum(region_mean_PA*region_area_PA, na.rm=TRUE)/sum(region_area_PA, na.rm=TRUE), 
           weighted_mean_Ctrl=sum(region_mean_Ctrl*region_area_Ctrl, na.rm=TRUE)/sum(region_area_Ctrl, na.rm=TRUE))%>% 
    mutate(country_PA_var=sum(weighted_region_var_PA, na.rm=TRUE),
           country_Ctrl_var=sum(weighted_region_var_Ctrl, na.rm=TRUE)) %>% 
    mutate(country_PA_se=round(sqrt(country_PA_var),5),
           country_Ctrl_se=round(sqrt(country_Ctrl_var),5)) %>% 
    mutate(weighted_agbd_diff=weighted_mean_PA-weighted_mean_Ctrl) 
  conti_extraAGB_only
  
  conti_extraAGB_output <-  conti_extraAGB_only %>% 
    dplyr::select(continent, biom, weighted_agbd_diff,weighted_mean_PA, weighted_mean_Ctrl, country_PA_var, country_Ctrl_var, country_PA_se, country_Ctrl_se,
                  sum_region_area_PA, sum_region_area_Ctrl) %>% 
    unique()
  return(conti_extraAGB_output)
  
}

conti_extraAGB_output <- countryAgg2(conti=country, biome=biome)
conti_extraAGB_output

# write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/BRA_notallsub_mar08.csv", sep = ",",
#             append = TRUE, quote = FALSE,
#             col.names =FALSE, row.names = FALSE)
write.table(conti_extraAGB_output, file ="/gpfs/data1/duncansongp/GEDI_global_PA/WDPA_L4b_output/BRA_allsub_apr22.csv", sep = ",",
            append = TRUE, quote = FALSE,
            col.names =FALSE, row.names = FALSE)

