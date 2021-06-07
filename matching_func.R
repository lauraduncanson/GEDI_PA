options(warn=-1)
options(dplyr.summarise.inform = FALSE)

# ppath <- "/gpfs/data1/duncansongp/amberliang/PADDDtracker_DataReleaseV2_May2019/"
# poly <- readOGR(paste(ppath,"PADDDtracker_DataReleaseV2_May2019_Poly.shp",sep=""),verbose = FALSE) %>% spTransform(., CRS("+init=epsg:6933"))
# pts <- readOGR(paste(ppath,"PADDDtracker_DataReleaseV2_May2019_Pts.shp",sep=""), verbose = FALSE) %>% spTransform(., CRS("+init=epsg:6933"))
# poly$Location_K <- toupper(poly$Location_K)
# poly <- poly[poly$Location_K =="Y",]
# poly$EventType <- as.numeric(poly$EventType) 
# poly$EventType[is.na(poly$EventType)] <- 0
# 
# pts$Location_K <- toupper(pts$Location_K)
# pts <- pts[pts$Location_K =="Y",]
# pts$EventType <- as.numeric(pts$EventType) 
# pts$EventType[is.na(pts$EventType)] <- 0
`%notin%` <- Negate(`%in%`)

# Function to allow rbinding dataframes with foreach even when some dataframes 
# may not have any rows
foreach_rbind <- function(d1, d2) {
  if (is.null(d1) & is.null(d2)) {
    return(NULL)
  } else if (!is.null(d1) & is.null(d2)) {
    return(d1)
  } else if (is.null(d1) & !is.null(d2)) {
    return(d2)
  } else  {
    return(rbind(d1, d2))
  }
}

match_wocat <- function(df, pid) {
  
  registerDoParallel(4)
  
  options("optmatch_max_problem_size"=Inf)
  
  # Filter out countries without at least one treatment unit or without at
  # least one control unit
  # df <- df %>%
  #   filter(complete.cases(.)) %>%
  
  #the following lines do nothing because only one PA at a time
  #mutate(n_treatment=sum(status),
  #       n_control=sum(!status)) %>%
  #the next line doesn't do 
  #filter(n_treatment >= 1, n_control >= 1)
  
  # Note custom combine to handle iterations that don't return any value
  #test nested foreach loops
  ret <- foreach (this_lc=unique(df$land_cover),
                  .packages=c('optmatch', 'dplyr'),
                  .combine=foreach_rbind, .inorder=FALSE) %dopar% {
                    this_d<-df
                    d_wocat <- filter(this_d, status)
                    # Filter out climates and land covers that don't appear in the wocat
                    # sample, and drop these levels from the factors
                    this_d <- filter(this_d,
                                     land_cover %in% unique(d_wocat$land_cover),
                                     wwfbiom %in% unique(d_wocat$wwfbiom),
                                     wwfecoreg %in% unique(d_wocat$wwfecoreg))
                    
                    this_d$land_cover <- droplevels(this_d$land_cover)
                    this_d$wwfbiom <- droplevels(this_d$wwfbiom)
                    this_d$wwfecoreg <- droplevels(this_d$wwfecoreg)
                    # table(this_d$status)
                    f <- status~ mean_temp + max_temp + min_temp + prec + elevation + slope + d2road + d2city + popden + popcnt + tt2city
                    # Can't stratify by land cover or climate if they only have one level
                    if (nlevels(this_d$land_cover) >= 2) {
                      f <- update(f, ~ . + strata(land_cover))
                    } else {
                      f <- update(f, ~ . - land_cover)
                    }
                    if (nlevels(this_d$wwfbiom) >= 2) {
                      f <- update(f, ~ . + strata(wwfbiom))
                    } else {
                      f <- update(f, ~ . - wwfbiom)
                    }
                    if (nlevels(this_d$wwfecoreg) >= 2) {
                      f <- update(f, ~ . + strata(wwfecoreg))
                    } else {
                      f <- update(f, ~ . - wwfecoreg)
                    }
                    if (nrow(d_wocat) > 2) {
                      model <- glm(f, data=this_d)
                      dists <- match_on(model, data=this_d)
                    } else {
                      # Use Mahalanobis distance if there aren't enough points to run a glm
                      dists <- match_on(f, data=this_d)
                    }
                    #potentially drop caliper line; will cut down dists matrix but not the speed issue
                    # dists <- caliper(dists, 2)
                    # If the controls are too far from the treatments (due to the caliper) 
                    # then the matching may fail. Can test for this by seeing if subdim 
                    # runs successfully
                    subdim_works <- tryCatch(is.data.frame(subdim(dists)),
                                             error=function(e)return(FALSE))
                    if (subdim_works) {
                      m <- fullmatch(dists, min.controls=1, max.controls=1, data=this_d)
                      prematch_d <- this_d
                      this_d$matched <- m
                      this_d <- this_d[matched(m), ]
                    } else {
                      this_d <- data.frame()
                    }
                    # Need to handle the possibility that there were no matches for this 
                    # treatment, meaning this_d will be an empty data.frame
                    if (nrow(this_d) == 0) {   #if matching w/ ecoreg return no results, match again w/o ecoreg and check matching results
                      this_d<-df
                      d_wocat <- filter(this_d, status)
                      this_d <- filter(this_d,
                                       land_cover %in% unique(d_wocat$land_cover),
                                       wwfbiom %in% unique(d_wocat$wwfbiom))
                      
                      this_d$land_cover <- droplevels(this_d$land_cover)
                      this_d$wwfbiom <- droplevels(this_d$wwfbiom)
                      f <- status ~ mean_temp + max_temp + min_temp + prec + elevation + slope + d2road + d2city + popden + popcnt + tt2city
                      if (nlevels(this_d$land_cover) >= 2) {
                        f <- update(f, ~ . + strata(land_cover))
                      } else {
                        f <- update(f, ~ . - land_cover)
                      }
                      if (nlevels(this_d$wwfbiom) >= 2) {
                        f <- update(f, ~ . + strata(wwfbiom))
                      } else {
                        f <- update(f, ~ . - wwfbiom)
                      }
                      if (nrow(d_wocat) > 2) {
                        model <- glm(f, data=this_d)
                        dists <- match_on(model, data=this_d)
                      } else {
                        dists <- match_on(f, data=this_d)
                      }
                      subdim_works <- tryCatch(is.data.frame(subdim(dists)),
                                               error=function(e)return(FALSE))
                      if (subdim_works) {
                        m <- fullmatch(dists, min.controls=1, max.controls=1, data=this_d)
                        prematch_d <- this_d
                        this_d$matched <- m
                        this_d <- this_d[matched(m), ]
                      } else {
                        this_d <- data.frame()
                      }
                      if(nrow(this_d)==0){
                        log_mes <- paste(pid,"-Matching without wwfecoreg:Failed\n",sep="")
                        dir.create(paste(paste(f.path,"WDPA_matching_log/",iso3,sep="")))
                        cat(log_mes,file=paste(f.path,"WDPA_matching_log/",iso3,"/",iso3,"_pa_",pid,"_matching_used_covar_log_wk", gediwk,".txt",sep=""),append=TRUE)
                        return(NULL)
                      } else{
                        log_mes <- paste(pid,"-Matching without wwfecoreg:Succeed\n",sep="")
                        dir.create(paste(paste(f.path,"WDPA_matching_log/",iso3,sep="")))
                        cat(log_mes,file=paste(f.path,"WDPA_matching_log/",iso3,"/",iso3,"_pa_",pid,"_matching_used_covar_log_wk", gediwk,".txt",sep=""),append=TRUE)
                        match_results <- list("match_obj" = m, "df" = this_d, "func"=f, "prematch_d"=prematch_d)
                        return(match_results)
                      }
                    } else {
                      log_mes <- paste(pid,"-Matching with wwfecoreg:Succeed\n",sep="")
                      match_results <- list("match_obj" = m, "df" = this_d, "func"=f, "prematch_d"=prematch_d)
                      dir.create(paste(paste(f.path,"WDPA_matching_log/",iso3,sep="")))
                      cat(log_mes,file=paste(f.path,"WDPA_matching_log/",iso3,"/",iso3,"_pa_",pid,"_matching_used_covar_log_wk", gediwk,".txt",sep=""),append=TRUE)
                      return(match_results)
                    }
                  }
  
  stopImplicitCluster()
  return(ret)
}

propensity_filter <- function(pa_df, d_control_local){
  pa_df <-pa_df[complete.cases(pa_df), ]  #filter away non-complete cases w/ NA in control set
  d <- dplyr::bind_rows(d_control_local, pa_df)
  ## bring in matching algorithm from STEP5 here to loop through each PA in d_PAs
  #filter controls based on propensity scores 
  d_all <- dplyr::select(d, lat, lon, UID, status, land_cover, wwfbiom, wwfecoreg, elevation, slope,
                         mean_temp,max_temp,min_temp, prec, d2road, d2city,  popden, tt2city, popcnt) 
  
  d_all$status <- ifelse(d_all$status==TRUE,1,0)
  
  #calculate the propensity scores & filter out controls not overlapping w/ treatment propensity scores
  ps <- glm(status ~ mean_temp+max_temp+min_temp + prec + elevation + slope+ d2road + d2city + popden +popcnt+ tt2city,data = d_all)
  # boxplot(ps)  #check the distribution of propensity scores for treatment and controls
  #filter out the controls with propensity scores outside of the overlapping region
  d_all$propensity_score <- fitted(ps)
  d_sep <- d_all %>% dplyr::group_by(status)
  d_sep_range <- d_all %>% dplyr::group_by(status)%>% 
    dplyr::summarise(propmin= min(propensity_score), promax=max(propensity_score)) 
  # cat(iso3, "Filtering the control sites by overlaping with the treatment PS\n")
  d_filtered <- d_sep %>% 
    filter(status==1 | between(propensity_score,d_sep_range$propmin[2],d_sep_range$promax[2])) %>% 
    ungroup() 
  
  d_filtered$status <- ifelse(d_filtered$status==1,TRUE,FALSE)
  
  return(d_filtered)
}

# isoPadddRas <- function(poly, pts, rtemplate){
#   
#   if((iso3 %in% unique(poly$ISO3166))&&(iso3 %notin% unique(pts$ISO3166))){
#     # print("in poly")
#     polysub <- poly[poly$ISO3166==iso3,]
#     polysubr <- rasterize(polysub,rtemplate,background=NA, field=polysub$EventType)
#     names(polysubr) <- "PADDD"
#     return(polysubr)
#     
#   } else if ((iso3 %notin% unique(poly$ISO3166))&&(iso3 %in% unique(pts$ISO3166))){
#     # print("in pts")
#     ptssub <- pts[pts$ISO3166==iso3,]
#     ptssubr <- rasterize(ptssub@coords[,1:2,drop=FALSE],rtemplate,background=NA, field=ptssub$EventType)
#     names(ptssubr) <- "PADDD"
#     return(ptssubr)
#     
#   } else if ((iso3 %in% unique(poly$ISO3166))&&(iso3 %in% unique(pts$ISO3166))){
#     # print("in both")
#     polysub <- poly[poly$ISO3166==iso3,]
#     polysubr <- rasterize(polysub,rtemplate,background=NA, field=polysub$EventType)
#     ptssub <- pts[pts$ISO3166==iso3,]
#     ptssubr <- rasterize(ptssub@coords[,1:2,drop=FALSE],rtemplate,background=NA, field=ptssub$EventType)
#     m <- merge(polysubr,ptssubr)
#     names(m) <- "PADDD"
#     return(m)
#   } else {
#     # print("in neither")
#     empr <- rtemplate
#     values(empr) <- NA
#     names(empr) <- "PADDD"
#     return(empr)
#   }
# }
matched2ras <- function(matched_df){
  cat(iso3,"converting the matched csv to a raster stack for extraction\n")
  
  matched_pts <- SpatialPointsDataFrame(coords=matched_df[,c("lon","lat")],
                                          proj4string=CRS("+init=epsg:4326"), data=matched_df) %>% 
    spTransform(., CRS("+init=epsg:6933"))
  
    matched_pts$UID <- as.integer(matched_pts$UID)
  matched_pts$pa_id <- as.integer(matched_pts$pa_id)
  matched_pts$status <- as.logical(matched_pts$status)
  # matched_pts$REP_AREA <- matched_pts$REP_AREA%>% as.numeric()
  # matched_pts$PA_STATUSYR <- matched_pts$PA_STATUSYR%>% as.integer()
  # 
  cols <- c("wwfbiom","wwfecoreg")
  matched_pts@data[,cols] %<>% lapply(function(x) as.numeric(x))
  matched_pts@data[,cols][is.na(matched_pts@data[,cols])]<- 0
  
  # #create an empty raster with 1km resolution
  r <- crop(MCD12Q1, extent(buffer(matched_pts,10000))) 
  continent <- crop(world_region,extent(buffer(matched_pts,10000)))
  names(r) <- "pft"
  names(continent) <- "region"
  
  # padddr <- isoPadddRas(poly=poly,pts=pts, rtemplate = r)
  
  matched_ras <- rasterize(matched_pts@coords, r,
                           field=matched_pts@data[,c("status","pa_id","wwfbiom","wwfecoreg","UID")],background=NA)%>% 
    stack(r) %>% stack(continent) 
  # %>% stack(padddr)
  
  return(matched_ras)
}

convertFactor <- function(matched0, exgedi){
  exgedi$pft <- as.character(exgedi$pft)
  
  exgedi$pft <- factor(exgedi$pft, levels=sequence(6),
                                 labels = c("ENT",
                                            "EBT",
                                            "ENT",
                                            "DBT",
                                            "GS",
                                            "GS"))
  exgedi$region <- as.character(exgedi$region)
  exgedi$region <- factor(exgedi$region, levels=c(1:7),
                       labels = c("Eu",
                                  "As",
                                  "Au",
                                  "Af",
                                  "As",
                                  "SA",
                                  "US"))
  
  exgedi$stratum <- paste(exgedi$pft, exgedi$region,sep="_")
  
  # exgedi$GOV_TYPE <- exgedi$GOV_TYPE %>% 
  #   factor(levels=seq(length(levels(matched0$GOV_TYPE))),
  #          labels=levels(matched0$GOV_TYPE))
  # 
  # exgedi$OWN_TYPE <- exgedi$OWN_TYPE %>% 
  #   factor(levels=seq(length(levels(matched0$OWN_TYPE))),
  #          labels=levels(matched0$OWN_TYPE))  
  
  # exgedi$DESIG_ENG <- exgedi$DESIG_ENG %>% 
  #   factor(levels=seq(length(levels(matched0$DESIG_ENG))),
  #          labels=levels(matched0$DESIG_ENG))  
  
  exgedi$wwfbiom <- exgedi$wwfbiom %>% 
    factor(levels=seq(length(levels(matched0$wwfbiom))),
           labels=levels(matched0$wwfbiom))  
  
  exgedi$wwfecoreg <- exgedi$wwfecoreg %>% 
    factor(levels=seq(length(levels(matched0$wwfecoreg))),
           labels=levels(matched0$wwfecoreg))  
  
  # tryCatch(exgedi$paddd <- as.character(exgedi$paddd), error=function(e) return(NULL))
  # tryCatch(exgedi$paddd[which(exgedi$paddd=="1")] <- "Downgrade", error=function(e) return(NULL))
  # tryCatch(exgedi$paddd[which(exgedi$paddd=="2")] <- "Degazette", error=function(e) return(NULL))
  # tryCatch(exgedi$paddd[which(exgedi$paddd=="3")] <- "Downsize", error=function(e) return(NULL))
  
  return(exgedi)
}

subdfExport <- function(filtered_df){
  #export invidual pa results
  spt2 <- split(filtered_df, filtered_df$pa_id)
  
  dfl <- lapply(names(spt2), function(x){
  
    if(dim(spt2[[x]])[1]>0){
      control_sub <- spt2[[x]][spt2[[x]]$status==0,]
      treat_sub <-  spt2[[x]][spt2[[x]]$status==1,]
      ncontrol <- nrow(control_sub)
      ntreat <- nrow(treat_sub)
      if (ntreat-ncontrol > 0){
        newtreatid <- sample(ntreat, ncontrol)
        newtreat <- treat_sub[newtreatid,]
        spt2_new <- rbind(newtreat, control_sub)
      } else if (ntreat - ncontrol< 0){
        newcontrolid <- sample(ncontrol, ntreat)
        newcontrol <- control_sub[newcontrolid,]
        spt2_new <- rbind(newcontrol, treat_sub)
      } else if (ntreat-ncontrol==0){
        spt2_new <- spt2[[x]]
      } else if (ntreat==0 || ncontrol==0){
        spt2_new=NA
      }
      biom <- spt2_new$wwfbiom %>% unique() %>% as.character() %>% gsub('\\b(\\pL)\\pL{4,}|.','\\U\\1',.,perl = TRUE)
      if(length(biom)>1){
        biom <- paste(c(biom), collapse="&")
      }
      # print(biom)
      write.csv(spt2_new, file=paste(f.path,"WDPA_GEDI_extract/",iso3,"_wk",gediwk,"/",iso3,"_PA_",unique(spt2_new$pa_id),"_",biom,".csv", sep=""))
      return(spt2_new)
    }
  })
  
  total_df <- do.call("rbind", dfl) 
  cat("Exported individual PAs results for ", iso3, "\n")
  
  return(total_df)
}

getmode <- function(v,na.rm) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

extract_gedi <- function(matched, mras){
    lon_bond <- range(matched$lon,na.rm=TRUE)
    lat_bond <- range(matched$lat,na.rm=TRUE)
    all_gedil2_f <- list.files(file.path(f.path,"WDPA_gedi_l2a+l2b_clean2",iso3), full.names = FALSE) 
    all_gedil4_f <- list.files(file.path(f.path,"WDPA_gedi_l4a_clean",iso3), full.names = FALSE) 
    gedil2_f <- all_gedil2_f%>% strsplit( "_") %>% 
      as.data.frame() %>% 
      t() %>% as.data.frame(row.names =all_gedil2_f, stringsAsFactors=FALSE,make.names=FALSE) %>% dplyr::select(V3,V4) %>% 
      mutate(lons=as.numeric(gsub('\\D','', V3)), ew= gsub('\\d','', V3) ) %>% 
      mutate(lats= as.numeric(gsub('\\D','', V4)), ns= gsub('\\d','', V4) ) %>% 
      mutate( lons = ifelse(ew!="E", -1*lons, lons)) %>% 
      mutate( lats = ifelse(ns!="N", -1*lats, lats)) %>% 
      dplyr::filter( between(lons, floor(lon_bond[1]), ceiling(lon_bond[2]))) %>% 
      dplyr::filter(between(lats, floor(lat_bond[1]), ceiling(lat_bond[2]))) %>% rownames()
    gedil4_f <- all_gedil4_f%>% strsplit( "_") %>% 
      as.data.frame() %>% 
      t() %>% as.data.frame(row.names =all_gedil4_f, stringsAsFactors=FALSE,make.names=FALSE) %>% dplyr::select(V3,V4) %>% 
      mutate(lons=as.numeric(gsub('\\D','', V3)), ew= gsub('\\d','', V3) ) %>% 
      mutate(lats= as.numeric(gsub('\\D','', V4)), ns= gsub('\\d','', V4) ) %>% 
      mutate( lons = ifelse(ew!="E", -1*lons, lons)) %>% 
      mutate( lats = ifelse(ns!="N", -1*lats, lats)) %>% 
      dplyr::filter( between(lons, floor(lon_bond[1]), ceiling(lon_bond[2]))) %>% 
      dplyr::filter(between(lats, floor(lat_bond[1]), ceiling(lat_bond[2]))) %>% rownames()  #should result in same # of files as the l2 products
    
    registerDoParallel(cores=round(mproc*0.5))
    ex_out <- foreach(this_csvid=seq(length(gedil2_f)), .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
        ##add the GEDI l4a model prediction for AGB here :
        cat("Readng in no. ", this_csvid,"csv of ", length(gedil2_f),"csvs for iso3",iso3,"\n")
        gedi_l2  <- read.csv(paste(f.path,"WDPA_gedi_l2a+l2b_clean2",iso3,gedil2_f[this_csvid], sep="/")) %>%
          dplyr::select(shot_number,lon_lowestmode, lat_lowestmode,rh_025, rh_050, rh_075, rh_098,cover, pai)
        l2_latlon <- gedil2_f[this_csvid] %>%  str_split("_") %>% unlist %>% .[3:4] %>% paste(sep="_", collapse ="_") %>% paste("_",.,sep="")
        l4_pattern <- tryCatch(grep(l2_latlon, gedil4_f, value=TRUE), error=function(cond){return(NA)})
        gedi_l4  <- tryCatch(read.csv(paste(f.path,"WDPA_gedi_l4a_clean",iso3,l4_pattern, sep="/")), error=function(cond){return(NA)})
                            
        if (is.na(gedi_l4) || nrow(gedi_l4) < 1){
          cat("error")
          gedi_l24 <- gedi_l2
          gedi_l24$agbd <- NA
          gedi_l24$agbd_se <- NA
          gedi_l24$agbd_t <- NA
          gedi_l24$agbd_t_se <- NA
        } else {
          gedi_l4_sub <- gedi_l4 %>%
            dplyr::select(shot_number, agbd, agbd_se, agbd_t, agbd_t_se)
          gedi_l24 <- inner_join(gedi_l2, gedi_l4_sub, by="shot_number")
        
        }
      # gedi_l24[rowSums(is.na(gedi_l24)) > 0, ]   
      # gedi_l24 <- left_join(gedi_l2, gedi_l4, by="shot_number") %>% drop_na()
      iso_matched_gedi_df <- data.frame()
      if(nrow(gedi_l24)>0){
        gedi_l24_sp <- gedi_l24 %>% 
          SpatialPointsDataFrame(coords=.[,c("lon_lowestmode","lat_lowestmode")],
                                 proj4string=CRS("+init=epsg:4326"), data=.) %>%spTransform(., CRS("+init=epsg:6933"))
        
        matched_gedi <- raster::extract(mras,gedi_l24_sp, df=TRUE)
        matched_gedi_metrics <- cbind(matched_gedi,gedi_l24_sp@data)
        matched_gedi_metrics_filtered <- matched_gedi_metrics %>% dplyr::filter(!is.na(status)) %>% 
          convertFactor(matched0 = matched,exgedi = .) 
        
        iso_matched_gedi_df <- rbind(matched_gedi_metrics_filtered,iso_matched_gedi_df)
        
      }
      return(iso_matched_gedi_df)
  }
  stopImplicitCluster()
  cat("Done GEDI for no. ",grep(unique(matched$pa_id), matched_PAs),"pa out of", length(matched_PAs),"\n")
  return(ex_out)
}

SplitRas <- function(raster,ppside){
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(raster,e1)
  }
  return(r_list)
}

rasExtract2020 <- function(l4_sp){
  # cat(iso3,"converting the matched csv to a raster stack for extraction\n")
  tif2020 <- c("pop_cnt_2020","pop_den_2020","lc2019","tt2cities_2015","wc_prec_2010-2018","wc_tavg_2010-2018","wc_tmax_2010-2018",
               "wc_tmin_2010-2018","wwf_biomes","wwf_ecoreg","dem","slope","d2roads","dcities")
  for (t in 1:length(tif2020)){
    # print(tif2020[t])
    covar2020 <- raster(paste(f.path, "WDPA_input_vars_iso3_v2/",iso3,"/",tif2020[t],".tif", sep=""))
    ras_ex <- raster::extract(covar2020, l4_sp@coords, method="simple", factors=F)
    nm <- names(covar2020)
    l4_sp <- cbind(l4_sp, ras_ex)
    names(l4_sp)[t+6] <- tif2020[t]
  }
  return(l4_sp)
}

# iso_matched_gedi <- foreach(this_csv=gedil2_f, .combine = foreach_rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
#   ##add the GEDI l4a model prediction for AGB here :
#   cat("Readng in no. ", match(this_csv, gedil2_f),"csv of ", length(gedil2_f),"csvs for iso3",iso3,"\n")
#   gedi_l2  <- read.csv(paste(f.path,"WDPA_gedi_l2a+l2b_clean",iso3,this_csv, sep="/")) %>%
#     dplyr::select(shot_number,lon_lowestmode, lat_lowestmode, starts_with("rh_"),cover, pai)%>%
#     SpatialPointsDataFrame(coords=.[,c("lon_lowestmode","lat_lowestmode")],
#                            proj4string=CRS("+init=epsg:4326"), data=.) %>%spTransform(., CRS("+init=epsg:6933"))
#   
#   iso_matched_gedi_df <- data.frame()
#   matched_gedi <- raster::extract(mras,gedi_l2, df=TRUE)
#   matched_gedi_metrics <- cbind(matched_gedi,gedi_l2@data)
#   
#   matched_gedi_metrics_filtered <- matched_gedi_metrics %>% dplyr::filter(!is.na(status)) %>% 
#     convertFactor(matched0 = matched,exgedi = .) 
#   
#   matched_gedi_l4a <-matched_gedi_metrics_filtered %>% 
#     dplyr::mutate(
#       LAT=lat_lowestmode,
#       LON=lon_lowestmode,
#       REGION=region,
#       PFT=pft,
#       RH_10=rh_010+100,
#       RH_20=rh_020+100,
#       RH_30=rh_030+100,
#       RH_40=rh_040+100,
#       RH_50=rh_050+100,
#       RH_60=rh_060+100,
#       RH_70=rh_070+100,
#       RH_80=rh_080+100,
#       RH_90=rh_090+100,
#       RH_98=rh_098+100) %>% 
#     modelr::add_predictions(model2, "AGBD")
#   iso_matched_gedi_df <- rbind(matched_gedi_l4a,iso_matched_gedi_df)
#   return(iso_matched_gedi_df)
# }
# stopImplicitCluster()
# 
