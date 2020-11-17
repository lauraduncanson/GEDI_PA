################################################################################
### match L2A filtered shots to L2B shots and keep only "good quality data"
################################################################################
#f.path <- "/Users/veronika/leitoldv/"
f.path <- "/gpfs/data1/duncansongp/leitoldv/"
WDPA_countries <- read.csv(paste(f.path,"WDPA_countries_table3.csv",sep=""))
IDs <- as.character(WDPA_countries[,"ISO3"])
#
for(v in 1:length(IDs)){
  id <- IDs[v]
  #
  path.l2a  <- paste(f.path,"WDPA_gedi_l2a_clean/",id,"/",sep="")
  path.l2b  <- paste(f.path,"WDPA_gedi_l2b/",id,"/",sep="")
  path.out  <- paste(f.path,"WDPA_gedi_l2a+l2b_clean/",id,"/",sep="")
  f.in  <- length(list.files(path.l2a)); f.in
  f.l2b <- length(list.files(path.l2b)); f.l2b
  #
  setwd(path.out)
  if(f.in == 0){
    sink("emptyDIR.txt")
  } else {
    for(i in 1:length(list.files(path.l2a)))
    {
      file.l2a <- list.files(path.l2a)[i]
      east <- strsplit(file.l2a,"_")[[1]][3]
      nort <- strsplit(file.l2a,"_")[[1]][4]
      file.l2b <- paste(id,"_l2b_",east,"_",nort,".csv",sep="")
      print(file.l2a)
      print(file.l2b)
      #
      data.l2a <- read.csv(paste(path.l2a, file.l2a, sep="")); nrow(data.l2a)
      data.l2b <- read.csv(paste(path.l2b, file.l2b, sep="")); nrow(data.l2b)
      #
      clean.l2a.l2b <- merge(data.l2a, data.l2b, by="shot_number", all.x=T); nrow(clean.l2a.l2b)
      if(nrow(clean.l2a.l2b) == nrow(data.l2a)) { print("row numbers match")}
      cols.to.keep <- c("shot_number", "lon_lowestmode.x", "lat_lowestmode.x", "elev_lowestmode",
                        "rh_000", "rh_001", "rh_002", "rh_003", "rh_004", "rh_005", "rh_006",
                        "rh_007", "rh_008", "rh_009", "rh_010", "rh_011", "rh_012", "rh_013",
                        "rh_014", "rh_015", "rh_016", "rh_017", "rh_018", "rh_019", "rh_020",
                        "rh_021", "rh_022", "rh_023", "rh_024", "rh_025", "rh_026", "rh_027",
                        "rh_028", "rh_029", "rh_030", "rh_031", "rh_032", "rh_033", "rh_034",
                        "rh_035", "rh_036", "rh_037", "rh_038", "rh_039", "rh_040", "rh_041",
                        "rh_042", "rh_043", "rh_044", "rh_045", "rh_046", "rh_047", "rh_048",
                        "rh_049", "rh_050", "rh_051", "rh_052", "rh_053", "rh_054", "rh_055",
                        "rh_056", "rh_057", "rh_058", "rh_059", "rh_060", "rh_061", "rh_062",
                        "rh_063", "rh_064", "rh_065", "rh_066", "rh_067", "rh_068", "rh_069",
                        "rh_070", "rh_071", "rh_072", "rh_073", "rh_074", "rh_075", "rh_076",
                        "rh_077", "rh_078", "rh_079", "rh_080", "rh_081", "rh_082", "rh_083",
                        "rh_084", "rh_085", "rh_086", "rh_087", "rh_088", "rh_089", "rh_090",
                        "rh_091", "rh_092", "rh_093", "rh_094", "rh_095", "rh_096", "rh_097",
                        "rh_098", "rh_099", "rh_100", "quality_flag", "sensitivity.x", 
                        "surface_flag.x", "l2a_quality_flag", "l2b_quality_flag", "surface_flag.y",
                        "sensitivity.y", "pai", "landsat_treecover", "rh100", "omega",
                        "cover_z_000", "cover_z_001", "cover_z_002", "cover_z_003", "cover_z_004",
                        "cover_z_005", "cover_z_006", "cover_z_007", "cover_z_008", "cover_z_009",
                        "cover_z_010", "cover_z_011", "cover_z_012", "cover_z_013", "cover_z_014",
                        "cover_z_015", "cover_z_016", "cover_z_017", "cover_z_018", "cover_z_019",
                        "cover_z_020", "cover_z_021", "cover_z_022", "cover_z_023", "cover_z_024",
                        "cover_z_025", "cover_z_026", "cover_z_027", "cover_z_028", "cover_z_029",
                        "cover", "fhd_normal")
      clean.l2a.l2b <- clean.l2a.l2b[,cols.to.keep]
      #
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "lon_lowestmode.x")] <- "lon_lowestmode"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "lat_lowestmode.x")] <- "lat_lowestmode"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "lat_lowestmode.x")] <- "lat_lowestmode"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "sensitivity.x")]    <- "l2a_sensitivity"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "sensitivity.y")]    <- "l2b_sensitivity"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "surface_flag.x")]   <- "l2a_surface_flag"
      colnames(clean.l2a.l2b)[which(colnames(clean.l2a.l2b) == "surface_flag.y")]   <- "l2b_surface_flag"
      colnames(clean.l2a.l2b)
      #
      ##############################################
      ## check if .csv file already exists for given east, nort tile
      file.old <- paste(id,"_l2a+l2b_",east,"_",nort,"_clean_q1sen95.csv",sep="")
      if(file.exists(paste(path.out, file.old, sep="")) == TRUE){
        #TRUE# add clean.l2a.l2b newdata to existing .csv file
        data.old <- read.csv(paste(path.out,id,"_l2a+l2b_",east,"_",nort,"_clean_q1sen95.csv",sep="")); nrow(data.old)
        if(sum(as.numeric(colnames(clean.l2a.l2b) != colnames(data.old))) == 0){print("all colnames match")}
        ##
        data.NEW <- rbind(data.old, clean.l2a.l2b); nrow(data.NEW)
        if(nrow(data.old)+nrow(clean.l2a.l2b)==nrow(data.NEW)){print("merge sucessful")}
        ##
        file.out <- paste(path.out,id,"_l2a+l2b_",east,"_",nort,"_clean_q1sen95.csv",sep="")
        print(file.out)
        write.csv(data.NEW, file = file.out, row.names = F)
      } else {
        #FALSE# save clean.l2a.l2b newdata to new .csv file
        data.NEW <- clean.l2a.l2b; nrow(data.NEW)
        file.out <- paste(path.out,id,"_l2a+l2b_",east,"_",nort,"_clean_q1sen95.csv",sep="")
        print(file.out)
        write.csv(data.NEW, file = file.out, row.names = F)
      }
      unlink(paste(path.l2b, file.l2b, sep=""))
    }
    l2b.nomatch <- length(list.files(path.l2b))
    l2b.matched <- f.l2b - l2b.nomatch
    unlink(list.files(path.l2a, full.names=T))
    setwd(path.l2a)
    sink(paste("matched",l2b.matched,"of",f.l2b,".txt",sep=""))
  }
  sink()
}

