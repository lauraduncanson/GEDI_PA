
path.in  <- "/Users/veronika/GEDI_global_PA/Dissolved_old"
countries <- dir(path.in)
countries


for(i in 1:length(countries)){
  #i=26
  countries[i]
  DIR <- paste(path.in, countries[i], sep="/")
  setwd(DIR)
  
  filenames.in <- list.files(DIR)
  print(filenames.in)
  
  for(z in 1:length(filenames.in)){
    file.in <- filenames.in[z]
    print(file.in)
    file.out <- gsub(" & ", "_", file.in)  ## replace " & " with "_"
    file.out <- gsub(" ", "_", file.out)  ## replace " " with "_"
    file.out <- gsub(", ", "_", file.out)  ## replace ", " with "_"
    file.out <- gsub(",_", "_", file.out)  ## replace ",_" with "_"
    file.out <- gsub("_PA", "_Control", file.out, ignore.case = FALSE)
    file.out <- gsub("_Ctrl", "_Treatment", file.out, ignore.case = FALSE)
    print(file.out)
    file.rename(file.in, file.out)
  }
  
  filenames.out <- list.files(DIR)
  print(filenames.out)
}

for(i in 1:length(countries)){
  #i=26
  countries[i]
  DIR <- paste(path.in, countries[i], sep="/")
  setwd(DIR)
  
  filenames.in <- list.files(DIR)
  print(filenames.in)
  
  for(z in 1:length(filenames.in)){
    file.in <- filenames.in[z]
    print(file.in)
    file.out <- gsub("_Treatment", "_PA", file.in, ignore.case = FALSE)
    file.out <- gsub("_Control", "_CTRL", file.out, ignore.case = FALSE)
    print(file.out)
    file.rename(file.in, file.out)
  }
  
  filenames.out <- list.files(DIR)
  print(filenames.out)
}

#################################################################3
##################################################################
path.in  <- "/Users/veronika/GEDI_global_PA/Dissolved"
countries <- dir(path.in)
countries

for(i in 1:length(countries)){
  #i=26
  print(countries[i])
  DIR <- paste(path.in, countries[i], sep="/")
  print(length(list.files(DIR, pattern=".shp")))
}
  
  