# GEDI_PA
Scripts for analyzing GEDI data in global protected areas

## PA analysis workflow (GLOBAL)  *11/23/2020*

### STEP0a.
Subset global GEDI data (gedi_subsets_by_country.R creates python scripts to subset 1º tiles country-by-country; then run python scripts on cluster to create subsets of L2A and L2B data)
### STEP0b.
Filter out “bad quality” GEDI data (gedi_filter_L2A.R keeps only quality == 1 & sensitivity >= 0.95 shots)
### STEP0c.
Match up L2B data with quality L2A shots to create combined data files (gedi_filter_L2A+L2B.R)

## PROCESSING: global_process.R (also using “matching_func.R”)

*This global processing script is derived from the global processing notebook; the input can be the iso3 code (3-character) for one or multiple countries* 

### STEP1:
Create 1km sampling grid with valid cells only where GEDI data is available

### STEP2:
Prepare CONTROL (i.e. non-PA) dataset for iso3 country
- 2.1) clip 1km sampling grid to nonPA area within country boundary (excluding 10km buffer around PAs)
- 2.2) compile covariates by extracting rasters at the nonPA grid cell coordinates
- 2.3) save dataset to prepped_control.RDS file

### STEP3:
Prepare TREATMENT (i.e. PA) datasets for iso3 country
- 3.1) clip 1km sampling grid to each PA within country
- 3.2) sample raster layers on each PA grid
- 3.3) save each PA sample into prepped_pa_##.RDS file

*Point matching adapted from Alex Zvoleff's original script (github.com/azvoleff/wocat_matching)*

### STEP4:
Perform matching for iso3 country
- 4.1) set up spatial points data frames (control + each PA) for point matching
- 4.2) filter away non-complete cases w/ NA in control set
- 4.3) return a df of control + treatment after complete cases and propensity filters are applied
          - sample the control dataset to the size of the sample dataset; iterate until full number of matches found
          - outside of the match_wocat function check the balance
          - do a glm here to throw out definitely not control data based on low propensity score

### STEP5:
GEDI data processing after point matching
- 5.1) use GEDI shots to extract the treatment/control status, also extract the MODIS PFT for AGB prediction 
- 5.2) rasterize the GEDI results ("rh_050","rh_098","cover","pai”, ”AGBD”)
- 5.3) compare treatment + control metrics and export per pa df, then return entire country result 

### STEP6:
Plotting treatment (PA) vs control (non-PA) comparison


### Examples for running the runisobash for global PA matching is 
- At the command prompt first run'chmod 775 runisobash' to run runisobashinto executable 
- Then run './runisobash -l "TCD TUR AGO" -w 275 -n 3 -p 10' to run countries with iso3 codes TCD, TUR, AGO on 3(-n) processors and each initiates 10(-p) cores to prarallelize across 10PAs; -w indicates the process uses GEDI data up till week 275 and this is reflected in the output file names
