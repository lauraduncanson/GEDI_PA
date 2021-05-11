# GEDI_PA analysis
Scripts for analyzing GEDI data in global protected areas

## PA analysis workflow (GLOBAL)  as of 05/11/2021

### PRE-PROCESSING:
#### STEP0a.
Subset & filter global GEDI L4A data country-by-country (filter: quality_flag==1)
Scripts: gedi_subsets_by_country_L4A.R and gedi_filter_L4A.R

#### STEP0b.
Subset global GEDI L2A data (with key metrics extracted for all algorithm setting selections 1-6) then select L2A metrics set based on “selected_algorithm” value from corresponding L4A data and filter L2A shots based on quality (==1) and sensitivity (>=0.95)
Scripts: gedi_subsets_by_country_L2AB.R and gedi_filter_L2A_w_algorithm_selection.R

#### STEP0c.
Subset global GEDI L2B data (with key metrics extracted for all algorithm setting selections 1-6) then filter L2B data by matching shots with L2A filtered data, then calculate cover and pai metrics based on correct algorithm setting using pgap_theta and local_beam_elevation info
Scripts: gedi_subsets_by_country_L2AB.R and gedi_filter_L2B_w_algorithm_selection.R


### PROCESSING1: global_process.R (also using “matching_func.R”)

*This global processing script is derived from the global processing notebook; the input can be the iso3 code (3-character) for one or multiple countries* 

*Currently using “gediwk <- 18” as input (referring to the first 18 months of GEDI data used)*

### Examples for running the runisobash for global PA matching 
- At the command prompt first run 'chmod 775 runisobash' to turn runisobash into executable 
- Then run './runisobash -l "TCD TUR AGO" -w 18 -n 3 -p 10' to run countries with iso3 codes TCD, TUR, AGO on 3(-n) processors and each initiates 10(-p) cores to parallelize across 10PAs; -w indicates the process uses GEDI data from first 18 months and this is reflected in the output file names

#### STEP1:
Create 1km sampling grid with points only where GEDI data is available (first check if grid file exist to avoid reprocessing); apply filter with min # of GEDI shots in each 1km cell (currently set to 5); Save sampling grid to /WDPA_grids/iso3_grid_wk##.RDS

#### STEP2:
Prepare CONTROL (i.e. non-PA) dataset for iso3 country
- 2.1) clip 1km sampling grid to nonPA area within country boundary (excluding 10km buffer around PAs)
- 2.2) compile covariates by extracting rasters at the nonPA grid cell coordinates
- 2.3) save dataset to /WDPA_matching_points/iso3/iso3_prepped_control.RDS file

#### STEP3:
Prepare TREATMENT (i.e. PA) datasets for iso3 country
- 3.1) clip 1km sampling grid to each PA within country
- 3.2) sample raster layers on each PA grid
- 3.3) save each PA sample into /WDPA_matching_points/iso3/iso3_testPAs/prepped_pa_##.RDS file

*Point matching adapted from Alex Zvoleff's original script (github.com/azvoleff/wocat_matching)*

#### STEP4:
Perform matching for iso3 country
- 4.1) set up spatial points data frames (control + each PA) for point matching
- 4.2) filter away non-complete cases w/ NA in control set
- 4.3) return a df of control + treatment after complete cases and propensity filters are applied
          - sample the control dataset to the size of the sample dataset; iterate until full number of matches found
          - outside of the match_wocat function check the balance
          - do a glm here to throw out definitely not control data based on low propensity score
- 4.4) save matching results to /WDPA_matching_results/iso3_wk##/iso3_pa_##_matching_results_wk##.RDS"

***update: matching mechanism has been altered to balance the matched protected and control

### PROCESSING2: global_process_part2.R

*This global processing script PART II is derived from the global processing notebook*
*the input can be the iso3 code (3-character) for one or multiple countries*

#### STEP5:
GEDI data processing after point matching - using GEDI shots to extract the treatment/control status, also extract the MODIS PFT for AGB prediction
- 5.1) use GEDI shots to extract the treatment/control status, also extract the MODIS PFT for AGB prediction 
- 5.2) rasterize the GEDI results ("rh_050","rh_098","cover","pai”, ”AGBD”)
- 5.3) compare treatment + control metrics and export per pa df, then return entire country result 

#### STEP6:
[FIGURE 4B] Calculating per pa summary stats, 1 pa per row, contain shot#/PA

#### STEP7:
[Figure 4A] Removing dup gedi shots in overlapping region, count shot#/PA w/o dups

#### STEP8:
Calculating per country summary stats, 1 country per row, summarize key stats for the country

#### STEP9a:
Random Forest Modelling AGBD w/ 2020 Covars

#### STEP9b:
RF model for other structure metrics RH98, COVER, PAI


-------------------------------------------------------------------------------------------------------------------------------

# GEDI_PA figures & tables
Scripts for generating figures and tables for the paper

...
