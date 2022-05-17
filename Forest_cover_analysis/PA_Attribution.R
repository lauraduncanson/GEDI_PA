################################################################################
## Attribution Plotting of PA AGBD and Cover Loss Effectiveness
################################################################################
library(raster)
library(sp)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#setwd('C:/Users/sruth/OneDrive - UGent/UMDPostdoc/PA_analysis/')
setwd('/Users/veronika/GEDI_global_PA/WDPA_lost_PA_polygons/New_method/')

setwd('/Users/lduncans/Downloads/')

all_pa_data <- read.csv('all_PAs_forest_loss_VS_agbd_diff_2022APR07.csv')

smoothScatter(all_pa_data$mean_area_loss_diff, all_pa_data$absolute_diff_AGBD, ylab = 'Protected - Unprotected Mean AGBD (Mg/ha)', xlab='Unprotected - Protected Forest Cover Loss (%)', bandwidth=5, nbin=500, nrpoints=0, ,colramp=colorRampPalette(c('white', 'lightgrey', 'dodgerblue3', 'yellow', 'red')), ylim=c(-100,200))
abline(v=0, col='red', lty=2)
abline(h=0, col='red', lty=2)

nPAs <- nrow(all_pa_data)
#calc quadrants
#Q1 is effective for biomass despite exhibiting no forest cover loss; growth signal (loss < 5%)
n_Q1 <- length(which(all_pa_data$mean_area_loss_diff < 0 & all_pa_data$absolute_diff_AGBD > 5.0))
pQ1 <- round(100*(n_Q1/nPAs),2)
label1 <- paste('Effective - Enhanced Growth: ', pQ1, '% of PAs', sep='')
text(-50,150,label1, col='forestgreen')

#Q2 is effective for biomass + cover, avoided emissions
n_Q2 <- length(which(all_pa_data$mean_area_loss_diff > 0 & all_pa_data$absolute_diff_AGBD > 5.0))
pQ2 <- round(100*(n_Q2/nPAs), 2)
label2 <- paste('Effective - Avoided Loss: ', pQ2, '% of PAs', sep='')
text(65,150,label2, col='forestgreen')


#No additionality AGBD is where biomass change is approximately zero (+/- 5 Mg/ha difference)
No_Add <- length(which(abs(all_pa_data$absolute_diff_AGBD) < 5.0))# & abs(all_pa_data$absolute_diff_AGBD) > 5.0))
pNoAdd <- round(100*(No_Add/nPAs))
label3 <- paste('No Additionality in Biomass: ', pNoAdd, '% of PAs', sep='')

#draw a rectangle
rect(-120, 5, 120, -5, col='darkblue', density=10)
text(-60, 10, label3, col='darkblue')

#Q3 Ineffective: encroachment is where AGBD is less in PAs than matches, cover loss higher in PAs (AGBD < -5 Mg/ha, loss <-5%)
n_Q3 <- length(which(all_pa_data$mean_area_loss_diff < -5.0 & all_pa_data$absolute_diff_AGBD < -5.0))
pQ3 <- round(100*(n_Q3/nPAs))
label4 <- paste('Forest Loss within PAs: ', pQ3, '% of PAs', sep='')
text(-60, -90, label4, col='red')

#Q4 Ineffective: AGBD loss that is not detectable in cover signal: degradation (AGBD <- -5 Mg/ha, loss >5%)
n_Q4 <- length(which(all_pa_data$mean_area_loss_diff > 5.0 & all_pa_data$absolute_diff_AGBD < -5.0))
pQ4 <- round(100*(n_Q4/nPAs))
label5 <- paste('Degradation in PAs not observed by Landsat: ', pQ4, '% of PAs', sep='')
text(65, -90, label5, col='red')
