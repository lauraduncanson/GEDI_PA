#-------------------------------------------------------------------------------
# 2023 FEB 07 -- updated version -- biomass numbers converted to CARBON (*0.49)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 2022 FEB 03 -- updated version -- fewer biomes to plot (&bars on different scales)
#-------------------------------------------------------------------------------
library(rgdal)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(rgeos)
library(maptools)
library(ggmap)
library(tidyr)
library(dplyr)
library(raster)
library(maps)
library(magick)
library(rnaturalearth)
library(rnaturalearthdata)
##original script: "/Users/veronika/leitoldv/figures/GEDI_PA_fig9_barchart_biomes2.R"
#f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/csv/"
f.path <- "/Users/veronika/GEDI_global_PA/"

#df1 <- read.csv(paste(f.path, "l4bAGB_contixbiome_extraAGB_err.csv", sep=""))
#df1 <- read.csv(paste(f.path, "l4bAGB_contixbiome_extraAGB_err_vapr22.csv", sep=""))
df1 <- read.csv(paste(f.path, "l4bAGB_contixbiome_extraAGB_err_vapr22_C.csv", sep=""))
df2 <- read.csv(paste(f.path, "l4bAGB_contixbiome_AllAGB_err.csv", sep=""))
head(df1); nrow(df1)
head(df2); nrow(df2)
##
df <- df1 %>% inner_join(df2, by=c("conti","newgroup"))
head(df); nrow(df)
colnames(df) <- c("X","continent","biome","contiBiomPAAGBextra","contiBiomPAAGBextra_err",
                  "Y","contiBiomPAAGBtotal","contiBiomPAAGBtotal_err")
##
df.biom <- sort(unique(as.vector(df$biome)))
df.biom
df$biomID2 <- as.numeric(df$biome)
df
#-------------------------------------------------------------------------------
################################################################################
biomes2 <- c("BorF: Boreal Forest / Taiga",
             "ConF: Temperate & Tropical Conifer Forests",  ##TeCF + TrCF
             "DesX: Deserts & Xeric Shrublands",
             "GrSS: Temperate & Tropical Grasslands, Savannas & Shrublands",  ##TeG + TrG + MtG + FlG
             "Mang: Mangroves",
             "MedF: Mediterranean Forests, Woodlands & Scrub",
             "TeBF: Temperate Broadleaf & Mixed Forests",
             "TrDF: Tropical & Subtropical Dry Broadleaf Forests",
             "TrMF: Tropical & Subtropical Moist Broadleaf Forests",
             "Tund: Tundra")
biomID2 <- c("BorF","ConF","DesX","GrSS","Mang","MedF","TeBF","TrDF","TrMF","Tund")
mypalette2 <- colors()[c(639,104,411,76,435,114,139,83,47,400)]
##
#-------------------------------------------------------------------------------
###################
## MAP-- APR 22.
###################
world <- ne_countries(scale = "medium", returnclass = "sf")
#-------------------------------------------------------------------------------
mp <-  NULL
#fig <- image_graph(width = 2950, height = 1400, res = 400)
fig <- image_graph(width = 5900, height = 2800, res = 200)
ggplot(data = world) +
  geom_sf(colour="gray70", fill="gray70") +
  coord_sf(xlim = c(-135, 160), ylim = c(-60, 80), expand = FALSE) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank() #, rect = element_blank()
  ) +
  annotate(geom="rect", xmin=-135, xmax=160, ymin=-60, ymax=-52, color="gray50", fill="gray90", alpha=0.75) +
  annotate(geom="rect", xmin=-135, xmax=160, ymin=52, ymax=80, color="gray50", fill="gray90", alpha=0.75)
  #annotate(geom="rect", xmin=-135, xmax=160, ymin=-52, ymax=52, color="yellow", size=5, fill="gray90", alpha=0.25)
  #geom_rect(aes(xmin=-135, xmax=160, ymin=52, ymax=80), color="gray90", fill=alpha("gray90", alpha=0.5))
dev.off()
#-------------------------------------------------------------------------------
empty.df <- matrix(0,nrow=length(biomes2),ncol=4)
row.names(empty.df) <- biomID2
empty.df <- cbind(empty.df,as.numeric(factor(biomID2)))
colnames(empty.df) <- c("total_PA_AGB","total_PA_AGB_err","extra_PA_AGB", "extra_PA_AGB_err", "biomID2")
empty.df
#-------------------------------------------------------------------------------
continents <- as.character(df[,"continent"])
continents[which(continents=="Au")] <- "Oc"
continents[which(continents=="US")] <- "NAm"
df[,"continent"] <- factor(continents)
#
regions <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
reg.ids <- c("Af", "As", "Eu", "NAm", "Oc", "SA")
##
df.list <- vector(mode="list", length=6)
##
for(r in 1:length(reg.ids)){
  df.out <- empty.df
  df.in <- df[df[,"continent"]==reg.ids[r],]
  df.in
  
  df2 <- cbind(df.in$contiBiomPAAGBtotal, df.in$contiBiomPAAGBtotal_err, df.in$contiBiomPAAGBextra, df.in$contiBiomPAAGBextra_err, df.in$biomID)
  colnames(df2) <- c("total_PA_AGB","total_PA_AGB_err","extra_PA_AGB","extra_PA_AGB_err","biomID2")
  print(df2)
  
  for(i in 1:nrow(df2)){
    biom <- df2[i,"biomID2"]
    df.out[df.out[,"biomID2"]==biom,] <- df2[i,]
  }
  
  #df2 <- df.out[df.out[,"total_PA_Area"]!=0,]
  #df2 <- df.out[df.out[,"extra_AGB_in_PA"]>0,]
  df2 <- df.out
  
  colors <- c()
  for(i in 1:nrow(df2)){colors <- c(colors, which(biomID2==row.names(df2)[i]))}
  
  AGB_total <- sum(df2[,"total_PA_AGB"])
  AGB_extra <- sum(df2[,"extra_PA_AGB"])
  
  percent_AGB_total <- round(100*df2[,"total_PA_AGB"]/AGB_total, digits=1)
  percent_AGB_extra <- round(100*df2[,"extra_PA_AGB"]/AGB_extra, digits=1)
  
  DF <- cbind(df2, percent_AGB_total, percent_AGB_extra, colors)
  DF2 <- as.data.frame(DF[rev(order(DF[,"percent_AGB_total"])),])
  DF2$rows <- factor(row.names(DF2), levels=row.names(DF2))
  colors2 <- mypalette2[DF2[,"colors"]]
  DF3 <- DF[rev(order(DF[,"extra_PA_AGB"])),]
  DF3 <- DF3[DF3[,"total_PA_AGB"]!=0, ]
  
  df.list[[r]] <- DF3
}
df.list
#-------------------------------------------------------------------------------
################################################################################
#-------------------------------------------------------------------------------
barfigs.list <- vector(mode="list", length=6)

##Africa------------------------------------------------------------------------
#for(r in 1:5){
width_bp <- 1000; height_bp <- 650; res_bp <- 300
r=1
barfigs.list[[r]] <- image_graph(width = width_bp, height = height_bp, res = res_bp)
DF3 <- df.list[[r]]
par(oma=c(0,0,0,0), mar=c(3,4.5,2,0), bg=alpha("white", 0.95))
x <- barplot(DF3[,"extra_PA_AGB"], xaxt="n",# names.arg=row.names(DF3), las=2,
             xlim=c(0,12), ylim=c(0,0.9),
             #main=paste(regions[r]),
             col=mypalette2[DF3[,"colors"]],
             cex.axis=1.25, cex.names=1.25)
labs <- paste(row.names(DF3))
text(cex=0.25, x=x-.25, y=-0.2, labs, xpd=TRUE, srt=45)
title(ylab="GtC", line=2.5, cex.lab=0.3)
text(5, 0.825, paste(regions[r]), cex=0.35)
x <- barplot(DF3[,"extra_PA_AGB"], plot=F)
y <- DF3[,"extra_PA_AGB"]
se <- DF3[,"extra_PA_AGB_err"]
arrows(x, y+se, x, y-se, angle=90, code=3, length=0.05)
dev.off()

##Asia------------------------------------------------------------------------
#for(r in 1:5){
width_bp <- 1000; height_bp <- 1.9*650; res_bp <- 300
r=2
barfigs.list[[r]] <- image_graph(width = width_bp, height = height_bp, res = res_bp)
DF3 <- df.list[[r]]
par(oma=c(0,0,0,0), mar=c(3,4.5,2,0), bg=alpha("white", 0.95))
x <- barplot(DF3[,"extra_PA_AGB"], xaxt="n",# names.arg=row.names(DF3), las=2,
             xlim=c(0,12), ylim=c(0,1.05*sum(DF3[1,"extra_PA_AGB"],DF3[1,"extra_PA_AGB_err"])),
             #main=paste(regions[r]),
             col=mypalette2[DF3[,"colors"]],
             cex.axis=1.25, cex.names=1.25)
labs <- paste(row.names(DF3))
text(cex=0.25, x=x-.25, y=-0.25, labs, xpd=TRUE, srt=45)
title(ylab="GtC", line=2.5, cex.lab=0.3)
text(5, 1.015*sum(DF3[1,"extra_PA_AGB"],DF3[1,"extra_PA_AGB_err"]), paste(regions[r]), cex=0.34)
x <- barplot(DF3[,"extra_PA_AGB"], plot=F)
y <- DF3[,"extra_PA_AGB"]
se <- DF3[,"extra_PA_AGB_err"]
arrows(x, y+se, x, y-se, angle=90, code=3, length=0.05)
dev.off()

##Europe,Oceania,NAmerica-------------------------------------------------------
width_bp <- 1000; height_bp <- 650; res_bp <- 300
for(r in 3:5){
  barfigs.list[[r]] <- image_graph(width = width_bp, height = height_bp, res = res_bp)
  DF3 <- df.list[[r]]
  par(oma=c(0,0,0,0), mar=c(3,4.5,2,0), bg=alpha("white", 0.95))
  x <- barplot(DF3[,"extra_PA_AGB"], xaxt="n",# names.arg=row.names(DF3), las=2,
               xlim=c(0,12), ylim=c(0,0.9),
               #main=paste(regions[r]),
               col=mypalette2[DF3[,"colors"]],
               cex.axis=1.25, cex.names=1.25)
  labs <- paste(names(DF3[,"extra_PA_AGB"]))
  text(cex=1, x=x-.25, y=-0.2, labs, xpd=TRUE, srt=45)
  title(ylab="GtC", line=2.5, cex.lab=1.25)
  text(5, 0.825, paste(regions[r]), cex=1.35)
  x <- barplot(DF3[,"extra_PA_AGB"], plot=F)
  y <- DF3[,"extra_PA_AGB"]
  se <- DF3[,"extra_PA_AGB_err"]
  arrows(x, y+se, x, y-se, angle=90, code=3, length=0.05)
  dev.off()
}

##--------------------------------------------- South America on different scale
width_bp <- 1000; height_bp <- 3.6*650; res_bp <- 300
r=6
barfigs.list[[r]] <- image_graph(width = width_bp, height = height_bp, res = res_bp)
DF3 <- df.list[[r]]
#bp
par(oma=c(0,0,0,0), mar=c(3,4,2,0), bg=alpha("white", 0.95))
x <- barplot(DF3[,"extra_PA_AGB"], xaxt="n",# names.arg=row.names(DF3), las=2,
             xlim=c(0,12), ylim=c(0, 1.05*sum(DF3[1,"extra_PA_AGB"],DF3[1,"extra_PA_AGB_err"])),
             #main=paste(regions[r]),
             col=mypalette2[DF3[,"colors"]],
             cex.axis=1.25, cex.names=1.25)
labs <- paste(names(DF3[,"extra_PA_AGB"]))
text(cex=0.25, x=x-.25, y=-0.25, labs, xpd=TRUE, srt=45)
title(ylab="GtC", line=2.5, cex.lab=0.3)
text(5, 1.03*sum(DF3[1,"extra_PA_AGB"],DF3[1,"extra_PA_AGB_err"]), paste(regions[r]), cex=0.34)
x <- barplot(DF3[,"extra_PA_AGB"], plot=F)
y <- DF3[,"extra_PA_AGB"]
se <- DF3[,"extra_PA_AGB_err"]
arrows(x, y+se, x, y-se, angle=90, code=3, length=0.05)

dev.off()

#-----------------------final image composite-----------------------------------
final <- image_composite(fig,   barfigs.list[[1]], offset = "+2600+1100") ##Af
final <- image_composite(final, barfigs.list[[2]], offset = "+4100+250") ##As
final <- image_composite(final, barfigs.list[[3]], offset = "+2800+100")  ##Eu
final <- image_composite(final, barfigs.list[[4]], offset = "+250+350")  ##NA
final <- image_composite(final, barfigs.list[[5]], offset = "+4650+1850") ##Oc
final <- image_composite(final, barfigs.list[[6]], offset = "+1400+150")  ##SA
final






#fig.path <- "/Users/veronika/leitoldv/figures/GEDI_PA_fig8/"
#fig.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/figures/"

fig.path <- "/Users/veronika/leitoldv/figures/"
image_write(final, path=paste(fig.path,"newFIG3_extraAGB_barplots_2022MAR17_err_updateAPR22_updateFEB2023.png",sep=""), format="png")

biomes2 <- c("BorF: Boreal Forest / Taiga",
             "ConF: Temperate & Tropical Conifer Forests",  ##TeCF + TrCF
             "DesX: Deserts & Xeric Shrublands",
             "GrSS: Temperate & Tropical Grasslands, Savannas & Shrub.",  ##TeG + TrG + MtG + FlG
             "Mang: Mangroves",
             "MedF: Mediterranean Forests, Woodlands & Scrub",
             "TeBF: Temperate Broadleaf & Mixed Forests",
             "TrDF: Tropical & Subtropical Dry Broadleaf Forests",
             "TrMF: Tropical & Subtropical Moist Broadleaf Forests",
             "Tund: Tundra")
biomID2 <- c("BorF","ConF","DesX","GrSS","Mang","MedF","TeBF","TrDF","TrMF","Tund")
mypalette2 <- colors()[c(639,104,411,76,435,114,139,83,47,400)]

par(mfrow=c(1,1), mar=c(1,1,1,1))
plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend=biomes2, pch=15, pt.cex=2, cex=1, bty='n',
       col=mypalette2)
mtext("WWF biomes", side=3, line=-0.5, at=0.085, cex=1.25, font=2)
#text(0.075, 1.05, "WWF biomes", cex=1.25, font=2)



