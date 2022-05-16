################################################################################
# "/gpfs/data1/duncansongp/GEDI_global_PA/csv/continential_result.txt"
################################################################################
################################################################################
#f.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/csv/"
f.path <- "/Users/veronika/leitoldv/figures/csv/"

df <- read.csv(paste(f.path, "continential_result_v2.csv", sep=""))

df2 <- cbind(df$percent_diff_AGBD,df$percent_diff_rh98,df$percent_diff_cover,df$percent_diff_pai)
colnames(df2) <- c("AGBD","Height","Cover","PAI")
row.names(df2) <- as.vector(df[,"continent"])
row.names(df2)[row.names(df2)=="North America"] <- "N.America"
row.names(df2)[row.names(df2)=="South America"] <- "S.America"
df2

regions <- row.names(df2)
regions
##[1] "Africa"    "Asia"      "Oceania"   "Europe"    "S.America" "N.America"
regions <- c("Africa","Asia","Oceania","Europe","South America","North America")
###################
## MAP
###################
library(raster)
library(ggmap)
library(maps)
library(ggplot2)
library(magick)
library(rnaturalearth)
library(rnaturalearthdata)
library(wesanderson)
world <- ne_countries(scale = "medium", returnclass = "sf")

mp <-  NULL
#fig <- image_graph(width = 2950, height = 1400, res = 200)
fig <- image_graph(width = 5900, height = 2800, res = 200)
ggplot(data = world) +
  geom_sf(colour="gray70", fill="gray70") +
  coord_sf(xlim = c(-135, 160), ylim = c(-60, 80), expand = FALSE)+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank() #, rect = element_blank()
  )
dev.off()

width_bp <- 1000; height_bp <- 750; res_bp <- 300
ymin <- 0; ymax <- ceiling(1.2*max(df2))
palette <- wes_palette("Cavalcanti1", 5, type = "continuous")[1:4]

barfig_list <- vector(mode="list", length=length(regions))

for(r in 1:length(regions)){
  #r=1
  regID <- row.names(df2)[r]
  mp <- data.frame(continent=rep(regID,4), metric=colnames(df2), mean_diffs=df2[regID,])
  mp$color <- palette
  
  barfig_1 <- image_graph(width = width_bp, height = height_bp, res = res_bp)
  par(oma=c(0,0,0,0), mar=c(3,4.5,1,0.5), bg=alpha("white", 0.85))
  barplot(mp[,"mean_diffs"], names.arg=mp[,"metric"], las=1,
          ylim=c(ymin,ymax), col=mp[,"color"],
          cex.axis=1.25, cex.names=0.9,
          ylab="% difference", cex.lab=1.5)
  text(2.25, ymax*.925, paste(regions[r]), cex=1.5)
  dev.off()
  barfig_list[[r]] <- barfig_1
}

final <- image_composite(fig,   barfig_list[[1]], offset = "+2600+1100")    ##Af
final <- image_composite(final, barfig_list[[2]], offset = "+4100+350") ##As
final <- image_composite(final, barfig_list[[4]], offset = "+2800+100")  ##Eu
final <- image_composite(final, barfig_list[[6]], offset = "+450+350")  ##US
final <- image_composite(final, barfig_list[[3]], offset = "+4600+1700") ##Au
final <- image_composite(final, barfig_list[[5]], offset = "+1200+1500")  ##SA
final

#fig.path <- "/Users/veronika/leitoldv/figures/GEDI_PA_fig8/"
#fig.path <- "/gpfs/data1/duncansongp/GEDI_global_PA/figures/"

fig.path <- "/Users/veronika/leitoldv/figures/"
image_write(final, path=paste(fig.path,"newFIG4_differences_barplots_2022MAR17.png",sep=""), format="png")
#-----------------------final image composite-----------------------------------

