# Analyze/Plot PCA of HumFlux

if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
if (!require(ncdf4)){install.packages("ncdf4", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
if(!require(caret)){install.packages('caret')}
library(caret)
#if(!require(mapview)){install.packages('mapview')}
library(mapview)
#install.packages( "hydroGOF")
library(hydroGOF)
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
install.packages("emdbook")
library(caTools)
library(RColorBrewer)
library(lubridate)
library(emdbook)
display.brewer.all()

filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))
/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/vector/Turkana Channel
/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/vector/
# read vector data
cntry <- readOGR(dsn = paste0(filebase_shp,"/world_boarders/TM_WORLD_BORDERS-0.3.shp"), layer = "TM_WORLD_BORDERS-0.3", stringsAsFactors = TRUE)
turkana <- readOGR(dsn = paste0(filebase_shp,"/Turkana_Channel/turkana_channel.shp"), layer = "turkana_channel", stringsAsFactors = TRUE)


fls_lst <- list.files(paste0(filebase_raster,"/hum_flux_tif/pca/"), full.names = TRUE)
real_mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
mth_lst <- c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")
hum_pca <- stack()
for (i in seq(1,length(real_mth))){
  print(mth_lst[i])
  print("Changing Month...")
  i <- match(real_mth[i], mth_lst)
  
  act_hum_pca <- raster(fls_lst[i])
  hum_pca <- stack(hum_pca,act_hum_pca)
}

#hum_pca[hum_pca < 0] <- 0

floor(c(8.53334,8.596234),2)
yat = round(seq(extent(hum_pca[[2]])@ymin, 
          extent(hum_pca[[2]])@ymax, length.out = 3)/.05)*.05


xat = round(seq(extent(hum_pca[[2]])@xmin, 
          extent(hum_pca[[2]])@xmax, length.out = 3)/.05)*.05
breaks <- c(0,lseq(0.00001,0.0002, 5))

names(hum_pca) <-real_mth
png(filename="/home/benjamin/Desktop/Fig_8.png", 
    units="cm", 
    width=20, 
    height=15, 
    res=300)

spplot(hum_pca, col.regions=colorRampPalette((brewer.pal(9, "YlGnBu")))(256),
      #at = breaks,      
      #colorkey=list(labels = as.character(breaks)),
       panel = function(...){
         panel.levelplot(...)
         sp.polygons(cntry, cex=0.1, lty=3, col="gray60")
         sp.polygons(turkana, cex=0.4, lty=2, col="red")
       }, 
       scales = list(x = list(at = xat, labels = paste0(as.character(xat)," E")),
                     y = list(at = yat, labels = c(paste0(as.character(abs(yat[1:2]))," S"),paste0(as.character(yat[3])," N"))))
      )


dev.off()

