# create Humidity Budget

if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
if (!require(ncdf4)){install.packages("ncdf4", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
if(!require(caret)){install.packages('caret')}
library(caret)
if(!require(mapview)){install.packages('mapview')}
library(mapview)
#install.packages( "hydroGOF")
library(hydroGOF)
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
#install.packages("caTools")
library(caTools)
library(RColorBrewer)
library(rasterVis)
library(gridExtra)
library(rgdal)


filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path,"raster/")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))

filebase_ERA5path <- "/media/benjamin/XChange/Masterarbeit/Copernicus_Download/"



# read vector data
cntry <- readOGR(dsn = paste0(filebase_shp,"/world_boarders/TM_WORLD_BORDERS-0.3.shp"), layer = "TM_WORLD_BORDERS-0.3", stringsAsFactors = TRUE)

nguru <- readOGR(dsn = paste0(filebase_shp,"/nguru/EasternArc_byBloc_incPlateaus&LowMatundu_DD.shp"), 
                 layer = "EasternArc_byBloc_incPlateaus&LowMatundu_DD", stringsAsFactors = TRUE)
kenya <- readOGR(dsn = paste0(filebase_shp,"/NationalParks/Kenya/WDPA_Mar2017_search_1d0087e1d160cce68ea3e5e320ce6506de5d4bda642e58dbe8d5466674679bc2-shapefile-polygons.shp"), 
                 layer = "WDPA_Mar2017_search_1d0087e1d160cce68ea3e5e320ce6506de5d4bda642e58dbe8d5466674679bc2-shapefile-polygons", stringsAsFactors = TRUE)
grvalley <- readOGR(dsn = paste0(filebase_shp,"/riftvalley/great_rift_valley_ofb.shp"), 
                    layer = "great_rift_valley_ofb", stringsAsFactors = TRUE)
rivers <- readOGR(dsn = paste0(filebase_shp,"/rivers/ne_10m_rivers_lake_centerlines.shp"), 
                  layer = "ne_10m_rivers_lake_centerlines", stringsAsFactors = TRUE)

#rivers <- rivers[rivers$name %in% c("Pangani", "Wami", "Tana"),]

kili <- readOGR(dsn=paste0(filebase_shp,"plots_shp/plots_unique.shp"), layer= "plots_unique")                 
kili <- spTransform(kili, CRSobj = crs(cntry))
kili <- kili[1,]


JF <- c("Jan", "Feb")
MAM <- c("Mar", "Apr", "May")
JJAS <- c("Jun", "Jul", "Aug", "Sep")
OND <- c("Oct", "Nov", "Dec")
seasons <- list(JF,MAM,JJAS,OND)
seasons_num <- list(c("01","02"),c("03","04","05"),c("06","07","08","09"),c("10","11","12"))

par(mfrow=c(1,1))


############################################################### Plot for Season JF

act_seas <- seasons_num[[1]]
years <- c("2013","2014",2015)


calc_mean_flux <- function(act_seas,years,div=150000){
  hum_x_stack <- stack()
  hum_y_stack <- stack()
  for (j in seq(1,length(act_seas))){
    print(j)
    for (r in seq(1,length(years))){
      model_base <- brick(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/",years[r],"/",act_seas[j],"/output_",years[r],"_pressure_mon_",act_seas[j],".nc"), varname="sp")
      hum_x <- model_base
      hum_y <- model_base
      
      # the hum flux has to be turned because it is saved with 90deg turn                  
      values(hum_x) <- values(stack(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/",years[r],"/",act_seas[j],"/hum_flux/hum_flux_x_direct.nc")))
      values(hum_y) <- values(stack(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/",years[r],"/",act_seas[j],"/hum_flux/hum_flux_y_direct.nc")))
      hum_x_stack <- stack(hum_x_stack,hum_x)
      hum_y_stack <- stack(hum_y_stack,hum_y)
    }
  }                         

  hum_seas <- stack(mean(hum_x_stack), mean(hum_y_stack))
  values(hum_seas) <- values(hum_seas)/div
  return(hum_seas)
}




topoERA5 <- stack("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/output_2013_oro_01.nc")
topoERA5 <- topoERA5[[1]]


my.settings <- rasterTheme(region = brewer.pal("OrRd", n = 9),   
                           par.main.text = list(font = 2, just = "left",x = grid::unit(5, "mm")))

hum_jf <- calc_mean_flux(seasons_num[[1]],years = years,div=150000)


hum_mam <- calc_mean_flux(seasons_num[[2]],years = years,div=150000)

hum_jjas <- calc_mean_flux(seasons_num[[3]],years = years,div=150000)

hum_ond <- calc_mean_flux(seasons_num[[4]],years = years,div=150000)



############################################################### Plot for Season MAM

############################################################## Plots

png(filename="/media/benjamin/XChange/Masterarbeit/Copernicus_Download/jf_plot_2013-2015.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

jfplot <- (vectorplot(hum_jf, isField = "dXY", region = topoERA5/10, margin = FALSE, par.settings = my.settings, 
                      narrows = 1000, at = seq(0,5000, 100), main="a)")  +   layer(sp.polygons(cntry, cex=1)) + layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))
jfplot
dev.off()

png(filename="/media/benjamin/XChange/Masterarbeit/Copernicus_Download/mam_plot_2013-2015.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
mamplot <- (vectorplot(hum_mam, isField = "dXY", region = topoERA5/10, margin = FALSE, par.settings = my.settings, 
                       narrows = 1000, at = seq(0,5000, 100), main="b)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))
mamplot
dev.off()

png(filename="/media/benjamin/XChange/Masterarbeit/Copernicus_Download/jjas_plot_2013-2015.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

jjasplot <- vectorplot(hum_jjas, isField = "dXY", region = topoERA5/10, margin = FALSE, par.settings = my.settings, 
                       narrows = 1000, at = seq(0,5000, 100), main="c)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black"))
jjasplot
dev.off()

png(filename="/media/benjamin/XChange/Masterarbeit/Copernicus_Download/ond_plot_2013-2015.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

ondplot <- (vectorplot(hum_ond, isField = "dXY", region = topoERA5/10, margin = FALSE, par.settings = my.settings, 
                       narrows = 1000, at = seq(0,5000, 100),  main="d)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))
ondplot
dev.off()





###########################################################################################