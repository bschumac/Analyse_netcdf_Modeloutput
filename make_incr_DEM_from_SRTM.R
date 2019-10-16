#### This script will calculate the RMSE of Precipitation of the TRMM Data compared 
#### to the real measured data.

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
library(hydroGOF)
#install.packages("hydroGOF")
library(chron)
library(stringr)
#installed.packages("rgl")
#install.packages("RSAGA")
library(RSAGA)
library(RColorBrewer)
library(rgl)
library(maptools)

#####################################################
filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
#####################################################

  
filebase_dem <- paste0(filebase_path, "DEM/")
filebase_dem_uz <- paste0(filebase_dem, "SRTM/")
filebase_model <-  paste0(filebase_path, "raster")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
filebase_results <- paste0(filebase_path, "results/DEM")

source(paste0(filebase_code,"analyse_fun.R"))


fls_SRTM <- list.files(paste0(filebase_dem,"SRTM_ORG"), full.names = TRUE, pattern="")

for (i in (1:length(fls_SRTM))){
  unzip(fls_SRTM[i], exdir = filebase_dem_uz, overwrite=TRUE)
}
# copy tif files by hand to SRTM/tif


fls_SRTM_tif <- list.files(paste0(filebase_dem_uz,"tif"), full.names = TRUE, pattern="")
for (i in seq(1,length(fls_SRTM_tif))){
  SRTM_dem_act <- raster(fls_SRTM_tif[i])
  vals_dem <- values(SRTM_dem_act)
  vals_dem <- replace(vals_dem, vals_dem<= 0, NA)
  values(SRTM_dem_act) <- vals_dem
  SRTM_agg_act <- aggregate(SRTM_dem_act, fact=218, fun=mean, filename=paste0(paste0(filebase_dem_uz,"tif"), 
                                                                              "/Tile_agg", i,".tif"),
                            overwrite=TRUE)

}

# merge raster tiles with qgis by hand


SRTM_full_agg <- raster(paste0(filebase_dem_uz,"SRTM_Domain_kili_agg.tif"))
#replace vals with NA
vals_dem <- values(SRTM_full_agg)
vals_dem <- replace(vals_dem, vals_dem<= 0, NA)
values(SRTM_full_agg) <- vals_dem


netcdf_topo <- read_modeloutput(paste0(filebase_model,"/20km_gul_6_6/Kiliman_20km_Apr_May2014_DOMAIN000.nc" ), variable = "topo")

SRTM_full_agg_res <- resample(SRTM_full_agg, netcdf_topo, method="ngb")
writeRaster(SRTM_full_agg_res, filename= paste0(filebase_results,"/SRTM_20km.tif"), overwrite=TRUE)

# increase vals of SRTM
vals_dem <- values(SRTM_full_agg_res)
vals_dem[ vals_dem<=500 & !is.na(vals_dem)] <- vals_dem[ vals_dem<=500 & !is.na(vals_dem)]+((vals_dem[ vals_dem<=500 & !is.na(vals_dem)]/100)*15)
vals_dem[ vals_dem>500 & vals_dem<=1500 &!is.na(vals_dem)] <- vals_dem[ vals_dem>500 & vals_dem<=1500 &!is.na(vals_dem)]+((vals_dem[ vals_dem>500 & vals_dem<=1500 &!is.na(vals_dem)]/100)*20)
vals_dem[ vals_dem>1500 & vals_dem<=2500 &!is.na(vals_dem)] <- vals_dem[ vals_dem>1500 & vals_dem<=2500 &!is.na(vals_dem)]+((vals_dem[ vals_dem>1500 & vals_dem<=2500 &!is.na(vals_dem)]/100)*25)
vals_dem[ vals_dem>2500 &!is.na(vals_dem)] <- vals_dem[ vals_dem>2500 &!is.na(vals_dem)]+((vals_dem[ vals_dem>2500 &!is.na(vals_dem)]/100)*25)

#Replace NA with 0
values(SRTM_full_agg_res) <- vals_dem
vals_dem <- values(SRTM_full_agg_res)
vals_dem <- replace(vals_dem, is.na(vals_dem) , 0)
values(SRTM_full_agg_res) <- vals_dem


writeRaster(SRTM_full_agg_res, filename=paste0(filebase_results,"/SRTM_20km_incr.nc"), overwrite= TRUE)
writeRaster(SRTM_full_agg_res, filename=paste0(filebase_results,"/SRTM_20km_incr.tif"), overwrite= TRUE)

# COMPARISION

fld_lst_model <- list.files(filebase_model, full.names = TRUE, pattern="20")
temp <- paste0(fld_lst_model,"/Kiliman_20km_Apr_May2014_SRF.2014041500.nc")
netcdf_topo <- read_modeloutput(filepath = temp, variable = "topo")



plot(SRTM_full_agg_res)
plot(netcdf_topo)
dif <- SRTM_full_agg_res-netcdf_topo
plot(dif)
density(dif)
