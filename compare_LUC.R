#### This script will calculate the RMSE of rH and ta_200 of the regCM Output compared 
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
#install.packages( "hydroGOF")
library(hydroGOF)

source("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/code/Analyse_netcdf_Modeloutput/crp_fun.R")

#####################################################



filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
landuse_model <- stack(paste0(filebase_path,"Kiliman_30km_Apr_May2014_DOMAIN000.nc"), varname = "landuse")
landuse_real <- raster("/media/dogbert/XChange/Masterarbeit/LUC_Kili/results/filled_scenes/classification_204_filled_agg2.tif")
plot(landuse_model)
lyr <- ogrListLayers(paste0(filebase_shp,"plots_unique.shp"))
plots_shp <- readOGR(paste0(filebase_shp,"plots_unique.shp"),
                     layer=  lyr)


#assign projection

crs <- crs("+proj=stere +lat_0=-3.06 +lon_0=37.36 +x_0=-15000. +y_0=-15000. +ellps=sphere +a=6371229. +b=6371229. +units=m +no_defs ")
crs(landuse_model) <- crs
plots_shp <- spTransform(plots_shp, crs(landuse_model))

lur_agg <- NULL
lur_agg5km <- aggregate(landuse_real, fact=500, fun=modal)
lur_agg30km <- aggregate(lur_agg5km, fact=6, fun=modal)



lum <- crp_raster(landuse_model, window_size = 1)

lur_res5km <- resample(landuse_real, lur_agg5km, method = "ngb")
lur_res30km <- resample(landuse_real, lur_agg30km, method = "ngb")


mapview(lum, legend = TRUE ) + mapview(lur_agg30km, legend=TRUE)
values(lum)
values(lur_agg30km)
mapview(lum, legend = TRUE ) + mapview(lur_res5km, legend=TRUE)

plot(lur_agg)
plot(plots_shp, add=T)

values(lur_res5km)




