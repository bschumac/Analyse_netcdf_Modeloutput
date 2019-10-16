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



filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))

#system('ncks -x -v xlon /media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/20km_gul_6_6/Kiliman_20km_Apr_May2014_DOMAIN001.nc /media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/20km_gul_6_6/Kiliman_20km_Apr_May2014_DOMAIN_compare.nc')

topo_vals <- raster("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results/DEM/SRTM_20km_incr.nc")


fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "20")
fls <- list.files(fld_lst, full.names = TRUE, pattern = "DOMAIN")

ncin <- nc_open(fls, write=TRUE)

ncin$var$topo$varsize
topo_vals_c <- c()
for (i in seq(210,1))
topo_vals_c <- c(topo_vals_c,c(getValues(topo_vals, i, 1)))

topo_vals <- rev(topo_vals)
head(topo_vals)
str(topo_vals)

ncvar_put(ncin, "topo", topo_vals_c, verbose = TRUE)

nc_close(ncin)
