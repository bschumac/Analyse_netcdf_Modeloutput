# NOT NEEDED FOR FULL YEAR ANALYSIS



#create Humidity Flux add Files together 


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

fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "20")


for(i in seq(1,length(fld_lst))){
  i <-2
  fld <- fld_lst[i]
  
  temp1 <- paste0(fld,"/hum_flux_full_xdirec_Apr.nc")
  temp2 <- paste0(fld,"/hum_flux_full_xdirec_May.nc")
  temp3 <- paste0(fld,"/hum_flux_full_ydirec_Apr.nc")
  temp4 <- paste0(fld,"/hum_flux_full_ydirec_May.nc")
  temp5 <- paste0(fld,"/Kiliman_20km_Apr_May2014_ATM.2014041500.nc")
  temp6 <- paste0(fld,"/Kiliman_20km_Apr_May2014_ATM.2014050100.nc")
  
  
  prc1 <- read_modeloutput(temp5, variable = "pr")
  prc2 <- read_modeloutput(temp6, variable = "pr")
  
  humflux_x1 <- stack(temp1)
  names(humflux_x1) <- names(prc1) 
  humflux_x2 <- stack(temp2)
  names(humflux_x2) <- names(prc2) 
  humflux_y1 <- stack(temp3)
  names(humflux_y1) <- names(prc1) 
  humflux_y2 <- stack(temp4)
  names(humflux_y2) <- names(prc2) 
  humflux_x <-  stack(humflux_x1, humflux_x2) 
  humflux_y <-  stack(humflux_y1, humflux_y2) 

  writeRaster(humflux_x, filename = paste0(fld,"/hum_flux_x_AprMay.nc"), overwrite=TRUE)
  writeRaster(humflux_y, filename = paste0(fld,"/hum_flux_y_AprMay.nc"), overwrite=TRUE)

}