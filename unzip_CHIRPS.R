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
library(R.utils)
filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_model <- paste0(filebase_path, "raster")
filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS_2014_daily")


fld_lst <- list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = "2014")

lst_fcl <- lapply(fld_lst, function(i){
  print(i)
  i <- fld_lst[1]
  fl_lst <- list.files(i, full.names = TRUE)  
  for (i in seq(1,length(fl_lst))){
    fl_name <- gsub(".tif.gz","",gsub("chirps-v2.0.","",gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/CHIRPS_2014_daily/2014/", "",fl_lst[i])))
    fl_name <- paste0(gsub("[.]", "_", fl_name), ".tif")
    gunzip(fl_lst[i], destname = paste0("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/CHIRPS_2014_daily/2014/",fl_name))
  }
})

