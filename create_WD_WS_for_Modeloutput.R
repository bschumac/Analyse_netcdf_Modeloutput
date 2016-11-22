# create Windspeed and Winddirection from Modeloutput 


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
#fld_lst <- fld_lst[2:5]
lst_fcl <- lapply(fld_lst, function(i){
  i <- fld_lst[1]
  
  res <- unique(na.omit(as.numeric(unlist(strsplit(i, "[^0-9]+")))))[1]
  
  fld_o <- paste0(gsub(filebase_raster, "", i))
  
  
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014041500.nc")
  
  
  netcdf_uas <- read_modeloutput(temp, variable = "uas", lvl=1, levels = FALSE)
  
  netcdf_topo <- read_modeloutput(temp, variable = "topo", lvl=1, levels = FALSE)
  netcdf_vas <- read_modeloutput(temp, variable = "vas", lvl=1, levels = FALSE)
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014050100.nc")
  netcdf_uas1 <- read_modeloutput(temp, variable = "uas", lvl=1, levels = FALSE)
  netcdf_vas1 <- read_modeloutput(temp, variable = "vas", lvl=1, levels = FALSE)
  
  netcdf_uas <- stack(netcdf_uas, netcdf_uas1)
  netcdf_vas <- stack(netcdf_vas, netcdf_vas1)
  
  lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
  plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                       layer=  lyr)
  plots_shp <- spTransform(plots_shp, crs(netcdf_vas))
  
  # 3x3 matrix around kili
  #netcdf_vas_kili <- crp_raster(netcdf_vas, window_size = 3)
  #netcdf_topo_kili <- crp_raster(netcdf_topo, window_size = 3)
  #netcdf_uas_kili <- crp_raster(netcdf_uas, window_size = 3)
  
  netcdf_vas_kili <- netcdf_vas
  netcdf_topo_kili <- netcdf_topo
  netcdf_uas_kili <- netcdf_uas
  test_lst <- create_WS_WD(netcdf_vas_kili = netcdf_vas_kili, netcdf_uas_kili = netcdf_uas_kili)
  
  writeRaster(netcdf_WD_kili, filename = paste0(filebase_raster, fld_o, "/WD_kiliman_Apr_May2014.nc"), overwrite=TRUE)
  writeRaster(netcdf_WS_kili, filename = paste0(filebase_raster, fld_o, "/WS_kiliman_Apr_May2014.nc"), overwrite=TRUE)
  
})


