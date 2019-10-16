

# write Direction out

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
#install.packages("lubridate")
library(caTools)
library(RColorBrewer)
library(lubridate)

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))

fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "complete")
flds <- list.files(fld_lst, full.names = TRUE, pattern="20")

res <- unique(na.omit(as.numeric(unlist(strsplit(flds, "[^0-9]+")))))[2]
fld_o <- paste0(gsub(filebase_raster, "", flds))
mth_lst <- c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")
real_mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
hum_flux_total <- list()
for (i in seq(1,length(mth_lst))){
  print(mth_lst[i])
  print("Changing Month...")
  i <- match(real_mth[i], mth_lst)
  print(mth_lst[i])
  act_fld <- flds[i]
  hum_flux <- stack(paste0(act_fld, "/hum_flux/hum_flux_total_",mth_lst[i],".nc"), varname="length")
  hum_direc <- stack(paste0(act_fld, "/hum_flux/hum_flux_total_",mth_lst[i],".nc"), varname="direct")
  ATM_fls <- list.files(paste0(act_fld,"/output"), pattern="ATM")
  prc1 <- read_modeloutput(paste0(act_fld,"/output/",ATM_fls[2]), variable = "pr")
  #prc2  <- read_modeloutput(paste0(act_fld,"/Kiliman_20km_Apr_May2014_ATM.2014041500.nc"), variable = "pr")
  prc <- prc1
  hum_flux_res <- prc
  hum_direc_res <- prc
  topo <- read_modeloutput(paste0(act_fld,"/output/",ATM_fls[2]))
  
  values(hum_flux_res) <- values(hum_flux)
  
  
  
  hum_flux_res[hum_flux_res <25000] <- 0
  
  values(hum_direc_res) <- values(hum_direc)
  #writeRaster(hum_direc_res, file="/home/dogbert/Desktop/testApril.tif" )
  #hum_flux_total[[i]] <- hum_flux_res
  writeRaster(hum_flux_res, file=paste0(filebase_raster,"/hum_flux_tif/hum_flux_",mth_lst[i],"_length.tif"  ))
}  
