

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

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))

fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "20")
res <- unique(na.omit(as.numeric(unlist(strsplit(fld_lst, "[^0-9]+")))))[1]
fld_o <- paste0(gsub(filebase_raster, "", fld_lst))


##############################################################################################################
# Windspeed/Winddirection calculation

temp1 <- paste0(fld_lst,"/Kiliman_",res,"km_Apr_May2014_ATM.2014041500.nc")
temp2 <- paste0(fld_lst,"/Kiliman_",res,"km_Apr_May2014_ATM.2014050100.nc")

for(i in seq(1,22)){
uas1 <- read_modeloutput(temp1, variable = "ua", levels = TRUE, lvl = i)
uas2 <- read_modeloutput(temp2, variable = "ua", levels = TRUE, lvl = i)
prc1 <- read_modeloutput(temp1, variable = "pr", levels = TRUE, lvl = i)
vas1 <- read_modeloutput(temp1, variable = "va", levels = TRUE, lvl = i)
vas2 <- read_modeloutput(temp2, variable = "va", levels = TRUE, lvl = i)
prc2 <- read_modeloutput(temp2, variable = "pr", levels = TRUE, lvl = i)

uas <- stack(uas1,uas2)
vas <- stack(vas1,vas2)
prc <- stack(prc1,prc2)
uas_kili <- crp_raster(uas, window_size = 15)
vas_kili <- crp_raster(vas, window_size = 15)
prc_kili <- crp_raster(prc, window_size = 15)
act_ws_wd_lst <- create_WS_WD(netcdf_vas_kili = vas_kili, netcdf_uas_kili = uas_kili)
names(act_ws_wd_lst$WD) <- names(uas)
names(act_ws_wd_lst$WS) <- names(uas)
writeRaster(act_ws_wd_lst$WD, filename = paste0(fld_lst, "/", "WD_lvl", i,".tif"))
writeRaster(act_ws_wd_lst$WS, filename = paste0(fld_lst, "/", "WS_lvl", i,".tif"))
writeRaster(prc_kili, filename = paste0(fld_lst, "/", "PR_lvl", i,".tif"))
}

#################################################################################################################
# Create Humidity Flux

temp1 <- paste0(fld_lst,"/Kiliman_",res,"km_Apr_May2014_ATM.2014041500.nc")
temp2 <- paste0(fld_lst,"/Kiliman_",res,"km_Apr_May2014_ATM.2014050100.nc")

# x direction (west-east)
uas1 <- read_modeloutput(temp1, variable = "ua", levels = TRUE, lvl = i)
uas2 <- read_modeloutput(temp2, variable = "ua", levels = TRUE, lvl = i)
uas <- stack(uas1,uas2)


x = c(1:22)
y= x*x

f <- y~x
plot(f)
trapz(c(1:22), (c(1:22)*c(1:22)))




