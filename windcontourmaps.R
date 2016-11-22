# Create Plots for real Data

if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
if (!require(ncdf4)){install.packages("ncdf4", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
#install.packages("devtools")
library(devtools)
#install_github('tim-salabim/metvurst')
library("metvurst")
#install.packages("latticeExtra")
library(latticeExtra)
#install.packages("grid")
#install.packages("abind")
library(fields)
library(dplyr)
#install.packages("dplyr")


#####################################################################################

### Watch folderlist <- names have to be fitted to create maps


filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"

###################################################################################

filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))

prc <- read.csv(paste0(filebase_csv,"prec/plots.csv"))
prc$datetime <- as.POSIXct(prc$datetime, format="%Y-%m-%dT%H:%M", tz="")
prc <-na.omit(prc)
prc$year <- as.numeric(substr(prc$datetime, 1,4))
prc$month <- as.numeric(substr(prc$datetime, 6,7))
prc$day <- as.numeric(substr(prc$datetime, 9,10))
prc$hour <- as.numeric(substr(prc$datetime, 12,13))
prc <- prc[prc$year > 2013,]
prc <- prc[prc$year < 2015,]
prc <- prc[prc$month < 6,]
prc <- prc[prc$month > 3,]

strip(x = prc$P_RT_NRT, date= prc$datetime, fun = "mean",
      main= "Prc all Stations Apr/May 2014")
plot_names <- as.character(unique(prc$plotID))

for (i in seq(1,length(plot_names))){
  prc_act <- prc[prc$plotID == plot_names[i],]
  prc_act <- prc_act[!prc_act$datetime == prc_act$datetime[1],]
  if (mean(prc_act$P_RT_NRT)> 0.001){
    plot<- strip(x = prc_act$P_RT_NRT, date= prc_act$datetime, fun = "mean",
          main= paste0("Prc ", plot_names[i], " Apr/May 2014"))
    print(plot)
  }
}

wd_ws <-  read.csv(paste0(filebase_csv,"WD_WS_SWDR/plots.csv"))
wd_ws$datetime <- as.POSIXct(wd_ws$datetime, format="%Y-%m-%dT%H:%M", tz="")
wd_ws$year <- as.numeric(substr(wd_ws$datetime, 1,4))
wd_ws$month <- as.numeric(substr(wd_ws$datetime, 6,7))
wd_ws$day <- as.numeric(substr(wd_ws$datetime, 9,10))
wd_ws$hour <- as.numeric(substr(wd_ws$datetime, 12,13))

#plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
wd_ws <- wd_ws[wd_ws$year > 2013,]
wd_ws <- wd_ws[wd_ws$year < 2015,]
wd_ws <- wd_ws[wd_ws$month < 6,]
wd_ws <- wd_ws[wd_ws$month > 3,]
plot_names_unique <- as.character(unique(wd_ws$plotID))
wd_ws <- na.omit(wd_ws)

#strip(prc$P_RT_NRT, prc$datetime)
plot_names <- as.character(unique(wd_ws$plotID))

for (i in 1:length(plot_names)){
    wd_ws_act <- wd_ws[wd_ws$plotID == plot_names[i],]
    if (length(unique(wd_ws_act$datetime))%%2 == 0){
    windContours(hour = wd_ws_act$datetime, wd = wd_ws_act$WD, 
             ws = wd_ws_act$WV, keytitle= paste0("Windcontours 2014 04/05 of Plot ", plot_names[i]))
    }else {
      wd_ws_act <- wd_ws_act[!wd_ws_act$datetime == wd_ws_act$datetime[1],]
      windContours(hour = wd_ws_act$datetime, wd = wd_ws_act$WD, 
                   ws = wd_ws_act$WV, keytitle= paste0("Windcontours 2014 04/05 of Plot ", plot_names[i]))
      }
}

########################################################################################################################

#Create Windcontourmaps for Modeldata

fld_lst <- list.files(filebase_raster, full.names = TRUE)

lst_fcl <- lapply(fld_lst, function(i){
  i <- fld_lst[2]
  
  res <- unique(na.omit(as.numeric(unlist(strsplit(i, "[^0-9]+")))))[1]
  
  fld_o <- paste0(gsub(filebase_raster, "", i))
  dir.create(paste0(filebase_results, "windcontours_model", fld_o), showWarnings = FALSE)
  
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014041500.nc")
  
  
  netcdf_prc <- read_modeloutput(temp, variable = "prc")
  netcdf_topo <- read_modeloutput(temp, variable = "topo")
  
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014050100.nc")
  netcdf_prc1 <- read_modeloutput(temp, variable = "prc")
  netcdf_prc <- stack(netcdf_prc, netcdf_prc1)
  
  temp <- paste0(i,"/WD_kiliman_Apr_May2014.nc")
  netcdf_WD <- stack(temp)
  temp <- paste0(i,"/WS_kiliman_Apr_May2014.nc")
  netcdf_WS <- stack(temp)
  
  names(netcdf_WD) <- names(netcdf_prc)
  names(netcdf_WS) <- names(netcdf_prc)
  netcdf_prc <- NULL
  netcdf_prc1 <- NULL
  
  lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
  plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                       layer=  lyr)
  
  
  plots_shp <- spTransform(plots_shp, crs(netcdf_WD))

  
  netcdf_topo_kili <- crp_raster(netcdf_topo, window_size = 0)
  #already cut
  netcdf_WD_kili <- netcdf_WD
  netcdf_WS_kili <- netcdf_WS
  
  plot(netcdf_topo_kili[[1]])

  plot(plots_shp, add=T)
  
  for (i in seq(1, length(plot_names_unique))){
    j <- plot_names[i]
    print(j)
    act_plot_shp <- plots_shp[plots_shp$PlotID == j,]
    #extract(netcdf_topo_kili, act_plot_shp)


    act_netcdf_topo_kili <- crp_raster(netcdf_topo_kili, window_size = 0)
    act_netcdf_WD_kili <- crp_raster(netcdf_WD_kili, window_size = 0)
    act_netcdf_WS_kili <- crp_raster(netcdf_WS_kili, window_size = 0)
    #names(act_netcdf_prc_kili)
    #add windspeed, WD here
    #act_netcdf_tas_kili <- crop(netcdf_tas_kili, ext2)
    #act_netcdf_hurs_kili <- crop(netcdf_hurs_kili, ext2)
    
    values(act_netcdf_topo_kili) <- abs(values(act_netcdf_topo_kili)-mean(act_plot_shp@data$Z_GPS))
    
    
    vals<-extract(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
    coords <-xyFromCell(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
    combine <- data.frame(cbind(coords,vals))
    min_dif_ele <- combine[combine$Surface.Model.Elevation == min(combine$Surface.Model.Elevation),]
    min_dif_ele <- data.frame(min_dif_ele$x, min_dif_ele$y)
    min_dif_ele <- SpatialPointsDataFrame(min_dif_ele, min_dif_ele) 
    
    #act_netcdf_tas_kili_vals <- data.frame(t(extract(act_netcdf_tas_kili, min_dif_ele, df=TRUE)))
    #act_netcdf_tas_kili_vals$time <-  rownames(act_netcdf_hurs_kili_vals)
    #names(act_netcdf_prc_kili)
    act_netcdf_WD_kili_vals <- data.frame(t(extract(act_netcdf_WD_kili, min_dif_ele, df=TRUE)))
    act_netcdf_WD_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_WD_kili_vals), 
                                                format = "X%Y.%m.%d.%H.%M.%S")
    act_netcdf_WS_kili_vals <- data.frame(t(extract(act_netcdf_WS_kili, min_dif_ele, df=TRUE)))
    act_netcdf_WS_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_WS_kili_vals), 
                                                format = "X%Y.%m.%d.%H.%M.%S")
    
    colnames(act_netcdf_WS_kili_vals) <- c("WS", "time")
    colnames(act_netcdf_WD_kili_vals) <- c("WD", "time")
    act_netcdf_WS_kili_vals <- act_netcdf_WS_kili_vals[-1,]
    act_netcdf_WD_kili_vals <- act_netcdf_WD_kili_vals[-1,]
    
    rownames(act_netcdf_WD_kili_vals) <- seq(1,length(act_netcdf_WD_kili_vals$WD))
    rownames(act_netcdf_WS_kili_vals) <- seq(1,length(act_netcdf_WS_kili_vals$WS))
    
    act_WD_WS <- merge(act_netcdf_WS_kili_vals,act_netcdf_WD_kili_vals, by="time")
    png(filename=paste0(filebase_results,"windcontours_model", fld_o,"/",
                        j, "_contours.png"), width = 1280, height = 960)
    windContours(hour = act_WD_WS$time, wd = act_WD_WS$WD, 
                 ws = act_WD_WS$WS, 
                 keytitle= paste0("Windcontours Model 2014 04/05 of plot ", j))

    dev.off()
    }

  return(act_WD_WS)  
}) 


########################################################################################################################
### Create Figure for prc-Output
fld_lst <- list.files(filebase_raster, full.names = TRUE)
lst_fcl <- lapply(fld_lst, function(i){
  #i <- fld_lst[2]
  
  res <- unique(na.omit(as.numeric(unlist(strsplit(i, "[^0-9]+")))))[1]
  
  fld_o <- paste0(gsub(filebase_raster, "", i))
  dir.create(paste0(filebase_results, "prc_pattern_plots_model", fld_o), showWarnings = FALSE)
  
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014041500.nc")
  netcdf_prc <- read_modeloutput(temp, variable = "prc")
  netcdf_topo <- read_modeloutput(temp, variable = "topo")
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014050100.nc")
  netcdf_prc1 <- read_modeloutput(temp, variable = "prc")
  netcdf_prc <- stack(netcdf_prc, netcdf_prc1)
  #names(netcdf_prc)
  #netcdf_tas <- stack(netcdf_tas, netcdf_tas1)
  lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
  plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                       layer=  lyr)

  plots_shp <- spTransform(plots_shp, crs(netcdf_prc))
  
  
  
  netcdf_prc_kili <- crp_raster(netcdf_prc, window_size = 3)
  netcdf_topo_kili <- crp_raster(netcdf_topo, window_size = 3)

  
  plot(netcdf_topo_kili[[1]])
  plot(plots_shp, add=T)
  

  plots_names <- as.character(unique(prc$plotID))
  out.df <- data.frame()
  for (i in seq(1, length(plots_names))){
    j <- plots_names[i]
    print(j)
    act_plot_shp <- plots_shp[plots_shp@data$PlotID == j,]


    act_netcdf_topo_kili <- crp_raster(netcdf_topo_kili, window_size = 0)
    act_netcdf_prc_kili <- crp_raster(netcdf_prc_kili, window_size = 0)

    
    values(act_netcdf_topo_kili) <- abs(values(act_netcdf_topo_kili)-mean(act_plot_shp@data$Z_GPS))
    
    
    vals<-extract(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
    coords <-xyFromCell(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
    combine <- data.frame(cbind(coords,vals))
    min_dif_ele <- combine[combine$Surface.Model.Elevation == min(combine$Surface.Model.Elevation),]
    min_dif_ele <- data.frame(min_dif_ele$x, min_dif_ele$y)
    min_dif_ele <- SpatialPointsDataFrame(min_dif_ele, min_dif_ele) 
    
    #act_netcdf_tas_kili_vals <- data.frame(t(extract(act_netcdf_tas_kili, min_dif_ele, df=TRUE)))
    #act_netcdf_tas_kili_vals$time <-  rownames(act_netcdf_hurs_kili_vals)
    #names(act_netcdf_prc_kili)
    act_netcdf_prc_kili_vals <- data.frame(t(extract(act_netcdf_prc_kili, min_dif_ele, df=TRUE)))
    act_netcdf_prc_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_prc_kili_vals), 
                                                 format = "X%Y.%m.%d.%H.%M.%S")
    #head(act_netcdf_prc_kili_vals)
    colnames(act_netcdf_prc_kili_vals) <- c("prc", "time")
    
    act_netcdf_prc_kili_vals <- act_netcdf_prc_kili_vals[-1,]
    rownames(act_netcdf_prc_kili_vals) <- seq(1,length(act_netcdf_prc_kili_vals$prc))
    act_netcdf_prc_kili_vals$prc <- act_netcdf_prc_kili_vals$prc*3600
    if(mean(act_netcdf_prc_kili_vals$prc)> 0.001){
        png(filename=paste0(filebase_results,"prc_pattern_plots_model", fld_o,"/",
                            j, "_prc.png"), width = 1280, height = 960)
          
        plot <- strip(x = act_netcdf_prc_kili_vals$prc, date= act_netcdf_prc_kili_vals$time, fun = "mean",
                  main= paste0("Prc ", j, " Apr/May 2014"))
        print(plot)
        dev.off()
    }
  }
  
  
  return(act_netcdf_prc_kili_vals)  
})   
