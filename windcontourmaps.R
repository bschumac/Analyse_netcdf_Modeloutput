# Create Plots for real Data

if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
if (!require(ncdf4)){install.packages("ncdf4", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
install.packages("devtools")
library(devtools)
install_github('tim-salabim/metvurst')
library("metvurst")
install.packages("latticeExtra")
library(latticeExtra)
#install.packages("grid")
install.packages("reshape")
library(dplyr)
install.packages("dplyr")

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/prec/")


filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")

prc <- read.csv("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/plots.csv")
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
  i <- 14
  prc_act <- prc[prc$plotID == plot_names[i],]
  #prc_act <- prc_act[!prc_act$datetime == prc_act$datetime[1],]
  strip(x = prc_act$P_RT_NRT, date= prc_act$datetime, fun = "mean",
        main= paste0("Prc ", plot_names[i], " Apr/May 2014"))
}

wd_ws <-  read.csv("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/csv/WD_WS_SWDR/plots.csv")
wd_ws$datetime <- as.POSIXct(wd_ws$datetime, format="%Y-%m-%dT%H:%M", tz="")
wd_ws$year <- as.numeric(substr(wd_ws$datetime, 1,4))
wd_ws$month <- as.numeric(substr(wd_ws$datetime, 6,7))
wd_ws$day <- as.numeric(substr(wd_ws$datetime, 9,10))
wd_ws$hour <- as.numeric(substr(wd_ws$datetime, 12,13))
head(wd_ws)
#plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
wd_ws <- wd_ws[wd_ws$year > 2013,]
wd_ws <- wd_ws[wd_ws$year < 2015,]
wd_ws <- wd_ws[wd_ws$month < 6,]
wd_ws <- wd_ws[wd_ws$month > 3,]
wd_ws <- na.omit(wd_ws)



#strip(prc$P_RT_NRT, prc$datetime)
plot_names <- as.character(unique(wd_ws$plotID))

windContours(hour = wd_ws$datetime, wd = wd_ws$WD, 
                     ws = wd_ws$WV, keytitle= "Windcontours 2014 04/05 of all Plots")

for (i in 1:length(plot_names)){
  i <- 1
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

#Create Maps for Modeldata
#### This script will calculate the RMSE of Precipitation of the regCM Output compared 
#### to the real measured data.


fld_lst <- list.files(filebase_raster, full.names = TRUE)

lst_fcl <- lapply(fld_lst, function(i){
  #i <- fld_lst[1]
  print(i)
  
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                       "", i))
  temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014041500.nc")
  netcdf_prc <- stack(temp, varname = "prc")
  netcdf_topo <- stack(temp, varname = "topo")
  temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014050100.nc")
  netcdf_prc1 <- stack(temp, varname = "prc")
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
  
  # assign projection
  crs <- crs("+proj=stere +lat_0=-3.06 +lon_0=37.36 +x_0=-15000. +y_0=-15000. +ellps=sphere +a=6371229. +b=6371229. +units=m +no_defs ")
  crs(netcdf_topo) <- crs
  crs(netcdf_WD) <- crs
  crs(netcdf_WS) <- crs
  plots_shp <- spTransform(plots_shp, crs(netcdf_WD))
  
  # Where is Kili in my Raster? 
  row <- rowFromY(netcdf_topo, -3.076475)
  col <- colFromX(netcdf_topo, 37.353205)
  
  # create Rastermask for the Kili Region. 
  raster_mask <- netcdf_topo[[1]]
  #write out the original Raster for control
  #writeRaster(raster_mask, "where_is_kili", format="GTiff", overwrite=TRUE)
  vals <- 0
  raster_mask <- setValues(raster_mask, vals)
  
  # get position of Cell in the middle (Kili Pixel)
  celln <- cellFromRowCol(raster_mask, row-1, col-1)
  ext1 <- data.frame(xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row+1, col-1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row-1, col+1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row+1, col+1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  
  # moving edgepoints to middle cell to get extent
  ext1[1,1] <- ext1[1,1]-3*30000-30000/2
  ext1[2,1] <- ext1[2,1]-3*30000-30000/2
  ext1[3,1] <- ext1[3,1]+3*30000+30000/2
  ext1[4,1] <- ext1[4,1]+3*30000+30000/2
  ext1[1,2] <- ext1[1,2]+3*30000+30000/2
  ext1[2,2] <- ext1[2,2]-3*30000-30000/2
  ext1[3,2] <- ext1[3,2]+3*30000+30000/2
  ext1[4,2] <- ext1[4,2]-3*30000-30000/2
  
  #write out the edgepoints for control reason
  #edgepoints2 <- SpatialPointsDataFrame(ext1, ext1)
  #ext1 <- as.matrix(ext1)
  ext1 <- extent(ext1)
  
  netcdf_topo_kili <- crop(netcdf_topo, ext1)
  #already cut
  netcdf_WD_kili <- netcdf_WD
  netcdf_WS_kili <- netcdf_WS
  
  plot(netcdf_topo_kili[[1]])
  plot(ext1, add=T)
  plot(plots_shp, add=T)
  
  # data management plots_csv
  plot_names <- as.character(unique(wd_ws$plotID))
  
  
  for (i in seq(1, length(plot_names))){
    j <- plot_names[i]
    print(j)
    act_plot_shp <- plots_shp[plots_shp$PlotID == j,]
    #extract(netcdf_topo_kili, act_plot_shp)
    #data.frame(act_plot_shp)
    
    
    row <- rowFromY(netcdf_topo_kili, act_plot_shp)[1]
    col <- colFromX(netcdf_topo_kili, act_plot_shp)[1]
    #plot(netcdf_topo_kili)
    celln <- cellFromRowCol(netcdf_topo_kili, row-1, col-1)
    ext2 <- data.frame(xyFromCell(netcdf_topo_kili, celln))
    celln <- cellFromRowCol(netcdf_topo_kili, row+1, col-1)
    ext2 <- rbind(ext2,xyFromCell(netcdf_topo_kili, celln))
    celln <- cellFromRowCol(netcdf_topo_kili, row-1, col+1)
    ext2 <- rbind(ext2,xyFromCell(netcdf_topo_kili, celln))
    celln <- cellFromRowCol(netcdf_topo_kili, row+1, col+1)
    ext2 <- rbind(ext2,xyFromCell(netcdf_topo_kili, celln))
    ext2[1,1] <- ext2[1,1]-30000/2
    ext2[2,1] <- ext2[2,1]-30000/2
    ext2[3,1] <- ext2[3,1]+30000/2
    ext2[4,1] <- ext2[4,1]+30000/2
    ext2[1,2] <- ext2[1,2]+30000/2
    ext2[2,2] <- ext2[2,2]-30000/2
    ext2[3,2] <- ext2[3,2]+30000/2
    ext2[4,2] <- ext2[4,2]-30000/2
    ext2 <- extent(ext2)
    act_netcdf_topo_kili <- crop(netcdf_topo_kili, ext2)
    act_netcdf_WD_kili <- crop(netcdf_WD_kili, ext2)
    act_netcdf_WS_kili <- crop(netcdf_WS_kili, ext2)
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
    png(filename=paste0(filebase_results,"windcontours_model/", fld_o,"_",
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
  i <- fld_lst[5]
  print(i)
  
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                       "", i))
  temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014041500.nc")
  
  netcdf_prc <- stack(temp, varname = "prc")
  netcdf_topo <- stack(temp, varname = "topo")
  #netcdf_tas <- stack(temp, varname = "tas")
  temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014050100.nc")
  
  netcdf_prc1 <- stack(temp, varname = "prc")
  #netcdf_tas1 <- stack(temp, varname = "tas")
  
  netcdf_prc <- stack(netcdf_prc, netcdf_prc1)
  #names(netcdf_prc)
  #netcdf_tas <- stack(netcdf_tas, netcdf_tas1)
  lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
  plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                       layer=  lyr)
  
  
  
  # assign projection
  crs <- crs("+proj=stere +lat_0=-3.06 +lon_0=37.36 +x_0=-15000. +y_0=-15000. +ellps=sphere +a=6371229. +b=6371229. +units=m +no_defs ")
  crs(netcdf_prc) <- crs
  plots_shp <- spTransform(plots_shp, crs(netcdf_prc))
  
  # Where is Kili in my Raster? 
  row <- rowFromY(netcdf_prc, -3.076475)
  col <- colFromX(netcdf_prc, 37.353205)
  
  # create Rastermask for the Kili Region. 
  raster_mask <- netcdf_prc[[1]]
  #write out the original Raster for control
  #writeRaster(raster_mask, "where_is_kili", format="GTiff", overwrite=TRUE)
  vals <- 0
  raster_mask <- setValues(raster_mask, vals)
  
  # get position of Cell in the middle (Kili Pixel)
  celln <- cellFromRowCol(raster_mask, row-1, col-1)
  ext1 <- data.frame(xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row+1, col-1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row-1, col+1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row+1, col+1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  
  # moving edgepoints to middle cell to get extent
  ext1[1,1] <- ext1[1,1]-3*30000-30000/2
  ext1[2,1] <- ext1[2,1]-3*30000-30000/2
  ext1[3,1] <- ext1[3,1]+3*30000+30000/2
  ext1[4,1] <- ext1[4,1]+3*30000+30000/2
  ext1[1,2] <- ext1[1,2]+3*30000+30000/2
  ext1[2,2] <- ext1[2,2]-3*30000-30000/2
  ext1[3,2] <- ext1[3,2]+3*30000+30000/2
  ext1[4,2] <- ext1[4,2]-3*30000-30000/2
  
  ext1 <- extent(ext1)
  #ext1 <- as(ext1, 'SpatialPolygons')
  
  netcdf_prc_kili <- crop(netcdf_prc, ext1)
  netcdf_topo_kili <- crop(netcdf_topo, ext1)
  #netcdf_tas_kili <- crop(netcdf_tas, ext1)
  
  plot(netcdf_topo_kili[[1]])
  plot(ext1, add=T)
  plot(plots_shp, add=T)
  

  plots_names <- as.character(unique(prc$plotID))
  out.df <- data.frame()
  for (i in seq(1, length(plots_names))){
    i <- 13
    j <- plots_names[i]
    print(j)
    act_plot_shp <- plots_shp[plots_shp@data$PlotID == j,]
    
    
    #extract(netcdf_topo_kili, act_plot_shp)
    #data.frame(act_plot_shp)
    
    
    row <- rowFromY(netcdf_topo_kili, act_plot_shp)[1]
    col <- colFromX(netcdf_topo_kili, act_plot_shp)[1]
    #plot(netcdf_topo_kili)
    celln <- cellFromRowCol(netcdf_topo_kili, row-1, col-1)
    ext2 <- data.frame(xyFromCell(netcdf_topo_kili, celln))
    celln <- cellFromRowCol(netcdf_topo_kili, row+1, col-1)
    ext2 <- rbind(ext2,xyFromCell(netcdf_topo_kili, celln))
    celln <- cellFromRowCol(netcdf_topo_kili, row-1, col+1)
    ext2 <- rbind(ext2,xyFromCell(netcdf_topo_kili, celln))
    celln <- cellFromRowCol(netcdf_topo_kili, row+1, col+1)
    ext2 <- rbind(ext2,xyFromCell(netcdf_topo_kili, celln))
    ext2[1,1] <- ext2[1,1]-30000/2
    ext2[2,1] <- ext2[2,1]-30000/2
    ext2[3,1] <- ext2[3,1]+30000/2
    ext2[4,1] <- ext2[4,1]+30000/2
    ext2[1,2] <- ext2[1,2]+30000/2
    ext2[2,2] <- ext2[2,2]-30000/2
    ext2[3,2] <- ext2[3,2]+30000/2
    ext2[4,2] <- ext2[4,2]-30000/2
    ext2 <- extent(ext2)
    act_netcdf_topo_kili <- crop(netcdf_topo_kili, ext2)
    act_netcdf_prc_kili <- crop(netcdf_prc_kili, ext2)
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
    act_netcdf_prc_kili_vals <- data.frame(t(extract(act_netcdf_prc_kili, min_dif_ele, df=TRUE)))
    act_netcdf_prc_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_prc_kili_vals), 
                                                 format = "X%Y.%m.%d.%H.%M.%S")
    #head(act_netcdf_prc_kili_vals)
    colnames(act_netcdf_prc_kili_vals) <- c("prc", "time")
    
    act_netcdf_prc_kili_vals <- act_netcdf_prc_kili_vals[-1,]
    rownames(act_netcdf_prc_kili_vals) <- seq(1,length(act_netcdf_prc_kili_vals$prc))
    act_netcdf_prc_kili_vals$prc <- act_netcdf_prc_kili_vals$prc*3600

    png(filename=paste0(filebase_results,"prc_pattern_plots_model/", fld_o,"_",
                        j, "_prc.png"), width = 1280, height = 960)
 
    strip(x = act_netcdf_prc_kili_vals$prc, date= act_netcdf_prc_kili_vals$time, fun = "mean",
          main= paste0("Prc ", j, " Apr/May 2014"))

    dev.off()

  }
  
  
  return(act_netcdf_prc_kili_vals)  
})   
