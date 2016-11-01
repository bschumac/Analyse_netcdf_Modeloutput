#### This script will calculate the RMSE of Precipitation of the regCM Output compared 
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

#####################################################

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/WD_WS_SWDR/")


filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")

fld_lst <- list.files(filebase_raster, full.names = TRUE)
fls_lst_o <- list.files(filebase_raster, full.names = TRUE)

lst_fcl <- lapply(fld_lst, function(i){
  #i <- fld_lst[1]
  print(i)
  
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                       "", i),"/")
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
  
  plots_csv <- read.csv(paste0(filebase_csv,"plots.csv"))
  
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
  
  plots_csv$year <- as.numeric(substr(plots_csv$datetime, 1,4))
  plots_csv$month <- as.numeric(substr(plots_csv$datetime, 6,7))
  plots_csv$day <- as.numeric(substr(plots_csv$datetime, 9,10))
  plots_csv$hour <- as.numeric(substr(plots_csv$datetime, 12,13))
  #plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
  
  plots_csv <- plots_csv[plots_csv$year > 2013,]
  plots_csv <- plots_csv[plots_csv$year < 2015,]
  plots_csv <- plots_csv[plots_csv$month > 3,]
  plots_csv <- plots_csv[plots_csv$month < 6,]
  
  plots_csv <- plots_csv[plots_csv$day >= 15 & plots_csv$month == 4 | plots_csv$month == 5,]
  plots_csv <- plots_csv[plots_csv$day <= 15 & plots_csv$month == 5 | plots_csv$month == 4 ,]
  plots_csv$SWDR_300 <- NULL
  
  head(plots_csv,10)
  plots_csv <- na.omit(plots_csv)
  plots_names <- as.character(unique(plots_csv$plotID))
  out.df <- data.frame()
  all_rmse_vals <- NULL
  
  
  for (i in seq(1, length(plots_names))){
    #i <- 1
    j <- plots_names[i]
    print(j)
    act_plot_shp <- plots_shp[plots_shp@data$PlotID == j,]
    act_plot_csv <- plots_csv[plots_csv$plotID == j,]
    
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
    head(act_WD_WS)
    #colnames(act_tas_hur) <- c("time", "Ta_200_model", "rH_model")
    #act_tas_hur <- act_tas_hur[-1,]
    #act_tas_hur$time <- as.POSIXct(act_tas_hur$time, format="X%Y.%m.%d.%H.%M.%S", tz="")
    #head(act_plot_csv)
    act_plot_csv <- data.frame(time = act_plot_csv$datetime, 
                               WS_plot = act_plot_csv$WV,
                               WD_plot = act_plot_csv$WD)
    
    #act_plot_csv$Ta_200_plot <- act_plot_csv$Ta_200_plot+273.15
    act_plot_csv$time <- as.POSIXct(act_plot_csv$time, format="%Y-%m-%dT%H:%M", tz="")
    #head(act_plot_csv)
    #match time of netcdf to observations
    
    
    act_merge <- merge(act_plot_csv, act_WD_WS, by="time", all = FALSE)
    head(act_merge)
    library(hydroGOF)
    
    
    rmse_WD <- rmse(act_merge$WD_plot, act_merge$WD, na.rm=TRUE)
    rmse_WS <- rmse(act_merge$WS_plot, act_merge$WS, na.rm=TRUE)
    
    ME_WD <- mean( act_merge$WD - act_merge$WD_plot, na.rm = TRUE)
    ME_WS <- mean( act_merge$WS - act_merge$WS_plot, na.rm = TRUE)
    
    #MAE <- mean(abs(pred_vals - obs_vals), na.rm = TRUE)
    MAE_WD <- mean(abs( act_merge$WD - act_merge$WD_plot), na.rm = TRUE)
    MAE_WS <- mean(abs( act_merge$WS - act_merge$WS_plot), na.rm = TRUE)
    
    png(filename=paste0(filebase_results, fld_o,
                        j, "_WD.png"), width = 1280, height = 960)
    plot(act_merge$time, act_merge$WD_plot, lwd = 2,
         type="l", ylim=c(0,3), main = paste0("Time series of WD at ", j))
    lines( act_merge$time, act_merge$WD, col="red", lty=2, lwd = 2)
    legend("bottomright", c("WD Plot", "WD Model"), col=c("black", "red"), lty=c(1,2))
    dev.off()
    
    png(filename=paste0(filebase_results, fld_o,
                        j, "_WS.png"), width = 1280, height = 960)
    plot(act_merge$time, act_merge$WS_plot, lwd = 2,
         type="l", ylim=c(0,3), main = paste0("Time series of WS at ", j))
    lines( act_merge$time, act_merge$WS, col="red", lty=2, lwd = 2)
    legend("bottomright", c("WS Plot", "WS Model"), col=c("black", "red"), lty=c(1,2))
    dev.off()
    
    act.out.df <- data.frame(plotID = j,
                             rmse_WD,
                             ME_WD,
                             MAE_WD,
                             rmse_WS,
                             ME_WS,
                             MAE_WS
                             )
    out.df <- rbind(act.out.df,out.df)
  }
  write.csv(out.df, file = paste0(filebase_results, fld_o,
                                  "error_stats_WD_WS.csv"))
  return(out.df)  
})    