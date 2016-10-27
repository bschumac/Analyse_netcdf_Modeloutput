#### This script will calculate the RMSE of rH and ta_200 of the regCM Output compared 
#### to the real measured data.

if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
if (!require(ncdf)){install.packages("ncdf", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
if(!require(caret)){install.packages('caret')}
library(caret)
if(!require(mapview)){install.packages('mapview')}
library(mapview)

#####################################################

filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")

fld_lst <- list.files(filebase_raster, full.names = TRUE)
fls_lst_o <- list.files(filebase_raster, full.names = TRUE)

lst_fcl <- lapply(fld_lst, function(i){
    #i <- fld_lst[2]
    print(i)
    fld_o <- paste0(gsub("/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                  "", i),"/")
    temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014041500.nc")
    netcdf_hurs <- stack(temp, varname = "hurs")
    
    netcdf_topo <- stack(temp, varname = "topo")
    netcdf_tas <- stack(temp, varname = "tas")
    temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014050100.nc")
    netcdf_hurs1 <- stack(temp, varname = "hurs")
    netcdf_tas1 <- stack(temp, varname = "tas")
    
    netcdf_hurs <- stack(netcdf_hurs, netcdf_hurs1)
    netcdf_tas <- stack(netcdf_tas, netcdf_tas1)
    
        lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
        plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                layer=  lyr)
        
        plots_csv <- read.csv(paste0(filebase_csv,"plots.csv"))
        
        plots_shp <- plots_shp[plots_shp@data$PlotID != "fer5",]
        plots_shp <- plots_shp[plots_shp@data$PlotID != "flm5",]
        plots_shp <- plots_shp[plots_shp@data$PlotID != "sun1",]
        plots_shp <- plots_shp[plots_shp@data$PlotID != "sun2",]
        plots_shp <- plots_shp[plots_shp@data$PlotID != "sun3",]
        plots_shp <- plots_shp[plots_shp@data$PlotID != "sun4",]
        plots_shp <- plots_shp[plots_shp@data$PlotID != "sun5",]
       
        plots_csv <- plots_csv[plots_csv$plotID != "fer5",]
        plots_csv <- plots_csv[plots_csv$plotID != "flm5",]
        plots_csv <- plots_csv[plots_csv$plotID != "sun1",]
        plots_csv <- plots_csv[plots_csv$plotID != "sun2",]
        plots_csv <- plots_csv[plots_csv$plotID != "sun3",]
        plots_csv <- plots_csv[plots_csv$plotID != "sun4",]
        plots_csv <- plots_csv[plots_csv$plotID != "sun5",]
 
        
        # assign projection
        
        crs <- crs("+proj=stere +lat_0=-3.06 +lon_0=37.36 +x_0=-15000. +y_0=-15000. +ellps=sphere +a=6371229. +b=6371229. +units=m +no_defs ")
        crs(netcdf_topo) <- crs
        crs(netcdf_tas) <- crs
        crs(netcdf_hurs) <- crs
        plots_shp <- spTransform(plots_shp, crs(netcdf_hurs))
        
        # Where is Kili in my Raster? 
        row <- rowFromY(netcdf_hurs, -3.076475)
        col <- colFromX(netcdf_hurs, 37.353205)
        
        
        # create Rastermask for the Kili Region. 
        raster_mask <- netcdf_hurs[[1]]
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
        #ext1 <- as(ext1, 'SpatialPolygons')
        
        netcdf_hurs_kili <- crop(netcdf_hurs, ext1)
        netcdf_topo_kili <- crop(netcdf_topo, ext1)
        netcdf_tas_kili <- crop(netcdf_tas, ext1)
        plot(netcdf_topo_kili[[1]])
        plot(ext1, add=T)
        plot(plots_shp, add=T)

        # data management plots_csv
        
        plots_csv$year <- as.numeric(substr(plots_csv$datetime, 1,4))
        plots_csv$month <- as.numeric(substr(plots_csv$datetime, 6,7))
        plots_csv$day <- as.numeric(substr(plots_csv$datetime, 9,10))
        plots_csv$hour <- as.numeric(substr(plots_csv$datetime, 12,13))
        #plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]

        plots_csv <- plots_csv[plots_csv$month > 3,]
        plots_csv <- plots_csv[plots_csv$month < 6,]
        
        plots_csv <- plots_csv[plots_csv$day >= 15 & plots_csv$month == 4 | plots_csv$month == 5,]
        plots_csv <- plots_csv[plots_csv$day <= 15 & plots_csv$month == 5 | plots_csv$month == 4 ,]
        plots_names <- as.character(unique(plots_csv$plotID))
        out.df <- data.frame()
        
        all_rmse_vals<- lapply(plots_names, function(j){
          #j <- plots_names[1]
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
          act_netcdf_tas_kili <- crop(netcdf_tas_kili, ext2)
          act_netcdf_hurs_kili <- crop(netcdf_hurs_kili, ext2)
          
          values(act_netcdf_topo_kili) <- abs(values(act_netcdf_topo_kili)-mean(act_plot_shp@data$Z_GPS))
          
          
          vals<-extract(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
          coords <-xyFromCell(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
          combine <- data.frame(cbind(coords,vals))
          min_dif_ele <- combine[combine$Surface.Model.Elevation == min(combine$Surface.Model.Elevation),]
          min_dif_ele <- data.frame(min_dif_ele$x, min_dif_ele$y)
          min_dif_ele <- SpatialPointsDataFrame(min_dif_ele, min_dif_ele) 
          act_netcdf_tas_kili_vals <- data.frame(t(extract(act_netcdf_tas_kili, min_dif_ele, df=TRUE)))
          act_netcdf_tas_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_tas_kili_vals), 
                                                       format = "X%Y.%m.%d.%H.%M.%S")
          
          act_netcdf_hurs_kili_vals <- data.frame(t(extract(act_netcdf_hurs_kili, min_dif_ele, df=TRUE)))
          act_netcdf_hurs_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_hurs_kili_vals), 
                                                        format = "X%Y.%m.%d.%H.%M.%S")
          
          act_tas_hur <- merge(act_netcdf_tas_kili_vals,act_netcdf_hurs_kili_vals, by="time")
          colnames(act_tas_hur) <- c("time", "Ta_200_model", "rH_model")
          act_tas_hur <- act_tas_hur[-1,]
          act_tas_hur$time <- as.POSIXct(act_tas_hur$time, format="X%Y.%m.%d.%H.%M.%S", tz="")
          
          act_plot_csv <- data.frame(time = act_plot_csv$datetime, 
                                     Ta_200_plot = act_plot_csv$Ta_200,
                                     rH_plot = act_plot_csv$rH_200)
          act_plot_csv$Ta_200_plot <- act_plot_csv$Ta_200_plot+273.15
          act_plot_csv$time <- as.POSIXct(act_plot_csv$time, format="%Y-%m-%dT%H:%M", tz="")
          head(act_plot_csv)
          #match time of netcdf to observations
          
          act_merge <- merge(act_plot_csv, act_tas_hur, by="time", all = FALSE)
          library(hydroGOF)
          
          #ME <- mean(pred_vals - obs_vals, na.rm = TRUE)
          ME_rH <- mean(act_merge$rH_model - act_merge$rH_plot, na.rm = TRUE)
          ME_tas <- mean( act_merge$Ta_200_model - act_merge$Ta_200_plot, na.rm = TRUE)
          #MAE <- mean(abs(pred_vals - obs_vals), na.rm = TRUE)
          MAE_rH <- mean(abs( act_merge$rH_model - act_merge$rH_plot), na.rm = TRUE)
          MAE_tas <- mean(abs( act_merge$Ta_200_model - act_merge$Ta_200_plot), na.rm = TRUE)
          
          rmse_rH <- rmse(act_merge$rH_plot, act_merge$rH_model, na.rm=TRUE)
          rmse_tas <- rmse(act_merge$Ta_200_plot, act_merge$Ta_200_model, na.rm=TRUE)
          
          png(filename=paste0(filebase_results, fld_o,
                                j, "_rH.png"), width = 1280, height = 960)
          plot( act_merge$time, act_merge$rH_plot, lwd = 2,
                type="l", ylim=c(0,100), main = paste0("Time series of rH at ", j))
          lines( act_merge$time, act_merge$rH_model, col="red", lty=2, lwd = 2)
          legend("bottomright", c("rH Plot", "rH Model"), col=c("black", "red"), lty=c(1,2))
          dev.off()
          png(filename=paste0(filebase_results, fld_o,
                              j, "_ta200.png"), width = 1280, height = 960)
          plot( act_merge$time, act_merge$Ta_200_plot, lwd = 2,
                type="l", ylim=c(270,310), main = paste0("Time series of Ta_200 at ", j))
          lines( act_merge$time, act_merge$Ta_200_model, col="red", lty=2, lwd = 2)
          legend("bottomright", c("Ta_200 Plot", "Ta_200 Model"), col=c("black", "red"), lty=c(1,2))
          dev.off()
          act.out.df <- data.frame(plotID = j,
                               rmse_rH, ME_rH, MAE_rH,
                               rmse_tas, ME_tas, MAE_tas)
          out.df <- rbind(act.out.df,out.df)
          return(out.df)
        })
        
        error_stats.out.df <- do.call("rbind", all_rmse_vals) 
        
        write.csv(error_stats.out.df, file = paste0(filebase_results, fld_o,
                                                    "error_stats.csv"))
    return(error_stats.out.df)
})