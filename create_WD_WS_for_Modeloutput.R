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

fld_lst <- list.files(filebase_raster, full.names = TRUE)

lst_fcl <- lapply(fld_lst, function(i){
  #i <- fld_lst[2]
  print(i)
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                       "", i),"/")
  temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014041500.nc")
  netcdf_uas <- stack(temp, varname = "uas")
  
  netcdf_topo <- stack(temp, varname = "topo")
  netcdf_vas <- stack(temp, varname = "vas")
  temp <- paste0(i,"/Kiliman_30km_Apr_May2014_SRF.2014050100.nc")
  netcdf_uas1 <- stack(temp, varname = "uas")
  netcdf_vas1 <- stack(temp, varname = "vas")
  
  netcdf_uas <- stack(netcdf_uas, netcdf_uas1)
  netcdf_vas <- stack(netcdf_vas, netcdf_vas1)
  
  lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
  plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                       layer=  lyr)
  # assign projection
  crs <- crs("+proj=stere +lat_0=-3.06 +lon_0=37.36 +x_0=-15000. +y_0=-15000. +ellps=sphere +a=6371229. +b=6371229. +units=m +no_defs ")
  crs(netcdf_uas) <- crs
  crs(netcdf_vas) <- crs
  plots_shp <- spTransform(plots_shp, crs(netcdf_vas))
  
  
  row <- rowFromY(netcdf_vas, -3.076475)
  col <- colFromX(netcdf_vas, 37.353205)
  
  # create Rastermask for the Kili Region. 
  raster_mask <- netcdf_vas[[1]]
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
  
  netcdf_vas_kili <- crop(netcdf_vas, ext1)
  netcdf_topo_kili <- crop(netcdf_topo, ext1)
  netcdf_uas_kili <- crop(netcdf_uas, ext1)
  
  plot(netcdf_vas_kili[[12]])
  plot(ext1, add=T)
  plot(plots_shp, add=T)
  
  netcdf_WD_kili <- stack()
  netcdf_WS_kili <- stack()
  raster_mask <- netcdf_uas_kili[[1]]
  vals <- 0
  values(raster_mask) <- vals
  act_ws_rst <- raster_mask
  act_wd_rst <- raster_mask
  print(fld_o)
  for (j in seq(1,nlayers(netcdf_uas_kili))){
    print(paste("Layer", j))
    values(act_wd_rst) <- 0
    values(act_ws_rst) <- 0
    act_netcdf_uas_kili <-  netcdf_uas_kili[[j]]
    act_netcdf_vas_kili <-  netcdf_vas_kili[[j]]
    for (i in (seq(1,ncell(act_netcdf_uas_kili)))){
      #print(i)
      #print(paste("Cell",i))
        act_val_uas <- values(act_netcdf_uas_kili)[i]
        act_val_vas <- values(act_netcdf_vas_kili)[i]
        ws <- sqrt(act_val_uas**2+ act_val_vas**2)
        
        if ( act_val_uas > 0 & act_val_vas > 0){
          wd <- NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
        }
        if (act_val_uas > 0 & act_val_vas < 0){
          wd <- 180 - NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
        }
        if (act_val_uas < 0 & act_val_vas < 0){
          wd <- 180 + NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
        }
        if (act_val_uas < 0 & act_val_vas > 0){
          wd <- 360 - NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
        }
        if (wd>360){
          print("WHAT?!")
          break
        }
        values(act_ws_rst)[i] <- ws
        values(act_wd_rst)[i] <- wd
    }
    if (wd>360){
      print("WHAT?!")
      break
    }
    netcdf_WD_kili <- stack(netcdf_WD_kili, act_wd_rst)
    netcdf_WS_kili <- stack(netcdf_WS_kili,act_ws_rst)
  }
  writeRaster(netcdf_WD_kili, filename = paste0(filebase_raster,"/", fld_o, "WD_kiliman_Apr_May2014.nc"), overwrite=TRUE)
  writeRaster(netcdf_WS_kili, filename = paste0(filebase_raster,"/", fld_o, "WS_kiliman_Apr_May2014.nc"), overwrite=TRUE)
  
})


