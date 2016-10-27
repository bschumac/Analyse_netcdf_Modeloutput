
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

temp <- "/media/benjamin/BC0014A800146C20/Kiliman_30km_ERA_Apr_May2014/Kiliman_30km_Apr_May2014_SRF.2014041500.nc"

netcdf_rsns <- stack(temp, varname = "rsns")
netcdf_topo <- stack(temp, varname = "topo")
#netcdf_tas <- stack(temp, varname = "tas")
temp <- "/media/benjamin/BC0014A800146C20/Kiliman_30km_ERA_Apr_May2014/Kiliman_30km_Apr_May2014_SRF.2014050100.nc"
nc_open(temp)
plot(netcdf_topo[[1]])

netcdf_rsns1 <- stack(temp, varname = "rsns")
#netcdf_tas1 <- stack(temp, varname = "tas")

netcdf_rsns <- stack(netcdf_rsns, netcdf_rsns)
#netcdf_tas <- stack(netcdf_tas, netcdf_tas1)

lyr <- ogrListLayers("/home/benjamin/Data_masterthesis/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
plots_shp <- readOGR("/home/benjamin/Data_masterthesis/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
                     layer=  lyr)

plots_csv <- read.csv("/home/benjamin/Data_masterthesis/WD_WS_prec/plots.csv")

# assign projection
crs <- crs("+proj=stere +lat_0=-3.06 +lon_0=37.36 +x_0=-15000. +y_0=-15000. +ellps=sphere +a=6371229. +b=6371229. +units=m +no_defs ")
crs(netcdf_rsns) <- crs
plots_shp <- spTransform(plots_shp, crs(netcdf_rsns))

plot(netcdf_topo[[1]])
# Where is Kili in my Raster? 
row <- rowFromY(netcdf_rsns, -3.076475)
col <- colFromX(netcdf_rsns, 37.353205)


# create Rastermask for the Kili Region. 
raster_mask <- netcdf_rsns[[1]]
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
edgepoints2 <- SpatialPointsDataFrame(ext1, ext1)
#ext1 <- as.matrix(ext1)
ext1 <- extent(ext1)
#ext1 <- as(ext1, 'SpatialPolygons')

netcdf_rsns_kili <- crop(netcdf_rsns, ext1)
netcdf_topo_kili <- crop(netcdf_topo, ext1)
#netcdf_tas_kili <- crop(netcdf_tas, ext1)

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
plots_names <- as.character(unique(plots_csv$plotID))
head(plots_csv,100)
plots_csv <- na.omit(plots_csv)
out.df <- data.frame()

all_rmse_vals<- lapply(plots_names, function(i){
  i <- as.character(plots_csv$plotID[1])
  print(i)
  act_plot_shp <- plots_shp[plots_shp@data$PlotID == i,]
  act_plot_csv <- plots_csv[plots_csv$plotID == i,]
  
  #extract(netcdf_topo_kili, act_plot_shp)
  #data.frame(act_plot_shp)
  
  
  row <- rowFromY(netcdf_topo_kili, act_plot_shp)[1]
  col <- colFromX(netcdf_topo_kili, act_plot_shp)[1]
  #plot(netcdf_topo_kili)
  print(row)
  print(col)
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
  act_netcdf_rsns_kili <- crop(netcdf_rsns_kili, ext2)
  
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
  
  act_netcdf_rsns_kili_vals <- data.frame(t(extract(act_netcdf_rsns_kili, min_dif_ele, df=TRUE)))
  act_netcdf_rsns_kili_vals$time <-  as.POSIXct(rownames(act_netcdf_rsns_kili_vals), 
                                             format = "X%Y.%m.%d.%H.%M.%S.1")
  head(act_netcdf_rsns_kili_vals)
  colnames(act_netcdf_rsns_kili_vals) <- c("rsns", "time")
  
  act_netcdf_rsns_kili_vals <- act_netcdf_rsns_kili_vals[-1,]
  rownames(act_netcdf_rsns_kili_vals) <- seq(1,length(act_netcdf_rsns_kili_vals$rsns))
  #act_tas_hur <- merge(act_netcdf_tas_kili_vals,act_netcdf_hurs_kili_vals, by="time")
  #head(act_tas_hur)
  #colnames(act_tas_hur) <- c("time", "Ta_200_model", "rH_model")
  #act_tas_hur <- act_tas_hur[-1,]
  #act_tas_hur$time <- as.POSIXct(act_tas_hur$time, format="X%Y.%m.%d.%H.%M.%S", tz="")
  
  act_plot_csv <- data.frame(time = act_plot_csv$datetime, 
                             #Ta_200_plot = act_plot_csv$Ta_200,
                             rsns_plot = act_plot_csv$SWDR_300*1000)

  #act_plot_csv$Ta_200_plot <- act_plot_csv$Ta_200_plot+273.15
  act_plot_csv$time <- as.POSIXct(act_plot_csv$time, format="%Y-%m-%dT%H:%M", tz="")
  head(act_plot_csv)
  #match time of netcdf to observations
  
  
  act_merge <- merge(act_plot_csv, act_netcdf_rsns_kili_vals, by="time", all = FALSE)
  library(hydroGOF)
  head(act_merge)
  
  rmse_rsns <- rmse(act_merge$rsns_plot, act_merge$rsns, na.rm=TRUE)
  #rmse_tas <- rmse(act_merge$Ta_200_plot, act_merge$Ta_200_model, na.rm=TRUE)
  
  act.out.df <- data.frame(plotID = i,
                           rmse_rH,
                           rmse_tas)
  out.df <- rbind(act.out.df,out.df)
  return(out.df)
})





