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
library(hydroGOF)
#####################################################


filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path,"CHIRPS_2014_daily/2014")
filebase_model <- paste0(filebase_path, "raster")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
filebase_results <- paste0(filebase_path, "results/plot_vs_model_vs_CHIRPS")
source(paste0(filebase_code,"analyse_fun.R"))

fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = c("2014_04"))
fld_lst <- c(fld_lst, list.files(filebase_raster, full.names = TRUE, pattern = c("2014_05")))

prc_apr_may <- stack(fld_lst[seq(15,45)])

vals_prc <- values(prc_apr_may) 
vals_prc <- replace(vals_prc, vals_prc==-9999, NA)
values(prc_apr_may) <- vals_prc



lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                     layer=  lyr)
plots_shp <- spTransform(plots_shp, crs(prc_apr_may))

plots_csv <- read.csv("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/raw/plots.csv")
# data management plots_csv
head(plots_csv)
plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="UTC")
str(plots_csv$datetime)
plots_csv$datetime <- plots_csv$datetime - 7200

plots_csv$year <- as.numeric(substr(plots_csv$datetime, 1,4))
plots_csv$month <- as.numeric(substr(plots_csv$datetime, 6,7))
plots_csv$day <- as.numeric(substr(plots_csv$datetime, 9,10))
#plots_csv$hourmin <- (substr(plots_csv$datetime, 12,16))
#plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
#plots_csv$hourmin <- times(paste0(plots_csv$hourmin, ":00"))
plots_csv <- plots_csv[plots_csv$year > 2013,]
plots_csv <- plots_csv[plots_csv$year < 2015,]
plots_csv <- plots_csv[plots_csv$month > 3,]
plots_csv <- plots_csv[plots_csv$month < 6,]

plots_csv <- plots_csv[plots_csv$day >= 15 & plots_csv$month == 4 | plots_csv$month == 5,]
plots_csv <- plots_csv[plots_csv$day <= 15 & plots_csv$month == 5 | plots_csv$month == 4 ,]
plots_csv$agg <- paste(plots_csv$month, plots_csv$day, sep = "-")

plots_csv <- na.omit(plots_csv)
plots_names <- as.character(unique(plots_csv$plotID))


fld_lst_model <- list.files(filebase_model, full.names = TRUE, pattern="15")

out.df <- data.frame()
all_rmse_vals <- NULL

for (i in seq(1, length(plots_names))){
  #i <- 1
  j <- plots_names[i]
  print(j)
  act_plot_shp <- plots_shp[plots_shp@data$PlotID == j,]
  act_plot_csv <- plots_csv[plots_csv$plotID == j,]
  
  
  lst_models <- lapply(fld_lst_model, function(i){
    #i <- fld_lst_model[3]
    print(i)
    fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                         "", i),"/")
    temp <- paste0(i,"/Kiliman_15km_Apr_May2014_SRF.2014041500.nc")
    netcdf_topo <- read_modeloutput(filepath = temp, variable = "topo")
    netcdf_prc2 <- read_modeloutput(temp, "prc")
    temp <- paste0(i,"/Kiliman_15km_Apr_May2014_SRF.2014050100.nc")
    netcdf_prc1 <- read_modeloutput(filepath = temp, variable = "prc")
    netcdf_prc <- stack(netcdf_prc2, netcdf_prc1)
    
    act_netcdf_topo_kili <- crp_raster(netcdf_topo, pointX = as.numeric(act_plot_shp[1,1]@coords[,1]) , pointY = as.numeric(act_plot_shp[1,1]@coords[,2]), window_size = 3)
    
    values(act_netcdf_topo_kili) <- abs(values(act_netcdf_topo_kili)-mean(act_plot_shp@data$Z_GPS))
    
    
    vals<-extract(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
    coords <-xyFromCell(act_netcdf_topo_kili,1:ncell(act_netcdf_topo_kili))
    combine <- data.frame(cbind(coords,vals))
    min_dif_ele <- combine[combine$Surface.Model.Elevation == min(combine$Surface.Model.Elevation),]
    min_dif_ele <- data.frame(min_dif_ele$x, min_dif_ele$y)
    min_dif_ele <- SpatialPointsDataFrame(min_dif_ele, min_dif_ele) 
    
    act_model_prc <- data.frame(t(extract(netcdf_prc, min_dif_ele, df=TRUE)))
    act_model_prc$time <-  as.POSIXct(rownames(act_model_prc), 
                                                 format = "X%Y.%m.%d.%H.%M.%S")
    colnames(act_model_prc) <- c("prc", "time")
    
    act_model_prc <- act_model_prc[-1,]
    rownames(act_model_prc) <- seq(1,length(act_model_prc$prc))
    act_model_prc$prc <- act_model_prc$prc*3600
    act_model_prc$year <- as.numeric(substr(act_model_prc$time, 1,4))
    act_model_prc$month <- as.numeric(substr(act_model_prc$time, 6,7))
    act_model_prc$day <- as.numeric(substr(act_model_prc$time, 9,10))
    act_model_prc$agg <- paste(act_model_prc$month, act_model_prc$day, sep = "-")
    actm_prc_agg <- aggregate(act_model_prc$prc, by= list(act_model_prc$agg), FUN=sum)
    actm_prc_agg <- data.frame(time= as.POSIXct(paste0("2014-","0",actm_prc_agg$Group.1),
                                      format="%Y-%m-%d" ,tz="GMT"),
                     prc = actm_prc_agg$x)
    actm_prc_agg <- actm_prc_agg[order(actm_prc_agg$time),]
    return(actm_prc_agg)
    
    # gib liste mit der timesieres zurueck
  })
  # lst mit for schleife als lines dazuplotten to dataframe all 3 timeseries of the models are converted
  
  
  actp_csv_agg <- aggregate(act_plot_csv$P_RT_NRT, by= list(act_plot_csv$agg), FUN=sum)
  
  actp_csv_agg <- data.frame(plotID = j,
                             MthDay = actp_csv_agg$Group.1,
                             time = as.POSIXct(paste0("2014-","0",actp_csv_agg$Group.1),
                                               format="%Y-%m-%d" ,tz="GMT"),
                             P_RT_NRT = as.numeric(actp_csv_agg$x)
                             )
  
  act_CHIRPS_prc <- data.frame(t(extract(prc_apr_may, act_plot_shp[1,1], df=TRUE)))
  act_CHIRPS_prc$time <-  as.POSIXct(rownames(act_CHIRPS_prc), 
                                     format = "X%Y_%m_%d",tz="GMT")
  
  colnames(act_CHIRPS_prc) <- c("prc", "time")
  
  act_CHIRPS_prc <- act_CHIRPS_prc[-1,]
  rownames(act_CHIRPS_prc) <- seq(1,length(act_CHIRPS_prc$prc))
  head(act_CHIRPS_prc)
  
  act_CHIRPS_prc$year <- as.numeric(substr(act_CHIRPS_prc$time, 1,4))
  act_CHIRPS_prc$month <- as.numeric(substr(act_CHIRPS_prc$time, 6,7))
  act_CHIRPS_prc$day <- as.numeric(substr(act_CHIRPS_prc$time, 9,10))
  #plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
  act_CHIRPS_prc <- act_CHIRPS_prc[act_CHIRPS_prc$year > 2013,]
  act_CHIRPS_prc <- act_CHIRPS_prc[act_CHIRPS_prc$year < 2015,]
  act_CHIRPS_prc <- act_CHIRPS_prc[act_CHIRPS_prc$month > 3,]
  act_CHIRPS_prc <- act_CHIRPS_prc[act_CHIRPS_prc$month < 6,]
  
  act_CHIRPS_prc <- act_CHIRPS_prc[act_CHIRPS_prc$day >= 15 & act_CHIRPS_prc$month == 4 | act_CHIRPS_prc$month == 5,]
  act_CHIRPS_prc <- act_CHIRPS_prc[act_CHIRPS_prc$day <= 15 & act_CHIRPS_prc$month == 5 | act_CHIRPS_prc$month == 4 ,]
  act_CHIRPS_prc <- data.frame(time = act_CHIRPS_prc$time, prc_CHIRPS = act_CHIRPS_prc$prc)
  
  actp_csv_agg <- data.frame(time = actp_csv_agg$time,
                             prc_plot = actp_csv_agg$P_RT_NRT)
  
  
  act_merge <- merge(act_CHIRPS_prc, actp_csv_agg, by="time", all = FALSE)
  
  rmse_prc <- rmse(act_merge$prc_plot, act_merge$prc_CHIRPS, na.rm=TRUE)
  
  #rmse_tas <- rmse(act_merge$Ta_200_plot, act_merge$Ta_200_model, na.rm=TRUE)
  ME_prc <- mean( act_merge$prc_CHIRPS - act_merge$prc_plot, na.rm = TRUE)
  #MAE <- mean(abs(pred_vals - obs_vals), na.rm = TRUE)
  MAE_prc <- mean(abs( act_merge$prc_CHIRPS - act_merge$prc_plot), na.rm = TRUE)
  
  png(filename=paste0(filebase_results,"/",
                      j, "_prc.png"), width = 1280, height = 960)
  act_plot <- plot(act_merge$time, act_merge$prc_plot, lwd = 2,
                   type="l", ylim=c(0,80), main = paste0("Time series of prc at ", j))
  
  lines(act_merge$time, act_merge$prc_CHIRPS, col="red", lty=2, lwd = 2)
  col <- c("green", "blue", "orange")
  for (i in seq(1, length(lst_models))){
    lines( lst_models[[i]]$time, lst_models[[i]]$prc, col=col[i], lty=2, lwd = 2)
  }
  
  legend("bottomright", c("prc Plot", "prc CHIRPS", "gul2", "gul5", "gul9"), col=c("black", "red", col), lty=c(1,2))
  dev.off()
  
  
  act.out.df <- data.frame(plotID = j,
                           rmse_prc,
                           ME_prc,
                           MAE_prc)
  out.df <- rbind(act.out.df,out.df)
}
write.csv(out.df, file = paste0(filebase_results,"/",
                                "error_stats_CHIRPS_plots.csv"))
