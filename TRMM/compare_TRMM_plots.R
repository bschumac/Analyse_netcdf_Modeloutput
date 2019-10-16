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
#####################################################


filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path,"TRMM/cut_raster")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
filebase_results <- paste0(filebase_path, "results/plot_vs_TRMM")


source(paste0(filebase_code,"crp_fun.R"))
fld_names1 <- list.files("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/TRMM/raster/201404", full.names = FALSE, pattern = "03hr.tif")
fld_names2 <-  list.files("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/TRMM/raster/201405", full.names = FALSE, pattern = "03hr.tif")

names_prc <- c(fld_names1, fld_names2)

fld_lst <- list.files(filebase_raster, full.names = TRUE)
prc_apr_may <- stack(fld_lst[c(7,9)])
names(prc_apr_may) <- as.POSIXct(gsub(".7.03hr.tif","",gsub("3B42RT.", "",names_prc)), format="%Y%m%d%H", tz="GMT")

  lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
  plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                     layer=  lyr)
  plots_shp <- spTransform(plots_shp, crs(prc_apr_may))

  plots_csv <- read.csv("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/raw/plots.csv")
  # data management plots_csv
  
  plots_csv$year <- as.numeric(substr(plots_csv$datetime, 1,4))
  plots_csv$month <- as.numeric(substr(plots_csv$datetime, 6,7))
  plots_csv$day <- as.numeric(substr(plots_csv$datetime, 9,10))
  plots_csv$hourmin <- (substr(plots_csv$datetime, 12,16))
  #plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
  plots_csv$hourmin <- times(paste0(plots_csv$hourmin, ":00"))
  plots_csv <- plots_csv[plots_csv$year > 2013,]
  plots_csv <- plots_csv[plots_csv$year < 2015,]
  plots_csv <- plots_csv[plots_csv$month > 3,]
  plots_csv <- plots_csv[plots_csv$month < 6,]
  
  plots_csv <- plots_csv[plots_csv$day >= 15 & plots_csv$month == 4 | plots_csv$month == 5,]
  plots_csv <- plots_csv[plots_csv$day <= 15 & plots_csv$month == 5 | plots_csv$month == 4 ,]
  vec <- sapply(seq(plots_csv$hourmin), function(i) {
    if (plots_csv$hourmin[i] > times("22:30:00") | plots_csv$hourmin[i] <= times("01:30:00"))1 else
      if (plots_csv$hourmin[i] > times("01:30:00") & plots_csv$hourmin[i] <= times("04:30:00")) 2 else
        if (plots_csv$hourmin[i] > times("04:30:00") & plots_csv$hourmin[i] <= times("07:30:00")) 3 else
          if (plots_csv$hourmin[i] > times("07:30:00") & plots_csv$hourmin[i] <= times("10:30:00")) 4 else
            if (plots_csv$hourmin[i] > times("10:30:00") & plots_csv$hourmin[i] <= times("13:30:00")) 5 else
              if (plots_csv$hourmin[i] > times("13:30:00") & plots_csv$hourmin[i] <= times("16:30:00")) 6 else
                if (plots_csv$hourmin[i] > times("16:30:00") & plots_csv$hourmin[i] <= times("19:30:00")) 7 else
                  if (plots_csv$hourmin[i] > times("19:30:00") & plots_csv$hourmin[i] <= times("22:30:00")) 8
  })
  plots_csv$agg <- paste(plots_csv$month, plots_csv$day, vec, sep = "-")
  
  plots_csv <- na.omit(plots_csv)
  plots_names <- as.character(unique(plots_csv$plotID))
  out.df <- data.frame()
  all_rmse_vals <- NULL
  for (i in seq(1, length(plots_names))){
    
    j <- plots_names[i]
    print(j)
    act_plot_shp <- plots_shp[plots_shp@data$PlotID == j,]
    act_plot_csv <- plots_csv[plots_csv$plotID == j,]
    actp_csv_agg <- aggregate(act_plot_csv$P_RT_NRT, by= list(act_plot_csv$agg), FUN=sum)
    P_RT_NRT = as.numeric(actp_csv_agg$x)
    actp_csv_agg <- data.frame(plotID = j,
                               MthDay = actp_csv_agg$Group.1,
                               time = as.numeric(str_sub(actp_csv_agg$Group.1, -1,-1)))
    
    
    actp_csv_agg[actp_csv_agg==1] <- "00:00:00"
    actp_csv_agg[actp_csv_agg==2] <- "03:00:00"
    actp_csv_agg[actp_csv_agg==3] <- "06:00:00"
    actp_csv_agg[actp_csv_agg==4] <- "09:00:00"
    actp_csv_agg[actp_csv_agg==5] <- "12:00:00"
    actp_csv_agg[actp_csv_agg==6] <- "15:00:00"
    actp_csv_agg[actp_csv_agg==7] <- "18:00:00"
    actp_csv_agg[actp_csv_agg==8] <- "21:00:00"
    actp_csv_agg$P_RT_NRT <- P_RT_NRT
    
    str(actp_csv_agg)
    actp_csv_agg$datetime <- paste0("2014-","0",str_sub(actp_csv_agg$MthDay,1,-3),"-",actp_csv_agg$time)
    actp_csv_agg$datetime <- as.POSIXct(actp_csv_agg$datetime, format="%Y-%m-%d-%H:%M:%S",tz="GMT")
    
    
    #test <- crp_raster(prc_apr_may, pointX = as.numeric(act_plot_shp[1,1]@coords[,1]), 
    #           pointY = as.numeric(act_plot_shp[1,1]@coords[,2]), window_size = 0)
    

    #act_netcdf_tas_kili_vals <- data.frame(t(extract(act_netcdf_tas_kili, min_dif_ele, df=TRUE)))
    #act_netcdf_tas_kili_vals$time <-  rownames(act_netcdf_hurs_kili_vals)
    #names(act_netcdf_prc_kili)
    act_netcdf_prc <- data.frame(t(extract(prc_apr_may, act_plot_shp[1,1], df=TRUE)))
    act_netcdf_prc$time <-  as.POSIXct(rownames(act_netcdf_prc), 
                                                 format = "X%Y.%m.%d.%H.%M.%S")
    #head(act_netcdf_prc_kili_vals)
    colnames(act_netcdf_prc) <- c("prc", "time")
    
    act_netcdf_prc <- act_netcdf_prc[-1,]
    rownames(act_netcdf_prc) <- seq(1,length(act_netcdf_prc$prc))
 
    
    act_netcdf_prc$year <- as.numeric(substr(act_netcdf_prc$time, 1,4))
    act_netcdf_prc$month <- as.numeric(substr(act_netcdf_prc$time, 6,7))
    act_netcdf_prc$day <- as.numeric(substr(act_netcdf_prc$time, 9,10))
    #plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
    act_netcdf_prc <- act_netcdf_prc[act_netcdf_prc$year > 2013,]
    act_netcdf_prc <- act_netcdf_prc[act_netcdf_prc$year < 2015,]
    act_netcdf_prc <- act_netcdf_prc[act_netcdf_prc$month > 3,]
    act_netcdf_prc <- act_netcdf_prc[act_netcdf_prc$month < 6,]
    
    act_netcdf_prc <- act_netcdf_prc[act_netcdf_prc$day >= 15 & act_netcdf_prc$month == 4 | act_netcdf_prc$month == 5,]
    act_netcdf_prc <- act_netcdf_prc[act_netcdf_prc$day <= 15 & act_netcdf_prc$month == 5 | act_netcdf_prc$month == 4 ,]
    act_netcdf_prc <- data.frame(time = act_netcdf_prc$time, prc_netcdf = act_netcdf_prc$prc)
    actp_csv_agg <- data.frame(time = actp_csv_agg$datetime, prc_plot= as.numeric(actp_csv_agg$P_RT_NRT))
    
    str(actp_csv_agg)
    act_merge <- merge(act_netcdf_prc, actp_csv_agg, by="time", all = FALSE)
    head(act_merge)
    library(hydroGOF)
    
    
    rmse_prc <- rmse(act_merge$prc_plot, act_merge$prc_netcdf, na.rm=TRUE)
    
    #rmse_tas <- rmse(act_merge$Ta_200_plot, act_merge$Ta_200_model, na.rm=TRUE)
    ME_prc <- mean( act_merge$prc_netcdf - act_merge$prc_plot, na.rm = TRUE)
    #MAE <- mean(abs(pred_vals - obs_vals), na.rm = TRUE)
    MAE_prc <- mean(abs( act_merge$prc_netcdf - act_merge$prc_plot), na.rm = TRUE)
    
    png(filename=paste0(filebase_results,"/",
                        j, "_prc.png"), width = 1280, height = 960)
    act_plot <- plot(act_merge$time, act_merge$prc_plot, lwd = 2,
                type="l", ylim=c(0,100), main = paste0("Time series of prc at ", j))
    print(act_plot)
    lines( act_merge$time, act_merge$prc_netcdf, col="red", lty=2, lwd = 2)
    
    legend("bottomright", c("prc Plot", "prc TRMM"), col=c("black", "red"), lty=c(1,2))
    dev.off()
    
    
    act.out.df <- data.frame(plotID = j,
                             rmse_prc,
                             ME_prc,
                             MAE_prc)
    out.df <- rbind(act.out.df,out.df)
  }
  write.csv(out.df, file = paste0(filebase_results,"/",
                                  "error_stats_TRMM_plots.csv"))
  