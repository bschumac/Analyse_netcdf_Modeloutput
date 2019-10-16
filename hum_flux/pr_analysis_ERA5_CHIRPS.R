# create Humidity Budget

#if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
#if (!require(ncdf4)){install.packages("ncdf4", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
#if(!require(caret)){install.packages('caret')}
library(caret)
#if(!require(mapview)){install.packages('mapview')}
#library(mapview)
#install.packages( "hydroGOF")
library(hydroGOF)
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

library(pracma)
library(caTools)
library(RColorBrewer)
library(lubridate)

display.brewer.all()




filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS")
filebase_raster <- paste0(filebase_path, "raster/")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))



base_path <- "/media/benjamin/XChange/Masterarbeit/Copernicus_Download/"


years <- c("2013","2014","2015")
months_num <- c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
real_mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ges_df <- NULL
prc_evt <- c()
prc_sum <- c()
total_flux <- c()
sum_mth <- c()
sum_mth_local <- c()
sum_mth_regional <- c()
sum_evt_local <- c()
sum_evt_regional <- c()
sum_timesteps <- c()
par(mfrow=c(1,1))
tot_sum_ERA5 <- c()
tot_sum_CHIRPS <- c()
tot_sum_RegCM <- c()

for (r in seq(1,3)){
  
  for (i in seq(1,12)){
    print(i)
    filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS")
    
    pattern = paste0(years[r],".",months_num[i])
    
    filebase_raster_CHIRPS <- paste0(filebase_raster_CHIRPS,"_",years[r],"_daily/",years[r],"/")
    if (r == 2){
      act_RegCMfile <- paste0(filebase_raster,"2014_complete/Kiliman_20km_ERA_",real_mth[i],"2014_GrellFC01/output/Kiliman_20km_",real_mth[i],"2014_ATM.2014",months_num[i],"0100.nc")
      
      #prcRegCM <- brick(act_RegCMfile, varname="pr")
      prcRegCM <- read_modeloutput(act_RegCMfile, variable = "pr")
      
      prc_RegCM_kili <- crp_raster(prcRegCM, window_size = -0.5, pointX = 37.353205, pointY = -3.276475)
      values(prc_RegCM_kili) <- values(prc_RegCM_kili)*3600
      tot_sum_RegCM <- c(tot_sum_RegCM,sum(c(values(prc_RegCM_kili))))
      }
    prcERA5 <- stack(paste0(base_path,years[r],"/",months_num[i],"/output_",years[r],"_pr_mon_",months_num[i],".nc"), varname="tp")
    #prc2  <- read_modeloutput(paste0(act_fld,"/Kiliman_20km_Apr_May2014_ATM.2014041500.nc"), variable = "pr")
    fld_lst <- list.files(filebase_raster_CHIRPS, full.names = TRUE,pattern=pattern)
    
    prc_CHIRPS <- stack(fld_lst)
    
    
    
    topo <- stack("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/output_2013_oro_01.nc")/10
    topo <- topo[[1]]
    
    #plot(hum_flux_res[[1]])
    
    
    topo_kili <- crp_raster(topo, window_size = 2)
    prc_ERA5_kili <- crp_raster(prcERA5, window_size = -1)
    prc_CHIRPS_kili <- crp_raster(prc_CHIRPS, window_size = 1)
    
    values(prc_ERA5_kili) <- values(prc_ERA5_kili)*3600
    tot_sum_ERA5 <- c(tot_sum_ERA5,sum(c(values(prc_ERA5_kili))))
    tot_sum_CHIRPS <- c(tot_sum_CHIRPS,sum(values(prc_CHIRPS_kili)))
    
    #nb.col=8
    #color <- rainbow(nb.col)
  }
}  
    pointchar = 19   
    plot(tot_sum_ERA5[1:12], type="b", col="black",ylim=c(0,15000), xlab="Month", ylab="Precipitation Sum (mm)", xaxt="n", pch=pointchar,cex=1.5, cex.lab=1.5,lwd=2)
    lines(tot_sum_ERA5[13:24], type="b", col="darkred", pch=pointchar,cex=1.5,lwd=2)
    lines(tot_sum_ERA5[25:36], type="b", col="blue", pch=pointchar,cex=1.5,lwd=2)
    lines(tot_sum_CHIRPS[1:12], type="b", col="darkgrey", pch=pointchar,cex=1.5,lwd=2)
    lines(tot_sum_CHIRPS[13:24], type="b", col="red", pch=pointchar,cex=1.5,lwd=2)
    lines(tot_sum_CHIRPS[25:36], type="b", col="aquamarine3", pch=pointchar,cex=1.5,lwd=2)
    lines(tot_sum_RegCM,type="b", col="purple", pch=pointchar,cex=1.5,lwd=2)
    axis(1, at = seq(1, 12),labels=real_mth, las=2, cex.axis=1.5)
    legend(6, 16000, legend=c("Prc ERA5 2013", "Prc CHIRPS 2013", "Prc ERA5 2014", "Prc CHIRPS 2014", "Prc RegCM 2014","Prc ERA5 2015", "Prc CHIRPS 2015"),
           col=c("black","darkgrey","darkred", "red", "purple", "blue","aquamarine3"),  lwd=2, cex=1.5, box.lty=0, bg="transparent", pch = pointchar,pt.bg = 'white', lty = 1, bty="n")
    
    #plot(hum_flux_kili[[15]], col=color)
    
    