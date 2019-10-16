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
#if(!require(mapview)){install.packages('mapview')}
library(mapview)
#devtools::install_github("r-spatial/mapview@develop")
library(hydroGOF)
#install.packages("hydroGOF")
library(chron)
library(stringr)
library(hydroGOF)

#####################################################


filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path,"CHIRPS_2014_daily/2014")
filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS")
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

prc_apr_may_kili <- crp_raster(prc_apr_may, window_size = 9)



lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                     layer=  lyr)
plots_shp <- spTransform(plots_shp, crs(prc_apr_may))



#plots_csv <- read.csv("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/raw/plots.csv")

plots_csv <- read.csv("/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/plots.csv")

agg_plots <- function(name){
  plots_csv_cof3 <- plots_csv[plots_csv$plotID==name,]
  # data management plots_csv
  plots_csv_cof3$datetime <- as.POSIXct(plots_csv_cof3$datetime, format="%Y-%m-%dT%H:%M", tz="UTC")
  plots_csv_cof3$datetime <- plots_csv_cof3$datetime - 7200
  
  plots_csv_cof3$yearmonth <-(substr(plots_csv_cof3$datetime, 1,7))
  plots_csv_cof3$month <- as.numeric(substr(plots_csv_cof3$datetime, 6,7))
  plots_csv_cof3$day <- as.numeric(substr(plots_csv_cof3$datetime, 9,10))
  plots_csv_cof3 <- plots_csv_cof3[plots_csv_cof3$year > 2013,]
  plots_csv_cof3 <- plots_csv_cof3[plots_csv_cof3$year < 2016,]
  plots_csv_agg<- aggregate(plots_csv_cof3$P_RT_NRT, by=list(plots_csv_cof3$yearmonth), FUN="sum", na.rm=TRUE,na.action=NULL)
return(plots_csv_agg)
}  





years <- c("2013","2014","2015")
months_num <- c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
real_mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

tot_sum_CHIRPS <- c()
tot_sum_RegCM <- c()



av_plots <- levels(factor(plots_csv$plotID))

for (j in seq(1,length(av_plots))){
  act_plot <- plots_shp[plots_shp$PlotID == av_plots[j],]
  print(av_plots[j])
  print(act_plot$Z_DEM_HMP[1])
  
}


act_plot <- plots_shp[plots_shp$PlotID == "sav0",]
sav0_pX<- mean(act_plot$coords.x1)
sav0_pY <- mean(act_plot$coords.x2)


act_plot <- plots_shp[plots_shp$PlotID == "cof3",]
cof3_pX<- mean(act_plot$coords.x1)
cof3_pY <- mean(act_plot$coords.x2)


act_plot <- plots_shp[plots_shp$PlotID == "mcg0",]
mcg0_pX<- mean(act_plot$coords.x1)
mcg0_pY <- mean(act_plot$coords.x2)

prc_CHIRPS_sav0 <- c()

prc_CHIRPS_cof3 <- c()

prc_CHIRPS_mcg0 <- c()




for (r in seq(1,3)){
  
  for (i in seq(1,12)){
    print(i)
    pattern = paste0(years[r],".",months_num[i])
    filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS")
    
    filebase_raster_CHIRPS <- paste0(filebase_raster_CHIRPS,"_",years[r],"_daily/",years[r],"/")
    
    #prc2  <- read_modeloutput(paste0(act_fld,"/Kiliman_20km_Apr_May2014_ATM.2014041500.nc"), variable = "pr")
    fld_lst <- list.files(filebase_raster_CHIRPS, full.names = TRUE,pattern=pattern)
    
    prc_CHIRPS <- stack(fld_lst)
    
    
    
    
    #prc_CHIRPS_kili <- crp_raster(prc_CHIRPS, window_size = 7)
    
    act_CHIRPS_sav0 <- crp_raster(prc_CHIRPS, window_size = -1, pointX =sav0_pX ,pointY = sav0_pY)
    act_CHIRPS_cof3 <- crp_raster(prc_CHIRPS, window_size = -1, pointX = cof3_pX,pointY = cof3_pY)
    act_CHIRPS_mcg0 <- crp_raster(prc_CHIRPS, window_size = -1, pointX = mcg0_pX,pointY = mcg0_pY)
    
    #prc_CHIRPS_sav0 <- stack(prc_CHIRPS_sav0,act_CHIRPS_sav0)
    #prc_CHIRPS_cof3 <- stack(prc_CHIRPS_cof3,act_CHIRPS_cof3)
    #prc_CHIRPS_mcg0 <- stack(prc_CHIRPS_mcg0,act_CHIRPS_mcg0)
    
    prc_CHIRPS_sav0 <- c(prc_CHIRPS_sav0,sum(values(act_CHIRPS_sav0)))
    prc_CHIRPS_cof3 <- c(prc_CHIRPS_cof3,sum(values(act_CHIRPS_cof3)))
    prc_CHIRPS_mcg0 <- c(prc_CHIRPS_mcg0,sum(values(act_CHIRPS_mcg0)))
    
    #nb.col=8
    #color <- rainbow(nb.col)
  }
}  






prc_CHIRPS_sav0

crp_raster(prc_CHIRPS, window_size = -1, )


act_plot <- plots_shp[plots_shp$PlotID == "sav0",]
sav0_pX<- mean(act_plot$coords.x1)
sav0_pY <- mean(act_plot$coords.x2)





sav0_plot <-agg_plots("sav0")
cof3_plot <-agg_plots("cof3")
mcg0_plot <- agg_plots("mcg0")

plots_shp <- plots_shp[plots_shp$PlotID %in% av_plots,]



pointchar = 19
plot(mcg0_plot$x,xaxt="n", type="b", xlab="", ylab="Precipitation sum (mm)", pch = pointchar)
lines(prc_CHIRPS_mcg0, col="black", lwd=2)


lines(sav0_plot$x, col="darkred", type="b", pch = pointchar)
lines(prc_CHIRPS_sav0, col="darkred", lwd=2)

lines(cof3_plot$x, col="blue", type="b", pch = pointchar)
lines(prc_CHIRPS_cof3, col="blue", lwd=2)

axis(1, at=c(seq(1,36,1)), labels=plots_csv_agg$Group.1,las=2)
legend(1, 900, legend=c("Prc 952 m a.s.l", "Prc 1287 m a.s.l.", "Prc 2167 m a.s.l."),
       col=c("black","darkred","blue"),  lwd=1, cex=1, box.lty=0, bg="transparent", pch = pointchar,pt.bg = 'white', lty = 1, bty="n")


mapview(plots_shp) + prc_CHIRPS_kili[[15]]
df

plot(prc_CHIRPS_kili[[15]])



















