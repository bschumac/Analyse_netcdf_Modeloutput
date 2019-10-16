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
#library(mapview)
library(hydroGOF)
#install.packages("hydroGOF")
library(chron)
library(stringr)
library(hydroGOF)
library(dplyr)
library(data.table)
#####################################################


filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
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

prc_apr_may_kili <- crp_raster(prc_apr_may, window_size = 9)



lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                     layer=  lyr)
plots_shp <- spTransform(plots_shp, crs(prc_apr_may))



#plots_csv <- read.csv("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/raw/plots.csv")

plots_csv <- read.csv("/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/csv/prec/plots.csv")

plots_csv_cof3 <- plots_csv[plots_csv$plotID=="cof3",]
plots_csv <- plots_csv_cof3
# data management plots_csv
head(plots_csv)
plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="UTC")
str(plots_csv$datetime)
plots_csv$datetime <- plots_csv$datetime - 7200

plots_csv$yearmonth <-(substr(plots_csv$datetime, 1,7))
plots_csv$month <- as.numeric(substr(plots_csv$datetime, 6,7))
plots_csv$day <- as.numeric(substr(plots_csv$datetime, 9,10))
tail(plots_csv)


#plots_csv$hourmin <- (substr(plots_csv$datetime, 12,16))
#plots_csv$datetime <- as.POSIXct(plots_csv$datetime, format="%Y-%m-%dT%H:%M", tz="")[26]
#plots_csv$hourmin <- times(paste0(plots_csv$hourmin, ":00"))
plots_csv <- plots_csv[plots_csv$year > 2013,]
plots_csv <- plots_csv[plots_csv$year < 2016,]
plots_csv_agg<- aggregate(plots_csv$P_RT_NRT, by=list(plots_csv$yearmonth), FUN="sum", na.rm=TRUE,na.action=NULL)

plot(plots_csv_agg$x,xaxt="n", type="b")
axis(1, at=c(seq(1,36,1)), labels=plots_csv_agg$Group.1,las=2)


plots_csv <- plots_csv[plots_csv$year < 2015,]
plots_csv <- plots_csv[plots_csv$month > 3,]
plots_csv <- plots_csv[plots_csv$month < 6,]

plots_csv <- plots_csv[plots_csv$day >= 15 & plots_csv$month == 4 | plots_csv$month == 5,]
plots_csv <- plots_csv[plots_csv$day <= 15 & plots_csv$month == 5 | plots_csv$month == 4 ,]
plots_csv$agg <- paste(plots_csv$month, plots_csv$day, plots_csv$plotID, sep = "-")

plots_csv <- na.omit(plots_csv)
p_csv_agg <- aggregate(plots_csv$P_RT_NRT, by= list(plots_csv$agg), FUN=sum)
colnames(p_csv_agg) <- c("agg","P_RT_NRT")
p_csv_agg$PlotID <- str_sub(p_csv_agg$agg, -4,-1) 
p_csv_agg$year <- "2014"
p_csv_agg$mthday <- str_sub(p_csv_agg$agg, -9,-5) 
p_csv_agg$date <- paste0(p_csv_agg$mthday,p_csv_agg$year)
p_csv_agg$date <- as.POSIXct(p_csv_agg$date,
                             format="%m-%d-%Y" ,tz="GMT")
p_csv_agg <- p_csv_agg[order(p_csv_agg$PlotID, p_csv_agg$date),]
plot_names <- unique(p_csv_agg$PlotID)
plot_dates <- unique(p_csv_agg$date)


p_plots_df <- data.frame(matrix(nrow = length(plot_dates), ncol= length(plot_names)))
rownames(p_plots_df) <- plot_dates
colnames(p_plots_df) <- plot_names

for (i in seq(1,length(plot_names))){
  act_plot <- plot_names[i]
  act_P_RT_NRT <- p_csv_agg[p_csv_agg$PlotID == act_plot,] 
  if (length(act_P_RT_NRT) != length(act_plot)){
    p_plots_df[rownames(p_plots_df) %in% substr(act_P_RT_NRT$date, 1,10),][,i] <- act_P_RT_NRT$P_RT_NRT
  } else {
    p_plots_df[,i] <- act_P_RT_NRT$P_RT_NRT
  }
}

p_plots_df <- p_plots_df[,order(plots_unique$Z_DEM_HMP)]




# pixel extrahieren von allen stationen
#boxplt erstellen

plots_unique<- data.frame(plots_shp) %>% distinct(PlotID, .keep_all = TRUE)
plots_unique <- SpatialPointsDataFrame(coords= data.frame(plots_unique$coords.x1,plots_unique$coords.x2), plots_unique)
crs(plots_unique) <- crs(plots_shp)
plots_unique<-spTransform(plots_unique, crs(prc_apr_may_kili))

plots_unique <- plots_unique[as.character(plots_unique$PlotID) %in% plot_names,]

head(plots_unique, 50)
round(plots_unique$Z_DEM_HMP[order(plots_unique$Z_DEM_HMP)],-2)

prc_CHIRPS <- data.frame(t(extract(prc_apr_may_kili, plots_unique, df=TRUE)))
names(prc_CHIRPS) <- as.character(plots_unique$PlotID)
prc_CHIRPS <- prc_CHIRPS[-1,]
prc_CHIRPS <- prc_CHIRPS[,order(plots_unique$Z_DEM_HMP)]


prc_dif <- prc_CHIRPS




for (i in seq(1,length(plot_names))){
  act_CHIRPS <- prc_CHIRPS[,colnames(prc_CHIRPS) %in% plot_names[i]]
  act_plot <- p_plots_df[,colnames(p_plots_df) %in% plot_names[i]]
  prc_dif[,colnames(prc_dif) %in% plot_names[i]] <- (act_CHIRPS-act_plot)
}
setcolorder(prc_dif, as.character(plots_unique$PlotID))

prc_dif <- prc_dif[,order(plots_unique$Z_DEM_HMP)]


boxplot(prc_dif, horizontal=TRUE, 
        #names=colnames(prc_dif),
        names=round(plots_unique$Z_DEM_HMP[order(plots_unique$Z_DEM_HMP)],-1),
        ylim=c(-250, 350), 
        ylab="Heigth in m above sealevel", xlab = "Difference CHIRPS - rain gauge in mm per Event"
)

error_scores <- read.csv(paste0(filebase_results, "/error_stats_CHIRPS_plots.csv"))
error_scores <- error_scores[,-1]
error_scores <- unique(error_scores)

error_scores <- error_scores[match(colnames(prc_dif), error_scores$plotID),]
error_scores$rmse_prc <- round(error_scores$rmse_prc, 1)

text(-200, c(1:11,11.5), labels=c(error_scores$rmse_prc, "RMSE: "), cex=0.8)
