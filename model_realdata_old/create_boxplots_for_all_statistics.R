#### This script will compare the validation of the models
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


filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")

fld_lst <- list.files(filebase_results, full.names = TRUE)
all_stats_rH_tas <- data.frame()


all_stats_rH_tas <- lapply(fld_lst, function(j){
    #j <- fld_lst[1]
  fld_o <- paste0(gsub("/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/results//Kiliman_30km_ERA_Apr_May2014_", 
                       "", j))
  csv_lst <- list.files(j, full.names = TRUE, pattern=".csv$")
  act_stats <- read.table(csv_lst[1], sep = ",", header=TRUE, row.names = "X")
  
  colnames(act_stats) <- paste0(colnames(act_stats),"_", fld_o)
  
  return(act_stats)
})

all_stats_rH_tas <- do.call("cbind", all_stats_rH_tas)
head(all_stats_rH_tas)
names_rmse_tas <- grep("rmse_tas", names(all_stats_rH_tas), value = TRUE)
names_rmse_rH <- grep("rmse_rH", names(all_stats_rH_tas), value = TRUE)
names_ME_tas <- grep("ME_tas", names(all_stats_rH_tas), value = TRUE)
names_ME_rH <- grep("ME_rH", names(all_stats_rH_tas), value = TRUE)
names_MAE_tas <- grep("MAE_tas", names(all_stats_rH_tas), value = TRUE)
names_MAE_rH <- grep("MAE_rH", names(all_stats_rH_tas), value = TRUE)


fld_name <- paste0(filebase_results, "boxplots/")

png(filename=paste0(fld_name, "tas/rmse_tas.png"), width = 1280, height = 960)
boxplot(all_stats_rH_tas[,which(names(all_stats_rH_tas)%in%names_rmse_tas)])
dev.off()
png(filename=paste0(fld_name, "tas/ME_tas.png"), width = 1280, height = 960)
boxplot(all_stats_rH_tas[,which(names(all_stats_rH_tas)%in%names_ME_tas)])
dev.off()
png(filename=paste0(fld_name, "tas/MAE_tas.png"), width = 1280, height = 960)
boxplot(all_stats_rH_tas[,which(names(all_stats_rH_tas)%in%names_MAE_tas)])
dev.off()
png(filename=paste0(fld_name, "rH/rmse_rH.png"), width = 1280, height = 960)
boxplot(all_stats_rH_tas[,which(names(all_stats_rH_tas)%in%names_rmse_rH)])
dev.off()
png(filename=paste0(fld_name, "rH/ME_rH.png"), width = 1280, height = 960)
boxplot(all_stats_rH_tas[,which(names(all_stats_rH_tas)%in%names_ME_rH)])
dev.off()
png(filename=paste0(fld_name, "rH/MAE_rH.png"), width = 1280, height = 960)
boxplot(all_stats_rH_tas[,which(names(all_stats_rH_tas)%in%names_MAE_rH)])
dev.off()

all_stats_prc <- lapply(fld_lst, function(j){
  #j <- fld_lst[1]
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results//Kiliman_30km_ERA_Apr_May2014_", 
                       "", j))
  csv_lst <- list.files(j, full.names = TRUE, pattern=".csv$")
  act_stats <- read.table(csv_lst[2], sep = ",", header=TRUE, row.names = "X")
  
  colnames(act_stats) <- paste0(colnames(act_stats),"_", fld_o)
  
  return(act_stats)
})

all_stats_prc <- do.call("cbind", all_stats_prc)
head(all_stats_prc)
names_rmse_prc <- grep("rmse_prc", names(all_stats_prc), value = TRUE)
names_ME_prc <- grep("ME_prc", names(all_stats_prc), value = TRUE)
names_MAE_prc <- grep("MAE_prc", names(all_stats_prc), value = TRUE)


png(filename=paste0(fld_name, "prc/rmse_prc.png"), width = 1280, height = 960)
boxplot(all_stats_prc[,which(names(all_stats_prc)%in%names_rmse_prc)])
dev.off()
png(filename=paste0(fld_name, "prc/ME_prc.png"), width = 1280, height = 960)
boxplot(all_stats_prc[,which(names(all_stats_prc)%in%names_ME_prc)])
dev.off()
png(filename=paste0(fld_name, "prc/MAE_prc.png"), width = 1280, height = 960)
boxplot(all_stats_prc[,which(names(all_stats_prc)%in%names_MAE_prc)])
dev.off()
all_stats_SWDR_300 <- lapply(fld_lst, function(j){
  #j <- fld_lst[1]
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results//Kiliman_30km_ERA_Apr_May2014_", 
                       "", j))
  csv_lst <- list.files(j, full.names = TRUE, pattern=".csv$")
  act_stats <- read.table(csv_lst[3], sep = ",", header=TRUE, row.names = "X")
  
  colnames(act_stats) <- paste0(colnames(act_stats),"_", fld_o)
  
  return(act_stats)
})

all_stats_SWDR_300 <- do.call("cbind", all_stats_SWDR_300)
head(all_stats_SWDR_300)
names_rmse_SWDR_300 <- grep("rmse_rsns", names(all_stats_SWDR_300), value = TRUE)
names_ME_SWDR_300 <- grep("ME_rsns", names(all_stats_SWDR_300), value = TRUE)
names_MAE_SWDR_300 <- grep("MAE_rsns", names(all_stats_SWDR_300), value = TRUE)

png(filename=paste0(fld_name, "SWDR_300/rmse_SWDR_300.png"), width = 1280, height = 960)
boxplot(all_stats_SWDR_300[,which(names(all_stats_SWDR_300)%in%names_rmse_SWDR_300)])
dev.off()
png(filename=paste0(fld_name, "SWDR_300/ME_SWDR_300.png"), width = 1280, height = 960)
boxplot(all_stats_SWDR_300[,which(names(all_stats_SWDR_300)%in%names_ME_SWDR_300)])
dev.off()
png(filename=paste0(fld_name, "SWDR_300/MAE_SWDR_300.png"), width = 1280, height = 960)
boxplot(all_stats_SWDR_300[,which(names(all_stats_SWDR_300)%in%names_MAE_SWDR_300)])
dev.off()
all_stats_WD_WS <- lapply(fld_lst, function(j){
  #j <- fld_lst[1]
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results//Kiliman_30km_ERA_Apr_May2014_", 
                       "", j))
  csv_lst <- list.files(j, full.names = TRUE, pattern=".csv$")
  act_stats <- read.table(csv_lst[4], sep = ",", header=TRUE, row.names = "X")
  
  colnames(act_stats) <- paste0(colnames(act_stats),"_", fld_o)
  
  return(act_stats)
})

all_stats_WD_WS <- do.call("cbind", all_stats_WD_WS)
head(all_stats_WD_WS)
names_rmse_WD <- grep("rmse_WD", names(all_stats_WD_WS), value = TRUE)
names_ME_WD <- grep("ME_WD", names(all_stats_WD_WS), value = TRUE)
names_MAE_WD <- grep("MAE_WD", names(all_stats_WD_WS), value = TRUE)
names_rmse_WS <- grep("rmse_WS", names(all_stats_WD_WS), value = TRUE)
names_ME_WS <- grep("ME_WS", names(all_stats_WD_WS), value = TRUE)
names_MAE_WS <- grep("MAE_WS", names(all_stats_WD_WS), value = TRUE)

png(filename=paste0(fld_name, "WD/rmse_WD.png"), width = 1280, height = 960)
boxplot(all_stats_WD_WS[,which(names(all_stats_WD_WS)%in%names_rmse_WD)])
dev.off()
png(filename=paste0(fld_name, "WD/ME_WD.png"), width = 1280, height = 960)
boxplot(all_stats_WD_WS[,which(names(all_stats_WD_WS)%in%names_ME_WD)])
dev.off()
png(filename=paste0(fld_name, "WD/MAE_WD.png"), width = 1280, height = 960)
boxplot(all_stats_WD_WS[,which(names(all_stats_WD_WS)%in%names_MAE_WD)])
dev.off()
png(filename=paste0(fld_name, "WS/rmse_WS.png"), width = 1280, height = 960)
boxplot(all_stats_WD_WS[,which(names(all_stats_WD_WS)%in%names_rmse_WS)])
dev.off()
png(filename=paste0(fld_name, "WS/ME_WS.png"), width = 1280, height = 960)
boxplot(all_stats_WD_WS[,which(names(all_stats_WD_WS)%in%names_ME_WS)])
dev.off()
png(filename=paste0(fld_name, "WS/MAE_WS.png"), width = 1280, height = 960)
boxplot(all_stats_WD_WS[,which(names(all_stats_WD_WS)%in%names_MAE_WS)])
dev.off()