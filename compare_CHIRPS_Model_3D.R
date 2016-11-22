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
#installed.packages("rgl")
#install.packages("RColorBrewer")
library(RColorBrewer)
library(rgl)
library(maptools)

#####################################################


filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_model <- paste0(filebase_path, "raster")
filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS_2014_daily/2014")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
filebase_results <- paste0(filebase_path, "results/CHIRPS_vs_model")

source(paste0(filebase_code,"analyse_fun.R"))



fld_lst_model <- list.files(filebase_model, full.names = TRUE, pattern="15")

fld_lst <- list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = c("2014_04"))
fld_lst <- c(fld_lst, list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = c("2014_05")))

prc_CHIRPS <- stack(fld_lst[seq(15,45)])

vals_prc <- values(prc_CHIRPS) 
vals_prc <- replace(vals_prc, vals_prc==-9999, NA)
values(prc_CHIRPS) <- vals_prc

temp <- paste0(fld_lst_model[1],"/Kiliman_15km_Apr_May2014_SRF.2014041500.nc")

lst_models <- lapply(fld_lst_model, function(i){
  #i <- fld_lst_model[2]
  print(i)
  
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                       "", i),"/")
  temp <- paste0(i,"/Kiliman_15km_Apr_May2014_SRF.2014041500.nc")
  netcdf_topo <- read_modeloutput(filepath = temp, variable = "topo")
  netcdf_prc2 <- read_modeloutput(temp, "prc")
  temp <- paste0(i,"/Kiliman_15km_Apr_May2014_SRF.2014050100.nc")
  netcdf_prc1 <- read_modeloutput(filepath = temp, variable = "prc")
  netcdf_prc <- stack(netcdf_prc2, netcdf_prc1)
  values(netcdf_prc) <- values(netcdf_prc) *3600
  netcdf_prc_daily <- stackApply(netcdf_prc, substr(names(netcdf_prc), 1,11), fun=sum)

  return(netcdf_prc_daily)
  })

names(lst_models) <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                                 "", fld_lst_model))  

gul2 <- lst_models[[1]]
gul5 <- lst_models[[2]]
gul9 <- lst_models[[3]]

ext_model <- extent(gul2)
ext_CHIRPS <- extent(prc_CHIRPS)  
ext_analysis <- ext_model
ext_analysis@xmin <- max(c(ext_model@xmin, ext_CHIRPS@xmin))
ext_analysis@xmax <- min(c(ext_model@xmax, ext_CHIRPS@xmax))
ext_analysis@ymin <- max(c(ext_model@ymin, ext_CHIRPS@ymin))
ext_analysis@ymax <- min(c(ext_model@ymax, ext_CHIRPS@ymax))

ext_analysis


prc_CHIRPS <- crop(prc_CHIRPS, ext_analysis)
gul2_daily <- crop(gul2, ext_analysis)
gul5_daily <- crop(gul5, ext_analysis)
gul9_daily <- crop(gul9, ext_analysis)
prc_CHIRPS <- resample(prc_CHIRPS, gul2_daily, method="bilinear")

prc_CHIRPS <- crp_raster(prc_CHIRPS, window_size = 15)
gul2_daily <- crp_raster(gul2_daily, window_size = 15)
gul5_daily <- crp_raster(gul2_daily, window_size = 15)
gul9_daily <- crp_raster(gul2_daily, window_size = 15)
temp <- paste0("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/20km_gul_6_6/Kiliman_20km_Apr_May2014_SRF.2014041500.nc")
temp <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results/DEM/SRTM_20km_incr.nc"
raster(temp)

netcdf_topo <- read_modeloutput(temp, variable = "topo" )
plot(netcdf_topo)
netcdf_topo_crp <- crp_raster(netcdf_topo, window_size = 13)


gul2_daily
gul2_var_ar <- as.array(mean((prc_CHIRPS- gul2_daily)))
gul2_mat <- matrix(nrow=nrow(gul2_daily), ncol=ncol(gul2_daily))
topo_crp_ar <- as.array(netcdf_topo_crp)
topo_crp_mat <- matrix(nrow=nrow(netcdf_topo_crp), ncol=ncol(netcdf_topo_crp))

for(i in seq(1,nrow(netcdf_topo_crp))){
  for (j in seq(1,ncol(netcdf_topo_crp))){
    gul2_mat[i,j] <- gul2_var_ar[i,j,1]
    topo_crp_mat[i,j] <- topo_crp_ar[i,j,1]
  }
}

x <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,1]))))
y <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,2]))))
z <- topo_crp_mat
w <- gul2_mat


nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

nb.col <- 100
xlg=TRUE
ylg=TRUE
nrz <- nrow(z)
ncz <- ncol(z) 
color <- colorRampPalette(rev(brewer.pal(9,"RdBu")))(nb.col)
zfacet <- w[-1, -1] + w[-1, -ncz] + w[-nrz, -1] + w[-nrz, -ncz]
facetcol <- cut(zfacet, nb.col)
par(xlog=xlg,ylog=ylg)

par(mar=c(2.5,2.5,2.5,2.5))

pmat <- persp(x = x, y = y, z = z, exp=0.2,phi=30,theta = 60, col = color[facetcol], box = FALSE)

min.x  <- round(min(x),1)
max.x  <- round(max(x),1)
x.axis <- seq(min.x, max.x,by=1) # by = 2 will get you 5 ticks
min.y  <- round(min(y),1)
max.y  <- round(max(y),1)
y.axis <- seq(min.y, max.y,  by = 1) # by = 5 will get you 2 ticks
min.z  <- round(min(z))
max.z  <- round(max(z))
z.axis <- seq(min.z, max.z, by=400) # by = 5 will get you 7 ticks 

lines(trans3d(c(x.axis,max(x)), min.y, min.z, pmat) , col="black")
lines(trans3d(max.x, y.axis, min.z, pmat) , col="black")
lines(trans3d(min.x, min.y, z.axis, pmat) , col="black")


tick.start <- trans3d(x.axis, min.y, min.z, pmat)
tick.end   <- trans3d(x.axis, (min.y - 0.20), min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

#Note the (min.y - 0.20) in the calculation of tick.end. This places the second line, parallel to the X axis, at the position -0.20 on the Y axis (i.e., into negative/unplotted space).

#The tick marks on the Y and Z axes can be handled similarly:

tick.start <- trans3d(max.x, y.axis, min.z, pmat)
tick.end   <- trans3d(max.x + 0.20, y.axis, min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

tick.start <- trans3d(min.x, min.y, z.axis, pmat)
tick.end <- trans3d(min.x, (min.y - 0.20), z.axis, pmat)
segments(tick.start$x[2:8], tick.start$y[2:8], tick.end$x[2:8], tick.end$y[2:8])

labels <- as.character(y.axis)
label.pos <- trans3d(x.axis, (min.y - 0.25), min.z, pmat)
text(label.pos$x[-3], label.pos$y[-3], labels=labels[-3], adj=c(0, NA), srt=270, cex=0.6)
text(label.pos$x[3], label.pos$y[3],"Longitude", srt=295, pos = 1, offset=0.5, cex=0.9)
#The adj=c(0, NA) expression is used to left-justify the labels, the srt=270 expression is used to rotate the labels 270°, and the cex=0.5 expression is used to scale the label text to 75% of its original size.

#The labels on the Y and Z axes are produced similarly:
labels <- as.character(x.axis)
label.pos <- trans3d((max.x + 0.25), y.axis, min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.5)

labels <- as.character(z.axis)
label.pos <- trans3d(min.x, (min.y - 0.5), z.axis, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex=0.5)


# add Latitude
# make Ticklabels bigger
# plot tick latitude show ticklabel 35.2


persp3d(x = x, y = y, z = z, exp=0.2,phi=45, theta=90, xlab="Longitude", ylab="Latitude", zlab="Elevation", col = color[zcol])


plot(mean((prc_CHIRPS- gul2_daily)), main= "gul2", col= colorRampPalette(rev(brewer.pal(9,"RdBu")))(1000))  
plot(mean((prc_CHIRPS- gul5_daily)), main= "gul5", col= colorRampPalette(rev(brewer.pal(9,"RdBu")))(1000))
plot(mean((prc_CHIRPS- gul9_daily)), main= "gul9", col= colorRampPalette(rev(brewer.pal(9,"RdBu")))(1000))

