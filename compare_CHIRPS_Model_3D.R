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
library(fields)

#####################################################


filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_model <- paste0(filebase_path, "raster")
filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS_2014_daily/2014")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
filebase_results <- paste0(filebase_path, "results/CHIRPS_vs_model")

source(paste0(filebase_code,"analyse_fun.R"))



fld_lst_model <- list.files(filebase_model, full.names = TRUE, pattern="20")
res <- unique(na.omit(as.numeric(unlist(strsplit(fld_lst_model, "[^0-9]+")))))[1]

fld_lst <- list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = c("2014_04"))
fld_lst <- c(fld_lst, list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = c("2014_05")))

prc_CHIRPS <- stack(fld_lst[seq(15,45)])

vals_prc <- values(prc_CHIRPS) 
vals_prc <- replace(vals_prc, vals_prc==-9999, NA)
values(prc_CHIRPS) <- vals_prc

temp <-paste0(fld_lst_model,"/Kiliman_",res,"km_Apr_May2014_SRF.2014041500.nc")

lst_models <- lapply(fld_lst_model, function(i){
  #i <- fld_lst_model[2]
  print(i)
  
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                       "", i),"/")
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014041500.nc")
  netcdf_topo <- read_modeloutput(filepath = temp, variable = "topo")
  netcdf_prc2 <- read_modeloutput(temp, "prc")
  temp <- paste0(i,"/Kiliman_",res,"km_Apr_May2014_SRF.2014050100.nc")
  netcdf_prc1 <- read_modeloutput(filepath = temp, variable = "prc")
  netcdf_prc <- stack(netcdf_prc2, netcdf_prc1)
  values(netcdf_prc) <- values(netcdf_prc) *3600
  netcdf_prc_daily <- stackApply(netcdf_prc, substr(names(netcdf_prc), 1,11), fun=sum)

  return(netcdf_prc_daily)
  })

names(lst_models) <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/", 
                                 "", fld_lst_model))  

eman <- lst_models[[1]]
grellfc <- lst_models[[2]]


ext_model <- extent(eman)
ext_CHIRPS <- extent(prc_CHIRPS)  
ext_analysis <- ext_model
ext_analysis@xmin <- max(c(ext_model@xmin, ext_CHIRPS@xmin))
ext_analysis@xmax <- min(c(ext_model@xmax, ext_CHIRPS@xmax))
ext_analysis@ymin <- max(c(ext_model@ymin, ext_CHIRPS@ymin))
ext_analysis@ymax <- min(c(ext_model@ymax, ext_CHIRPS@ymax))


prc_CHIRPS <- crop(prc_CHIRPS, ext_analysis)
eman_daily <- crop(eman, ext_analysis)
grellfc_daily <- crop(grellfc, ext_analysis)
prc_CHIRPS <- resample(prc_CHIRPS, eman_daily, method="bilinear")

prc_CHIRPS <- crp_raster(prc_CHIRPS, window_size = 23)
eman_daily <- crp_raster(eman_daily, window_size = 23)
grellfc_daily <- crp_raster(grellfc_daily, window_size = 23)

netcdf_topo <- read_modeloutput(temp, variable = "topo" )
netcdf_topo_crp <- crp_raster(netcdf_topo, window_size = 23)



eman_var_ar <- as.array(mean((eman_daily- prc_CHIRPS)))
eman_mat <- matrix(nrow=nrow(eman_daily), ncol=ncol(eman_daily))
topo_crp_ar <- as.array(netcdf_topo_crp)
topo_crp_mat <- matrix(nrow=nrow(netcdf_topo_crp), ncol=ncol(netcdf_topo_crp))

grellfc_var_ar <- as.array(mean((grellfc_daily- prc_CHIRPS)))
grellfc_mat <- matrix(nrow=nrow(grellfc_daily), ncol=ncol(grellfc_daily))



for(i in seq(1,nrow(netcdf_topo_crp))){
  for (j in seq(1,ncol(netcdf_topo_crp))){
    eman_mat[i,j] <- eman_var_ar[i,j,1]
    grellfc_mat[i,j] <- grellfc_var_ar[i,j,1]
    topo_crp_mat[i,j] <- topo_crp_ar[i,j,1]
  }
}

x <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,1]))))
y <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,2]))))
z <- topo_crp_mat
w_eman <- eman_mat
w_grellfc<- grellfc_mat
#plot(raster(w_eman), col = color)
#oma=c(0,0,0,0)
par(mfrow=c(1,1))
mar.default <- c(5,4,4,2) + 0.1
#par(mar.default)
par(mar = mar.default + c(0, 0, 0, 4)) 
#?par

#########################################################################
# 1.Plot
w_grellfc<- grellfc_mat

nb.col <- 100
xlg=TRUE
ylg=TRUE
nrz <- nrow(z)
ncz <- ncol(z) 
color <- colorRampPalette((brewer.pal(9,"RdBu")))(nb.col)
max_abolute_value= 25
#max(abs(c(cellStats(mean(grellfc_daily- prc_CHIRPS), min), cellStats(mean(grellfc_daily- prc_CHIRPS), max)))) #what is the maximum absolute value of raster?
color_sequence=seq(-max_abolute_value,max_abolute_value,length.out=nb.col+1)
zfacet <- w_grellfc
#zfacet <- w_grellfc[-1, -1] + w_grellfc[-1, -ncz] + w_grellfc[-nrz, -1] + w_grellfc[-nrz, -ncz]
facetcol <- cut(zfacet, color_sequence)
#par(xlog=xlg,ylog=ylg)



pmat <- persp(x = x, y = y, z = z, exp=0.15,phi=30,theta = 130, col = color[facetcol], box = FALSE)
#plot(mar.default)

title("GRELLFC - CHIRPS")

min.x  <- round(min(x),1)
max.x  <- round(max(x),1)
x.axis <- seq(min.x, max.x,by=2) # by = 2 will get you 5 ticks
min.y  <- round(min(y),1)
max.y  <- round(max(y),1)
y.axis <- seq(min.y, max.y,  by = 2) # by = 5 will get you 2 ticks
min.z  <- round(min(z))
max.z  <- round(max(z))
z.axis <- seq(min.z, max.z, by=1000) # by = 5 will get you 7 ticks 

lines(trans3d(c(x.axis,max(x)), max.y, min.z, pmat) , col="black", lwd=1.5)
lines(trans3d(max(x), c(y.axis, max(y)), min.z, pmat) , col="black", lwd=1.5)
lines(trans3d(min.x, max.y, z.axis, pmat) , col="black", lwd=1.5)


tick.start <- trans3d(x.axis, max.y, min.z, pmat)
tick.end   <- trans3d(x.axis, (max.y + 0.20), min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

#Note the (min.y - 0.20) in the calculation of tick.end. This places the second line, parallel to the X axis, at the position -0.20 on the Y axis (i.e., into negative/unplotted space).

#The tick marks on the Y and Z axes can be handled similarly:

tick.start <- trans3d(max.x, y.axis, min.z, pmat)
tick.end   <- trans3d(max.x + 0.20, y.axis, min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

tick.start <- trans3d(min.x, max.y, z.axis, pmat)
tick.end <- trans3d((min.x-0.20), (max.y + 0.20), z.axis, pmat)
segments(tick.start$x, tick.start$y[1:8], tick.end$x[1:8], tick.end$y[1:8])

labels <- as.character(rev(y.axis))
label.pos <- trans3d(x.axis, (max.y + 0.25), min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt=0, cex=0.6)

labels <- as.character(("Latitude"))
label.pos <- trans3d(x.axis+1.5, (max.y + 0.65), min.z, pmat)
text(label.pos$x[3], label.pos$y[3], labels=labels, adj=c(0, NA), cex=0.8, srt = 55)
#The labels on the Y and Z axes are produced similarly:
labels <- as.character((x.axis))
label.pos <- trans3d((max.x + 0.45), y.axis, min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.6)


labels <- as.character(("Longitude"))
label.pos <- trans3d((max.x + 0.65), y.axis-0.5, min.z, pmat)
text(label.pos$x[3], label.pos$y[3], labels=labels, adj=c(0, NA), cex=0.8, srt = 320)

labels <- as.character(z.axis)
label.pos <- trans3d((min.x-0.35), (max.y + 0.35), z.axis+550, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex=0.6, srt= 2.5)

labels <- as.character(paste0("Range of GRELLFC-CHIRPS: ", 
                              round(range(zfacet, na.rm=TRUE)[1],2), " to ", 
                              round(range(zfacet, na.rm=TRUE)[2],2)))
label.pos <- trans3d((max.x)+1, max.y-0.5, min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.8)



image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=color)













###########################################################################################################################
# 2. Plot
zfacet <- w_eman
#zfacet <- w_eman[-1, -1] + w_eman[-1, -ncz] + w_eman[-nrz, -1] + w_eman[-nrz, -ncz]
#zfacet[zfacet>25] <- NA
#zfacet[zfacet<-25] <- NA

facetcol <- cut(zfacet, color_sequence)
par(xlog=xlg,ylog=ylg)
range(zfacet, na.rm=TRUE)
pmat <- persp(x = x, y = y, z = z, exp=0.15,phi=30,theta = 130, col = color[facetcol], box = FALSE)

#image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=(color))
title("EMAN - CHIRPS")

min.x  <- round(min(x),1)
max.x  <- round(max(x),1)
x.axis <- seq(min.x, max.x,by=2) # by = 2 will get you 5 ticks
min.y  <- round(min(y),1)
max.y  <- round(max(y),1)
y.axis <- seq(min.y, max.y,  by = 2) # by = 5 will get you 2 ticks
min.z  <- round(min(z))
max.z  <- round(max(z))
z.axis <- seq(min.z, max.z, by=1000) # by = 5 will get you 7 ticks 

lines(trans3d(c(x.axis,max(x)), max.y, min.z, pmat) , col="black", lwd=1.5)
lines(trans3d(max(x), c(y.axis, max(y)), min.z, pmat) , col="black", lwd=1.5)
lines(trans3d(min.x, max.y, z.axis, pmat) , col="black", lwd=1.5)


tick.start <- trans3d(x.axis, max.y, min.z, pmat)
tick.end   <- trans3d(x.axis, (max.y + 0.20), min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

#Note the (min.y - 0.20) in the calculation of tick.end. This places the second line, parallel to the X axis, at the position -0.20 on the Y axis (i.e., into negative/unplotted space).

#The tick marks on the Y and Z axes can be handled similarly:

tick.start <- trans3d(max.x, y.axis, min.z, pmat)
tick.end   <- trans3d(max.x + 0.20, y.axis, min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

tick.start <- trans3d(min.x, max.y, z.axis, pmat)
tick.end <- trans3d((min.x-0.20), (max.y + 0.20), z.axis, pmat)
segments(tick.start$x, tick.start$y[1:8], tick.end$x[1:8], tick.end$y[1:8])

labels <- as.character(rev(y.axis))
label.pos <- trans3d(x.axis, (max.y + 0.25), min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt=0, cex=0.6)

labels <- as.character(("Latitude"))
label.pos <- trans3d(x.axis+1.5, (max.y + 0.65), min.z, pmat)
text(label.pos$x[3], label.pos$y[3], labels=labels, adj=c(0, NA), cex=0.8, srt = 55)

#The labels on the Y and Z axes are produced similarly:
labels <- as.character((x.axis))
label.pos <- trans3d((max.x + 0.45), y.axis, min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.6)

labels <- as.character(("Longitude"))
label.pos <- trans3d((max.x + 0.65), y.axis-0.5, min.z, pmat)
text(label.pos$x[3], label.pos$y[3], labels=labels, adj=c(0, NA), cex=0.8, srt = 320)

labels <- as.character(z.axis)
label.pos <- trans3d((min.x-0.35), (max.y + 0.35), z.axis+550, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex=0.6, srt= 2.5)

labels <- as.character(paste0("Range of EMAN-CHIRPS: ", 
                              round(range(zfacet, na.rm=TRUE)[1],2), " to ", 
                              round(range(zfacet, na.rm=TRUE)[2],2)))
label.pos <- trans3d((max.x)+1, max.y-0.5, min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.8)



image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=color)




