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
fld_lst_model <- fld_lst_model[2:4]
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

eman60 <- lst_models[[1]]
eman80 <- lst_models[[2]]
grellfc <- lst_models[[3]]


ext_model <- extent(eman60)
ext_CHIRPS <- extent(prc_CHIRPS)  
ext_analysis <- ext_model
ext_analysis@xmin <- max(c(ext_model@xmin, ext_CHIRPS@xmin))
ext_analysis@xmax <- min(c(ext_model@xmax, ext_CHIRPS@xmax))
ext_analysis@ymin <- max(c(ext_model@ymin, ext_CHIRPS@ymin))
ext_analysis@ymax <- min(c(ext_model@ymax, ext_CHIRPS@ymax))


prc_CHIRPS <- crop(prc_CHIRPS, ext_analysis)
eman60_daily <- crop(eman60, ext_analysis)
eman80_daily <- crop(eman80, ext_analysis)
grellfc_daily <- crop(grellfc, ext_analysis)
prc_CHIRPS <- resample(prc_CHIRPS, eman60_daily, method="bilinear")

prc_CHIRPS <- crp_raster(prc_CHIRPS, window_size = 23)
eman60_daily <- crp_raster(eman60_daily, window_size = 23)
eman80_daily <- crp_raster(eman60_daily, window_size = 23)
grellfc_daily <- crp_raster(grellfc_daily, window_size = 23)

netcdf_topo <- read_modeloutput(temp, variable = "topo" )
netcdf_topo_crp <- crp_raster(netcdf_topo, window_size = 23)



eman60_var_ar <- as.array(mean((eman60_daily- prc_CHIRPS)))
eman60_mat <- matrix(nrow=nrow(eman60_daily), ncol=ncol(eman60_daily))

topo_crp_ar <- as.array(netcdf_topo_crp)
topo_crp_mat <- matrix(nrow=nrow(netcdf_topo_crp), ncol=ncol(netcdf_topo_crp))

grellfc_var_ar <- as.array(mean((grellfc_daily- prc_CHIRPS)))
grellfc_mat <- matrix(nrow=nrow(grellfc_daily), ncol=ncol(grellfc_daily))

eman80_var_ar <- as.array(mean((eman80_daily- prc_CHIRPS)))
eman80_mat <- matrix(nrow=nrow(eman80_daily), ncol=ncol(eman80_daily))



for(i in seq(1,nrow(netcdf_topo_crp))){
  for (j in seq(1,ncol(netcdf_topo_crp))){
    eman80_mat[i,j] <- eman80_var_ar[i,j,1]
    eman60_mat[i,j] <- eman60_var_ar[i,j,1]
    grellfc_mat[i,j] <- grellfc_var_ar[i,j,1]
    topo_crp_mat[i,j] <- topo_crp_ar[i,j,1]
  }
}

x <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,1]))))
y <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,2]))))
z <- topo_crp_mat
w_eman <- eman60_mat
w_grellfc<- grellfc_mat
w_eman80 <- eman80_mat
#plot(raster(w_eman), col = color)
#oma=c(0,0,0,0)
par(mfrow=c(1,1))
mar.default <- c(5,4,4,2) + 0.1
#par(mar.default)
par(mar = mar.default + c(0, 0, 0, 8)) 
#?par



#########################################################################
# 1.Plot
png(filename="/home/dogbert/Desktop/GrellFC_CHIRPS.png", 
    units="cm", 
    width=30, 
    height=20, 
    res=150)



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

title("a)", adj=0)

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

#labels <- as.character(paste0("Range of GRELLFC-CHIRPS: ", 
                              #round(range(zfacet, na.rm=TRUE)[1],2), " to ", 
                              #round(range(zfacet, na.rm=TRUE)[2],2)))
#label.pos <- trans3d((max.x)+1, max.y-0.5, min.z, pmat)
#text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.8)



image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=color)


dev.off()










###########################################################################################################################
# 2. Plot
png(filename="/home/dogbert/Desktop/EMAN_CHIRPS.png", 
    units="cm", 
    width=30, 
    height=20, 
    res=150)


zfacet <- w_eman
#zfacet <- w_eman[-1, -1] + w_eman[-1, -ncz] + w_eman[-nrz, -1] + w_eman[-nrz, -ncz]
#zfacet[zfacet>25] <- NA
#zfacet[zfacet<-25] <- NA

facetcol <- cut(zfacet, color_sequence)
par(xlog=xlg,ylog=ylg)
range(zfacet, na.rm=TRUE)
pmat <- persp(x = x, y = y, z = z, exp=0.15,phi=30,theta = 130, col = color[facetcol], box = FALSE)

#image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=(color))
title("b)", adj=0)

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

#labels <- as.character(paste0("Range of EMAN-CHIRPS: ", 
#                             round(range(zfacet, na.rm=TRUE)[1],2), " to ", 
#                              round(range(zfacet, na.rm=TRUE)[2],2)))
#label.pos <- trans3d((max.x)+1, max.y-0.5, min.z, pmat)
#text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.8)



image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=color)

dev.off()

###########################################################################################################################
# 3. Plot
zfacet <- w_eman80
#zfacet <- w_eman[-1, -1] + w_eman[-1, -ncz] + w_eman[-nrz, -1] + w_eman[-nrz, -ncz]
#zfacet[zfacet>25] <- NA
#zfacet[zfacet<-25] <- NA

facetcol <- cut(zfacet, color_sequence)
par(xlog=xlg,ylog=ylg)
range(zfacet, na.rm=TRUE)
pmat <- persp(x = x, y = y, z = z, exp=0.15,phi=30,theta = 130, col = color[facetcol], box = FALSE)

#image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=(color))
title("EMAN80 - CHIRPS")

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



#################################################################################################################################

################################### COMPARE CHIRPS TO WHOLE YEAR ################################################

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_model <- paste0(filebase_path, "raster/2014_complete")
filebase_raster_CHIRPS <- paste0(filebase_path,"CHIRPS_2014_daily/2014")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
filebase_results <- paste0(filebase_path, "results/CHIRPS_corrected/")

source(paste0(filebase_code,"analyse_fun.R"))



# read and write first/second half -> clean data from -9999 vals

# fld_lst <- list.files(filebase_raster_CHIRPS, full.names = TRUE)
# #fld_lst <- c(fld_lst, list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = c("2014_05")))
# 
# prc_CHIRPS <- stack(fld_lst)
# 
# 
# for(i in seq(1,length(names(prc_CHIRPS)))){
#   print(i)
#   prc_vals <- values(prc_CHIRPS[[i]])
#   prc_vals <- replace(prc_vals, prc_vals==-9999, NA)
#   values(prc_CHIRPS[[i]]) <- prc_vals
#   prc_vals <- NULL
#   gc()
#   writeRaster(prc_CHIRPS[[i]], filename = paste0(filebase_results,"prc_CHIRPS_fh_", i,".tif"))
# }

fld_lst <- list.files(filebase_results, full.names = TRUE)
#fld_lst <- c(fld_lst, list.files(filebase_raster_CHIRPS, full.names = TRUE, pattern = c("2014_05")))
prc_CHIRPS <- stack(fld_lst)

real_mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
mth_lst <- c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")
pr <- stack()
for (i in seq(1,length(real_mth))){
  #i <-1
  print(mth_lst[i])
  print("Changing Month...")
  i <- match(real_mth[i], mth_lst)
  act_mth <- mth_lst[i]
  print(act_mth)
  act_fld <- paste0("Kiliman_20km_ERA_",act_mth ,"2014_GrellFC01")
  act_fld_full <- paste0(filebase_model,"/",act_fld,"/output/")
  lst_fls <- list.files(act_fld_full, full.names = TRUE)
  act_pr <- read_modeloutput(lst_fls[2], variable = "pr")
  values(act_pr) <- values(act_pr)*3600
  
  
  pr <- stack(pr,act_pr)
  
}

ext_model <- extent(pr)
ext_CHIRPS <- extent(prc_CHIRPS)  
ext_analysis <- ext_model
ext_analysis@xmin <- max(c(ext_model@xmin, ext_CHIRPS@xmin))
ext_analysis@xmax <- min(c(ext_model@xmax, ext_CHIRPS@xmax))
ext_analysis@ymin <- max(c(ext_model@ymin, ext_CHIRPS@ymin))
ext_analysis@ymax <- min(c(ext_model@ymax, ext_CHIRPS@ymax))

prc_CHIRPS <- crop(prc_CHIRPS, ext_analysis)


pr_daily <- crop(pr, ext_analysis)


prc_CHIRPS <- resample(prc_CHIRPS, pr_daily, method="bilinear")
pr_crp <- crp_raster(pr_daily, window_size = 5)

ext_crp <- extent(pr_crp)

#pr_CHIRPS_crp <- crop(prc_CHIRPS, ext_crp)
#prc_CHIRPS <- crp_raster(prc_CHIRPS, window_size = 5)
plot(pr_CHIRPS_crp[[1]])

netcdf_topo <- read_modeloutput(lst_fls[2], variable = "topo" )
netcdf_topo_crp <- crp_raster(netcdf_topo, window_size = 5)

pr_daily <- stackApply(pr_crp, substr(names(pr), 1,11), fun=sum)

# Values of CHIRPS against Model Prc
# prc_CHIRPS <- prc_CHIRPS[[1:364]]
# names(prc_CHIRPS) <- names(pr_daily)
# sum(values(pr_daily[[1:31]])) 
# plot(prc_CHIRPS[[62]])
# sum(values(prc_CHIRPS[[92:122]]))
# plot(pr_CHIRPS_crp[[208:213]])
#   

#plot(pr_daily[[151]])
#plot(prc_CHIRPS[[151]])


pr_var_ar <- as.array((mean((pr_daily- prc_CHIRPS[[1:364]]))))
#RMSE <- sqrt(colMeans((pred_vals - obs_vals)^2, na.rm = TRUE))
pr_mat <- matrix(nrow=nrow(pr_daily), ncol=ncol(pr_daily))

topo_crp_ar <- as.array(netcdf_topo_crp)
topo_crp_mat <- matrix(nrow=nrow(netcdf_topo_crp), ncol=ncol(netcdf_topo_crp))


for(i in seq(1,nrow(netcdf_topo_crp))){
  for (j in seq(1,ncol(netcdf_topo_crp))){
    
    pr_mat[i,j] <- pr_var_ar[i,j,1]
    topo_crp_mat[i,j] <- topo_crp_ar[i,j,1]
  }
}

x <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,1]))))
y <- unique(sort(c(t(coordinates(netcdf_topo_crp)[,2]))))
z <- topo_crp_mat
w_pr <- pr_mat

#plot(raster(w_eman), col = color)
#
par(mfrow=c(1,1))
mar.default <- c(5,4,4,2) + 0.1
#par(mar.default)
par(mar = mar.default + c(4, 4, 4, 8), oma=c(1,1,1,1)) 
#?par

#########################################################################
# 4.Plot
png(filename="/home/dogbert/Desktop/GrellFC_CHIRPS_research_area.png", 
    units="px", 
    width=1024, 
    height=768, 
    res=150)



w_grellfc<- w_pr

nb.col <- 100
xlg=TRUE
ylg=TRUE
nrz <- nrow(z)
ncz <- ncol(z) 
color <- colorRampPalette((brewer.pal(9,"RdBu")))(nb.col)
max_abolute_value= 5
#max(abs(c(cellStats(mean(grellfc_daily- prc_CHIRPS), min), cellStats(mean(grellfc_daily- prc_CHIRPS), max)))) #what is the maximum absolute value of raster?
color_sequence=seq(-max_abolute_value,max_abolute_value,length.out=nb.col+1)
zfacet <- w_grellfc
#zfacet <- w_grellfc[-1, -1] + w_grellfc[-1, -ncz] + w_grellfc[-nrz, -1] + w_grellfc[-nrz, -ncz]
facetcol <- cut(zfacet, color_sequence)
#par(xlog=xlg,ylog=ylg)



pmat <- persp(x = x, y = y, z = z, exp=0.30,phi=30,theta = 130, col = color[facetcol], box = FALSE)
#plot(mar.default)

#title("a)", adj=0)

min.x  <- round(min(x),1)
max.x  <- round(max(x),1)
x.axis <- seq(min.x, max.x,by=1) # by = 2 will get you 5 ticks
min.y  <- round(min(y),1)
max.y  <- round(max(y),1)
y.axis <- seq(min.y, max.y,  by = 1) # by = 5 will get you 2 ticks
min.z  <- round(min(z))
max.z  <- round(max(z))
z.axis <- seq(min.z, max.z, by=1000) # by = 5 will get you 7 ticks 

lines(trans3d(c(min(x),x.axis,max(x)), max.y, min.z, pmat) , col="black", lwd=1.5)

lines(trans3d(max(x), c(y.axis, max(y)), min.z, pmat) , col="black", lwd=1.5)
lines(trans3d(min.x, max.y, z.axis, pmat) , col="black", lwd=1.5)

# lat lon ticks
tick.start <- trans3d(x.axis, max.y, min.z, pmat)
tick.end   <- trans3d(x.axis, (max.y + 0.10), min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

#Note the (min.y - 0.20) in the calculation of tick.end. This places the second line, parallel to the X axis, at the position -0.20 on the Y axis (i.e., into negative/unplotted space).

#The tick marks on the Y and Z axes can be handled similarly:

tick.start <- trans3d(max.x - 0.05, y.axis, min.z, pmat)
tick.end   <- trans3d(max.x + 0.05, y.axis, min.z, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)


# z axis ticks
tick.start <- trans3d(min.x, max.y, z.axis, pmat)
tick.end <- trans3d((min.x-0.1), (max.y + 0.1), z.axis, pmat)
segments(tick.start$x, tick.start$y[1:8], tick.end$x[1:8], tick.end$y[1:8])





labels <- as.character(rev(y.axis))
label.pos <- trans3d(x.axis, (max.y + 0.13), min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt=0, cex=0.6)

labels <- as.character(("Latitude"))
label.pos <- trans3d(x.axis+0.5, (max.y + 0.15), min.z, pmat)
text(label.pos$x[2], label.pos$y[2], labels=labels, adj=c(0, NA), cex=0.8, srt = 50)

#The labels on the Y and Z axes are produced similarly:
labels <- as.character((x.axis))
label.pos <- trans3d((max.x + 0.15), y.axis, min.z, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.6)


labels <- as.character(("Longitude"))
label.pos <- trans3d((max.x + 0.1), y.axis-0.5, min.z, pmat)
text(label.pos$x[3], label.pos$y[3], labels=labels, adj=c(0, NA), cex=0.8, srt = 325)




labels <- as.character(round(z.axis,-3))
label.pos <- trans3d((min.x-0.22), (max.y + 0.08), z.axis, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex=0.6, srt= 2.5)

labels <- as.character(("m a.s.l."))
label.pos <- trans3d((min.x + 0.1), max.y -0.05, max.z+400, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.8, srt = 0)






# 
# labels <- as.character(paste0("Range of GRELLFC-CHIRPS: ", 
# round(range(zfacet, na.rm=TRUE)[1],2), " to ", 
# round(range(zfacet, na.rm=TRUE)[2],2)))
# label.pos <- trans3d((max.x)+1, max.y-0.5, min.z, pmat)
# text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=0.8)
# 


image.plot(legend.only=T, zlim=range(color_sequence, na.rm=TRUE), col=color)


dev.off()









