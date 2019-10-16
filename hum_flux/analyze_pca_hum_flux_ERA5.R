# Analyze/Plot PCA of HumFlux

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
#install.packages( "sfsmisc")
library(hydroGOF)
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
#install.packages("emdbook")
library(RColorBrewer)
library(sfsmisc)
library(gridExtra)
display.brewer.all()

filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))



base_path <- "/data/KILI/data/Copernicus_Download/"
months_num <- c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c("2013","2014","2015")



# read vector data
cntry <- readOGR(dsn = paste0(filebase_shp,"/world_boarders/TM_WORLD_BORDERS-0.3.shp"), layer = "TM_WORLD_BORDERS-0.3", stringsAsFactors = TRUE)
turkana <- readOGR(dsn = paste0(filebase_shp,"/Turkana_Channel/turkana_channel.shp"), layer = "turkana_channel", stringsAsFactors = TRUE)



hum_pca <- stack()


year_lst <- c("2013","2014","2015")
act_mon <- c("01","02","03","04","05","06","07","08","09","10","11","12")
for (k in seq(1,3)){
    pc <- as.character(k)
    for (j in seq(1,3)){
      year <- year_lst[j]
  for (i in seq(1,12)){
    act_hum_pca <- brick(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/",year,"/",act_mon[i],"/output_",year,"_pressure_mon_",act_mon[i],".nc"), varname="sp")
    #/media/benjamin/XChange/Masterarbeit/Copernicus_Download/PCA/PCA_PC1_2013_mon1.tif
    # the hum flux has to be turned because it is saved with 90deg turn                  
    act_hum_pca <- act_hum_pca[[1]]
    values(act_hum_pca) <- values(raster(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/PCA/PCA_PC",k,"_",year,"_mon",as.character(i),".tif")))
    
    act_hum_pca <- act_hum_pca+(min(values(act_hum_pca))*-1)
    
    hum_pca <- stack(hum_pca,act_hum_pca)
  
  
  }
  }
}
#hum_pca[hum_pca < 0] <- 0

plot(hum_pca[[12]])


yat = round(seq(extent(hum_pca[[2]])@ymin, 
          extent(hum_pca[[2]])@ymax, length.out = 3),2)
yat[1] <- -27.7

xat = round(seq(extent(hum_pca[[2]])@xmin, 
          extent(hum_pca[[2]])@xmax, length.out = 3)/.05)*.05
breaks <- c(0,lseq(0.00001,0.0002, 5))



months <-rep(c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),3)

names(hum_pca) <- substr(names(hum_pca),1,8)

expl_var_pca <- c("37%","28%","25%","39%","53%","76%","76%","72%","58%","36%","24%","25%")

expl_var_pca2013 <- read.csv("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/out_expl_var2013.txt", header = FALSE, sep= " ")

expl_var_pca2014 <- read.csv("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/out_expl_var2014.txt", header = FALSE, sep= " ")

expl_var_pca2015 <- read.csv("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/out_expl_var2015.txt", header = FALSE, sep= " ")


expl_var_pca<- rbind(expl_var_pca2013,expl_var_pca2014,expl_var_pca2015)

expl_var_pca$V4 <- as.character(round(expl_var_pca$V1*100))

expl_var_pca$V5 <- as.character(round(expl_var_pca$V2*100))

expl_var_pca$year <- c(rep(2013,12),rep(2014,12),rep(2015,12))

expl_var_pca$month <- months
expl_var_pca$V6 <- as.character(round(expl_var_pca$V3*100))

title <- paste0(months," ",expl_var_pca$V3,"%")


hum_pca2 <- hum_pca
hum_pca2[hum_pca2>9] <- 9
names(hum_pca2) <- paste0(rep(months,3)," ",c(expl_var_pca$V4,expl_var_pca$V5,expl_var_pca$V6),"%")

#hum_pca2 <- (hum_pca2*-1)+max(hum_pca2)

#hum_pca2 <- hum_pca2-6
#hum_pca2[hum_pca2<0] <- 0

hum_pca2[hum_pca2>3 && hum_pca2<7] <- hum_pca2[hum_pca2>3 && hum_pca2<7]+1
val_pca <- values(hum_pca2[[4]])

val_pca[val_pca>1] <-  val_pca[val_pca>1] + 2

hum_pca3 <-  hum_pca 


for (i in seq(1,12)){
  val_pca <- values(stack(hum_pca3[[i]],hum_pca3[[i+12]],hum_pca3[[i+24]]))
  val_act <- values(stack(hum_pca3[[i]]))
  val_pca <- val_act/max(val_pca)
  values(hum_pca3[[i]]) <- val_pca
  
}



png(filename="/home/benjamin/Desktop/Fig_12_scaled.png", 
    units="cm", 
    width=30, 
    height=15, 
    res=600)

spplot(hum_pca2[[1:72]], col.regions=colorRampPalette((brewer.pal(9, "YlGnBu")))(512), layout = c(12,6),
      #at = breaks,      
      #colorkey=list(labels = as.character(breaks)),
       panel = function(...){
         panel.levelplot(...)
         sp.polygons(cntry, cex=0.1, lty=3, col="gray60")
         #sp.polygons(turkana, cex=0.4, lty=2, col="red")
       }, 
       scales = list(x = list(at = xat, labels = paste0(as.character(xat)," E")),
                     y = list(at = yat, labels = c(paste0(as.character(abs(yat[1:2]))," S"),paste0(as.character(yat[3])," N"))))
      )


dev.off()
par(mfrow=c(1,12))
grid.arrange(,nrow=2,ncol=6)
layout(matrix(1:3, 1, 3), widths = c(4,4,1))
spplot(hum_pca2,layout = c(12,3))


writeRaster(rr,"/home/benjamin/Masterarbeit/test.tif")


library(rasterVis)
library(gridExtra)

# load sample raster
f <- system.file("external/test.grd", package="raster")
r <- raster(f)

my_theme <- rasterTheme(region = blues9)

# create plots
for (k in seq(1,48)){
k = 1
actplot <-  spplot(hum_pca[[k]]) 
  
p1 <- levelplot(r, xlab=NULL, ylab=NULL, margin=FALSE, par.settings = my_theme)
leg <- p1$legend$right$args$key
p1$legend <- list()
p2 <- levelplot(r*2, xlab=NULL, ylab=NULL, margin=FALSE,colorkey=FALSE, par.settings = my_theme)

# put plots in list
p.list <- list(p1,p2,p2,p2,p2,p2,p2)

# create layout
lay <- rbind(c(1,2,3,4,5,6,7,8,9,10,11,12),
             c(13,14,15,16,17,18,19,20,21,22,23,24),
             c(25,26,27,28,29,30,31,32,33,34,35,36))

leg$col <- my_theme$regions$col
legGrob <- draw.colorkey(key = leg, vp = grid::viewport(height=0.5))
# arrange plots
grid.arrange(grobs=c(p.list, list(legGrob)), layout_matrix=lay,
             vp = grid::viewport(width=0.7,height=1))



