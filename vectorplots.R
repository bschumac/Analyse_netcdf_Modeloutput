# create Humidity Budget

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
#install.packages( "hydroGOF")
library(hydroGOF)
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
#install.packages("caTools")
library(caTools)
library(RColorBrewer)
library(rasterVis)
library(gridExtra)

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster/")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))


# read vector data
cntry <- readOGR(dsn = paste0(filebase_shp,"/world_boarders/TM_WORLD_BORDERS-0.3.shp"), layer = "TM_WORLD_BORDERS-0.3", stringsAsFactors = TRUE)

kili <- readOGR(dsn=paste0(filebase_shp,"plots_shp/plots_unique.shp"), layer= "plots_unique")                 
kili <- spTransform(kili, CRSobj = crs(cntry))
kili <- kili[1,]

                                               
JF <- c("Jan", "Feb")
MAM <- c("Mar", "Apr", "May")
JJAS <- c("Jun", "Jul", "Aug", "Sep")
OND <- c("Oct", "Nov", "Dec")
seasons <- list(JF,MAM,JJAS,OND)

par(mfrow=c(1,1))


############################################################### Plot for Season JF

  act_seas <- seasons[[1]]
  
  
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[1],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  hum_x_stack <- stack()
  hum_y_stack <- stack()
  for (j in seq(1,length(act_seas))){
    print(j)
    hum_x <- model_base
    hum_y <- model_base

    act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
                      
    values(hum_x) <- values(raster(paste0(act_file,"hum_flux_x_direct.nc")))
    values(hum_y) <- values(raster(paste0(act_file,"hum_flux_y_direct.nc")))
    hum_x_stack <- stack(hum_x_stack,hum_x)
    hum_y_stack <- stack(hum_y_stack,hum_y)
  }                         
                            
                            
topo <- read_modeloutput(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_Apr2014_GrellFC01/output/Kiliman_20km_Apr2014_ATM.2014040100.nc"))

  
hum_jf <- stack(mean(hum_x_stack), mean(hum_y_stack))
values(hum_jf) <- values(hum_jf)/1500


############################################################### Plot for Season MAM


act_seas <- seasons[[2]]


#model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[1],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]

#model_base <- read_modeloutput(model_base_file, variable = "pr")
hum_x_stack <- stack()
hum_y_stack <- stack()
for (j in seq(1,length(act_seas))){
  print(j)
  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(raster(paste0(act_file,"hum_flux_x_direct.nc")))
  values(hum_y) <- values(raster(paste0(act_file,"hum_flux_y_direct.nc")))
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
}                         


topo <- read_modeloutput(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_Apr2014_GrellFC01/output/Kiliman_20km_Apr2014_ATM.2014040100.nc"))




hum_mam <- stack(mean(hum_x_stack), mean(hum_y_stack))
values(hum_mam) <- values(hum_mam)/1500

############################################################### Plot for Season JJAS


act_seas <- seasons[[3]]


#model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[1],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]

#model_base <- read_modeloutput(model_base_file, variable = "pr")
hum_x_stack <- stack()
hum_y_stack <- stack()
for (j in seq(1,length(act_seas))){
  print(j)
  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(raster(paste0(act_file,"hum_flux_x_direct.nc")))
  values(hum_y) <- values(raster(paste0(act_file,"hum_flux_y_direct.nc")))
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
}                         


topo <- read_modeloutput(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_Apr2014_GrellFC01/output/Kiliman_20km_Apr2014_ATM.2014040100.nc"))




hum_jjas <- stack(mean(hum_x_stack), mean(hum_y_stack))
values(hum_jjas) <- values(hum_jjas)/1500


############################################################### Plot for Season OND


act_seas <- seasons[[4]]


#model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[1],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]

#model_base <- read_modeloutput(model_base_file, variable = "pr")
hum_x_stack <- stack()
hum_y_stack <- stack()
for (j in seq(1,length(act_seas))){
  print(j)
  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(raster(paste0(act_file,"hum_flux_x_direct.nc")))
  values(hum_y) <- values(raster(paste0(act_file,"hum_flux_y_direct.nc")))
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
}                         


topo <- read_modeloutput(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_Apr2014_GrellFC01/output/Kiliman_20km_Apr2014_ATM.2014040100.nc"))


  



hum_ond <- stack(mean(hum_x_stack), mean(hum_y_stack))
values(hum_ond) <- values(hum_ond)/1500

########### Plots

my.settings <- rasterTheme(region = brewer.pal("OrRd", n = 9),   
                           par.main.text = list(font = 2, just = "left",x = grid::unit(5, "mm")))


png(filename="/home/dogbert/Desktop/jf_plot.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

jfplot <- (vectorplot(hum_jf, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                      narrows = 1000, at = seq(0,5000, 100), main="a)")  +   layer(sp.polygons(cntry, cex=1)) + layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))
jfplot
dev.off()


png(filename="/home/dogbert/Desktop/mam_plot.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
mamplot <- (vectorplot(hum_mam, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                       narrows = 1000, at = seq(0,5000, 100), main="b)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))
mamplot
dev.off()

png(filename="/home/dogbert/Desktop/jjas_plot.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

jjasplot <- vectorplot(hum_jjas, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                       narrows = 1000, at = seq(0,5000, 100), main="c)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black"))
jjasplot
dev.off()

png(filename="/home/dogbert/Desktop/ond_plot.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

ondplot <- (vectorplot(hum_ond, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
           narrows = 1000, at = seq(0,5000, 100),  main="d)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))
ondplot
dev.off()

### SINGLE VECTOR


hum_total_dec <- model_base


act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")

values(hum_total_dec) <- values(raster(paste0(act_file,"hum_flux_total_Dec.nc"), varname="length"))

round(values(hum_total_dec[[1]]),-1)[9375]

hum_x <- model_base
hum_y <- model_base

act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")

values(hum_x) <- values(raster(paste0(act_file,"hum_flux_x_direct.nc")))
values(hum_y) <- values(raster(paste0(act_file,"hum_flux_y_direct.nc")))

hum_single <- stack(hum_x[[1]],hum_y[[1]])
val_x_sing <- hum_single[[1]][9375]
val_y_sing <- hum_single[[2]][9375]
values(hum_single) <- NA
hum_single[[1]][9375] <- val_x_sing
hum_single[[2]][9375] <- val_y_sing

values(hum_single) <- values(hum_single)/1500

png(filename="/home/dogbert/Desktop/single_vec_plot.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
vectorplot(hum_single, isField = "dXY",  
           narrows = 10000000000, at = seq(0,5000, 100),  main="single vector") 

dev.off()
