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

nguru <- readOGR(dsn = paste0(filebase_shp,"/nguru/EasternArc_byBloc_incPlateaus&LowMatundu_DD.shp"), 
                 layer = "EasternArc_byBloc_incPlateaus&LowMatundu_DD", stringsAsFactors = TRUE)
kenya <- readOGR(dsn = paste0(filebase_shp,"/NationalParks/Kenya/WDPA_Mar2017_search_1d0087e1d160cce68ea3e5e320ce6506de5d4bda642e58dbe8d5466674679bc2-shapefile-polygons.shp"), 
                 layer = "WDPA_Mar2017_search_1d0087e1d160cce68ea3e5e320ce6506de5d4bda642e58dbe8d5466674679bc2-shapefile-polygons", stringsAsFactors = TRUE)
grvalley <- readOGR(dsn = paste0(filebase_shp,"/riftvalley/great_rift_valley_ofb.shp"), 
                    layer = "great_rift_valley_ofb", stringsAsFactors = TRUE)
rivers <- readOGR(dsn = paste0(filebase_shp,"/rivers/ne_10m_rivers_lake_centerlines.shp"), 
                  layer = "ne_10m_rivers_lake_centerlines", stringsAsFactors = TRUE)

rivers <- rivers[rivers$name %in% c("Pangani", "Wami", "Tana"),]

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
    model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
    
    model_base <- read_modeloutput(model_base_file, variable = "pr")
    hum_x <- model_base
    hum_y <- model_base

    act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
    
    # the hum flux has to be turned because it is saved with 90deg turn                  
    values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
    values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
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
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  print(j)
  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  
  values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
  values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
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
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  print(j)
  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
  values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
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
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  print(j)
  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
  values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
}                         


topo <- read_modeloutput(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_Apr2014_GrellFC01/output/Kiliman_20km_Apr2014_ATM.2014040100.nc"))


  



hum_ond <- stack(mean(hum_x_stack), mean(hum_y_stack))
values(hum_ond) <- values(hum_ond)/1500

############################################################## Plots

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
###########################################################################################
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


########################################################################################

# Vectorplot composits IOZM

act_seas <- c(seasons[[2]][3],seasons[[3]])


hum_x_stack <- stack()
hum_y_stack <- stack()
for (j in seq(1,length(act_seas))){
  #j <- 1
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  print(j)

  hum_x <- model_base
  hum_y <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
  values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
  names(hum_x)<- names(model_base)
  names(hum_y)<- names(model_base)
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
}                         

names(hum_x_stack) <- as.POSIXct(names(hum_x_stack), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
names(hum_y_stack) <- as.POSIXct(names(hum_y_stack), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
# Extract time vector
time <- as.POSIXct(names(hum_x_stack), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
# You can get the indices which fit your subset criteria using which() and inject them into raster::subset()
id <- which(time %in% seq(as.POSIXct("2014-06-01", tz="UTC"), as.POSIXct("2014-09-02", tz="UTC"),by= "hour"))
hum_x_aug <- subset(hum_x_stack, id)
hum_y_aug <- subset(hum_y_stack, id)
time <- as.POSIXct(names(hum_x_aug), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
id1 <- which(time %in% seq(as.POSIXct("2014-06-01", tz="UTC"), as.POSIXct("2014-06-02", tz="UTC"),by= "hour"))
id1 <- id1[-length(id1)]
hum_x_aug_1 <- subset(hum_x_aug, id1)
hum_y_aug_1 <- subset(hum_y_aug, id1)
hum_aug_1 <- stack(mean(hum_x_aug_1), mean(hum_y_aug_1))
values(hum_aug_1) <- values(hum_aug_1)/1500

time <- as.POSIXct(names(hum_x_aug), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
id2 <- which(time %in% seq(as.POSIXct("2014-08-06", tz="UTC"), as.POSIXct("2014-08-07", tz="UTC"),by= "hour"))
id2 <- id2[-length(id2)]
hum_x_aug_2 <- subset(hum_x_aug, id2)
hum_y_aug_2 <- subset(hum_y_aug, id2)

hum_aug_2 <- stack(mean(hum_x_aug_2), mean(hum_y_aug_2))
values(hum_aug_2) <- values(hum_aug_2)/1500

time <- as.POSIXct(names(hum_x_aug), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
id3 <- which(time %in% seq(as.POSIXct("2014-08-05", tz="UTC"), as.POSIXct("2014-08-06", tz="UTC"),by= "hour"))
id3 <- id3[-length(id3)]
hum_x_aug_3 <- subset(hum_x_aug, id3)
hum_y_aug_3 <- subset(hum_y_aug, id3)

hum_aug_3 <- stack((hum_x_aug_3[[1]]), (hum_y_aug_3[[1]]))
values(hum_aug_3) <- values(hum_aug_3)/1500

time <- as.POSIXct(names(hum_x_aug), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
id4 <- which(time %in% seq(as.POSIXct("2014-06-08", tz="UTC"), as.POSIXct("2014-06-09", tz="UTC"),by= "hour"))
id4 <- id4[-length(id4)]
hum_x_aug_4 <- subset(hum_x_aug, id4)
hum_y_aug_4 <- subset(hum_y_aug, id4)

hum_aug_4 <- stack(mean(hum_x_aug_4), mean(hum_y_aug_4))
values(hum_aug_4) <- values(hum_aug_4)/1500
################### plots

my.settings <- rasterTheme(region = brewer.pal("OrRd", n = 9),   
                           par.main.text = list(font = 2, just = "left",x = grid::unit(5, "mm")))

png(filename="/home/dogbert/Desktop/IOZM_pos.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)

#1.06.14
aug_1plot <- vectorplot(hum_aug_1, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                       narrows = 1000, at = seq(0,5000, 100), main="a)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black"))
aug_1plot
dev.off()

aug_2plot <- vectorplot(hum_aug_2, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                        narrows = 1000, at = seq(0,5000, 100), main="2.08.14")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black"))
#aug_2plot

aug_3plot <- vectorplot(hum_aug_3, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                        narrows = 1000, at = seq(0,5000, 100), main="5.08.14")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black"))
#aug_3plot

png(filename="/home/dogbert/Desktop/IOZM_neg.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
#8.06.14
aug_4plot <- vectorplot(hum_aug_4, isField = "dXY", region = topo, margin = FALSE, par.settings = my.settings, 
                        narrows = 1000, at = seq(0,5000, 100), main="b)")  + layer(sp.polygons(cntry, cex=2))+ layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black"))
aug_4plot

dev.off()


############### Moisture Flux Apr - Sept Vectorplots Kilimanjaro Region

act_seas <- c(seasons[[2]][2:3],seasons[[3]])


hum_x_stack <- stack()
hum_y_stack <- stack()
hum_len_total <- stack()
for (j in seq(1,length(act_seas))){
  #j <- 1
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", act_seas[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  print(j)
  
  hum_x <- model_base
  hum_y <- model_base
  hum_len <- model_base
  
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",act_seas[j],"2014_GrellFC01/hum_flux/")
  
  values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
  values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
  values(hum_len) <- values(stack(paste0(act_file,paste0("hum_flux_total_",act_seas[j],".nc")), varname="length"))
  names(hum_x)<- names(model_base)
  names(hum_y)<- names(model_base)
  names(hum_len)<- names(model_base)
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
  hum_len_total <- stack(hum_len_total, hum_len)
}                         
hum_x_stack_crp <- crp_raster(hum_x_stack,window_size = 23)
hum_y_stack_crp <- crp_raster(hum_y_stack,window_size = 23)
topo_crp <- crp_raster(hum_len_total[[474]],window_size = 23)
#plot(hum_x_stack_crp[[1]])


names(hum_x_stack_crp) <- as.POSIXct(names(hum_x_stack_crp), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")
names(hum_y_stack_crp) <- as.POSIXct(names(hum_y_stack_crp), format = "X%Y.%m.%d.%H.%M.%S", tz="UTC")



png(filename="/home/dogbert/Desktop/Apr_meso_flux.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
hum_apr_1 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 474, titleboo=TRUE)
dev.off()
###############
#hum_apr_2 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 641, titleboo=TRUE)

############################


#hum_apr_3 <-vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 716, titleboo=TRUE)

################################

#hum_may_1 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 741, titleboo=TRUE)

###############################

#hum_may_2 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 840, titleboo=TRUE)

###############################

#hum_may_3 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 1002, titleboo=TRUE)

###############################
png(filename="/home/dogbert/Desktop/May_meso_flux.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
hum_may_4 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 1296, titleboo=TRUE)
dev.off()
###############################
png(filename="/home/dogbert/Desktop/Jun_meso_flux.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
hum_jun_1 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 1669, titleboo=TRUE)
dev.off()
###dev.off()############################

#hum_jun_2 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 1901, titleboo=TRUE)

###############################

#hum_jul_1 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 2301, titleboo=TRUE)

###############################

#hum_jul_2 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 2558, titleboo=TRUE)

###############################
png(filename="/home/dogbert/Desktop/Jul_meso_flux.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
hum_jul_3 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 2847, titleboo=TRUE)

dev.off()
###############################

#hum_aug_1 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 3288, titleboo=TRUE)

###############################
png(filename="/home/dogbert/Desktop/Aug_meso_flux.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
hum_aug_2 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 3308, titleboo=TRUE)
dev.off()
###############################

#hum_aug_3 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 3445, titleboo=TRUE)

###############################

#hum_sep_1 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 3830, titleboo=TRUE)

###############################
png(filename="/home/dogbert/Desktop/Sep_meso_flux.png", 
    units="cm", 
    width=20, 
    height=20, 
    res=150)
hum_sep_2 <- vec_plot(hum_x_stack_crp, hum_y_stack_crp, hum_len_total, cntry, kili, nguru, rastnum = 4208, titleboo=TRUE)
dev.off()
