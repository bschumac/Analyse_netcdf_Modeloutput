# create nc to pngs

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
seasons_c <- c(JF,MAM,JJAS,OND)

par(mfrow=c(1,1))


############################################################### Read Modelfiles

model_base <- read_modeloutput(model_base_file, variable = "pr")
hum_x_stack <- stack()
hum_y_stack <- stack()
hum_len_stack <- stack()
for (j in seq(1,length(seasons_c))){
  print(j)
  model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", seasons_c[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
  
  model_base <- read_modeloutput(model_base_file, variable = "pr")
  hum_x <- model_base
  hum_y <- model_base
  hum_len <- model_base
  act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",seasons_c[j],"2014_GrellFC01/hum_flux/")
  
  # the hum flux has to be turned because it is saved with 90deg turn                  
  values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
  values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
  values(hum_len) <- values(stack(paste0(act_file,"hum_flux_total_",seasons_c[j],".nc"), varname="length"))
  names(hum_len) <- names(model_base)
  hum_x_stack <- stack(hum_x_stack,hum_x)
  hum_y_stack <- stack(hum_y_stack,hum_y)
  hum_len_stack <- stack(hum_len_stack, hum_len)
}                         

plot(hum_len_stack[[1]])


##################################################################### create files
# redefine print Posixct
print.POSIXct <- function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))

my.settings <- rasterTheme(region = brewer.pal("YlGnBu", n = 9),   
                           par.main.text = list(font = 2, just = "left",x = grid::unit(5, "mm")))


for(i in seq(1,length(names(hum_len_stack)))){
  #i <- 15
  
  if (i < 10){
    png(filename=paste0("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results/Animation/pngs/000",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)

  } else if (i < 100){
    png(filename=paste0("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results/Animation/pngs/00",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)
  } else if (i < 1000){
    png(filename=paste0("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results/Animation/pngs/0",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)

  } else {
    png(filename=paste0("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/results/Animation/pngs/",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)
  }
  
  
  act_hum_xy <- stack(hum_x_stack[[i]],hum_y_stack[[i]])
  values(act_hum_xy) <- values(act_hum_xy)/5000
  act_hum_len <- hum_len_stack[[i]] 
  title = print(as.POSIXct(names(act_hum_len)[1],format="X%Y.%m.%d.%H.%M.%S"))
  act_plot <- (vectorplot(act_hum_xy, isField = "dXY", region = act_hum_len, margin = FALSE, par.settings = my.settings, 
                        narrows = 300, at = seq(0,300000, 3000), 
                        colorkey = list(labels = list( labels =  c("0 m³/s", format(seq(50000,300000,50000), digits=6), width = 1, cex = 1))) , main=title)  +   layer(sp.polygons(cntry, cex=1)) + layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))

  print(act_plot)
  dev.off()
  
}  

