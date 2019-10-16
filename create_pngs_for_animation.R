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
if(!require(hydroGOF)){install.packages('hydroGOF')}
library(hydroGOF)
if(!require(NISTunits)){install.packages('NISTunits')}
library(NISTunits)
if(!require(caTools)){install.packages('caTools')}
library(caTools)
if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library(RColorBrewer)
if(!require(rasterVis)){install.packages('rasterVis')}
library(rasterVis)
if(!require(gridExtra)){install.packages('gridExtra')}
library(gridExtra)
library(rgdal)


filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/")
filebase_csv <- paste0(filebase_path, "csv/prec/")
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

#model_base <- read_modeloutput(model_base_file, variable = "pr")

# for (j in seq(1,length(seasons_c))){
#   j <-1
#   print(j)
#   model_base_file <- list.files(paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_", seasons_c[j],"2014_GrellFC01/output/"), pattern="ATM", full.names = TRUE)[2]
#   
#   model_base <- read_modeloutput(model_base_file, variable = "pr")
#   hum_x <- model_base
#   hum_y <- model_base
#   hum_len <- model_base
#   act_file <- paste0(filebase_raster,"/2014_complete/Kiliman_20km_ERA_",seasons_c[j],"2014_GrellFC01/hum_flux/")
#   
#   # the hum flux has to be turned because it is saved with 90deg turn                  
#   values(hum_x) <- values(t(stack(paste0(act_file,"hum_flux_x_direct.nc"))))
#   values(hum_y) <- values(t(stack(paste0(act_file,"hum_flux_y_direct.nc"))))
#   values(hum_len) <- values(stack(paste0(act_file,"hum_flux_total_",seasons_c[j],".nc"), varname="length"))
#   names(hum_len) <- names(model_base)
#   hum_x_stack <- stack(hum_x_stack,hum_x)
#   hum_y_stack <- stack(hum_y_stack,hum_y)
#   hum_len_stack <- stack(hum_len_stack, hum_len)
# }                         

hum_x_stack <- stack()
hum_y_stack <- stack()
hum_len_stack <- stack()
names_hum <- c()
fld_lst <- list.files(paste0(filebase_raster), full.names = TRUE,include.dirs = TRUE)

for (j in seq(1,length(fld_lst))){
  print(j)
  if (j == 1){
    prefix <- "2013"
  }
  if (j == 2){
    prefix <- "2014"
  }
  if (j == 3){
    prefix <- "2015"
  }
  
  act_fld_lst <- list.files(fld_lst[j])
  
  for (k in seq(1,length(act_fld_lst))){
    print(k)
    act_fld <- paste0(fld_lst[j],"/",act_fld_lst[k],"/")
    
    act_hum_x <- brick(paste0(act_fld,"output_",prefix,"_mon_",act_fld_lst[k],".nc"), varname = "u", level=1)
    act_hum_y <- brick(paste0(act_fld,"output_",prefix,"_mon_",act_fld_lst[k],".nc"), varname = "u", level=1)
    act_hum_len <- brick(paste0(act_fld,"output_",prefix,"_mon_",act_fld_lst[k],".nc"), varname = "u", level=1)
    
    names_hum_x <- names(act_hum_x)
    
      
    values_hum_x <- stack(paste0(act_fld,"hum_flux/hum_flux_x_direct.nc"),varname = "length")
    values_hum_y <- stack(paste0(act_fld,"hum_flux/hum_flux_y_direct.nc"),varname = "length")
    values_hum_len <- stack(paste0(act_fld,"hum_flux/hum_flux_total.nc"),varname = "length")
    
    values(act_hum_x) <- values(values_hum_x)
    values(act_hum_y) <- values(values_hum_y)
    values(act_hum_len) <- values(values_hum_len)
    
    hum_x_stack <- stack(hum_x_stack,act_hum_x)
    hum_y_stack <- stack(hum_y_stack,act_hum_y)
    hum_len_stack <- stack(hum_len_stack, act_hum_len)
    names_hum <- c(names_hum,names_hum_x)
    
  }
 
}

names(hum_x_stack) <- names_hum
names(hum_y_stack) <- names_hum
names(hum_len_stack) <- names_hum


plot(hum_len_stack[[1]])


##################################################################### create files
# redefine print Posixct
print.POSIXct <- function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))

my.settings <- rasterTheme(region = brewer.pal("YlGnBu", n = 9),   
                           par.main.text = list(font = 2, just = "left",x = grid::unit(5, "mm")))


for(i in seq(1,length(names(hum_len_stack)))){
  if (i < 10){
    png(filename=paste0(filebase_raster,"/Animation/pngs/0000",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)

  } else if (i < 100){
    png(filename=paste0(filebase_raster,"/Animation/pngs/000",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)
  } else if (i < 1000){
    png(filename=paste0(filebase_raster,"/Animation/pngs/00",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)

  } else if (i < 10000){
    png(filename=paste0(filebase_raster,"/Animation/pngs/0",i,".png"),
        units="cm",
        width=20,
        height=20,
        res=150)
    
  } else {
    png(filename=paste0(filebase_raster,"/Animation/pngs/",i,".png"),
    units="cm",
    width=20,
    height=20,
    res=150)
  }
  act_hum_xy <- stack(hum_x_stack[[i]],hum_y_stack[[i]])
  values(act_hum_xy) <- values(act_hum_xy)/500000
  
  act_hum_len <- hum_len_stack[[i]]/100
  act_hum_len[act_hum_len>300000] <- 300000
  
  title = print(as.POSIXct(names(act_hum_len)[1],format="X%Y.%m.%d.%H.%M.%S"))
  act_plot <- (vectorplot(act_hum_xy, isField = "dXY", region = act_hum_len, margin = FALSE, par.settings = my.settings, 
                        narrows = 300, at = seq(0,300000, 3000), 
                        colorkey = list(labels = list( labels =  c("0 m³/s", format(seq(50000,300000,50000), digits=7), width = 1, cex = 1))) , main=title)  +   layer(sp.polygons(cntry, cex=1)) + layer(sp.points(kili, lwd=3, cex=1.5, pch=24, col="black")))

  print(act_plot)
  dev.off()
  
}  
plot(act_hum_len)

save(hum_x_stack, file = paste0(filebase_raster,"ERA5_hum_x_stack_2013-2015.RData"))
save(hum_y_stack, file = paste0(filebase_raster,"ERA5_hum_y_stack_2013-2015.RData"))
save(hum_len_stack, file = paste0(filebase_raster,"ERA5_hum_len_stack_2013-2015.RData"))


