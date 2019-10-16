
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
library(spplot)
#####################################################
filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))


fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "complete")
flds <- list.files(fld_lst, full.names = TRUE, pattern="20")


res <- unique(na.omit(as.numeric(unlist(strsplit(flds, "[^0-9]+")))))[2]
fld_o <- paste0(gsub(filebase_raster, "", flds))
mth_lst <- c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")
real_mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ges_df <- NULL
prc_evt <- c()
prc_sum <- c()
total_flux <- c()
sum_mth <- c()
sum_mth_local <- c()
sum_mth_regional <- c()
sum_evt_local <- c()
sum_evt_regional <- c()
sum_timesteps <- c()
par(mfrow=c(3,4))



  i <- 12
  print(mth_lst[i])
  print("Changing Month...")
  i <- match(real_mth[i], mth_lst)
  print(mth_lst[i])
  act_fld <- flds[i]
  ATM_fls <- list.files(paste0(act_fld,"/output"), pattern="ATM")
  
  
  topo <- read_modeloutput(paste0(act_fld,"/output/",ATM_fls[2]))
  topo_kili <- crp_raster(topo, window_size = 4.5)
  topo_kili2 <- crp_raster(topo, window_size = 8)
  
  
  e <- extent(topo_kili)
  p <- as(e, 'SpatialPolygons') 
  #mapview(plots_shp) + p
  
  
  kili_top <- data.frame(X = 37.353205, Y = -3.076475)
  
  kili_top <- SpatialPointsDataFrame(kili_top,data=kili_top)
  crs(kili_top) <- crs(cntry)
  pts=list("sp.points", kili_top, pch=2, col="black")
  
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
  
  kili_park <- readOGR (dsn = "/media/benjamin/XChange/Masterarbeit/daten_benny/Kili_Park_boarders/WDPA_Oct2019_protected_area_17761-shapefile-polygons.shp", 
                        layer = "WDPA_Oct2019_protected_area_17761-shapefile-polygons", stringsAsFactors = TRUE)
  
  crs(p) <- crs(cntry)
 
  
  
 plot1 <-  spplot(p, fill = NA, colorkey = FALSE)
 plot1 + layer(sp.polygons(cntry, cex=0.1, lty=3, col="gray60"))

 library(RColorBrewer)
 spplot(topo_kili2, col.regions="transparent", colorkey = F,
          #par.settings = list(axis.line = list(col = "transparent")),
         #colorkey = list(axis.line = list(col = "black")), 
         #at = breaks,      
         #colorkey=list(labels = as.character(breaks)),
         panel = function(...){
           panel.levelplot(...)
           
           
           sp.polygons(cntry, cex=0.5, lty=3, col="darkgrey",fill = "transparent")
           sp.polygons(nguru, cex=0.9, lty=2, col="darkred",fill = "transparent")
           sp.polygons(kenya, cex=0.9, lty=2, col="red",fill = "transparent")
           sp.polygons(grvalley, cex=0.9, lty=2, col="darkviolet",fill = "transparent")
           sp.polygons(rivers, cex=0.9, lty=1, col="blue",fill = "transparent")
           sp.polygons(kili_park, cex=0.9, lty=1, col="black",fill = "transparent")
           sp.polygons(p, cex=4, lty=1, col="black")
           sp.points(kili_top, pch =2, col="black")
         } 
         #scales = list(x = list(at = xat, labels = paste0(as.character(xat)," E")),
                       #y = list(at = yat, labels = c(paste0(as.character(abs(yat[1:2]))," S"),paste0(as.character(yat[3])," N"))))
  )
  
  