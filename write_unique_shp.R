filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")

library("sp")
library(raster)
library("rgdal")
lyr <- ogrListLayers(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"))
plots_shp <- readOGR(paste0(filebase_shp,"PlotPoles_ARC1960_mod_20140807_final.shp"),
                     layer=  lyr)
plots_unique <- data.frame(plots_shp)
plots_unique <- data.frame(Name = plots_unique$PlotID,
                           X = plots_unique$X,
                           Y = plots_unique$Y,
                           Z_GPS = plots_unique$Z_GPS)

plots_unique <- plots_unique[!duplicated(plots_unique$Name), ]
plots_unique <- SpatialPointsDataFrame(coords= data.frame(plots_unique$X,plots_unique$Y), plots_unique)
crs(plots_unique) <- crs(plots_shp)
plots_unique<-spTransform(plots_unique, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

writeOGR(plots_unique, "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/vector/plots_shp/plots_unique.kml",
         layer = "plots_unique", driver="KML")
