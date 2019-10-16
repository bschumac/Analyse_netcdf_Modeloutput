# Cutting Trimm to Kilimajaro Size


library(raster)
library(mapview)
library(rgdal)



filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "TRMM/raster")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"crp_fun.R"))


fld_lst <- list.files(filebase_raster, full.names = TRUE)

prc_12_st <- lapply(fld_lst, function(i){
  print(i)
  fld_o <- paste0(gsub("/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/TRMM/raster/","", i))
  file_lst <- list.files(i, full.names = TRUE, pattern = "03hr.tif")
  prc_st <- stack(file_lst)
  crs(prc_st) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
  prc_crp <- crp_raster(prc_st, window_size = 50)
  writeRaster(prc_crp, filename = paste0(filebase_path,"TRMM/cut_raster/", fld_o,".tif"), overwrite= TRUE)
  return(prc_crp)
  })



