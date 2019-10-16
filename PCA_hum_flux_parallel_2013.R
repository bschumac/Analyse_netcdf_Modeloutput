

library(raster)
library(ncdf4)
library(RStoolbox)
require(foreach)
require(doParallel)

base_path <- "/media/benjamin/XChange/Masterarbeit/Copernicus_Download/"
#base_path <- "/data/KILI/data/Copernicus_Download/"
months_num <- c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- c("2013","2014","2015")

calc_pca <- function(i){
  hum_flux2013 <- stack(paste0(base_path,years[1],"/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
  #hum_flux2014 <- stack(paste0(base_path,years[2],"/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
  #hum_flux2015 <- stack(paste0(base_path,years[3],"/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
  
  hum_flux <- stack(hum_flux2013[[1:10]])
  
  #hum_flux <- hum_flux2013
    #stack(hum_flux2013,hum_flux2014,hum_flux2015)
  
  hum_flux_rel <- hum_flux/max(values(hum_flux))
  #pca <- rasterPCA(hum_flux, nComp = 1)
  
  pca <- rasterPCA(hum_flux_rel, nComp = 2)
  pcamap <- pca$map#/max(values(pca$map))
  #pca2 <- pcamap+(min(values(pcamap))*-1)
  pca1 <- pcamap[[1]]
  pca2 <- pcamap[[2]]
  #pca2 <- (pca2*-1)+max(values(pca2))
  #plot(pca2)
  summary(pca$model)
  # https://stats.stackexchange.com/questions/254592/calculating-pca-variance-explained/254598
  a <- sum(pca$model$sdev**2)
  proportion_of_Var_expl_PC1 = pca$model$sdev[1]*pca$model$sdev[1]/a
  proportion_of_Var_expl_PC2 = pca$model$sdev[2]*pca$model$sdev[2]/a
  proportion_of_Var_expl_PC3 = pca$model$sdev[3]*pca$model$sdev[3]/a
  return(list(pca1,pca2,c(proportion_of_Var_expl_PC1,proportion_of_Var_expl_PC2)))
}

test <- calc_pca(1)

no_cores <- detectCores() - 2  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  
result_pca <- foreach(i=seq(1,length(months_num))) %dopar% calc_pca(i)

out_expl_var <- c()

for (k in seq(1, length(result_pca))){
  
  #writeRaster(result_pca[[k]][[1]], paste0(base_path,"abcdPCA2013_mon",as.character(k),".tif"), format="GTiff" )
  out_expl_var<- c(out_expl_var,result_pca[[k]][[3]])
}
write(out_expl_var, paste0(base_path,"out_expl_var.txt"))
