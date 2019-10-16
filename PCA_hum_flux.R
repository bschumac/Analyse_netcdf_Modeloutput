library(raster)
library(R.matlab)
library(ncdf4)
library(RStoolbox)
require(foreach)
require(doParallel)



hum_flux <- stack("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/2013/05/hum_flux/hum_flux_total.nc", varname="length")
hum_flux_rel <- hum_flux/max(values(hum_flux))

plot()

pca <- rasterPCA(hum_flux_rel, nComp = 1)
pca2 <- pca$map+(min(values(pca$map))*-1)
pca2 <- (pca2*-1)+max(values(pca2))
plot(pca2[[1]])

setOption("max.print", 5000)
summary(pca$model)


months_num <- c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
summary_lst <- c()
hum_pca <- stack()
for (i in seq(1,length(months_num))){
  i <- 1
  print(months_num)
  hum_flux2013 <- stack(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/2013/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
  hum_flux2014 <- stack(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/2014/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
  hum_flux2015 <- stack(paste0("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/2015/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
  
  hum_flux <- stack(hum_flux2013,hum_flux2014,hum_flux2015)
  hum_flux_rel <- hum_flux/max(values(hum_flux))
  #pca <- rasterPCA(hum_flux, nComp = 1)
  
  pca <- ??rasterPCA(hum_flux_rel, nComp = 1)
  pca2 <- pca$map+(min(values(pca$map))*-1)
  pca2 <- (pca2*-1)+max(values(pca2))
  
  proportion_of_Var_expl_PC1 = pca$model$sdev[1]*pca$model$sdev[1]/a
  summary_lst[i] <- proportion_of_Var_expl_PC1
  hum_pca<- stack(hum_pca,pca2)
}



data.frame(months,unlist(summary_lst))





# Explained Variance:
# Apr Comp1 : 3.632903e-01 
# Apr Comp2 : 1.062572e-01
# Apr Comp3 : 7.731666e-02


# Explained Variance:
# May Comp1 : 4.950257e-01 
# May Comp2 : 1.514175e-01
# May Comp3 : 6.570629e-02

# Explained Variance:
# Jun Comp1 : 7.617976e-01 
# Jun Comp2 : 5.417209e-02
# Jun Comp3 : 2.722264e-02

# Explained Variance:
# Oct Comp1 : 4.179106e-01 
# Oct Comp2 : 1.177864e-01
# Oct Comp3 : 8.929086e-02