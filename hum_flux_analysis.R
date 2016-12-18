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

filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))


fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "Grell")

res <- unique(na.omit(as.numeric(unlist(strsplit(fld_lst, "[^0-9]+")))))[1]
fld_o <- paste0(gsub(filebase_raster, "", fld_lst))

hum_flux <- stack(paste0(fld_lst, "/hum_flux_total.nc"), varname="length")
hum_direc <- stack(paste0(fld_lst, "/hum_flux_total.nc"), varname="direct")

prc1 <- read_modeloutput(paste0(fld_lst,"/Kiliman_20km_Apr_May2014_ATM.2014050100.nc"), variable = "pr")
prc2  <- read_modeloutput(paste0(fld_lst,"/Kiliman_20km_Apr_May2014_ATM.2014041500.nc"), variable = "pr")
prc <- stack(prc1,prc2)
hum_flux_res <- prc
hum_direc_res <- prc
topo <- read_modeloutput(paste0(fld_lst,"/Kiliman_20km_Apr_May2014_ATM.2014050100.nc"))

values(hum_flux_res) <- values(hum_flux)
values(hum_direc_res) <- values(hum_direc)

plot(hum_flux_res[[1]])

hum_flux_kili <- crp_raster(hum_flux_res, window_size = 5)
hum_direc_kili <- crp_raster(hum_direc_res, window_size = 5)
topo_kili <- crp_raster(topo, window_size = 5)
prc_kili <- crp_raster(prc, window_size = 5)
values(prc_kili) <- values(prc_kili)*3600
nb.col=8
color <- rainbow(nb.col)
plot(hum_flux_kili[[15]], col=color)
plot(hum_direc_kili[[15]], col=color, breaks = c(0,45,90,135,180,215,270,315,360))
plot(topo_kili)

erg_flux <- c()

for (i in seq(1,length(names(hum_flux_kili)))){
  act_direct_mat <- as.matrix(hum_direc_kili[[i]])
  act_flux_mat <- as.matrix(hum_flux_kili[[i]])
  n_direct <- act_direct_mat[1,][3:ncol(hum_direc_kili)-1]
  s_direct <- act_direct_mat[ncol(hum_direc_kili),][2:ncol(hum_direc_kili)-1]
  w_direct <- act_direct_mat[,1][3:ncol(hum_direc_kili)-1]
  e_direct <- act_direct_mat[,ncol(hum_direc_kili)][2:ncol(hum_direc_kili)-1]
  nw_cor_direct <- act_direct_mat[1,1]
  ne_cor_direct <- act_direct_mat[1,ncol(hum_direc_kili)]
  sw_cor_direct <- act_direct_mat[ncol(hum_direc_kili),1]
  se_cor_direct <- act_direct_mat[ncol(hum_direc_kili),nrow(hum_direc_kili)]
  
  n_values <- act_flux_mat[1,][3:ncol(hum_flux_kili)-1]
  s_values <- act_flux_mat[ncol(hum_flux_kili),][2:ncol(hum_direc_kili)-1]
  w_values <- act_flux_mat[,1][3:ncol(hum_flux_kili)-1]
  e_values <- act_flux_mat[,ncol(hum_direc_kili)][2:ncol(hum_flux_kili)-1]
  nw_cor_values <- act_direct_mat[1,1]
  ne_cor_values <- act_direct_mat[1,ncol(hum_flux_kili)]
  sw_cor_values <- act_direct_mat[ncol(hum_flux_kili),1]
  se_cor_values <- act_direct_mat[ncol(hum_flux_kili),nrow(hum_flux_kili)]
  
  
  for (j in seq(1,length(n_direct))){

    
    if (!(n_direct[j]>90 & n_direct[j]<270)){
      n_values[j] <- n_values[j]*-1
    }
    if ((s_direct[j]>90 & s_direct[j]<270)){
      s_values[j] <- s_values[j]*-1
    }
    if (!(w_direct[j]>0 & w_direct[j]<180)){
      w_values[j] <- w_values[j]*-1
    }
    if ((e_direct[j]>0 & e_direct[j]<180)){
      e_values[j] <- e_values[j]*-1
    }
    
    
  
  }
  s_n <- sum(n_values)
  s_s <- sum(s_values)
  s_e <-   sum(e_values)
  s_w <-   sum(w_values)
  erg_flux[i] <- sum(s_n, s_s, s_e, s_w)

}
#plot(prc_kili[[211]]*3600)

local_prc_idx <- which(erg_flux <= 0)
reg_prc_idx <- which(erg_flux > 0)

prc_kili_local <- prc_kili[[local_prc_idx]]
prc_kili_reg <- prc_kili[[reg_prc_idx]]



(sum(values(prc_kili_local))/(sum(values(prc_kili_local))+sum(values(prc_kili_reg))))*100
prc_df <- data.frame(colSums(values(prc)))
prc_df$col <- ""
colnames(prc_df)<- c("colSums", "col")
prc_df$datetime <- as.POSIXct(rownames(prc_df), format="X%Y.%m.%d.%H.%M.%S")
prc_df <- prc_df[order(prc_df$datetime),]
prc_df$col[local_prc_idx] <- "red"
prc_df$col[reg_prc_idx] <- "black"


plot(prc_df$colSums, col = prc_df$col, cex=0.5)
axis(1, at=seq(1,721,72), labels=prc_df$datetime[seq(1,721,72)])




