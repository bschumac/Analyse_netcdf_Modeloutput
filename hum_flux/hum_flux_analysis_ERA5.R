# create Humidity Budget

#if (!require(raster)){install.packages('raster')}
library(sp)
library(raster)
#if (!require(ncdf4)){install.packages("ncdf4", type = "source", configure.args="--with-netcdf-include=/usr/include")}
library(ncdf4)
library(rgdal)
#if(!require(caret)){install.packages('caret')}
library(caret)
#if(!require(mapview)){install.packages('mapview')}
#library(mapview)
#install.packages( "hydroGOF")
library(hydroGOF)
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
#install.packages("lubridate")
library(caTools)
library(RColorBrewer)
library(lubridate)

filebase_path <- "/media/benjamin/XChange/Masterarbeit/Analyse_Modeloutput/"
filebase_raster <- paste0(filebase_path, "raster")
filebase_csv <- paste0(filebase_path, "csv/")
filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
filebase_results <- paste0(filebase_path, "results/")
filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
source(paste0(filebase_code,"analyse_fun.R"))

base_path <- "/media/benjamin/XChange/Masterarbeit/Copernicus_Download/"


years <- c("2013","2014","2015")
months_num <- c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
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


for (r in seq(1,3)){
  
  
  for (i in seq(1,12)){
    print(i)
    hum_flux <- stack(paste0(base_path,years[r],"/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="length")
    hum_direc <- stack(paste0(base_path,years[r],"/",months_num[i],"/hum_flux/hum_flux_total.nc"), varname="direct")
    
     
    prc1 <- stack(paste0(base_path,years[r],"/",months_num[i],"/output_",years[r],"_pr_mon_",months_num[i],".nc"), varname="tp")
    #prc2  <- read_modeloutput(paste0(act_fld,"/Kiliman_20km_Apr_May2014_ATM.2014041500.nc"), variable = "pr")
    prc <- prc1
    hum_flux_res <- prc
    hum_direc_res <- prc
    topo <- stack("/media/benjamin/XChange/Masterarbeit/Copernicus_Download/output_2013_oro_01.nc")/10
    topo <- topo[[1]]
    
    values(hum_flux_res) <- values(hum_flux)
    values(hum_direc_res) <- values(hum_direc)
    
    #plot(hum_flux_res[[1]])
    
    hum_flux_kili <- crp_raster(hum_flux_res, window_size = 2)
    hum_direc_kili <- crp_raster(hum_direc_res, window_size = 2)
    topo_kili <- crp_raster(topo, window_size = 2)
    prc_kili <- crp_raster(prc, window_size = 2)
    values(prc_kili) <- values(prc_kili)*3600
    #nb.col=8
    #color <- rainbow(nb.col)
    #plot(prc_kili[[15]])
    #plot(hum_flux_kili[[15]], col=color)
    #plot(hum_direc_kili[[15]], col=color, breaks = c(0,45,90,135,180,215,270,315,360))
    #par(mfrow=c(1,1))
    plot(topo)
    
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
    #plot(prc_kili[[211]])
    total_flux <- c(total_flux,erg_flux)
    
    
    local_prc_idx <- which(erg_flux <= 0)
    reg_prc_idx <- which(erg_flux > 0)
      
    prc_kili_local <- prc_kili[[local_prc_idx]]
    prc_kili_reg <- prc_kili[[reg_prc_idx]]
    
    
    print("Percentage of Prc-SUM is local:")
    print(sum(values(prc_kili_local))/(sum(values(prc_kili_local))+sum(values(prc_kili_reg)))*100)
    prc_sum <- c(prc_sum,sum(values(prc_kili_local))/(sum(values(prc_kili_local))+sum(values(prc_kili_reg)))*100)
    sum_mth <- c(sum_mth,(sum(values(prc_kili_local))+sum(values(prc_kili_reg))))
    sum_mth_local <- c(sum_mth_local,sum(values(prc_kili_local)))
    sum_mth_regional <- c(sum_mth_regional, sum(values(prc_kili_reg)))
    prc_df <- data.frame(colSums(values(prc_kili)))
    prc_df$col <- ""
    colnames(prc_df)<- c("colSums", "col")
    prc_df$datetime <- as.POSIXct(rownames(prc_df), format="X%Y.%m.%d.%H.%M.%S")
    prc_df <- prc_df[order(prc_df$datetime),]
    prc_df$col[local_prc_idx] <- "red"
    prc_df$col[reg_prc_idx] <- "black"
    print("Percentage of Prc-Events are local:")
    print(sum(prc_df$col == "red")/(sum(prc_df$col == "black")+sum(prc_df$col == "red"))*100)
    sum_evt_local <- c(sum_evt_local,sum(prc_df$col == "red"))
    sum_evt_regional <- c(sum_evt_regional,sum(prc_df$col == "black"))
    sum_timesteps <- c(sum_timesteps,(sum(prc_df$col == "black")+sum(prc_df$col == "red")))
    prc_evt <- c(prc_evt,sum(prc_df$col == "red")/(sum(prc_df$col == "black")+sum(prc_df$col == "red"))*100)
    
    
    ges_df <- rbind(ges_df,prc_df)
    
    
    plot(prc_df$colSums, col = prc_df$col, cex=0.5, axes=FALSE, xlab="", ylab="", ylim=c(0, 350))
    xticks <- seq(1,length(prc_df$datetime),(length(prc_df$datetime)/2)-2)
    xlabels <- substr(prc_df$datetime[seq(1,length(prc_df$datetime),(length(prc_df$datetime)/2)-2)], 1,10)
    yticks <- seq(0,350, 50)
    axis(side=2, labels=TRUE, at=yticks)
    axis(1, at= xticks, labels=xlabels)
    mtext(text="Prc Sum in mm", side = 2, line=2.5)
    
  } 
}
head(ges_df)
tail(ges_df)




test_df <- data.frame(month=rep(real_mth,3),prc_sum)

reg_prc_sum <- c(4,11,8,4,20,19,37,22,7,20,4,8)
reg_prc_evt <- c(4,16,12,10,26,41,56,40,23,28,5,3)
index_prc_sum <- seq(13,24)
plot(prc_sum, type = "b",xaxt = "n", xlab="Month",ylim=c(0,60), ylab="% local Precipitation ", lwd=2, lty=1, pch=19, cex.lab=1.2)
lines(prc_evt, col="grey", lwd=2, type = "b", pch=19)
lines(x=index_prc_sum, y=reg_prc_sum, col="red", lwd=2, type = "b", pch=19)
lines(x=index_prc_sum, y=reg_prc_evt, col="purple", lwd=2,  type = "b", pch=19)
axis(1, at=1:36, labels=rep(real_mth,3), cex.axis=1.2)
axis(1, at=c(1,13,25), labels=c(2013,2014,2015), tick=FALSE, lwd=0, line=1, cex.axis=1.2)
mtext("Year", side = 1, line=4, cex=1.2)
legend(0, 62, legend=c(expression("ERA5 Relative amount of local pr"["sum"]) , expression("ERA5 Frequency of occurence of pr"["local"]), expression("RegCM4 Relative amount of local pr"["sum"]), expression("RegCM4 Frequency of occurence of pr"["local"])),
       col=c("black","grey","red", "purple"),  lwd=2, cex=1.2, box.lty=0, bg="transparent", pch = 19,pt.bg = 'white', lty = 1, bty="n")


lines()

lines(reg_prc_evt)
prc_evt




# Total DF for analyse
par(mfrow=c(1,1))
ges_df <- ges_df[order(ges_df$datetime),]
ges_df$total_flux <- total_flux
plot(ges_df$colSums, col = prc_df$col, cex=0.5, axes=FALSE)
print(sum(ges_df$col == "red")/(sum(ges_df$col == "black")+sum(ges_df$col == "red"))*100)
axis(side=2, labels=TRUE)
axis(1, at=seq(1,length(ges_df$colSums),720), labels=substr(ges_df$datetime[seq(1,length(ges_df$colSums),720)],1,10))
write.csv(ges_df, file = paste0(filebase_csv,"ges_df_hum_flux_ERA5.csv"))

# analyse Helen/Nilofar
hellen <- ges_df[ges_df$datetime>"2014-03-26 00:00:00",]
hellen <- hellen[hellen$datetime<"2014-04-06 00:00:00",]
sum(hellen$col == "black")
sum(hellen$col == "red")
hellen1 <- ges_df[ges_df$datetime>"2014-03-01 00:00:00",]
hellen1 <- hellen1[hellen1$datetime<="2014-05-31 00:00:00",]
sum(hellen$col == "red")/(sum(hellen1$col == "black")+sum(hellen1$col == "red"))


# DF compare for Table in Paper


df_compare <- NULL
df_compare <- data.frame(real_mth,
                         prc_evt,
                         prc_sum,
                         sum_mth,
                         sum_mth_local,
                         sum_mth_regional,
                         sum_timesteps,
                         sum_evt_local,
                         sum_evt_regional)

sum(df_compare$sum_mth)
sum(df_compare$sum_mth_local)/sum(df_compare$sum_mth)

sum(df_compare$sum_mth_local, df_compare$sum_mth_regional)
df_compare$mth_lst <- paste0("2014-",mth_lst)
df_compare$mth_lst <- as.yearmon(df_compare$mth_lst, format = "%Y-%B")
df_compare <- df_compare[order(df_compare$mth_lst),]
mean(total_flux[total_flux[1:(744+672)]>0])/mean(total_flux[total_flux[3624:(3624+2928)]>0])
write.csv(df_compare, file = paste0(filebase_csv,"df_compare.csv"))



# moving mean for lineplot




mov_mean <- lapply(seq(2,length(total_flux)-1), function(i){
  
  return(mean(c(total_flux[i-1],total_flux[i],total_flux[i+1])))
  
})
mov_mean<- c(mean(total_flux[1],total_flux[1+1]),unlist(mov_mean),mean(total_flux[length(total_flux)-1],total_flux[length(total_flux)]) )
ges_df$flux_mov_mean <- mov_mean
ges_df$negpos <- ges_df$flux_mov_mean/abs(ges_df$flux_mov_mean)

linecol_red <- which(ges_df$negpos <= 0)
linecol_black <- which(ges_df$negpos > 0)
ges_df$linecol[linecol_red] <- "red"
ges_df$linecol[linecol_black] <- "black"

png(filename="/home/dogbert/Desktop/Fig_5.png", 
    units="cm", 
    width=10, 
    height=10, 
    res=300)
###################### Final Plot
######## JF
par(mfrow=c(4,1))

jf_prc <- ges_df[ges_df$datetime<"2014-05-01 00:00:00",]
jf_prc[jf_prc$datetime>="2014-03-01 00:00:00",]$colSums <- NA 
used_date <- jf_prc[jf_prc$datetime<="2014-03-02 00:00:00",]$datetime
plot(jf_prc$colSums, col = jf_prc$col, cex=0.5, axes=FALSE, xlab="", ylab="", ylim=c(0, 250))
title("a)", adj=0)
for(i in seq(1,length(used_date))){
  segments(i, 250, x1 = i, y1 = 250,col = jf_prc$linecol[i], lwd = 3)
}
#lines(jf_prc$datetime,rep(250,length(jf_prc$datetime)), col=jf_prc$linecol, type = "l")
xlabels <- seq(used_date[1], used_date[length(used_date)], by="month")
xticks <- match(xlabels,used_date)
xlabels <- substr(xlabels,1,10)
substr(jf_prc$datetime[seq(1,length(used_date),(length(used_date)/2)-2)], 1,10)
yticks <- seq(0,350, 50)
axis(side=2, labels=TRUE, at=yticks)
axis(1, at= xticks, labels=xlabels)
#mtext(text="Prc Sum in mm", side = 2, line=2.5)


######## MAM


mam_prc <- ges_df[ges_df$datetime>"2014-03-01 00:00:00",]
mam_prc <- mam_prc[mam_prc$datetime<"2014-07-01 00:00:00",]
mam_prc[mam_prc$datetime>="2014-06-01 00:00:00",]$colSums <- NA 
used_date <- mam_prc[mam_prc$datetime<="2014-06-02 00:00:00",]$datetime

plot(mam_prc$colSums, col = mam_prc$col, cex=0.5, axes=FALSE, xlab="", ylab="", ylim=c(0, 250))
title("b)", adj=0)
for(i in seq(1,length(used_date))){
  segments(i, 250, x1 = i, y1 = 250,col = mam_prc$linecol[i], lwd = 3)
}
xlabels <- seq(used_date[1], used_date[length(used_date)], by="month")
xticks <- match(xlabels,used_date)
xlabels <- substr(xlabels,1,10)
yticks <- seq(0,350, 50)
axis(side=2, labels=TRUE, at=yticks)
axis(1, at= xticks, labels=xlabels)
#mtext(text="Prc Sum in mm", side = 2, line=2.5)

maxy = 15
ticks_all = 5
######## JJAS
par(mfrow=c(1,1))
jjas_prc <- ges_df[ges_df$datetime>="2014-07-05 00:00:00",]
jjas_prc <- jjas_prc[jjas_prc$datetime<="2014-7-15 00:00:00",]
#jjas_prc[jjas_prc$datetime>="2014-06-01 00:00:00",]$colSums <- NA 
used_date <- jjas_prc[jjas_prc$datetime<="2014-10-01 00:00:00",]$datetime
#used_date <- c(used_date,jjas_prc[jjas_prc$datetime<="2014-10-03 01:00:00",]$datetime)

plot(jjas_prc$colSums, col = jjas_prc$col, cex=0.5, axes=FALSE, xlab="", ylab="", ylim=c(0, maxy))
#title("c)", adj=0)
for(i in seq(1,length(used_date))){
  segments(i, maxy-5, x1 = i, y1 = maxy-5,col = jjas_prc$linecol[i], lwd = 3)
  #segments(i, jjas_prc$colSums, x1 = i, y1 = jjas_prc$colSums,col = jjas_prc$linecol[i], lwd = 3)
}
xlabels <- seq(used_date[1], used_date[length(used_date)], by="day")
xlabels <- xlabels[c(1,round(length(xlabels)/2),length(xlabels))]
xticks <- match(xlabels,used_date)
xlabels <- substr(xlabels,1,10)
yticks <- seq(0,maxy, ticks_all)
axis(side=2, labels=TRUE, at=yticks)
axis(1, at= xticks, labels=xlabels)
#mtext(text="Prc Sum in mm", side = 2, line=2.5)

############### Lens Plot:


jjas_prc <- ges_df[ges_df$datetime>="2014-07-01 00:00:00",]
jjas_prc <- jjas_prc[jjas_prc$datetime<="2014-9-01 00:00:00",]
#jjas_prc[jjas_prc$datetime>="2014-06-01 00:00:00",]$colSums <- NA 
used_date <- jjas_prc[jjas_prc$datetime<="2014-10-01 00:00:00",]$datetime
#used_date <- c(used_date,jjas_prc[jjas_prc$datetime<="2014-10-03 01:00:00",]$datetime)

plot(jjas_prc$colSums, col = jjas_prc$col, cex=0.5, axes=FALSE, xlab="", ylab="", ylim=c(0, 250))
title("c)", adj=0)
for(i in seq(1,length(used_date))){
  segments(i, 150, x1 = i, y1 = 150,col = jjas_prc$linecol[i], lwd = 3)
}
xlabels <- seq(used_date[1], used_date[length(used_date)], by="month")
xticks <- match(xlabels,used_date)
xlabels <- substr(xlabels,1,10)
yticks <- seq(0,150, 25)
axis(side=2, labels=TRUE, at=yticks)
axis(1, at= xticks, labels=xlabels)
#mtext(text="Prc Sum in mm", side = 2, line=2.5)
######## OND


ond_prc <- ges_df[ges_df$datetime>"2014-10-01 00:00:00",]
ond_prc <- rbind(ond_prc, ges_df[ges_df$datetime<="2014-02-03 00:00:00",])
ond_prc[ond_prc$datetime<="2014-02-03 00:00:00",]$datetime <- gsub("2014","2015",ond_prc[ond_prc$datetime<="2014-02-03 00:00:00",]$datetime)
ond_prc[ond_prc$datetime>="2015-01-01 00:00:00",]$colSums <- NA 
used_date <- ond_prc[ond_prc$datetime<="2015-01-02 00:00:00",]$datetime


plot(ond_prc$colSums, col = ond_prc$col, cex=0.5, axes=FALSE, xlab="", ylab="", ylim=c(0, 250))
title("d)", adj=0)
for(i in seq(1,length(used_date))){
  segments(i, 250, x1 = i, y1 = 250,col = ond_prc$linecol[i], lwd = 3)
}
xlabels <- seq(used_date[2], used_date[length(used_date)], by="month")
xticks <- match(xlabels,used_date)
xlabels <- substr(xlabels,1,10)
yticks <- seq(0,350, 50)
axis(side=2, labels=TRUE, at=yticks)
axis(1, at= xticks, labels=xlabels)
#mtext(text="Prc Sum in mm", side = 2, line=2.5)

dev.off()


############# Diurnal Structure of July Prec

july_df <- jjas_prc[jjas_prc$datetime<="2014-07-15 00:00:00",]
july_df <- july_df[july_df$datetime>="2014-07-04 00:00:00",]
july_df$time <- as.numeric(substr(july_df$datetime, 12,13))
plot(july_df$time,july_df$colSums, col=july_df$col)


head(july_df,300)
par(mfrow=c(1,1))
plot(july_df$colSums, col = july_df$col, cex=0.5, axes=FALSE)
print(sum(july_df$col == "red")/(sum(july_df$col == "black")+sum(july_df$col == "red"))*100)
axis(side=2, labels=TRUE)
axis(1, at=seq(1,length(july_df$colSums),72), labels=substr(july_df$datetime[seq(1,length(july_df$colSums),72)],1,10))

