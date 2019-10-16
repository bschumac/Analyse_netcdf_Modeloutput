# implementation of van der Ent et al 2010  
  
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
  
  filebase_path <- "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/"
  filebase_raster <- paste0(filebase_path, "raster/2014_complete")
  filebase_csv <- paste0(filebase_path, "csv/rH_ta_200/")
  filebase_shp <- paste0(filebase_path, "vector/plots_shp/")
  filebase_results <- paste0(filebase_path, "results/")
  filebase_code <- paste0(filebase_path, "code/Analyse_netcdf_Modeloutput/")
  source(paste0(filebase_code,"analyse_fun.R"))
  
  fld_lst <- list.files(filebase_raster, full.names = TRUE, pattern = "20")
  #res <- unique(na.omit(as.numeric(unlist(strsplit(fld_lst, "[^0-9]+")))))[2]
  fld_o <- paste0(gsub(filebase_raster, "", fld_lst))




  #################################################################################################################
  # Create Humidity Flux
  # x direction (west-east)
  
  for (o in seq(1,length(fld_lst))){
      o <- 1
      dir.create(paste0(fld_lst[o],"/hum_flux"))
      act_out_dir <- paste0(fld_lst[o],"/hum_flux")
      temp <- paste0(fld_lst[o],"/output/", list.files( paste0(fld_lst[o],"/output"), pattern = "ATM")[2])
      num <- o

  ncin <- nc_open(temp)
  sigma <- ncvar_get(ncin, "sigma")
  lvls <- ncin$var$ua$varsize[3]
  nc_close(ncin)
  
  lvl_lst <- seq(1,lvls)
  
  uas_lvl_lst <- lapply(lvl_lst, function(i){
    i <- i-1
    print(i)
    uas <- read_modeloutput(temp, variable = "ua", levels = TRUE, lvl = i)
    #uas2 <- read_modeloutput(temp2, variable = "ua", levels = TRUE, lvl = i)
    #uas <- stack(uas1,uas2)
    #uas1 <- NULL
    #rm(uas2)
    #uas <- crp_raster(uas, window_size = 23)
    uas <- uas[[1:48]]
    return(uas)
  })
  uas <- NULL
  gc()
  #save(uas_lvl_lst, file = paste0(act_out_dir, "/uas_lvl_lst.rda"))
  qas_lvl_lst <- lapply(lvl_lst, function(i){
    i <- i-1
    print(i)
    qas <- read_modeloutput(temp, variable = "qas", levels = TRUE, lvl = i)
    #qas2 <- read_modeloutput(temp2, variable = "qas", levels = TRUE, lvl = i)
    ##qas <- stack(qas1[[t]],qas2[[t]])
    #rm(qas1)
    #rm(qas2)
    #qas <- crp_raster(qas, window_size = 23)
    qas <- qas[[1:48]]
    return(qas)
  })
  qas <- NULL
  gc()
  
  ps <-  read_modeloutput(temp, variable = "ps", levels = FALSE)
  #ps2 <-  read_modeloutput(temp2, variable = "ps", levels = FALSE)
  #ps <- stack(ps1[[t]], ps2[[t]])
  #ps <- crp_raster(ps,  window_size = 23)
  # pressure at top of model = 5.0 cbar = 50 hPa from namefile of Model
  ps <- ps[[1:48]]
  ps_top <- ps[[1]]
  values(ps_top) <- c(rep(50, length(values(ps_top) )))
  
  
  timesteps <- names(ps)
  
  
  res_stack <- ps
  values(res_stack) <- NA  
    for (j in seq(1, length(timesteps))){
      j <- 1      
      print(j)
            dp_lvl_lst <- lapply(lvl_lst, function(i){
              dp <- (sigma[i]*(ps[[j]]-ps_top))+ps_top
              return(dp)
            })
            
            dp_act <- as.array(stack(dp_lvl_lst))
            qas_lvl_stck <- stack()
            for (i in seq(1,lvls)) {
                qas_lvl_rst <- qas_lvl_lst[[i]][[j]]
                qas_lvl_stck<- stack(qas_lvl_stck, qas_lvl_rst)
              }
            names(qas_lvl_stck) <- seq(1,lvls)
            uas_lvl_stck <- stack()
            for (i in seq(1,lvls)) {
              uas_lvl_rst <- uas_lvl_lst[[i]][[j]]
              uas_lvl_stck<- stack(uas_lvl_stck, uas_lvl_rst)
            }
            names(uas_lvl_stck) <- seq(1,lvls)
            
            qas_act <- as.array(qas_lvl_stck)
            uas_act <- as.array(uas_lvl_stck)
            
            
            res_mat <- vas_act[,,1]  
            res_mat[] <- NA
            for (k in seq(1,nrow(qas_act))){
              for(l in seq(1,ncol(qas_act))){
              x <- dp_act[k,l,1:23]
              y <- qas_act[k,l,1:23]*vas_act[k,l,1:23]
              Sa <- trapz(x, y)
              hum_flux <- (20000/9.81*1)*Sa
              res_mat[k,l] <- hum_flux
              }
            }
            res_stack[[j]] <- res_mat
            
            #f <- y~x
            #plot(f)
      
            
          }
  vas_lvl_lst <- NULL
  qas_lvl_lst <- NULL
  uas <- NULL
  ps <- NULL
  gc()
  
  writeRaster(res_stack, filename = paste0(act_out_dir,"/hum_flux_full_xdirec_",num,".nc") )
  
  
  }
  
  res_stack <- NULL
  
  gc()
  
  
  
  
  
  ###################################################################################################################
  
  
  # y-Component
  
  
  
  
  
  
  for (o in seq(1,4)){
    
    if (o %in% c(1)){
      temp <- paste0(fld_lst[o],"/Kiliman_",res,"km_Apr_May2014_ATM.2014041500.nc")
      num <- paste0("Apr",o)
    }
    if (o %in% c(2)){
      o <- 1  
      #temp <- paste0(fld_lst[o],"/Kiliman_",res,"km_Apr_May2014_ATM.2014041500.nc")
      temp <- paste0(fld_lst[o],"/Kiliman_",res,"km_Apr_May2014_ATM.2014050100.nc")
      num <- paste0("May",o)
    }
    if (o %in% c(3)){
      o <- 2
      temp <- paste0(fld_lst[o],"/Kiliman_",res,"km_Apr_May2014_ATM.2014041500.nc")
      num <- paste0("Apr",o)
    }
    if (o %in% c(4)){
      o <- 2  
      #temp <- paste0(fld_lst[o],"/Kiliman_",res,"km_Apr_May2014_ATM.2014041500.nc")
      temp <- paste0(fld_lst[o],"/Kiliman_",res,"km_Apr_May2014_ATM.2014050100.nc")
      num <- paste0("May",o)
    }
    
    ncin <- nc_open(temp)
    sigma <- ncvar_get(ncin, "sigma")
    lvls <- ncin$var$ua$varsize[3]
    nc_close(ncin)
    
    lvl_lst <- seq(1,lvls)
    
    
    vas_lvl_lst <- lapply(lvl_lst, function(i){
      i <- i-1
      print(i)
      vas <- read_modeloutput(temp, variable = "va", levels = TRUE, lvl = i)
      #vas2 <- read_modeloutput(temp2, variable = "ua", levels = TRUE, lvl = i)
      #vas <- stack(vas1,vas2)
      #vas1 <- NULL
      #rm(vas2)
      #vas <- crp_raster(vas, window_size = 23)
      return(vas)
    })
    qas_lvl_lst <- lapply(lvl_lst, function(i){
      i <- i-1
      print(i)
      qas <- read_modeloutput(temp, variable = "qas", levels = TRUE, lvl = i)
      #qas2 <- read_modeloutput(temp2, variable = "qas", levels = TRUE, lvl = i)
      #qas <- stack(qas1[[t]],qas2[[t]])
      #rm(qas1)
      #rm(qas2)
      #qas <- crp_raster(qas, window_size = 23)
      
      return(qas)
    })
    
    ps <-  read_modeloutput(temp, variable = "ps", levels = FALSE)
    #ps2 <-  read_modeloutput(temp2, variable = "ps", levels = FALSE)
    #ps <- stack(ps1[[t]], ps2[[t]])
    #ps <- crp_raster(ps,  window_size = 23)
    # pressure at top of model = 5.0 cbar = 50 hPa from namefile of Model
    ps_top <- ps[[1]]
    values(ps_top) <- c(rep(50, length(values(ps_top) )))
    
    
    timesteps <- names(ps)
    
    
    res_stack <- ps
    values(res_stack) <- NA  
    for (j in seq(1, length(timesteps))){
      print(j)
      dp_lvl_lst <- lapply(lvl_lst, function(i){
        dp <- (sigma[i]*(ps[[j]]-ps_top))+ps_top
        return(dp)
      })
      
      dp_act <- as.array(stack(dp_lvl_lst))
      qas_lvl_stck <- stack()
      for (i in seq(1,lvls)) {
        qas_lvl_rst <- qas_lvl_lst[[i]][[j]]
        qas_lvl_stck<- stack(qas_lvl_stck, qas_lvl_rst)
      }
      names(qas_lvl_stck) <- seq(1,lvls)
      vas_lvl_stck <- stack()
      for (i in seq(1,lvls)) {
        vas_lvl_rst <- vas_lvl_lst[[i]][[j]]
        vas_lvl_stck<- stack(vas_lvl_stck, vas_lvl_rst)
      }
      names(vas_lvl_stck) <- seq(1,lvls)
      
      qas_act <- as.array(qas_lvl_stck)
      vas_act <- as.array(vas_lvl_stck)
      
      
      res_mat <- vas_act[,,1]  
      res_mat[] <- NA
      for (k in seq(1,nrow(qas_act))){
        for(l in seq(1,ncol(qas_act))){
          x <- dp_act[k,l,1:23]
          y <- qas_act[k,l,1:23]*vas_act[k,l,1:23]
          Sa <- trapz(x, y)
          hum_flux <- (20000/9.81*1)*Sa
          res_mat[k,l] <- hum_flux
        }
      }
      res_stack[[j]] <- res_mat
      
      #f <- y~x
      #plot(f)
      
      
    }
    uas1 <- NULL
    uas2 <- NULL
    uas_lvl_lst <- NULL
    vas_lvl_lst <- NULL
    uas <- NULL
    ps <- NULL
    gc()
    
    writeRaster(res_stack, filename = paste0(fld_lst[o],"/hum_flux_full_ydirec_",num,".nc") )
    res_stack <- NULL
    
    gc()
    
  }
