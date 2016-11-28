create_WS_WD <- function(netcdf_vas_kili,netcdf_uas_kili){
  
  netcdf_WD_kili <- stack()
  netcdf_WS_kili <- stack()
  raster_mask <- netcdf_uas_kili[[1]]
  vals <- 0
  values(raster_mask) <- vals
  act_ws_rst <- raster_mask
  act_wd_rst <- raster_mask
  for (j in seq(1,nlayers(netcdf_uas_kili))){
    print(paste("Layer", j))
    act_netcdf_uas_kili <-  netcdf_uas_kili[[j]]
    act_netcdf_vas_kili <-  netcdf_vas_kili[[j]]
    for (i in (seq(1,ncell(act_netcdf_uas_kili)))){
      #print(i)
      #print(paste("Cell",i))
      act_val_uas <- values(act_netcdf_uas_kili)[i]
      act_val_vas <- values(act_netcdf_vas_kili)[i]
      ws <- sqrt(act_val_uas**2+ act_val_vas**2)
      
      if ( act_val_uas > 0 & act_val_vas > 0){
        wd <- NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
      }
      if (act_val_uas > 0 & act_val_vas < 0){
        wd <- 180 - NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
      }
      if (act_val_uas < 0 & act_val_vas < 0){
        wd <- 180 + NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
      }
      if (act_val_uas < 0 & act_val_vas > 0){
        wd <- 360 - NISTradianTOdeg(atan(abs(act_val_uas/act_val_vas)))
      }
      if (wd>360){
        print("WHAT?!")
        break
      }
      values(act_ws_rst)[i] <- ws
      values(act_wd_rst)[i] <- wd
    }
    if (wd>360){
      print("WHAT?!")
      break
    }
    netcdf_WD_kili <- stack(netcdf_WD_kili, act_wd_rst)
    netcdf_WS_kili <- stack(netcdf_WS_kili,act_ws_rst)
  }
  wd_ws_lst <- list(netcdf_WD_kili, netcdf_WS_kili)
  names(wd_ws_lst) <- c("WD", "WS")
  return(wd_ws_lst)
}

persp.withcol <- function(x,y,z,pal,nb.col,...,xlg=TRUE,ylg=TRUE)
{
  colnames(z) <- y
  rownames(z) <- x
  
  nrz <- nrow(z)
  ncz <- ncol(z) 
  
  color <- pal(nb.col)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nb.col)
  par(xlog=xlg,ylog=ylg)
  persp(
    as.numeric(rownames(z)),
    as.numeric(colnames(z)),
    as.matrix(z),
    col=color[facetcol],
    ...
  )
}


read_modeloutput <- function(filepath, variable = "topo", lvl=0, levels=FALSE){
  temp <- filepath
  netcdf_lon <- stack(temp, varname = "xlon")
  netcdf_lat <- stack(temp, varname = "xlat")
  lon_min <- min(values(netcdf_lon))
  lon_max <- max(values(netcdf_lon))
  lat_min <- min (values(netcdf_lat))
  lat_max <- max(values(netcdf_lat))
  if (levels){ 
    netcdf_rst <- brick(temp, varname = variable, level= lvl) 
    } else {
        netcdf_rst <- stack(temp, varname = variable)
        }
  
  netcdf_raw <- raster(nrows = netcdf_rst@nrows, ncols = netcdf_rst@ncols,
                       crs= crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "), 
                       xmn=lon_min, xmx=lon_max, ymn = lat_min, ymx = lat_max)
  out_rst <- stack()
  for (i in seq(1,  nlayers(netcdf_rst))){
    out_rst <- stack(out_rst,netcdf_raw)
  }
  
  values(out_rst) <- values(netcdf_rst)
  return(out_rst)
  
}


crp_raster <- function(raster, pointX = 37.353205, pointY = -3.076475, window_size = 5){
  row <- rowFromY(raster, pointY)
  col <- colFromX(raster, pointX)
  
  
  # create Rastermask for the Kili Region. 
  raster_mask <- raster[[1]]
  #write out the original Raster for control
  #writeRaster(raster_mask, "where_is_kili", format="GTiff", overwrite=TRUE)
  vals <- 0
  raster_mask <- setValues(raster_mask, vals)
  
  # get position of Cell in the middle (Kili Pixel)
  celln <- cellFromRowCol(raster_mask, row-1, col-1)
  ext1 <- data.frame(xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row+1, col-1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row-1, col+1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  celln <- cellFromRowCol(raster_mask, row+1, col+1)
  ext1 <- rbind(ext1,xyFromCell(raster_mask, celln))
  
  # moving edgepoints to middle cell to get extent
  ext1[1,1] <- ext1[1,1]-window_size*res(raster)[1]-res(raster)[1]/2
  ext1[2,1] <- ext1[2,1]-window_size*res(raster)[1]-res(raster)[1]/2
  ext1[3,1] <- ext1[3,1]+window_size*res(raster)[1]+res(raster)[1]/2
  ext1[4,1] <- ext1[4,1]+window_size*res(raster)[1]+res(raster)[1]/2
  ext1[1,2] <- ext1[1,2]+window_size*res(raster)[2]+res(raster)[2]/2
  ext1[2,2] <- ext1[2,2]-window_size*res(raster)[2]-res(raster)[2]/2
  ext1[3,2] <- ext1[3,2]+window_size*res(raster)[2]+res(raster)[2]/2
  ext1[4,2] <- ext1[4,2]-window_size*res(raster)[2]-res(raster)[2]/2
  
  ext1 <- extent(ext1)
  #ext1 <- as(ext1, 'SpatialPolygons')
  raster_out <- crop(raster, ext1)
  return(raster_out)
  
}
