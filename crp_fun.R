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
    ext1[1,2] <- ext1[1,2]+window_size*res(raster)[1]+res(raster)[1]/2
    ext1[2,2] <- ext1[2,2]-window_size*res(raster)[1]-res(raster)[1]/2
    ext1[3,2] <- ext1[3,2]+window_size*res(raster)[1]+res(raster)[1]/2
    ext1[4,2] <- ext1[4,2]-window_size*res(raster)[1]-res(raster)[1]/2
    
    ext1 <- extent(ext1)
    #ext1 <- as(ext1, 'SpatialPolygons')
    
    raster_out <- crop(raster, ext1)
    
    return(raster_out)
    
}