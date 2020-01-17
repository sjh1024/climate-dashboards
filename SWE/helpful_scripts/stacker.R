library(rgdal)
library(ncdf4)
library(raster)
source('/srv/shiny-server/dashboards/SWE/wbm_load.R')
print("Loading in historical...")
wbmHist<-wbm_load("/data1/modeledNcdfs/historical/daily", "snowPack", NA) #RCP Historical data
print("Loading in rcp45...")
wbm45 <-wbm_load("/data1/modeledNcdfs/rcp45/daily", "snowPack", NA) #RCP 4.5 data
print("Loading in rcp85...")
wbm85 <-wbm_load("/data1/modeledNcdfs/rcp85/daily", "snowPack", NA) #RCP 8.5 data
print("Writing in historical...")
outfile1 <- writeRaster(wbmHist, "/data1/modeledNcdfs/histImg.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
print("Writing in rcp45...")
outfile2 <- writeRaster(wbm45, "/data1/modeledNcdfs/rcp45Img.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
print("Writing in rcp85...")
outfile3 <- writeRaster(wbm85, "/data1/modeledNcdfs/rcp85Img.tif", options="INTERLEAVE=BAND", overwrite=TRUE)

