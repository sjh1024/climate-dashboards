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
wbmHist<-readAll(wbmHist)
save(wbmHist, file= "/data1/modeledNcdfs/wbmHist.RData")
print("Writing in rcp45...")
wbm45 <- readAll(wbm45)
save(wbm45, file="/data1/modeledNcdfs/wbm45.RData")
print("Writing in rcp85...")
wbm85<-readAll(wbm85)
save(wbm85, file="/data1/modeledNcdfs/wbm85.RData")


