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
print("saving hist stack...")
stackSave(wbmHist, "histStackTest")
print("saving 45 stack...")
stackSave(wbm45, "45StackTest")
print("saving 85 stack...")
stackSave(wbm85, "85StackTest")
