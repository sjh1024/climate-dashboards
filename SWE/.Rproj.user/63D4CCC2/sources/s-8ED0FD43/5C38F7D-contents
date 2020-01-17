#1. Load in all data.frames
print("Loading data frames...")
MEhist <- read.csv("ME_hist.csv", header=TRUE, check.names=FALSE)
NYhist <- read.csv("NY_hist.csv", header=TRUE, check.names=FALSE)
GHCNDhist <- read.csv("GHCND_hist.csv", header=TRUE, check.names=FALSE)
SCANhist <- read.csv("SCAN_hist.csv", header=TRUE, check.names=FALSE)

ME45 <- read.csv("ME_45.csv", header=TRUE, check.names=FALSE)
NY45 <- read.csv("NY_45.csv", header=TRUE, check.names=FALSE)
GHCND45 <- read.csv("GHCND_45.csv", header=TRUE, check.names=FALSE)
SCAN45 <- read.csv("SCAN_45.csv", header=TRUE, check.names=FALSE)

ME85 <- read.csv("ME_85.csv", header=TRUE, check.names=FALSE)
NY85 <- read.csv("NY_85.csv", header=TRUE, check.names=FALSE)
GHCND85 <- read.csv("GHCND_85.csv", header=TRUE, check.names=FALSE)
SCAN85 <- read.csv("SCAN_85.csv", header=TRUE, check.names=FALSE)
print("Data frames loaded.")
#2. Rename columns with paste0, if applicable (DON'T NEED TO DO TO GHCND)
print("Renaming columns...")
colnames(MEhist) <- paste0("ME_", colnames(MEhist))
colnames(NYhist) <- paste0("NY_", colnames(NYhist))
colnames(SCANhist) <- paste0("SCAN_", colnames(SCANhist))

colnames(ME45) <- paste0("ME_", colnames(ME45))
colnames(NY45) <- paste0("NY_", colnames(NY45))
colnames(SCAN45) <- paste0("SCAN_", colnames(SCAN45))

colnames(ME85) <- paste0("ME_", colnames(ME85))
colnames(NY85) <- paste0("NY_", colnames(NY85))
colnames(SCAN85) <- paste0("SCAN_", colnames(SCAN85))

#3. Rename first column to XX_DATE (DON'T NEED TO DO TO GHCND)
names(MEhist)[names(MEhist) == 'ME_'] <- 'ME_DATE'
names(NYhist)[names(NYhist) == 'NY_'] <- 'NY_DATE'
names(SCANhist)[names(SCANhist) == 'SCAN_'] <- 'SCAN_DATE'

names(ME45)[names(ME45) == 'ME_'] <- 'ME_DATE'
names(NY45)[names(NY45) == 'NY_'] <- 'NY_DATE'
names(SCAN45)[names(SCAN45) == 'SCAN_'] <- 'SCAN_DATE'

names(ME85)[names(ME85) == 'ME_'] <- 'ME_DATE'
names(NY85)[names(NY85) == 'NY_'] <- 'NY_DATE'
names(SCAN85)[names(SCAN85) == 'SCAN_'] <- 'SCAN_DATE'
print("Changing values...")
#4. Change relevant values to NA
mergeME <- ME45
mergeNY <- NY45
mergeSCAN <- SCAN45
mergeGHCND <- GHCND45

mergeMEHistNA <- MEhist
mergeNYHistNA <- NYhist
mergeSCANHistNA <- SCANhist
mergeGHCNDHistNA <- GHCNDhist

mergeME[,2:ncol(mergeME)] = NA
mergeNY[,2:ncol(mergeNY)] = NA
mergeSCAN[,2:ncol(mergeSCAN)] = NA
mergeGHCND[,2:ncol(mergeGHCND)] = NA

mergeMEHistNA[,2:ncol(mergeMEHistNA)] = NA
mergeNYHistNA[,2:ncol(mergeNYHistNA)] = NA
mergeSCANHistNA[,2:ncol(mergeSCANHistNA)] = NA
mergeGHCNDHistNA[,2:ncol(mergeGHCNDHistNA)] = NA

#5. Merge rows.
print("Merging...")
newHistME <- rbind(MEhist, mergeME)
newHistNY <- rbind(NYhist, mergeNY)
newHistSCAN <- rbind(SCANhist, mergeSCAN)
newHistGHCND <- rbind(GHCNDhist, mergeGHCND)

new45ME <- rbind(mergeMEHistNA, ME45)
new45NY <-  rbind(mergeNYHistNA, NY45)
new45SCAN <-  rbind(mergeSCANHistNA, SCAN45)
new45GHCND <-  rbind(mergeGHCNDHistNA, GHCND45)

new85ME <- rbind(mergeMEHistNA, ME85)
new85NY <-  rbind(mergeNYHistNA, NY85)
new85SCAN <-  rbind(mergeSCANHistNA, SCAN85)
new85GHCND <-  rbind(mergeGHCNDHistNA, GHCND85)

names(newHistME)<- paste0(names(newHistME), "_HIST")
names(newHistME)[names(newHistME) == "ME_DATE_HIST"] <- "ME_DATE"

names(newHistNY)<- paste0(names(newHistNY), "_HIST")
names(newHistNY)[names(newHistNY) == "NY_DATE_HIST"] <- "NY_DATE"

names(newHistGHCND)<- paste0(names(newHistGHCND), "_HIST")
names(newHistGHCND)[names(newHistGHCND) == "DATE_HIST"] <- "DATE"

names(newHistSCAN)<- paste0(names(newHistSCAN), "_HIST")
names(newHistSCAN)[names(newHistSCAN) == "SCAN_DATE_HIST"] <- "SCAN_DATE"

names(new45ME)<- paste0(names(new45ME), "_4.5")
names(new45ME)[names(new45ME) == "ME_DATE_4.5"] <- "ME_DATE"

names(new45NY)<- paste0(names(new45NY), "_4.5")
names(new45NY)[names(new45NY) == "NY_DATE_4.5"] <- "NY_DATE"

names(new45GHCND)<- paste0(names(new45GHCND), "_4.5")
names(new45GHCND)[names(new45GHCND) == "DATE_4.5"] <- "DATE"

names(new45SCAN)<- paste0(names(new45SCAN), "_4.5")
names(new45SCAN)[names(new45SCAN) == "SCAN_DATE_4.5"] <- "SCAN_DATE"

names(new85ME)<- paste0(names(new85ME), "_8.5")
names(new85ME)[names(new85ME) == "ME_DATE_8.5"] <- "ME_DATE"

names(new85NY)<- paste0(names(new85NY), "_8.5")
names(new85NY)[names(new85NY) == "NY_DATE_8.5"] <- "NY_DATE"

names(new85GHCND)<- paste0(names(new85GHCND), "_8.5")
names(new85GHCND)[names(new85GHCND) == "DATE_8.5"] <- "DATE"

names(new85SCAN)<- paste0(names(new85SCAN), "_8.5")
names(new85SCAN)[names(new85SCAN) == "SCAN_DATE_8.5"] <- "SCAN_DATE"

#print(names(newHistME))
#print(names(newHistNY))
#print(names(newHistSCAN))
#print(names(newHistGHCND))

#print(names(new45ME))
#print(names(new45NY))
#print(names(new45SCAN))
#print(names(new45GHCND))

#print(names(new85ME))
#print(names(new85NY))
#print(names(new85SCAN))
#print(names(new85GHCND))

print("Writing results to rds...")
saveRDS(newHistME, "MEHistorical.rds")
saveRDS(newHistNY, "NYHistorical.rds")
saveRDS(newHistSCAN, "SCANHistorical.rds")
saveRDS(newHistGHCND, "GHCNDHistorical.rds")
saveRDS(new45ME, "MErcp45.rds")
saveRDS(new45NY, "NYrcp45.rds")
saveRDS(new45SCAN, "SCANrcp45.rds")
saveRDS(new45GHCND, "GHCNDrcp45.rds")
saveRDS(new85ME, "MErcp85.rds")
saveRDS(new85NY, "NYrcp85.rds")
saveRDS(new85SCAN, "SCANrcp85.rds")
saveRDS(new85GHCND, "GHCNDrcp85.rds")
print("DONE!")