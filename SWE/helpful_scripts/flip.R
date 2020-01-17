
# list of files to be transformed to one row per date, one col per station
#files=c("wbm_GHCND.csv","wbm_HubbardBrook.csv","wbm_ME_Snow_Survey.csv","wbm_NY_Snow_Survey.csv","wbm_SCAN_Hubbard_Brook.csv", "wbm_SCAN_MascomaRiver.csv","wbm_SCAN_MtMansfield.csv")
#files=c("wbm_HubbardBrook.csv","wbm_ME_Snow_Survey.csv","wbm_NY_Snow_Survey.csv","wbm_SCAN_Hubbard_Brook.csv", "wbm_SCAN_MascomaRiver.csv","wbm_SCAN_MtMansfield.csv")

files=c("wbm_GHCND.csv")
#files = c("wbm_SCAN_Snow.csv")
for(i in 1:length(files)){

  # read in datafile
  print(paste0("reading in file ",files[i]))
  dat=read.csv(files[i],check.names=FALSE)


  # turn first col into factor to be used for names
  print(paste0("transforming to matrix ",files[i]))
  x=as.factor(dat[,1])
  rownames(dat)=x

  # convert to matrix and transpose
  print(paste0("transpose rows and columns ",files[i]))
  dat.m=as.matrix(dat)
  dat.t=data.frame(t(dat.m))
  dat.t=data.frame(dat.t[-1,])

  # reformat date to YYYY-MM-DD for rownames, and get rid of leading 'X'
  print(paste0("clean up  row and column names ",files[i]))
  rownames(dat.t)=gsub(".","-",rownames(dat.t),fixed=TRUE)
  colnames(dat.t) <- sub("^X", "", colnames(dat.t))
  rownames(dat.t) <- sub("^X", "", rownames(dat.t))

  # write out file
  print(paste0("write out file ",files[i]))
  rownames(dat.t)=gsub(".","-",rownames(dat.t),fixed=TRUE)
  write.csv(dat.t,paste0("t.",files[i]),quote=FALSE)

}






