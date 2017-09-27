## Upload model executables to TAF database

## Before: sam, sam.exe (user dir)
## After:  sam, sam.exe (TAF database)

library(icesTAF)

owd <- setwd("d:/projects/ices-taf/ftp/wgnssk/2016/cod-347d/model")
upload("2016_cod-347d", "model", "sam.exe")
upload("2016_cod-347d", "model", "sam")
setwd(owd)
