## Upload model executables to TAF database

## Before: sam, sam.exe (user dir), model.cfg, sam.pin (stockassessment.org)
## After:  model.cfg, sam, sam.exe, sam.pin (TAF database)

library(icesTAF)

url <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/run/"

## From stockassessment.org
download(paste0(url, "model.cfg"))
download(paste0(url, "sam.pin"))
upload("2016_cod-347d", "model", "model.cfg")
upload("2016_cod-347d", "model", "sam.pin")

## From user dir
owd <- setwd("d:/projects/ices-taf/ftp/wgnssk/2016/cod-347d/model")
upload("2016_cod-347d", "model", "sam.exe")
upload("2016_cod-347d", "model", "sam")
setwd(owd)
