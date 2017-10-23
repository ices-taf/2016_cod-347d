## Upload model executables to TAF database

## Before: model.cfg, sam, sam.exe, sam.pin (user dir)
## After:  model.cfg, sam, sam.exe, sam.pin (TAF database)

library(icesTAF)

owd <- setwd("d:/projects/ices-taf/ftp/wgnssk/2016/cod-347d/model")
upload("2016_cod-347d", "model", "sam.exe")
upload("2016_cod-347d", "model", "sam")
upload("2016_cod-347d", "model", "model.cfg")
upload("2016_cod-347d", "model", "sam.pin")
setwd(owd)
