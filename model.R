## Run analysis, write model results

## Before: model.cfg, sam.pin, sam (stockassessment.org), sam.dat (input)
## After:  sam.par, sam.rep, sam.res, sam.cor, confclone.log, model.cfg (model)

library(icesTAF)

mkdir("model")

url <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/"

download(paste0(url,"run/model.cfg"), "model")
download(paste0(url,"run/sam.pin"), "model")

sam <- if(.Platform$OS.type == "unix") "sam" else "sam.exe"
download(paste0(url,"/run/",sam), "model")

cp("input/sam.dat", "model")

setwd("model")
system("./sam -nr 2 -noinit")
