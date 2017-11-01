## Run analysis, write model results

## Before: model.cfg, sam, sam.pin (TAF database), sam.dat (input)
## After:  confclone.log, model.cfg, sam.cor, sam.par, sam.rep, sam.res (model)

library(icesTAF)

mkdir("model")

url <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/"

sam <- if(.Platform$OS.type == "unix") "sam" else "sam.exe"
download(paste0(url,"/run/",sam), "model")
download(paste0(url,"run/model.cfg"), "model")
download(paste0(url,"run/sam.pin"), "model")

cp("input/sam.dat", "model")

setwd("model")
system("./sam -nr 2 -noinit")
setwd("..")
