## Run analysis, write model results

## Before: model.cfg, sam, sam.pin (begin/model), sam.dat (input)
## After:  confclone.log, model.cfg, sam.cor, sam.par, sam.rep, sam.res (model)

library(icesTAF)

mkdir("model")

sam <- if(.Platform$OS.type == "unix") "sam" else "sam.exe"
cp(file.path("begin/model",sam), "model")
cp("begin/model/model.cfg", "model")
cp("begin/model/sam.pin", "model")

cp("input/sam.dat", "model")

setwd("model")
system("./sam -nr 2 -noinit")
setwd("..")
