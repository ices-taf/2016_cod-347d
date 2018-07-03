## Run analysis, write model results

## Before: model.cfg, sam, sam.pin (begin/initial/model), sam.dat (input)
## After:  confclone.log, model.cfg, sam.cor, sam.par, sam.rep, sam.res (model)

library(icesTAF)

mkdir("model")

sam <- if(.Platform$OS.type == "unix") "sam" else "sam.exe"
cp(file.path("begin/initial/model",sam), "model")
cp("begin/initial/model/model.cfg", "model")
cp("begin/initial/model/sam.pin", "model")

cp("input/sam.dat", "model")

setwd("model")
system("./sam -nr 2 -noinit")
setwd("..")
