## Run analysis, write model results

## Before: sam (bootstrap/software), model.cfg, sam.pin (bootstrap/config),
##         sam.dat (data)
## After:  confclone.log, model.cfg, sam.cor, sam.par, sam.rep, sam.res (model)

library(icesTAF)

mkdir("model")

## Get model executable
exefile <- if(os.unix()) "sam" else "sam.exe"
taf.unzip("bootstrap/software/sam.zip", files=exefile, exdir="model")

## Get model configuration
cp("bootstrap/config/*", "model")

## Get model input file
cp("data/sam.dat", "model")

setwd("model")
system("./sam -nr 2 -noinit")
setwd("..")
