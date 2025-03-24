# Run analysis, write model results

# Before: sam (boot/software), model.cfg, sam.pin (boot/data/config),
#         sam.dat (data)
# After:  confclone.log, model.cfg, sam.cor, sam.par, sam.rep, sam.res (model)

library(icesTAF)

mkdir("model")

# Get model executable
exefile <- if(os.linux()) "sam" else "sam.exe"
cp(file.path("boot/software/sam", exefile), "model")

# Get model settings
cp("boot/data/config/*", "model")

# Get model input file
cp("data/sam.dat", "model")

setwd("model")
system("./sam -nr 2 -noinit")
setwd("..")
