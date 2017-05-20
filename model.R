## Run analysis, write model results

library(icesTAF)

mkdir("model")

ftp <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/"

download.file(paste0(ftp,"run/model.cfg"), "model/model.cfg", quiet=TRUE)
download.file(paste0(ftp,"run/sam.pin"), "model/sam.pin", quiet=TRUE)

sam <- if(.Platform$OS.type == "unix") "sam" else "sam.exe"
download.file(paste0(ftp,"/run/",sam), paste0("model/",sam), quiet=TRUE)
Sys.chmod(paste0("model/", sam))

cp("input/sam.dat", "model")

setwd("model")
system("sam -nr 2 -noinit")
