## Run analysis, write model results

## Before: model.cfg, sam, sam.pin (TAF database), sam.dat (input)
## After:  confclone.log, model.cfg, sam.cor, sam.par, sam.rep, sam.res (model)

library(icesTAF)

mkdir("model")

url <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/run/"

download(paste0(url,"model.cfg"), "model")
download(paste0(url,"sam.pin"), "model")

cp("input/sam.dat", "model")

setwd("model")

if (.Platform$OS.type == "unix") {
  download(paste0(url, "sam"))
  system("./sam -nr 2 -noinit")
} else {
  winurl <- "https://github.com/ices-taf/ftp/raw/master/wgnssk/2016/cod-347d/model/"
  download(paste0(winurl, "sam.exe"))
  system("cmd.exe", input = "sam.exe -nr 2 -noinit")
}

setwd("..")
