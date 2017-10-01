## Gather TAF input and output tables to be uploaded

## Before: catage.csv, datage.csv, data.RData, ibts_1.csv, ibts_3.csv,
##         latage.csv, maturity.csv, mo.dat, mo_raw.dat, natmort.csv,
##         wcatch.csv, wdiscards.csv, wlandings.csv,, catage_fit.csv, catch.csv,
##         fatage.csv, multiplier.csv, natage.csv, summary.csv (data, output)
## After:  catage.csv, datage.csv, data.RData, ibts_1.csv, ibts_3.csv,
##         latage.csv, maturity.csv, mo.dat, mo_raw.dat, natmort.csv,
##         wcatch.csv, wdiscards.csv, wlandings.csv,, catage_fit.csv, catch.csv,
##         fatage.csv, multiplier.csv, natage.csv, summary.csv (upload)

library(icesTAF)

mkdir("upload/input")
mkdir("upload/output")

## All CSV except *_full.csv
data <- Sys.glob("data/*.csv")
data <- grep("_full.csv", data, value=TRUE, invert=TRUE)

cp(data, "upload/input")
cp("output/*.csv", "upload/output")
