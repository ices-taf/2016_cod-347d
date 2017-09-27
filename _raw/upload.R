## Upload raw data to TAF database

## Before: cn.dat, cw.dat, dw.dat, lw.dat, sw.dat, mo_raw.dat, nm.dat,
##         survey.dat, lf.dat, pf.dat, pm.dat (stockassessment.org)
## After:  cn.dat, cw.dat, dw.dat, lw.dat, sw.dat, mo_raw.dat, nm.dat,
##         survey.dat, lf.dat, pf.dat, pm.dat (TAF database)

library(icesTAF)

url <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/data/"

## Download from stockassessment.org
download(paste0(url, "cn.dat"))
download(paste0(url, "cw.dat"))
download(paste0(url, "dw.dat"))
download(paste0(url, "lw.dat"))
download(paste0(url, "sw.dat"))
download(paste0(url, "mo_raw.dat"))
download(paste0(url, "nm.dat"))
download(paste0(url, "survey.dat"))
download(paste0(url, "lf.dat"))
download(paste0(url, "pf.dat"))
download(paste0(url, "pm.dat"))
download(paste0(url, "mo_raw.dat"))

## Upload to TAF database
upload("2016_cod-347d", "raw", "cn.dat")
upload("2016_cod-347d", "raw", "cw.dat")
upload("2016_cod-347d", "raw", "dw.dat")
upload("2016_cod-347d", "raw", "lw.dat")
upload("2016_cod-347d", "raw", "sw.dat")
upload("2016_cod-347d", "raw", "mo_raw.dat")
upload("2016_cod-347d", "raw", "nm.dat")
upload("2016_cod-347d", "raw", "survey.dat")
upload("2016_cod-347d", "raw", "lf.dat")
upload("2016_cod-347d", "raw", "pf.dat")
upload("2016_cod-347d", "raw", "pm.dat")
upload("2016_cod-347d", "raw", "mo_raw.dat")

## Remove files
unlink("*.dat")
