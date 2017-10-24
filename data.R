## Preprocess data, write TAF input tables

## Before: cn.dat, cw.dat, dw.dat, lw.dat, sw.dat, mo_raw.dat, nm.dat,
##         survey.dat, lf.dat, pf.dat, pm.dat (TAF database)
## After:  catage.csv, catage_full.csv, catch_sop.csv, datage.csv,
##         datage_full.csv, ibts1.csv, ibts3.csv, landfrac.csv, latage.csv,
##         latage_full.csv, maturity.csv, maturity_full.csv, natmort.csv,
##         propf.csv, propm.csv, wcatch.csv, wcatch_full.csv, wdiscards.csv,
##         wdiscards_full.csv, wlandings.csv, wlandings_full.csv, wstock.csv,
##         wstock_full.csv (data)

library(icesTAF)
suppressMessages(library(mgcv))
source("utilities_input.R")

url <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/data/"

mkdir("data")

## Get data
setwd("data")
catch.no <- read.ices(paste0(url, "cn.dat"))
catch.mean.weight <- read.ices(paste0(url, "cw.dat"))
dis.mean.weight <- read.ices(paste0(url, "dw.dat"))
land.mean.weight <- read.ices(paste0(url, "lw.dat"))
stock.mean.weight <- read.ices(paste0(url, "sw.dat"))
prop.mature <- read.ices(paste0(url, "mo_raw.dat"))
natural.mortality <- read.ices(paste0(url, "nm.dat"))
surveys <- read.surveys(paste0(url, "survey.dat"))
land.no <- read.ices(paste0(url, "lf.dat"))
dis.no <- catch.no - land.no
prop.f <- read.ices(paste0(url, "pf.dat"))
prop.m <- read.ices(paste0(url, "pm.dat"))
## full datasets, including all ages
catch.no.full <- catch.no
land.no.full <- land.no
land.mean.weight.full <- land.mean.weight
dis.mean.weight.full <- dis.mean.weight
catch.mean.weight.full <- catch.mean.weight
stock.mean.weight.full <- stock.mean.weight

## Smooth maturity
skipYears <- c(1:10, 54)
columnsToSmooth <- 1:5
mo <- prop.mature[-c(skipYears),]
for(cc in columnsToSmooth){
  ww <- mo[,cc]
  tt <- 1:length(ww)
  tmp <- gam(ww ~ s(tt))
  mo[,cc] <- predict(tmp)
}
prop.mature[-c(skipYears),] <- mo
prop.mature[54,] <- prop.mature[53,]
prop.mature.full <- prop.mature

## Modify to 6+ data
cutage <- 6
low <- 1
GE <- which(as.numeric(colnames(catch.no)) >= cutage)
E <- which(as.numeric(colnames(catch.no)) == cutage)
w <- catch.no[,GE] / rowSums(catch.no[,GE])
wex <- rbind(w, w[nrow(w),])
wD <- dis.no[,GE] / ifelse(rowSums(dis.no[,GE])>0, rowSums(dis.no[,GE]), 1)
wL <- land.no[,GE] / rowSums(land.no[,GE])
catch.no[,E] <- rowSums(catch.no[,GE])
catch.no <- catch.no[,low:E]
prop.mature[,E] <- rowSums(prop.mature[,GE] * wex)
prop.mature <- prop.mature[,low:E]
stock.mean.weight[,E] <- rowSums(stock.mean.weight[,GE] * wex)
stock.mean.weight <- stock.mean.weight[,low:E]
catch.mean.weight[,E] <- rowSums(catch.mean.weight[,GE] * w)
catch.mean.weight <- catch.mean.weight[,low:E]
dis.mean.weight[,E] <- rowSums(dis.mean.weight[,GE] * wD)
dis.mean.weight <- dis.mean.weight[,low:E]
land.mean.weight[,E] <- rowSums(land.mean.weight[,GE] * wL)
land.mean.weight <- land.mean.weight[,low:E]
natural.mortality[,E] <- rowSums(natural.mortality[,GE] * wex)
natural.mortality <- natural.mortality[,low:E]
land.no[,E] <- rowSums(land.no[,GE])
land.no <- land.no[,low:E]
land.frac <- ifelse(catch.no>0, land.no/catch.no, 1)
prop.f <- prop.f[,low:E]
prop.m <- prop.m[,low:E]

## Catch as sum of products
sop <- data.frame(Landings=rowSums(land.no * land.mean.weight),
                  Discards=rowSums((catch.no-land.no) * dis.mean.weight))
sop$Catch <- sop$Landings + sop$Discards
## sop$Catch2 <- rowSums(catch.no * catch.mean.weight)

## Prepare tables for export
latage <- xtab2taf(land.no)
datage <- xtab2taf(catch.no - land.no)
catage <- xtab2taf(catch.no)
wlandings <- xtab2taf(land.mean.weight)
wdiscards <- xtab2taf(dis.mean.weight)
wcatch <- xtab2taf(catch.mean.weight)
wstock <- xtab2taf(stock.mean.weight)
maturity <- xtab2taf(prop.mature)
natmort <- xtab2taf(natural.mortality)
ibts1 <- xtab2taf(surveys[[1]])
ibts3 <- xtab2taf(surveys[[2]])
surveytime <- as.data.frame(lapply(surveys, attr, "time"),
                            col.names=c("ibts1","ibts3"))
catch_sop <- xtab2taf(sop)
landfrac <- xtab2taf(land.frac)
propf <- xtab2taf(prop.f)
propm <- xtab2taf(prop.m)
## full datasets, including all ages
latage_full <- xtab2taf(land.no.full)
datage_full <- xtab2taf(catch.no.full - land.no.full)
catage_full <- xtab2taf(catch.no.full)
wlandings_full <- xtab2taf(land.mean.weight.full)
wdiscards_full <- xtab2taf(dis.mean.weight.full)
wcatch_full <- xtab2taf(catch.mean.weight.full)
wstock_full <- xtab2taf(stock.mean.weight.full)
maturity_full <- xtab2taf(prop.mature.full)

## Write tables to data directory
write.taf(latage, "latage.csv") # 2a
write.taf(datage, "datage.csv") # 2b
write.taf(catage, "catage.csv") # 2c
write.taf(wlandings, "wlandings.csv") # 3a
write.taf(wdiscards, "wdiscards.csv") # 3b
write.taf(wcatch, "wcatch.csv")       # 3c
write.taf(wstock, "wstock.csv")
write.taf(catch_sop, "catch_sop.csv") # 4
write.taf(landfrac, "landfrac.csv")
write.taf(maturity, "maturity.csv") # 5a
write.taf(natmort, "natmort.csv")   # 5b
write.taf(ibts1, "ibts1.csv") # 6a
write.taf(ibts3, "ibts3.csv") # 6b
write.taf(surveytime, "surveytime.csv")
write.taf(propf, "propf.csv")
write.taf(propm, "propm.csv")
## full datasets, including all ages
write.taf(latage_full, "latage_full.csv")
write.taf(datage_full, "datage_full.csv")
write.taf(catage_full, "catage_full.csv")
write.taf(wlandings_full, "wlandings_full.csv")
write.taf(wdiscards_full, "wdiscards_full.csv")
write.taf(wcatch_full, "wcatch_full.csv")
write.taf(wstock_full, "wstock_full.csv")
write.taf(maturity_full, "maturity_full.csv")
setwd("..")
