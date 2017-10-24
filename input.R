## Convert data to model format, write model input files

## Before: catage.csv, ibts1.csv, ibts3.csv, landfrac.csv, maturity.csv,
##         natmort.csv, propf.csv, propm.csv, surveytime.csv, wcatch.csv,
##         wdiscards.csv, wlandings.csv, wstock.csv (data)
## After:  sam.dat (input)

library(icesTAF)
source("utilities_input.R")

mkdir("input")

ibts1 <- read.taf("data/ibts1.csv")
ibts3 <- read.taf("data/ibts3.csv")
surveytime <- read.taf("data/surveytime.csv")
catage <- read.taf("data/catage.csv")
maturity <- read.taf("data/maturity.csv")
wstock <- read.taf("data/wstock.csv")
wcatch <- read.taf("data/wcatch.csv")
wdiscards <- read.taf("data/wdiscards.csv")
wlandings <- read.taf("data/wlandings.csv")
propf <- read.taf("data/propf.csv")
propm <- read.taf("data/propm.csv")
natmort <- read.taf("data/natmort.csv")
landfrac <- read.taf("data/landfrac.csv")

t2m <- function(x) as.matrix(taf2xtab(x))

surveys <- list(ibts1=t2m(ibts1), ibts3=t2m(ibts3))
attr(surveys$ibts1, "time") <- surveytime$ibts1
attr(surveys$ibts3, "time") <- surveytime$ibts3

## Write the file with data prepared for state-space assessment
input <- write.records(surveys=surveys,
                       residual.fleet=t2m(catage),
                       prop.mature=t2m(maturity),
                       stock.mean.weight=t2m(wstock),
                       catch.mean.weight=t2m(wcatch),
                       dis.mean.weight=t2m(wdiscards),
                       land.mean.weight=t2m(wlandings),
                       prop.f=t2m(propf),
                       prop.m=t2m(propm),
                       natural.mortality=t2m(natmort),
                       land.frac=t2m(landfrac),
                       file="input/sam.dat")
