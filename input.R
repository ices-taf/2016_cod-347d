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

taf2sam <- function(x)
{
  y <- as.matrix(taf2xtab(x))
  colnames(y) <- sub("\\+", "", colnames(y))
  y
}

surveys <- list(ibts1=taf2sam(ibts1), ibts3=taf2sam(ibts3))
attr(surveys$ibts1, "time") <- surveytime$ibts1
attr(surveys$ibts3, "time") <- surveytime$ibts3

## Write the file with data prepared for state-space assessment
input <- write.records(surveys=surveys,
                       residual.fleet=taf2sam(catage),
                       prop.mature=taf2sam(maturity),
                       stock.mean.weight=taf2sam(wstock),
                       catch.mean.weight=taf2sam(wcatch),
                       dis.mean.weight=taf2sam(wdiscards),
                       land.mean.weight=taf2sam(wlandings),
                       prop.f=taf2sam(propf),
                       prop.m=taf2sam(propm),
                       natural.mortality=taf2sam(natmort),
                       land.frac=taf2sam(landfrac),
                       file="input/sam.dat")
