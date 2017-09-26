## Convert data to model format, write model input files

## Before: data.RData (data)
## After:  sam.dat (input)

library(icesTAF)
source("utilities_input.R")

mkdir("input")

load("data/data.RData")

## Write the file with data prepared for state-space assessment
data <- write.records(surveys=surveys,
                      residual.fleet=catch.no,
                      prop.mature=prop.mature,
                      stock.mean.weight=stock.mean.weight,
                      catch.mean.weight=catch.mean.weight,
                      dis.mean.weight=dis.mean.weight,
                      land.mean.weight=land.mean.weight,
                      prop.f=prop.f,
                      prop.m=prop.m,
                      natural.mortality=natural.mortality,
                      land.frac=land.frac, file="input/sam.dat")
