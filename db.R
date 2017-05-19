require(icesTAF, quietly=TRUE)
suppressMessages(require(mgcv, quietly=TRUE))
source("support_functions.R") # require(stockassessment)

ftp.remote <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/nscod16-ass02/"
ftp.local <- "../../../ftp/wgnssk/2016/cod-347d/"

dir.create("db", showWarnings=FALSE)
dir.create(paste0(ftp.local,"input"), showWarnings=FALSE, recursive=TRUE)

## Get data
setwd("db")
catch.no <- read.ices(paste0(ftp.remote, "data/cn.dat"))
catch.no <- read.ices(paste0(ftp.remote, "data/cn.dat"))
catch.mean.weight <- read.ices(paste0(ftp.remote, "data/cw.dat"))
dis.mean.weight <- read.ices(paste0(ftp.remote, "data/dw.dat"))
land.mean.weight <- read.ices(paste0(ftp.remote, "data/lw.dat"))
stock.mean.weight <- read.ices(paste0(ftp.remote, "data/sw.dat"))
prop.mature <- read.ices(paste0(ftp.remote, "data/mo_raw.dat"))
natural.mortality <- read.ices(paste0(ftp.remote, "data/nm.dat"))
surveys <- read.surveys(paste0(ftp.remote, "data/survey.dat"))
land.no <- read.ices(paste0(ftp.remote, "data/lf.dat"))
dis.no <- catch.no - land.no
prop.f <- read.ices(paste0(ftp.remote, "data/pf.dat"))
prop.m <- read.ices(paste0(ftp.remote, "data/pm.dat"))
download.file(paste0(ftp.remote,"data/mo_raw.dat"), "mo_raw.dat", quiet=TRUE)

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
head <- readLines("mo_raw.dat", n=5)
file <- "mo.dat"
cat(paste(head[1],"smoothed"), "\n", file=file)
cat(head[2], "\n", file=file, append=TRUE)
cat(head[3], "\n", file=file, append=TRUE)
cat(head[4], "\n", file=file, append=TRUE)
cat(head[5], "\n", file=file, append=TRUE)
write.table(round(prop.mature,4), file=file, row.names=FALSE, col.names=FALSE, append=TRUE)
setwd("..")

## Modify to 7+ data
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

## Prepare tables for export
latage <- xtab2taf(land.no)
datage <- xtab2taf(catch.no - land.no)
catage <- xtab2taf(catch.no)
wlandings <- xtab2taf(land.mean.weight)
wdiscards <- xtab2taf(dis.mean.weight)
wcatch <- xtab2taf(catch.mean.weight)
maturity <- xtab2taf(prop.mature)
natmort <- xtab2taf(natural.mortality)
ibts_1 <- xtab2taf(surveys[[1]])
ibts_3 <- xtab2taf(surveys[[2]])

## Write tables to local FTP directory
write.taf(latage, paste0(ftp.local,"input/latage.csv")) # 2a
write.taf(datage, paste0(ftp.local,"input/datage.csv")) # 2b
write.taf(catage, paste0(ftp.local,"input/catage.csv")) # 2c
write.taf(wlandings, paste0(ftp.local,"input/wlandings.csv")) # 3a
write.taf(wdiscards, paste0(ftp.local,"input/wdiscards.csv")) # 3b
write.taf(wcatch, paste0(ftp.local,"input/wcatch.csv"))       # 3c
write.taf(maturity, paste0(ftp.local,"input/maturity.csv")) # 5a
write.taf(natmort, paste0(ftp.local,"input/natmort.csv"))   # 5b
write.taf(ibts_1, paste0(ftp.local,"input/ibts_1.csv"))   # 6a
write.taf(ibts_3, paste0(ftp.local,"input/ibts_3.csv"))   # 6b

## Save objects required by input.R
save(surveys, catch.no, prop.mature,
     stock.mean.weight, catch.mean.weight, dis.mean.weight, land.mean.weight,
     prop.f, prop.m, natural.mortality, land.frac,
     file="db/input.RData")
