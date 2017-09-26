## Prepare tables for report

## Before: catage.csv, smh.csv, wstock.csv, wcatch.csv,
##         maturity.csv, summary.csv, natage.csv,
##         fatage.csv (data, upload)
## After:  catage_rep.csv, smh_rep.csv, wstock_rep.csv, wcatch_rep.csv,
##         maturity_rep.csv, summary_rep.csv, natage_rep.csv,
##         fatage_rep.csv (upload)

library(icesTAF)

mkdir("upload/report")

## latage (trim age, round, transpose)
latage <- read.taf("data/latage_full.csv")
latage$"11" <- rowSums(latage[as.character(11:15)])
latage <- latage[c("Year",as.character(1:11))]
names(latage)[ncol(latage)] <- "+gp"
latage <- round(latage)
latage <- tt(latage)
write.taf(latage, "upload/report/latage_rep.csv")

## datage (trim age, round, transpose)
datage <- read.taf("data/datage_full.csv")
datage$"11" <- rowSums(datage[as.character(11:15)])
datage <- datage[c("Year",as.character(1:11))]
names(datage)[ncol(datage)] <- "+gp"
datage <- round(datage)
datage <- tt(datage)
write.taf(datage, "upload/report/datage_rep.csv")

## catage (trim age, round, transpose)
catage <- read.taf("data/catage_full.csv")
catage$"11" <- rowSums(catage[as.character(11:15)])
catage <- catage[c("Year",as.character(1:11))]
names(catage)[ncol(catage)] <- "+gp"
catage <- round(catage)
catage <- tt(catage)
write.taf(catage, "upload/report/catage_rep.csv")

## wlandings (trim age, round, transpose)
wlandings <- read.taf("data/wlandings_full.csv")
prop <- read.taf("data/latage_full.csv")[as.character(11:15)]
wplus <- rowSums(wlandings[as.character(11:15)] * prop / rowSums(prop))
wplus[is.nan(wplus)] <- 0
wlandings$"11" <- wplus
wlandings <- wlandings[c("Year",as.character(1:11))]
names(wlandings)[ncol(wlandings)] <- "+gp"
wlandings <- round(wlandings, 3)
wlandings <- tt(wlandings)
write.taf(wlandings, "upload/report/wlandings_rep.csv")

## wdiscards (trim age, round, transpose)
wdiscards <- read.taf("data/wdiscards_full.csv")
prop <- read.taf("data/datage_full.csv")[as.character(11:15)]
wplus <- rowSums(wdiscards[as.character(11:15)] * prop / rowSums(prop))
wplus[is.nan(wplus)] <- 0
wdiscards$"11" <- wplus
wdiscards <- wdiscards[c("Year",as.character(1:11))]
names(wdiscards)[ncol(wdiscards)] <- "+gp"
wdiscards <- round(wdiscards, 3)
wdiscards <- tt(wdiscards)
write.taf(wdiscards, "upload/report/wdiscards_rep.csv")

## wcatch (trim age, round, transpose)
wcatch <- read.taf("data/wcatch_full.csv")
prop <- read.taf("data/catage_full.csv")[as.character(11:15)]
wplus <- rowSums(wcatch[as.character(11:15)] * prop / rowSums(prop))
wplus[is.nan(wplus)] <- 0
wcatch$"11" <- wplus
wcatch <- wcatch[c("Year",as.character(1:11))]
names(wcatch)[ncol(wcatch)] <- "+gp"
wcatch <- round(wcatch, 3)
wcatch <- tt(wcatch)
write.taf(wcatch, "upload/report/wcatch_rep.csv")

## catch_sop (round)
catch_sop <- read.taf("upload/input/catch_sop.csv")
catch_sop <- round(catch_sop)
write.taf(catch_sop, "upload/report/catch_sop_rep.csv")

## maturity (round)
maturity <- read.taf("upload/input/maturity.csv")
maturity <- round(maturity, 3)
write.taf(maturity, "upload/report/maturity_rep.csv")

## natmort (round)
natmort <- read.taf("upload/input/natmort.csv")
natmort <- round(natmort, 3)
write.taf(natmort, "upload/report/natmort_rep.csv")

## ibts_1 (round)
ibts_1 <- read.taf("upload/input/ibts_1.csv")
ibts_1 <- round(ibts_1, 2)
write.taf(ibts_1, "upload/report/ibts_1_rep.csv")

## ibts_3 (round)
ibts_3 <- read.taf("upload/input/ibts_3.csv")
ibts_3 <- round(ibts_3, 2)
write.taf(ibts_3, "upload/report/ibts_3_rep.csv")

## fatage (average, round)
fatage <- read.taf("upload/output/fatage.csv")
fatage$"Fbar 2-4" <- rowMeans(fatage[as.character(2:4)])
fatage <- round(fatage, 3)
write.taf(fatage, "upload/report/fatage_rep.csv")

## natage (sum, round)
natage <- read.taf("upload/output/natage.csv")
natage$Total <- rowSums(natage[-1])
natage <- round(natage)
write.taf(natage, "upload/report/natage_rep.csv")

## catage_fit (round)
catage_fit <- read.taf("upload/output/catage_fit.csv")
catage_fit <- round(catage_fit)
write.taf(catage_fit, "upload/report/catage_fit_rep.csv")

## summary (round)
summary <- read.taf("upload/output/summary.csv")
summary <- as.data.frame(mapply(round, summary, digits=c(rep(0,10),3,3,3)))
write.taf(summary, "upload/report/summary_rep.csv")

## catch (multiplier, round)
catch <- read.taf("upload/output/catch.csv")
catch$"Catch multiplier" <- catch$"Total Removal" / catch$Catch
catch$"Catch multiplier"[catch$"Catch multiplier"==1] <- NA
catch <- catch[c(1:4, 6, 5)]
write.taf(catch, "upload/report/catch_rep.csv")

## multiplier (round)
multiplier <- read.taf("upload/output/multiplier.csv")
multiplier <- round(multiplier, 2)
write.taf(multiplier, "upload/report/multiplier_rep.csv")
