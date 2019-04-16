## Prepare tables for report

## Before: catage_full.csv, catch_sop.csv, datage_full.csv, ibts1.csv,
##         ibts3.csv, latage_full.csv, maturity.csv, natmort.csv,
##         wcatch_full.csv, wdiscards_full.csv, wlandings_full.csv (data),
##         catage_fit.csv, catch_est.csv, fatage.csv, multiplier.csv,
##         natage.csv, summary.csv (output)
## After:  catage.csv, catage_fit.csv, catch_est.csv, catch_sop.csv, datage.csv,
##         fatage.csv, ibts1.csv, ibts3.csv, latage.csv, maturity.csv,
##         multiplier.csv, natage.csv, natmort.csv, summary.csv, wcatch.csv,
##         wdiscards.csv, wlandings.csv (report)

library(icesTAF)

mkdir("report")

## latage (trim age, round, transpose)
latage <- read.taf("data/latage_full.csv")
latage$"11" <- rowSums(latage[as.character(11:15)])
latage <- latage[c("Year",as.character(1:11))]
names(latage)[ncol(latage)] <- "+gp"
latage <- round(latage)
latage <- tt(latage, TRUE)
write.taf(latage, dir="report")

## datage (trim age, round, transpose)
datage <- read.taf("data/datage_full.csv")
datage$"11" <- rowSums(datage[as.character(11:15)])
datage <- datage[c("Year",as.character(1:11))]
names(datage)[ncol(datage)] <- "+gp"
datage <- round(datage)
datage <- tt(datage, TRUE)
write.taf(datage, dir="report")

## catage (trim age, round, transpose)
catage <- read.taf("data/catage_full.csv")
catage$"11" <- rowSums(catage[as.character(11:15)])
catage <- catage[c("Year",as.character(1:11))]
names(catage)[ncol(catage)] <- "+gp"
catage <- round(catage)
catage <- tt(catage, TRUE)
write.taf(catage, dir="report")

## wlandings (trim age, round, transpose)
wlandings <- read.taf("data/wlandings_full.csv")
prop <- read.taf("data/latage_full.csv")[as.character(11:15)]
wplus <- rowSums(wlandings[as.character(11:15)] * prop / rowSums(prop))
wplus[is.nan(wplus)] <- 0
wlandings$"11" <- wplus
wlandings <- wlandings[c("Year",as.character(1:11))]
names(wlandings)[ncol(wlandings)] <- "+gp"
wlandings <- round(wlandings, 3)
wlandings <- tt(wlandings, TRUE)
write.taf(wlandings, dir="report")

## wdiscards (trim age, round, transpose)
wdiscards <- read.taf("data/wdiscards_full.csv")
prop <- read.taf("data/datage_full.csv")[as.character(11:15)]
wplus <- rowSums(wdiscards[as.character(11:15)] * prop / rowSums(prop))
wplus[is.nan(wplus)] <- 0
wdiscards$"11" <- wplus
wdiscards <- wdiscards[c("Year",as.character(1:11))]
names(wdiscards)[ncol(wdiscards)] <- "+gp"
wdiscards <- round(wdiscards, 3)
wdiscards <- tt(wdiscards, TRUE)
write.taf(wdiscards, dir="report")

## wcatch (trim age, round, transpose)
wcatch <- read.taf("data/wcatch_full.csv")
prop <- read.taf("data/catage_full.csv")[as.character(11:15)]
wplus <- rowSums(wcatch[as.character(11:15)] * prop / rowSums(prop))
wplus[is.nan(wplus)] <- 0
wcatch$"11" <- wplus
wcatch <- wcatch[c("Year",as.character(1:11))]
names(wcatch)[ncol(wcatch)] <- "+gp"
wcatch <- round(wcatch, 3)
wcatch <- tt(wcatch, TRUE)
write.taf(wcatch, dir="report")

## catch.sop (round)
catch.sop <- read.taf("data/catch_sop.csv")
catch.sop <- round(catch.sop)
write.taf(catch.sop, dir="report")

## maturity (round)
maturity <- read.taf("data/maturity.csv")
maturity <- round(maturity, 3)
write.taf(maturity, dir="report")

## natmort (round)
natmort <- read.taf("data/natmort.csv")
natmort <- round(natmort, 3)
write.taf(natmort, dir="report")

## ibts1 (round)
ibts1 <- read.taf("data/ibts1.csv")
ibts1 <- round(ibts1, 2)
write.taf(ibts1, dir="report")

## ibts3 (round)
ibts3 <- read.taf("data/ibts3.csv")
ibts3 <- round(ibts3, 2)
write.taf(ibts3, dir="report")

## fatage (average, round)
fatage <- read.taf("output/fatage.csv")
fatage$"Fbar 2-4" <- rowMeans(fatage[as.character(2:4)])
fatage <- round(fatage, 3)
write.taf(fatage, dir="report")

## natage (sum, round)
natage <- read.taf("output/natage.csv")
natage$Total <- rowSums(natage[-1])
natage <- round(natage)
write.taf(natage, dir="report")

## catage.fit (round)
catage.fit <- read.taf("output/catage_fit.csv")
catage.fit <- round(catage.fit)
write.taf(catage.fit, dir="report")

## summary (trim year, round)
summary <- read.taf("output/summary.csv")
summary[nrow(summary), grep("Rec|TSB",names(summary))] <- NA
summary <- rnd(summary, "Rec|TSB|SSB|Removals", grep=TRUE)
summary <- rnd(summary, "Fbar", 3, grep=TRUE)
names(summary) <- c("Year", "Recruits age 1 ('000)", "Low", "High",
                    "TSB (tons)", "Low", "High",
                    "SSB (tons)", "Low", "High",
                    "Total removals (tons)", "Low", "High",
                    "Fbar 2-4", "Low", "High")
write.taf(summary, dir="report")

## catch.est (multiplier, round)
catch.est <- read.taf("output/catch_est.csv")
names(catch.est)[names(catch.est)=="TotalRemovals"] <- "Total Removals"
catch.est$"Catch multiplier" <- catch.est$"Total Removals" / catch.est$Catch
catch.est$"Catch multiplier"[catch.est$"Catch multiplier"==1] <- NA
catch.est <- catch.est[c("Year","Landings","Discards","Catch",
                         "Catch multiplier","Total Removals")]
catch.est <- rnd(catch.est, c("Landings","Discards","Catch","Total Removals"))
catch.est <- rnd(catch.est, "Catch multiplier", 2)
write.taf(catch.est, dir="report")

## multiplier (round)
multiplier <- read.taf("output/multiplier.csv")
names(multiplier)[names(multiplier)=="CatchMultiplier"] <- "Catch multiplier"
multiplier <- round(multiplier, 2)
write.taf(multiplier, dir="report")
