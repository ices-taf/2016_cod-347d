## Prepare plots for report

## Before: summary.csv (output)
## After:  biomass.png, f_mortality.png (report)

library(icesTAF)
library(areaplot)

mkdir("report")

## Change unit
summary <- read.taf("output/summary.csv")
x <- div(summary, "SSB", grep=TRUE)

## Plots
tafpng("biomass")
confplot(x[c("Year","SSB_lo","SSB_hi")], ylim=lim(x$SSB_hi), yaxs="i",
         ylab="SSB (1000 t)", main="Biomass")
lines(x$Year, x$SSB)
dev.off()

tafpng("f_mortality")
confplot(x[c("Year","Fbar_lo","Fbar_hi")], ylim=lim(x$Fbar_hi), yaxs="i",
         ylab="Fbar (2-4)", main="Fishing mortality")
lines(x$Year, x$Fbar)
dev.off()
