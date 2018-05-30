## ------------------
## Name: AccuracyAnal.R
## Program version: R 3.1.0
## Dependencies: plyr, ggplot2, grid, scales, lattice, RColorBrewer, modeest, xtable, gridExtra, quantreg
## Function file(s): Functions_RadMatch10km.r
## Author: J.H. Belle
## Purpose: Record of final accuracy analysis figures, modeling, and tables for 3km and 10 km products
## ------------------

## ------------------
## Starting from AllCollocs - Imagery and table creation for paper
## ------------------

# Read AllCollocs back in and load libraries and self-defined functions
AllCollocs <- read.csv("H:/Rotation_Yang/AllCollocs.csv")
library(ggplot2)
library(grid)
library(scales)
library(xtable)
library(gridExtra)
library(quantreg)
library(plyr)
library(cowplot)
source("H:/Rotation_Yang/Fuctions_RadMatch10km.r")
#par(mai=c(0.5, 0.5, 0.5, 0.5))

## -------
# Process data for plots
## -------
AllCollocs$East2 <- cut(AllCollocs$East, breaks=c(-0.5,0.5,1.5), labels=c("West", "East"))
AllCollocs$ResLab <- paste(AllCollocs$Resolution, "km")
AllCollocs$QAClab <- paste("QAC", AllCollocs$QACcode)
AllCollocs$PImpervMean <- ifelse(AllCollocs$PImpervMean > 100, NA, AllCollocs$PImpervMean)
AllCollocs$AODerr <- AllCollocs$ModAOD - AllCollocs$AeroAOD
AllCollocs$Month <- strftime(strptime(paste(AllCollocs$JulianDate, AllCollocs$Year, sep="-"), "%j-%Y"), "%b")
AllCollocs$MonthNum <- as.numeric(strftime(strptime(paste(AllCollocs$JulianDate, AllCollocs$Year, sep="-"), "%j-%Y"), "%m"))
AllCollocs$Season <- ifelse(AllCollocs$Month %in% c("Jan", "Feb", "Dec"), "Winter", ifelse(AllCollocs$Month %in% c("Mar", "Apr", "May"), "Spring", ifelse(AllCollocs$Month %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
AllCollocs$Red2 <- ifelse(AllCollocs$Hematite_red == 0, NA, AllCollocs$Hematite_red)
AllCollocs$Algorithm <- ifelse(AllCollocs$Algorithm == "B", "M", as.character(AllCollocs$Algorithm))
AllCollocs$CombResAlg <- ifelse(AllCollocs$Algorithm=="DT" & AllCollocs$ResLab == "3 km", "3 km DT", paste("10 km ", as.character(AllCollocs$Algorithm), sep=""))
AllCollocs$CombResAlg <- ifelse(AllCollocs$CombResAlg == "10 km M", "10 km DB-DT", AllCollocs$CombResAlg)
AllCollocs$CombResAlg <- as.ordered(AllCollocs$CombResAlg)
AllCollocs$CombResAlg <- reorder(AllCollocs$CombResAlg, ifelse(AllCollocs$CombResAlg == "3 km DT", 1, ifelse(AllCollocs$CombResAlg == "10 km DT", 2, ifelse(AllCollocs$CombResAlg == "10 km DB", 4, 3))), median)

#SubCollocs <- subset(AllCollocs, AllCollocs$QAClab == "QAC 3" | (AllCollocs$QAClab == "QAC 1" & AllCollocs$CombResAlg == "10 km DB"))
SubCollocs <- subset(AllCollocs, AllCollocs$QAClab == "QAC 3")
SubCollocs$LandUse <- cut(SubCollocs$NLCDMode, breaks=c(18, 26,36,46,56,76,86,96), labels=c("Developed", "Barren", "Forest", "Shrub", "Grass", "Cultivated", "Wetland"))
SubCollocs$CombResAlgQAC <- ifelse(SubCollocs$QAClab=="QAC 1", "10 km DB; QAC = 1", paste(SubCollocs$CombResAlg, "QAC = 3", sep="; "))

LinModCoefs <- ddply(AllCollocs, .(East2, QAClab, CombResAlg), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(AllCollocs, .(East2,QAClab,CombResAlg), PercEE, Mod="ModAOD", Aero="AeroAOD")
PropEE$MeetsPreLaunch <- cut(PropEE$PInEE, breaks=c(0,0.68,1), labels=c("No","Yes"))
OneOneL <- cbind.data.frame(LinModCoefs[,1:3], rep(0,22), rep(1,22), rep(NA,22))
colnames(OneOneL) <- c("East2", "QAClab", "CombResAlg", "a", "b", "adjR2")
UpEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(0.05,22), rep(1.15,22), rep(NA,22))
colnames(UpEEL) <- c("East2", "QAClab", "CombResAlg", "a", "b", "adjR2")
DownEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(-0.05,22), rep(0.85,22), rep(NA,22))
colnames(DownEEL) <- c("East2", "QAClab", "CombResAlg", "a", "b", "adjR2")
ABLines <- rbind.data.frame(LinModCoefs, OneOneL, UpEEL, DownEEL)
ABLines$lntyp <- c(rep("dotted",22), rep("solid",22), rep("dotdash",44))
rm(OneOneL, UpEEL, DownEEL)
Length <- aggregate(AODerr ~ CombResAlg + QAClab + East2, data=AllCollocs, length)
AODerr <- aggregate(AODerr ~ CombResAlg + QAClab + East2, data=AllCollocs, median)
AODerr <- merge(Length, AODerr, by=c("CombResAlg", "QAClab", "East2"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("CombResAlg", "QAClab", "East2"))
TableRegs <- merge(TableRegs, PropEE, by=c("CombResAlg", "QAClab", "East2"))

#toLatex.xtable(xtable(TableRegs[,1:12]), include.rownames=F)


rm(AODerr, PropEE, LinModCoefs, Length)

## -------
# First figure - panel of MODIS vs. AERONET AOD plots - by region - removed from final version of paper
## -------

pdf("H:/Rotation_Yang/Imagery/PaperImages/PaneledModxAero.pdf", family="Times", width=7.48, height=4.53)
ggplot(AllCollocs, aes(x=AeroAOD, y=ModAOD)) +
  geom_bin2d(bins=40) +
  geom_abline(data=subset(ABLines, lntyp!="dotted"), aes(intercept=a, slope=b, linetype=as.factor(lntyp)), show_guide=T) +
  #geom_text(data=TableRegs, aes(y=0.05,x=0.4, label=paste("R^2==", round(adjR2, digits=2))), size=3, parse=T) +
  geom_point(data=TableRegs, aes(x=0.48, y=0.05, shape=MeetsPreLaunch)) +
  scale_shape_discrete(">68% Obs. \nin EE?")  +
  scale_y_continuous("MODIS AOD", limits=c(0,0.5), breaks=c(0,0.2,0.4)) +
  scale_x_continuous("AERONET AOD", limits=c(0,0.5), breaks=c(0,0.2,0.4)) +
  scale_fill_gradient2("No. \nObs.", trans="log", low="dodgerblue", mid="gold", high="darkseagreen", midpoint=2.7, space="Lab", breaks=c(1,10,40,100,400)) +
  scale_linetype_identity("", guide="legend", labels=c("solid"="1-1 line", "dotted"="Best fit", "dotdash"="EE")) +
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  facet_grid(CombResAlg ~ QAClab + East2) +
  theme_classic(base_size=8)
dev.off()

rm(ABLines)
## -------
# Second figure - AOD error (MODIS AOD - AERONET AOD) vs. predictor values - by region, algorithm, QACcode, and resolution for sources of bias associated with flight geometry
## -------

##----
# Scattering Angle
##----

SubCollocs$ScatterCat <- cut(SubCollocs$Scatter, quantile(SubCollocs$Scatter, seq(0,1,0.2)))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + ScatterCat, data=SubCollocs, quantile)
CatsScatSub <- aggregate(Scatter ~ CombResAlgQAC + East2 + ScatterCat, data=SubCollocs, median)
CatsScatSub <- merge(CatsScatSub, CatsMed, by=c("CombResAlgQAC", "East2", "ScatterCat"))
CatsScatSub <- cbind.data.frame(CatsScatSub[,1:4], CatsScatSub$AODerr[,2:4])
colnames(CatsScatSub) <- c("CombResAlgQAC", "East2", "ScatterCat", "ScatterCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
ScatterBinsPltShort <- ggplot(CatsScatSub, aes(x=ScatterCatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=Scatter, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=5), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous(expression(paste("Retrieval error ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Scattering Angle", breaks=c(80,120,160)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none", axis.title.y=element_text(margin=margin(0,10,0,0)))
rm(CatsMed)

##----
## Solar Zenith Angle
##----

SubCollocs$SolZCat <- cut(SubCollocs$SolarZenith, quantile(SubCollocs$SolarZenith, seq(0,1,0.2)))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + SolZCat, data=SubCollocs, quantile)
CatsSolZSub <- aggregate(SolarZenith ~ CombResAlgQAC + East2 + SolZCat, data=SubCollocs, median)
CatsSolZSub <- merge(CatsSolZSub, CatsMed, by=c("CombResAlgQAC", "East2", "SolZCat"))
CatsSolZSub <- cbind.data.frame(CatsSolZSub[,1:4], CatsSolZSub$AODerr[,2:4])
colnames(CatsSolZSub) <- c("CombResAlgQAC", "East2", "SolZCat", "SolZCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
SolZBinsPltShort <- ggplot(CatsSolZSub, aes(x=SolZCatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=SolarZenith, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=3), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Solar Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none", axis.title.y=element_text(margin=margin(0,13,0,0)))
rm(CatsMed)

##----
## Sensor Zenith Angle
##----

SubCollocs$SenZCat <- cut(SubCollocs$SensorZenith, quantile(SubCollocs$SensorZenith, seq(0,1,0.2)))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + SenZCat, data=SubCollocs, quantile)
CatsSenZSub <- aggregate(SensorZenith ~ CombResAlgQAC + East2 + SenZCat, data=SubCollocs, median)
CatsSenZSub <- merge(CatsSenZSub, CatsMed, by=c("CombResAlgQAC", "East2", "SenZCat"))
CatsSenZSub <- cbind.data.frame(CatsSenZSub[,1:4], CatsSenZSub$AODerr[,2:4])
colnames(CatsSenZSub) <- c("CombResAlgQAC", "East2", "SenZCat", "SenZCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
SenZBinsPltShort <- ggplot(CatsSenZSub, aes(x=SenZCatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=SensorZenith, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=3), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Sensor Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black"), labels=c("3 km DT; QAC = 3"="3 km DT", "10 km DT; QAC = 3"="10 km DT", "10 km DB-DT; QAC = 3"="10 km DB-DT", "10 km DB; QAC = 3"="10 km DB", "10 km DB; QAC = 1"="10 km DB 2")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7"), labels=c("3 km DT; QAC = 3"="3 km DT", "10 km DT; QAC = 3"="10 km DT", "10 km DB-DT; QAC = 3"="10 km DB-DT", "10 km DB; QAC = 3"="10 km DB", "10 km DB; QAC = 1"="10 km DB 2")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="bottom", axis.title.y=element_text(margin=margin(0,13,0,0)))
AODProdLegend <- g_legend(SenZBinsPltShort)
SenZBinsPltShort <- SenZBinsPltShort+theme(legend.position='none')
rm(CatsMed)

AnglePltShort <- plot_grid(ScatterBinsPltShort, SolZBinsPltShort, SenZBinsPltShort, labels=c('(a) ', '(b) ', '(c) '), ncol=3, label_size=10)
AnglePltShort <- arrangeGrob(AnglePltShort, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

# Make pdf for separate submission
#pdf("H:/Rotation_Yang/Imagery/PaperImages/AngesxAODerr_Bins_Short_QAC3.pdf", family="Times", width=7.48, height=4.53)
#grid.draw(AnglePltShort)
#dev.off()

# Make jpeg for document insertion - 300 dpi is the most word can really handle
jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/AnglesxAODerr.jpg", width = 6, height=4.5, units="in", res=300, pointsize=10)
grid.draw(AnglePltShort)
dev.off()

# Plot 4

SubCollocs$CombResAlg2 <- ifelse(SubCollocs$CombResAlg == "3 km DT", "(a) 3 km DT", ifelse(SubCollocs$CombResAlg == "10 km DT", "(b) 10 km DT", ifelse(SubCollocs$CombResAlg == "10 km DB-DT", "(c) 10 km DB-DT", "(d) 10 km DB")))
SeasonBox <- ggplot(subset(SubCollocs, SubCollocs$QAClab=="QAC 3"), aes(x=as.factor(MonthNum), y=AODerr, ymin=quantile(AODerr, probs=0.05), ymax=quantile(AODerr, probs=0.95))) + geom_boxplot(outlier.colour = NA) + geom_hline(aes(yintercept=0), linetype="dotted") + scale_y_continuous(expression(paste("Retrieval error ( ", tau[M], "-", tau[A], ")")), limits=c(-0.2,0.2)) + scale_x_discrete("Month of Year", breaks=c("3", "6", "9"), labels=c("Mar", "Jun", "Sep")) + facet_wrap(c("CombResAlg2"), nrow=2, ncol=2, dir="v") + theme_minimal(base_size = 8)

jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/MOYxAODerr.jpg", width=3.74, height=4.53, units="in", res=300)
SeasonBox
dev.off()

# Plot 5

SubCollocs$NDVICat <- cut(SubCollocs$NDVI, quantile(SubCollocs$NDVI, seq(0,1,0.2), na.rm=T))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + NDVICat, data=SubCollocs, quantile, na.rm=T)
CatsNDVISub <- aggregate(NDVI ~ CombResAlgQAC + East2 + NDVICat, data=SubCollocs, median, na.rm=T)
CatsNDVISub <- merge(CatsNDVISub, CatsMed, by=c("CombResAlgQAC", "East2", "NDVICat"))
CatsNDVISub <- cbind.data.frame(CatsNDVISub[,1:4], CatsNDVISub$AODerr[,2:4])
colnames(CatsNDVISub) <- c("CombResAlgQAC", "East2", "NDVICat", "NDVICatMed", "AODerr25p", "AODerr50p", "AODerr75p")
NDVIBinsPltShort <- ggplot(CatsNDVISub, aes(x=NDVICatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=NDVI, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=0.01), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous(expression(paste("Retrieval error ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("NDVI", limits=c(0.05,0.95), breaks=c(0.1,0.5,0.9)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none", axis.title.y=element_text(margin=margin(0,10,0,0)))
rm(CatsMed)

SubCollocs$PrecipCat <- cut(SubCollocs$AeroWatercm, quantile(SubCollocs$AeroWatercm, seq(0,1,0.2), na.rm=T))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + PrecipCat, data=SubCollocs, quantile, na.rm=T)
CatsPrecipSub <- aggregate(AeroWatercm ~ CombResAlgQAC + East2 + PrecipCat, data=SubCollocs, median, na.rm=T)
CatsPrecipSub <- merge(CatsPrecipSub, CatsMed, by=c("CombResAlgQAC", "East2", "PrecipCat"))
CatsPrecipSub <- cbind.data.frame(CatsPrecipSub[,1:4], CatsPrecipSub$AODerr[,2:4])
colnames(CatsPrecipSub) <- c("CombResAlgQAC", "East2", "PrecipCat", "PrecipCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
PrecipBinsPltShort <- ggplot(CatsPrecipSub, aes(x=PrecipCatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=AeroWatercm, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=0.05), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("TCPW (cm)", limits=c(0,3), breaks=c(0,1,2)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none", axis.title.y=element_text(margin=margin(0,15,0,0)))
rm(CatsMed)

OtherPltShort <- plot_grid(NDVIBinsPltShort, PrecipBinsPltShort, labels=c('(a)', '(b)'), ncol=2, label_size=10)
OtherPltShort <- arrangeGrob(OtherPltShort, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/NDVIWetxAODerr.jpg", width = 5, height=4, units="in", res=300, pointsize=10)
grid.draw(OtherPltShort)
dev.off()

##----
## Land Use Type Box Plot - 1/4 page - removed from final version of paper
##----

#SubCollocs <- subset(AllCollocs, AllCollocs$QAClab == "QAC 3" | (AllCollocs$QAClab == "QAC 1" & AllCollocs$CombResAlg == "10 km DB"))
#SubCollocs$LandUse <- cut(SubCollocs$NLCDMode, breaks=c(18, 26,36,46,56,76,86,96), labels=c("Developed", "Barren", "Forest", "Shrub", "Grass", "Cultivated", "Wetland"))
#SubCollocs$CombResAlgQAC <- ifelse(SubCollocs$QAClab=="QAC 1", "10 km DB; QAC = 1", paste(SubCollocs$CombResAlg, "QAC = 3", sep="; "))

LandUsePlt <- ggplot(subset(SubCollocs, !is.na(SubCollocs$LandUse)), aes(y=AODerr, x=LandUse, fill=CombResAlg)) +
  geom_hline(aes(yintercept=0), linetype="dotted") +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual("", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99", "10 km DB"="#f7f7f7")) +
  scale_y_continuous(expression(paste("Retrieval error ( ", tau[M], "-", tau[A], ")")), limits=c(-0.25,0.5), oob=squish) +
  xlab("Land Use Type") +
  facet_grid(East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="bottom", axis.text.x = element_text(angle = 40, hjust = 1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/LandUseBox_QAC3.pdf", family="Times", width=3.74, height=4.53)
LandUsePlt# + guides(fill=guide_legend(nrow=2,byrow=TRUE))
dev.off()

# Need to summarize retrieval errors by land use type
aggregate(AODerr~LandUse + East2 + CombResAlg, subset(SubCollocs, !is.na(SubCollocs$LandUse)), summary)

## -------
# Trend testing as part of bias analysis
## -------
check <- function(data, form){
  Model <- summary(lm(form, data))
  Out <- cbind.data.frame(Model$coefficients[2,1], Model$coefficients[2,4])
  colnames(Out) <- c("EstCoef", "PVal")
  Out$Sig <- ifelse(Out$PVal < 0.05, T, F)
  return(Out)
}

# Looking at AOD error as a function of Sensor Zenith, Solar Zenith, and Scattering angles; NDVI; Total Column Precipitable Water; % MODIS pixels included in collocation
test <- rbind.data.frame(ddply(SubCollocs, .(East2, QAClab, CombResAlg), check, form=AODerr~SensorZenith),
ddply(SubCollocs, .(East2, QAClab, CombResAlg), check, form=AODerr~SolarZenith),
ddply(SubCollocs, .(East2, QAClab, CombResAlg), check, form=AODerr~Scatter),

ddply(SubCollocs, .(East2, QAClab, CombResAlg), check, form=AODerr~NDVI),
ddply(SubCollocs, .(East2, QAClab, CombResAlg), check, form=AODerr~AeroWatercm),
ddply(SubCollocs, .(East2, QAClab, CombResAlg), check, form=AODerr~PercAvail))
test$Var <- c(rep("SensorZenith", 10), rep("SolarZenith", 10), rep("Scatter", 10), rep("NDVI", 10), rep("ColPrecipWater", 10), rep("PercModAvail", 10))

toLatex.xtable(xtable(test))


# Also need to do t-tests within months and categories of land use
ttests <- function(data){
  ttestres <- try(t.test(data$AODerr), silent = T)
  pval <- ifelse(is.list(ttestres), ttestres$p.value, NA)
  return(pval)
}

landuse <- ddply(subset(SubCollocs, !is.na(SubCollocs$LandUse)), .(East2, QAClab, CombResAlg, LandUse), ttests)
landuse$sig <- ifelse(landuse$V1 < 0.05, T, F)
monthyear <- ddply(SubCollocs, .(QAClab, CombResAlg, Month), ttests)
monthyear$sig <- ifelse(monthyear$V1 < 0.05, T, F)

## -------
# Fifth figure - site-level correlations (prep dataset here, and map in Arc)
# Note - need to eliminate sites w/ less than 2-3 collocations - check too see if can find citation for this or better threshold - Dropping this for now
## -------

# AERONET site/representativeness analysis - need to calculate % observations from each station within categories of East, Algorithm, QACcode, Resolution
# Get number of observations for each category of east, algorithm, qac, and resolution
AeroInv <- as.data.frame(xtabs(~ East + Algorithm + QACcode + Resolution, AllCollocs))
StationNums <- as.data.frame(xtabs(~ East + Algorithm + QACcode + Resolution + AeroLoc, AllCollocs))
AeroInv <- merge(StationNums, AeroInv, by=c("East", "Algorithm", "QACcode", "Resolution"), suffixes = c(".Station", ".Category"))
AeroInv$PercStation <- AeroInv$Freq.Station/AeroInv$Freq.Category

ggplot(AeroInv, aes(PercStation)) + geom_histogram() + facet_grid(East + Algorithm ~ QACcode + Resolution)

# Also want to calculate pairwise distances between stations and plot the percentage from each as a function of closeness
Lat <- aggregate(AeroLat ~ AeroLoc, AllCollocs, mean)
Long <- aggregate(AeroLong ~ AeroLoc, AllCollocs, mean)
LatLong <- merge(Lat, Long, by="AeroLoc")
hist(dist(LatLong[,c(2,3)]))
AeroInv <- merge(AeroInv, LatLong, by="AeroLoc")

# Create dataset for collocations map
collocNums <- as.data.frame(xtabs( ~ CombResAlg + AeroLoc, SubCollocs))
collocNums <- reshape(collocNums, timevar="CombResAlg", idvar="AeroLoc", direction="wide")
colnames(collocNums) <- c("AeroLoc", "DT3km", "DT10km", "DBDT10km", "DB10km")
collocNums <- merge(collocNums, LatLong, by="AeroLoc")
collocNums$Dragon <- ifelse(substr(collocNums$AeroLoc, 1, 6)=="DRAGON", 1, 0)
# Cutting into categories here - ArcGIS is being irritating
#for (i in seq(2, 5)){
#  collocNums[,i] <- as.integer(cut(collocNums[,i], c(-1, 15, 100, 250, 500, 1000), labels = c(1,2,3,4,5)))
#}
write.csv(collocNums, "T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/collocNumsMap.csv", row.names = F)
