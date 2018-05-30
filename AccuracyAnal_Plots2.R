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
# First figure - panel of MODIS vs. AERONET AOD plots - by region
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

# Using categories defined by quantiles
AllCollocs$ScatterCat <- cut(AllCollocs$Scatter, quantile(AllCollocs$Scatter, seq(0,1,0.2)))
CatsMed <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + ScatterCat, data=AllCollocs, quantile)
CatsScat <- aggregate(Scatter ~ CombResAlg + East2 + QAClab + ScatterCat, data=AllCollocs, median)
CatsScat <- merge(CatsScat, CatsMed, by=c("CombResAlg", "East2", "QAClab", "ScatterCat"))
CatsScat <- cbind.data.frame(CatsScat[,1:5], CatsScat$AODerr[,2:4])
colnames(CatsScat) <- c("CombResAlg", "East2", "QAClab", "ScatterCat", "ScatterCatMed", "AODerr25p", "AODerr50p", "AODerr75p")

rm(CatsMed)

ggplot(SubCatsScat, aes(x=ScatterCatMed, y=AODerr50p)) +
  geom_density(data=subset(AllCollocs, AllCollocs$QAClab == "QAC 3" | (AllCollocs$QAClab == "QAC 1" & AllCollocs$CombResAlg == "10 km DB")), aes(x=Scatter, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg, linetype=QAClab), position=position_jitter(width=5), size=0.8, show.legend = F) +
  geom_line(aes(linetype=QAClab, color=CombResAlg), size=0.8) +
  #geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Scattering Angle", breaks=c(80,120,160)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  scale_linetype_manual("", values=c("QAC 3" = "solid", "QAC 1" = "dashed")) +
  facet_grid(East2~.) +
  theme(legend.position="bottom")
rm(SubCatsScat)

ScatterBinsPlt <- ggplot(CatsScat, aes(x=ScatterCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=Scatter, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg, linetype=QAClab), position=position_jitter(width=5), size=0.4, show.legend = F) +
  geom_line(aes(linetype=QAClab, color=CombResAlg), size=0.5) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Scattering Angle", breaks=c(80,120,160)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  scale_linetype_manual("", values=c("QAC 3" = "solid", "QAC 1" = "dashed", "QAC 2" = "dotted")) +
  facet_grid(East2~.) +
  theme_classic(base_size=8) + theme(legend.position="bottom")

ScatterBinsPltLong <- ggplot(CatsScat, aes(x=ScatterCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=Scatter, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=5), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Scattering Angle", breaks=c(80,120,160)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size=8) + theme(legend.position="none")

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
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Scattering Angle", breaks=c(80,120,160)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none")
rm(CatsMed)


pdf("H:/Rotation_Yang/Imagery/PaperImages/ScatterxAODerr_Bins.pdf", family="Times", width=3.74, height=4.53)
ScatterBinsPlt
dev.off()

##----
## Solar Zenith Angle
##----

# Using categories defined by quantiles
AllCollocs$SolZCat <- cut(AllCollocs$SolarZenith, quantile(AllCollocs$SolarZenith, seq(0,1,0.2)))
CatsMedSolZ <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + SolZCat, data=AllCollocs, quantile)
CatsSolZ <- aggregate(SolarZenith ~ CombResAlg + East2 + QAClab + SolZCat, data=AllCollocs, median)
CatsSolZ <- merge(CatsSolZ, CatsMedSolZ, by=c("CombResAlg", "East2", "QAClab", "SolZCat"))
CatsSolZ <- cbind.data.frame(CatsSolZ[,1:5], CatsSolZ$AODerr[,2:4])
colnames(CatsSolZ) <- c("CombResAlg", "East2", "QAClab", "SolZCat", "SolZCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedSolZ)

SolZBinsPlt <- ggplot(CatsSolZ, aes(x=SolZCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=SolarZenith, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg, linetype=QAClab), position=position_jitter(width=5), size=0.4, show.legend = F) +
  geom_line(aes(linetype=QAClab, color=CombResAlg), size=0.5) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Solar Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  scale_linetype_manual("", values=c("QAC 3" = "solid", "QAC 1" = "dashed", "QAC 2" = "dotted")) +
  facet_grid(East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="bottom")


SolZBinsPltLong <- ggplot(CatsSolZ, aes(x=SolZCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=SolarZenith, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=5), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Solar Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="none")

SolZBinsPltShort <- ggplot(subset(CatsSolZ, CatsSolZ$QAClab == "QAC 3"), aes(x=SolZCatMed, y=AODerr50p)) +
  geom_density(data=subset(AllCollocs, AllCollocs$QAClab=="QAC 3"), aes(x=SolarZenith, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=3), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Solar Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(East2~.) +
  theme_classic(base_size = 10) + theme(legend.position="none")

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
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Solar Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none")
rm(CatsMed)

pdf("H:/Rotation_Yang/Imagery/PaperImages/SolZxAODerr_Bins.pdf", family="Times", width=3.74, height=4.53)
SolZBinsPlt
dev.off()

##----
## Sensor Zenith Angle
##----

# Using categories defined by quantiles
AllCollocs$SenZCat <- cut(AllCollocs$SensorZenith, quantile(AllCollocs$SensorZenith, seq(0,1,0.2)))
CatsMedSenZ <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + SenZCat, data=AllCollocs, quantile)
CatsSenZ <- aggregate(SensorZenith ~ CombResAlg + East2 + QAClab + SenZCat, data=AllCollocs, median)
CatsSenZ <- merge(CatsSenZ, CatsMedSenZ, by=c("CombResAlg", "East2", "QAClab", "SenZCat"))
CatsSenZ <- cbind.data.frame(CatsSenZ[,1:5], CatsSenZ$AODerr[,2:4])
colnames(CatsSenZ) <- c("CombResAlg", "East2", "QAClab", "SenZCat", "SenZCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedSenZ)

SenZBinsPlt <- ggplot(CatsSenZ, aes(x=SenZCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=SensorZenith, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg, linetype=QAClab), position=position_jitter(width=5), size=0.4, show.legend = F) +
  geom_line(aes(linetype=QAClab, color=CombResAlg), size=0.5) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Sensor Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  scale_linetype_manual("", values=c("QAC 3" = "solid", "QAC 1" = "dashed", "QAC 2" = "dotted")) +
  facet_grid(East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="bottom")

SenZBinsPltLong <- ggplot(CatsSenZ, aes(x=SenZCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=SensorZenith, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=5), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Sensor Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="bottom", legend.text=element_text(size=8))
AODProdLegend <- g_legend(SenZBinsPltLong)
SenZBinsPltLong <- SenZBinsPltLong+theme(legend.position='none')


SenZBinsPltShort <- ggplot(subset(CatsSenZ, CatsSenZ$QAClab=="QAC 3"), aes(x=SenZCatMed, y=AODerr50p)) +
  geom_density(data=subset(AllCollocs, AllCollocs$QAClab=="QAC 3"), aes(x=SensorZenith, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=3), size=0.4, color="black", show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Sensor Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(East2~.) +
  theme_classic(base_size = 10) + theme(legend.position="none")

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
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Sensor Zenith Angle", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black"), labels=c("3 km DT; QAC = 3"="3 km DT", "10 km DT; QAC = 3"="10 km DT", "10 km DB-DT; QAC = 3"="10 km DB-DT", "10 km DB; QAC = 3"="10 km DB", "10 km DB; QAC = 1"="10 km DB 2")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7"), labels=c("3 km DT; QAC = 3"="3 km DT", "10 km DT; QAC = 3"="10 km DT", "10 km DB-DT; QAC = 3"="10 km DB-DT", "10 km DB; QAC = 3"="10 km DB", "10 km DB; QAC = 1"="10 km DB 2")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="bottom")
AODProdLegend <- g_legend(SenZBinsPltShort)
SenZBinsPltShort <- SenZBinsPltShort+theme(legend.position='none')
rm(CatsMed)


pdf("H:/Rotation_Yang/Imagery/PaperImages/SenZxAODerr_Bins.pdf", family="Times", width=3.74, height=4.53)
SenZBinsPlt
dev.off()

# Try pre-paneling with labels
AnglePltLong <- plot_grid(ScatterBinsPltLong, SolZBinsPltLong, SenZBinsPltLong, labels="AUTO", ncol=3, label_size=10)
AnglePltLong <- arrangeGrob(AnglePltLong, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/AngesxAODerr_Bins_Long.pdf", family="Times", width=7.48, height=9.06)
grid.draw(AnglePltLong)
dev.off()

AnglePltShort <- plot_grid(ScatterBinsPltShort, SolZBinsPltShort, SenZBinsPltShort, labels=c('(a)', '(b)', '(c)'), ncol=3, label_size=10)
AnglePltShort <- arrangeGrob(AnglePltShort, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/AngesxAODerr_Bins_Short_QAC3.pdf", family="Times", width=7.48, height=4.53)
grid.draw(AnglePltShort)
dev.off()

jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/AnglesxAODerr.jpg", width = 6, height=4.5, units="in", res=300, pointsize=10)
grid.draw(AnglePltShort)
dev.off()

##----
## Smoothed version
##----

#Also make version from continuous data with smoothed lines
SubCollocs <- subset(AllCollocs, AllCollocs$QAClab == "QAC 3" | (AllCollocs$QAClab == "QAC 1" & AllCollocs$CombResAlg == "10 km DB"))
Scatter <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "Scatter")]
colnames(Scatter) <- c("East2", "CombResAlg", "QAClab", "AODerr", "AngleVal")
Scatter$AngleType <- rep("Scattering Angle", nrow(Scatter))
SolarZenith <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "SolarZenith")]
colnames(SolarZenith) <- c("East2", "CombResAlg", "QAClab", "AODerr", "AngleVal")
SolarZenith$AngleType <- rep("Solar Zenith Angle", nrow(SolarZenith))
SensorZenith <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "SensorZenith")]
colnames(SensorZenith) <- c("East2", "CombResAlg", "QAClab", "AODerr", "AngleVal")
SensorZenith$AngleType <- rep("Sensor Zenith Angle", nrow(SolarZenith))
Angles <- rbind.data.frame(Scatter, SolarZenith, SensorZenith)
Angles$CombResAlgQAC <- ifelse(Angles$QAClab=="QAC 1", "10 km DB; QAC = 1", paste(Angles$CombResAlg, "QAC = 3", sep="; "))
rm(Scatter, SolarZenith, SensorZenith)


plt <- ggplot(Angles, aes(x=AngleVal, y=AODerr, linetype=CombResAlgQAC, color=CombResAlgQAC)) +
  geom_hline(yintercept=0, linetype=3) +
  geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F) +
  scale_linetype_manual("AOD product", values=c("3 km DT; QAC = 3"="dotted", "10 km DT; QAC = 3"="dashed", "10 km DB-DT; QAC = 3"="twodash", "10 km DB; QAC = 3"="solid", "10 km DB; QAC = 1" = "solid")) +
  scale_color_manual("AOD product", values=c("3 km DT; QAC = 3"="#998ec3", "10 km DT; QAC = 3"="#998ec3", "10 km DB-DT; QAC = 3"="#998ec3", "10 km DB; QAC = 3"="#998ec3", "10 km DB; QAC = 1" = "#f1a340")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")"))) +
  scale_x_continuous("Angle value (degrees)") +
  facet_grid(East2~AngleType, scales="free_x") +
  theme_classic(base_size=8)

pdf("H:/Rotation_Yang/Imagery/PaperImages/AnglesxAODerr.pdf", family="Times", width=7.48, height=4.53)
plt
dev.off()

pdf("H:/Rotation_Yang/Imagery/PaperImages/AnglesxAODerr_BW.pdf", family="Times", width=7.48, height=4.53)
ggplot(Angles, aes(x=AngleVal, y=AODerr, linetype=CombResAlgQAC)) +
  geom_hline(yintercept=0, linetype=3) +
  geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F, color="black") +
  scale_linetype_manual("AOD product", values=c("3 km DT; QAC = 3"="dotted", "10 km DT; QAC = 3"="dashed", "10 km DB-DT; QAC = 3"="twodash", "10 km DB; QAC = 3"="solid", "10 km DB; QAC = 1" = "longdash")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")"))) +
  scale_x_continuous("Angle value (degrees)") +
  facet_grid(East2~AngleType, scales="free_x") +
  theme_classic(base_size=8)
dev.off()


# For last bias analysis figure - including NDVI, %MODIS pixels with QAC value, column precipitable water, and maybe %impervious surface area

SubCollocs$PImpervCat <- cut(SubCollocs$PImpervMean, quantile(SubCollocs$PImpervMean, seq(0,1,0.2), na.rm=T))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + PImpervCat, data=SubCollocs, quantile, na.rm=T)
CatsPImpervSub <- aggregate(PImpervMean ~ CombResAlgQAC + East2 + PImpervCat, data=SubCollocs, median, na.rm=T)
CatsPImpervSub <- merge(CatsPImpervSub, CatsMed, by=c("CombResAlgQAC", "East2", "PImpervCat"))
CatsPImpervSub <- cbind.data.frame(CatsPImpervSub[,1:4], CatsPImpervSub$AODerr[,2:4])
colnames(CatsPImpervSub) <- c("CombResAlgQAC", "East2", "PImpervCat", "PImpervCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
PImpervBinsPltShort <- ggplot(CatsPImpervSub, aes(x=PImpervCatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=PImpervMean, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=3), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Impervious Surface Area (%)", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none")
rm(CatsMed)

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
  scale_y_continuous(expression(paste("Residual error ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("NDVI", limits=c(0.05,0.95), breaks=c(0.1,0.5,0.9)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none")
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
  scale_x_continuous("Total Column Precipitable Water (cm)", limits=c(0,3), breaks=c(0,1,2)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none")
rm(CatsMed)

SubCollocs$PModCat <- cut(SubCollocs$PercAvail, quantile(SubCollocs$PercAvail, seq(0,1,0.2), na.rm=T))
CatsMed <- aggregate(AODerr ~ CombResAlgQAC + East2 + PModCat, data=SubCollocs, quantile, na.rm=T)
CatsPModSub <- aggregate(PercAvail ~ CombResAlgQAC + East2 + PModCat, data=SubCollocs, median, na.rm=T)
CatsPModSub <- merge(CatsPModSub, CatsMed, by=c("CombResAlgQAC", "East2", "PModCat"))
CatsPModSub <- cbind.data.frame(CatsPModSub[,1:4], CatsPModSub$AODerr[,2:4])
colnames(CatsPModSub) <- c("CombResAlgQAC", "East2", "PModCat", "PModCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
PModBinsPltShort <- ggplot(CatsPModSub, aes(x=PModCatMed, y=AODerr50p, color=CombResAlgQAC)) +
  geom_density(data=SubCollocs, aes(x=PercAvail, y=(..scaled../4)), alpha=0.5, fill="grey80", color="white") +
  geom_line(size=0.5) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, fill=CombResAlgQAC), position=position_jitter(width=3), size=0.4, shape=23, show.legend = T) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("% MODIS pixels in collocation", breaks=c(20,50,100)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size=10) + theme(legend.position="none")
rm(CatsMed)

OtherPltShort <- plot_grid(NDVIBinsPltShort, PrecipBinsPltShort, labels=c('(a)', '(b)'), ncol=2, label_size=10)
OtherPltShort <- arrangeGrob(OtherPltShort, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/OtherxAODerr_Bins_Short_QAC3.jpg", family="Times", width=6.53, height=4.53, units="in", res=300)
grid.draw(OtherPltShort)
dev.off()

jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/NDVIWetxAODerr.jpg", width = 5, height=4, units="in", res=300, pointsize=10)
grid.draw(OtherPltShort)
dev.off()

## -------
# Third figure - AOD error (MODIS AOD - AERONET AOD) vs. predictor values - by region, algorithm, QACcode, and resolution for sources of bias associated with land use (static) - Elevation (all QAC - col 1); PImperv (); Red2; PWetNLCD; NLCDMode
## -------


##----
# Elevation
##----

# Using categories defined by quantiles
AllCollocs$ElevCat <- cut(AllCollocs$MedElev, quantile(AllCollocs$MedElev, seq(0,1,0.2), na.rm = T))
CatsMedElev <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + ElevCat, data=AllCollocs, quantile, na.rm=T)
CatsElev <- aggregate(MedElev ~ CombResAlg + East2 + QAClab + ElevCat, data=AllCollocs, median, na.rm=T)
CatsElev <- merge(CatsElev, CatsMedElev, by=c("CombResAlg", "East2", "QAClab", "ElevCat"))
CatsElev <- cbind.data.frame(CatsElev[,1:5], CatsElev$AODerr[,2:4])
colnames(CatsElev) <- c("CombResAlg", "East2", "QAClab", "ElevCat", "ElevCatMed", "AODerr25p", "AODerr50p", "AODerr75p")

ElevBinsPltLong <- ggplot(CatsElev, aes(x=ElevCatMed/1000, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=MedElev/1000, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=0.1), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Elevation (km)", breaks=c(0,1,2), limits=c(0,2)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size=8) + theme(legend.position="none")

ElevBinsPltLong
##----
## Percent Impervious land type
##----

# Using categories defined by quantiles
AllCollocs$PImpervCat <- cut(AllCollocs$PImpervMean, quantile(AllCollocs$PImpervMean, seq(0,1,0.2), na.rm=T))
CatsMedPI <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + PImpervCat, data=AllCollocs, quantile)
CatsPI <- aggregate(PImpervMean ~ CombResAlg + East2 + QAClab + PImpervCat, data=AllCollocs, median)
CatsPI <- merge(CatsPI, CatsMedPI, by=c("CombResAlg", "East2", "QAClab", "PImpervCat"))
CatsPI <- cbind.data.frame(CatsPI[,1:5], CatsPI$AODerr[,2:4])
colnames(CatsPI) <- c("CombResAlg", "East2", "QAClab", "PICat", "PICatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedPI)

PIBinsPltLong <- ggplot(CatsPI, aes(x=PICatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=PImpervMean, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=2), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Impervious surface area (%)", breaks=c(0,30,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="none")

PIBinsPltLong

##----
## Percent Water/Wetland/Ice
##----

# Using categories defined by quantiles
AllCollocs$PWetCat <- cut(AllCollocs$PWetNLCD, quantile(AllCollocs$PWetNLCD, seq(0,1,0.2), na.rm = T))
CatsMedPWet <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + PWetCat, data=AllCollocs, quantile)
CatsPWet <- aggregate(PWetNLCD ~ CombResAlg + East2 + QAClab + PWetCat, data=AllCollocs, median)
CatsPWet <- merge(CatsPWet, CatsMedPWet, by=c("CombResAlg", "East2", "QAClab", "PWetCat"))
CatsPWet <- cbind.data.frame(CatsPWet[,1:5], CatsPWet$AODerr[,2:4])
colnames(CatsPWet) <- c("CombResAlg", "East2", "QAClab", "PWetCat", "PWetCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedPWet)

PWetBinsPltLong <- ggplot(CatsPWet, aes(x=PWetCatMed*100, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=PWetNLCD*100, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=1), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Percent Water/Swamp/", breaks=c(20,40,60)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="none")

PWetBinsPltLong

# Try pre-paneling with labels
LandPltLong <- plot_grid(ElevBinsPltLong, PIBinsPltLong, PWetBinsPltLong, labels="AUTO", ncol=3, label_size=10)
LandPltLong <- arrangeGrob(LandPltLong, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/LandxAODerr_Bins_Long.pdf", family="Times", width=7.48, height=9.06)
grid.draw(LandPltLong)
dev.off()

##----
## Land Use Type Box Plot - 1/4 page
##----

#SubCollocs <- subset(AllCollocs, AllCollocs$QAClab == "QAC 3" | (AllCollocs$QAClab == "QAC 1" & AllCollocs$CombResAlg == "10 km DB"))
#SubCollocs$LandUse <- cut(SubCollocs$NLCDMode, breaks=c(18, 26,36,46,56,76,86,96), labels=c("Developed", "Barren", "Forest", "Shrub", "Grass", "Cultivated", "Wetland"))
#SubCollocs$CombResAlgQAC <- ifelse(SubCollocs$QAClab=="QAC 1", "10 km DB; QAC = 1", paste(SubCollocs$CombResAlg, "QAC = 3", sep="; "))

LandUsePlt <- ggplot(subset(SubCollocs, !is.na(SubCollocs$LandUse)), aes(y=AODerr, x=LandUse, fill=CombResAlg)) +
  geom_hline(aes(yintercept=0), linetype="dotted") +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual("", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99", "10 km DB"="#f7f7f7")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.25,0.5), oob=squish) +
  xlab("Land Use Type") +
  facet_grid(East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="bottom", axis.text.x = element_text(angle = 40, hjust = 1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/LandUseBox_QAC3.pdf", family="Times", width=3.74, height=4.53)
LandUsePlt# + guides(fill=guide_legend(nrow=2,byrow=TRUE))
dev.off()

# Need to summarize retrieval errors by land use type
aggregate(AODerr~LandUse + East2 + CombResAlg, subset(SubCollocs, !is.na(SubCollocs$LandUse)), summary)

LandUsePltBW <- ggplot(subset(SubCollocs, !is.na(SubCollocs$LandUse)), aes(y=AODerr, x=LandUse, fill=CombResAlgQAC)) +
  geom_hline(aes(yintercept=0), linetype="dotted") +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual("", values=c("3 km DT; QAC = 3"="#252525", "10 km DT; QAC = 3"="#636363", "10 km DB-DT; QAC = 3"="#969696", "10 km DB; QAC = 3"="#636363", "10 km DB; QAC = 1"="#f7f7f7")) +
  scale_y_continuous(expression(paste("( ", tau[M], "-", tau[A], ")")), limits=c(-0.25,0.5), oob=squish) +
  xlab("Land Use Type") +
  facet_grid(East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="bottom", axis.text.x = element_text(angle = 40, hjust = 1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/LandUseBox_BW.pdf", family="Times", width=3.74, height=4.53)
LandUsePltBW + guides(fill=guide_legend(nrow=2,byrow=TRUE))
dev.off()

##----
## Smoothed version
##----

#Also make version from continuous data with smoothed lines
Elev <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "MedElev")]
colnames(Elev) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
Elev$Val <- Elev$Val/1000
Elev$VarType <- rep("Elevation (km)", nrow(Elev))
PImperv <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "PImpervMean")]
colnames(PImperv) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
PImperv$Val <- PImperv$Val*100
PImperv$VarType <- rep("Impervious surface area (%)", nrow(PImperv))
PWet <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "PWetNLCD")]
colnames(PWet) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
PWet$VarType <- rep("Water/Ice/Wetland (%)", nrow(PWet))
Land <- rbind.data.frame(Elev, PImperv, PWet)
Land$CombResAlgQAC <- ifelse(Land$QAClab=="QAC 1", "10 km DB; QAC = 1", paste(Land$CombResAlg, "QAC = 3", sep="; "))
rm(Elev, PImperv, PWet)


plt <- ggplot(Land, aes(x=Val, y=AODerr, linetype=CombResAlgQAC, color=CombResAlgQAC)) +
  geom_hline(yintercept=0, linetype=3) +
  geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F) +
  scale_linetype_manual("AOD product", values=c("3 km DT; QAC = 3"="dotted", "10 km DT; QAC = 3"="dashed", "10 km DB-DT; QAC = 3"="twodash", "10 km DB; QAC = 3"="solid", "10 km DB; QAC = 1" = "solid")) +
  scale_color_manual("AOD product", values=c("3 km DT; QAC = 3"="#998ec3", "10 km DT; QAC = 3"="#998ec3", "10 km DB-DT; QAC = 3"="#998ec3", "10 km DB; QAC = 3"="#998ec3", "10 km DB; QAC = 1" = "#f1a340")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")"))) +
  scale_x_continuous("") +
  facet_grid(East2~VarType, scales="free_x") +
  theme_classic(base_size=8)

pdf("H:/Rotation_Yang/Imagery/PaperImages/LandxAODerr_Smooth.pdf", family="Times", width=7.48, height=4.53)
plt
dev.off()

pdf("H:/Rotation_Yang/Imagery/PaperImages/LandxAODerr_Smooth_BW.pdf", family="Times", width=7.48, height=4.53)
ggplot(Land, aes(x=Val, y=AODerr, linetype=CombResAlgQAC)) +
  geom_hline(yintercept=0, linetype=3) +
  geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F, color="black") +
  scale_linetype_manual("AOD product", values=c("3 km DT; QAC = 3"="dotted", "10 km DT; QAC = 3"="dashed", "10 km DB-DT; QAC = 3"="twodash", "10 km DB; QAC = 3"="solid", "10 km DB; QAC = 1" = "longdash")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")"))) +
  scale_x_continuous("") +
  facet_grid(East2~VarType, scales="free_x") +
  theme_classic(base_size=8)
dev.off()

## -------
# Fourth figure - AOD error (MODIS AOD - AERONET AOD) vs. predictor values - by region, algorithm, QACcode, and resolution for sources of bias associated with atmospheric conditions and season: Month of year; NDVI; Watercm. At bottom, show Watercm by SO4 and Na content (boxplot, high high high vs low low low)
## -------

##----
# NDVI
##----

# Using categories defined by quantiles
AllCollocs$NDVICat <- cut(AllCollocs$NDVI, quantile(AllCollocs$NDVI, seq(0,1,0.2), na.rm = T))
CatsMedNDVI <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + NDVICat, data=AllCollocs, quantile, na.rm=T)
CatsNDVI <- aggregate(NDVI ~ CombResAlg + East2 + QAClab + NDVICat, data=AllCollocs, median, na.rm=T)
CatsNDVI <- merge(CatsNDVI, CatsMedNDVI, by=c("CombResAlg", "East2", "QAClab", "NDVICat"))
CatsNDVI <- cbind.data.frame(CatsNDVI[,1:5], CatsNDVI$AODerr[,2:4])
colnames(CatsNDVI) <- c("CombResAlg", "East2", "QAClab", "NDVICat", "NDVICatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedNDVI)

NDVIBinsPltLong <- ggplot(CatsNDVI, aes(x=NDVICatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=NDVI, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=0.02), size=0.1, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("NDVI", breaks=c(0,0.5,1)) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size=8) + theme(legend.position="none")

NDVIBinsPltLong

##----
## Total column precipitable water (cm)
##----

# Using categories defined by quantiles
AllCollocs$PrecipWaterCat <- cut(AllCollocs$AeroWatercm, quantile(AllCollocs$AeroWatercm, seq(0,1,0.2), na.rm=T))
CatsMedPrecipWater <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + PrecipWaterCat, data=AllCollocs, quantile)
CatsPrecipWater <- aggregate(AeroWatercm ~ CombResAlg + East2 + QAClab + PrecipWaterCat, data=AllCollocs, median)
CatsPrecipWater <- merge(CatsPrecipWater, CatsMedPrecipWater, by=c("CombResAlg", "East2", "QAClab", "PrecipWaterCat"))
CatsPrecipWater <- cbind.data.frame(CatsPrecipWater[,1:5], CatsPrecipWater$AODerr[,2:4])
colnames(CatsPrecipWater) <- c("CombResAlg", "East2", "QAClab", "PrecipWaterCat", "PrecipWaterCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedPrecipWater)

PrecipWaterBinsPltLong <- ggplot(CatsPrecipWater, aes(x=PrecipWaterCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=AeroWatercm, y=(..scaled../4)), alpha=0.5, fill="white", color="black") +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=0.2), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Total column precipitable water (cm)") +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="none")

PrecipWaterBinsPltLong

##----
## MOY
##----

# Using categories defined by quantiles
AllCollocs$MOYCat <- cut(AllCollocs$MonthNum, quantile(AllCollocs$MonthNum, seq(0,1,0.2), na.rm = T))
CatsMedMOY <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + MOYCat, data=AllCollocs, quantile)
CatsMOY <- aggregate(MonthNum ~ CombResAlg + East2 + QAClab + MOYCat, data=AllCollocs, median)
CatsMOY <- merge(CatsMOY, CatsMedMOY, by=c("CombResAlg", "East2", "QAClab", "MOYCat"))
CatsMOY <- cbind.data.frame(CatsMOY[,1:5], CatsMOY$AODerr[,2:4])
colnames(CatsMOY) <- c("CombResAlg", "East2", "QAClab", "MOYCat", "MOYCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedMOY)

MOYBinsPltLong <- ggplot(CatsMOY, aes(x=MOYCatMed, y=AODerr50p)) +
  geom_density(data=AllCollocs, aes(x=MonthNum, y=(..scaled../4)), alpha=0.5, fill="white", color="black", adjust=2) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlg), position=position_jitter(width=0.2), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlg), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Month of Year", breaks=c(3,6,9), labels=c("Mar", "Jun", "Sep")) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT"="#e66101", "10 km DT"="#fdb863", "10 km DB-DT"="#b2abd2", "10 km DB"="#5e3c99")) +
  facet_grid(QAClab + East2~.) +
  theme_classic(base_size = 8) + theme(legend.position="none")

MOYBinsPltLong

#MOY plot
CatsMedMOY <- aggregate(AODerr ~ CombResAlgQAC + East2 + MonthNum, data=SubCollocs, quantile)
#CatsMOY <- aggregate(MonthNum ~ CombResAlg + East2 + QAClab + MonthNum, data=AllCollocs, median)
#CatsMOY <- merge(CatsMOY, CatsMedMOY, by=c("CombResAlg", "East2", "QAClab", "MonthNum"))
CatsMOY <- cbind.data.frame(CatsMedMOY[,1:3], CatsMedMOY$AODerr[,2:4])
colnames(CatsMOY) <- c("CombResAlgQAC", "East2", "MonthNum", "AODerr25p", "AODerr50p", "AODerr75p")
rm(CatsMedMOY)

MOYBinsPltShort <- ggplot(CatsMOY, aes(x=MonthNum, y=AODerr50p)) +
  geom_density(data=SubCollocs, aes(x=MonthNum, y=(..scaled../4)), alpha=0.5, fill="white", color="black", adjust=2) +
  geom_pointrange(aes(ymin=AODerr25p, ymax=AODerr75p, color=CombResAlgQAC), position=position_jitter(width=0.2), size=0.4, show.legend = F) +
  geom_line(aes(color=CombResAlgQAC), size=0.5) +
  scale_y_continuous("", limits=c(-0.05, 0.3), oob=squish) +
  scale_x_continuous("Month of Year", breaks=c(2,5,8,11), labels=c("Feb", "May", "Aug", "Nov")) +
  scale_color_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="black")) +
  scale_fill_manual("AOD \nProduct", values=c("3 km DT; QAC = 3"="#e66101", "10 km DT; QAC = 3"="#fdb863", "10 km DB-DT; QAC = 3"="#b2abd2", "10 km DB; QAC = 3"="#5e3c99", "10 km DB; QAC = 1"="#f7f7f7")) +
  facet_grid(East2~.) +
  theme_classic(base_size = 10) + theme(legend.position="none")

SubCollocs$CombResAlg2 <- ifelse(SubCollocs$CombResAlg == "3 km DT", "(a) 3 km DT", ifelse(SubCollocs$CombResAlg == "10 km DT", "(b) 10 km DT", ifelse(SubCollocs$CombResAlg == "10 km DB-DT", "(c) 10 km DB-DT", "(d) 10 km DB")))
SeasonBox <- ggplot(subset(SubCollocs, SubCollocs$QAClab=="QAC 3"), aes(x=as.factor(MonthNum), y=AODerr, ymin=quantile(AODerr, probs=0.05), ymax=quantile(AODerr, probs=0.95))) + geom_boxplot(outlier.colour = NA) + geom_hline(aes(yintercept=0), linetype="dotted") + scale_y_continuous(expression(paste("Residual error in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.2,0.2)) + scale_x_discrete("Month of Year", breaks=c("3", "6", "9"), labels=c("Mar", "Jun", "Sep")) + facet_wrap(c("CombResAlg2"), nrow=2, ncol=2, dir="v") + theme_minimal(base_size = 8)

jpeg("T:/eohprojs/CDC_climatechange/Jess/ValidationPaperRewrite/MOYxAODerr_Boxplot2.jpg", width=3.74, height=4.53, units="in", res=300)
SeasonBox
dev.off()

# Try pre-paneling with labels
SeasonPltLong <- plot_grid(NDVIBinsPltLong, PrecipWaterBinsPltLong, MOYBinsPltLong, labels="AUTO", ncol=3, label_size=10)
SeasonPltLong <- arrangeGrob(SeasonPltLong, AODProdLegend, nrow=2, ncol=1, heights=c(12,1))

pdf("H:/Rotation_Yang/Imagery/PaperImages/SeasonxAODerr_Bins_Long.pdf", family="Times", width=7.48, height=9.06)
grid.draw(SeasonPltLong)
dev.off()



##----
## Smoothed version - 4-panel includes: % Impervious surface area, NDVI, Column precipitable water, % MODIS QAC=3,
##----

# Should include below in paper - evidence of cloud contamination
ggplot(subset(SubCollocs, SubCollocs$QAClab=="QAC 3"), aes(x=PercAvail, y=AODerr, linetype=CombResAlg)) + geom_hline(yintercept=0, linetype=3) + geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F, color="black") + facet_grid(East2~.)

# Check month number
ggplot(SubCollocs, aes(x=MonthNum, y=AODerr, linetype=CombResAlg, color=QAClab)) + geom_hline(yintercept=0, linetype=3) + geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F) + facet_grid(East2~.)


#Also make version from continuous data with smoothed lines
PImperv <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "PImpervMean")]
colnames(PImperv) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
PImperv$Val <- PImperv$Val*100
PImperv$VarType <- rep("Impervious surface area (%)", nrow(PImperv))
NDVI <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "NDVI")]
colnames(NDVI) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
NDVI$VarType <- rep("NDVI", nrow(NDVI))
PrecipWater <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "AeroWatercm")]
colnames(PrecipWater) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
PrecipWater$VarType <- rep("Total column precipitable water (cm)", nrow(PrecipWater))
PMODIS <- SubCollocs[,c("East2", "CombResAlg", "QAClab", "AODerr", "PercAvail")]
colnames(PMODIS) <- c("East2", "CombResAlg", "QAClab", "AODerr", "Val")
PMODIS$VarType <- rep("MODIS pixels with QAC = 3 (%)", nrow(PMODIS))
PMODIS <- subset(PMODIS, PMODIS$QAClab == "QAC 3")

Season <- rbind.data.frame(PImperv, NDVI, PrecipWater, PMODIS)
Season$CombResAlgQAC <- ifelse(Season$QAClab=="QAC 1", "10 km DB; QAC = 1", paste(Season$CombResAlg, "QAC = 3", sep="; "))
rm(NDVI, PImperv, PrecipWater, PMODIS)
Season$VarTypeOrd <- ordered(Season$VarType, levels=c("Impervious surface area (%)", "NDVI", "Total column precipitable water (cm)", "MODIS pixels with QAC = 3 (%)"))


SeasonPlt <- ggplot(Season, aes(x=Val, y=AODerr, linetype=CombResAlgQAC, color=CombResAlgQAC)) +
  geom_hline(yintercept=0, linetype=3) +
  geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F) +
  scale_linetype_manual("AOD product", values=c("3 km DT; QAC = 3"="dotted", "10 km DT; QAC = 3"="dashed", "10 km DB-DT; QAC = 3"="twodash", "10 km DB; QAC = 3"="solid", "10 km DB; QAC = 1" = "solid")) +
  scale_color_manual("AOD product", values=c("3 km DT; QAC = 3"="#998ec3", "10 km DT; QAC = 3"="#998ec3", "10 km DB-DT; QAC = 3"="#998ec3", "10 km DB; QAC = 3"="#998ec3", "10 km DB; QAC = 1" = "#f1a340")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")"))) +
  scale_x_continuous("") +
  facet_grid(East2~VarTypeOrd, scales="free_x") +
  theme_classic(base_size=8) + theme(legend.position="bottom")

pdf("H:/Rotation_Yang/Imagery/PaperImages/SeasonxAODerr_Smooth.pdf", family="Times", width=7.48, height=4.53)
SeasonPlt
dev.off()

pdf("H:/Rotation_Yang/Imagery/PaperImages/SeasonxAODerr_Smooth_BW.pdf", family="Times", width=7.48, height=4.53)
ggplot(Season, aes(x=Val, y=AODerr, linetype=CombResAlgQAC)) +
  geom_hline(yintercept=0, linetype=3) +
  geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F, color="black") +
  scale_linetype_manual("AOD product", values=c("3 km DT; QAC = 3"="dotted", "10 km DT; QAC = 3"="dashed", "10 km DB-DT; QAC = 3"="twodash", "10 km DB; QAC = 3"="solid", "10 km DB; QAC = 1" = "longdash")) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")"))) +
  scale_x_continuous("") +
  facet_grid(East2~VarTypeOrd, scales="free_x") +
  theme_classic(base_size=8) + theme(legend.position="bottom")
dev.off()


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
