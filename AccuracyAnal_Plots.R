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

toLatex.xtable(xtable(TableRegs[,1:12]), include.rownames=F)


rm(AODerr, PropEE, LinModCoefs, Length)

## -------
# First figure - panel of MODIS vs. AERONET AOD plots - by region
## -------

# Add n into plot - no; shifting some of this information to table - remove R^2 text and best fit lines
# png("H:/Rotation_Yang/Imagery/PaperImages/PaneledModxAero.png", width=7.5, height=6.5, units="in", res=300)
# ggplot(AllCollocs, aes(x=AeroAOD, y=ModAOD)) +
#     geom_bin2d(bins=40) +
#     geom_abline(data=ABLines, aes(intercept=a, slope=b, linetype=as.factor(lntyp)), show_guide=T) +
#     geom_text(data=TableRegs, aes(y=0.05,x=0.4, label=paste("R^2==", round(adjR2, digits=2))), size=3, parse=T) +
#     geom_point(data=TableRegs, aes(x=0.48, y=0.1, shape=MeetsPreLaunch)) +
#     scale_shape_discrete(">68% Obs. \nin EE?")  +
#     scale_y_continuous("MODIS AOD", limits=c(0,0.5), breaks=c(0,0.2,0.4)) +
#     scale_x_continuous("AERONET AOD", limits=c(0,0.5), breaks=c(0,0.2,0.4)) +
#     scale_fill_gradient2("No. \nObs.", trans="log", low="dodgerblue", mid="gold", high="darkseagreen", midpoint=2.7, space="Lab", breaks=c(1,10,40,100,400)) +
#     scale_linetype_identity("", guide="legend", labels=c("solid"="1-1 line", "dotted"="Best fit", "dotdash"="EE")) +
#     guides(shape = guide_legend(override.aes = list(linetype = 0))) +
#     facet_grid(East2 + QAClab ~ Algorithm + ResLab) +
#     theme_classic()
# dev.off()

# png("H:/Rotation_Yang/Imagery/PaperImages/PaneledModxAero2.png", width=7.5, height=6.5, units="in", res=300)
# ggplot(AllCollocs, aes(x=AeroAOD, y=ModAOD)) +
#   geom_bin2d(bins=40) +
#   geom_abline(data=subset(ABLines, lntyp!="dotted"), aes(intercept=a, slope=b, linetype=as.factor(lntyp)), show_guide=T) +
#   #geom_text(data=TableRegs, aes(y=0.05,x=0.4, label=paste("R^2==", round(adjR2, digits=2))), size=3, parse=T) +
#   geom_point(data=TableRegs, aes(x=0.48, y=0.05, shape=MeetsPreLaunch)) +
#   scale_shape_discrete(">68% Obs. \nin EE?")  +
#   scale_y_continuous("MODIS AOD", limits=c(0,0.5), breaks=c(0,0.2,0.4)) +
#   scale_x_continuous("AERONET AOD", limits=c(0,0.5), breaks=c(0,0.2,0.4)) +
#   scale_fill_gradient2("No. \nObs.", trans="log", low="dodgerblue", mid="gold", high="darkseagreen", midpoint=2.7, space="Lab", breaks=c(1,10,40,100,400)) +
#   scale_linetype_identity("", guide="legend", labels=c("solid"="1-1 line", "dotted"="Best fit", "dotdash"="EE")) +
#   guides(shape = guide_legend(override.aes = list(linetype = 0))) +
#   facet_grid(East2 + QAClab ~ Algorithm + ResLab) +
#   theme_classic()
# dev.off()


#relevel(AllCollocs$CombResAlg, "3 km DT")
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

#ggplot(AllCollocs, aes(x=Scatter)) + geom_density() + facet_grid(East2 ~ QAClab) + theme_classic()
#ggplot(AllCollocs, aes(x=SolarZenith)) + geom_density() + facet_grid(East2 ~ QAClab) + theme_classic()
#ggplot(AllCollocs, aes(x=SensorZenith)) + geom_density() + facet_grid(East2 ~ QAClab) + theme_classic()
#ggplot(subset(AllCollocs, AllCollocs$SolarAzimuth < -50), aes(x=SolarAzimuth)) + geom_density() + facet_grid(East2 ~ QAClab) + theme_classic()
#ggplot(AllCollocs, aes(x=SensorAzimuth)) + geom_density() + facet_grid(East2 ~ QAClab) + theme_classic()

#AllCollocs$SensorAzimuthCat <- cut(AllCollocs$SensorAzimuth, c(-200,0,200), c("Negative Angle", "Positive Angle"))
#ggplot(AllCollocs, aes(x=SensorAzimuthCat, y=AODerr, linetype=ResLab, fill=Algorithm)) + geom_boxplot() + facet_grid(East2~QAClab)
# Ignoring Azimuth Angles - no clear differences between products
#AllCollocs$ScatterCat <- cut(AllCollocs$Scatter, c(60,100,120,140,160,180),c(90,110,130,150,170))
# Using categories defined by quantiles
AllCollocs$ScatterCat <- cut(AllCollocs$Scatter, quantile(AllCollocs$Scatter, seq(0,1,0.2)))

#CatsMean <- aggregate(AODerr ~ Algorithm + East2 + QAClab + ScatterCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ CombResAlg + East2 + QAClab + ScatterCat, data=AllCollocs, quantile)
Cats <- aggregate(Scatter ~ CombResAlg + East2 + QAClab + ScatterCat, data=AllCollocs, median)
Cats <- merge(Cats, CatsMed, by=c("CombResAlg", "East2", "QAClab", "ScatterCat"))
Cats <- cbind.data.frame(Cats[,1:5], Cats$AODerr[,2:4])
colnames(Cats) <- c("CombResAlg", "East2", "QAClab", "ScatterCat", "ScatterCatMed", "AODerr25p", "AODerr50p", "AODerr75p")
#Catsp <- aggregate(AODerr ~ Algorithm + East2 + QAClab + ScatterCat, data=AllCollocs, quantile)
#Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "ScatterCat"))
#Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
#colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "ScatterCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
#Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))
SubCats <- subset(Cats, Cats$QAClab == "QAC 3" | (Cats$QAClab == "QAC 1" & Cats$CombResAlg == "10 km DB"))


# Fix labeling, add density box to legend, do paneling with Zenith angles, change scales and colorschemes
ScatteringAnglePlt <- ggplot(Cats, aes(x=ScatterCatMed , y=AODerr50p)) +
    geom_density(data=AllCollocs, aes(x=Scatter, y=(..scaled../3)), alpha=0.5, fill="grey70", color=NA) +
    geom_pointrange(aes(color=CombResAlg, ymax=AODerr75p, ymin=AODerr25p), alpha=0.8) +
    geom_line(aes(color=CombResAlg), alpha=0.8) +
    geom_hline(yintercept=0, linetype=3) +
    scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.3), oob=squish) +
    scale_x_continuous("Scattering Angle", breaks=c(80,120,160)) +
    #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
    scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
    #scale_fill_manual("", values = c("Distribution \n of Scattering \n Angle Values"="grey70")) +
    facet_grid(East2 + QAClab~.) +
    theme(legend.position="bottom", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"), text=element_text(size=8))
AODProdLegend <- g_legend(ScatteringAnglePlt)
ScatteringAnglePlt <- ScatteringAnglePlt+theme(legend.position='none')

ggplot(SubCats, aes(x=ScatterCatMed, y=AODerr50p)) +
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




AllCollocs$SolarZCat <- cut(AllCollocs$SolarZenith, c(0,20,30,40,50,60,100),c(15,25,35,45,55,65))

CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SolarZCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SolarZCat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SolarZCat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "SolarZCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "SolarZCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

SolarZAnglePlt <- ggplot(Cats, aes(x=as.numeric(as.character(SolarZCat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=SolarZenith, y=(..scaled../4), fill="Distribution \n of Angle\n Values"), alpha=0.5, color=NA) +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=3), size=1) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Solar Zenith Angle") +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c"), guide=F) +
  scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="bottom", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"), text=element_text(size=8))
DensValsLegend <- g_legend(SolarZAnglePlt)
SolarZAnglePlt <- SolarZAnglePlt+theme(legend.position='none')


AllCollocs$SensorZCat <- cut(AllCollocs$SensorZenith, c(0,10,20,30,40,50,60,100),c(5,15,25,35,45,55,65))

CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SensorZCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SensorZCat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SensorZCat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "SensorZCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "SensorZCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

SensorZAnglePlt <- ggplot(Cats, aes(x=as.numeric(as.character(SensorZCat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=SensorZenith, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=3), size=1) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Sensor Zenith Angle") +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"), text=element_text(size=8))

png("H:/Rotation_Yang/Imagery/PaperImages/Angles_AODBias.png", width=7.5, height=10, units="in", res=300)
grid.arrange(grid.arrange(ScatteringAnglePlt, SolarZAnglePlt, SensorZAnglePlt, nrow=1, ncol=3, widths=c(3,3,3)), grid.arrange(grid.rect(gp=gpar(col="white")), DensValsLegend, AODProdLegend, nrow=1, ncol=3, widths=c(1,1,5)), nrow=2, ncol=1, heights=c(12,1))
dev.off()

pdf("H:/Rotation_Yang/Imagery/PaperImages/Angles_AODBias.pdf", family="Times", width=3.74, height=4.53)
grid.arrange(grid.arrange(ScatteringAnglePlt, SolarZAnglePlt, SensorZAnglePlt, nrow=1, ncol=3, widths=c(3,3,3)), grid.arrange(grid.rect(gp=gpar(col="white")), DensValsLegend, AODProdLegend, nrow=1, ncol=3, widths=c(1,1,5)), nrow=2, ncol=1, heights=c(12,1))
dev.off()

#Also make bw version from continuous data with smoothed lines
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

# Should include below in paper - evidence of cloud contamination
ggplot(subset(SubCollocs, SubCollocs$QAClab=="QAC 3"), aes(x=PercAvail, y=AODerr, linetype=CombResAlg)) + geom_hline(yintercept=0, linetype=3) + geom_smooth(method="gam", formula=y~s(x), alpha=0.5, se=F, color="black") + facet_grid(East2~.)

#ggplot(AllCollocs, aes(x=SolarZenith, y=SensorZenith)) + geom_point() + facet_grid(East2 ~ QAClab)
#cor(AllCollocs$Scatter, AllCollocs$SolarZenith)
#cor(AllCollocs$Scatter, AllCollocs$SensorZenith)
#cor(AllCollocs$SensorZenith, AllCollocs$SolarZenith)
## -------
# Third figure - AOD error (MODIS AOD - AERONET AOD) vs. predictor values - by region, algorithm, QACcode, and resolution for sources of bias associated with land use (static) - Elevation (all QAC - col 1); PImperv (); Red2; PWetNLCD; NLCDMode
## -------

# Removing all QAC=2 from plotting dataset
AllCollocsInt <- subset(AllCollocs, AllCollocs$QAClab != "QAC 2")

ggplot(AllCollocsInt, aes(y=AODerr, x=Season, color=Algorithm, linetype=as.factor(ResLab))) + geom_boxplot() + facet_wrap(~ QAClab + East2, nrow=1)

AllCollocsInt$LandUse <- cut(AllCollocsInt$NLCDMode, breaks=c(18, 26,36,46,56,76,86,96), labels=c("Developed", "Barren", "Forest", "Shrub", "Grass", "Cultivated", "Wetland"))

#png("H://Rotation_Yang/Imagery/PaperImages/LandUseAODErr.png", width=7.5, height=2, units="in", res=300)
ggplot(subset(AllCollocsInt, !is.na(AllCollocsInt$LandUse)), aes(y=AODerr, x=LandUse, linetype=ResLab, fill=Algorithm)) + geom_hline(aes(yintercept=0), linetype="dotted") + geom_boxplot() + facet_grid(East2 ~ QAClab)
#dev.off()


AllCollocs$MedElevCat <- cut(AllCollocs$MedElev, c(0,25,75,200,450,1100,1600,5000),c(13,48,113,309,779,1385,1865))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + MedElevCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + MedElevCat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + MedElevCat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "MedElevCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "MedElevCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

MedElevPlt <- ggplot(Cats, aes(x=as.numeric(as.character(MedElevCat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=MedElev, y=(..scaled../4), fill="Distribution \n of Values"), alpha=0.5, color=NA) +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=10)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Elevation", limits=c(0,2000)) +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  scale_fill_manual("", values = c("Distribution \n of Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="bottom", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))


AllCollocsInt$PImpervCat <- cut(AllCollocsInt$PImpervMean, c(-0.1,0.05,1,10,25,50,100),c(0,0.5,3,15,35,70))

CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + PImpervCat, data=AllCollocsInt, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + PImpervCat, data=AllCollocsInt, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + PImpervCat, data=AllCollocsInt, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "PImpervCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "PImpervCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

PImpervPlt <- ggplot(Cats, aes(x=as.numeric(as.character(PImpervCat)),y=p50AODerr)) +
  geom_density(data=AllCollocsInt, aes(x=PImpervMean, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=3)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("% Developed Land", limits=c(0,75)) +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))

AllCollocsInt$PWetCat <- cut(AllCollocsInt$PWetNLCD*100, c(-0.1,0.05,1,10,25,50,100),c(0,0.5,3,15,35,70))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + PWetCat, data=AllCollocsInt, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + PWetCat, data=AllCollocsInt, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + PWetCat, data=AllCollocsInt, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "PWetCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "PWetCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

PWetPlt <- ggplot(Cats, aes(x=as.numeric(as.character(PWetCat)),y=p50AODerr)) +
  geom_density(data=AllCollocsInt, aes(x=PWetNLCD*100, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=3)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("% Water or Wetland", limits=c(0,75)) +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))

AllCollocsInt$RedCat <- cut(AllCollocsInt$Hematite_red, c(-0.1,0.03,0.2,0.4,0.6,0.8),c(0,0.1,0.3,0.5,0.7))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + RedCat, data=AllCollocsInt, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + RedCat, data=AllCollocsInt, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + RedCat, data=AllCollocsInt, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "RedCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "RedCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

RedPlt <- ggplot(Cats, aes(x=as.numeric(as.character(RedCat)),y=p50AODerr)) +
  geom_density(data=AllCollocsInt, aes(x=Hematite_red, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=0.02)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Soil Redness") +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))

AllCollocsInt$CombResAlg <- ifelse(AllCollocsInt$Algorithm=="DT" & AllCollocsInt$ResLab == "3 km", "3km-DT", paste("10km-", as.character(AllCollocsInt$Algorithm), sep=""))
LandUsePlt <- ggplot(subset(AllCollocsInt, !is.na(AllCollocsInt$LandUse)), aes(y=AODerr, x=LandUse, fill=CombResAlg)) +
  geom_hline(aes(yintercept=0), linetype="dotted") +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  scale_y_continuous(expression(paste("( ", tau[M], "-", tau[A], ")")), limits=c(-0.25,0.5), oob=squish) +
  xlab("Land Use Type") +
  facet_grid(East2 ~ QAClab) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"), axis.text.x=element_text(angle=325, vjust=0.5))

png("H:/Rotation_Yang/Imagery/PaperImages/LandUse_AODBias.png", width=8, height=7, units="in", res=300)
grid.arrange(MedElevPlt, grid.arrange(grid.arrange(PImpervPlt, PWetPlt, RedPlt, nrow=1, ncol=3, widths=c(3,3,3)), LandUsePlt, nrow=2, ncol=1, heights=c(5,3)), ncol=2, nrow=1, widths=c(2,5))
dev.off()


# Look at bias in any red by land type
ggplot(subset(AllCollocsInt, !is.na(AllCollocsInt$LandUse) & !is.na(AllCollocsInt$Red2)), aes(y=AODerr, x=LandUse, linetype=ResLab, fill=Algorithm)) + geom_hline(aes(yintercept=0), linetype="dotted") + geom_boxplot() + facet_grid(East2 ~ QAClab) # Nothing interesting
## -------
# Fourth figure - AOD error (MODIS AOD - AERONET AOD) vs. predictor values - by region, algorithm, QACcode, and resolution for sources of bias associated with atmospheric conditions and season: Month of year; NDVI; Watercm. At bottom, show Watercm by SO4 and Na content (boxplot, high high high vs low low low)
## -------

AllCollocs$MOYCat <- cut(AllCollocs$MonthNum, c(0,2,4,6,8,10,12),c(1,3,5,7,9,11))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + MOYCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + MOYCat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + MOYCat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "MOYCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "MOYCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

MOYPlt <- ggplot(Cats, aes(x=as.numeric(as.character(MOYCat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=MonthNum, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70", adjust=2) +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=0.1)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous(expression(paste("Bias in MODIS AOD relative to AERONET ( ", tau[M], "-", tau[A], ")")), limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Month of Year", breaks=c(2,5,8,11), labels=c("Feb", "May", "Aug", "Nov")) +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))


AllCollocs$NDVICat <- cut(AllCollocs$NDVI, c(-0.3,0,0.2,0.4,0.6,0.8,1.1),c(-0.1,0.1,0.3,0.5,0.7,0.9))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + NDVICat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + NDVICat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + NDVICat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "NDVICat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "NDVICat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

NDVIPlt <- ggplot(Cats, aes(x=as.numeric(as.character(NDVICat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=NDVI, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=0.05)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("NDVI") +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Angle\n Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))



AllCollocs$WcmCat <- cut(AllCollocs$AeroWatercm, c(0,0.75,1.25,1.75,2.5,4,7),c(0.5, 1,1.5,2.1,3,5))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + WcmCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + WcmCat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + WcmCat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "WcmCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "WcmCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

WatercmPlt <- ggplot(Cats, aes(x=as.numeric(as.character(WcmCat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=AeroWatercm, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=0.2)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Total column precipitable \nwater (cm)") +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))

#AllCollocsInt$NaCat <- cut(AllCollocsInt$Na_Castnet, c(-0.1,0.08,0.13,0.22,4), c("Q1", "Q2", "Q3", "Q4"))
#AllCollocsInt$SO4Cat <- cut(AllCollocsInt$SO4_Castnet, c(-0.1,0.75,1,2.4,4), c("Q1", "Q2", "Q3", "Q4"))
#AllCollocsInt$WcmCat2 <- cut(AllCollocsInt$AeroWatercm, c(0,0.68, 1.1, 1.7,7), c("Q1", "Q2", "Q3", "Q4"))
AllCollocsInt$Hygroscop <- ifelse(AllCollocsInt$Na_Castnet >1 | AllCollocsInt$SO4_Castnet > 2.5, "High", ifelse(AllCollocs$SO4_Castnet > 0.8, "Med", "Low"))
AllCollocsInt$CombResAlg <- ifelse(AllCollocsInt$Algorithm=="DT" & AllCollocsInt$ResLab == "3 km", "3km-DT", paste("10km-", as.character(AllCollocsInt$Algorithm), sep=""))

HygroPlt <- ggplot(AllCollocsInt, aes(x=Hygroscop, y=AODerr, fill=CombResAlg)) +
  geom_boxplot(outlier.size=0) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("",limits=c(-0.05,0.35)) +
  scale_x_discrete("Relative concentration of Sodium and Sulfate" , breaks=c("High", "Med", "Low"), limits=c("Low", "Med", "High")) +
  scale_fill_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c"), guide=guide_legend(nrow=2, byrow = T)) +
  facet_grid(East2 + QAClab ~.) +
  theme(legend.position="bottom", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))


AllCollocs$NaCat <- cut(AllCollocs$Na_Castnet, c(-0.1,0.08,0.11,0.2,0.22,4),c(0,0.1,0.16,0.21,0.5))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + NaCat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + NaCat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + NaCat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "NaCat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "NaCat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

NaPlt <- ggplot(Cats, aes(x=as.numeric(as.character(NaCat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=Na_Castnet, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=0.01)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Ambient sodium \nconcentration (ug/m^3)", limits=c(0,1)) +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="mone", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm")) # Meh - maybe only show part of this?

AllCollocs$SO4Cat <- cut(AllCollocs$SO4_Castnet, c(-0.1,1,2,3,4),c(0.5,1.5,2.5,3.5))
CatsMean <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SO4Cat, data=AllCollocs, mean)
CatsMed <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SO4Cat, data=AllCollocs, median)
Catsp <- aggregate(AODerr ~ Algorithm + East2 + ResLab + QAClab + SO4Cat, data=AllCollocs, quantile)
Cats <- merge(CatsMean, Catsp, by=c("Algorithm", "East2", "ResLab", "QAClab", "SO4Cat"))
Cats <- cbind.data.frame(Cats[1:6], Cats$AODerr.y)
colnames(Cats) <- c("Algorithm", "East2", "ResLab", "QAClab", "SO4Cat", "MeanAODerr", "MinAODerr", "p25AODerr", "p50AODerr", "p75AODerr", "MaxAODerr")
Cats$CombResAlg <- ifelse(Cats$Algorithm=="DT" & Cats$ResLab == "3 km", "3km-DT", paste("10km-", as.character(Cats$Algorithm), sep=""))

SO4Plt <- ggplot(Cats, aes(x=as.numeric(as.character(SO4Cat)),y=p50AODerr)) +
  geom_density(data=AllCollocs, aes(x=SO4_Castnet, y=(..scaled../4)), alpha=0.5, color=NA, fill="grey70") +
  geom_pointrange(aes(color=CombResAlg, ymax=p75AODerr, ymin=p25AODerr), alpha=0.8, position=position_jitter(width=0.1)) +
  geom_line(aes(color=CombResAlg), alpha=0.8) +
  geom_hline(yintercept=0, linetype=3) +
  scale_y_continuous("", limits=c(-0.05, 0.35), oob=squish) +
  scale_x_continuous("Ambient sulfate \nconcentration (ug/m^3)") +
  #scale_color_brewer("AOD \nProduct", type="div", palette="Spectral") +
  scale_color_manual("AOD \nProduct", values=c("3km-DT"="#2b83ba", "10km-DT"="#abdda4", "10km-M"="#fdae61", "10km-DB"="#d7191c")) +
  #scale_fill_manual("", values = c("Distribution \n of Values"="grey70")) +
  facet_grid(East2 + QAClab~.) +
  theme(legend.position="none", panel.background=element_rect(fill="white", color="grey30"), legend.key=element_rect(fill="white"), strip.background=element_rect(color="grey30", fill="grey90"), panel.grid.minor=element_line(color=NA), panel.grid.major=element_line(color=NA), plot.margin=unit(rep(0.05,4), "cm"))


png("H:/Rotation_Yang/Imagery/PaperImages/SeasonWeather_AODBias.png", width=8, height=7, units="in", res=300)
grid.arrange(MOYPlt, NDVIPlt, WatercmPlt, HygroPlt, nrow=1, ncol=4, widths=c(2,2,2,3))
dev.off()


## ------------------
# Accuracy table - all CONUS and seasonal breakdowns
## ------------------

LinModCoefs <- ddply(AllCollocs, .(QAClab, Algorithm, ResLab), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(AllCollocs, .(QAClab,Algorithm,ResLab), PercEE, Mod="ModAOD", Aero="AeroAOD")
PropEE$MeetsPreLaunch <- cut(PropEE$PInEE, breaks=c(0,0.68,1), labels=c("No","Yes"))
OneOneL <- cbind.data.frame(LinModCoefs[,1:3], rep(0,11), rep(1,11), rep(NA,11))
colnames(OneOneL) <- c("QAClab", "Algorithm", "ResLab", "a", "b", "adjR2")
UpEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(0.05,11), rep(1.15,11), rep(NA,11))
colnames(UpEEL) <- c("QAClab", "Algorithm", "ResLab", "a", "b", "adjR2")
DownEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(-0.05,11), rep(0.85,11), rep(NA,11))
colnames(DownEEL) <- c("QAClab", "Algorithm", "ResLab", "a", "b", "adjR2")
ABLines <- rbind.data.frame(LinModCoefs, OneOneL, UpEEL, DownEEL)
ABLines$lntyp <- c(rep("dotted",11), rep("solid",11), rep("dotdash",22))
rm(OneOneL, UpEEL, DownEEL)
AODerr <- aggregate(AODerr ~ ResLab + QAClab + Algorithm, data=AllCollocs, median)
Length <- aggregate(AODerr ~ ResLab + QAClab + Algorithm, data=AllCollocs, length)
AODerr <- merge(AODerr, Length, by=c("ResLab", "QAClab", "Algorithm"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("ResLab", "QAClab", "Algorithm"))
TableRegs <- merge(TableRegs, PropEE, by=c("ResLab", "QAClab", "Algorithm"))

toLatex.xtable(xtable(TableRegs[,c(1:3,5,4,6:11)]), include.rownames=F)


LinModCoefs <- ddply(AllCollocs, .(QAClab, Algorithm, ResLab, Season), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(AllCollocs, .(QAClab,Algorithm,ResLab, Season), PercEE, Mod="ModAOD", Aero="AeroAOD")
#PropEE$MeetsPreLaunch <- cut(PropEE$PInEE, breaks=c(0,0.68,1), labels=c("No","Yes"))
OneOneL <- cbind.data.frame(LinModCoefs[,1:4], rep(0,44), rep(1,44), rep(NA,44))
colnames(OneOneL) <- c("QAClab", "Algorithm", "ResLab", "Season", "a", "b", "adjR2")
UpEEL <- cbind.data.frame(LinModCoefs[,1:4], rep(0.05,44), rep(1.15,44), rep(NA,44))
colnames(UpEEL) <- c("QAClab", "Algorithm", "ResLab", "Season", "a", "b", "adjR2")
DownEEL <- cbind.data.frame(LinModCoefs[,1:4], rep(-0.05,44), rep(0.85,44), rep(NA,44))
colnames(DownEEL) <- c("QAClab", "Algorithm", "ResLab", "Season", "a", "b", "adjR2")
ABLines <- rbind.data.frame(LinModCoefs, OneOneL, UpEEL, DownEEL)
ABLines$lntyp <- c(rep("dotted",44), rep("solid",44), rep("dotdash",88))
rm(OneOneL, UpEEL, DownEEL)
AODerr <- aggregate(AODerr ~ ResLab + QAClab + Algorithm + Season, data=AllCollocs, median)
Length <- aggregate(AODerr ~ ResLab + QAClab + Algorithm + Season, data=AllCollocs, length)
AODerr <- merge(AODerr, Length, by=c("ResLab", "QAClab", "Algorithm", "Season"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("ResLab", "QAClab", "Algorithm", "Season"))
TableRegs <- merge(TableRegs, PropEE, by=c("ResLab", "QAClab", "Algorithm", "Season"))

toLatex.xtable(xtable(TableRegs[,c(1:4,6,5,7:12)]), include.rownames=F)

## -------
# Fifth figure - site-level correlations (prep dataset here, and map in Arc)
# Note - need to eliminate sites w/ less than 2-3 collocations - check too see if can find citation for this or better threshold - Dropping this for now
## -------


## ----------
# Build 2-piece spline model (regional) for DB observations (high quality): AOD_A ~ \alpha + \beta*AOD_M + \beta_2*(Knot-AOD_M)
## ----------

DBdat <- subset(AllCollocs, AllCollocs$Algorithm=="DB" & AllCollocs$QAClab == "QAC 3")
splineknots <- seq(0,1,0.01)
KnotAnal <- sapply(splineknots, Splknot, dat=DBdat, eqn=AeroAOD ~ ModAOD + spl - 1)
plot(KnotAnal~splineknots)
splineknots[which.max(KnotAnal)]

KnotAnal <- sapply(splineknots, Splknot, dat=subset(DBdat, DBdat$East==1), eqn=AeroAOD~ModAOD + spl)
plot(KnotAnal~splineknots)
splineknots[which.min(KnotAnal)]

KnotAnal <- sapply(splineknots, Splknot, dat=subset(DBdat, DBdat$East==0), eqn=AeroAOD~ModAOD + spl)
plot(KnotAnal~splineknots)
splineknots[which.min(KnotAnal)]

DBdat$HighScatter <- ifelse(DBdat$Scatter > 160, 1, 0)
DBdat$SensorASpline <- ifelse(DBdat$SensorZenith > 40, DBdat$SensorZenith - 40, 0)
KnotAnal <- sapply(splineknots, Splknot, dat=subset(DBdat, DBdat$East==1), eqn=AeroAOD~ModAOD + spl + SensorZenith + SensorASpline + HighScatter)
plot(KnotAnal~splineknots)
splineknots[which.max(KnotAnal)]

KnotAnal <- sapply(splineknots, Splknot, dat=subset(DBdat, DBdat$East==0), eqn=AeroAOD~ModAOD + spl + SensorZenith + SensorASpline + HighScatter + ModAOD*SensorZenith + ModAOD*SensorASpline + spl*SensorZenith + spl*SensorASpline + ModAOD*HighScatter + spl*HighScatter + HighScatter*SensorZenith + HighScatter*SensorASpline)
plot(KnotAnal~splineknots)
splineknots[which.max(KnotAnal)]

DBdatWest <- subset(DBdat, DBdat$East == 0)
DBdatWest$ModSpline <- ifelse(DBdatWest$ModAOD < 0.15, 0, DBdatWest$ModAOD-0.15)
summary(lm(AeroAOD ~ ModAOD + ModSpline + SensorZenith + SensorASpline + SensorZenith*ModAOD + SensorZenith*ModSpline + SensorASpline*ModAOD + SensorASpline*ModSpline, data=DBdatWest))
plot(predict(lm(AeroAOD ~ ModAOD + ModSpline + SensorZenith + SensorASpline + SensorZenith*ModAOD + SensorZenith*ModSpline + SensorASpline*ModAOD + SensorASpline*ModSpline, data=DBdatWest)), DBdatWest$ModAOD)

DBdatWest$ModSpline2 <- ifelse(DBdatWest$ModAOD < 0.4, 0, DBdatWest$ModAOD-0.4)
Mod <- lm(AeroAOD ~ ModAOD + ModSpline + ModSpline2, data=DBdatWest)
plot(DBdatWest$AeroAOD, predict(Mod), ylim=c(0,1.2), xlim=c(0,1.2))
summary(Mod)


## ------------------
# Modeling - add this in if room and time later
## ------------------

## -------
# Check for multicollinearity
## -------

# Look at correlation matrix - highish off-diagonal values indicate potential problem w/ those variables

# Run PCA on predictors - examine # components

# Calculate VIF scores for each predictor

# Look into transformations and categorizations of variables

# Redo VIF calculations for any variables that have been altered

# Remove or combine any collinear variables

## -------
# Regression modeling - by region
## -------

# East - gold standard = all variables remaining after categorization/tranformation and collinearity analysis

# East - optimal model = gold standard w/ non confounders and non-sig vars removed + interactions

# West - gold standard

# West - optimal model

## -------
# Table - regression modeling results
## -------

# Table structure: Res, alg, QAC, Region, Model, predictors, fitted model, R^2/AIC

## -------
# Fourth figure - Funnel plots of regression results - from optimal models only
## -------


