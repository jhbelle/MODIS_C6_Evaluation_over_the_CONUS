## ------------------
## Name: AccuracyAnal_Tables.R
## Program version: R 3.2.2
## Dependencies: xtable; plyr
## Author: J.H.Belle
## Purpose: Create tables for validation paper
## ------------------

library(xtable)
library(plyr)
library(dplyr)
source("H:/Rotation_Yang/Fuctions_RadMatch10km.r")
## --------
# Coverage analysis
## --------

# Read in data - 10 km
All <- read.csv("H:/Rotation_Yang/AggDatDecade.csv")[,c(2,12,14,15,17,18,20)]
Winter <- read.csv("H:/Rotation_Yang/AggDatDecade12.csv")[,c(2,12,14,15,17,18,20)]
Spring <- read.csv("H:/Rotation_Yang/AggDatDecade3.csv")[,c(2,12,14,15,17,18,20)]
Summer <- read.csv("H:/Rotation_Yang/AggDatDecade6.csv")[,c(2,12,14,15,17,18,20)]
Fall <- read.csv("H:/Rotation_Yang/AggDatDecade9.csv")[,c(2,12,14,15,17,18,20)]
AllKinds <- rbind.data.frame(All, Winter, Spring, Summer, Fall)
AllKinds$Season <- as.character(c(rep("All", nrow(All)), rep("Winter", nrow(Winter)), rep("Spring", nrow(Spring)), rep("Summer", nrow(Summer)), rep("Fall", nrow(Fall))))

Spring3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km3.csv")[,c(2,6:8)]
Summer3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km6.csv")[,c(2,6:8)]
Fall3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km9.csv")[,c(2,6:8)]
Winter3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km12.csv")[,c(2,6:8)]
All3km <- read.csv("H:/Rotation_Yang/3kmGridding/AggDat3km.csv")[,c(2,6:8)]
AllKinds3km <- rbind.data.frame(All3km, Winter3km, Spring3km, Summer3km, Fall3km)
AllKinds3km <- cbind.data.frame(AllKinds3km[,c(1,2,4)])
colnames(AllKinds3km) <- c("ID", "PCov3kmDT3", "PCov3kmDT123")
AllKinds3km$Season <- as.character(c(rep("All", nrow(All3km)), rep("Winter", nrow(Winter3km)), rep("Spring", nrow(Spring3km)), rep("Summer", nrow(Summer3km)), rep("Fall", nrow(Fall3km))))

# Put it all together
AllKinds <- merge(AllKinds, AllKinds3km, by=c("ID", "Season"))
rm(AllKinds3km, Fall, Fall3km, Spring, Spring3km, Summer, Summer3km, Winter, Winter3km, All, All3km)
# Add in grid values
East <- read.csv("H:/Rotation_Yang/GridPopLakes2.csv")[,c(3,7:14)]
AllKinds2 <- merge(AllKinds, East, by.x="ID", by.y="Input_FID", all.x=T)
# Remove lake and ocean pixels
AllKinds <- AllKinds2[AllKinds2$Lake==0,]
AllKinds$Lake <- NULL
PercImperv <- read.csv("H:/Rotation_Yang/Coverage/PercImperv_Grid.csv")[,c(2,5)]
AllKinds <- merge(AllKinds, PercImperv, by.x="ID", by.y="Input_FID")
AllKinds$PImpervCat <- as.numeric(as.character(cut(AllKinds$MEAN, c(-1,10,20,30,40,50,60,70,80), labels=c(5,15,25,35,45,55,65,75))))
rm(AllKinds2, East, PercImperv)
AllKinds$East <- cut(AllKinds$East2, c(-1,0.5,2), c("West", "East"))

##----
# Table 1: Coverage and population-weighted coverage statistics, overall and broken down by season
##----

toLatex.xtable(xtable(ddply(AllKinds, .(Season), CalcRes2), digits=1), include.rownames=F)

##----
# Table S1: Coverage broken down by dominant land use type
##----
SubKinds <- subset(AllKinds, AllKinds$Landcover < 25 & AllKinds$Landcover > 20)
SubKinds$Landcover2 <- cut(SubKinds$Landcover, c(5,11.5,13,21.5,22.5,23.5,30,40,41.5,42.5,45,60,80,81.5,85,92,100), c("Open Water", "Perennial Ice/Snow", "< 20%", "20-50%", "50-80%", "> 80%", "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest","Shrub/Scrub", "Grassland", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"))
toLatex.xtable(xtable(ddply(SubKinds, .(Landcover2), CalcRes2), digits=1), include.rownames=F)
toLatex.xtable(xtable(ddply(SubKinds, .(Landcover2, East2), CalcRes2), digits=1), include.rownames=F)
#rm(SubKinds, AllKinds)


# Same table but with Percent impervious data
toLatex.xtable(xtable(ddply(AllKinds, .(East2, PImpervCat), CalcRes2), digits=1), include.rownames=F)

test <- ddply(AllKinds, .(East2, PImpervCat), CalcRes2)
library(ggplot2)
ggplot(test, aes(y=Covs3, x=PImpervCat)) + geom_line() + geom_line(aes(y=PCovs3), color="dodgerblue", alpha=0.5) + facet_grid(East2 ~ Alg) + theme_classic(base_size = 10)
# Conclusion - sampling gradient likely a geographic accident

# Last check association between coverage and population count
AllKinds$PopCat <- cut(AllKinds$Pop, c(-1, 6, 24, 76, 231, 562, 1200, 2655, 10000, 100000, 2294500))
test <- ddply(AllKinds, .(East2, PopCat), CalcRes2)
library(ggplot2)
ggplot(test, aes(y=Covs3, x=PopCat, color=Alg)) + geom_point(shape=) + facet_grid(East2 ~ .) + theme_classic()
summary(subset(AllKinds$Pop, AllKinds$Season=="All"))

gDT <- ggplot(subset(AllKinds, AllKinds$Season=="All"), aes(y=PCovDT3, x=Pop/1000000)) + geom_bin2d(bins=50) + facet_grid(East~.) + scale_fill_gradient("No. Grid cells", low="gray80", high="black") + xlab("Population (millions)") + scale_y_continuous("Coverage (%)", c(0,0.5,2)) + theme_classic(base_size = 10)

gDB <- ggplot(subset(AllKinds, AllKinds$Season=="All"), aes(y=PCovDB3, x=Pop/1000000)) + geom_bin2d(bins=50) + facet_grid(East~.) + scale_fill_gradient("No. Grid cells", low="gray80", high="black") + xlab("Population (millions)") + scale_y_continuous("Coverage (%)", c(0,0.5,2)) + theme_classic(base_size = 10)

UrbanPlt <- plot_grid(gDT, gDB, labels="AUTO", ncol=2, nrow=1, label_size=10)


pdf("H:/Rotation_Yang/Imagery/PaperImages/PopxCovDist.pdf", family="Times", width=7.48, height=4.53)
grid.draw(UrbanPlt)
dev.off()

## ------
# Accuracy analysis
## ------

EQCollocs <- read.csv("H:/Rotation_Yang/AllCollocs.csv", stringsAsFactors = F)
EQCollocs$East2 <- cut(EQCollocs$East, breaks=c(-0.5,0.5,1.5), labels=c("West", "East"))
EQCollocs$ResLab <- paste(EQCollocs$Resolution, "km")
EQCollocs$QAClab <- paste("QAC", EQCollocs$QACcode)
EQCollocs$PImpervMean <- ifelse(EQCollocs$PImpervMean > 100, NA, EQCollocs$PImpervMean)
EQCollocs$AODerr <- EQCollocs$ModAOD - EQCollocs$AeroAOD
EQCollocs$Month <- strftime(strptime(paste(EQCollocs$JulianDate, EQCollocs$Year, sep="-"), "%j-%Y"), "%b")
EQCollocs$MonthNum <- as.numeric(strftime(strptime(paste(EQCollocs$JulianDate, EQCollocs$Year, sep="-"), "%j-%Y"), "%m"))
EQCollocs$Season <- ifelse(EQCollocs$Month %in% c("Jan", "Feb", "Dec"), "Winter", ifelse(EQCollocs$Month %in% c("Mar", "Apr", "May"), "Spring", ifelse(EQCollocs$Month %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
EQCollocs$Algorithm <- ifelse(EQCollocs$Algorithm == "B", "M", as.character(EQCollocs$Algorithm))
EQCollocs$CombResAlg <- ifelse(EQCollocs$Algorithm=="DT" & EQCollocs$ResLab == "3 km", "3 km DT", paste("10 km ", as.character(EQCollocs$Algorithm), sep=""))
EQCollocs$CombResAlg <- ifelse(EQCollocs$CombResAlg == "10 km M", "10 km DB-DT", EQCollocs$CombResAlg)
EQCollocs$CombResAlg <- as.ordered(EQCollocs$CombResAlg)
EQCollocs$CombResAlg <- reorder(EQCollocs$CombResAlg, ifelse(EQCollocs$CombResAlg == "3 km DT", 1, ifelse(EQCollocs$CombResAlg == "10 km DT", 2, ifelse(EQCollocs$CombResAlg == "10 km DB", 4, 3))), median)
EQCollocs$Dragon <- ifelse(substr(EQCollocs$AeroLoc, 1, 6) == "DRAGON", T, F)

aggregate(AeroAOD ~ CombResAlg + QAClab + East2, EQCollocs, summary)
aggregate(ModAOD ~ CombResAlg + QAClab + East2, EQCollocs, summary)
##----
# Table 2: Accuracy statistics - broken down by region
##----

LinModCoefs <- ddply(EQCollocs, .(East2, QAClab, CombResAlg), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(EQCollocs, .(East2,QAClab,CombResAlg), PercEE, Mod="ModAOD", Aero="AeroAOD")
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
Length <- aggregate(AODerr ~ CombResAlg + QAClab + East2, data=EQCollocs, length)
AODerr <- aggregate(AODerr ~ CombResAlg + QAClab + East2, data=EQCollocs, median)
AODerr <- merge(Length, AODerr, by=c("East2", "QAClab", "CombResAlg"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("East2", "QAClab", "CombResAlg"))
TableRegs <- merge(TableRegs, PropEE, by=c("East2", "QAClab", "CombResAlg"))

TableRegs[,9:11] <- TableRegs[,9:11]*100
TableRegs$adjR2 <- sqrt(TableRegs$adjR2)
TableRegs <- arrange(TableRegs[,1:11], TableRegs$East2, TableRegs$QAClab, TableRegs$CombResAlg)
TableRegs$East2 <- replace(as.character(TableRegs$East2), c(1:5,7:16,18:22), " ")
TableRegs$QAClab <- replace(as.character(TableRegs$QAClab), c(1,3:5,7,8,10:12,14:16,18,19,21,22), " ")
toLatex.xtable(xtable(TableRegs, digits=c(1,1,1,0,2,2,2,2,2,1,1,1)), include.rownames=F, format.args=list(big.mark=","), hline.after=c(-1,0,4,7,11,15,18,22))

##----
# Table S2: Accuracy statistics - broken down by season
##----

LinModCoefs <- ddply(EQCollocs, .(Season, QACcode, CombResAlg), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(EQCollocs, .(Season,QACcode,CombResAlg), PercEE, Mod="ModAOD", Aero="AeroAOD")
PropEE$MeetsPreLaunch <- cut(PropEE$PInEE, breaks=c(0,0.68,1), labels=c("No","Yes"))
OneOneL <- cbind.data.frame(LinModCoefs[,1:3], rep(0,44), rep(1,44), rep(NA,44))
colnames(OneOneL) <- c("Season", "QACcode", "CombResAlg", "a", "b", "adjR2")
UpEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(0.05,44), rep(1.15,44), rep(NA,44))
colnames(UpEEL) <- c("Season", "QACcode", "CombResAlg", "a", "b", "adjR2")
DownEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(-0.05,44), rep(0.85,44), rep(NA,44))
colnames(DownEEL) <- c("Season", "QACcode", "CombResAlg", "a", "b", "adjR2")
ABLines <- rbind.data.frame(LinModCoefs, OneOneL, UpEEL, DownEEL)
ABLines$lntyp <- c(rep("dotted",44), rep("solid",44), rep("dotdash",88))
rm(OneOneL, UpEEL, DownEEL)
Length <- aggregate(AODerr ~ CombResAlg + QACcode + Season, data=EQCollocs, length)
AODerr <- aggregate(AODerr ~ CombResAlg + QACcode + Season, data=EQCollocs, median)
AODerr <- merge(Length, AODerr, by=c("CombResAlg", "QACcode", "Season"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("CombResAlg", "QACcode", "Season"))
TableRegs <- merge(TableRegs, PropEE, by=c("CombResAlg", "QACcode", "Season"))

toLatex.xtable(xtable(TableRegs[,1:11]), include.rownames=F, format.args=list(big.mark=","))
# Leaving out of paper - graphic S4 basically shows same

##----
# Table S3: Accuracy statistics - broken down by Dragon/Permanent status of AERONET station
##----

LinModCoefs <- ddply(EQCollocs, .(Dragon, QACcode, CombResAlg), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(EQCollocs, .(Dragon,QACcode,CombResAlg), PercEE, Mod="ModAOD", Aero="AeroAOD")
PropEE$MeetsPreLaunch <- cut(PropEE$PInEE, breaks=c(0,0.68,1), labels=c("No","Yes"))
OneOneL <- cbind.data.frame(LinModCoefs[,1:3], rep(0,21), rep(1,21), rep(NA,21))
colnames(OneOneL) <- c("Dragon", "QACcode", "CombResAlg", "a", "b", "adjR2")
UpEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(0.05,21), rep(1.15,21), rep(NA,21))
colnames(UpEEL) <- c("Dragon", "QACcode", "CombResAlg", "a", "b", "adjR2")
DownEEL <- cbind.data.frame(LinModCoefs[,1:3], rep(-0.05,21), rep(0.85,21), rep(NA,21))
colnames(DownEEL) <- c("Dragon", "QACcode", "CombResAlg", "a", "b", "adjR2")
ABLines <- rbind.data.frame(LinModCoefs, OneOneL, UpEEL, DownEEL)
ABLines$lntyp <- c(rep("dotted",21), rep("solid",21), rep("dotdash",42))
rm(OneOneL, UpEEL, DownEEL)
Length <- aggregate(AODerr ~ CombResAlg + QACcode + Dragon, data=EQCollocs, length)
AODerr <- aggregate(AODerr ~ CombResAlg + QACcode + Dragon, data=EQCollocs, median)
AODerr <- merge(Length, AODerr, by=c("CombResAlg", "QACcode", "Dragon"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("CombResAlg", "QACcode", "Dragon"))
TableRegs <- merge(TableRegs, PropEE, by=c("CombResAlg", "QACcode", "Dragon"))

toLatex.xtable(xtable(TableRegs[,1:11]), include.rownames=F, format.args=list(big.mark=","))
# Including in supplement for completeness??? - Nothing notable
rm(ABLines, AODerr, EQCollocs, LinModCoefs, PropEE, TableRegs, Length)

# Overall stats
LinModCoefs <- ddply(EQCollocs, .(QAClab, CombResAlg), RunLMDD, form=ModAOD~AeroAOD)
PropEE <- ddply(EQCollocs, .(QAClab,CombResAlg), PercEE, Mod="ModAOD", Aero="AeroAOD")
PropEE$MeetsPreLaunch <- cut(PropEE$PInEE, breaks=c(0,0.68,1), labels=c("No","Yes"))
OneOneL <- cbind.data.frame(LinModCoefs[,1:2], rep(0,11), rep(1,11), rep(NA,11))
colnames(OneOneL) <- c("QAClab", "CombResAlg", "a", "b", "adjR2")
UpEEL <- cbind.data.frame(LinModCoefs[,1:2], rep(0.05,11), rep(1.15,11), rep(NA,11))
colnames(UpEEL) <- c("QAClab", "CombResAlg", "a", "b", "adjR2")
DownEEL <- cbind.data.frame(LinModCoefs[,1:2], rep(-0.05,11), rep(0.85,11), rep(NA,11))
colnames(DownEEL) <- c("QAClab", "CombResAlg", "a", "b", "adjR2")
ABLines <- rbind.data.frame(LinModCoefs, OneOneL, UpEEL, DownEEL)
ABLines$lntyp <- c(rep("dotted",11), rep("solid",11), rep("dotdash",22))
rm(OneOneL, UpEEL, DownEEL)
Length <- aggregate(AODerr ~ CombResAlg + QAClab, data=EQCollocs, length)
AODerr <- aggregate(AODerr ~ CombResAlg + QAClab, data=EQCollocs, median)
AODerr <- merge(Length, AODerr, by=c("CombResAlg", "QAClab"))
TableRegs <- merge(AODerr, LinModCoefs, by=c("CombResAlg", "QAClab"))
TableRegs <- merge(TableRegs, PropEE, by=c("CombResAlg", "QAClab"))

toLatex.xtable(xtable(TableRegs[,1:11]), include.rownames=F, format.args=list(big.mark=","))

## ---------
# Analysis of ability to detect high aerosol loadings
## ---------

GECollocs <- read.csv("H:/Rotation_Yang/AllCollocs_GECollocs.csv", stringsAsFactors = F)
GECollocs$East2 <- cut(GECollocs$East, breaks=c(-0.5,0.5,1.5), labels=c("West", "East"))
GECollocs$ResLab <- paste(GECollocs$Resolution, "km")
GECollocs$QAClab <- paste("QAC", GECollocs$QACcode)
GECollocs$PImpervMean <- ifelse(GECollocs$PImpervMean > 100, NA, GECollocs$PImpervMean)
GECollocs$AODerr <- GECollocs$ModAOD - GECollocs$AeroAOD
GECollocs$Month <- strftime(strptime(paste(GECollocs$JulianDate, GECollocs$Year, sep="-"), "%j-%Y"), "%b")
GECollocs$MonthNum <- as.numeric(strftime(strptime(paste(GECollocs$JulianDate, GECollocs$Year, sep="-"), "%j-%Y"), "%m"))
GECollocs$Season <- ifelse(GECollocs$Month %in% c("Jan", "Feb", "Dec"), "Winter", ifelse(GECollocs$Month %in% c("Mar", "Apr", "May"), "Spring", ifelse(GECollocs$Month %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))
GECollocs$Algorithm <- ifelse(GECollocs$Algorithm == "B", "M", as.character(GECollocs$Algorithm))
GECollocs$CombResAlg <- ifelse(GECollocs$Algorithm=="DT" & GECollocs$ResLab == "3 km", "3 km DT", paste("10 km ", as.character(GECollocs$Algorithm), sep=""))
GECollocs$CombResAlg <- ifelse(GECollocs$CombResAlg == "10 km M", "10 km DB-DT", GECollocs$CombResAlg)
GECollocs$CombResAlg <- as.ordered(GECollocs$CombResAlg)
GECollocs$CombResAlg <- reorder(GECollocs$CombResAlg, ifelse(GECollocs$CombResAlg == "3 km DT", 1, ifelse(GECollocs$CombResAlg == "10 km DT", 2, ifelse(GECollocs$CombResAlg == "10 km DB", 4, 3))), median)
GECollocs$Dragon <- ifelse(substr(GECollocs$AeroLoc, 1, 6) == "DRAGON", T, F)

OverallSeSp <- ddply(GECollocs, .(QACcode, CombResAlg), ExVals, xvar="AeroAOD", yvar="ModAOD")
OverallSeSp

##----
# Table 3: Se/Sp statistics for high aerosol loadings, regional
##----

toLatex.xtable(xtable(ddply(GECollocs, .(East2, QACcode, CombResAlg), ExVals, xvar="AeroAOD", yvar="ModAOD")[,c(1:3,5:9)], format=c("s", "s", "s", "d", "d", "d", "f", "f")), include.rownames=F, format.args=list(big.mark=",", drop0trailing=T))

##----
# Table S4: Se/Sp statistics for high aerosol loadings - broken down by Dragon/permanent status of AERONET station
##----

toLatex.xtable(xtable(ddply(GECollocs, .(Dragon, QACcode, CombResAlg), ExVals, xvar="AeroAOD", yvar="ModAOD")[,c(1:3,5:9)], format=c("s", "s", "s", "d", "d", "d", "f", "f")), include.rownames=F, format.args=list(big.mark=",", drop0trailing=T))


rm(GECollocs, OverallSeSp)
