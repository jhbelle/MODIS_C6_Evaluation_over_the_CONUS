## ---------------
## Name: RmNA_ForLinkfiles.r
## Program version: R 3.1.0
## Dependencies:
## Author: J.H. Belle
## Purpose: Read in the ForLink files and remove missing values properly (i.e. - remove entire collocation if pixel could not be reconstructed because it was in a corner)
## ---------------

QA1B <- read.csv("H:/Rotation_Yang/ForLink_QA1B.csv")
str(QA1B)
summary(QA1B)
# Removing Atype1 var - all missing
QA1B$Atype1 <- NULL
write.csv(QA1B, "H:/Rotation_Yang/ForLink_QA1B.csv")

QA2B <- read.csv("H:/Rotation_Yang/ForLink_QA2B.csv")
str(QA2B)
summary(QA2B)
# Removing Atype1 var - all missing
QA2B$Atype1 <- NULL
write.csv(QA2B, "H:/Rotation_Yang/ForLink_QA2B.csv")

QA3B <- read.csv("H:/Rotation_Yang/ForLink_QA3B.csv")
str(QA3B)
summary(QA3B)
# Removing Atype1 var - all missing
QA3B$Atype1 <- NULL
NAs <- cbind.data.frame(QA3B[which(is.na(QA3B$DL_Lat)), 2:5], "Mark"=rep(1, 12))
QA3Bt <- merge(QA3B, NAs, by=c("Year", "AeroLoc", "JulianDate", "Passtime"), all.x=T)
QA3B <- QA3Bt[is.na(QA3Bt$Mark),-24]
write.csv(QA3B, "H:/Rotation_Yang/ForLink_QA3B.csv")

rm(QA1B, QA2B, QA3B, QA3Bt, NAs)

QA1DB <- read.csv("H:/Rotation_Yang/ForLink_QA1DB.csv")
str(QA1DB)
summary(QA1DB)
# Removing Atype1 var - all missing
QA1DB$Atype1 <- NULL
NAs <- cbind.data.frame(QA1DB[which(is.na(QA1DB$DL_Lat)), 2:5], "Mark"=rep(1, 2))
QA1DBt <- merge(QA1DB, NAs, by=c("Year", "AeroLoc", "JulianDate", "Passtime"), all.x=T)
QA1DB <- QA1DBt[is.na(QA1DBt$Mark),-24]
write.csv(QA1DB, "H:/Rotation_Yang/ForLink_QA1DB.csv")

QA2DB <- read.csv("H:/Rotation_Yang/ForLink_QA2DB.csv")
str(QA2DB)
summary(QA2DB)
# Removing Atype1 var - all missing
QA2DB$Atype1 <- NULL
write.csv(QA2DB, "H:/Rotation_Yang/ForLink_QA2DB.csv")

QA3DB <- read.csv("H:/Rotation_Yang/ForLink_QA3DB.csv")
str(QA3DB)
summary(QA3DB)
# Removing Atype1 var - all missing
QA3DB$Atype1 <- NULL
NAs <- cbind.data.frame(QA3DB[which(is.na(QA3DB$DL_Lat)), 2:5], "Mark"=rep(1, 21))
QA3DBt <- merge(QA3DB, NAs, by=c("Year", "AeroLoc", "JulianDate", "Passtime"), all.x=T)
QA3DB <- QA3DBt[is.na(QA3DBt$Mark),-24]
write.csv(QA3DB, "H:/Rotation_Yang/ForLink_QA3DB.csv")

rm(QA1DB, QA1DBt, QA2DB, QA3DB, QA3DBt, NAs)

## For DT - just recording the number missing from each dataset - will remove on post-processing 

QA0DT <- read.csv("H:/Rotation_Yang/ForLink_QA0DT.csv")
str(QA0DT)
summary(QA0DT)
# 1 NA

QA1DT <- read.csv("H:/Rotation_Yang/ForLink_QA1DT.csv")
str(QA1DT)
summary(QA1DT)
# 0 NA

QA2DT <- read.csv("H:/Rotation_Yang/ForLink_QA2DT.csv")
str(QA2DT)
summary(QA2DT)
# 1 NA

QA3DT <- read.csv("H:/Rotation_Yang/ForLink_QA3DT.csv")
str(QA3DT)
summary(QA3DT)
QA3DT$Atype1 <- NULL
write.csv(QA3DT, "H:/Rotation_Yang/ForLink_QA3DT.csv")
# 7 NA - also missing Atype1 (a lot)
