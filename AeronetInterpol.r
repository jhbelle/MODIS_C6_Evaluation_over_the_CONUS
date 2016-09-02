## --------------------------
## Name: AeronetInterpol.r
## Date Created: 6/17/14
## Program version: R 3.1.0
## Dependencies: plyr, foreach, doParallel, iterators (required by doParallel)
## Author: J.H. Belle
## Purpose: Read in AERONET data points and interpolate each set to 550 nm
## --------------------------

library(plyr)
#library(foreach)
#library(doParallel)

## Function - to do interpolation in each row
GetAOD550 <- function(lineaod) {
  Wavelength <- c(1640, 1020, 870, 675, 667, 555, 551, 532, 500, 490, 443, 440, 412, 380, 340)
  Vect <- as.numeric(c(lineaod))
  #NoNeg <- as.numeric(ifelse(Vect <= 0, 0.000001, Vect))
  NoMiss <- na.omit(cbind.data.frame(log(Wavelength), log(Vect)))
  colnames(NoMiss) <- c('wl', 'aod')
  if (length(NoMiss$wl) < 4){
    AOD550 <- NA
  } else{
    fitM <- lm(aod~poly(wl, 2), NoMiss)
    AOD550 <- exp(predict(fitM, data.frame(wl=log(550))))
  }
  return(AOD550)
}

#registerDoParallel(cores=3)
for (filename in c('AERONETcollocs11', 'AERONETcollocs12', 'AERONETcollocs13')){
  Aero <- read.csv(paste('D:/YangRotation/', filename, '.csv', sep=""), na.strings='N/A', stringsAsFactors=FALSE)
  AODonly <- as.array(unname(Aero[,c(8:15,17:23)]))
  AOD550 <- alply(AODonly, 1, GetAOD550)
  Outp <- cbind.data.frame(Aero$Location, Aero$Day, Aero$lat, Aero$long, Aero$Elevation, Aero$Timestamp, Aero$Water_cm, as.numeric(AOD550))
  colnames(Outp) <- c('Location', 'Day', 'Lat', 'Long', 'Elev', 'Timestamp', 'Water_cm', 'AOD550nm')
  write.csv(Outp, file=paste('D:/YangRotation/', filename, '_550Ints.csv', sep=""))
}
