## ------------
## Name: CaseStudy_ExtractMODIS_NDVI.R
## Program version: R 3.3.0
## Dependencies: 
## Author: J.H. Belle
## Purpose: Go through NDVI files and extract to CMAQ polygons
## ------------
library(rgdal)
library(raster)

# Get list of NDVI files
NDVIfiles <- list.files(path="/terra/MODIS_Jess/MODIS_NDVI", pattern="MYD13A2*", full.names=T)

# Read in CMAQ polygons
CMAQpolys <- readOGR("/aqua/Jess/Data/CMAQ_polys/CaseStudy_CMAQcellsProj.shp", "CaseStudy_CMAQcellsProj")

# For each NDVI file, read, and extract to CMAQ
for (i in NDVIfiles){
  NDVI = raster(i)
  NDVI[NDVI < -2000] <- NA
  Extr <- as.data.frame(extract(NDVI, CMAQpolys, median, na.rm=T))
  Extr$Year = as.numeric(substr(i, 39, 42))
  Extr$Day = as.numeric(substr(i, 43, 45))
  Extr <- cbind.data.frame(Extr, CMAQpolys@data$Input_FID)
  colnames(Extr) <- c("NDVI", "Year", "Jday", "Input_FID")
  if (exists("Outp")){
    Outp <- rbind.data.frame(Outp, Extr)
  } else { Outp = Extr }
}

write.csv(Outp, "/aqua/Jess/Data/CaseStudy_NDVIvals.csv")
