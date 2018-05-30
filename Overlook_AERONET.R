## --------------
## Name: Overlook_AERONET.R
## Program version: R 3.2.3
## Dependencies:
## Author: J.H. Belle
## Purpose: Go back through AERONET datasets and tabulate the number of observations prior to checking for valid collocations on MODIS end
## --------------

## Read in AERONET files and compile
AllAero <- read.csv("H:/Rotation_Yang/AERONETCollocs04_550Ints.csv", stringsAsFactors = F)
AllAero$Year = 4
for (yr in seq(5,13)){
  temp <- read.csv(sprintf("H:/Rotation_Yang/AERONETCollocs%02d_550Ints.csv", yr))
  temp$Year = yr
  AllAero <- rbind.data.frame(AllAero, temp)
  rm(temp)
}
gc()

str(AllAero)
AllAero <- subset(AllAero, !is.na(AllAero$AOD550nm))
AllAero$Dragon <- ifelse(substr(AllAero$Location, 1, 6) == "DRAGON", T, F)
checkUnique <- unique(AllAero$Location)
summary(ifelse(substr(checkUnique, 1,6)=="DRAGON", T,F))
summary(AllAero$Dragon)
# Bring in region definitions
East_locs <- read.csv("H:/Rotation_Yang/EastWestSplit/EastLocs100Long.csv")[,3]
East <- rep(1, length(East_locs))
East <- cbind.data.frame(East_locs, East)
AllAero <- merge(AllAero, East, by.x="Location", by.y="East_locs", all.x=T)
AllAero$East <- ifelse(is.na(AllAero$East), 0,1)
summary(as.factor(AllAero$East))
str(unique(subset(AllAero$Location, AllAero$East==0)))
str(unique(subset(AllAero$Location, AllAero$East==1)))

summary(as.factor(subset(AllCollocs$East, AllCollocs$QACcode==3)))
summary(as.factor(subset(AllCollocs$NLCDMode, AllCollocs$QACcode==3)))
summary(subset(AllCollocs, AllCollocs$QACcode==3))
summary(subset(AllCollocs, AllCollocs$QACcode==3 & AllCollocs$NLCDMode==82))

# Get duration of sampling period at each site
AllAero$Date = as.Date(sprintf("%02d - %03d", AllAero$Year, AllAero$Day), "%y - %j")
LocsDurs <- aggregate(Date~Location + Dragon, AllAero, min)
LocsDurs2 <- aggregate(Date~Location, AllAero, max)
LocsDurs <- merge(LocsDurs, LocsDurs2, by="Location")
LocsDurs$Duration = as.numeric(LocsDurs$Date.y - LocsDurs$Date.x, units="days")/30.43
aggregate(Duration~Dragon, LocsDurs, summary)
LocsDurs$DurLess6mon <- ifelse(LocsDurs$Duration < 6, 1, 0)
aggregate(DurLess6mon~Dragon, LocsDurs, summary)
LocsDurs$DurLess1yr <- ifelse(LocsDurs$Duration < 12, 1, 0)
aggregate(DurLess1yr~Dragon, LocsDurs, summary)
