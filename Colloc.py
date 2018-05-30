## --------------------------------
## Filename: Colloc.py
## Date created: 6/1/2014
## Program version: 2.7
## Dependencies: csv, math, datetime
## Author: J.H. Belle
## Purpose: Identify collocated AERONET and MODIS observations over the continental U.S. in 2011
##      Note: This script does not differentiate between valid and non-valid collocations, it simply identifies collocations where level 2 AERONET observations likely exist. Further analysis will need to be done to identify which of these collocations are valid (MODIS data actually exists and >= 2 AERONET obs can be interpolated to 550 nm)
## User-defined functions: CalcBoundBox, ContainsCoord, Addline
## Function file: Functions_colloc.py (all functions moved here after testing)
## --------------------------------

## ------------------
# Input parameters
## ------------------

AeroOut = "H:/Rotation_Yang/AERONETCollocs04.csv"
ModOut = "H:/Rotation_Yang/MODIScollocs04.csv"
Active = "H:/Rotation_Yang/AERONET/Active2011.csv"
Yearint = 2011

# import required modules
import csv, math, datetime

## ---------------------
# User-defined functions
## ---------------------

## ----------
# Function 1: CalcBoundBox
## ----------

def CalcBoundBox(lat, lon):
    """This function takes in a set of latitude/longitude coordinates, and outputs a set of bounding coordinates for a box around a circle with approximately a 25 km radius around the original coordinates"""
    East = lon + ((25./6371.)/math.cos(lat))*(180./math.pi)
    West = lon - ((25./6371.)/math.cos(lat))*(180./math.pi)
    North = lat + (25./6371.)*(180./math.pi)
    South = lat - (25./6371.)*(180./math.pi)
    listall = [East, North, South, West]
    return listall

## ----------
# Function 2: ContainsCoord
## ----------

def ContainsCoord(AeroCoor, GranCoor):
    """This function identifies if the bounding box for an AERONET station (AeroCoor) falls within a MODIS granule (GranCoor) (although it could be used for any situation where you want to know if two boxes overlap or one contains the other) and returns a boolean value. Both AeroCoor and GranCoor are expected to be lists of the form [E, N, S, W]"""
    n = float(GranCoor[2]) < float(AeroCoor[1]) < float(GranCoor[1])
    s = float(GranCoor[2]) < float(AeroCoor[2]) < float(GranCoor[1])
    e = float(GranCoor[3]) < float(AeroCoor[0]) < float(GranCoor[0])
    w = float(GranCoor[3]) < float(AeroCoor[3]) < float(GranCoor[0])
    LocIn = (n or s) and (e or w)
    return LocIn

## ----------
# Function 3: Addline
## ----------

def Addline(time1, time2):
    """Evaluates two timestamps and determines if the first is within +/- 30 minutes of the other. Returns a boolean value. The expectation is that both timestamps are formatted in the datetime format, and time2 is actually a list of the form [min, max]"""
    timerangep = time2[1] + datetime.timedelta(minutes=30) 
    timerangem = time2[0] - datetime.timedelta(minutes=30)
    Inrange = timerangem <= time1 <= timerangep
    return Inrange

## ----------
# Function 4: CalcMean
## ----------
def CalcMean(Timelist):
    """Calculates the mean time of a MODIS overpass when multiple granules are involved. This function expects a list to be input containing up to 4 timestamps in a '%H%M' format, and works by converting to a timestamp in the datetime format, identifies the maximum and miminum values and outputs these values in a list of the form [min, max]"""
    DecTimes = []
    for time in Timelist:
        Hr = int(str(time)[0:2])
        Min = int(str(time)[2:4])
        timestamp = datetime.datetime(1900, 1, 1, Hr, Min)
        DecTimes.append(timestamp)
    Maxtime = max(DecTimes)
    Mintime = min(DecTimes)
    RetTime = [Mintime, Maxtime]
    return RetTime

## --------------------
# Procedural code
## --------------------

# Initialize databases: Database 1 - location to granule matches, Database 2 - AERONET observations for each match
Database1 = {}
Data1csv = open(ModOut, 'wb')
fieldnames1 = ('Location', 'lat', 'long', 'Day', 'Passtime_min', 'Passtime_max', 'Granule1', 'Granule2', 'Granule3', 'Granule4')
Outdata1 = csv.DictWriter(Data1csv, fieldnames=fieldnames1)
header1 = dict( (n,n) for n in fieldnames1)
Outdata1.writerow(header1)
Database2 = {}
Data2csv = open(AeroOut, 'wb')
fieldnames2 = ('Location', 'Day', 'lat', 'long', 'Elevation', 'Timestamp', 'Water_cm', 'AOD_1640', 'AOD_1020', 'AOD_870', 'AOD_675', 'AOD_667', 'AOD_555', 'AOD_551', 'AOD_532', 'AOD_531', 'AOD_500', 'AOD_490', 'AOD_443', 'AOD_440', 'AOD_412', 'AOD_380', 'AOD_340', '440-870Angstrom', '380-500Angstrom', '440-675Angstrom', '500-870Angstrom', '340-440Angstrom')
Outdata2 = csv.DictWriter(Data2csv, fieldnames=fieldnames2)
header2 = dict( (n,n) for n in fieldnames2)
Outdata2.writerow(header2)
# Open list of stations active in 2011 and parse through each active station
with open(Active, 'rb') as csvfile:
    Activelist = csv.reader(csvfile)
    next(Activelist)
    for row in Activelist:
        # Store relevant values from here in databases
        Database1['lat'] = row[2]
        Database2['lat'] = row[2]
        Database1['long'] = row[1]
        Database2['long'] = row[1]
        Database1['Location'] = row[0]
        Database2['Location'] = row[0]
        Database2['Elevation'] = row[3]
        print row[0]
        # Calculate the bounding box that will include these records
        boundbox = CalcBoundBox(float(row[2]), float(row[1]))
        Stationfile = "H:/Rotation_Yang/AERONET/ALL_POINTS/920801_140524_" + row[0] + ".lev20"
        # Set up values for next steps
        linenumS = 0
        JulDaylast = 0
        passtime = [datetime.datetime(1900, 1, 1, 0, 0), datetime.datetime(1900, 1,1,0,0)]
        # Open station file and go through it line by line
        for line in open(Stationfile):
            # The first 4 lines are all header, skip these
            if linenumS > 4:
                # Pull out year and timestamp
                linelist = line.split(',')
                yr = int(linelist[0].split(':')[2])
                Timestamp = datetime.datetime.strptime(linelist[1], '%H:%M:%S')
                # If observation could conceivably lead to a collocation, take a closer look
                if yr == Yearint and Timestamp > datetime.datetime.strptime('15:30', '%H:%M'):
                    JulDay = int(math.floor(float(linelist[2])))
                    # If this observation was recorded on the same day as the previous one and is within +/- 30 min of an overpass, pull it. 
                    if JulDay == JulDaylast:
                        if Addline(Timestamp, passtime) == True:
                            # Compile line for Database 2 and write to csv
                            Database2['Timestamp'] = datetime.datetime.strftime(Timestamp, '%H:%M')
                            Database2['Day'] = JulDay
                            Database2['Water_cm'] = linelist[19]
                            indexaod = 3
                            for aod in [1640, 1020, 870, 675, 667, 555, 551, 532, 531, 500, 490, 443, 440, 412, 380, 340]:
                                Database2['AOD_%i' % aod] = linelist[indexaod]
                                indexaod += 1
			    indexang = 37
			    for var in ['440-870Angstrom', '380-500Angstrom', '440-675Angstrom', '500-870Angstrom', '340-440Angstrom']:
			    	Database2[var] = linelist[indexang]
				indexang += 1
                            Outdata2.writerow(Database2)	
                        JulDaylast = JulDay
                    else:
                        # Pull the Geo-metadata file and load in the matching granule timestamps
                        Date = datetime.datetime.strptime(str(JulDay), '%j').strftime('%m-%d')
                        GeoMetaFile = "H:/Rotation_Yang/GeoMetaData/geoMeta/6/AQUA/" + Yearint + "/MYD03_" + Yearint + "-" + Date + ".txt"
                        ContCoor = []
                        linenumG = 0
                        for granule in open(GeoMetaFile):
                            if linenumG > 2:
                                granlist = granule.split(',')
                                time = int(granlist[0].split('.')[2])
                                if time > 1530:
                                    GranuleBox = [granlist[5], granlist[6], granlist[7], granlist[8]]
                                    if ContainsCoord(boundbox, GranuleBox) == True:
                                        ContCoor.append(time)
                            linenumG += 1
                    
                        #GeoMetaFile.close()
                        passtime = CalcMean(ContCoor)
                        
                        # Write line to database 1
                        Database1['Passtime_min'] = datetime.datetime.strftime(passtime[0], '%H:%M')
                        Database1['Passtime_max'] = datetime.datetime.strftime(passtime[1], '%H:%M')
                        while len(ContCoor) < 4:
                            ContCoor.append('NA')
                        Database1['Granule1'] = ContCoor[0]
                        Database1['Granule2'] = ContCoor[1]
                        Database1['Granule3'] = ContCoor[2]
                        Database1['Granule4'] = ContCoor[3]
                        Database1['Day'] = JulDay
                        Outdata1.writerow(Database1)

                        if Addline(Timestamp, passtime) == True:
                            # Compile line for Database 2 and write to csv
                            Database2['Timestamp'] = datetime.datetime.strftime(Timestamp, '%H:%M')
                            Database2['Day'] = JulDay
                            Database2['Water_cm'] = linelist[19]
                            indexaod = 3
                            for aod in [1640, 1020, 870, 675, 667, 555, 551, 532, 531, 500, 490, 443, 440, 412, 380, 340]:
                                Database2['AOD_%i' % aod] = linelist[indexaod]
                                indexaod += 1
			    indexang = 37
			    for var in ['440-870Angstrom', '380-500Angstrom', '440-675Angstrom', '500-870Angstrom', '340-440Angstrom']:
			    	Database2[var] = linelist[indexang]
				indexang += 1 
                            Outdata2.writerow(Database2)
                        JulDaylast = JulDay
            linenumS += 1
        #Stationfile.close()

