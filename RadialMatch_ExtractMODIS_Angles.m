% -----------------------------
% Name: RadialMatch_Extract10km_2011.m
% Date Created: 6/15/14
% Program version: Matlab R2014a
% Author: J.H. Belle
% Depends: vdist.m (contains vdist function - user contributed)
% Purpose: Extract Modis data values within a 27.5 km radius of the station - following Petrenko et. al. 2012,
% from the 10km aerosol product
%     Note: Also extracting the bounding box coordinates for each pixel -
%     this will make it possible to match up other site parameters (land 
%     use, landscape complexity, and NDVI) while accounting for 
%     changes in pixel area over time
% -----------------------------

% -----------------------
% Change these parameters!!!!
% -----------------------
Collocsfile = '/liu_group/remotesensing1/Jess/Data/MODIScollocs04.csv';
Year = 2004;
FPath = '/liu_group/remotesensing3/MODIS_Jess/ladsweb.nascom.nasa.gov/orders/10km_2004/';
OutpFile = '/liu_group/remotesensing1/Jess/Data/MODISExRadMatchAngles_2004.csv';
% -----------------------


% Import datafile with list of MODIS granules for each site
Collocs11 = fopen(Collocsfile);
formatspec = '%s %f %f %u %s %s %f %f %f %f';
Collocs = textscan(Collocs11, formatspec, 'delimiter', ',', 'treatAsEmpty', 'NA', 'HeaderLines', 1);
% Calculate the total number of granules in this file
NumGrans = length(Collocs{7})*4 - (sum(isnan(Collocs{7})) + sum(isnan(Collocs{8})) + sum(isnan(Collocs{9})) + sum(isnan(Collocs{10})));
Outp = fopen(OutpFile, 'w');
Varnames = {'AeroLoc', 'JulianDate', 'Passtime', 'LatMatch', 'LongMatch', 'DistStation', 'SolarZenith', 'SolarAzimuth', 'SensorZenith', 'SensorAzimuth', 'Scatter'};
formatHeader = '%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s\n';
fprintf(Outp, formatHeader, Varnames{1,:});

RowOut = 1;
for i=1:length(Collocs{1})
    Granshere = 4 - (sum(isnan(Collocs{7}(i))) + sum(isnan(Collocs{8}(i))) + sum(isnan(Collocs{9}(i))) + sum(isnan(Collocs{10}(i))));
    for j=1:Granshere
        AeroLoc = cellstr(Collocs{1}{i});
        JulDay = sprintf('%03d', Collocs{4}(i));
        Grantime = sprintf('%04d', Collocs{6+j}(i));
        filen1 = dir(sprintf('%sMYD04_L2.A%u%s.%s.006.*.hdf', FPath, Year, JulDay, Grantime));
        filen = strcat(FPath, filen1.name);
        if length(filen1) == 1
            fileinfo=hdfinfo(filen, 'eos');
            swathname = fileinfo.Swath.Name();

            Lat=hdfread(filen, swathname, 'Fields', 'Latitude');
            Long=hdfread(filen, swathname, 'Fields', 'Longitude');
            RefLat = ones(size(Lat))*Collocs{2}(i);
            RefLon = ones(size(Long))*Collocs{3}(i);
	    if (min(Lat) > -900) & (min(Long) > -900)
                Dists = arrayfun(@vdist, Lat, Long, RefLat, RefLon);
                Mask = find(Dists/1000 < 27.5);
                NumPix = length(Mask)
                SolarZenith=hdfread(filen, swathname, 'Fields', 'Solar_Zenith');
                SolarAzimuth=hdfread(filen, swathname, 'Fields', 'Solar_Azimuth');
                SensorZenith= hdfread(filen, swathname, 'Fields', 'Sensor_Zenith');
                SensorAzimuth = hdfread(filen, swathname, 'Fields', 'Sensor_Azimuth');
                Scatter = hdfread(filen, swathname, 'Fields', 'Scattering_Angle');
                for k=1:NumPix
		    Index = Mask(k);
                    DistStation = Dists(Index);
                    LongMatchO = Long(Index);
                    LatMatchO = Lat(Index);
                    SolarZenithO = SolarZenith(Index);
                    SolarAzimuthO = SolarAzimuth(Index);
                    SensorZenithO = SensorZenith(Index);
                    SensorAzimuthO = SensorAzimuth(Index);
                    ScatterO = Scatter(Index);
                    rowfile = {AeroLoc{:}, JulDay, Grantime, LatMatchO, LongMatchO, DistStation, SolarZenithO, SolarAzimuthO, SensorZenithO, SensorAzimuthO, ScatterO};
                    rowform = '%s, %s, %s, %f, %f, %f, %f, %f, %f, %f, %f\n';
                    fprintf(Outp, rowform, rowfile{1,:});
                end
	    end
        else
            rowfile = {AeroLoc{:}, JulDay, Grantime, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN};
            rowform = '%s, %s, %s, %f, %f, %f, %f, %f, %f, %f, %f\n';
            fprintf(Outp, rowform, rowfile{1,:});
        end
        RowOut = RowOut + 1;
    end;
end;
fclose(Outp);


