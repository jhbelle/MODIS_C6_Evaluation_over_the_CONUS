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
Pixelsfile = '/aqua/Jess/Data/PBounds.csv';
FPath = '/terra/MODIS_Jess/MODISTCPW/Raw/';
OutpFile = '/aqua/Jess/Data/MODISExCaseStudyTCPW.csv';
% -----------------------


% Import datafile with list of MODIS granules for each site
Pixels = fopen(Pixelsfile);
formatspec = '%f %f %f %f %f %f %f';
Pixels2 = textscan(Pixels, formatspec, 'delimiter', ',', 'treatAsEmpty', 'NA', 'HeaderLines', 1);
Outp = fopen(OutpFile, 'w');
Varnames = {'Input_FID', 'Year', 'JulianDate', 'TCPW'};
formatHeader = '%s, %s, %s, %s\n';
fprintf(Outp, formatHeader, Varnames{1,:});

for i=1:length(Pixels2{1})
    InputFID = Pixels2{1}(i);
    JulDay = sprintf('%03d', Pixels2{7}(i));
    Year = Pixels2{6}(i);
    Lbound = Pixels2{2}(i);
    Rbound = Pixels2{3}(i);
    Lowbound = Pixels2{4}(i);
    Ubound = Pixels2{5}(i);
    Files = dir(sprintf('%sMYD05_L2.A%u%s.*.006.*.hdf', FPath, Year, JulDay));
    for j=1:length(Files)
        filen = strcat(FPath, Files(j).name);
        fileinfo=hdfinfo(filen, 'eos');
        swathname = fileinfo.Swath.Name();
        Lat=hdfread(filen, swathname, 'Fields', 'Latitude');
        Long=hdfread(filen, swathname, 'Fields', 'Longitude');
        Mask = find((Lat < Ubound & Lat > Lowbound & Long > Lbound & Long < Rbound));
        if length(Mask) > 0
            TCPW=hdfread(filen, swathname, 'Fields', 'Water_Vapor_Near_Infrared');
	    TCPWvals = TCPW(Mask)
            TCPW0 = median(TCPWvals(TCPWvals >= 0));
            rowfile = {InputFID, Year, JulDay, TCPW0};
            rowform = '%f, %f, %s, %f\n';
            fprintf(Outp, rowform, rowfile{1,:});
        end;
    end;
end;
fclose(Outp);


