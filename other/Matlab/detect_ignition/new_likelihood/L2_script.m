function [] = L2_script()

prefix = '/bigdisk/james.haley/wrfcycling/wrf-fire/wrfv2_fire/test/TIFs/';

pl2 = sort_rsac_files(prefix);
if pl2.file{1}(end) == 't'
    fprintf('TIF files selected \n')
    return
end

num_files = length(pl2.file);
fprintf('Level 2 data selected \n')
fprintf('%d files in the data \n',num_files)

if mod(num_files,2) ~= 0
    fprintf('Warning, odd number of files \n')
end

sets = num_files/2;


for i=1:num_files-1
    if mod(i,2) == 1
        set_num = (i+1)/2;
        file1 = pl2.file{i};
        file2 = pl2.file{i+1};
        fprintf('Set %d %s %s \n', set_num, file1(1:25), file2(1:25))
        v(set_num) = readl2data(prefix,file1,file2);
    end
end

pause

% example
% long = h5read('VNP03MODLL.A2013222.0848.001.2017280021857.h5','/HDFEOS/SWATHS/VNP_750M_GEOLOCATION/Geolocation Fields/Longitude');
% lat  = h5read('VNP03MODLL.A2013222.0848.001.2017280021857.h5','/HDFEOS/SWATHS/VNP_750M_GEOLOCATION/Geolocation Fields/Latitude');
% mask = h5read('VNP14.A2013222.0848.001.2017280021837.nc','/fire mask')


end % function
