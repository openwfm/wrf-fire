disp('Set NDWI in wrfinput for testing')
f='wrfinput_d01'
ndwi=ncread(f,'NDWI');
ndwi=0.5*ones(size(ndwi));
ncreplace(f,'NDWI',ndwi)
