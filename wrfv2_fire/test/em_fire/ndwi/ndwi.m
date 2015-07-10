disp('Set NDWI in wrfinput for testing')
f='wrfinput_d01'
w=ncread(f,'NDWI');
w=0.5*ones(size(w));
ncreplace(f,'NDWI',w)
