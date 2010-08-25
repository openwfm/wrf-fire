function gattval=ncgetgatt(filename,gattname) 
% get the value of global attribute
quiet=exist('q','var');
% fprintf('ncgetatt: file %s\n',filename);
ncid = netcdf.open(filename,'NC_NOWRITE');
gattval = netcdf.getAtt(ncid,netcdf.getConstant('NC_GLOBAL'),gattname);
disp(['global attribute ',gattname,'=',num2str(gattval)])
netcdf.close(ncid);
end
