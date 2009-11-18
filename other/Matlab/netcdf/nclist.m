function var=nclist(filename,q) 
% get info on all variables
% 14/11 1st file by vk

quiet=exist('q','var');
fprintf('ncdump: file %s\n',filename);
ncid = netcdf.open(filename,'NC_NOWRITE');
% [ncid,status] = mexnc('OPEN',filename,'nowrite');
%nccheck(status)
% [ndims,nvars, ngatts, unlimdim, status] = mexnc('INQ',ncid); % global info
% no 'status' in new version
[ndims,nvars,ngatts,unlimdim] = netcdf.inq(ncid);
%nccheck(status)
for varid=1:nvars, % one variable at a time
    var(varid)=ncvarinfo(ncid,varid-1);
    if ~quiet,
        fprintf('%i ',varid);
        dispvarinfo(var(varid));
    end
end
%status=mexnc('CLOSE',ncid);
netcdf.close(ncid);
%nccheck(status)
end
