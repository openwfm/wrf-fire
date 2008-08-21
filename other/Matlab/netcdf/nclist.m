function var=nclist(filename)
% var=nclist(filename)
% get a listing of all variables in netcdf file

[ncid,status] = mexnc('OPEN',filename,'nowrite');
nccheck(status)
[ndims,nvars, ngatts, unlimdim, status] = mexnc('INQ',ncid); % global info
nccheck(status)
for varid=nvars-1:-1:0, % one variable at a time
    var(varid+1)=ncvarinfo(ncid,varid);
end
end
