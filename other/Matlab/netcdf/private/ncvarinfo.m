function v=ncvarinfo(ncid,varid)
% v=ncvarinfo(ncid,varid)
% get info on variable number varid in file f
% returns a structure with fields

[v.varname,v.vartype,v.ndims,v.dimids,v.natts,status] = mexnc('INQ_VAR',ncid,varid);
nccheck(status)
for idim=1:v.ndims
	dimid=v.dimids(idim);
	[dimname,dimlength,status] = mexnc('INQ_DIM',ncid,dimid);	
	nccheck(status)
	v.dimname{idim}=dimname;
	v.dimlength(idim)=dimlength;
end
end
