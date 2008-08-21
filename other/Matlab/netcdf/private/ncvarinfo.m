function v=ncvarinfo(ncid,varid)
% v=ncvarinfo(ncid,varid)
% get info on variable number varid in file f
% returns a structure with fields

[v.varname,v.vartype,v.ndims,v.dimids,v.natts,status] = mexnc('INQ_VAR',ncid,varid);
nccheck(status)

% translate variable type
[v.vartype_nc,v.vartype_m]=ncdatatype(v.vartype);
% get dimensions
for idim=v.ndims:-1:1 
	dimid=v.dimids(idim);
	[dimname,dimlength,status] = mexnc('INQ_DIM',ncid,dimid);	
	nccheck(status)
	v.dimname{idim}=dimname;
	v.dimlength(idim)=dimlength;
end

% get attributes
v.att_name=cell(1,v.natts);
v.att_datatype=zeros(1,v.natts);
v.att_datatype_m=cell(1,v.natts);
v.att_len=zeros(1,v.natts);
v.att_value=cell(1,v.natts);
for iatt=v.natts:-1:1
	[attname,status] = mexnc('INQ_ATTNAME',ncid,varid,iatt-1);
	nccheck(status)
	[datatype,attlen,status] = mexnc('INQ_ATT',ncid,varid,attname);
	nccheck(status)
	[att_type_nc,att_type_m]=ncdatatype(datatype); % TEXT or DOUBLE
	[att_value,status] = mexnc(['GET_ATT_',att_type_nc],ncid,varid,attname);
	nccheck(status)
	v.att_name{iatt}=attname;
	v.att_datatype(iatt)=datatype;
	v.att_datatype_m{iatt}=att_type_m;
	v.att_len(iatt)=attlen;
	v.att_value{iatt}=att_value;
end
end
