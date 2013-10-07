function v=ncvarinfo(ncid,varid)
% v=ncvarinfo(ncid,varid)
% get info on variable number varid in file f
% returns a structure with fields containing the 
% variable propertices and attributes

% Jan Mandel, September 2008
% developed from an earlier code by Jon Beezley


[v.varname,v.vartype,v.dimids,v.natts]=netcdf.inqVar(ncid,varid);
v.ndims=length(v.dimids);
% translate variable type
[v.vartype_nc,v.vartype_m]=ncdatatype(v.vartype);
% get dimensions
for idim=v.ndims:-1:1 
	dimid=v.dimids(idim);
	[dimname,dimlength]=netcdf.inqDim(ncid,dimid);
        v.dimname{idim}=dimname;
    if isempty(regexp(dimname,'_subgrid$','ONCE'))
        v.dimlength(idim)=dimlength;
    else  % fix fire grid variables
        v.dimlength(idim)=0;  % default
        stagname=[regexprep(dimname,'_subgrid$',''),'_stag'];
        try
            stagid=netcdf.inqDimID(ncid,stagname);
            [tmp,staglen]=netcdf.inqDim(ncid,stagid);
        catch STAG
            % warning(['dimension ',stagname,' not found'])
            staglen=0;
        end
        atmname=dimname(1:end-8);
        try
            atmid=netcdf.inqDimID(ncid,atmname);
            [tmp,atmlen]=netcdf.inqDim(ncid,atmid);
        catch ATM
            % warning(['dimension ',atmname,' not found'])
            atmlen=0;
        end
        if atmlen & staglen,
            if atmlen +1 ~= staglen,
                error(['inconsistent',atmname,' and ',stagname])
            end
        end
        if atmlen==0 & staglen==0,
            warning(['dimensions ',stagname,' or ',atmname,' not found, cannot fix fire variable size'])
            v.dimlength(idim)=dimlength;
        else
            ratio=dimlength/max(staglen,atmlen+1);
            v.dimlength(idim)=dimlength-ratio;
        end
    end
end

% get attributes
v.att_name=cell(1,v.natts);
v.att_datatype=zeros(1,v.natts);
v.att_datatype_m=cell(1,v.natts);
v.att_len=zeros(1,v.natts);
v.att_value=cell(1,v.natts);
for iatt=v.natts:-1:1
	attname=netcdf.inqAttName(ncid,varid,iatt-1);
        [datatype,attlen]=netcdf.inqAtt(ncid,varid,attname);
        [att_type_nc,att_type_m]=ncdatatype(datatype); % TEXT or DOUBLE
	att_value=netcdf.getAtt(ncid,varid,attname);
        v.att_name{iatt}=attname;
	v.att_datatype(iatt)=datatype;
	v.att_datatype_m{iatt}=att_type_m;
	v.att_len(iatt)=attlen;
	v.att_value{iatt}=att_value;
end
end
