function p=nc2struct(filename,varnames,gattnames,times,p)
% p=ncread2str(filename,varnames,g       p.(lower(varname))=[];attnames,times)
% read from netcdf file to structure
%
% arguments
% input
%   filename        string, name of file to read
%   varnames        cell array of strings, names of variable to read
%   gattnames       cell array of strings, names of global attributes to read
%   times           (optional) matrix of indices in the last dimension to extract
%   p               (optional) the structure to add to
% output
%   p               matlab structure with the specifed variables and attributes as fields
%                   with names in lowercase.  The types are kept and the dimensions 
%                   are not squeezed
%
% example
%   p=ncread2struct('wrfinput_d01',{'U','V'},{'DX','DY'})
% will read variables U,V into p.u, p.v and global attributes DX DY into
% p.dx p.dy, respectively

disp(['nc2struct: reading from file ',filename])
for i=1:length({varnames{:}}),
    varname=varnames{i};
    try
        v=ncvar(filename,varname);
    catch ME
        warning(['variable ',varname,' does not exist in file ',filename])
        v.var_value=[];
    end
    if ~isempty(times) && ~isempty(v.var_value),
        dims=length(v.dimlength);
        switch dims
            case 2
                val=v.var_value(:,times);
            case 3
                val=v.var_value(:,:,times);
            case 4
                val=v.var_value(:,:,:,times);
            otherwise
                warning('unsupported number of dimensions')
                val=v.var_value;
        end % case
    else
        val=v.var_value; 
    end
    p.(lower(varname))=double(val);
end

for i=1:length({gattnames{:}}),
    gattname=gattnames{i};
    try
        val=ncgetgatt(filename,gattname);
    catch ME
        warning(['global attribute ',gattname,' does not exist in file ',filename])
        v=[];
    end
    p.(lower(gattname))=double(val);
end

end
