function p=ncread2struct(filename,varnames,gattnames,p)
% p=ncread2str(filename,vars,gatts)
% read from netcdf file to structure
%
% arguments
% input
%   filename        string, name of file to read
%   varnames        cell array of strings, names of variable to read
%   gattnames       cell array of strings, names of global attributes to read
%   p               (optional) the structure to add the fields to
% output
%   p               matlab structure with the specifed variables and attributes as fields
%                   NOTE: the values types are kept and dimensions are not squeezed
%
% example
%   p=ncread2struct('wrfinput_d01',{'U','V'},{'DX','DY'})
% will read variables U,V into p.U, p.V and global attributes DX DY into
% p.DX p.DY, respectively


for i=1:length({varnames{:}}),
    varname=varnames{i};
    v=ncvar(filename,varname);
    p.(varname)=v.var_value;
end

for i=1:length({gattnames{:}}),
    gattname=gattnames{i};
    v=ncgetgatt(filename,gattname);
    p.(gattname)=v;
end

end