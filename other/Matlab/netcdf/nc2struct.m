function [p,dims]=nc2struct(filename,varnames,gattnames,timestep,p)
% p=ncread2str(filename,varnames,gattnames,timestep,p)
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

fprintf('nc2struct: reading from file %s',filename)

% reading values

if ~exist('timestep','var'),
    t=-1;
    fprintf(' all timesteps\n')
elseif isscalar(timestep) & isnumeric(timestep),
    fprintf(' timestep %i only\n',timestep)
    t=timestep-1; % netcdf dimensions start from 0
else
    error('timestep must be numeric scalar')
end
for i=1:length({varnames{:}}),
    varname=varnames{i};
    try
        v=ncvar(filename,varname,[]);
    catch ME
        warning(['variable ',varname,' does not exist in file ',filename])
        v=[];
    end
    if ~ isempty(v),
        ndims=length(v.dimlength);
        start=zeros(1,ndims);
        count=v.dimlength;
        if t >= 0, % read only one dimestep
            if(v.dimids(ndims)~=0),
                 warning('id of the last dimension is not 0, is it timestep?')
            end
	    start(ndims)=t;
            count(ndims)=1;
        end
        v = ncvar(filename,varname,start, count); 
        p.(lower(varname))=double(v.var_value);
        dims.(lower(varname))=v.dimlength;
    else 
        p.(lower(varname))=[];;
    end
end

% reading attributes

for i=1:length({gattnames{:}}),
    gattname=gattnames{i};
    try
        val=ncgetgatt(filename,gattname);
    catch ME
        warning(['global attribute ',gattname,' does not exist in file ',filename])
        val=[];
    end
    p.(lower(gattname))=double(val);
end

end
