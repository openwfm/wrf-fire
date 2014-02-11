function e=read_namelist_fire_emissions(file)
% e=read_namelist_emissions(file)
% read namelist.fire_emissions and return structure with one field per
% species with array emission factors for each fuel category
%
if ~exist('file','var'),
    file='namelist.fire_emissions';
end
command = ['grep "[a-zA-Z0-9]=" ',file,...
    ' | sed ''s/=/=[/'' | sed ''s/$/];/'' | grep -v ''^\!'' | grep -v printsums | sed ''s/^/e./'' | grep -v compatible'];
[status,result]=system(command);
if status,
    disp(command)
    error('shell command failed')
end
eval(result)  % now all the lines in namelist.fire_emissions are variables length number of fuel categories
% add zero at the end
f=fields(e);
for i=1:length(f)
    species=f{i};
    ncat(i)=length(e.(species));
    e.(species)(ncat(i)+1)=0;
end
if any(ncat~=ncat(1)),
    error('all lines must have the same number of fuel categories');
end
