function e=read_namelist_fire_emissions
% e=read_namelist_emissions
% read namelist.fire_emissions and return structure with one field per
% species with array emission factors for each fuel category
%
command = 'grep "[a-zA-Z0-9]=" namelist.fire_emissions | sed "s/=/=[/" | sed "s/$/];/" | grep -v "^!" | grep -v printsums | sed "s/^/e./" | grep -v compatible';
[status,result]=system(command);
if status,
    disp(command)
    error('shell command failed')
end
eval(result)  % now all the lines in namelist.fire_emissions are variables length number of fuel categories
end