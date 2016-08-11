%harvest data script
% run this in the remote wwrf-fire/wrfv2_fire/test/ where 
% the make_grid function did its work 

% grid_nfo = 
%
%           pts: [49x2 double]
%      lat_done: [40.358 40.366 40.374 40.382]   % unneeded
%      lon_done: [-112.66 -112.65 -112.64 -112.63] % unneeded
%    lon_square: [7x7 double]
%    lat_square: [7x7 double]
%         names: {245x1 cell}
%         times: [5x1 double]
%          logs: [245x1 double]

%load information about the grid
load grid_nfo.mat

%start of logging commands - *****unfinished******
%fprintf(fileID,
%create report file
%fileID = fopen('report.txt','w');


fold_spec1 = 'Folder %s does not exist \n';
mkdir_spec1 = 'mkdir ../g_run_data/%s';
name_spec = 'cp namelist.input ../g_run_data/%s/namelist.input';
w_spec = 'cp w.mat ../g_run_data/%s/w.mat'
fuel_spec = 'cp fuels.m ../g_run_data/%s/fuels.m'

%
for i = 1:size(grid_nfo.names)
    if exist(grid_nfo.names{i}) == 7
        %fprintf('Folder exists\n');
        %cd into folder
       cd(grid_nfo.names{i})
       
       %make the w.mat file
       make_w3;
       %make a storage directory
       system(sprintf(mkdir_spec1,grid_nfo.names{i}));
       %copy files into the storage
       system(sprintf(name_spec,grid_nfo.names{i}));
       system(sprintf(w_spec,grid_nfo.names{i}));
       system(sprintf(fuel_spec,grid_nfo.names{i}));
       
       
       %cd back up
       cd('../');
    else
        fold_str1 = sprintf(fold_spec1,grid_nfo.names{i});
        %fprintf(fileID,'%s',fold_str1);
        fprintf('%s \n',fold_str1);
        %fprintf('Folder does not exist. \n');
    end
    
%close text file
%fclose(fileID);
end

    