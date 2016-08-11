%harvest data script

%load information about the grid
load grid_nfo.mat

%fprintf(fileID,
%create report file
%fileID = fopen('report.txt','w');


%load information about the grid
load grid_nfo.mat

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

    