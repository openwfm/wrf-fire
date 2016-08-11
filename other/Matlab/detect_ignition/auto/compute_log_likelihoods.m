function [ out_struct ] = compute_log_likelihoods( in_struct )
% [ out_struct ] = compute_log_likelihoods( in_struct )
% function takes in a struct with fire data and returns a struct with log
% likelihoods corresponding to fire data

% inputs arguments
% in_struct Matlab structure with the following fields
%     in_struct.pts : nx2 array containing coordinates of ignitions
%     in_struct.dirs : cell array with directories containing w.mat and
%          fuels.m files

% output arguments
% out_struct Matlb structure with the same fields as input as well as 
%     out_struct.logs : nx1 array with log likelihoods corresponding to the
%     ignition points
 
out_struct = in_struct;
cwd = pwd;
[m n] = size(in_struct.names);
logs = zeros(m,1);
% loop over number of ignitions
cd_spec = 'g_run_data/%s';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% test with 3 only !!!!!!!!!!!!!!!!!!
for i = 1:m
    
    %cd(in_struct.dirs{i});
    str = sprintf(cd_spec,in_struct.names{i});
    cd('g_run_data');
    if exist(in_struct.names{i}) == 7
        cd(in_struct.names{i});
        clear w;
        load w.mat;
        %copyfile('w.mat','C:\cygwin\home\paulc\fs\w.mat')
        %copyfile('fuels.m','C:\cygwin\home\paulc\fs\fuels.m')
        logs(i) = detection_log_likelihood('../../TIFs/',w);
        cd(cwd);
    else
        fprintf('folder DNE \n');
        cd(cwd);
    end
    
    %logs(i) = detection_log_likelihood('TIFs/',w)
end
out_struct.logs = logs;
end

