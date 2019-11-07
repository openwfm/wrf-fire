function [ out_struct ] = compute_log_likelihoods( in_struct )
% [ out_struct ] = compute_log_likelihoods( in_struct )
% function takes in a struct with fire data and returns a struct with log
% likelihoods corresponding to fire data

% inputs arguments
% in_struct Matlab structure with the following fields
%     in_struct.pts : nx2 array containing coordinates of ignitions
%     in_struct.dirs : n^2x1 cell array with directories containing w.mat and
%          fuels.m files

% output arguments
% out_struct Matlb structure with the same fields as input as well as 
%     out_struct.logs : nx1 array with log likelihoods corresponding to the
%     ignition points
 
out_struct = in_struct;
%needs to run in fs
cwd = pwd
[m n] = size(in_struct.names');
logs = zeros(m,1);
% loop over number of ignitions

for i = 1:m
    % cd folders to get w,fuels files
    cd g_run_data;
    cd(in_struct.dirs{i});
    copyfile('w.mat','C:\cygwin64\home\paulc\fs\w.mat')
    copyfile('fuels.m','C:\cygwin64\home\paulc\fs\fuels.m')
    
    cd(cwd)
    clear w;
    load w.mat
    logs(i) = detection_log_likelihood('TIFs/',w);
end
out_struct.logs = logs;
%save out_struct_new_objective.mat out_struct;
end

