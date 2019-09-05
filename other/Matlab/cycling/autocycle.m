% script automates cycling process
fprintf('autocycle script starting. \n');

%get cycle number
force = 0;
cycle_num = input_num('Enter the cycle number',1,force);

%start the cycle script
fprintf('Starting cycle script for cycle %i \n',cycle_num);
cycles(cycle_num)

% cleanup stage
% make_dir = sprintf('mkdir cycle_%i',cycle_num);
% fprintf('Making new storage directory cycle_%i\n',cycle_num)
% system(make_dir);
% mv_str = sprintf('mv wrfout* *.png *.fig rsl.* cycle_%i/.',cycle_num);
% fprintf('Moving files with: %s \n',mv_str);
% system(mv_str);
% close all

%exit
fprintf('autocycle script completed. \n');





