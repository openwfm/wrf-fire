% go.m
% uses code from dddas cvs server : wf1-2006a/fire1_vvk/Matlab
% to produce a plot of the tracer output from wrffire
%
% requires the modified version of ncdump that accepts the -w
% flag.  the source for this can be found in other/netcdf_write_matrix
% or on wf, you can just copy /home/jbeezley/bin/ncdump somewhere
% to your PATH.

% this likely will only work on linux!!! if you use any other 
% OS you are on your own.

% you may need to modify these variables:

% absolute path to ncdump binary, 'ncdump' will work if it is in PATH
ncdump_binary='ncdump';

% base netcdf file name, will create plots for all files that
% match $(netcdf_basename)*
% can include path/to/file if necessary
netcdf_basename='wrfrst';

% set this variable to 1 to stop between plots to examine
% them manually
pause_after_plot=0;

% the directory that the figures will be saved to
figs_dir='figs';

%%%%%%%%% begin code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath('../util1_jan');

os_test=1;
[os_test,output]=unix('true');
if os_test ~= 0 
    fprintf('this script only works in a *NIX OS\n')
    return
end
    
unix(sprintf('mkdir -p %s',figs_dir));
netcdf_files=dir(sprintf('%s*',netcdf_basename));

for i=1:length(netcdf_files)
    outdir=sprintf('vars_%s',netcdf_files(i).name);
    file=sprintf('%s',netcdf_files(i).name);

    % output the write_matrix files
    [s,output]=unix(sprintf('./ncdump_wrapper.sh ''%s'' ''%s'' ''%s''',...
                        ncdump_binary,file,outdir));
    if s ~= 0,
        fprintf('ncdump_wrapper failed with:\n%s',output);
        return
    end
                    
    xfg=read_m(sprintf('%s/XFG',outdir));
    yfg=read_m(sprintf('%s/YFG',outdir));
    xcd=read_m(sprintf('%s/XCD',outdir));
    ycd=read_m(sprintf('%s/YCD',outdir));
    
    fire_plot_cd(xfg,yfg,xcd,ycd);
    
    saveas(gcf,sprintf('%s/%s.fig',figs_dir,file));
    saveas(gcf,sprintf('%s/%s.png',figs_dir,file));
    
    if(pause_after_plot ~=0)
        pause;
    end
    
end
return