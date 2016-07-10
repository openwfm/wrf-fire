function cycles
base_datestr='2013-08-11 00:00:00';
base=datenum(base_datestr);
num_cycles=5;
spinup_time=ones(1,num_cycles);
cycle_length=ones(1,num_cycles);
cycle_start =[0,2,3,4,5,6]
spinup_time =[2,1,1,1,1,1]
times_format='yyyy-mm-dd_HH:MM:SS';
for i=1:num_cycles
    t(i).forecast_time=cycle_start(i+1)+1;
    t(i).obs_start=cycle_start(i);
    t(i).obs_end=cycle_start(i+1)-1e-6;
    t(i).replay_start=cycle_start(i);
    t(i).replay_end=cycle_start(i+1);
    t(i).run_end=cycle_start(i+1)+2;
    t(i).perimeter_time=t(i).replay_end*24*3600;
    print_times(i)
    forecast_times{i}=datestr(base+t(i).forecast_time,times_format);
    wrfout_time = base+t(i).forecast_time;
    if i>1,
        wrfout_time = wrfout_time - 23.5/24;  % no wrfout produced on restart => written 30 min later
    end
    wrfout{i}=['wrfout_d01_',datestr(wrfout_time,times_format)];
    wrfrst{i}=['wrfrst_d01_',datestr(base+t(i).replay_start,times_format)];
end
print_times_table

i=input_num('cycle number',1);
link_namelist_command=sprintf('rm -f namelist.input; ln -s namelist.input_%i namelist.input',i);
if i==0,
    fprintf('Initial simulation from %s to at least %s\n',base_datestr,forecast_times{1})
    fprintf('Execute %s now?\n',link_namelist_command);
    q=sprintf('0/1');
    if input_num(q,1)
        if system(link_namelist_command),
             error('failed')
        end
    end
    input('Run WRF-SFIRE and continue when done\n')
    
else
    print_times(i)
    fprintf('%s %s %s %s\n','Reading fire arrival time at',forecast_times{i},' from ',wrfout{i})
    if t(i).replay_start==0;
       rewrite='wrfinput_d01';
       restart='.false.';
    else
       rewrite=wrfrst{i};
       restart='.true.';
    end
    fprintf('%s %s\n','Will write modified time into     ',rewrite)
    w=read_wrfout_tign(wrfout{i},forecast_times{i});
    time_bounds=[t(i).obs_start,t(i).obs_end,t(i).replay_start,t(i).replay_end]+base;
    savew=sprintf('w_%i',i);
    fprintf('saving to %s\n',savew)
    cycle=i;
    save(savew,'w','cycle','time_bounds','t')
    disp_bounds=[];
    p=detect_fit_level2(i,time_bounds,disp_bounds,w)
    print_times(i)
    fprintf('perimeter_time=%10.3f\nrestart=%s\n',t(i).perimeter_time,restart)
    q=sprintf('replace TIGN_G in %s and run\n %s\n [0/1]',rewrite,link_namelist_command);
    if input_num(q,1)
        ncreplace(rewrite,'TIGN_G',p.spinup)
        if system(link_namelist_command),
             error('link failed')
        end
    end
    input('Run WRF-SFIRE and continue when done\n')
end

function print_times(ii)
ptime(ii,'Forecast used     ',t(ii).forecast_time)  
ptime(ii,'Observations start',t(ii).obs_start)
ptime(ii,'Observations end  ',t(ii).obs_end)
ptime(ii,'Replay start      ',t(ii).replay_start)
ptime(ii,'Replay end        ',t(ii).replay_end)
ptime(ii,'Run end           ',t(ii).run_end)
fprintf('perimeter_time=%10.3f\n',t(ii).perimeter_time)
end

function print_times_table
fmt='  %g %g %g %g %g\n';
fprintf(['Cycle             ',fmt],1:length(t))
fprintf(['Forecast used     ',fmt],t(:).forecast_time)  
fprintf(['Observations start',fmt],t(:).obs_start)
fprintf(['Observations end  ',fmt],t(:).obs_end)
fprintf(['Replay start      ',fmt],t(:).replay_start)
fprintf(['Replay end        ',fmt],t(:).replay_end)
fprintf(['Run end           ',fmt],t(:).run_end)
end

function ptime(ii,s,t)
        fprintf('Cycle %i %s%7.3f days %s\n',ii,s,t,datestr(t+base,'dd-mmm-yyyy HH:MM:SS'))
end
	
end
