function cycles
base=datenum('2013-08-11 00:00:00');
num_cycles=5;
spinup_time=ones(1,num_cycles);
cycle_length=ones(1,num_cycles);
cycle_start =[0,2,3,4,5,6]
spinup_time =[2,1,1,1,1,1]
for i=1:num_cycles
    t(i).forecast_time=cycle_start(i+1)+1;
    t(i).obs_start=cycle_start(i);
    t(i).obs_end=cycle_start(i+1)-1e-6;
    t(i).replay_start=cycle_start(i);
    t(i).replay_end=cycle_start(i+1);
    t(i).run_end=cycle_start(i+1)+2;
    print_times(i)
    wrfout{i}=['wrfout_d01_',datestr(base+t(i).forecast_time,'yyyy-mm-dd_HH:MM:SS')];
    wrfrst{i}=['wrfrst_d01_',datestr(base+t(i).replay_start,'yyyy-mm-dd_HH:MM:SS')];
    t(i).perimeter_time=t(i).replay_end*24*3600;
end
   i=input_num('cycle number',1);
    cycle=i; 
    print_times(i)
    fprintf('%s %s\n','Reading fire arrival time from ',wrfout{i})
    if t(i).replay_start==0;
       rewrite='wrfinput_d01';
       restart='.false.';
    else
       rewrite=wrfrst{i};
       restart='.true.';
    end
    fprintf('%s %s\n','Will write modified time into     ',rewrite)
    w=read_wrfout_tign(wrfout{i});
    time_bounds=[t(i).obs_start,t(i).obs_end,t(i).replay_start,t(i).replay_end]+base;
    savew=sprintf('w_%i',cycle);
    fprintf('saving to %s\n',savew)
    save(savew,'w','cycle','time_bounds','t')
    p=detect_fit_level2(i,time_bounds,w)
    print_times(i)
    fprintf('perimeter_time=%10.3f\nrestart=%s\n',t(i).perimeter_time,restart)
    
    command=sprintf('rm -f namelist.input; ln -s namelist.input_%i namelist.input',i);
    q=sprintf('replace TIGN_G in %s and run\n %s\n [0/1]',rewrite,command);
    if input_num(q,1)
        ncreplace(rewrite,'TIGN_G',p.spinup)
        if system(command),
             error('command failed')
        end
    end
    input('Run WRF-SFIRE and continue when done\n')

function print_times(ii)
ptime(ii,'Forecast until    ',t(ii).forecast_time)  
ptime(ii,'Observations start',t(ii).obs_start)
ptime(ii,'Observations end  ',t(ii).obs_end)
ptime(ii,'Replay start      ',t(ii).replay_start)
ptime(ii,'Replay end        ',t(ii).replay_end)
ptime(ii,'Run end           ',t(ii).run_end)
end
function ptime(ii,s,t)
        fprintf('Cycle %i %s%7.3f days %s\n',ii,s,t,datestr(t+base,'dd-mmm-yyyy HH:MM:SS'))
end
	
end
