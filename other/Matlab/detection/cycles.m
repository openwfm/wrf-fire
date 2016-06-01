function cycles
base=datenum('2013-08-11 00:00:00');
num_cycles=5;
spinup_time=ones(1,num_cycles);
cycle_length=ones(1,num_cycles);
cycle_start =[0,2,3,4,5,6]
spinup_time =[2,1,1,1,1,1]
for i=1:num_cycles
    forecast_time(i)=cycle_start(i+1)+1;
    obs_start(i)=cycle_start(i);
    obs_end(i)=cycle_start(i+1)-1e-6;
    replay_start(i)=cycle_start(i);
    replay_end(i)=cycle_start(i+1);
    run_end(i)=cycle_start(i+1)+2;
    print_times(i)
    wrfout{i}=['wrfout_d01_',datestr(base+forecast_time(i),'yyyy-mm-dd_HH:MM:SS')];
    wrfrst{i}=['wrfrst_d01_',datestr(base+replay_start(i),'yyyy-mm-dd_HH:MM:SS')];
    perimeter_time(i)=replay_end(i)*24*3600;
end
   i=input_num('cycle number',1);
    print_times(i)
    fprintf('%s %s\n','Reading fire arrival time from ',wrfout{i})
    if replay_start(i)==0;
       rewrite='wrfinput_d01';
       restart='.false.';
    else
       rewrite=wrfrst{i};
       restart='.true.';
    end
    fprintf('%s %s\n','Will write modified time into     ',rewrite)
    w=read_wrfout_tign(wrfout{i});
    time_bounds=[obs_start(i),obs_end(i),replay_start(i),replay_end(i)]+base;
    p=detect_fit_level2(i,time_bounds,w)
    print_times(i)
    fprintf('perimeter_time=%10.3f\nrestart=%s\n',perimeter_time(i),restart)
    
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
ptime(ii,'Forecast until    ',forecast_time(ii))  
ptime(ii,'Observations start',obs_start(ii))
ptime(ii,'Observations end  ',obs_end(ii))
ptime(ii,'Replay start      ',replay_start(ii))
ptime(ii,'Replay end        ',replay_end(ii))
ptime(ii,'Run end           ',run_end(ii))
end
function ptime(ii,s,t)
        fprintf('Cycle %i %s%7.3f days %s\n',ii,s,t,datestr(t+base,'dd-mmm-yyyy HH:MM:SS'))
end
	
end
