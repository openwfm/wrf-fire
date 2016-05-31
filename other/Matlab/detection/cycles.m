base=datenum('2013-08-11 00:00:00');
num_cycles=5;
spinup_time=0.5*ones(1,num_cycles);
spinup_time(1)=23/24;
heads={'Observations from              ',...
       'Observations to                ',...
       'Next cycle restart at          ',...
       'Next cycle replay until        ',...
       'Next cycle end time            '};
start_cycle=input_num('starting cycle',1)
for i=start_cycle:num_cycles
    start=base+i-1;
    restart=start+1-spinup_time(i);
    time_bounds(i,:)=[start,start+1-4e-6,restart,start+1,start+3];
    for j=1:5,
        fprintf('Cycle %i %s %s\n',i,heads{j},datestr(time_bounds(i,j),'dd-mmm-yyyy HH:MM:SS'))
    end
    wrfout{i}=['wrfout_d01_',datestr(start+2,'yyyy-mm-dd_HH:MM:SS')];
    wrfrst{i}=['wrfrst_d01_',datestr(restart,'yyyy-mm-dd_HH:MM:SS')];
    perimeter_time(i)=(time_bounds(i,4)-base)*24*3600;
    fprintf('perimeter_time=%10.3f\n',perimeter_time(i))
end
for i=start_cycle:num_cycles
    %savefile(wrfout{i})
    w=read_wrfout_tign(wrfout{i});
    % start, end observations; restart time, perimeter time
    fprintf('%s %s\n','Reading fire arrival time from ',wrfout{i})
    fprintf('%s %s\n','Writing modified time into     ',wrfrst{i})
    for j=1:4,
        fprintf('Cycle %i %s %s\n',i,heads{j},datestr(time_bounds(i,j),'dd-mmm-yyyy HH:MM:SS'))
    end
    fprintf('perimeter_time=%10.3f\n',perimeter_time(i))
    p=detect_fit_level2(1,time_bounds(i,:),w)
    %savefile(wrfrst{i})
    for j=3:5,
        fprintf('Cycle %i %s %s\n',i,heads{j},datestr(time_bounds(i,j),'dd-mmm-yyyy HH:MM:SS'))
    end
    fprintf('perimeter_time=%10.3f\n',perimeter_time(i))
    command=sprintf('rm -f namelist.input; ln -s namelist.input_%i namelist.input',i);
    q=sprintf('replace TIGN_G in %s and run\n %s 0/1',wrfrst{i},command);
    y=input_num(q,1);
    if y
        ncreplace(wrfrst{i},'TIGN_G',p.spinup)
        if system(command),
            warning('command failed')
        end
    end
    input('Run WRF-SFIRE and continue when done\n')
end
