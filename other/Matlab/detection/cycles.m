base=datenum('2013-08-11 00:00:00');
num_cycles=5;
spinup_time=0.5*ones(1,num_cycles);
spinup_time(1)=23/24;
heads={'Observations from              ',...
       'Observations to                ',...
       'Next cycle restart at          ',...
       'Next cycle replay until        ',...
       'Next cycle continue  until     '};
for i=1:num_cycles
    start=base+i-1;
    restart=start+1-spinup_time(i);
    time_bounds(i,:)=[start,start+1-4e-6,restart,start+1,start+3];
    for j=1:5,
        fprintf('Cycle %i %s %s\n',i,heads{j},datestr(time_bounds(i,j),'dd-mmm-yyyy HH:MM:SS'))
    end
    wrfout{i}=['wrfout_',datestr(start+2,'yyyy-mm-dd_HH:MM:SS')];
    wrfrst{i}=['wrfrst_',datestr(restart,'yyyy-mm-dd_HH:MM:SS')];
end
for i=1:num_cycles
    % w=read_wrfout_tign(wrfout{i});
    % start, end observations; restart time, perimeter time
    fprintf('%s %s\n','Reading fire arrival time from ',wrfout{i})
    fprintf('%s %s\n','Writing modified time into     ',wrfrst{i})
    for j=1:4,
        fprintf('Cycle %i %s %s\n',i,heads{j},datestr(time_bounds(i,j),'dd-mmm-yyyy HH:MM:SS'))
    end
    
    p=detect_fit_level2(1,time_bounds(i,:),w)
    ncreplace(wrfrst{i},'TIGN_G',p.spinup)
    for j=3:5,
        fprintf('Cycle %i %s %s\n',i,heads{j},datestr(time_bounds(i,j),'dd-mmm-yyyy HH:MM:SS'))
    end
    command=sprintf('ln -s namelist.input_%i namelist.input',cycle);
    disp(command)
    if system(command),
        warning('command failed')
    end
    input('Run WRF-SFIRE and continue when done\n')
end