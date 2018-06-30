function w=read_wrfout_tign(f,ts)
    % w=read_wrfout_tign(f) 
    % w=read_wrfout_tign(f,timestep) 
    % read variables from wrfout f for detect_fit_level2 and cycles
    % in
    %    f file name
    %    ts time step (string, optional). Read last time step if not given.
    % 
    t=nc2struct(f,{'Times'},{});  nframes=size(t.times,2);
    times=char(t.times');
    fprintf('Last time step in %s is %i at %s\n',f,nframes,times(nframes,:))
    if exist('ts','var')
        frame=0;
        for i=nframes:-1:1,
            if strcmp(ts,times(i,:))
                fprintf('Found timestep %i at %s\n',i,ts)
                frame=i;
                break
            end
        end
        if(frame==0),
            warning(['Time step ',ts,' not found'])
            w=[];
            return
        end
    else
        frame=nframes;
    end
    w=nc2struct(f,{'Times','TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG','LFN','UF','VF',...
        'XLONG','XLAT','NFUEL_CAT','ITIMESTEP'},{'DX','DY','DT'},frame);
end
