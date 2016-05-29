function w=read_wrfout_tign(f)
    % w=read_wrfout_tign(f) 
    % read variables from wrfout f for detect_fit_level2
    t=nc2struct(f,{'Times'},{});  nframes=size(t.times,2)  
    w=nc2struct(f,{'Times','TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG',...
        'XLONG','XLAT','NFUEL_CAT'},{'DX','DY'},nframes);
end