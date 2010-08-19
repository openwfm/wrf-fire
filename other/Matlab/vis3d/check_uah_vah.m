% check_uah_vah
filename='wrfrst_d01_0001-01-01_00:01:00' % wrfrst, not wrfout
height=6*0.3048                           % 6ft
p=nc2struct(filename,{'U_2','V_2','PH_2','PHB','HGT','Z0','UAH','VAH'},...
    {'DX','DY'},1);
p