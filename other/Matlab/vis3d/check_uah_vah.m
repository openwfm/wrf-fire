% function check_uah_vah

filename='wrfrst_d01_0001-01-01_00:01:00'; % wrfrst, not wrfout
height=20*0.3048;                           % 6ft
filename,height

p=nc2struct(filename,{'U_2','V_2','PH_2','PHB','HGT','Z0','UAH','VAH',...
    'XLONG','XLAT'},{'DX','DY'},1);

altw=(p.ph_2+p.phb)/9.81; % geopotential altitude at w-points

[uah,vah]=u_v_at_h(p.z0,p.u_2,p.v_2,altw,height);

err_uah=big(uah-p.uah)
err_uah=big(vah-p.vah)

% end