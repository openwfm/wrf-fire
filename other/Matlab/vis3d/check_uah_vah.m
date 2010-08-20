% function check_uah_vah

%filename='wrfrst_d01_0001-01-01_00:01:00'; % wrfrst, not wrfout
%filename,height

% to validate computation of uah,vah

% run wrf with fire_print_file=1
% start matlab in em_fire
% cd to the run directory
% run this script 

height=20*0.3048;                           % 6ft

id=10003

if(id==0),
    p=nc2struct(filename,{'U_2','V_2','PH_2','PHB','HGT','Z0','UAH','VAH',...
    'XLONG','XLAT'},{'DX','DY'},1);
    z0=p.z0; u_2=p.u_2; v_2=p.v_2; ph_2= p.ph_2; phb=p.phb; uah=p.uah; vah=p.vah;
else
    z0=read_array_m('z0',id);
    u_2=read_array_m('u_2',id);
    v_2=read_array_m('v_2',id);
    ph_2=read_array_m('ph_2',id);
    s=size(ph_2);k=s(3);
    u_2=u_2(:,:,1:k-1,:); % vertically staggered
    v_2=v_2(:,:,1:k-1,:);
    phb=read_array_m('phb',id);
    uah=read_array_m('uah',id);
    vah=read_array_m('vah',id);
end

altw=(ph_2+phb)/9.81; % geopotential altitude at w-points

[uahm,vahm]=uah_vah(z0,u_2,v_2,altw,height);

err_uah=big(uah-uahm)
err_uah=big(vah-vahm)

% end
