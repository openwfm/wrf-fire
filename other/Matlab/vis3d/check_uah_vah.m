% check_uah_vah

filename='wrfrst_d01_0001-01-01_00:01:00'; % wrfrst, not wrfout
height=6*0.3048;                           % 6ft
filename,height

p=nc2struct(filename,{'U_2','V_2','PH_2','PHB','HGT','Z0','UAH','VAH'},...
    {'DX','DY'},1);

p.alt_at_w=(p.ph_2+p.phb)/9.81; % geopotential altitude at w-points
altitude=(p.alt_at_w(:,:,1:end-1,:)+p.alt_at_w(:,:,2:end,:))*0.5; % interpolate to center altitude

% altitudes of the location at cell bottoms under u and v points 
[alt_bu,alt_bv]=interp_w2buv(p.alt_at_w);

% roughness of the ground under the u and v points
[z0_bu,z0_bv]=interp_w2buv(p.z0);

% log interpolation of the wind at u and v points to height
uah=log_interp_vert(p.u_2,alt_bu,z0_bu,height);
vah=log_interp_vert(p.v_2,alt_bv,z0_bv,height);

err_uah=big(uah-p.uah)
err_vah=big(vah-p.vah)


% clear alt_bu alt_bv z0_bu z0_bv % free some memory
