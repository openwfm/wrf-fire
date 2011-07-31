function vprofile(p,x1,x2,tstep)
% vprofile(filename,x1,x2,tstep)
% compute and plot vertical profiles at given location and time
% input
% p             string, WRF file name, or structure p where the file was read using the command
%               p=wrfatm2struct(filename,tstep);       
% x1            distance in m from lower left end of domain in coordinate 1
% x2            distance in m from lower left end of domain in coordinate 2
% tstep         the index in input files to compute this at, or empty
%
% examples:
%             vprofile('wrfinput_d01',100,100,[])
%             vprofile('wrfout_d01_0001-01-01_00:00:00',100,100,1)

% the data for this time step
if ~isstruct(p),
    p=wrfatm2struct(filename,tstep);  
end
% horizontal coordinates in terms of mesh index, cell centered 
i1=x1/p.dx-0.5;
i2=x2/p.dy-0.5;
fi1=x1/p.fdx-0.5;
fi2=x2/p.fdy-0.5;
% interpolate the vertical profiles
altitude=interp_12(p.altitude,i1,i2);

disp(['Time is ',p.times{1}])

% wind profile
u=interp_12(p.u,i1+0.5,i2);  % staggered in x1
v=interp_12(p.v,i1,i2+0.5);  % staggered in x2
[direction,speed]=cart2pol(-v,u); % wind from north to south is direction zero  
direction=180*direction/pi;  % convert to degrees

layers=5;
fprintf('Wind profile at (%gm %gm) from lower left corner of domain\n',x1,x2)
z0 = interp2(p.z0, i1, i2);
dz0=z0;  % for display
fprintf('Rougness height from LANDUSE %5.3fm\n',z0);
fz0 = interp2(p.fz0, fi1, fi2);
if fz0 > 0,
fprintf('Roughness height on fire mesh %5.3fm\n',fz0);
  dz0=fz0;
end
fwh = interp2(p.fwh, fi1, fi2);
if fwh > 0,
  fprintf('Wind height fire mesh        %5.3fm\n',fwh);
end
uf = interp2(p.uf, fi1, fi2);
vf = interp2(p.vf, fi1, fi2);
[df,sf]=cart2pol(-vf,uf);
if sf<=0,
  fprintf('No fuel at this location, wind not interpolated.\n') 
end

fprintf('layer altitude     U       V   speed  direction (degrees)\n')  
fprintf('z0   %7.3f %7.3f %7.3f %7.3f\n',dz0,0,0,0)  % at roughness height

if fwh>0,
     fprintf('fwh  %7.3f %7.3f %7.3f %7.3f %7.3f\n',fwh,uf,vf,sf,df)
elseif sf>0,
     fprintf('             %7.3f %7.3f %7.3f %7.3f\n',uf,vf,sf,df)
end
for i=1:layers,
     fprintf('%3i  %7.3f %7.3f %7.3f %7.3f %7.3f\n',i,altitude(i),u(i),v(i),speed(i),direction(i))
end


figure(1)
plot(speed,altitude)
xlabel('speed m/s')
ylabel('alitude m')
title('Wind speed');
figure(2)
plot(direction,altitude)
xlabel('direction degrees')
ylabel('alitude m')
title('Wind direction');
figure(3)
plot(u,altitude)
xlabel('speed m/s')
ylabel('alitude m')
title('Wind U');
figure(4)
plot(v,altitude)
xlabel('speed m/s')
ylabel('alitude m')
title('Wind V');


% no temperature yet
% t=interp2(t,i1,i2);
% q=interp2(qvapor,i1,i2);
    
end 
 



