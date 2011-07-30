function vprofile(filename,x1,x2,tstep)
% vprofile(filename,loc,tstep)
% compute and plot vertical profiles at given location and time
% input
% filename      string, WRF file name
% x1            distance in m from lower left end of domain in coordinate 1
% x2            distance in m from lower left end of domain in coordinate 2
% tstep         the index in input files to compute this at, or empty
%
% examples:
%             vprofile('wrfinput_d01',100,100,[])
%             vprofile('wrfout_d01_0001-01-01_00:00:00',100,100,1)

% the data for this time step
p=wrfatm2struct(filename,tstep);  
% horizontal coordinates in terms of mesh index, cell centered 
i1=x1/p.dx-0.5;
i2=x2/p.dy-0.5;
% interpolate the vertical profiles
altitude=interp_12(p.altitude,i1,i2);

disp(['Time is ',p.times{1}])

% wind profile
u=interp_12(p.u,i1+0.5,i2);  % staggered in x1
v=interp_12(p.v,i1,i2+0.5);  % staggered in x2
[direction,speed]=cart2pol(-v,u); % wind from north to south is direction zero  
direction=180*direction/pi;  % convert to degrees
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

function r=interp_12(a,i1,i2)
% horizontal interpolation
    if isempty(a),
        r=[];
    else
        nv=size(a,3); % number of vertical layers
        r=zeros(nv,1);
        for i=1:nv
            b=squeeze(double(a(:,:,i)));
            r(i)=interp2(b,i1,i2);
        end
    end
end
        



