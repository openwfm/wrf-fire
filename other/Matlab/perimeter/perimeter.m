function result=perimeter(long,lat,uf,vf,dzdxf,dzdyf,time_now,bound)

% Volodymyr Kondratenko           December 8 2012	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
% The function creates the initial matrix of times of ignitions
%			given the perimeter and time of ignition at the points of the perimeter and
%			is using wind and terrain gradient in its calculations
% Input: We get it after reading the data using function read_file_perimeter.m 
%        
%        long			FXLONG*UNIT_FXLONG, longtitude coordinates of the mesh converted into meters
%        lat			FXLAT*UNIT_FXLAT, latitude coordinates of the mesh converted into meters 
%        uf,vf			horizontal wind velocity vectors of the points of the mesh 
%        dzdxf,dzdyf	terrain gradient of the points of the mesh
%        time_now		time of ignition on the fireline (fire perimeter)
%        bound			set of ordered points of the fire perimeter 1st=last 
%						bound(i,1)-horisontal; bound(i,1)-vertical coordinate
%
% Output:   Matrix of time of ignition
%
% Code:

%addpath('../../other/Matlab/util1_jan');
%addpath('../../other/Matlab/netcdf');
tic
fuels % This function is needed to create fuel variable, that contains all the characteristics 
      % types of fuel, this function lies in the same folder where you run the main_function.m
	  % (where is it originally located/can be created?)

format long

display('check function')
bnd_size=size(bound);
n=size(long,1);
m=size(long,2);

%tign=zeros(n,m);      % "time of ignition matrix" of the nodes 
%A=zeros(n,m);         % flag matrix of the nodes, A(i,j)=1 if the time of ignition of the 
                      % point (i,j) was updated at least once 

data_steps='started' 
	% string variable, where the status of the code is printed
fid = fopen('data_out_steps.txt', 'w'); 
	% Output file, where data_steps is written, shows the status of the code, while the code is still running

fprintf(fid,'%s',data_steps);  
fclose(fid);
toc
%  IN - matrix, that shows, whether the point is inside (IN(x,y)>0) the burning region
%  or outside (IN(x,y)<0)
%  ON - matrix that, shows whether the point is on the boundary or not
%  Both matrices evaluated using "polygon", coefficients are multiplied by
%  10^6, because the function looses acuracy when it deals with decimals

xv=bound(:,1);
yv=bound(:,2);
xv=xv*100000;
yv=yv*100000;
lat1=lat*100000;
long1=long*100000;
[IN,ON] = inpolygon(long1,lat1,xv,yv);

% Code 

[ichap,bbb,phiwc,betafl,r_0]=fire_ros_new(fuel);
toc
delta_tign=delta_tign_calculation(long,lat,vf,uf,dzdxf,dzdyf,ichap,bbb,phiwc,betafl,r_0);
toc
% Calculates needed variables for rate of fire spread calculation

%%%%%%% First part %%%%%%%
% Set everything inside to time_now and update the tign of the points outside

% Initializing flag matrix A and time of ignition (tign)
% Extending the boundaries, in order to speed up the algorythm
A=[];
C=zeros(n+2,m+2);
% A contains coordinates of the points that were updated during the last
% step
IN_ext=(2)*ones(n+2,m+2);
IN_ext(2:n+1,2:m+1)=IN(:,:,1);

toc
for i=2:n+1
    for j=2:m+1
        if IN_ext(i,j)==1
            if sum(sum(IN_ext(i-1:i+1,j-1:j+1)))<9
                A=[A;[i,j]];
            end
       end
    end
end
toc
tign=ones(n+2,m+2)*1000*time_now;
tign(2:n+1,2:m+1)=IN(:,:,1)*time_now+(1-IN(:,:,1))*1000*time_now;	% and their time of ignition is set to time_now
toc


changed=1;

% The algorithm stops when the matrix converges (tign_old-tign==0) or if
% the amount of iterations
% reaches the max(size()) of the mesh
for istep=1:max(2*size(tign)),
    if changed==0, 
        % The matrix coverged
        data_steps=sprintf('%s\n%s',data_steps,'first part done');
        fid = fopen('output_tign_outside.txt', 'w');
        dlmwrite('output_tign_outside.txt', tign(2:n+1,2:m+1), 'delimiter', '\t','precision', '%.4f');
        fclose(fid);
        'first part done'
        break
    end
        
    
    tign_last=tign;
time_toc=toc;
    data_steps=sprintf('%s\n %f -- How long does it take to run step %i',data_steps,time_toc,istep-1);

    % tign_update - updates the time of ignition of the points
 
[tign,A,C]=tign_update(tign,A,IN_ext,delta_tign,time_now,0);
    % tign_update - updates the time of ignition of the points

    changed=sum(tign(:)~=tign_last(:));

    data_steps=sprintf('%s\n step %i outside tign changed at %i points',data_steps,istep,changed);
    data_steps=sprintf('%s\n %f -- norm of the difference',data_steps,norm(tign-tign_last));

    fid = fopen('data_out_steps.txt', 'w');
    fprintf(fid,'%s',data_steps); 
    fclose(fid);
end

if changed~=0,
   data_steps=sprintf('%s\n%s',data_steps,'did not find fixed point outside');
    warning('did not find fixed point')
end

%%%%%%% Second part %%%%%%%

% Set all the points outside to time_now and update the points inside

% Initializing flag matrix A and time of ignition (tign)
% Extending the boundaries, in order to speed up the algorythm
A=[];
toc
for i=2:n+1
    for j=2:m+1
        if IN_ext(i,j)==0
            if sum(sum(IN_ext(i-1:i+1,j-1:j+1)))>0
         A=[A;[i,j]];
            end
       end
    end
end
toc

tign_in=zeros(n+2,m+2);
tign_in(2:n+1,2:m+1)=(1-IN(:,:,1)).*tign(2:n+1,2:m+1);
changed=1;

% The algorithm stops when the matrix converges (tign_old-tign==0) or if the amount of iterations
% % reaches the max(size()) of the mesh
for istep=1:max(size(tign)),
    if changed==0, 
		% The matrix of tign converged
		fid = fopen('output_tign.txt', 'w');
		dlmwrite('output_tign.txt', tign(2:n+1,2:m+1), 'delimiter', '\t','precision', '%.4f');
		fclose(fid);
		'printed'
		break
    end
    
    tign_last=tign_in;
    time_toc=toc;
    data_steps=sprintf('%s\n %f -- How long does it take to run step %i',data_steps,time_toc,istep-1);

    
    % tign_update - updates the tign of the points
    [tign_in,A,C]=tign_update(tign_in,A,IN_ext,delta_tign,time_now,1);
  % hwen it is outside the last parameter is 0, inside 1  
    changed=sum(tign_in(:)~=tign_last(:));

    data_steps=sprintf('%s\n step %i inside tign changed at %i points',data_steps,istep,changed);
    data_steps=sprintf('%s\n %f -- norm of the difference',data_steps,norm(tign_in-tign_last));

    fid = fopen('data_out_steps.txt', 'w');
    fprintf(fid,'%s',data_steps); 
    fclose(fid);
end
final_tign=zeros(n+2,m+2);
final_tign(2:n+1,2:m+1)=(IN(:,:,1)>0).*tign_in(2:n+1,2:m+1)+(IN(:,:,1)==0).*tign(2:n+1,2:m+1);
result=final_tign(2:n+1,2:m+1);
mesh(result)

fid = fopen('output_tign.txt', 'w');
    dlmwrite('output_tign.txt', final_tign(2:n+1,2:m+1), 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
    
if changed~=0,
    data_steps=sprintf('%s\n%s',data_steps,'did not find fixed point inside');
    warning('did not find fixed point inside')
    
    'printed'
end
end

function [tign,A,C]=tign_update(tign,A,IN,delta_tign,time_now,where)
  
% Does one iteration of the algorythm and updates the tign of the points of
% the mesh that are next to the previously updated neighbors
%B=A(end,:);
C=zeros(size(tign,1),size(tign,2));
for i=1:size(A,1)
    for dx=-1:1   
        for dy=-1:1  
                if where*IN(A(i,1)+dx,A(i,2)+dy)==1
                    tign_new=tign(A(i,1),A(i,2))-0.5*(delta_tign(A(i,1)+dx,A(i,2)+dy,dx+2,dy+2)+delta_tign(A(i,1),A(i,2),2-dx,2-dy));
                    if (tign(A(i,1)+dx,A(i,2)+dy)<tign_new)&&(tign_new<=time_now)
                        % Looking for the max tign, which
                        % should be <= than time_now, since the
                        % point is inside of the preimeter
                   %     if (B(end,1)~=A(i,1)+dx)||(B(end,2)~=A(i,2)+dy)
                   %         B=[B;[A(i,1)+dx,A(i,2)+dy]];
                            tign(A(i,1)+dx,A(i,2)+dy)=tign_new;
                            C(A(i,1)+dx,A(i,2)+dy)=1;
                   %    end
                    end
            
                elseif (1-where)*(1-IN(A(i,1)+dx,A(i,2)+dy))==1
                    tign_new=tign(A(i,1),A(i,2))+0.5*(delta_tign(A(i,1)+dx,A(i,2)+dy,dx+2,dy+2)+delta_tign(A(i,1),A(i,2),2-dx,2-dy));
                    
                  if (A(i,1)+dx==2)&&(A(i,2)+dy==2)
                        display('i=1,j=1')
                        display('tign_new')   
                         tign_new
                        display('tign(A(i,1),A(i,2))')
tign(A(i,1),A(i,2))
display('delta_tign(A(i,1)+dx,A(i,2)+dy,dx+2,dy+2) ')
delta_tign(A(i,1)+dx,A(i,2)+dy,dx+2,dy+2)
display('delta_tign(A(i,1),A(i,2),2-dx,2-dy)')
delta_tign(A(i,1),A(i,2),2-dx,2-dy)
display('tign(A(i,1)+dx,A(i,2)+dy')
tign(A(i,1)+dx,A(i,2)+dy)

display('time_now')
time_now    
end
            
                    if (tign(A(i,1)+dx,A(i,2)+dy)>tign_new)&&(tign_new>=time_now);
                        % Looking for the min tign, which
                        % should be >= than time_now, since the
                        % point is outside of the preimeter
                   %     if (B(end,1)~=A(i,1)+dx+1)||(B(end,2)~=A(i,2)+dy)
                   %     B=[B;[A(i,1)+dx,A(i,2)+dy]];
                        tign(A(i,1)+dx,A(i,2)+dy)=tign_new;
                        C(A(i,1)+dx,A(i,2)+dy)=1;
                   %     end
                    end
            end
        end
    end
end
A=[];
[A(:,1),A(:,2)]=find(C>0);
end

function [ichap,bbb,phiwc,betafl,r_0]=fire_ros_new(fuel,fmc_g)
% ros=fire_ros(fuel,speed,tanphi)
% ros=fire_ros(fuel,speed,tanphi,fmc_g)
% in
%       fuel    fuel description structure
%       speed   wind speed
%       tanphi  slope
%       fmc_g   optional, overrides fuelmc_g from the fuel description
% out
%       ros     rate of spread

% given fuel params

windrf=fuel.windrf;               % WIND REDUCTION FACTOR
fgi=fuel.fgi;                     % INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
fueldepthm=fuel.fueldepthm;       % FUEL DEPTH (M)
savr=fuel.savr;                   % FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
fuelmce=fuel.fuelmce;             % MOISTURE CONTENT OF EXTINCTION
fueldens=fuel.fueldens;           % OVENDRY PARTICLE DENSITY, LB/FT^3
st=fuel.st;                       % FUEL PARTICLE TOTAL MINERAL CONTENT
se=fuel.se;                       % FUEL PARTICLE EFFECTIVE MINERAL CONTENT
weight=fuel.weight;               % WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
fci_d=fuel.fci_d;                 % INITIAL DRY MASS OF CANOPY FUEL
fct=fuel.fct;                     % BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
ichap=fuel.ichap;                 % 1 if chaparral, 0 if not
fci=fuel.fci;                     % INITIAL TOTAL MASS OF CANOPY FUEL
fcbr=fuel.fcbr;                   % FUEL CANOPY BURN RATE (KG/M**2/S)
hfgl=fuel.hfgl;                   % SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)
cmbcnst=fuel.cmbcnst;             % JOULES PER KG OF DRY FUEL
fuelheat=fuel.fuelheat;           % FUEL PARTICLE LOW HEAT CONTENT, BTU/LB
fuelmc_g=fuel.fuelmc_g;           % FUEL PARTICLE (SURFACE) MOISTURE CONTENT, jm: 1 by weight?
fuelmc_c=fuel.fuelmc_c;           % FUEL PARTICLE (CANOPY) MOISTURE CONTENT, 1

if exist('fmc_g','var') % override moisture content by given
    fuelmc_g = fmc_g;
end

% computations from CAWFE code: wf2_janice/fire_startup.m4 

bmst     = fuelmc_g/(1+fuelmc_g);          % jm: 1 
fuelheat = cmbcnst * 4.30e-04;             % convert J/kg to BTU/lb
fci      = (1.+fuelmc_c)*fci_d;
fuelloadm= (1.-bmst) * fgi;                % fuelload without moisture
                                           % jm: 1.-bmst = 1/(1+fuelmc_g) so fgi includes moisture? 
fuelload = fuelloadm * (.3048)^2 * 2.205;  % to lb/ft^2
fueldepth= fueldepthm/0.3048;              % to ft
betafl   = fuelload/(fueldepth * fueldens);% packing ratio  jm: lb/ft^2/(ft * lb*ft^3) = 1
betaop   = 3.348 * savr^(-0.8189);         % optimum packing ratio jm: units??  
qig      = 250. + 1116.*fuelmc_g;          % heat of preignition, btu/lb
epsilon  = exp(-138./savr );               % effective heating number
rhob     = fuelload/fueldepth;             % ovendry bulk density, lb/ft^3
c        = 7.47 * exp(-0.133 * savr^0.55); % const in wind coef
bbb      = 0.02526 * savr^0.54;            % const in wind coef
c        = c * windrf^bbb;                 % jm: wind reduction from 20ft per Baughman&Albini(1980)
e        = 0.715 * exp( -3.59e-4 * savr);  % const in wind coef
phiwc    = c * (betafl/betaop)^(-e); 
rtemp2   = savr^1.5;
gammax   = rtemp2/(495. + 0.0594*rtemp2);  % maximum rxn vel, 1/min
a        = 1./(4.774 * savr^0.1 - 7.27);   % coef for optimum rxn vel
ratio    = betafl/betaop;   
gamma    = gammax*(ratio^a)*exp(a*(1.-ratio)); % optimum rxn vel, 1/min
wn       = fuelload/(1 + st);              % net fuel loading, lb/ft^2
rtemp1   = fuelmc_g/fuelmce;
etam     = 1.-2.59*rtemp1 +5.11*rtemp1^2 -3.52*rtemp1^3;  % moist damp coef
etas     = 0.174* se^(-0.19);              % mineral damping coef
ir       = gamma * wn * fuelheat * etam * etas; % rxn intensity,btu/ft^2 min
irm      = ir * 1055./( 0.3048^2 * 60.) * 1.e-6;% for mw/m^2 (set but not used)
xifr     = exp( (0.792 + 0.681*savr^0.5)...
            * (betafl+0.1)) /(192. + 0.2595*savr); % propagating flux ratio
%        ... r_0 is the spread rate for a fire on flat ground with no wind.
r_0      = ir*xifr/(rhob * epsilon *qig);  % default spread rate in ft/min

% computations from CAWFE code: wf2_janice/fire_ros.m4 


end

function delta_tign=delta_tign_calculation(long,lat,vf,uf,dzdxf,dzdyf,ichap,bbb,phiwc,betafl,r_0)
    %Extend the boundaries to speed up the algorithm, the values of the
    %extended boundaries would be set to zeros and are never used in the
    %code
	result=1;
    'hello'
    delta_tign=zeros(size(long,1)+2,size(long,2)+2,3,3);
    rate_of_spread=zeros(size(long,1)+2,size(long,2)+2,3,3);
    
    long2=zeros(size(long,1)+2,size(long,2)+2);
    long2(2:size(long,1)+1,2:size(long,2)+1)=long;
    long=long2;
    
    lat2=zeros(size(lat,1)+2,size(lat,2)+2);
    lat2(2:size(lat,1)+1,2:size(lat,2)+1)=lat;
    lat=lat2;

    vf2=zeros(size(vf,1)+2,size(vf,2)+2);
    vf2(2:size(vf,1)+1,2:size(vf,2)+1)=vf;
    vf=vf2;

    uf2=zeros(size(uf,1)+2,size(uf,2)+2);
    uf2(2:size(uf,1)+1,2:size(uf,2)+1)=uf;
    uf=uf2;

    dzdxf2=zeros(size(dzdxf,1)+2,size(dzdxf,2)+2);
    dzdxf2(2:size(dzdxf,1)+1,2:size(dzdxf,2)+1)=dzdxf;
    dzdxf=dzdxf2;

    dzdyf2=zeros(size(dzdyf,1)+2,size(dzdyf,2)+2);
    dzdyf2(2:size(dzdyf,1)+1,2:size(dzdyf,2)+1)=dzdyf;
    dzdyf=dzdyf2;

for i=2:size(long,1)-1
    for j=2:size(long,2)-1
        for a=-1:1
            for b=-1:1
                wind=0.5*((long(i,j,1)-long(i+a,j+b,1))*vf(i,j,1)+  ... 
                      (lat(i,j,1)-lat(i+a,j+b,1))*uf(i,j,1));
                angle=0.5*((long(i,j,1)-long(i+a,j+b,1))*dzdxf(i,j,1)+  ... 
                       (lat(i,j,1)-lat(i+a,j+b,1))*dzdyf(i,j,1));
                if ~ichap,
                    %       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
                    spdms = max(wind,0.);
                    umidm = min(spdms,30.);                    % max input wind spd is 30 m/s   !param!
                    umid = umidm * 196.850;                    % m/s to ft/min
                    %  eqn.: phiw = c * umid**bbb * (betafl/betaop)**(-e) ! wind coef
                    phiw = umid^bbb * phiwc;                   % wind coef
                    phis = 5.275 * betafl^(-0.3) *max(0,angle)^2;   % slope factor
                    ros = r_0*(1. + phiw + phis)  * .00508; % spread rate, m/s
                else  % chapparal
                    %        .... spread rate has no dependency on fuel character, only windspeed.
                    spdms = max(wind,0.);
                    ros = max(.03333,1.2974 * spdms^1.41);       % spread rate, m/s
                end
                rate_of_spread(i,j,a+2,b+2)=min(ros,6);
                % DEscribe the coefficient below
                delta_tign(i,j,a+2,b+2)=sqrt((long(i+a,j+b,1)-long(i,j,1))^2+    ...
                          (lat(i+a,j+b,1)-lat(i,j,1))^2)/ros;
            end
        end
 
    end

end
end

function [tign,A]=point_update(i,j,tign,delta_tign,inside)
    
    for dx=-1:+1   
        for dy=-1:+1  
            % loop over all neighbors
        	if (A(i+dx,j+dy)==1) % the neighbor was already updated 
        	% All the vectors are split in half-intervals
        	% to get better calculation
            %%% Make a picture of what is happening %%%
                            
            tign_new=tign(i+dx,j+dy)-delta_tign(i,j,dx+2,dy+2)-delta_tign(i+dx,j+dy,2-dx,2-dy);
                           
                if (tign(i,j)<tign_new)&&(tign_new<=time_now);
                    % Looking for the max tign, which
                    % should be <= than time_now, since the
                    % point is inside of the preimeter
                    tign(i,j)=tign_new;
                    A(i,j)=1;
                end
            end
        end
    end
end

function result=print_matrix(tign,fid)


fprintf(fid,'%s\n','j=1740:1744, i=2750:2754');

for ii = 2750:2754
    fprintf(fid,'%g\t',tign(ii,1740:1744));
    fprintf(fid,'\n');
end
fprintf(fid,'%s\n','i=100');

for ii = 98:102
    fprintf(fid,'%g\t',tign(ii,98:102));
    fprintf(fid,'\n');
end
fprintf(fid,'%s\n','i=1000');

for ii = 998:1002
    fprintf(fid,'%g\t',tign(ii,998:1002));
    fprintf(fid,'\n');
end
result=0;
end

% for i=2:size(tign,1)-1
%     for j=2:size(tign,2)-1
%         % sum_A is needed to know what is the amount of points that surrounds (i,j)
%         sum_A=sum(sum(A(i-1:i+1,j-1:j+1)));
%         
%         if (sum_A~=0)
%             
%             % sum_A>0 then at least on eneighbor was previously updated and its
%             % tign can be used to update the tign of the point (i,j)
%             
%             % I subtract 1 in all IN, long, lat, uf, vf, dzdxf, dzdyf
%             % matrices, because their boundaries were not updated, unlike
%             %%% tign (do the loop 1:n, 1:m and change the indexes in tign) %%%
%             
%                
%             for dx=-1:1   
%                 for dy=-1:1  
%                     % loop over all neighbors
%                     if (A(i+dx,j+dy)==1) % the neighbor was already updated 
%                         % All the vectors are split in half-intervals
%                         % to get better calculation
%                         %%% Make a picture of what is happening %%%
%                         % I do multiplication by 0.5 here 0.5-(IN(i-1,j-1)>0
%                         
%                         tign_new=tign(i+dx,j+dy)+(0.5-(IN(i-1,j-1)>0))*(delta_tign(i,j,dx+2,dy+2)+delta_tign(i+dx,j+dy,2-dx,2-dy));
%                         
%                         if (IN(i-1,j-1)>0)
%                             
%                             if (tign(i,j)<tign_new)&&(tign_new<=time_now)
%                                 % Looking for the max tign, which
%                                 % should be <= than time_now, since the
%                                 % point is inside of the preimeter
%                                 tign(i,j)=tign_new;
%                                 A(i,j)=1;
%                             end
%                         elseif (tign(i,j)>tign_new)&&(tign_new>=time_now);
%                                 % Looking for the min tign, which
%                                 % should be >= than time_now, since the
%                                 % point is outside of the preimeter
%                                 tign(i,j)=tign_new;
%                                 A(i,j)=1;
%                             end 
%                         end
%                     end
%                 end
%             end
%         end
%     end



% Old Code -- tign_update

                            
%                             % wind1 = vect.*(uf,vf)
%                             wind1=0.5*((long(a-1,b-1,1)-long(i-1,j-1, 1))*vf(i-1,j-1,1)+  ... 
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))*uf(i-1,j-1,1));                            
%                             angle1=0.5*((long(a-1,b-1,1)-long(i-1,j-1,1))*dzdxf(i-1,j-1,1)+  ... 
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))*dzdyf(i-1,j-1,1));
%                             
%                             % This is needed to calculate the ros(i,j)     
%                              if ~ichap,
%                                 %       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
%                                 spdms = max(wind1,0.);
%                                 umidm = min(spdms,30.);                    % max input wind spd is 30 m/s   !param!
%                                 umid = umidm * 196.850;                    % m/s to ft/min
%                                 %  eqn.: phiw = c * umid**bbb * (betafl/betaop)**(-e) ! wind coef
%                                 phiw = umid^bbb * phiwc;                   % wind coef
%                                 phis = 5.275 * betafl^(-0.3) *max(0,angle1)^2;   % slope factor
%                                 ros = r_0*(1. + phiw + phis)  * .00508; % spread rate, m/s
%                             else  % chapparal
%                                 %        .... spread rate has no dependency on fuel character, only windspeed.
%                                 spdms = max(wind1,0.);
%                                 ros = max(.03333,1.2974 * spdms^1.41);       % spread rate, m/s
%                             end
%                             ros=min(ros,6);
%                             
%                             % tign_new=tign(a,b)-delta(t);
%                         	tign_new1=tign(a,b)-0.5*sqrt((long(a-1,b-1,1)-long(i-1,j-1,1))^2+   ...
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))^2)/ros;                   ...
%                                     
%                             % Same calculation for the second half of the
%                             % interval
%                                  
%                             wind2=0.5*((long(a-1,b-1,1)-long(i-1,j-1, 1))*vf(a-1,b-1,1)+  ... 
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))*uf(a-1,b-1,1));
%                             angle2=0.5*((long(a-1,b-1,1)-long(i-1,j-1,1))*dzdxf(a-1,b-1,1)+  ... 
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))*dzdyf(a-1,b-1,1));
%                             if ~ichap,
%                                 %       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
%                                 spdms = max(wind2,0.);
%                                 umidm = min(spdms,30.);                    % max input wind spd is 30 m/s   !param!
%                                 umid = umidm * 196.850;                    % m/s to ft/min
%                                 %  eqn.: phiw = c * umid**bbb * (betafl/betaop)**(-e) ! wind coef
%                                 phiw = umid^bbb * phiwc;                   % wind coef
%                                 phis = 5.275 * betafl^(-0.3) *max(0,angle2)^2;   % slope factor
%                                 ros = r_0*(1. + phiw + phis)  * .00508; % spread rate, m/s
%                             else  % chapparal
%                                 %        .... spread rate has no dependency on fuel character, only windspeed.
%                                 spdms = max(wind2,0.);
%                                 ros = max(.03333,1.2974 * spdms^1.41);       % spread rate, m/s
%                             end
%                             ros=min(ros,6);
%                         	tign_new2=-0.5*sqrt((long(a-1,b-1,1)-long(i-1,j-1,1))^2+   ...
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))^2)/ros;
%                             tign_new=tign_new1+tign_new2;



% OUTSIDE

                            
                            
                            
%                             wind1=0.5*((long(i-1,j-1,1)-long(a-1,b-1,1))*vf(i-1,j-1,1)+  ... 
%                                      (lat(i-1,j-1,1)-lat(a-1,b-1,1))*uf(i-1,j-1,1));
%                             angle1=0.5*((long(i-1,j-1,1)-long(a-1,b-1,1))*dzdxf(i-1,j-1,1)+  ... 
%                                      (lat(i-1,j-1,1)-lat(a-1,b-1,1))*dzdyf(i-1,j-1,1));
%                             if ~ichap,
%                                 %       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
%                                 spdms = max(wind1,0.);
%                                 umidm = min(spdms,30.);                    % max input wind spd is 30 m/s   !param!
%                                 umid = umidm * 196.850;                    % m/s to ft/min
%                                 %  eqn.: phiw = c * umid**bbb * (betafl/betaop)**(-e) ! wind coef
%                                 phiw = umid^bbb * phiwc;                   % wind coef
%                                 phis = 5.275 * betafl^(-0.3) *max(0,angle1)^2;   % slope factor
%                                 ros = r_0*(1. + phiw + phis)  * .00508; % spread rate, m/s
%                             else  % chapparal
%                                 %        .... spread rate has no dependency on fuel character, only windspeed.
%                                 spdms = max(wind1,0.);
%                                 ros = max(.03333,1.2974 * spdms^1.41);       % spread rate, m/s
%                             end
%                             ros=min(ros,6);
%                             % tign_new=tign(a,b)-delta(t);
%                             tign_new1=tign(a,b)+0.5*sqrt((long(a-1,b-1,1)-long(i-1,j-1,1))^2+    ...
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))^2)/r
%                                      os;s
%                         
%                             wind2=0.5*((long(i-1,j-1,1)-long(a-1,b-1,1))*vf(a-1,b-1,1)+  ... 
%                                      (lat(i-1,j-1,1)-lat(a-1,b-1,1))*uf(a-1,b-1,1));
%                             angle2=0.5*((long(i-1,j-1,1)-long(a-1,b-1,1))*dzdxf(a-1,b-1,1)+  ... 
%                                      (lat(i-1,j-1,1)-lat(a-1,b-1,1))*dzdyf(a-1,b-1,1));
%                             if ~ichap,
%                                 %       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
%                                 spdms = max(wind2,0.);
%                                 umidm = min(spdms,30.);                    % max input wind spd is 30 m/s   !param!
%                                 umid = umidm * 196.850;                    % m/s to ft/min
%                                 %  eqn.: phiw = c * umid**bbb * (betafl/betaop)**(-e) ! wind coef
%                                 phiw = umid^bbb * phiwc;                   % wind coef
%                                 phis = 5.275 * betafl^(-0.3) *max(0,angle2)^2;   % slope factor
%                                 ros = r_0*(1. + phiw + phis)  * .00508; % spread rate, m/s
%                             else  % chapparal
%                                 %        .... spread rate has no dependency on fuel character, only windspeed.
%                                 spdms = max(wind2,0.);
%                                 ros = max(.03333,1.2974 * spdms^1.41);       % spread rate, m/s
%                             end
%                             ros=min(ros,6);
%                             tign_new2=0.5*sqrt((long(a-1,b-1,1)-long(i-1,j-1,1))^2+    ...
%                                      (lat(a-1,b-1,1)-lat(i-1,j-1,1))^2)/ros;
%                             tign_new=tign_new1+tign_new2;                    ...
%                             % Here the direction of the vector is
%                             % opposite, since fire is going from the
%                             % inside point towards the point that was
%                             % already computed
















 
