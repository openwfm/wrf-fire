function result=perimeter_in(long,lat,ros,time_now,bound,wrfout,interval,count)

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

bnd_size=size(bound);
n=size(long,1);
m=size(long,2);

%tign=zeros(n,m);      % "time of ignition matrix" of the nodes 
%A=zeros(n,m);         % flag matrix of the nodes, A(i,j)=1 if the time of ignition of the 
                      % point (i,j) was updated at least once 

'started' 
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

[delta_tign]=delta_tign_calculation(long,lat,ros);

% Calculates needed variables for rate of fire spread calculation

%%%%%%% First part %%%%%%%
% Set everything inside to time_now and update the tign of the points outside

% Initializing flag matrix A and time of ignition (tign)
% Extending the boundaries, in order to speed up the algorythm
%A=[];
%C=zeros(n+2,m+2);
% A contains coordinates of the points that were updated during the last
% step

IN_ext=(2)*ones(n+2,m+2);
IN_ext(2:n+1,2:m+1)=IN(:,:,1);


% Set all the points outside to time_now and update the points inside

% Initializing flag matrix A and time of ignition (tign)
% Extending the boundaries, in order to speed up the algorythm
A=[];
C=zeros(n+2,m+2);
for i=2:n+1
    for j=2:m+1
        if IN_ext(i,j)==0
            if sum(sum(IN_ext(i-1:i+1,j-1:j+1)))>0
            A=[A;[i,j]];
            end
       end
    end
end

tign_in=zeros(n+2,m+2);
tign_in(2:n+1,2:m+1)=(1-IN(:,:,1)).*time_now;
changed=1;

time_old=time_now;
% The algorithm stops when the matrix converges (tign_old-tign==0) or if the amount of iterations
% % reaches the max(size()) of the mesh
count
interval
count*interval
for istep=1:max(size(tign_in)),
    if changed==0, 
		% The matrix of tign converged
		'printed'
		break
    end
    
    tign_last=tign_in;
    time_toc=toc;
    str= sprintf('%f -- How long does it take to run step %i',time_toc,istep-1);
   
    
    if ((time_old-min(min(tign_in(A(:,1),A(:,2)))))>=(count*interval))&&((time-count)>0)
    'getting new ros'
        time_old
        tign_in(A(1,1),A(1,2))
        min(min(tign_in(A(:,1),A(:,2))))
        time_old=time_old-count*interval
        time=time-count
       ros=read_data_from_wrfout(wrfout,size(long,1),size(long,2),time);
       delta_tign=delta_tign_calculation(long,lat,ros);
    end

    
    % tign_update - updates the tign of the points
    [tign_in,A,C]=tign_update(tign_in,A,IN_ext,delta_tign,time_now,1);
  % when it is outside the last parameter is 0, inside 1  
    changed=sum(tign_in(:)~=tign_last(:));
%    if (changed<=5)
%       for i=1:size(A,1)
%           A(i,:)
%        end
%    end 

    sprintf('%s \n step %i inside tign changed at %i points \n %f -- norm of the difference',str,istep,changed,norm(tign_in-tign_last))
   sprintf('size of A- %i',size(A,1))   
end
final_tign=tign_in;
%final_tign(2:n+1,2:m+1)=(IN(:,:,1)>0).*tign_in(2:n+1,2:m+1)+(IN(:,:,1)==0).*tign(2:n+1,2:m+1);
result=final_tign(2:n+1,2:m+1);

fid = fopen('output_tign.txt', 'w');
    dlmwrite('output_tign.txt', result, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
    
if changed~=0,
    'did not find fixed point inside'
   end
end

function result=perimeter_in_tign(long,lat,ros,time_now,A,tign_g,wrfout,interval,count)

% Volodymyr Kondratenko           July 19 2013	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
n=size(long,1);
m=size(long,2);

'perimeter_in_tign'

[delta_tign]=delta_tign_calculation(long,lat,ros);

C=zeros(n+2,m+2);

tign_in=zeros(n+2,m+2);
tign_in(2:n+1,2:m+1)=(tign_g(:,:)==time_now).*time_now;

changed=1;
time_old=time_now;

count
interval
count*interval
tic
for istep=1:max(size(tign_in)),
    if changed==0, 
		'printed'
		break
    end
    
    tign_last=tign_in;
    time_toc=toc;
    str= sprintf('%f -- How long does it take to run step %i',time_toc,istep-1);
   
    
    if ((time_old-min(min(tign_in(A(:,1),A(:,2)))))>=(count*interval))&&((time-count)>0)
       'getting new ros'
       time_old=time_old-count*interval
       time=time-count
       ros=read_data_from_wrfout(wrfout,size(long,1),size(long,2),time);
       delta_tign=delta_tign_calculation(long,lat,ros);
    end

    [tign_in,A,C]=tign_update(tign_in,A,IN_ext,delta_tign,time_now,1);

    changed=sum(tign_in(:)~=tign_last(:));

   sprintf('%s \n step %i inside tign changed at %i points \n %f -- norm of the difference',str,istep,changed,norm(tign_in-tign_last))
   sprintf('size of A- %i',size(A,1))   
end

final_tign=tign_in;
result=final_tign(2:n+1,2:m+1);

fid = fopen('output_tign.txt', 'w');
    dlmwrite('output_tign.txt', result, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
    
if changed~=0,
    'did not find fixed point inside'
   end
end




function [tign,A,C]=tign_update(tign,A,delta_tign,time_now,where)
  
% Does one iteration of the algorythm and updates the tign of the points of
% the mesh that are next to the previously updated neighbors
%B=A(end,:);
C=zeros(size(tign,1),size(tign,2));
for i=1:size(A,1)
    for dx=-1:1   
        for dy=-1:1  
                if where*(tign(A(i,1)+dx,A(i,2)+dy)<time_now)==1
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
            
%                 elseif (1-where)*(1-(tign(A(i,1)+dx,A(i,2)+dy)<time_now))==1
%                     tign_new=tign(A(i,1),A(i,2))+0.5*(delta_tign(A(i,1)+dx,A(i,2)+dy,dx+2,dy+2)+delta_tign(A(i,1),A(i,2),2-dx,2-dy));
%                     
%  %
%  % ifs for checking the boundary were here
%  %
%  
%             
%                     if (tign(A(i,1)+dx,A(i,2)+dy)>tign_new)&&(tign_new>=time_now);
%                         % Looking for the min tign, which
%                         % should be >= than time_now, since the
%                         % point is outside of the preimeter
%                    %     if (B(end,1)~=A(i,1)+dx+1)||(B(end,2)~=A(i,2)+dy)
%                    %     B=[B;[A(i,1)+dx,A(i,2)+dy]];
%                         tign(A(i,1)+dx,A(i,2)+dy)=tign_new;
%                         C(A(i,1)+dx,A(i,2)+dy)=1;
%                    %     end
%                     end
            end
        end
    end
end
A=[];
[A(:,1),A(:,2)]=find(C>0);
end


function delta_tign=delta_tign_calculation(long,lat,ros)
    %Extend the boundaries to speed up the algorithm, the values of the
    %extended boundaries would be set to zeros and are never used in the
    %code
	delta_tign=zeros(size(long,1)+2,size(long,2)+2,3,3);
    
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
                delta_tign(i,j,a+2,b+2)=sqrt((long(i+a,j+b,1)-long(i,j,1))^2+    ...
                          (lat(i+a,j+b,1)-lat(i,j,1))^2)/ros(i,j,a+2,b+2);
            end
        end
 
    end

end
end

