function B=ignition(unit_long,unit_lat,long,lat,time_now,mesh_size,ign_pnt,bound)

% Volodymyr Kondratenko           April 3 2012

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The function Ignition showes how the fire was propagating,
%           given ignition point, boundary region and time_now
% Input: We get it after reading the data using function read_file_ignition.m 
%        unit_long = UNIT_FXLONG;
%        unit_lat = UNIT_FXLAT;
%        long = FXLONG, longtitude coordinates of the mesh
%        lat = FXLAT, latitude coordinates of the mesh 
%        time_now = time of ignition on the boundary
%        mesh_size = size of the mesh
%        ign_pnt - point of ignition
%        bound - set of ordered points of the boundary 1st=last 
%        bound(i,1)-horisontal; bound(i,1)-vertical coordinate
%
% Output:   Matrix of time of ignition
%
%
% Code:

addpath('../../other/Matlab/util1_jan');
addpath('../../other/Matlab/netcdf');

format long

mat_size=size(long);
grid_1=mat_size(1);
grid_2=mat_size(2);

bnd_size=size(bound);

B=zeros(mesh_size);        % "time of ignition matrix" of the nodes 
                           % Ignition point corresponds to B()=0

%  IN - matrix that shows, whether the point is inside (IN(x,y)>0) the burning region
%  or outside (IN(x,y)<0)
%  ON - matrix that, shows whether the point is on the boundary or not
%  Both matrices evaluated using "polygon" 
xv=bound(:,1);
yv=bound(:,2);
xv=xv*100000;
yv=yv*100000;
lat1=lat*100000;
long1=long*100000;
[IN,ON] = inpolygon(long1,lat1,xv,yv);
 
% Calculation of the matrix of distances
% || ( (coord_xf(i,j) - x)*unit_xf , (coord_yf(i,j) - y)*unit_yf ) ||
% unit_fxlat=pi2/(360.*reradius)
% unit_fxlong=cos(lat_ctr*pi2/360.)*unit_fxlat
% lat_ctr=config_flags%cen_lat
%reradius,    & ! 1/earth radiusw
%                 pi2   ! 2*pi   

%for i=1:grid_1
%     for j=1:grid_2
%          C(i,j)=sqrt((unit_long(1,1,1)*(long(i)-ign_pnt(1)))^2+(unit_lat(1,1,1)*(lat(i)-ign_pnt(2)))^2);
%     end
%end
 
%--------------------------------------------------------------------%
% Algorythm
%--------------------------------------------------------------------%

% For each point in the burning area, we 
%     a) Build a line through this point and ignition point
%     b) Find two(or one) points between which, line intersects boundary area,
%        should be 2 variants
%     This is done by checking the F(x,y)=(y-y1)*(x2-x1)-(x-x1)*(y2-y1)
%     Needed points are that satisfy 2 conditions:
%      1) they are sequential;
%      2) F(x1,y1)*F(x2,y2)<0 
%     (In ase with one point, F(x,y)=0) 
%     c) Find out which side is right (from 2 possible choices of points)
%        By similiar way 
%     d) Compute tign of the point by using Thales's theorem
%     (it is not the same as on Wikipedia, see the link below, page 4)
%        http://legacy.lclark.edu/~istavrov/geo-minithales-07.pdf   
% 
 
aa=0;
eps=0.1;
for i=1:grid_1     
    for j=1:grid_2
        if (ON(i,j)>0)          % The point we check is on the boundary
            B(i,j)=time_now;
            k=-1;
        elseif IN(i,j)>0        % The point is inside the burning region
            a_old=line_sign(ign_pnt(1),ign_pnt(2),long(i,j,1),lat(i,j,1),bound(1,1),bound(1,2));
            k=2;
            while (k>0)&&(k<=bnd_size(1))
                if (a_old==0)
                    a_new=a_old;
                    k=1;
                else
                    a_new=line_sign(ign_pnt(1),ign_pnt(2),long(i,j,1),lat(i,j,1),bound(k,1),bound(k,2));
                end
                if a_old*a_new<0  
                    % Check if the point is on the line between 
                    % the ignition point and 2 boundary points
                    a1=line_sign(long(i,j,1),lat(i,j,1),bound(k,1),bound(k,2),ign_pnt(1),ign_pnt(2));
                    a2=line_sign(long(i,j,1),lat(i,j,1),bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2));
                    if a1*a2<0
                     
                        dist1=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),ign_pnt(1),ign_pnt(2));
                        dist2=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),long(i,j,1),lat(i,j,1));
                        B(i,j)=time_now*(dist1-dist2)/dist1;
                        k=-1;
                    end
                elseif a_new==0
                    % Case if the line goes exactly through the boundary point                                           
                    % Check if the point lies between ignition point and boundary point
                    b1=sqrt((long(i,j,1)-ign_pnt(1))^2+(lat(i,j,1)-ign_pnt(2))^2);
                    b2=sqrt((long(i,j,1)-bound(k,1))^2+(lat(i,j,1)-bound(k,2))^2);
                    b3=sqrt((bound(k,1)-ign_pnt(1))^2+(bound(k,2)-ign_pnt(2))^2);
                    if (b1+b2<b3+eps)&&(b1+b2>b3-eps)
                        B(i,j)=time_now*b1/b3; 
                        k=-1;
                    else
                        a_new=line_sign(ign_pnt(1),ign_pnt(2),long(i,j,1),lat(i,j,1),bound(k+1,1),bound(k+1,2));
                        k=k+1;
                    end
                end
                a_old=a_new;   
                k=k+1;       
            end
        else  
            
           % point is outside the burning region
           % B(i,j)=time_now+1;
        a_old=line_sign(ign_pnt(1),ign_pnt(2),long(i,j,1),lat(i,j,1),bound(1,1),bound(1,2));
            k=2;
            while (k>0)&&(k<=bnd_size(1))
                if (a_old==0)
                    a_new=a_old;
                    k=1;
                else
                    a_new=line_sign(ign_pnt(1),ign_pnt(2),long(i,j,1),lat(i,j,1),bound(k,1),bound(k,2));
                end
                if a_old*a_new<0  
                    % The point and second boundary point should be on the 
                    % same side from the line going between ignition point
                    % and first boundary point
                    a1=line_sign(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),ign_pnt(1),ign_pnt(2));
                    a2=line_sign(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),long(i,j,1),lat(i,j,1));
                    if a1*a2<0
                        dist1=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),ign_pnt(1),ign_pnt(2));
                        b1=sqrt((long(i,j,1)-ign_pnt(1))^2+(lat(i,j,1)-ign_pnt(2))^2);
                        %   dist2=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),long(i,j,1),lat(i,j,1));
                        B(i,j)=time_now*b1/dist1;
                       
                        k=-1;
                    end
                elseif a_new==0
                    % Case if the line goes exactly through the boundary point                                           
                    % Check if the boundary point lies between ignition point and the point
                    b1=sqrt((long(i,j,1)-ign_pnt(1))^2+(lat(i,j,1)-ign_pnt(2))^2);
                    b2=sqrt((long(i,j,1)-bound(k,1))^2+(lat(i,j,1)-bound(k,2))^2);
                    b3=sqrt((bound(k,1)-ign_pnt(1))^2+(bound(k,2)-ign_pnt(2))^2);
                    if (b2+b3<b1+eps)&&(b2+b3>b1-eps)
                        B(i,j)=time_now*b1/b3; 
                        k=-1;
                    else
                        a_new=line_sign(ign_pnt(1),ign_pnt(2),long(i,j,1),lat(i,j,1),bound(k+1,1),bound(k+1,2));
                        k=k+1;
                    end
                end
                a_old=a_new;   
                k=k+1;       
          end
        
        end                
    end
end

% check if ign_pnt(1), ign_pnt(2) - integers, 
% than set B(ignition point)=0
if (rem(ign_pnt(1),1)==0) && (rem(ign_pnt(2),1)==0)
    B(ign_pnt(1),ign_pnt(1))=0;
end    

% Writing the data to the file data_out.txt
fid = fopen('data_out.txt', 'w');
dlmwrite('data_out.txt', B, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

write_array_2d('data_out1.txt',B)


% Plot the results
% questio: if ignition point coordinates are real and not on the mesh, than
% plot will not print them
%size(B)
%grid_1
%grid_2
%x=1:1:grid_2;
%y=1:1:grid_1;
%figure(3)
%surf(x,y,B)
%B
 
