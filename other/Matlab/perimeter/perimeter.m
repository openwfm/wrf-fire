       
function result=perimeter(long,lat,time_now,bound,V)

% Description of the function

% This function creates the initial matrix of times of ignitions
% given the perimeter and tign at the points of the perimeter and
% is using Wind variable in its calculations
%
%--------------------------------------------------------------------%
% Algorythm
%--------------------------------------------------------------------%
% 1) mark the points that are on the perimeter on the flag matrix
% 2) Go over the points and check if any of 9 around is =0
% 3) If yes, calculate


%  Comments for myself

% 1 ---This function works if the original perimeter is defined on the grid
% points;
% 2) So far I will do the loop until all the flags would be marked
% 2a) Think how to go over only unmarked flags (create a matrix of indexes and updated every time)
%
% 3) So far the coordinates of the matrix are integer indexes, later
% coefficients of the matrix
% 
% 3a) Think what to do if the points of the boundary don't lie on the grid
%
% 4) Will the tign of the points inside be less than the perimeter points
%
% 5) Updating everything but the boundary points
%
% 6) Why max and not a sum over points with weight
%
% 7) if my update (March 8) is correct than we dont set it to inf but just keep the previous tign
% tign(IN()>0)=inf, <0 = 0 thats for the initialization 
% then tign=min(tign, inf) ot max(0,tign)
% 
% 8) When R=0, and it is obstacle

% Tasks

% 1) Correct my writing of the code
% 2) Do the boundary
% 3) Fedir&Oshkiv the case when the points of the perimeter are not on the
%    mesh
% 4) Rewrite the code for longtitude and latitude and add their reading
% 5) Write the function that reads R as a separate function
% 6) reshape to long and lat

% Reading the data

addpath('../../other/Matlab/util1_jan');
addpath('../../other/Matlab/netcdf');

format long
result=1;

bnd_size=size(bound);
n=size(long,1);
m=size(long,2);

tign=zeros(n,m);      % "time of ignition matrix" of the nodes 
A=zeros(n,m);         % A=1 where time of ignition was updated at least once 

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

% First step, we set everything inside to time_now and update the points
% outside

% Initializing flag matrix A and time of ignition (tign)
% Extending the boundaries, in order to speed up the algorythm
A=zeros(n+2,m+2);
tign=zeros(n+2,m+2);
A(2:n+1,2:m+1)=IN(:,:,1);
tign(2:n+1,2:m+1)=IN(:,:,1)*time_now;

% Stop when the matrix converges
changed=1;
for istep=1:max(size(tign)),
    if changed==0, 
        % Writing the data to the file data_out.txt
%fid = fopen('data_out_tign_fstep.txt', 'w');
%dlmwrite('data_out_tign_out_fstep.txt', tign, 'delimiter', '\t','precision', '%.4f');
%fclose(fid);
'first part done'

%write_array_2d('data_out_wrf_tign.txt',tign)
        break
    end
        
    
tign_last=tign;

% tign_update - updates the tign of the points
tign=tign_update(long,lat,tign,A,IN,V,time_now);

changed=sum(tign(:)~=tign_last(:));
fprintf('step %i tign changed at %i points\n',istep,changed)
fid = fopen('data_out_steps.txt', 'w');
fprintf(fid,'step %i tign changed at %i points\n',istep,changed); % It has two rows now.
fclose(fid);
figure(1),mesh(tign_last(2:n+1,2:m+1)),title('tign last (outside)')
figure(2),mesh(tign-tign_last),title('Difference (outside)')
figure(3),mesh(tign(2:n+1,2:m+1)),title('tign new (outside)')

drawnow

end

% Second step, we keep the values of the points outside and update the
% points inside

% Initializing flag matrix A and time of ignition (tign)
A(2:n+1,2:m+1)=1-IN(:,:,1);
tign(2:n+1,2:m+1)=(1-IN(:,:,1)).*tign(2:n+1,2:m+1);

changed=1;
for istep=1:max(size(tign)),
    if changed==0, 
        break
    fid = fopen('data_out_tign_fstep.txt', 'w');
    dlmwrite('data_out_tign_out_fstep.txt', tign(2:n+1,2:m+1), 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
    'printed'
   
        result=0;

    end
    
tign_last=tign;

% tign_update - updates the tign of the points
tign=tign_update(long,lat,tign,A,IN,V,time_now);

changed=sum(tign(:)~=tign_last(:));
fprintf('step %i tign changed at %i points\n',istep,changed)
% Writing the data to the file data_out.txt
fid = fopen('data_out_steps.txt', 'w');
fprintf(fid,'step %i tign changed at %i points\n',istep,changed); % It has two rows now.
fclose(fid);

figure(4),mesh(tign_last(2:n+1,2:m+1)),title('tign last')
figure(5),mesh(tign-tign_last),title('Difference')
figure(6),mesh(tign(2:n+1,2:m+1)),title('tign new')

drawnow
result=0;
end

if changed~=0,
    warning('did not find fixed point')
end

end

function R=R_wind(x) % Calculates ros (ideal case)

a0=1;
alpha=1;
b0=0.5;
x=max(x,zeros(size(x)));
        
R=a0*x^(alpha)+b0;

end

function tign=tign_update(long,lat,tign,A,IN,V,time_now)
  
% tign  array same size as A and V: time of ignition at all points
% A,IN flags from above
% V - wind; the first dim = x y coord
% time_now = time of ignition on the perimeter
n=size(tign,1);
m=size(tign,2);
for i=2:n-1
    for j=2:m-1
        
        % [a1,a2,b1,b2]=point_location(i,j,n,m);
        % Needed to know what is the amount of points that surrounds (i,j)
        sum_A=sum(sum(A(i-1:i+1,j-1:j+1)));
        % sum_A>0 if tign available at at least one neighbor
        
        if (sum_A~=0)
            if (IN(i-1,j-1)>0) 
                % Points are inside the perimeter
                tign_old=(A(i,j)==1)*tign(i,j);  % previous tign if exists at this point
                    
                for a=i-1:i+1   
                    for b=j-1:j+1  
                    	% loop over all neighbors
                        if (A(a,b)==1) % was already updated 
                        	tign_new=tign(a,b)-sqrt((long(a-1,b-1,1)-long(i-1,j-1,1))^2+   ...
                                     (lat(a-1,b-1,1)-lat(i-1,j-1,1))^2)/                   ...
                                     R_wind((long(a-1,b-1,1)-long(i-1,j-1,1))*V(1,i-1,j-1)+  ... 
                                     (lat(a-1,b-1,1)-lat(i-1,j-1,1))*V(2,i-1,j-1));
                            % update of the tign based on tign and ros
                            % of the neighbour
         
                            if (tign_old<tign_new)&&(tign_new<=time_now);
                            	% Looking for the max tign, which
                                % should be <= than time_now, since the
                                % point is inside of the preimeter
                                tign_old=tign_new;
                                A(i,j)=1;
                            end
                        end
                    end
                end
            
                tign(i,j)=tign_old;
            
            else
                % Points are outside of the perimeter
                % Previous tign if exists at this point, 
                % if not then inf
                if (A(i,j)==1)
                    tign_old=tign(i,j);
                else
                    tign_old=inf;
                end
                    
                for a=i-1:i+1  
                	for b=j-1:j+1  
                    	if (A(a,b)==1)                                
                        	tign_new=tign(a,b)+sqrt((long(a-1,b-1,1)-long(i-1,j-1,1))^2+    ...
                                     (lat(a-1,b-1,1)-lat(i-1,j-1,1))^2)/                    ...
                                     R_wind((long(i-1,j-1,1)-long(a-1,b-1,1))*V(1,a-1,b-1)+ ... 
                                     (lat(i-1,j-1,1)-lat(a-1,b-1,1))*V(2,a-1,b-1));
                            % Here the direction of the vector is
                            % opposite, since fire is going from the
                            % inside point towards the point that was
                            % already computed
                            if (tign_old>tign_new)&&(tign_new>=time_now);
                            	tign_old=tign_new;
                            	A(i,j)=1;
                            end
                        end
                    end
                end
                
                tign(i,j)=tign_old;
          
            end
        end
    end
end
end

% function [a1,a2,b1,b2]=point_location(i,j,n,m);
% % Depending on the location of the point, gives the bounds for the loop
% if (i==1) a1=i; a2=i+1;
%     
% elseif (i==n) a1=i-1; a2=i;
% 
% else a1=i-1; a2=i+1;
% 
% end
% 
% if (j==1) b1=j; b2=j+1;
%     
% elseif (j==m) b1=j-1; b2=j;
% 
% else b1=j-1; b2=j+1;
% 
% end
% 
% end




% Previous versions of the code

%%%%%% (1) %%%%%%
% tign(i,j)=min([ ...
% (A(i-1,j-1)==0)*2*time_now+c,  ...
% (A(i-1,j)==0)*2*time_now+tign(i-1,j)+1/R_wind(dot([0,-1],V(:,i-1,j))),                      ...
% (A(i-1,j+1)==0)*2*time_now+tign(i-1,j+1)+sqrt(2)/R_wind(dot([-1,-1],V(:,i-1,j+1))/sqrt(2)), ...
% (A(i,j-1)==0)*2*time_now+tign(i,j-1)+1/R_wind(dot([1,0],V(:,i,j-1))),                       ...
% (A(i,j+1)==0)*2*time_now+tign(i,j+1)+1/R_wind(dot([-1,0],V(:,i,j+1))),                      ...
% (A(i+1,j-1)==0)*2*time_now+tign(i+1,j-1)+sqrt(2)/R_wind(dot([1,1],V(:,i+1,j-1))/sqrt(2)),   ...
% (A(i+1,j)==0)*2*time_now+tign(i+1,j)+1/R_wind(dot([0,1],V(:,i+1,j))),                       ...
% (A(i+1,j+1)==0)*2*time_now+tign(i+1,j+1)+sqrt(2)/R_wind(dot([-1,1],V(:,i+
% 1,j+1))/sqrt(2))]);
%if (tign(i,j)>1.5*time_now) % think about this later
%                    display('tign(i,j)>time_now');
%                    i
%                    j
%                end
% if (tign(i,j)>time_now+dt)||(tign(i,j)<time_now)
%                     tign(i,j)=0;
%                 else
%                     A(i,j)=1;
%                 end
%%%%%% (1) %%%%%%

%%%%%% (2) %%%%%%
% tign(i,j)=max([ ...
% (A(i-1,j-1)==1)*(tign(i-1,j-1)+sqrt(2)/R_wind(dot([1,-1],V(:,i-1,j-1))/sqrt(2))),  ...
% (A(i-1,j)==1)*(tign(i-1,j)+1/R_wind(dot([0,-1],V(:,i-1,j)))),                      ...
% (A(i-1,j+1)==1)*(tign(i-1,j+1)+sqrt(2)/R_wind(dot([-1,-1],V(:,i-1,j+1))/sqrt(2))), ...
% (A(i,j-1)==1)*(tign(i,j-1)+1/R_wind(dot([1,0],V(:,i,j-1)))),                       ...
% (A(i,j+1)==1)*(tign(i,j+1)+1/R_wind(dot([-1,0],V(:,i,j+1)))),                      ...
% (A(i+1,j-1)==1)*(tign(i+1,j-1)+sqrt(2)/R_wind(dot([1,1],V(:,i+1,j-1))/sqrt(2))),   ...
% (A(i+1,j)==1)*(tign(i+1,j)+1/R_wind(dot([0,1],V(:,i+1,j)))),                       ...
% (A(i+1,j+1)==1)*(tign(i+1,j+1)+sqrt(2)/R_wind(dot([-1,1],V(:,i+1,j+1))/sqrt(2)))]);
%                 
%                 if (tign(i,j)==0) % think about this later
%                     display('tign(i,j)==0');
%                     i
%                     j
%                 end
%                 
%                 if (tign(i,j)<time_now-dt)||(tign(i,j)>time_now)
%                     tign(i,j)=0;
%                 else
%                     A(i,j)=1;
%                 end
%%%%%% (2) %%%%%%                     

%%%%%% (3) %%%%%%
%     for i=2:mesh_size(1)-1
%         for j=2:mesh_size(2)-1
%           
%             if (B(i,j)<2)
%             sum=A(i-1,j-1)+A(i-1,j)+A(i-1,j+1)+A(i,j-1)+ ...
%                 A(i,j+1)+A(i+1,j-1)+A(i+1,j)+A(i+1,j+1);  
%             if (sum~=0)
%                  if (IN(i,j)>0)
%                  % Do all of it as a separate function
%                  tign_old=(A(i,j)==0)*0+(A(i,j)==1)*tign(i,j);  % if my update is correct than we dont set it to inf but just keep the previous tign
%                      for a=i-1:i+1  % tign(IN()>0)=inf, <0 = 0 thats for the initialization 
%                          for b=j-1:j+1  % then tign=min(tign, inf) ot max(0,tign)
%                              if (A(a,b)==1) % add this if Jan likes my idea &&(tign(a,b)~=inf) 
%                                  if (i==5) &&(j==6)
%                                          c=1;
%                                       end
%                                  tign_new=tign(a,b)+sqrt((a-i)^2+(b-j)^2)/R_wind(dot([b-j,a-i],V(:,a,b))); % I stoppped here
%                                  if (tign_old<tign_new)&&(tign_new<=time_now);
%                                      tign_old=tign_new;
%                                  end
%                              end
%                          end
%                      end
%                         tign(i,j)=tign_old;
%                         A(i,j)=1;
%                                  
% %%%%%% (1) %%%%%%
%                 
%                 
%                  else
%                      tign_old=(A(i,j)==0)*inf+(A(i,j)==1)*tign(i,j);
%                         for a=i-1:i+1  % tign(IN()>0)=inf, <0 = 0 thats for the initialization 
%                             for b=j-1:j+1  % then tign=min(tign, inf) ot max(0,tign)
%                                 if (A(a,b)==1) % add this if Jan likes my idea &&(tign(a,b)~=0)  
%                                       if (i==2) &&(j==9)
%                                          c=1;
%                                       end
%                                     tign_new=tign(a,b)+sqrt((a-i)^2+(b-j)^2)/R_wind(dot([b-j,a-i],V(:,a,b)));
%                                     if (tign_old>tign_new)&&(tign_new>=time_now);
%                                         tign_old=tign_new;
%                                     end
%                                 end
%                             end
%                         end
%                         tign(i,j)=tign_old;
%                         A(i,j)=1;
% 
%                         
% %%%%%% (2) %%%%%%                        
%                     end
%                 
%             end        
%             end
%             
%        end
% 
%     end

%%%%%% (3) %%%%%%

















 
