function tign=perimeter_in(long,lat,fire_area,wrfout,time,interval,time_step,num_wrf)

% Volodymyr Kondratenko           July 19 2013	
% Ideas line 261

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Input:    
%    long,lat      longtitude and latitude converted to meters
%    fire_area     fire map,[0,1] array, where 0- not
%                  burning area, >0 burning area, 1 - area that was burnt
%    wrfout        name of the wrfout file, that is being used to read ros
%    time_now      the perimeter time (s)
%    time          update the wind every count time steps
%    interval      time step in wrfout in seconds
%
% Output: 
%    Final Matrix of times of ignition will be printed to 'output_tign.txt' % JM use save tign instead, create tign.mat

% Algorithm state is stored in arrays A D C
%
% A contains rows [i,j] of indices of nodes not burning that have at least one burning neighbor
%   and time of ignition > time_now 
% Computing 4d array of distances between a point and its 8 neighbors
%ideas
% Change to this later (if it speeds up the algorithm)
               % [row,col]=find(C(A(jj,1)-1:A(jj,1)+1,A(jj,2)-1:A(jj,2)+1)==0);
               % for index=1:size(row,1)
               %   dx=row(index)-2;
               %   dy=col(index)-2;
% Iine 180, maybe I hould do it each step toavoid extra calculations
data_steps='started perimeter_in';

distance=get_distances(long,lat);

clear long
clear lat

tign=get_tign_from_dif_eq(wrfout,fire_area,distance,time,interval,time_step,num_wrf,data_steps);

fid = fopen('output_tign_test.txt', 'w');
    dlmwrite('output_tign_test.txt', tign, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
    
end


function [tign]=get_tign_from_dif_eq(wrfout,fire_area,distance,time,interval,time_step,num_wrf,data_steps)

pnt_a=1000;
pnt_b=1000;    %Point around which I print Big_matrix
myfile = ['data_out_' num2str(pnt_a) '_' num2str(pnt_b) '.txt'];

% Getting matrix A from the initial tign_g, where
[A,C,D,tign]=get_perim_from_initial_tign(fire_area,time,interval,time_step,num_wrf); 
%contour(C);title(sprintf('Original perimeter')); drawnow 
clear fire_area

I=zeros(size(distance));            % Matrix of distances
P=A; %Points on the perimeter, who has at least one neighbor, that was not updated yet.
C_old=C;
ros_old=read_ros_from_wrfout(wrfout{num_wrf},time);
for ts=(time_step*(num_wrf-1)+time):-1:2 % ts -time step

   if (ts<time) % At the first step we initialize A and D from get_perim_from_initial_tign
      A=[];
      [A(:,1),A(:,2)]=find(C(2:end-1,2:end-1)==1);
      A(:,1)=A(:,1)+1;
      A(:,2)=A(:,2)+1;                 
      for k=1:size(A,1)
         D(A(k,1),A(k,2))=interval;
      end
      if ~isempty(P)
          P_new=[];
        for l=1:size(P,1)
           if (any(any(C(P(l,1)-1:P(l,1)+1,P(l,2)-1:P(l,2)+1)==0))==1)
             P_new=[P_new; P(l,:)];  
             D(P(l,1),P(l,2))=interval;
           end         
        end
        P=P_new;
        A=[A;P];
      end

   end 
   if size(A,1)==0
      data_steps=sprintf('%s\n finished at the time %i',data_steps,ts);
      fid = fopen(myfile, 'w');
      fprintf(fid,'%s',data_steps);   
      fclose(fid);
      break;
   end
   data_steps=sprintf('%s \n Step %i',data_steps,ts);
   data_steps=sprintf('%s \n first element of A',data_steps);
   data_steps=sprintf('%s \n %s',data_steps,mat2str(A(1,:)));
   data_steps=sprintf('%s \n C around [%i %i]',data_steps,pnt_a,pnt_b);
   data_steps=print_big_mat(data_steps,pnt_a,pnt_b,C);
   data_steps=sprintf('%s \n tign around [%i %i]',data_steps,pnt_a,pnt_b);
   data_steps=print_big_mat(data_steps,pnt_a,pnt_b,tign);
   
   cur_num_wrf=ceil((ts-1)/time_step);
   cur_ts=mod((ts-1),time_step);
   if (cur_ts==0) 
       cur_ts=time_step;
   end
   ts-1
   cur_num_wrf
   cur_ts
   ros_new=read_ros_from_wrfout(wrfout{cur_num_wrf},cur_ts);
   
   [tign,C,D,I,data_steps]=get_tign_one_timestep(tign,ros_old,ros_new,A,C,D,I,distance,interval,ts,data_steps);
      
   data_steps=sprintf('%s \n Main cycle for step %i is over',data_steps,ts);
   % figure(2); contour(C);title(sprintf('step %i, Matrix C, before subfunction',ts)); drawnow       
   if any(any(D~=0))
      data_steps=sprintf('%s \n Error: D needs to be=0',data_steps);
   end      
   ros_old=ros_new;   
   %figure(1); contour(tign);title(sprintf('step %i, tign',ts)); drawnow 
   %figure(2); contour(C);title(sprintf('step %i, Matrix C',ts)); drawnow 
   changed=sum(C(:)~=C_old(:));
   data_steps=sprintf('%s\n After a cycle tign changed in %i points',data_steps,changed);
   fid = fopen(myfile, 'w');
   fprintf(fid,'%s',data_steps); 
   fclose(fid);
   C_old=C;
end
index=find(C==0);
data_steps=sprintf('%s\n All the steps are done, but there are still %i points, that were not updated yet',data_steps,size(index));
fid = fopen(myfile, 'w');
fprintf(fid,'%s',data_steps); 
fclose(fid);
end
                                  

function [tign,C,D,I,data_steps]=get_tign_one_timestep(tign,ros_old,ros_new,B,C,D,I,distance,interval,ts,data_steps)

step=1;
data_steps=sprintf('%s\n subcycle in step %i',data_steps,ts);
while any(any(D>0))
   data_steps=sprintf('%s\n substep %i',data_steps,step);
   data_steps=sprintf('%s\n points whose neighbors are being updated (size(B,1)=) %i',data_steps,size(B,1));
   D_old=D;
   for j=1:size(B,1)         
      for dx=-1:1
         for dy=-1:1
            if (C(B(j,1)+dx,B(j,2)+dy)==0) 
               F=0.25*D(B(j,1),B(j,2))*(ros_old(B(j,1),B(j,2),2-dx,2-dy)+ros_new(B(j,1),B(j,2),2-dx,2-dy) + ...
                 ros_old(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)+ros_new(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)); 
             I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)=(step==1)*I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy);  
             if (I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)>0)&&(step>1)
                  data_steps=sprintf('%s\n Error(5): I happens to be more than 0',data_steps);
             end                     
               if (I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)+F>=distance(B(j,1),B(j,2),2-dx,2-dy))
                  tign(B(j,1)+dx,B(j,2)+dy)=min(tign(B(j,1),B(j,2)),ts*interval)- ...
                    ((distance(B(j,1),B(j,2),2-dx,2-dy)-I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy))/F)*(D(B(j,1),B(j,2)));
                  C(B(j,1)+dx,B(j,2)+dy)=1;
                  D(B(j,1)+dx,B(j,2)+dy)=tign(B(j,1)+dx,B(j,2)+dy)-(ts-1)*interval;
                  if (D(B(j,1)+dx,B(j,2)+dy)<0)
                     data_steps=sprintf('%s\n Error(3) D happens to be less than 0',data_steps);
                     data_steps=sprintf('%s\n At point (i,j)%d%d',data_steps,i,j);
                     data_steps=sprintf('%s\n B(j,1),B(j,2), B(j,1)+dx,B(j,2)+dy %d%d%d%d',data_steps,B(j,1),B(j,2), B(j,1)+dx,B(j,2)+dy);
                     data_steps=sprintf('%s\n D(B(j,1)+dx,B(j,2)+dy %d)',data_steps,D(B(j,1)+dx,B(j,2)+dy));
                  end
               else
                  I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)=I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)+F;
               end
            elseif (C(B(j,1)+dx,B(j,2)+dy)==1)
              F=0.25*D(B(j,1),B(j,2))*(ros_old(B(j,1),B(j,2),2-dx,2-dy)+ros_new(B(j,1),B(j,2),2-dx,2-dy) + ...
                 ros_old(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)+ros_new(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy));                                                  
               if (I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)+F>=distance(B(j,1),B(j,2),2-dx,2-dy))
                  tign_new=min(tign(B(j,1),B(j,2)),ts*interval)- ...
                    ((distance(B(j,1),B(j,2),2-dx,2-dy)-I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy))/F)*(D(B(j,1),B(j,2)));
                  I(B(j,1)+dx,B(j,2)+dy,2-dx,2-dy)=0;                    
                  if tign_new>tign(B(j,1)+dx,B(j,2)+dy)
                     tign(B(j,1)+dx,B(j,2)+dy)=tign_new;
                     D(B(j,1)+dx,B(j,2)+dy)=tign(B(j,1)+dx,B(j,2)+dy)-(ts-1)*interval;
                     if (D(B(j,1)+dx,B(j,2)+dy)<0)
                        data_steps=sprintf('%s\n Error(4): D happens to be less than 0',data_steps);                              
                     end                           
                  end                           
               end                       
            end
         end    
      end
      D(B(j,1),B(j,2))=0;            
   end
   % figure(3); contour(C);title(sprintf('step %i, Matrix C in subfunction substep %i',ts2,step)); drawnow 
   step=step+1;
   [row,col]=find((D_old==D)&(D_old~=0));
   if (size(row,1)~=0)
      data_steps=sprintf('%s\n Error find((D_old==D_new)&(D_old~=0)) is not equal, D(row(1),col(1))= %f',data_steps,D(row(1),col(1)));
   end
   % I do this part in the end, since I initialize A for the first step
   B=[];
   [B(:,1),B(:,2)]=find(D(2:end-1,2:end-1)>0);
   B(:,1)=B(:,1)+1;
   B(:,2)=B(:,2)+1;   
end
[row,col]=find(C==1);
for b=1:size(row,1)
   if ~any(any(C(row(b)-1:row(b)+1,col(b)-1:col(b)+1)==0))
      C(row(b),col(b))=2;
   end        
end
end

function data_steps=print_big_mat(data_steps,A11,A12,C)
for index = A11-3:A11+3
   data_steps=sprintf('%s \n %s',data_steps,mat2str(C(index,A12-3:A12+3)));
end
end

function distance=get_distances(long,lat)
% computing 4d array of distances between a point and its 8 neighbors
% 
% input:
%   long(i,j), lat(i,j) geographical coordinates of node [i,j], i=1:m, j=1:n, [m,n]=size(long)=size(lat)
%
% output
%   distance(i+1,j+1,a+2,b+2) = geographical distance between node [i,j] and [i+a,j+b] , a,b=-1:1
    
distance=zeros(size(long,1),size(long,2),3,3);    
for i=2:size(long,1)-1
   for j=2:size(long,2)-1
      for a=-1:1
         for b=-1:1
            % distance between node [i,j] and [i+a,j+b]
            distance(i,j,a+2,b+2)=sqrt((long(i+a,j+b,1)-long(i,j,1))^2+(lat(i+a,j+b,1)-lat(i,j,1))^2);
         end
      end
   end
end    
%IDEAS
% some ideas to look at
% maybe use mirroring?
% maybe keep numbering [i,j] always same as original outside of routines?
% now need to remember which array is shifted and bordered by zeros and which one is not
% if I was doing it: keep distance size (m,n,3,3) but not compute distance(1,1,...) etc
% and keep it zero, same with delta_tign, etc.    
end

function [A,C,D,tign]=get_perim_from_initial_tign(fire_area,time,interval,time_step,num_wrf)

% in:
% tign    ignition time
% out:
% C = 3 - area outside of the fire perimeter;
%     2 - area whose tign was already computed and it was used to
%         compute the tign of its neighbors + not burning points that lie  
%         on the perimeter;
%     1 - area whose tign was computed, but may still updated from
%         neighbors. This area will be used to compute the neighbors either
%         next time step or during the subcycle;
%     0 - area inside the fire perimeter, that needs to be computed;
% A       rows [i,j] of indices of nodes not burning that have at least one
%         burning neighbor. (equal to area where to C=2, but only in the 
%         first time step)
C=4*(fire_area==0);
D=zeros(size(fire_area));
tign=zeros(size(fire_area));
format long
for i=2:size(fire_area,1)-1
   for j=2:size(fire_area,2)-1
      % if (i,j) is not burning
      if (fire_area(i,j)==0) 
         % if any neighbor is burning
         if (any(any(fire_area(i-1:i+1,j-1:j+1)>0))==1)
            % add [i,j] to A
            C(i,j)=3;
            D(i,j)=interval;
          % why?  tign(i,j)=interval*(time_step*(num_wrf-1)+time-1); % I do that because time starts with 00:00
          % Why did I have above line before
            tign(i,j)=interval*(time_step*(num_wrf-1)+time); % I do that because time starts with 00:00
         end
      end
   end
end
[row,col]=find(C==3);
A=[row,col];
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%***% function [tign,distance,A,D]=tign_update(tign,A,D,delta_tign,time_now,distance,interval,ros)
% function [tign,distance,A,D]=tign_update(tign,A,D,delta_tign,time_now,distance,interval)
% 
% % A - set of points whose neighbors will be updated
% % C - if C[i,j]=1, then the neighbors of the point [i,j]
% % will be updated in the next iteration within the same timestep
% % D - if D[i,j]=1, then the neighbors of the point [i,j]
% % will be updated only in the next timestep (only after we update ros)
% 
% C=zeros(size(tign));
% 
% for i=1:size(A,1)
%     for dx=-1:1   
%         for dy=-1:1  
%                 if (tign(A(i,1)+dx,A(i,2)+dy)<time_now)==1
%                     tign_new=tign(A(i,1),A(i,2))-0.5*(delta_tign(A(i,1)+dx,A(i,2)+dy,-dx+2,-dy+2)+delta_tign(A(i,1),A(i,2),2-dx,2-dy));
%                     if (tign(A(i,1)+dx,A(i,2)+dy)<tign_new)&&(tign_new<=time_now)
%                            tign(A(i,1)+dx,A(i,2)+dy)=tign_new;
%                             C(A(i,1)+dx,A(i,2)+dy)=1;
% 
%                             if (time_now-tign_new>interval)
%                                 D(A(i,1),A(i,2))=1;
%                             end
% %                             
% %                         if (time_now-tign_new<interval)
% %                             
% %                         else
% %                             D(A(i,1),A(i,2))=1;
% %                         end
% 
% % % %                         if (time_now-tign_new<interval)
% % % %                             tign(A(i,1)+dx,A(i,2)+dy)=tign_new;
% % % %                             C(A(i,1)+dx,A(i,2)+dy)=1;
% % % %                         else
% % % %                             distance(A(i,1),A(i,2),2-dx,2-dy)=distance(A(i,1),A(i,2),2-dx,2-dy)-interval*ros(A(i,1),A(i,2),2-dx,2-dy);
% % % %                             distance(A(i,1)+dx,A(i,2)+dy,2-dx,2-dy)=distance(A(i,1)+dx,A(i,2)+dy,2-dx,2-dy)-interval*ros(A(i,1)+dx,A(i,2)+dy,2-dx,2-dy);
% % % %                             D(A(i,1),A(i,2))=1;
% % % %                         end
%                         
%                     end
%             
%                 end            
%         end
%     end
% end
% A=[];
% [A(:,1),A(:,2)]=find(C>0);
% end
% 
% 
% 
% % maybe get_delta_tign ?



%***% function delta_tign=get_delta_tign(distance,ros)
% % computing 4d array of differences of tign (delta_tign) between a point and its 8 neighbors
% % input:
% %    distance(i+1,j+1,a+2,b+2)  geographical distance between nodes [i,j] and [i+a,j+b]
% %                           from get_distances
% %    ros(i,j,a+2,b+2)   rate of spread at node [i,j] in the direction towards [i+a,j+b]
% %
% % output:
% %    delta_tign(i,j,a+2,b+2) time the fire takes to propagate from [i,j] to [i+a,j+b]
% 
% delta_tign=zeros(size(distance));
%   
% for i=2:size(delta_tign,1)-1
%     for j=2:size(delta_tign,2)-1
%         for a=-1:1
%             for b=-1:1
%                 delta_tign(i,j,a+2,b+2)=distance(i,j,a+2,b+2)/ros(i-1,j-1,a+2,b+2);
%             end
%         end
%     end
% end
% 
% end





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%***% function result=perimeter_in_const_ros(long,lat,fire_area,wrfout,time_now,time,interval)
% 
% % Volodymyr Kondratenko           July 19 2013	
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% % Input:    
% %    long,lat      longtitude and latitude converted to meters
% %    fire_area     fire map,[0,1] array, where 0- not
% %                  burning area, >0 burning area, 1 - area that was burnt
% %    wrfout        name of the wrfout file, that is being used to read ros
% %    time_now      the perimeter time (s)
% %    time          update the wind every count time steps
% %    interval      time step in wrfout in seconds
% %  JM NEVER CHANGE INPUT VARIABLES
% % 
% % Output: 
% %    Final Matrix of times of ignition will be printed to 'output_tign.txt' % JM use save tign instead, create tign.mat
% %
% %
% [n,m]=size(long);
% 
% 
% 'perimeter_in'
% 
% % JM algorithm state is stored in arrays A D C
% %
% % A contains rows [i,j] of indices of nodes not burning that have at least one burning neighbor
% %   and time of ignition > time_now 
% 
% % Reading Rate of spread
% ros=read_ros_from_wrfout(wrfout,time); % JM should be read_ros_from_wrfout
% 
% % Getting matrix A from the initial tign_g, where
% A=get_perim_from_initial_tign(fire_area); 
% % Computing 4d array of distances between a point and its 8 neighbors
% distance=get_distances(long,lat);
% % JM do not change this later, use adjustment array instead if you must
% 
% % Computing 4d array of differences of tign (delta_tign) between a point 
% % and its 8 neighbors
% [delta_tign]=get_delta_tign(distance,ros);
% 
% % Initializing tign
% % Everything outside of the burning area is set to time_now, 
% % inside is set to 0
% tign_in=zeros(n,m);
% tign_in=(fire_area(:,:)==0).*time_now;  %Should I use |a-b|<eps ? %JM never test reals on equality
% 
% % D - if D[i,j]=1, then the neighbors of the point [i,j]
% % will be updated only in the next timestep (only after we update ros)
% D=zeros(n,m);
% 
% changed=1;
% tic
%     sprintf('size of A- %i',size(A,1))
% for istep=1:max(size(tign_in)),
%     if size(A,1)==0, 
% 		'printed'
% 		break
%     end
%     
%     time_toc=toc;
%     str= sprintf('%f -- How long does it take to run step %i',time_toc,istep-1);
%     tign_last=tign_in;
%     %contour(tign_in(2:end-1,2:end-1));drawnow
%     [tign_in,distance,A,D]=tign_update(tign_in,A,D,delta_tign,time_now,distance,interval);
% %%%    [tign_in,distance,A,D]=tign_update(tign_in,A,D,delta_tign,time_now,distance,interval,ros);
%     %contour(tign_in(2:end-1,2:end-1));title(sprintf('step %i',istep)),drawnow
% 
%     changed=sum(tign_in(:)~=tign_last(:));
% 
%     sprintf('%s \n step %i inside tign changed at %i points \n %f -- norm of the difference',str,istep,changed,norm(tign_in-tign_last))
%     sprintf('size of A- %i',size(A,1))
% % % %     if (size(A,1)==0)
% % % %         if (time-count)>0
% % % %             'getting new ros'
% % % %             A=[];
% % % %             [A(:,1),A(:,2)]=find(D>0);
% % % %             sprintf('size of A- %i',size(A,1))
% % % %             D=zeros(n,m);
% % % %             time_now=time_now-count*interval
% % % %             time=time-1;
% % % %             ros=read_ros_from_wrfout(wrfout,time);
% % % %             delta_tign=get_delta_tign(distance,ros);
% % % %     
% % % %         elseif (time-count)<0
% % % %         
% % % %         'time-count<0'
% % % %         end
% % % %     end
% % % %        
% end
% 
% result=tign_in;
% 
% fid = fopen('output_tign.txt', 'w');
%     dlmwrite('output_tign.txt', result, 'delimiter', '\t','precision', '%.4f');
%     fclose(fid);
%     
% if changed~=0,
%     'did not find fixed point inside'
%    end
% end
% 
% 
% % dead code
%***% function result=perimeter_in_2(long,lat,ros,time_now,bound,wrfout,interval,count)
% 
% % Volodymyr Kondratenko           December 8 2012	
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
% % The function creates the initial matrix of times of ignitions
% %			given the perimeter and time of ignition at the points of the perimeter and
% %			is using wind and terrain gradient in its calculations
% % Input: We get it after reading the data using function read_file_perimeter.m 
% %        
% %        long			FXLONG*UNIT_FXLONG, longtitude coordinates of the mesh converted into meters
% %        lat			FXLAT*UNIT_FXLAT, latitude coordinates of the mesh converted into meters 
% %        uf,vf			horizontal wind velocity vectors of the points of the mesh 
% %        dzdxf,dzdyf	terrain gradient of the points of the mesh
% %        time_now		time of ignition on the fireline (fire perimeter)
% %        bound			set of ordered points of the fire perimeter 1st=last 
% %						bound(i,1)-horisontal; bound(i,1)-vertical coordinate
% %
% % Output:   Matrix of time of ignition
% %
% % Code:
% 
% %addpath('../../other/Matlab/util1_jan');
% %addpath('../../other/Matlab/netcdf');
% tic
% fuels % This function is needed to create fuel variable, that contains all the characteristics 
%       % types of fuel, this function lies in the same folder where you run the main_function.m
% 	  % (where is it originally located/can be created?)
% 
% format long
% 
% bnd_size=size(bound);
% n=size(long,1);
% m=size(long,2);
% 
% %tign=zeros(n,m);      % "time of ignition matrix" of the nodes 
% %A=zeros(n,m);         % flag matrix of the nodes, A(i,j)=1 if the time of ignition of the 
%                       % point (i,j) was updated at least once 
% 
% 'started' 
% %  IN - matrix, that shows, whether the point is inside (IN(x,y)>0) the burning region
% %  or outside (IN(x,y)<0)
% %  ON - matrix that, shows whether the point is on the boundary or not
% %  Both matrices evaluated using "polygon", coefficients are multiplied by
% %  10^6, because the function looses acuracy when it deals with decimals
% 
% xv=bound(:,1);
% yv=bound(:,2);
% xv=xv*100000;
% yv=yv*100000;
% lat1=lat*100000;
% long1=long*100000;
% [IN,ON] = inpolygon(long1,lat1,xv,yv);
% 
% % Code 
% 
% [delta_tign]=delta_tign_calculation(long,lat,ros);
% 
% % Calculates needed variables for rate of fire spread calculation
% 
% %%%%%%% First part %%%%%%%
% % Set everything inside to time_now and update the tign of the points outside
% 
% % Initializing flag matrix A and time of ignition (tign)
% % Extending the boundaries, in order to speed up the algorythm
% %A=[];
% %C=zeros(n+2,m+2);
% % A contains coordinates of the points that were updated during the last
% % step
% 
% IN_ext=(2)*ones(n+2,m+2);
% IN_ext(2:n+1,2:m+1)=IN(:,:,1);
% 
% 
% % Set all the points outside to time_now and update the points inside
% 
% % Initializing flag matrix A and time of ignition (tign)
% % Extending the boundaries, in order to speed up the algorythm
% A=[];
% C=zeros(n+2,m+2);
% for i=2:n+1
%     for j=2:m+1
%         if IN_ext(i,j)==0
%             if sum(sum(IN_ext(i-1:i+1,j-1:j+1)))>0
%             A=[A;[i,j]];
%             end
%        end
%     end
% end
% 
% tign_in=zeros(n+2,m+2);
% tign_in(2:n+1,2:m+1)=(1-IN(:,:,1)).*time_now;
% changed=1;
% 
% time_old=time_now;
% % The algorithm stops when the matrix converges (tign_old-tign==0) or if the amount of iterations
% % % reaches the max(size()) of the mesh
% count
% interval
% count*interval
% for istep=1:max(size(tign_in)),
%     if changed==0, 
% 		% The matrix of tign converged
% 		'printed'
% 		break
%     end
%     
%     tign_last=tign_in;
%     time_toc=toc;
%     str= sprintf('%f -- How long does it take to run step %i',time_toc,istep-1);
%    
%     
%     if ((time_old-max(max(tign_in(A(:,1),A(:,2)))))>=(count*interval))&&((time-count)>0)
%     'getting new ros'
%       sprintf('time_old= %f',time_old)  
%       sprintf('tign_in(A(1,1),A(1,2))= %f',tign_in(A(1,1),A(1,2)))
%       sprintf('min(min(tign_in(A(:,1),A(:,2))))= %f',min(min(tign_in(A(:,1),A(:,2)))))
%       sprintf('max(max(tign_in(A(:,1),A(:,2))))= %f',max(max(tign_in(A(:,1),A(:,2)))))      
%       sprintf('time_old-max(max(tign_in(A(:,1),A(:,2)))= %f',time_old-max(max(tign_in(A(:,1),A(:,2)))))
%         
%         count1=mod(time_old-max(max(tign_in(A(:,1),A(:,2)))),count*interval)
%         time_old=time_old-count1*interval
%         time=time-count1
%        ros=read_ros_from_wrfout(wrfout,size(long,1),size(long,2),time);
%        delta_tign=delta_tign_calculation(long,lat,ros);
%     end
% 
%     
%     % tign_update - updates the tign of the points
%     [tign_in,A,C]=tign_update(tign_in,A,IN_ext,delta_tign,time_now);
%   % when it is outside the last parameter is 0, inside 1  
%     changed=sum(tign_in(:)~=tign_last(:));
% %    if (changed<=5)
% %       for i=1:size(A,1)
% %           A(i,:)
% %        end
% %    end 
% 
%     sprintf('%s \n step %i inside tign changed at %i points \n %f -- norm of the difference',str,istep,changed,norm(tign_in-tign_last))
%    sprintf('size of A- %i',size(A,1))   
% end
% final_tign=tign_in;
% %final_tign(2:n+1,2:m+1)=(IN(:,:,1)>0).*tign_in(2:n+1,2:m+1)+(IN(:,:,1)==0).*tign(2:n+1,2:m+1);
% result=final_tign(2:n+1,2:m+1);
% 
% fid = fopen('output_tign.txt', 'w');
%     dlmwrite('output_tign.txt', result, 'delimiter', '\t','precision', '%.4f');
%     fclose(fid);
%     
% if changed~=0,
%     'did not find fixed point inside'
%    end
% end





