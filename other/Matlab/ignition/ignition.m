function B=ignition(data)

%%%%% What is new in this version %%%%%
%  Added calculation of ignition time for points, that are outside
%  the burning area

% The function Ignition plots and showes how the fire was propagating,
%           given ignition point, boundary region and time_now
% Input:    data1 : String - data, that contains the name of the Text file.
%           1rt row - time_now (second number is not needed, is set to 0);
%           2nd row - size of the mesh (m,n);
%           3rd row - coordinates of the ignition point(ign_x,ign_y);
%           All next rows - coordinates of all the
%           points of the fire perimeter (lon,lat). 
%
%           data2: matrix of the lon coordinates of the mesh points 
%
%           data3: matrix of the lat coordinates of the mesh points
%
% Output:   Matrix of time of ignition of all the mesh points
%
%
% Volodymyr Kondratenko           April 8 2011
%--------------------------------------
% Command line: what to do to call the function
%--------------------------------------

%  data='data.txt'
%  B=ignition6(data)

% utility to creat the test files
% Code:
% Getting data

fid = fopen(data);
data = fscanf(fid,'%g %g',[2 inf]) % It has two rows now.
data = data';
fclose(fid)
data_size=size(data);

time_now=data(1,1);  % time of ignition on the boundary 
mesh_size=data(2,:); % size of the matrix
ign_pnt=data(3,:);   % coordinates of the ignition point    

bound=data(4:data_size(1),:); 
% bound - set of ordered points of the boundary 1st=last 
% bound(i,1)-horisontal; bound(i,1)-vertical coordinate
bnd_size=size(bound);

B=zeros(mesh_size);        % "time of ignition matrix" of the nodes 
                           % Ignition point corresponds to B()=0
C=zeros(mesh_size);        % Matrix of distances from ignition point to all points


%  IN - matrix that shows, whether the point is inside (IN(x,y)>0) the burning region
%  or outside (IN(x,y)<0)
%  ON - matrix that, shows whether the point is on the boundary or not
%  Both matrices evaluated using "polygon" 

x=ones(mesh_size);
for i=1:mesh_size(1)
     x(i,:)=i*x(i,:);
end
y=x';
xv=bound(:,1);
yv=bound(:,2);
[IN,ON] = inpolygon(x,y,xv,yv);
 
% Calculation of the matrix of distances

for i=1:mesh_size(1)
     for j=1:mesh_size(2)
          C(i,j)=sqrt((i-ign_pnt(1))^2+(j-ign_pnt(2))^2);
     end
end
 
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
for i=1:mesh_size(1)     
    for j=1:mesh_size(2)
        if (ON(i,j)>0)          % The point we check is on the boundary
            B(i,j)=time_now;
            k=-1;
        else
            a_old=line_sign(ign_pnt(1),ign_pnt(2),i,j,bound(1,1),bound(1,2));
            k=2;
           
            while (k>0)&&(k<=bnd_size(1))
                if (a_old==0)
                    a_new=a_old;
                    k=1;
                else
                    a_new=line_sign(ign_pnt(1),ign_pnt(2),i,j,bound(k,1),bound(k,2));
                end
                if a_old*a_new<0  
            
                    if IN(i,j)>0        % The point is inside the burning region
                    
                        % Check if the point is on the line between 
                        % the ignition point and 2 boundary points
                        a1=line_sign(i,j,bound(k,1),bound(k,2),ign_pnt(1),ign_pnt(2));
                        a2=line_sign(i,j,bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2));
                        if a1*a2<0
                            dist1=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),ign_pnt(1),ign_pnt(2));
                            dist2=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),i,j);
                            B(i,j)=time_now*(dist1-dist2)/dist1;
                            k=-1;
                        end
                    else % point outside the burning region
                        % Check if 2 boundary points on the line between 
                        % the point and the ignition point
                        a1=line_sign(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),ign_pnt(1),ign_pnt(2));
                        a2=line_sign(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),i,j);
                        if a1*a2<0
                            dist1=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),ign_pnt(1),ign_pnt(2));
                            dist2=line_dist(bound(k,1),bound(k,2),bound(k-1,1),bound(k-1,2),i,j);
                            B(i,j)=time_now*dist2/dist1;
                            k=-1;
                        end 
                    end
                elseif a_new==0
                    % Case if the line goes exactly through the boundary point                                           
                    % Check if the point lies between ignition point and boundary point
                    b1=sqrt((i-ign_pnt(1))^2+(j-ign_pnt(2))^2);
                    b2=sqrt((i-bound(k,1))^2+(j-bound(k,2))^2);
                    b3=sqrt((bound(k,1)-ign_pnt(1))^2+(bound(k,2)-ign_pnt(2))^2);
                    if IN(i,j)>0
                        if (b1+b2<b3+eps)&&(b1+b2>b3-eps)
                            B(i,j)=time_now*b1/b3; 
                            k=-1;
                        else
                            a_new=line_sign(ign_pnt(1),ign_pnt(2),i,j,bound(k+1,1),bound(k+1,2));
                            k=k+1;
                        end
                    else
                        if (b2+b3<b1+eps)&&(b2+b3>b1-eps)
                            B(i,j)=time_now*b1/b3; 
                            k=-1;
                        else
                            a_new=line_sign(ign_pnt(1),ign_pnt(2),i,j,bound(k+1,1),bound(k+1,2));
                            k=k+1;
                        end
                        
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
%x=1:1:9
%y=1:1:9
%figure(2)
%surf(x,y,B)
%B
 
