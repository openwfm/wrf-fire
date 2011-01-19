% from 01/13/11
function ignition2()
% bound - set of ordered points of the boundary 1st=last 
% bound :: 2*n  bound(1,i) - horisontal coordinate, 
% bound(2,i) - vertical coordinate
bound(1,1)= 3; bound(2,1)= 3;
bound(1,2)= 3; bound(2,2)= 4;
bound(1,3)= 3; bound(2,3)= 5;
bound(1,4)= 3; bound(2,4)= 6;
bound(1,5)= 3; bound(2,5)= 7;
bound(1,6)= 4; bound(2,6)= 7;
bound(1,7)= 5; bound(2,7)= 7;
bound(1,8)= 6; bound(2,8)= 7;
bound(1,9)= 7; bound(2,9)= 7;
bound(1,10)= 7; bound(2,10)= 6;
bound(1,11)= 7; bound(2,11)= 5;
bound(1,12)= 7; bound(2,12)= 4;
bound(1,13)= 7; bound(2,13)= 3;
bound(1,14)= 6; bound(2,14)= 3;
bound(1,15)= 5; bound(2,15)= 3;
bound(1,16)= 4; bound(2,16)= 3;
bound(1,17)= 3; bound(2,17)= 3;
bnd_size=size(bound(1,:));
% QUESTIONS
 % how again the input is represented? pairs of numbers of the boundary?
 % if yes, how do we indicate that anouther loop started?
 % 2. how to identify clockwise or counterclockwise?
 % 3. What if it already burn out on the boundary of the obstacle?
 % 4. between obstacles by proportion?
  
 % matrix of nodes that tells whether the node is/was on fire or not
 %A(i,j)>0 - on fire, A(i,j)=0- not
 %Maybe we should add smthing like: A>0 - on fire, A<0 - not on fire; 
 %A=0 - unburning area
 A=[zeros(2,9);zeros(5,2),ones(5,5),zeros(5,2);zeros(2,9)];
 
 n=9; m=9; % size of the matrix
 ign_x=5;
 ign_y=5;
 time_now=10;
 % matrix of 'time of ignition' of the nodes ignition point has time_ign=0
 B=zeros(n,m);
 C=zeros(n,m);
 % dist(x,ign_p)/dist(Y,ign_p)=time_of_ign(x)/time_now
 % Finding the matrix of distances C
 for i=1:n
     for j=1:m
             C(i,j)=sqrt((i-ign_x)^2+(j-ign_y)^2);
     end
 end
 % filling the matrix of distances for all other points that are not on
 % fire and calculating 'time of ignition' for all points
 % line - going through 2 points
 % (y-y1)*(x2-x1)-(x-x1)*(y2-y1)=0
 % then check for all couples, whether they are on one or on different
 % sides of the line
 aa=0;
 for i=1:n
     for j=1:m
         if A(i,j)>0
            a_old=line_dist(ign_x,ign_y,i,j,bound(1,1),bound(2,1));
             for k=2:bnd_size(2)
                a_new=line_dist(ign_x,ign_y,i,j,bound(1,k),bound(2,k));
                if a_old*a_new<0
                    if aa==0
                        aa=(C(bound(1,k-1),bound(2,k-1))+C(bound(1,k),bound(2,k)))/2;
                    else
                        aaa=(C(bound(1,k-1),bound(2,k-1))+C(bound(1,k),bound(2,k)))/2;
                    end
                elseif a_new==0
                    if aa==0
                         aa=C(bound(1,k),bound(2,k));
                    else
                         aaa=C(bound(1,k),bound(2,k));
                    end

                    if k<bnd_size-1
                       a_old=line_dist(ign_x,ign_y,i,j,bound(1,k+1),bound(2,k+1));
                    end                  
                k=k+1;       
                end
                a_old=a_new;
             end
             aaaa=max(aa,aaa);
             B(i,j)=(C(i,j)*time_now)/aaaa;    
         else
             B(i,j)=time_now;
         end                
     end
 end
 x=1:1:9
 y=1:1:9
 surf(x,y,B)
 A
 B