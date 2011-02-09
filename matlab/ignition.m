%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Did not do the case when ignition point lies on the boundary!!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% from 02/07/11
function ignition5()
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
 % figure(1);
 % line(bound(1,:),bound(2,:));
 
 %A=[zeros(2,9);zeros(5,2),ones(5,5),zeros(5,2);zeros(2,9)];
 % We do inpolygon
 x=[ones(1,9);2*ones(1,9);3*ones(1,9);4*ones(1,9);5*ones(1,9);6*ones(1,9);7*ones(1,9);8*ones(1,9);9*ones(1,9)];
 y=[ones(1,9);2*ones(1,9);3*ones(1,9);4*ones(1,9);5*ones(1,9);6*ones(1,9);7*ones(1,9);8*ones(1,9);9*ones(1,9)]';
 
 xv=bound(1,:);
 yv=bound(2,:);
 
 [IN,ON] = inpolygon(x,y,xv,yv);
 
 
 
 
 
 
 
 n=9; m=9; % size of the matrix
 ign_x=3;
 ign_y=3;
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
 eps=0.1;
 for i=1:n
     for j=1:m
         if IN(i,j)>0
            a_old=line_sign(ign_x,ign_y,i,j,bound(1,1),bound(2,1));
            k=2;
         while (k>0)&&(k<=bnd_size(2))
             if (a_old==0)
                 a_new=a_old;
                 k=1;
             else
                a_new=line_sign(ign_x,ign_y,i,j,bound(1,k),bound(2,k));
             end 
                if a_old*a_new<0
 % Check if the point is on the line between the ignition point and this 2 points
                a1=line_sign(i,j,bound(1,k),bound(2,k),ign_x,ign_y);
                a2=line_sign(i,j,bound(1,k),bound(2,k),bound(1,k-1),bound(2,k-1));
                    if a1*a2<0
                    dist1=line_dist(bound(1,k),bound(2,k),bound(1,k-1),bound(2,k-1),ign_x,ign_y);
                    dist2=line_dist(bound(1,k),bound(2,k),bound(1,k-1),bound(2,k-1),i,j);
 % We consider that tign og ignition point is=0                   
                    
                    B(i,j)=time_now*(dist1-dist2)/dist1;
                        k=-1;
                    end
          
                elseif a_new==0
                    if (ON(i,j)>0)
                        B(i,j)=time_now;
                        k=-1;
                        
                    else
                        b1=sqrt((i-ign_x)^2+(j-ign_y)^2);
                        b2=sqrt((i-bound(1,k))^2+(j-bound(2,k))^2);
                        b3=sqrt((bound(1,k)-ign_x)^2+(bound(2,k)-ign_y)^2);
                        if (b1+b2<b3+eps)&&(b1+b2>b3-eps)
                            B(i,j)=time_now*b1/b3; 
                            k=-1;
                        else
                            a_new=line_sign(ign_x,ign_y,i,j,bound(1,k+1),bound(2,k+1));
                            k=k+1;
                        end
                    end  
                  end
                a_old=a_new;   
                k=k+1;       
          end
                    
         else
             B(i,j)=time_now+1;
         end                
     end
 end
 
 x=1:1:9
 y=1:1:9
 figure(2)
 surf(x,y,B)
 A
 B
