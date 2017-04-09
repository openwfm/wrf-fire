function vec=ros2vec(ros)
% from the ros values in 8 directions make vector representation
s=1/sqrt(2);
r=squeeze(ros)
Y=[-s 0 s
   -1 0 1
   -s 0 s]
X=Y'
xx=X.*r
yy=Y.*r
ii=[3 3 2 1 1 1 2 3];
jj=[2 3 3 3 2 1 1 1];    
for k=1:8
    i=ii(k);
    j=jj(k);
    plot([xx(i,j)],[yy(i,j)],'*k')
    rr(k)=r(i,j);
    hold on
end
tt=linspace(0,9/4,10)*pi;
rr(9:10)=rr(1:2);
pp = csape(tt,rr,'periodic');
fpolarplot([0,0],@(theta)ppval(pp,theta),500,'-r');

% find the vector xs,yz of largest spread
% X(i,j)*xs + Y(i,j)*ys = r(i,j)-omni
%A = [X(:),Y(:)]; A(5,:)=[];
%b = r(:)-omni; b(5)=[];
%v= A\b;
%f = @(theta)2*omni+v(1)*cos(theta)+v(2)*sin(theta);

hold off
end

function H=fpolarplot(center,f,np,LineSpec)
% H=radplot(center,f,np,style)
%   center 
%   function handle
%   np 
% adapted from https://www.mathworks.com/matlabcentral/fileexchange/2876-draw-a-circle
theta=linspace(0,2*pi,np);
rho=f(theta);
[x,y]=pol2cart(theta,rho);
x=x+center(1);
y=y+center(2);
H=plot(x,y,LineSpec);
axis square;
end