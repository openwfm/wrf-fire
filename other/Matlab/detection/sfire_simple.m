function tign=sfire_simple(r,dx,dy,tign_init,tol)
% in:
% r             structure with 2d fields f_ros11,...,f_ros33 (m/s)
% dx,dy         mesh spacing (m)
% tign_init     initial ignition times (s)
% tol           stopping tolerance (s)
% out:
% tign          ignition times
%
t0=tign_init;
t1=t0;
max_it=10*prod(size(t0));
dd=sqrt(dx^2+dy^2); % diagonal length
r11=dd./r.f_ros11;
r12=dx./r.f_ros12;
r13=dd./r.f_ros13;
r21=dx./r.f_ros21;
r23=dy./r.f_ros23;
r31=dd./r.f_ros31;
r32=dy./r.f_ros32;
r33=dd./r.f_ros33;
[m,n]=size(t0);
for it=1:max_it,
    % t1(2:end,:)=min(t1(2:end,:),t0(1:end-1,:)+r32(1:end-1,:));
    % t1(1:end-1,:)=min(t1(1:end-1,:),t0(2:end,:)+r12(2:end,:));
    % t1(:,2:end)=min(t1(:,2:end),t0(:,1:end-1)+r23(:,1:end-1));
    % t1(:,1:end-1)=min(t1(:,1:end-1),t0(:,2:end)+r21(:,2:end));
    % t1(2:end,2:end)=min(t1(2:end,2:end),t0(1:end-1,1:end-1)+r33(1:end-1,1:end-1));
    % t1(1:end-1,2:end)=min(t1(1:end-1,2:end),t0(2:end,1:end-1)+r13(2:end,1:end-1));
    % t1(2:end,1:end-1)=min(t1(2:end,1:end-1),t0(1:end-1,2:end)+r31(1:end-1,2:end));
    % t1(1:end-1,1:end-1)=min(t1(1:end-1,1:end-1),t0(2:end,2:end)+r11(2:end,2:end));
    for j=2:n-1
        for i=2:m-1
            t=t0(i,j);
            t=min(t,t0(i-1,j)+r32(i-1,j));
            t=min(t,t0(i+1,j)+r12(i+1,j));
            t=min(t,t0(i,j-1)+r23(i,j-1));
            t=min(t,t0(i,j+1)+r21(i,j+1));
            t=min(t,t0(i-1,j-1)+r33(i-1,j-1));
            t=min(t,t0(i+1,j-1)+r13(i+1,j-1));
            t=min(t,t0(i-1,j+1)+r31(i-1,j+1));
            t=min(t,t0(i+1,j+1)+r11(i+1,j+1));
            t1(i,j)=t;
        end
    end
    err=big(t1-t0);
    count=sum(t1(:)~=t0(:));
    fprintf('%i %g %i\n',it,err,count) 
    if err<=tol,fprintf('\n'),break,end
    t0=t1;
    mesh(t1); drawnow
end
tign=t1;
