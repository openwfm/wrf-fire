function t1=sfire_simple(r,dx,dy,t_init,mask,tol,rangex,rangey)
% t1=sfire_simple(t11,t11.dx,t11.dy,t11.tign_g,t11.mask,1e-5);
% in:
% r             structure with 2d fields f_ros11,...,f_ros33 (m/s)
% dx,dy         mesh spacing (m)
% tign_init     initial ignition times (s)
% tol           stopping tolerance (s)
% out:
% tign          ignition times
%
ext=0;
dd=sqrt(dx^2+dy^2); % diagonal length
if ~exist('rangex','var') || isempty(rangex),
    rangex=1:size(r.f_ros11,1);
end
if ~exist('rangey','var') || isempty(rangey),
    rangey=1:size(r.f_ros11,2);
end
rr.r11=dd./r.f_ros11(rangex,rangey);
rr.r12=dx./r.f_ros12(rangex,rangey);
rr.r13=dd./r.f_ros13(rangex,rangey);
rr.r21=dx./r.f_ros21(rangex,rangey);
rr.r23=dy./r.f_ros23(rangex,rangey);
rr.r31=dd./r.f_ros31(rangex,rangey);
rr.r32=dy./r.f_ros32(rangex,rangey);
rr.r33=dd./r.f_ros33(rangex,rangey);
sfire_simple_ext(rr,t_init,mask,0);
ncycles=100;
t0=ones(size(t_init))*inf;
t0(2:end-1,2:end-1)=t_init(2:end-1,2:end-1);
mint=false(size(t_init));mint(2:end-1,2:end-1)=mask(2:end-1,2:end-1);
% border=true(size(t_init));border(2:end-1,2:end-1)=false;
max_it=10*prod(size(t0))
for it=1:ncycles:max_it,
    t1=sfire_simple_ext([],t0,mask,ncycles);
    tt=abs(t1-t0);
    err=norm(tt(~isnan(tt(:))),1);
    count=sum(t1(:)~=t0(:));
    fprintf('%i %g %i\n',it,err,count) 
    t0=t1;
    mesh(t1); drawnow
    if err<=tol,break,end
end
