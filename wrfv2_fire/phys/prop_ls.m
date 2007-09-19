function [t,phi]=prop_ls(phi0,time0,time1,vx,vy,r,dx,dy,spread_rate)
% a jm/mkim version of propagate
% follows Osher-Fedkiw and Mitchell toolbox

% phi       level function out
% phi0      level function in
% time0     starting time
% time1     end time
% vx        component x of velocity field, passed to spread_rate
% vy        component y of velocity field, passed to spread_rate
% r         propagation speed in normal direction
% dx        mesh step in x direction
% dy        mesh step in y direction
% 

% initialize
phi=phi0; % allocate level function
[m,n]=size(phi);
t=time0;  % current time
tol=300*eps;
istep=0;
msteps=1000;
diffLx=zeros(m,n);
diffLy=zeros(m,n);
diffRx=zeros(m,n);
diffRy=zeros(m,n);

% time loop
while t<time1-tol & istep < msteps
    istep=istep+1;
    % one-sided differences
    [diffLx,diffRx,diffLy,diffRy,diffCx,diffCy]=get_diff(phi,dx,dy);         
    % gradient of level function is the normal direction
    scale=sqrt(diffCx.*diffCx+diffCy.*diffCy+eps); 
    nvx=diffCx./scale;
    nvy=diffCy./scale;
    % propagation speed 
    speed=spread_rate(r,vx,vy,nvx,nvy,scale);
    speed=max(speed,0);
    % to recover advection vv and spread r, transition between:
    % r = 0 & (normal,v) > const*speed => vv=speed*v/(normal,v) & rr=0
    % speed >> (normal,v) => vv=0 & rr=speed
    nvv = nvx .* vx + nvy .* vy;
    a=2*nvv>speed;
    nvv(a==0)=1;
    %no_wind_cases=sum(sum(1-a))
    rr=speed.*(1-a);
    vvx = vx .* a .* speed ./ nvv;
    vvy = vy .* a .* speed ./ nvv;
    % vvx=vx;vvy=vy;
    % Godunov scheme for normal motion
    flowLx=(diffLx>=0 & diffCx>=0);
    flowRx=(diffRx<=0 & diffCx<0);
    diff2x=diffLx.*flowLx + diffRx.*flowRx; %
    flowLy=(diffLy>=0 & diffCy>=0);
    flowRy=(diffRy<=0 & diffCy<0);
    diff2y=diffLy.*flowLy + diffRy.*flowRy; %
    grad=sqrt(diff2x.*diff2x + diff2y.*diff2y);
    nz=find(grad);
    %tbound_n(1)=max(abs(r.*sqrt(diff2x(nz)))./grad(nz))/dx; % time step bnd
    %tbound_n(2)=max(abs(r.*sqrt(diff2y(nz)))./grad(nz))/dy;
    tbound_np=r.*(abs(diff2x)./dx+abs(diff2y)./dy); % pointwise time step bnd
    tbound_n=max(tbound_np(nz)./abs(grad(nz))); % worst case
    tend_n =-rr.*grad;   % the result, tendency
    % standard upwinding for advection
    tend_a=-(diffLx.*max(vvx,0)+diffRx.*min(vvx,0)+...
        diffLy.*max(vvy,0)+diffRy.*min(vvy,0));
    tbound_ax=max(abs(vvx(:)))/dx;
    tbound_ay=max(abs(vvy(:)))/dy;
    % complete right-hand side
    tend=tend_n + tend_a;
    % complete time step bound
    tbound = 1/(tbound_n+tbound_ax+tbound_ay+eps); 
    % decide on timestep
    dt=min(time1-t,0.5*tbound);
    % trailing edge correction - do not allow fireline to go backwards
    %tt=max(dx,dy);
    %ins=find(phi<=-0);
    %tend(ins)=min(tend(ins),0);
    tend=min(tend,0);
    % advance
    phi=phi+dt*tend;
    t=t+dt;
end
fprintf('prop_ls: %g steps from %g to %g\n',istep,time0,t)
end % of the function prop_ls

