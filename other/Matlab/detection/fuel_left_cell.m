function varargout=fuel_left_cell(t,fuel_time)
% frac=fuel_left(t,fuel_time)
% [frac,dfracdt]=fuel_left(t,fuel_time)
% [frac,dfracdt,dfrac]=fuel_left(t,fuel_time)
% 
% compute fuel fraction left in one cell
% 
% in: 
%   t           fire arrival time (0=now) at corners, size (2,2)
%   fuel_time   time the fuel takes to burn to 1/e = 0.36
% out:
%   frac        the fuel fraction
%   dfracdt     derivative the all components of t change by the same
%   dfrac       partial derivatives wrf to components of t

% to test:
% t=[-1,0;0,1.1], s=0.1, f=1, dt=0.01
% p1=fuel_left_cell(t-dt-s,f);p2=fuel_left_cell(t+dt-s,f);
% [p,dp]=fuel_left_cell(t-s,f);d=(p2-p1)/(2*dt);err=d-dp

ps=ssum(t);
aps=ssum(abs(t))+realmin;
area=0.5*(1-ps/aps);
t0=min(t,0);
ta=0.25*ssum(t0);
frac=area*exp(ta/fuel_time) + (1. - area);

varargout(1)={frac};

if nargout < 2, return, end

% derivative wrt same increment in all 4 values of t
dpsdt=4;
dapsdt=ssum(sign(t));
dareadt=0.5*(dapsdt*ps-dpsdt*aps)./(aps.*aps);
dtadt=0.25*ssum(0.5*(1-sign(t)));
dfracdt=(dareadt+(area/fuel_time)*dtadt)*exp(ta/fuel_time)-dareadt;

varargout(2)={dfracdt};

if nargout < 3, return, end

% for heat flux, need derivative wrt same increment
dps=ones(2);
daps=sign(t);
darea=0.5*(daps*ps-dps*aps)./(aps.*aps);    
dta=0.25*(0.5*(1-sign(t)));
dfrac=((area/fuel_time)*dta+darea)*exp(ta/fuel_time)-darea;

varargout(3)={dfrac};

end