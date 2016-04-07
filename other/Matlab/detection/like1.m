function [v0,v1]=like1(dw,t,stretch)
% [v0,v1]=like1(dw,t,stretch)
% dw > 0 if fire detected, <0 if not, 0 if cloud
% t = time since fire arrival (days)
Tmin=stretch(1);Tmax=stretch(2);Tneg=stretch(3);Tpos=stretch(4);


tneg = (t-Tmin)./Tneg;
tpos = (t-Tmax)./Tpos;
v0y = -tneg.*tneg.*(t<Tmin)-tpos.*tpos.*(t>Tmax);

g0 = @(x) -0.25.*x.*x.*x+0.75*x-0.5;
% v0n1 = (t<Tmin-2*Tneg);
v0n2 = g0(-tneg-1).*(Tmin-2*Tneg<=t & t<=Tmin);
v0n3 =  - (Tmin < t & t<Tmax);
v0n4 = g0(tpos-1).*(Tmax <= t & t <= Tmax+2*Tpos);
% v0n5 = (t>Tmax+2*Tpos);
% v0n = v0n1 + v0n2 + v0n3 + v0n4 + v0n5;
v0n =  v0n2 + v0n3 + v0n4 ;
v0 = -dw.*(dw<0).*v0n + dw.*(dw>0).*v0y;

v1y = -2.*tneg.*(t<Tmin)-2.*tpos.*(t>Tmax);

g1 = @(x) -1.5*x.*x+1.5;
v1n = - g1(-tneg-1).*(Tmin-2*Tneg<=t & t<=Tmin) ...
    + g1(tpos-1).*(Tmax <= t & t <= Tmax+2*Tpos);
v1 = -dw.*(dw<0).*v1n + dw.*(dw>0).*v1y;

end        

    
    
    
