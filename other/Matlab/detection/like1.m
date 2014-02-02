function [v0,v1]=like1(dw,T,Peak,Wpos,Wneg)
% [v0,v1]=like1(T,P,Wpos,Wneg)
% dw > 0 if fire detected, <0 if not, 0 if cloud
% T = time since fire arrival (days)
% Peak = length of time fire peaks after arrival
% Wpos = length of time fire is observable at 50% after peak
% Wneg = length of time fire is observable at 50% before peak

% map linearly [Peak,Peak+Wpos] -> [0,1] and [Peak-Wneg,Peak] -> [1,0]
% g(0)=1, g'(0)=1, g(1)=0.5, g decreasing in [0,infty), not too fast
a=5;
g0 = @(x) 4*(a*x+1)./(a*x+2).^2;
% g1 = @(x) -(4*a^2*x)./(a*x + 2).^3;
% replace g' by a function f(0)=0  f(1)=-0.5 f(large)=-1 if detected
g1y = @(x) -x.*x./(1+x.*x);
% replace g' by a function f(0)=0  f(1)=-0.5 f(2)=0 for T>Peak if
% fire is not detected
g1n = @(x) -x.*x.*(1-0.25*x.*x).*(x<2);


Tpos=max(0,(T-Peak-Wpos)./Wpos);  % max to avoid division by zero in g by chance
% Tneg=max(0,(Peak-T)./Wneg);  % burning already
Tneg=max(0,(Peak-T)./Wneg);  % constant in [Peak-Wneg,Peak]

v0 = g0(Tpos).*(T>Peak) + g0(Tneg).*(T<=Peak);
v0 =  v0.*dw.*(dw>0)+(-dw+v0.*dw).*(dw<0);
v1 = (dw>0).*(g1y(Tpos).*(T>Peak) - g1y(Tneg).*(T<=Peak)) +...
     (dw<0).*(g1n(Tpos).*(T>Peak) - g1n(Tneg).*(T<=Peak));
v1 = v1.*dw;
end        

    
    
    
