function [v0,v1]=like1(T,Peak,Wpos,Wneg)
% [v0,v1]=like1(T,P,Wpos,Wneg)
% T = time since fire arrival (days)
% P = length of time fire peaks after arrival
% Wpos = length of time fire is observable at 50% after peak
% Wneg = length of time fire is observable at 50% before peak

% map [P,P+Wpos] -> [1,2] and [P-Wneg,P] -> [1,2]
% g(0)=1, g'(0)=1, g(1)=0.5, g decreasing in [0,infty), not too fast
a=5;
g0 = @(x) 4*(a*x+1)./(a*x+2).^2;
% g1 = @(x) -(4*a^2*x)./(a*x + 2).^3;
% replace g' by a function f(0)=0  f(1)=-0.5 f(large)=-1 for T>Peak
g1 = @(x) -x.*x./(1+x.*x);


Tpos=max(0,(T-Peak-Wpos)./Wpos);  % max to avoid division by zero in g by chance
% Tneg=max(0,(Peak-T)./Wneg);  % burning already
Tneg=max(0,(Peak-T)./Wneg);  % constant in [Peak-Wneg,Peak]

v0 = g0(Tpos).*(T>Peak) + g0(Tneg).*(T<=Peak);
v1 = g1(Tpos).*(T>Peak) - g1(Tneg).*(T<=Peak);
end    
    

    
    
    
