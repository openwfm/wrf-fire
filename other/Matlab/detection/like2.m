function [v0,v1]=like2(dw,t,stretch)

one=ones(size(t));
max_like=0.90;

% likelihood for dw=1 and dw=-1
[p0,p1]=like1(one,t,stretch);
p0=p0+log(max_like);
n0=log(1-exp(p0));
n1=-p1.*exp(p0)./(1-exp(p0)).^2;

v0 = dw.*p0.*(dw>0) - dw.*n0.*(dw<0);
v1 = dw.*p1.*(dw>0) - dw.*n1.*(dw<0);

% p0 + p1 = 1
% p0 = 1 - p1
% logp0 = log(1 - p1) = log(1 -exp(logp0))
    
end        
    
    
