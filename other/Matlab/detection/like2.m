function [v0,v1]=like2(dw,t,stretch)
% [v0,v1]=like2(dw,t,stretch)
% input
%   dw > 0  confidence, positive detection
%   dw < 0  minus confidence, negative detection
%   t       time since fire arrival
%   stretch time scale (hours)
% output
%   v0      value of the data likelihood
%   v1      derivative dv0/dt

one=ones(size(t));

max_like=0.90;

% get detection likelihood
% p0 = log likelihood of detection, max=log(1)=0
% p1 = derivative
[p0,p1]=like1(one,t,stretch);

% scale so that max p0=log(max_like)
p0=p0+log(max_like);
% p1 does not change, derivative of constant is zero

% find likelihood of non-detection from
% probability(yes) + probability(no)=1
% exp(p0) + exp(n0)=1
n0=log(1-exp(p0));
% d(n0)/dt = d(log(1-exp(p0)))/dt  
n1=-p1.*exp(p0)./(1-exp(p0));

v0 = dw.*p0.*(dw>0) - dw.*n0.*(dw<0);
v1 = dw.*p1.*(dw>0) - dw.*n1.*(dw<0);
    
end        
    
    
