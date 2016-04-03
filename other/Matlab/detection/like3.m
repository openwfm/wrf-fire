function [v0,v1]=like3(dw,t,stretch)

% stretch: 
Tmin=stretch(1);Tmax=stretch(2);Tneg=stretch(3);Tpos=stretch(4);

max_like=log(0.9);

% get p0 = data likelhood of 1 as a function of t, p1=derivative

slope_neg=0.5;
slope_pos=0.1;

p0=max_like*(t>=Tmin & t<=Tmax) + ...
   (max_like+(t-Tmin)*slope_neg).*(t<Tmin) + ...
   (max_like+(Tmax-t)*slope_pos).*(t>Tmax); 

p1= (t<Tmin)*slope_neg - (t>Tmax)*slope_pos; 

% everything else is derived from that

% p0 + p1 = 1
% p0 = 1 - p1
% logp0 = log(1 - p1) = log(1 -exp(logp0))

n0=log(1-exp(p0));
n1=-p1.*exp(p0)./(1-exp(p0)).^2;

v0 = dw.*p0.*(dw>0) - dw.*n0.*(dw<0);
v1 = dw.*p1.*(dw>0) - dw.*n1.*(dw<0);

    
end        
    
    
