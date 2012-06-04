function [d,w]=equilibrium_moisture(RH,T)
d=0.942*RH.^0.679 + 0.000499*exp(0.1*RH) + 0.18*(21.1+273.15-T).*(1-exp(-0.115*RH)); % equilibrium moisture for drying
w=0.618*RH.^0.753 + 0.000454*exp(0.1*RH) + 0.18*(21.1+273.15-T).*(1-exp(-0.115*RH)); % equilibrium moisture for wetting

