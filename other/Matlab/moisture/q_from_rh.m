function q = q_from_rh( rh, P, T)
% in:
%  rh - relative humidity [1]
%   P - atmospheric pressure [pa]
%   T - atmospheric temperature [K]
% out:
%   q - water vapor mixing ratio [kg/kg]

epsilon = 0.622; % Molecular weight of water (18.02 g/mol) to molecular weight of dry air (28.97 g/mol)

% saturation vapor pressure [Pa]
Pws= exp( 54.842763 - 6763.22/T - 4.210 * log(T) + 0.000367*T + ...
    tanh(0.0415*(T - 218.8)) * (53.878 - 1331.22/T - 9.44523 * log(T) + 0.014025*T)); 

Pw = rh*Pws; % vapor pressure [Pa]

% solve for q from
% Pw=q*P/(epsilon+(1-epsilon)*q); 

rPw = Pw/P; % relative vapor pressure

% rPw = q*/(epsilon+(1-epsilon)*q)
% rPw*epsilon+rPw*(1-epsilon)*q = q
% rPw*epsilon = -rPw*(1-epsilon)*q + q

q = rPw*epsilon/(1-rPw*(1-epsilon)); 
