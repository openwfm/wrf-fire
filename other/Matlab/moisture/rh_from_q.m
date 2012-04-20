function  rh  = rh_from_q( q, P, T )
% in:
%   q - water vapor mixing ratio [kg/kg]
%   P - atmospheric pressure [pa]
%   T - atmospheric temperature [K]
% out:
%  rh - relative humidity [1]

epsilon = 0.622; % Molecular weight of water (18.02 g/mol) to molecular weight of dry air (28.97 g/mol)

% vapor pressure [Pa]
Pw=q*P/(epsilon+(1-epsilon)*q); 

% saturation vapor pressure [Pa]
Pws= exp( 54.842763 - 6763.22/T - 4.210 * log(T) + 0.000367*T + ...
    tanh(0.0415*(T - 218.8)) * (53.878 - 1331.22/T - 9.44523 * log(T) + 0.014025*T)); 

%realtive humidity [1]
rh = Pw/Pws ;

end

