function ros = fire_ros_balbi(fuel,speed,tanphi,fmc_g)
% in
%       fuel    fuel description structure
%       speed   wind speed
%       tanphi  slope
%       fmc_g   optional, overrides fuelmc_g from the fuel description
% out
%       ros     rate of spread

% given fuel params

windrf=fuel.windrf;               % WIND REDUCTION FACTOR
fgi=fuel.fgi;                     % INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
fueldepthm=fuel.fueldepthm;       % FUEL DEPTH (M)
savr=fuel.savr;                   % FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
fuelmce=fuel.fuelmce;             % MOISTURE CONTENT OF EXTINCTION
fueldens=fuel.fueldens;           % OVENDRY PARTICLE DENSITY, LB/FT^3
st=fuel.st;                       % FUEL PARTICLE TOTAL MINERAL CONTENT
se=fuel.se;                       % FUEL PARTICLE EFFECTIVE MINERAL CONTENT
weight=fuel.weight;               % WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
fci_d=fuel.fci_d;                 % INITIAL DRY MASS OF CANOPY FUEL
fct=fuel.fct;                     % BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
ichap=fuel.ichap;                 % 1 if chaparral, 0 if not
fci=fuel.fci;                     % INITIAL TOTAL MASS OF CANOPY FUEL
fcbr=fuel.fcbr;                   % FUEL CANOPY BURN RATE (KG/M**2/S)
hfgl=fuel.hfgl;                   % SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)
cmbcnst=fuel.cmbcnst;             % JOULES PER KG OF DRY FUEL
fuelheat=fuel.fuelheat;           % FUEL PARTICLE LOW HEAT CONTENT, BTU/LB
fuelmc_g=fuel.fuelmc_g;           % FUEL PARTICLE (SURFACE) MOISTURE CONTENT, jm: 1 by weight?
fuelmc_c=fuel.fuelmc_c;           % FUEL PARTICLE (CANOPY) MOISTURE CONTENT, 1

if exist('fmc_g','var') % override moisture content by given
    fuelmc_g = fmc_g;
end

% Universal constants
s = 9;                          % Stoichiometric constant - Balbi 2009
Chi_0 = 0.3;                    % Thin Flame Radiant Fraction - ?Balbi 2009?
a = 0.05;                       % Constant from Balbi 2008
A_0 = 2.25;                     % Constant from Balbi 2008
eps = 0.2;                      % Pastor 2002
B = 5.67e-8;                    % Stefan-Boltzman 
Deltah_v = 2.257e6;             % Water Evap Enthalpy [J/kg]
C_p = 2e3;                      % Calorific Capacity [J/kg] - Balbi 2009

% Fuel Constants
m = fuelmc_g*100;               % FUEL PARTICLE MOISTURE CONTENT [%]
rho_v = fueldens*16.0185;       % FUEL Particle Density [Kg/m^3]
DeltaH = cmbcnst;               % Combustion Enthalpy [J/kg]
sigma = fgi;
e_delta = (savr/0.3048)*sigma/(4*rho_v);
rho = 0.25;                     % Gas Flame Density [kg/m^3]

% Calculate R_0
ii = 1;
% USE Roethermel Model for no slope, no wind ros
if ii == 1
    bmst     = fuelmc_g/(1+fuelmc_g);          % jm: 1 
    fuelloadm= (1.-bmst) * fgi;                % fuelload without moisture
                                           % jm: 1.-bmst = 1/(1+fuelmc_g) so fgi includes moisture? 
    fuelload = fuelloadm * (.3048)^2 * 2.205;  % to lb/ft^2
    fueldepth= fueldepthm/0.3048;              % to ft
    betafl   = fuelload/(fueldepth * fueldens);% packing ratio  jm: lb/ft^2/(ft * lb*ft^3) = 1
    betaop   = 3.348 * savr^(-0.8189);         % optimum packing ratio jm: units?? 
    qig      = 250. + 1116.*fuelmc_g;          % heat of preignition, btu/lb
    epsilon  = exp(-138./savr );               % effective heating number
    rhob     = fuelload/fueldepth;             % ovendry bulk density, lb/ft^3
    rtemp2   = savr^1.5;
    gammax   = rtemp2/(495. + 0.0594*rtemp2);  % maximum rxn vel, 1/min
    ar       = 1./(4.774 * savr^0.1 - 7.27);   % coef for optimum rxn vel
    ratio    = betafl/betaop;   
    gamma    = gammax*(ratio^ar)*exp(ar*(1.-ratio)); % optimum rxn vel, 1/min
    wn       = fuelload/(1 + st);              % net fuel loading, lb/ft^2
    rtemp1   = fuelmc_g/fuelmce;
    etam     = 1.-2.59*rtemp1 +5.11*rtemp1^2 -3.52*rtemp1^3;  % moist damp coef
    etas     = 0.174* se^(-0.19);              % mineral damping coef
    ir =    gamma * wn * fuelheat * etam * etas; % rxn intensity,btu/ft^2 min
    xifr =  exp( (0.792 + 0.681*savr^0.5)...
            * (betafl+0.1)) /(192. + 0.2595*savr); % propagating flux ratio
    roth_r_0 = ir*xifr/(rhob * epsilon *qig);  % default spread rate in ft/min
    R_0 = roth_r_0 * .00508;                   % default spread rate in m/s
else
    % Calculate R_00 from flame temp
    %R_00 = eps*B*a*T^4/(2*Deltah_v);
    % Use R_00 = 0.05 from Balbi 2009
    R_00 = .05;
    R_0 = fueldepthm*R_00/(sigma*(1+a*m));
end

% Calculate u_0
u_00 = 2*(s+1)/rho;
tau = fct/30;                      % Burnout time from Rothermel fuel model/3 [s]
u_0 = u_00*sigma/tau;

% Calculate A
nu = min(e_delta,1);
A = nu*A_0/(1+a*m);

% Calculate flame tilt angle (gamma)
alpha = atan(tanphi);           % Slope angle [rad]
psi = 0;                        % Angle between wind and flame; assume parallel currently
phi = 0;                        % Angle between flame front vector and slope vector
gamma = atan(tan(alpha)*cos(phi)+speed*cos(psi)/u_0);

if(gamma <= 0)
    R = R_0;
else
    % Solve for ros quadratically, eq(13b) Balbi 2009
    qa = cos(gamma)/(12*R_0);
    qb = 1 - cos(gamma)/12 - A*(1 + sin(gamma) - cos(gamma));
    qc = -1;
    R1 = (-qb+sqrt(qb*qb-(4*qa*qc)))/(2*qa);
    R2 = (-qb-sqrt(qb*qb-(4*qa*qc)))/(2*qa);
    R = max(R1,R2);
end
% Calculate Radiant Fraction
Chi = Chi_0/(1 + R*cos(gamma)/(12*R_0));
%Chi = Chi_0;

% Calculate Flame T
T_a = 300;                      % Air Temp [K]
T = T_a + (1-Chi)*DeltaH/((1+s)*C_p);

% Calculate flame height (H)
gstar = 9.81*(T/T_a - 1);
H = u_0*u_0/(gstar*cos(alpha)*cos(alpha));

% Calculate radiant flux (Q)
Q = (eps/2)*B*(T^4);

ros = min(R,6);

% Calculate depth
L = ros*tau;
end