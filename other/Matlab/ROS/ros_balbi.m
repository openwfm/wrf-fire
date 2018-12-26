function ros = ros_balbi(fuel,speed,tanphi,fmc_g)
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
g = 9.81;                        % Gravitational acceleration [m/s2]
Chi_0 = 0.3;                     % Radiative factor
st = 17;                         % Stochiometric coefficient
Chi_0 = 0.3;                     % Thin Flame Radiant Fraction - ?Balbi 2009?
B = 5.67e-8;                     % Stefan-Boltzman [W/m2/K4)
deltah_w = 2.257e6;              % Water Evap Enthalpy [J/kg]
tau_0 = 75591;                   % Anderson's residence time coefficient
T_a = 293.15;                    % air temperature [K]
T_i = 600;                       % ignition temperature [K]
% Fuel Constants
C_p = 2e3;                      % Specific heat of fuel [J/kg/K] - Balbi 2009
C_pa = 1150;                    % Specific heat of air [J/Kg/K]
m = fuelmc_g;                   % FUEL PARTICLE MOISTURE CONTENT as fraction (0 - 1) [-]
rho_v = fueldens*16.0185;       % FUEL Particle Density [Kg/m^3]
e = fueldepthm                  % Fuel thickness (depth) (m)
DeltaH = cmbcnst;               % Combustion Enthalpy [J/kg]
sigma = fgi;                    % Dead fuel load [kg/m2]
sigma_t = fgi;                  % Total fuel load [kg/m2]
rho = 1500;                      % Fuel density [kg/m3]
rho_a = 1.125;                  % Air density [kg/m3]
rho_flame = 0.25;                            % Gas Flame Density [kg/m^3]
beta    = sigma/(e * rho);                   % packing ratio of the dead fuel (eq. 1)
beta_t  = sigma_t/(e * rho);                 % total packing ratio (eq. 2)
s = savr/0.3048;                             % surface area to volume ratio 1/m converted form 1/ft
LAI = (s * e * beta) / 2;            % Leaf area index for dead fuel (eq. 3)
LAI_t= (s * e * beta_t) / 2;         % Total fuel leaf area index (eq. 4)
nu = min(2*LAI,2*pi*beta/beta_t);            % Absorption coeffcient (eq. 5) 
lv =   e;                                    % fuel length (m)
%Model parameters
K1 = 100;                                    % 100 for field, 1400 for the lab
r_00 = 2.5e-5;                               % Model parameter

alpha = atan(tanphi)           % Slope angle [rad]
U = speed                      % winds speed normal to the fire front line (m/s)
simple_radiation = 0

tol_R = 1e-5;                   % tolerance to compute R
maxit_R = 20;                   % max iterations to compute R

% compute drag force coefficient (eq. 7)
K_drag = K1 * beta_t * min(e/lv,1)

% compute activation energy (eq. 14)
q = C_p * (T_i - T_a) + m * deltah_w

% compute radiant coeffcient (eq. 13)
A = min (s/(2*pi), beta/beta_t) * Chi_0 * DeltaH / (4 * q)

% as a first guess take Rothermell ROS
R_1st_guess = ros_rothermel(fuel,speed,tanphi,fmc_g)
R = R_1st_guess
R_old = R;
gamma = alpha         % first guess no extra tilt

for i=1:maxit_R
    
    i  % iteration
    
    % compute radiative fraction (eq. 20)
    if (simple_radiation) == 1
        Chi = Chi_0 % start from the initial guess
    else
        Chi = Chi_0/(1 + R * cos(gamma) / (s * r_00)) % compute radiative fraction from rate of spread and gamma which are unkown...
    end

    % compute flame temperature (eq. 16)
    T_f = T_a + DeltaH * (1-Chi) / ((st+1) * C_pa)

    % compute upward gas velocity (eq. 19)
    u_0 = 2*nu * ((st+1)/tau_0) * (rho/rho_a)  * (T_f/T_a)

    % compute flame tilt angle (eq. 15)
    gamma = atan(tan(alpha) + U / u_0)

    % compute flame height (eq. 17)
    H_f = (u_0)*(u_0) / (g * (T_f/T_a -1) * (cos(alpha))^2)

    % compute convective coefficient (eq. 8)
    b = 1 / (q * tau_0 * u_0 * beta_t) * deltah_w * nu * min( st/30,1)

    % compute rate of spread 
    % rate of spread from base radiation
    R_b = min (s * e *  beta_t/pi , 1) * (beta/beta_t)^2 * (B*T_f^4)/(beta * rho * q)

    % compute rate of spread due to flame radiation (eq. 11)
    R_f = A * R * (1 + sin(gamma) - cos(gamma)) / (1 + R * cos(gamma) / (s * r_00))

    % compute rate of spread due to convection
    R_c = b * (tan(alpha) + 2*U/u_0 * exp (-K_drag * R))

    % compute the total rate of spread
    R = R_b + R_f + R_c
    
    fprintf('iteration %i R=%g change=%g\n', i, R, R-R_old)
    if abs(R-R_old) < tol_R
        break
    end
    
    R_old = R;
end
if abs(R-R_old) > tol_R
    warning('iterations to compute R did not converge to given tolerance')
end
ros=R;
end


