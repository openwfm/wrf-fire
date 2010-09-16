function ros=fire_ros(fuel,speed,tanphi)

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
fuelmc_g=fuel.fuelmc_g;           % FUEL PARTICLE (SURFACE) MOISTURE CONTENT
fuelmc_c=fuel.fuelmc_c;           % FUEL PARTICLE (CANOPY) MOISTURE CONTENT

% computations from CAWFE code: wf2_janice/fire_startup.m4 

bmst     = fuelmc_g/(1+fuelmc_g);
fuelheat = cmbcnst * 4.30e-04;             % convert J/kg to BTU/lb
fci      = (1.+fuelmc_c)*fci_d;
fuelloadm= (1.-bmst) * fgi;                % fuelload without moisture
fuelload = fuelloadm * (.3048)^2 * 2.205;  % to lb/ft^2
fueldepth= fueldepthm/0.3048;              % to ft
betafl   = fuelload/(fueldepth * fueldens);% packing ratio
betaop   = 3.348 * savr^(-0.8189);       % optimum packing ratio
qig      = 250. + 1116.*fuelmc_g;          % heat of preignition, btu/lb
epsilon  = exp(-138./savr );               % effective heating number
rhob     = fuelload/fueldepth;             % ovendry bulk density, lb/ft^3
c        = 7.47 * exp(-0.133 * savr^0.55); % const in wind coef
bbb      = 0.02526 * savr^0.54;            % const in wind coef
c        = c * windrf^bbb;                 % jm: wind reduction from 20ft per Baughman&Rothermel(1980)
e        = 0.715 * exp( -3.59e-4 * savr);  % const in wind coef
phiwc    = c * (betafl/betaop)^(-e); 
rtemp2   = savr^1.5;
gammax   = rtemp2/(495. + 0.0594*rtemp2);  % maximum rxn vel, 1/min
a        = 1./(4.774 * savr^0.1 - 7.27);   % coef for optimum rxn vel
ratio    = betafl/betaop;   
gamma    = gammax*(ratio^a)*exp(a*(1.-ratio)); % optimum rxn vel, 1/min
wn       = fuelload/(1 + st);              % net fuel loading, lb/ft^2
rtemp1   = fuelmc_g/fuelmce;
etam     = 1.-2.59*rtemp1 +5.11*rtemp1^2 -3.52*rtemp1^3;  % moist damp coef
etas     = 0.174* se^(-0.19);              % mineral damping coef
ir       = gamma * wn * fuelheat * etam * etas; % rxn intensity,btu/ft^2 min
irm      = ir * 1055./( 0.3048^2 * 60.) * 1.e-6;% for mw/m^2 (set but not used)
xifr     = exp( (0.792 + 0.681*savr^0.5)...
            * (betafl+0.1)) /(192. + 0.2595*savr); % propagating flux ratio
%        ... r_0 is the spread rate for a fire on flat ground with no wind.
r_0      = ir*xifr/(rhob * epsilon *qig);  % default spread rate in ft/min

% computations from CAWFE code: wf2_janice/fire_ros.m4 

if ~ichap,
    %       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
    spdms = max(speed,0.);
    umidm = min(spdms,30.);                    % max input wind spd is 30 m/s   !param!
    umid = umidm * 196.850;                    % m/s to ft/min
    %  eqn.: phiw = c * umid**bbb * (betafl/betaop)**(-e) ! wind coef
    phiw = umid^bbb * phiwc;                   % wind coef
    phis = 5.275 * betafl^(-0.3) *max(0,tanphi)^2;   % slope factor
    ros = r_0*(1. + phiw + phis)  * .00508; % spread rate, m/s
else  % chapparal
    %        .... spread rate has no dependency on fuel character, only windspeed.
    spdms = max(speed,0.);
    ros = max(.03333,1.2974 * spdms^1.41);       % spread rate, m/s
end
ros=min(ros,6);
end
