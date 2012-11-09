function [E_CO2, E_CO, E_CH4, E_PM25] = FEMIS(fgi, burn_rate, U_F, V_F, RH4, mode)
%FEMIS function computing emission rates of CO2, CO, CH4 and particlate matter
%based on:
% fgi - fuel load [kg/m2]
% burn_rate [kg/s] - mass of the fuel burnt in unit time.
% U_F, V_F [m/s]  mid-flame wind speed components (used for computation of the WS_F)
% RH4 [%] -relative humidity 4 hours before (?)
% mode - 0 - default, computes consumption rates according to FEMIS
%           allowing for the total consumption greater than the combustion rate
%      - 1 - limits maximum consumption rate to the actual burn rate
%      - 2 - computes emissions straight from burnt rate ignoring smoldering adjustment 

%all the constants and equations come from the following US Forest Service
%technical report: Fire Emission Production Simulator (FEPS) User's Guide
%Version 1.0, Gary K. Anderson, David V. Sandberg and Robert A. Norheim
%January 2004
%available from: www.fs.fed.us/pnw/fera/feps/FEPS_users_guide.pdf

%Adam Kochanski 05.26.2012 

%CONSTANTS

%emission factor constants according to FEPS user's manual p. 63. [g/kg]
Kef_CO2=0;
Kef_CO=961;
Kef_CH4=42.7;
Kef_PM25=67.4;

%emission factor coefficients according to FEPS user's manual p. 63. [g/kg]
EF_CO2=1833;
EF_CO=984;
EF_CH4=43.2;
EF_PM25=66.8;

%flam involvment sensitivity coeffcient (eq 5, p. 53)
k_agi =1.0;

% combustion effciency factors according to FEPS user's manual p. 62 [-]:
k_f=0.9;     %flame phase (f)
k_sts=0.76;  %short-term smoldering (sts)
k_lts=0.76;  %long-term smoldering (lts)

%flaming phase consumption coeffcients from to FEPS user's manual p. 53 [-]
k_cag=0.5;
k_cbg=0.2; % not used since Cbg=0

% Duff bulk density [tons/acre-inch] p. 54
B_f=20;
% STS depth sensitivity to consumption [tons/acre-inch] p.55
B_sts=12;

%flaming phase resindence time coeficients  (eq. 8, p. 54)
k_tflam1= 4/3;
k_tflam2= 8.0;

%short-term moldering residence time coeffcients (eq.13 p. 55)
k_edr1=8/3;
k_edr2=0.5;

%flame phase diffusivity dependency
N_tflam=0.5;

%short-term smoldering diffusivity dependency
N_edr=0.5;

%consumption threshold for flame involvment [tons/acre], 
% 1 t/acre = 0.224 kg/m2
Cti=10;          % [tons/acre]
Cti=Cti * 0.224; % [kg/m2] 

%wind speed benchmark for smoldering 1 mph = 0.44704 m/s
u_b= 3; %[mph]
u_b= u_b * 0.44704; % [m/s]

%benchmark relative humidity deficit [%]
RHb=60.0; 

%Molecular weights [g/mol]
MW_CO2=44.0; 
MW_CO=28.0;
MW_CH4=16.0;

%COMPUTATIONS
    
    % by default computation from FEPS allowing greater and smaller consumption than the burn_rate
    
    %==========================================================================
    % computing flaming phase consumption, assuming no below ground consumption
    % Cbg=0; and the above ground consumption (canopy, shrub, grass, wood
    % litter) Cag equal to fuel combustion rate. eq. 6 p. 53

    % flaming phase involvment
    % flaming phase involvment
    Cag=fgi; % we assume above the ground consuption equal to the fuel load [kg/m2]
    %Cbg=0;   % we assume no below ground consumption

    Inv_f=100*(1-k_agi*exp(-Cag/Cti))

    %==========================================================================
    % computing short-term smoldering phase consumption [kg/m2]
    Inv_sts=Inv_f

    % computing flaming phase consumption rate [kg/s]
    CR_f= burn_rate * Inv_f/100 * k_cag 

    % computing smoldering adjustment from eq. 26, p 59
    WS_F = sqrt (U_F^2 + V_F^2); % wind speed at midflame height [m/s]
    %U_F midflame X wind speed component
    %V_F midflame Y wind speed component
    SA=sqrt(WS_F/u_b) * ((100-RH4)/RHb) 

    %computing sort-term smoldering consumption rate [kg/s]
    CR_sts= SA* burn_rate * Inv_sts/100 * k_cag

    % we assume no long term smoldering (>2h) since in most cases withing
    % 2h all fuel within a grid cell will burn, and we have no data about the
    % duff layer
    CR_lts = 0;

    %computing total consumption rate according for FEPS (eq. 30, p.61):
    CR_total=CR_f + CR_sts + CR_lts; %[kg/s]

    if (mode==1)  % limit the consumption rate to the burn rate
        if (CR_total>burn_rate) 
            CR_total=burn_rate;
        end
    elseif (mode==2) % compute the consumption rate straight from burn rate
            CR_total=burn_rate;
    end
CR_total

%computing combustion efficiency:
CEff=(k_f*CR_f + k_sts*CR_sts + k_lts*CR_lts)/(CR_f + CR_sts + CR_lts + 1)

ER_CO2= (Kef_CO2 - EF_CO2*CEff)*CR_total/1000 %[kg/s]
ER_CO=(Kef_CO - EF_CO*CEff)*CR_total/1000 %[kg/s]
ER_CH4=(Kef_CH4 - EF_CH4*CEff)*CR_total/1000 %[kg/s]
ER_PM25=(Kef_PM25 - EF_PM25*CEff)*CR_total/1000 %[kg/s]

E_C02 = ER_CO2/(MW_CO2*3600) % CO2 emissions in mol/hr
E_C0  = ER_CO /(MW_CO *3600) % CO emissions in mol/hr
E_CH4 = ER_CH4/(MW_CH4*3600) % emissions in mol/hr
E_PM25 = ER_PM25*1000 % PM25 emissions in ug/s
end

