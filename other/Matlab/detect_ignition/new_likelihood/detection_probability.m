function [ detect_prob ] = detection_probability(tign)
%[ detect_prob ] = detection_probability(pixel_heat)
%returns the probability of detection based on logistic curve of validation
%   studies
%inputs
%   tign : time since fire arrival
%       
%outputs
%   detect_prob : proability of satellite detection at that pixel
detect_prob = zeros(size(tign));

% use exponential increase in heat
heat_up = 1;

%for new burn model, set to zero for old model
% length of time heat is maximum, constant
const_time = 10;

%needed for computing with time instead of heat
decay = 0.3;
heat = zeros(size(tign));
m1 = tign < 0;
m2 = tign >= 0;
m3 = tign >= const_time;


if heat_up == 1
    heat(m1) = exp(10*decay*tign(m1));
    heat(m2) = 1;
    tign_shift = tign-const_time;
    heat(m3) = exp(-decay*tign_shift(m3));
else 
    heat(m2) = exp(-decay*tign(m2));
end

%figure,mesh(heat)


% constants 
%a = 100;  %controls shape of curve  20 for patch
%b = 2.2; %controls false positive rate  2.2 for patch

%can comput false pos rate as 
false_rate = 0.0002;
b = log(false_rate/(1-false_rate));

%can compute a as
p = 0.30; % percent detection prob at time t
t = 24; % hours since fire arrival
h_t = exp(-decay*t);
a = (log(p/(1-p))-b)/h_t;



if heat_up == 1
    % for exponential heat-up...
    detect_prob = 1./(1+exp(-a*heat-b));
else
    %without exponential heat-up
    m1 = tign < 0;
    m2 = tign >= 0;
    detect_prob(m1) = 1./(1 + exp(-b));
    detect_prob(m2) = 1./(1 + exp(-a*heat(m2) - b));
end




end

