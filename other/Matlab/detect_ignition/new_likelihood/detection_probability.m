function [ detect_prob ] = detection_probability(pixel_heat)
%[ detect_prob ] = detection_probability(pixel_heat)
%returns the probability of detection based on logistic curve of validation
%   studies
%inputs
%   pixel_heat : estimated heat from a pixel
%        changing this to time in hours since fire arrival
%outputs
%   detect_prob : proability of satellite detection at that pixel
detect_prob = zeros(size(pixel_heat));

% use exponential increase in heat
heat_up = 0;

%needed for computing with time instead of heat
decay = 0.3;
heat = zeros(size(pixel_heat));
m1 = pixel_heat < 0;
m2 = pixel_heat >= 0;
if heat_up == 0
    heat(m1) = exp(10*decay*pixel_heat(m1));
end
heat(m2) = exp(-decay*pixel_heat(m2));
%figure,mesh(heat)


% constants 
%a = 100;  %controls shape of curve  20 for patch
%b = 2.2; %controls false positive rate  2.2 for patch

%can comput false pos rate as 
false_rate = 0.02;
b = log(false_rate/(1-false_rate));

%can compute a as
p = 0.30; % percent detection prob at time t
t = 30; % hours since fire arrival
h_t = exp(-decay*t);
a = (log(p/(1-p))-b)/h_t;



if heat_up == 1
    % for exponential heat-up...
    detect_prob = 1./(1+exp(-a*heat-b));
else
    %without exponential heat-up
    m1 = pixel_heat < 0;
    m2 = pixel_heat >= 0;
    detect_prob(m1) = 1./(1 + exp(-b));
    detect_prob(m2) = 1./(1 + exp(-a*heat(m2) - b));
end




end

