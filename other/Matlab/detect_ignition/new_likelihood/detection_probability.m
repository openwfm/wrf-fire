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


%needed for computing with time instead of heat
decay = 0.3;
heat = zeros(size(pixel_heat));
m1 = pixel_heat < 0;
m2 = pixel_heat >= 0;
heat(m1) = exp(10*decay*pixel_heat(m1));
heat(m2) = exp(-decay*pixel_heat(m2));
%figure,mesh(heat)


% constants 
a = 100;  %controls shape of curve  20 for patch
%b = 2.2; %controls false positive rate  2.2 for patch

%can comput false pos rate as 
false_rate = 0.01;
b = log(false_rate/(1-false_rate));

%can compute a as
p = 0.95; % 95% detection prob at time t
t = 10; %fifteen hours since fire arrival
h_t = exp(-0.3*t);
a = (log(p/(1-p))-b)/h_t;

heat_up = 1;

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

