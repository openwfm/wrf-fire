function [ detect_prob ] = detection_probability(pixel_heat)
%[ detect_prob ] = detection_probability(pixel_heat)
%returns the probability of detection based on logistic curve of validation
%   studies
%inputs
%   pixel_heat : estimated heat from a pixel
%outputs
%   detect_prob : proability of satellite detection at that pixel

% constants 
a = 30;  %controls shape of curve  20 for patch
b = 2.2; %controls false positive rate  2.2 for patch

detect_prob = (1 + exp(-a*pixel_heat + b))^(-1);


end

