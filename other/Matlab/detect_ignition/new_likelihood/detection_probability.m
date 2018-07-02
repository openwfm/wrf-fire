function [ detect_prob ] = detection_probability(pixel_heat)
%[ detect_prob ] = detection_probability(pixel_heat)
%returns the probability of detection based on logistic curve of validation
%   studies
%inputs
%   pixel_heat : estimated heat from a pixel
%outputs
%   detect_prob : proability of satellite detection at that pixel

% constants 
a = 20;  %controls shape of curve
b = 2.2; %controls false positive rate

detect_prob = (1 + exp(-a*pixel_heat + b))^(-1);


end

