function [ detection_probs ] = make_detection_probabilities( heat_matrix )
% [ detection_probs ] = make_detection_probabilities( heat_matrix )
% Function returns a matrix with probaility of satellite detections at each
% pixel
%
%Inputs: Heat_matrix - matrix containing heat fluxes at each pixel
%Output: detection_probs - matrix with proability of satellite detection at
%each pixel

[m,n] = size(heat_matrix);
detection_probs = zeros(m,n);

for i = 1:m
    for j = 1:n
        dp =  detection_probability(heat_matrix(i,j));
        detection_probs(i,j) = dp;
    end
end


end

