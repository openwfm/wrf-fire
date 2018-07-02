function [ likelihood ] = compute_likelihood(heat,mask,radius,weight )
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

[m n] = size(heat);
log_sum = 0;

for i=1:m
    for j=1:n
        pd(i,j) = detection_probability(heat(i,j));
    end 
end




for i = radius+1:m-radius
    for j = radius+1:m-radius
        pp = 1;
        if mask(i,j)>0
            pp = compute_pixel_probability(i,j,heat,radius,weight,pd);
%         else if mod(i*j,25)==0
%                 %pp = 1;
%                 pp = 1 - compute_pixel_probability(i,j,heat,radius,weight,pd);
            end
            
            
            log_sum = log_sum + log(pp);
        end
    end

likelihood = log_sum;
    
    
end

