function [ rate ] = avg_rate( ros )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

rate = zeros(3);

rate(1,1) = sum(sum(ros.f_ros11));
rate(1,2) = sum(sum(ros.f_ros12));
rate(1,3) = sum(sum(ros.f_ros13));
rate(2,1) = sum(sum(ros.f_ros21));
rate(2,3) = sum(sum(ros.f_ros23));
rate(3,1) = sum(sum(ros.f_ros31));
rate(3,2) = sum(sum(ros.f_ros32));
rate(3,3) = sum(sum(ros.f_ros33));

% avegare the values in the ros matrices
[m,n] = size(ros.f_ros11);
rate = rate*(1/m/n);

%scale so the largest value is normalized
l = max(max(rate));
rate = 1/l*rate;


end

