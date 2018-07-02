function [ tign ] = make_times( ig_x, ig_y, ig_time, domain_size )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

tign = zeros(domain_size,domain_size);

for i=1:domain_size
    for j = 1:domain_size
        tign(i,j) = sqrt((i-ig_x)^2+(j-ig_y)^2) + ig_time;
    end
end

        

end

