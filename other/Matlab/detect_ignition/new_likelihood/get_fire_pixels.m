function [ fire_pixels ] = get_fire_pixels( data )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

[m n] = size(data);
fire_pixels = zeros(m,n);

for ii = 1:m
    for jj = 1:n
        if data(ii,jj) >= 7
            fire_pixels(ii,jj) = data(ii,jj);
        end
    end
end

%contour(fire_pixels)

end

