function [ heat_map ] = make_heats( tign , t_now )
% [ heat_map ] = make_heats( tign )
% function returns an array of heat output in pixels
%inputs
%   tign : matrix with fire arrival times for a simulation
%   t_now : present time, (time of satellite imaging)
%output
%   heap_map : matrix with heat in each pixel, based on exponential decay
%       with time since fire arrival

[m n] = size(tign);
heat_map = zeros(m,n);

%const
T_0 = 1;
%decay = -0.3 --> 1.1% of heat output at 15 hours
decay = -0.3; % ?? -->  %set to10 for patch  ,0.02 for hill2 test
for ii = 1:m
    for jj = 1:n
        if tign(ii,jj) < t_now
            heat_map(ii,jj) = 0;
        else
            heat_map(ii,jj) = T_0*exp((tign(ii,jj)-t_now)*decay);
        end
    end
end

%figure
%contour(heat_map)

end

