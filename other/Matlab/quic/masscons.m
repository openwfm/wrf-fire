function masscons
% mass consistent approximation
% given
%      lambda0
%      u0        3 wind vectors on staggered grids

% solve  
% min_u 1/2 <D(u-u0),u-u0> s.t. div u = 0
% <=>
% min_u max_lambda 1/2 sum_i  <D(u-u0),u-u0> + <lambda,div u> 
% <=>     (using <lambda,div u> = - <grad lambda, u>) 
%    D(u-u0) - grad lambda = 0
%    div u                 = 0
% <=> 
%    u = u0 + inv(D) grad lambda
%    div u = div u0 + div inv(D) grad lambda = 0
