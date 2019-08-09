function u=masscons(u0,h,w)
% mass consistent approximation
% given
%   u0  3 wind vectors on staggered grids
%   h   step, vector length 3
%   w   weights, vector length 3

% solve  
% min_u 1/2 <D(u-u0),u-u0> s.t. div u = 0, D=diag[w(1)I,w(2)I,w(3)I]
% <=>
% min_u max_lambda 1/2 sum_i  <D(u-u0),u-u0> + <lambda,div u> 
% <=>     (using <lambda,div u> = - <grad lambda, u>) 
%    D(u-u0) - grad lambda = 0
%    div u                 = 0
% <=>  
%    - div inv(D) grad lambda = u0
%    u = u0 + inv(D) grad lambda
