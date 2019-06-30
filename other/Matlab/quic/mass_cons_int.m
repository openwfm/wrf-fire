function u=mass_cons_int(u0,h,w,check)
% mass consistent approximation
% given
%   u0  3 wind vectors on staggered grids
%   h   step, vector length 3
%   w   weights, vector length 3

% solve  
% min_u 1/2 <D(u-u0),u-u0> s.t. div u = 0, D=diag[w(1)I,w(2)I,w(3)I]
% <=> min_u max_lambda 1/2 sum_i  <D(u-u0),u-u0> + <lambda,div u> 
% (using <lambda,div u> = - <grad lambda, u>) 
% <=> D(u-u0) - grad lambda = 0, div u = 0
% <=> inv(D) grad lambda = u - u0, div u = 0
% <=> div inv(D) grad lambda = div u - div u0
% <=>
% ______________________________________________________
%    - div inv(D) grad lambda = div u0,
%     u = u0 + inv(D) grad lambda
% with boundary condidions lambda=0 on the sides and top
% and dlambda/dz = 0 on the bottom
%-------------------------------------------------------

if ~exist('check','var')
    check = false;
end
tstart=tic;
% divergence of u0
f = div3(u0,h); 
% reflect the right hand side abound the bottom in 3rd coordinate
n = size(f);
fprintf('mass_cons_int mesh size %i %i %i\n',n)
r = zeros(n(1),n(2),2*n(3));
r(:,:,1:n(3))=f(:,:,n(3):-1:1);
r(:,:,n(3)+1:2*n(3))=f;
% solve with zero boundary conditions
lambda=poisson_fft3z(r,h,1./w);
% check symmetry about the bottom
if check, err_sym = big(lambda(:,:,n(3):-1:1)-lambda(:,:,n(3)+1:2*n(3))), end
% extract the upper half of the solution
lambda = lambda(:,:,n(3)+1:2*n(3));
% grad with reflection about the bottom
g = grad3z(lambda,h,true);
% update u
for i=1:3
    u{i} = u0{i} + (1/w(i))*g{i};
end
% check divergence
if check, err_div = big(div3(u,h)), end
% check no correction in vertical speed at the bottom
if check, err_corr_w_bottom = big(u{3}(:,:,1)-u0{3}(:,:,1)), end
fprintf('mass_cons_int %g seconds\n',toc(tstart))
end
