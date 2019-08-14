function u=mass_cons_flux(U0,X,w)
% mass consistent flux approximation
% input:
%   U0  cell array, wind vectors u v w on staggered grids
%   X   cell array, nodal coordinatees
%   W   relative weights for u v w, vector length 3
% output:
%   U   cell array, wind vectors u v w 
%
% using:
%   D = divergence of flux on cell sides
%   M = transformation of wind to flux through cell sides with
%       zero flux boundary condition at the bottom (the terrain)
%   A = weights of the wind vector components, U'*A*U approx ||U||_L2^2
%    
% compute:  
%
%    min 1/2 (U - U0)'*A*(U - U0) s.t. D*M*U = 0
%
%    <=>    A*(U - U0) + M'*D'*Lambda = 0
%
%           D*M*U                     = 0
%
%
%    <=>    U = U0 - inv(A)*M'*D'*Lambda
%
%           D*M*inv(A)*M'*D'*Lambda = D*M*U0 


if ~exist('check','var')
    check = false;
end
tstart=tic;
switch method
    case direct
        DM = 
% divergence of u0
f = div3(wind2flux(U0,X));
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
    u{i} = U0{i} + (1/w(i))*g{i};
end
% check divergence
if check, err_div = big(div3(u,h)), end
% check no correction in vertical speed at the bottom
if check, err_corr_w_bottom = big(u{3}(:,:,1)-U0{3}(:,:,1)), end
fprintf('mass_cons_int %g seconds\n',toc(tstart))
end
