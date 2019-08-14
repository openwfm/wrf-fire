function [U,varargout]=mass_cons_flux(U0,X,check)
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
s = cell_sizes(X);
Avec = cell2vec({s.weight_u,s.weight_v,s.weight_w});
Ainv = diag(sparse(1./Avec));
method = 'direct'
switch method
    case 'direct'
        DM = mat_wind_flux_div(X); % divergence of u0
        U0vec = cell2vec(U0);
        rhs = DM * U0vec;
        L = DM * Ainv * DM';
        Lambda = L \ rhs;
        Uvec = U0vec - Ainv * DM' * Lambda;
        U = vec2cell(Uvec,U0);
end

if check, 
    err_div = big(div3(wind2flux(U,X)))
    varargout{2} = err_div;
    fprintf('mass_cons_flux %g seconds\n',toc(tstart))
end
% check no correction in normal speed at the bottom
% if check, err_corr_w_bottom = big(u{3}(:,:,1)-U0{3}(:,:,1)), end
end
