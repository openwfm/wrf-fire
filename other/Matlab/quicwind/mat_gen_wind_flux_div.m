function DM=mat_gen_wind_flux_div(X)
% get matrix of flux divergence
% input:
%     X   mesh
% output:
%     DM  flux divergence matrix
%
% Error checking
check_mesh(X);

% Generate wind flux matrix
Mmat = mat_flux(X);

% Generate divergence matrix
Dmat = mat_div3(X);

% Generate flux divergence matrix
DM = Dmat * Mmat;
end