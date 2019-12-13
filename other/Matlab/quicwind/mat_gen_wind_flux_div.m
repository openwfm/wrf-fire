function DM=mat_gen_wind_flux_div(X,type)
% get matrix of flux divergence
% input:
%     X   mesh
% output:
%     DM  flux divergence matrix
%
% Error checking
check_mesh(X);

if ~exist('type','var')
    type = 'DM'
end

% Generate wind flux matrix
Mmat = mat_flux(X);

% Generate divergence matrix
Dmat = mat_div3(X);

% Generate flux divergence matrix
if (type == 'M')
    DM = Mmat;
elseif (type == 'D')
    DM = Dmat;
else
    DM = Dmat * Mmat;
end

end