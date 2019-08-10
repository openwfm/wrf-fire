function g=grad3z(f,h,b)
% u=grad3z(f,h)
% compute gradient in 3d assuming zero boundary conditions
% u=grad3z(f,h,b) 
% in 3rd coordinate use reflection at bottom instead of zero
% arguments:
%    f       3d array
%    h(1:3)  stepsize
%    b       zero boundary conditiom on output at the bottom

if ~exist('b','var')
    b = false;
end

% wrap in zeros
n = size(f);
fz = zeros(n+2); 
fz(2:n(1)+1,2:n(2)+1,2:n(3)+1)=f;
if b,
    fz(:,:,1) = fz(:,:,2);
end

% derivatives
g{1}=(fz(2:end,2:end-1,2:end-1)-fz(1:end-1,2:end-1,2:end-1))/h(1);
g{2}=(fz(2:end-1,2:end,2:end-1)-fz(2:end-1,1:end-1,2:end-1))/h(2);
g{3}=(fz(2:end-1,2:end-1,2:end)-fz(2:end-1,2:end-1,1:end-1))/h(3);
end

