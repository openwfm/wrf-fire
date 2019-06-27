function g=grad3z(f,h)
% u=grad3z(f,h)
% compute gradiend in 3d assuming zero boundary conditions
% arguments:
%    f  3d array

% wrap in zeros
n = size(f);
fz = zeros(n+2); 
fz(2:n(1)+1,2:n(2)+1,2:n(3)+1)=f;

% derivatives
g{1}=(fz(2:end,2:end-1,2:end-1)-fz(1:end-1,2:end-1,2:end-1))/h(1);
g{2}=(fz(2:end-1,2:end,2:end-1)-fz(2:end-1,1:end-1,2:end-1))/h(2);
g{3}=(fz(2:end-1,2:end-1,2:end)-fz(2:end-1,2:end-1,1:end-1))/h(3);
end

