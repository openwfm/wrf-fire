function poisson_test

n=[111,155]; % gridpoints in each direction excluding boundaries
h=rand(1,2); % meshstep

F=ones(n);

U=poisson_fft2(F,h);
FF=mlap(U,h);
R=F-FF;
err_PS=norm(R,inf)

return