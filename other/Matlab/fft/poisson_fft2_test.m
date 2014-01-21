function poisson_test

n=[111,155]; % gridpoints in each direction excluding boundaries
h=rand(1,2); % meshstep

F=ones(n);
F=rand(n);

U=poisson_fft2(F,h,1);
FF=mlap(U,h);
R=F-FF;
err=norm(R,inf)
F2=poisson_fft2(U,h,-1/2);
F2=poisson_fft2(F2,h,-1/2);
R=F-F2;
err=norm(R,inf)

return