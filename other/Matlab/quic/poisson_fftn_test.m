function poisson_fftn_test
fprintf('against poisson_fft2: ')
n=[10,15];
h=rand(1,2);
F=rand(n);
u2=poisson_fft2(F,h,-1);
un=poisson_fftn(F,h);
fprintf('err=%s\n',norm(un-u2,inf));
fprintf('3D Poisson equation:  ')
n=[1000,1000,12];
h=rand(1,3);
u=rand(n);
f=mlap3(u,h);
v=poisson_fftn(f,h);
fprintf('err=%s\n',big(u-v));
end