function poisson_fft3z_test
fprintf('against poisson_fft2: ')
n=[10,15];
h=rand(1,2);
F=rand(n);
u2=poisson_fft2(F,h,-1);
u3=poisson_fft3z(F,h);
fprintf('err=%s\n',norm(u3-u2,inf));
n=[1000,1000,24];
fprintf('3D Poisson equation size %d %d %d: ',n)
h=rand(1,3);
u=rand(n);
f=-mlap3z(u,h);
v=poisson_fft3z(f,h);
fprintf('err=%s\n',big(u-v));
end