function err=poisson_fft3z_test
disp('poisson_fft3z_test')
fprintf('against poisson_fft2: ')
n=[10,15];
h=rand(1,2);
F=rand(n);
u2=poisson_fft2(F,h,-1);
u3=poisson_fft3z(F,h);
fprintf('err=%s\n',norm(u3-u2,inf));
n=[500,500,24];
fprintf('3D Poisson equation size %d %d %d: ',n)
% params
h=rand(1,3);
u=rand(n);
d=rand(1,3);
% test
g=grad3z(u,h);
for i=1:3
    g{i}=g{i}*d(i);
end
f=-div3(g,h);
v=poisson_fft3z(f,h,d);
err=big(u-v)
end