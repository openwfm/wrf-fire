function err=mlap3z_test
disp('mlap3z_test')
disp('testing mlap3z = div3 grad3z')
n=[5,4,10];
f=randn(n);
h=rand(1,3);
err=big(mlap3z(f,h)+div3(grad3z(f,h),h))