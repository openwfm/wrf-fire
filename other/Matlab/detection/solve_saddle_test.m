% set up test problem
m=10;n=1;
A = rand(m);
C = zeros(m,n); C(1:n,:)=eye(n);
V = 0; 
C = rand(m,n);
delta=rand(m,1);delta(1:n,1)=0;
lambda=rand(n,1);
H=rand(m,1);
F = A*delta + C*lambda - A*H;
V = C'*delta;
delta2 = solve_saddle(C,H,F,V,@(X)A\X);
err=big(delta-delta2)