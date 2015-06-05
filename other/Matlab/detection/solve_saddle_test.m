% set up test problem
m=10;n=1;
A = rand(m); A=A;
C = zeros(m,n); C(1:n,:)=eye(n);
delta=rand(m,1);delta(1:n,1)=0;
lambda=rand(n,1);
D=rand(m,1);
F = A*delta + C*lambda - A*D;
delta2 = solve_saddle(C,D,F,@(X) A\X);
err=big(delta-delta2)