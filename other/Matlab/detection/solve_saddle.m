function delta=solve_saddle(C,D,F,invA)
% 
% solve the saddle point problem
%
%  A*delta   + C*lambda = A*D + F
%  C'*delta             = 0 
%
% input: 
%  C    matrix
%  D    column vector
%  F    column vector
%  invA function to multiply by the inverse of square matrix A

%  eliminate delta and compute lambda
%  delta = inv(A)*(A*D + F - C*lambda) = D + inv(A)*(F - C*lambda)
%  0 = C'*delta = C'*(D + inv(A)*(F - C*lambda)) 
%  C'*inv(A)*C*lambda = C'*(D + inv(A)*F)
%  lambda =  C'*inv(A)*C \ C'*(D + inv(A)*F)
%  
invA_C = invA(C);
invA_F = invA(F);
R = D+invA_F;
lambda = (C'*invA_C)\(C'*R);
delta = R - invA_C*lambda;