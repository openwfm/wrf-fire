function delta=solve_saddle(C,H,F,V,invA)
%
% delta=solve_saddle(C,H,F,V,invA)
% 
% solve the saddle point problem
%
%  A*delta   + C*lambda = A*H + F
%  C'*delta             = V 
%
% input: 
%  C    column vector, may be given as matrix
%  H    column vector, may be given as matrix
%  F    column vector, may be given as matrix
%  invA function to multiply by the inverse of square matrix A
%  if the inputs are not column vectors they are reshaped into columns
%  for computations and the output reshaped back

%  eliminate delta and compute lambda:
%  delta = inv(A)*(A*H + F - C*lambda) = H + inv(A)*(F - C*lambda)
%  V = C'*delta 
%  V = C'*(H + inv(A)*(F - C*lambda)) 
%  V = C'*(H + inv(A)*F) - C'*inv(A)*C*lambda 
%  C'*inv(A)*C*lambda = C'*(H + inv(A)*F) - V
%  lambda =  C'*inv(A)*C \ (C'*(H + inv(A)*F) - V)
%  

invA_C = invA(C);
invA_F = invA(F);
R = H+invA_F;
S=C(:)'*invA_C(:);
lambda = S \ (C(:)'*R(:) - V);
delta = R - invA_C*lambda;
err_C=big(C(:)'*delta(:))/(big(C)*big(delta));
tol=100*eps*sqrt(prod(size(C)));
if err_C > tol,
    warning(['Large relative error ',num2str(err_C),'  in the constraint'])
end
end
