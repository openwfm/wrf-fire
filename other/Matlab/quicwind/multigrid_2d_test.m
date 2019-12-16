function multigrid_test
N=10;
nc=[4,3];   % the coarsest grid points
a=[1,3];    % domain size 
% solve Dirichlet problem with vertex nodes
% homogeneous boundary conditions 
% mesh is 0:n1+1  times 0:n2+1 but the boundary is not stored
% refinement:
%    0       1     ...      n1          n1+1        level l+1
%    0   1   2     ...     2*n1  2*n1+1 2*(n1+1)    level l

for l=N:-1:1
    n{l} = (nc+1)*2^(N-l)-1;                % mesh points
    h{l}=[a./(n{l}+1)];                     % mesh step  
    A{l}=@(x)flat(@mlap,x,n{l},h{l});       % matrix vector multiply
    M{l}=@(x)x;                             % identity preconditioner
    if l<N
          P{l}=@(x)flat(@prolongation_2d,x,n{l+1}); 
          R{l}=@(x)flat(@restriction_2d,x,n{l}); 
    end
    fprintf('level %i size %i %i\n',l,n{l})
end
f=zeros(n{1});
c=round(n{1}/2);
f(c(1),c(2))=1;
% set params
p.tol=1e-6;       % relative residual tolerance
p.maxit=7;        % maximum number of multigrid iterations 
p.tolsm1=p.tol;    % tolerance in pre-smoothing, 
p.tolsm2=p.tol;    % tolerance in pre-smoothing
p.tolcr=p.tol;     % tolerance for coarse solver
p.maxsm1=7;       % max pre-smoothing iterations
p.maxsm2=7;       % max post-smoothing iterations   
p.maxcr=20;       % max coarse solve iterations
p.ncoarse=1;      % number of coarse solves 1=V-cycle, 2=W-cycle

x=multigrid(A,f(:),[],M,P,R,p);
x = reshape(x,n{1});
mesh(x)

end
