function [x,myrelres]=multigrid(A,b,x0,M,P,R,p)
% x=multigrid(A,b,P,R)
% solve A{1}*x = b by multigrid method 
% In:
%   A   cell vector length N of matrix-vector multiplication functions
%   b   right hand side
%   x0  initial approximation
%   M   cell vector length N of preconditioner functions
%   P   cell vector length N-1 of prolongation functions (interpolation)
%   R   cell vector length N-1 of restriction functions (averaging)
%   p   structure with parameters

N=length(A);
r{1}=b(:);
if isempty(x0),
    u{1}=zeros(size(r{1}));
else
    u{1}=x0(:);
end
for it=1:p.maxit
    myrelres=norm(r{1}-A{1}(u{1}))/max(norm(r{1}),realmin);
    fprintf('multigrid iteration %i start relative residual %g\n',it,myrelres)
    mglevel(1)
    myrelres=norm(r{1}-A{1}(u{1}))/max(norm(r{1}),realmin);
    fprintf('multigrid iteration %i end relative residual %g\n',it,myrelres)
    if myrelres(end)<p.tol
        fprintf('Multigrid iterations converged to given tolerance %g\n',p.tol)
        break
    elseif it>=p.maxit
        fprintf('Multigrid iterations did not converge to given tolerance %g\n',p.tol)
    end
end     
x = reshape(u{1},size(b));

function mglevel(l) 
    % multigrid on level l, all matrices and vectors are global
    [u{l},flag,relres,iter,resvec] = pcg(A{l},r{l},p.tolsm1,p.maxsm1,M{l},[],u{l});% pre-smoothing
    fprintf('level %i relative residuals ',l);  print_in_line(resvec); fprintf('\n')
    r{l+1} = R{l}(r{l} - A{l}(u{l}));  % set up coarse problem
    u{l+1} = zeros(size(r{l+1}));
    if l < N-1
        for i=1:p.ncoarse
            mglevel(l+1);  % iterate recursively on coarse level
        end
    else    % coarsest level
        [u{l+1},flag,relres,iter,resvec] = pcg(A{l+1},r{l+1},p.tolcr,p.maxcr,M{l+1},[],u{l+1});
        fprintf('level %i relative residuals ',l+1); print_in_line(resvec); fprintf('\n')
    end
    u{l} = u{l} + P{l}(u{l+1}); % apply coarse correction
    [u{l},flag,relres,iter,resvec] = pcg(A{l},r{l},p.tolsm2,p.maxsm2,M{1},[],u{l});% post-smoothing
    fprintf('level %i relative residuals ',l); print_in_line(resvec); fprintf('\n')
end
end

function print_in_line(a)
for i=1:length(a)
    fprintf(' %6.3g',a(i))
end
end
    