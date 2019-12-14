function x=multigrid(A,b,x0,M,P,R)
% x=multigrid(A,b,P,R)
% solve A{1}*x = b by multigrid method 
% In:
%   A   cell vector length N of matrix-vector multiplication functions
%   b   right hand side vector
%   x0  initial approximatin
%   M   cell vector length N of preconditioner functions
%   P   cell vector length N-1 of prolongation functions (interpolation)
%   R   cell vector length N-1 of restriction functions (averaging)

% set params
p.ncycle=1;
p.tolsm1=1e=6;
p.tolsm2=1e=6;
p.tolcr=1e=6;
p.maxcr=20;
p.maxsm1=5
p.maxsm2=5

N=length(A);
r{1}=b;
u{1}=x0;
mglevel(1)
x = u{1};
function mglevel(l) 
    % multigrid on level l, all matrices and vectors are global
    u{l} = pcg(A{l},r{l},p.tolsm1,p.maxsm1,M{l},u{l});% pre-smoothing
    r{l+1} = R{l}(r{l} - A{l}(u{l}));  % set up coarse problem
    u{l+1} = zeros(size(r{l+1}));
    if l < N-1
        for i=1:ncycle
            mglevel(l+1);  % iterate recursively on coarse problem
        end
    else    % coarsest level
        u{l+1} = pcg(A{l+1},r{l+1},p.tolcr,p.maxrc,M{l+1},u{l+1})
    end
    u{l} = u{l} + P{l}(u{l+1}); % apply coarse correction
    u{l} = pcg(A{l},r{l},p.tolsm2,p.maxsm2,M{1},u{l});% post-smoothing
end
end
    