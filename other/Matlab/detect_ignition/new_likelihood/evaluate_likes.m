function [like,deriv]= temp_liker(psi,t,p_like_spline,p_deriv_spline,n_deriv_spline)

tic
%convert to seconds
t = 3600*t;
[m,n] = size(psi);
%create masks
m1 = psi > 0;
m2 = psi < 0;
%m3 = psi == 0;

%evaluate likelihood on masks 
% need to think about the bounds of the splines ....
l1 = reshape(p_like_spline(t),m,n);
d1 = reshape(p_deriv_spline(t),m,n);
l2 = log(1-exp(l1)); % can use other spline here, fastest?  time ~= 0.012 sec
%l2 = reshape(n_like_spline(t),m,n); %time ~=  0.017
d2 = reshape(n_deriv_spline(t),m,n);
l3 = 0*l1;
d3 = 0*d1;


%put back together
like = l3;
like(m1) = l1(m1);
like(m2) = l2(m2);
deriv = d3;
deriv(m1) = d1(m1);
deriv(m2) = d2(m2);

toc 

end



