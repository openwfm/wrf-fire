function fuel_frac=fuel_test 
for i=1:4,figure(i),end
input('Position figure windows and press Enter >');
fd=[.5,.5];
[c1,c2]=ndgrid([0,fd(1)],[0,fd(2)]); % corners of the mesh cell
maxerr=0;

%tnow=1;
%fuel_time=1;
%lfn0=[0    0.3479;0.6521    1.0000]; % to debug fortran
tnow=2
lfn0=   [0.4192792     -1.9766893E-02    ;
         6.310914       5.983462 ]
fuel_time= 8.235294 ;
f_debug=[];

%!./ifmake clean
%!./ifmake fuel_burnt_test

tmp=zeros(23,1);
k=1;
for off=[0:0.05:1.1]
    if off >= 1.0 
        disp(off)
    end    
    lfn=lfn0-off;
%    tign=tnow+2*lfn+0.0*randn(2,2);
    tign = [2.000000      1.565244    ;
           2.000000       2.000000    ]
    f=fuel_burnt(lfn,tign,tnow,fd,fuel_time); 
    f_debug=[f_debug f];
    tmp(k)=f;
    k=k+1;
    f_debug=[f_debug f];
    err=[];
    for i=[1:9]
       n=2^(i+1);
       fq=fuel_burnt_quad(lfn,tign,tnow,fd,fuel_time,n);
        nn(i)=n;
        err(i)=abs(f-fq);
        last=max(1,i-1);
        figure(4)
        loglog(nn(last:i),err(last:i),'-ok')
        xlabel n,ylabel difference,title('Comparison with numerical quadrature')
        axis([3 1100 1e-8 1]),grid on, hold on
    end
     hold off
end

fid=fopen('tmp.txt','r');
A= fscanf(fid,'%g'); 
fclose(fid);
err_mat_fortran=abs(A-tmp)
save('err.mat','err_mat_fortran')
disp(f_debug);
end


function fuel_frac=fuel_burnt_quad(lfn,tign,tnow,fd,fuel_time,n)
% compute the same numerically for comparison
% if fireline passes through the cell then 
% lfn and tign must be linear
%otherwise the result will differ
% evaluate by quadrature on n by n points
% the mesh
g=(0.5+[0:n-1])/n;
[q1,q2]=meshgrid(fd(1)*g,fd(2)*g); % quad nodes
[c1,c2]=meshgrid([0,fd(1)],[0,fd(2)]); % corners of the mesh cell
% T and L on quad nodes
T = interp2(c1,c2,tign,q1,q2)-tnow;
L = interp2(c1,c2,lfn,q1,q2);
% the integrand
f = (1 - exp((L<0).*T./fuel_time)).*(L<0);
% integrate
fuel_frac = sum(f(:))/(n*n);
end
