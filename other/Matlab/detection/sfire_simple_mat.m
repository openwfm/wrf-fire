function t1=sfire_simple_mat(rr,t0,mask,ncycles)
% in:
% rr            structure with 2d fields r11...r33 
% t0            starting tign
% mask          if not 0, update 
% out:
% t1            updated ignition times
%
persistent r11 r12 r13 r21 r23 r31 r32 r33 

if ncycles<=0,
    r11=rr.r11;
    r12=rr.r12;
    r13=rr.r13;
    r21=rr.r21;
    r23=rr.r23;
    r31=rr.r31;
    r32=rr.r32;
    r33=rr.r33;
else
    [m,n]=size(t0);
    t1=t0;
    for k=1:ncycles,
        for j=2:n-1
            for i=2:m-1
                if mask(i,j)>0,
                t=inf;
                t=min(t,t0(i-1,j)+r32(i-1,j));
                t=min(t,t0(i+1,j)+r12(i+1,j));
                t=min(t,t0(i,j-1)+r23(i,j-1));
                t=min(t,t0(i,j+1)+r21(i,j+1));
                t=min(t,t0(i-1,j-1)+r33(i-1,j-1));
                t=min(t,t0(i+1,j-1)+r13(i+1,j-1));
                t=min(t,t0(i-1,j+1)+r31(i-1,j+1));
                t=min(t,t0(i+1,j+1)+r11(i+1,j+1));
                t1(i,j)=t;
                end
            end
        end
    end
end

