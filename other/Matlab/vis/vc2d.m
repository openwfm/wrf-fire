function vc2d
j=0;
t1=clock;
%a=avifile('sfire.avi');
for k=1:10:4001
    j=j+1;
    oneframe(k);
    %M=getframe;
    %a=addframe(a,M);
    grid off
    %M(j)=getframe(gcf);
    M(j)=getframe;
    if mod(j,10)==0 | j< 10,
        savemovie(M)
    end
        fprintf('frame %g model time %g rendering time %g s\n',j,k,etime(clock,t1))
end
savemovie(M)
%a=close(a);
end

function savemovie(M)
    mf='sfire.mat';
    save(mf,'M')
    fprintf('saved to file %s\n',mf)
end