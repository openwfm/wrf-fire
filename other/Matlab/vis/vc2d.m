function vc2d(dt)
j=0;
t1=clock;
%a=avifile('sfire.avi');
for k=[10:10:1000]
    j=j+1;
    oneframe(k,dt);
    %M=getframe;
    %a=addframe(a,M);
    grid off
    %print('-djpeg',sprintf('frame%5.5i',j))
    %M(j)=getframe(gcf);
    %M(j)=getframe;
    %if mod(j,10)==0 | j< 10,
    %    savemovie(M)
    %end
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