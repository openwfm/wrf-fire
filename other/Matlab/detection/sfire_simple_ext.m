function t1=sfire_simple_ext(rr,t0,mask,ncycles)
% in:
% rr            structure with 2d fields r11...r33 
% t0            starting tign
% mask          if not 0, update 
% out:
% t1            updated ignition times
%
if ncycles<=0,
    [m,n]=size(rr.r11);
    f=fopen('r.dat','w');
    ft_write(f,[m,n],'int')
    ft_write(f,rr.r11,'double')
    ft_write(f,rr.r12,'double')
    ft_write(f,rr.r13,'double')
    ft_write(f,rr.r21,'double')
    ft_write(f,rr.r23,'double')
    ft_write(f,rr.r31,'double')
    ft_write(f,rr.r32,'double')
    ft_write(f,rr.r33,'double')
    fclose(f);
else
    [m,n]=size(t0);
    f=fopen('in.dat','w');
    ft_write(f,ncycles,'int')
    ft_write(f,t0,'double')
    ft_write(f,mask,'double')
    fclose(f);
    ! DYLD_FRAMEWORK_PATH=; DYLD_LIBRARY_PATH=; GFORTRAN_STDOUT_UNIT=6; ./sfire_simple.exe
    f=fopen('out.dat','r');
    t1=ft_read(f,'double',m*n);
    t1=reshape(t1,m,n);
    fclose(f);
end
