!   test driver for subroutine prop_ls
 
    program prop_test
    use module_fr_sfire_prop
    use module_fr_sfire_speed
    
    implicit none
    integer::m1,m2,n1,n2,i,j,m1m,m2m,n1m,n2m,halo
    real::rm,rn,ts,te,dx,dy
    real,allocatable,dimension(:,:)::phi,tc
    !external spread_rate
    open(1,form='formatted',file='prop_test_in.txt')
    read(1,1)rm,rn
    m1=1
    m2=nint(rm)
    n1=1
    n2=nint(rn)
    halo=1
    m1m=m1-halo      ! memory dims
    m2m=m2+halo
    n1m=n1-halo
    n2m=n2+halo
    

    print *,'prop_test_m:',m1,m2,n1,n2 
    allocate (phi(m1m:m2m,n1m:n2m),&
              tc(m1m:m2m,n1m:n2m),&
              r(m1m:m2m,n1m:n2m),&
              vx(m1m:m2m,n1m:n2m),&
              vy(m1m:m2m,n1m:n2m))
    read(1,1)ts,te
    read(1,1)((r(i,j),i=m1,m2),j=n1,n2)
    !print *,'prop_test_m: read r'
    read(1,1)dx,dy
    read(1,1)((phi(i,j),i=m1,m2),j=n1,n2)
    !print *,'prop_test_m: read phi'
    read(1,1)((vx(i,j),i=m1,m2),j=n1,n2)
    !print *,'prop_test_m: read vx'
    read(1,1)((vy(i,j),i=m1,m2),j=n1,n2)
    !print *,'prop_test_m: read vy'
    !print *,'prop_test_m: calling prop_test'
    close(1)
    ! prop_ls(ims,ime,jms,jme,ids,ide,jds,jde,phi,ts,te,dx,dy)
    ! print *,'phi   :',phi(m1:m1+2,n1:n1:2)
    call prop_ls(m1,m2-1,n1,n2-1,m1m,m2m,n1m,n2m,phi,tc,ts,te-ts,dx,dy)
    open (2,form='formatted',file='prop_test_out.txt')
    write(2,1)((phi(i,j),i=m1,m2),j=n1,n2)
    close(2)
    deallocate(r,vx,vy)
1   format((e25.15,1x)) 

    end program prop_test
