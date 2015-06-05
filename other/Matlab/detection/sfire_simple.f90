	program sfire_simple
	implicit none
	double precision, allocatable,dimension(:,:)::t0,t1,mask,r11,r12,r13,r21,r23,r31,r32,r33
	integer::m,n,i,j,k,nsteps,changed,masked
	double precision::diff,a
	open(1,file='r.dat',form='unformatted',status='old')
	read(1)m,n
	allocate(t0(m,n))
	allocate(t1(m,n))
	allocate(mask(m,n))
	allocate(r11(m,n))
	allocate(r12(m,n))
	allocate(r13(m,n))
	allocate(r21(m,n))
	allocate(r23(m,n))
	allocate(r31(m,n))
	allocate(r32(m,n))
	allocate(r33(m,n))
	read(1)r11
	read(1)r12
	read(1)r13
	read(1)r21
	read(1)r23
	read(1)r31
	read(1)r32
	read(1)r33
	close(1)
	open(1,file='in.dat',form='unformatted',status='old')
        read(1)nsteps
	read(1)t0
	read(1)mask
	close(1)

        t1=t0; ! keep old where not updated
	do k=1,nsteps
	    diff=0.
	    changed=0
	    masked=0
            do j=2,n-1
                do i=2,m-1
                   if(mask(i,j)>0.)then
                     t1(i,j)=min(t0(i-1,j)+r32(i-1,j), &
                                 t0(i+1,j)+r12(i+1,j), &
                                 t0(i,j-1)+r23(i,j-1), &
                                 t0(i,j+1)+r21(i,j+1), &
                                 t0(i-1,j-1)+r33(i-1,j-1), &
                                 t0(i+1,j-1)+r13(i+1,j-1), &
                                 t0(i-1,j+1)+r31(i-1,j+1), &
                                 t0(i+1,j+1)+r11(i+1,j+1)) 
 	             a=abs(t1(i,j)-t0(i,j))
	             if(a>0.)then
	                 changed=changed+1
                         diff=max(diff,a)
                     endif
                   else
                     masked=masked+1
                   endif
                enddo
            enddo
	    t0=t1
            print *,'iter ',k,' diff ',diff,' changed ',changed,' masked ',masked
	enddo
	open(1,file='out.dat',form='unformatted',status='unknown')
	write(1)t1
	close(1)
	end program sfire_simple
	
