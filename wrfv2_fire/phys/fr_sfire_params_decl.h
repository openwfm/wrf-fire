! declaration of fire model parameter arrays, passed to normal_spread as arguments
integer, intent(in)::xifms,xifme,xjfms,xjfme  ! redundant dimensions, for pass-through only
real,intent(in),dimension(xifms:xifme,xjfms:xjfme):: vx,vy                ! (node) wind velocity (m/s)
real,intent(IN),dimension(xifms:xifme,xjfms:xjfme):: dzfsdx,dzfsdy        ! (node) terrain gradient (1)
real,intent(IN),dimension(xifms:xifme,xjfms:xjfme)::    bbb,betafl,phiwc,r_0 ! (node) spread formula coefficients
real,intent(IN),dimension(xifms:xifme,xjfms:xjfme):: fgip                 ! (cell) init mass of surface fuel (kg/m^2)
integer,intent(IN),dimension(xifms:xifme,xjfms:xjfme):: ischap            ! (node) .ne.0 if chapparal
