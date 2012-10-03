program groupprojq2

implicit none
integer :: n,nn
real :: alpha,beta,t,told,s,sold,i,iold,r,rold,eold,fold,gold,h

open(unit=7,file='groupprojq2.dat')  ! File where to save results
t=0.0                         ! Initial t
s=9999.0                      ! Initial s
i=1.0                         ! Initial i
r=0.0                         ! Initial r
h=0.001                       ! Time step
nn=12000                      ! Number of time steps
do n=1,nn                     ! Time loop
! Update old values
  told=t                        ! Update told
  sold=s                        ! Update sold
  iold=i                        ! Update iold
  rold=r                        ! Update rold
  call rhs(alpha,beta,sold,iold,eold,fold,gold) ! Find each RHS
  t=h*n                         ! Evolve t
  s=sold+h*eold                 ! Evolve s
  i=iold+h*fold                 ! Evolve i
  r=rold+h*gold                 ! Evolve r
  write(unit=7,fmt="(4e12.4)") t,s,i,r   ! Output to file
enddo                                ! Close time loop

close(unit=7)                        ! Tidy up
end program groupprojq2

subroutine rhs(alpha,beta,s,i,e,f,g)
! Calculate the RHS of both equations
implicit none
real :: alpha,beta,s,i,e,f,g
alpha=0.001                   ! alpha
beta=3.0                      ! beta
e=-alpha*i*s
f=alpha*i*s-beta*i
g=beta*i
end subroutine rhs
