program measles

implicit none
integer :: n,nn,m
real :: alpha,beta,gamma,t,told,s,sold,i,iold,r,rold,eold,fold,gold,h

open(unit=7,file='measles.dat') ! File where to save results
alpha=0.05                      ! alpha
beta=10.0                       ! beta
gamma=0.014                     ! gamma
t=0.0                           ! Initial t
m=1000                          ! Total population
s=998.0                         ! Initial s
i=2.0                           ! Initial i
r=0.0                           ! Initial r
h=0.001                         ! Time step
nn=120000                       ! Number of time steps
do n=1,nn                       ! Time loop
! Update old values
  told=t                        ! Update told
  sold=s                        ! Update sold
  iold=i                        ! Update iold
  rold=r                        ! Update rold
  call rhs(m,alpha,beta,gamma,sold,iold,rold,eold,fold,gold) ! Find each RHS
  t=h*n                         ! Evolve t
  s=sold+h*eold                 ! Evolve s
  i=iold+h*fold                 ! Evolve i
  r=rold+h*gold                 ! Evolve r
  write(unit=7,fmt="(4e12.4)") t,s,i,r   ! Output to file
enddo                                ! Close time loop

close(unit=7)                        ! Tidy up
end program measles

subroutine rhs(m,alpha,beta,gamma,s,i,r,e,f,g)
! Calculate the RHS of both equations
implicit none
integer :: m
real :: alpha,beta,gamma,s,i,r,e,f,g
e=-alpha*i*s+gamma*m-gamma*s
f=alpha*i*s-beta*i-gamma*i
g=beta*i-gamma*r
end subroutine rhs

