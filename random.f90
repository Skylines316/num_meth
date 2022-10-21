subroutine BoxMuller(sigma,mean,x,y)
  implicit none
  real :: u,v
  real, intent(in) :: sigma, mean
  real, intent(out) :: x,y 
  real :: r,t
  call random_number(u)
  call random_number(v)
  r = sqrt(-2*sigma**2*log(1-u))
  t = 2*4.0*atan(1.0)*v
  x = (r-mean)*sin(t)
  y = (r-mean)*cos(t)
end subroutine BoxMuller

real function f(u) result (x)
implicit none
real, intent(in) :: u
x=u**(1.0/3.0)
end function f

real function g(x) result (y)
implicit none
real, intent(in) :: x
y=3.0*x**(2)
end function g

subroutine transformation(x,n)
  implicit none
  integer :: n, i
  real, dimension(n) :: x
  real :: r
  real, external :: f
  !generate 10000 random numbers
  do i=1,n
    call random_number(r)
    x(i)=f(r)
    !print '(f9.3)',x(i)
  end do
end subroutine transformation

subroutine rejection (x,n)
  implicit none
  integer :: n
  integer :: i=1
  real, dimension(n) :: x
  real :: r, s
  real, external :: g
  !generate 10000 random numbers
  do while (i<= n)
    call random_number(s)
    call random_number(r)
    !print *,g(s)
    if (r < g(s)/1.0) then
      x(i)=s
      !print '(f9.3)',x(i)
      i = i+1
    end if
  end do
end subroutine rejection

program random
  implicit none
  integer :: n
  integer, allocatable :: seed(:) 
  real :: r
  integer :: numbers 
  real, dimension(:), allocatable :: x
  real, dimension(:), allocatable :: y

  integer :: ac2=0,ac4=0,ac6=0,ac8=0,ac10=0,i

  numbers=10000
  allocate(x(numbers), y(numbers))

  call random_seed(size=n)
  allocate(seed(n))
  seed = 123456
  call random_seed(put=seed)
  deallocate(seed)
  
  call transformation(x,numbers)
  call rejection(y,numbers)
  ! do i=1,numbers
  !   if (x(i)<0.2) then
  !     ac2=ac2+1
  !   elseif (0.2<y(i) .and. y(i)<0.4) then
  !     ac4=ac4+1
  !   elseif (0.4<y(i) .and. y(i)<0.6) then
  !     ac6=ac6+1
  !   elseif (0.6<y(i) .and. y(i)<0.8) then
  !     ac8=ac8+1
  !   else
  !     ac10=ac10+1
  !   end if
  ! end do
  ! print '(i6)', ac2,ac4,ac6,ac8,ac10

end program random

