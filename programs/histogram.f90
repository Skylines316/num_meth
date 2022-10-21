subroutine bubble(x,N)
  implicit none
  integer :: i, newn , N , m
  real :: x(N),tmp
  m=N
  do while (m>1)
    newn=0
    do i=2,m
      if (x(i-1)>x(i)) then
        tmp=x(i)
        x(i)=x(i-1)
        x(i-1)=tmp
        newn=i
      endif
    enddo
    m=newn
  enddo
end subroutine bubble

real function normal(x, sigma, xo) result(y)
  implicit none
  real :: x
  real, intent(in) :: xo, sigma
  real, parameter :: pi = 4.0*atan(1.0)
  y = (1/sqrt(2*sigma**2*pi))*exp(-0.5*(x-xo)**2/sigma**2)
end function normal

real function g(x) result (y)
  implicit none
  real, intent(in) :: x
  real, external :: normal
  y=(15.0*x**(2)*normal(x,0.25,-0.5) + 13*normal(x,0.3,-1.5) + 7*normal(x,1.0,3.0))/35
end function g

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
    if (r < g(s)/0.5) then
      x(i)=s
      !print '(f9.3)',x(i)
      i = i+1
    end if
  end do
end subroutine rejection

subroutine new_interval(x,n,a,b)
  implicit none
  integer :: n, i
  real :: a,b
  real, dimension(n) :: x
  do i=1,n
    x(i) = a+(b-a)*x(i)
  end do
end subroutine new_interval

real function delta_x(x, n) result(delta)
  implicit none
  integer :: n, iqr
  real :: x(n)
  iqr = x(nint(n*3.0/4.0))-x(nint(n*1.0/4.0))
  delta = 2 * iqr / n**(1.0/3.0)
end function delta_x

integer function n_bins(x,d,delta) result(n)
  implicit none
  integer :: d
  real :: x(d)
  real :: delta
  n = floor((x(1)-x(n))/delta)+1
end function n_bins

program density
  implicit none
  integer :: n
  integer, allocatable :: seed(:) 
  real :: r
  integer :: numbers 
  real, dimension(:), allocatable :: x
  real, external :: delta_x
  integer :: ac2=0,ac4=0,ac6=0,ac8=0,ac10=0,i

  numbers=5000
  allocate(x(numbers))

  call random_seed(size=n)
  allocate(seed(n))
  seed = 123456
  call random_seed(put=seed)
  deallocate(seed)
  
  call rejection(x,numbers)
  call new_interval(x,numbers,-10.0,10.0)
  call bubble(x,numbers)
  print *, delta_x(x,numbers)
  !print '(f8.3)', x
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

end program density

