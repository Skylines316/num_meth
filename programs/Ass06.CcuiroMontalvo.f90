real function equation(x) result(y)
  implicit none
  real, intent(in) :: x
  y = 2 * exp(x) - 2 * x**2 - 3
end function equation

subroutine bisection(a,b,c)
  implicit none
  real :: a, b, c, equation, alk, dif, eps
  integer :: icount
  character(len=14) :: file_name='bisection.csv'
  integer :: u=10, ios
  open(unit=u, iostat=ios, file=file_name, status='replace', action='write')
  eps = 1.0e-7
  c = (a+b)/2
  icount = 1
  dif = b - c
  do while (abs(dif) > eps)
    alk = sign(1.0,equation(b)) * sign(1.0,equation(c))
    !print *, equation(0.0)
    if (ios == 0) then
      write(u,'(i3, f8.5)') icount, c
    else
      print *, 'File error'
    end if
    if (alk <= 0) then
      a=c
    else
      b=c
    end if
    c = (a+b)/2
    dif = b - c
    !print *, dif
    icount = icount + 1
  end do
  print *, 'bisection took ', icount, ' iterations.'
  close(u)
end subroutine bisection

subroutine false_position(a,b,c)
  implicit none
  real :: a, b, c, equation, alk, dif, eps
  integer :: icount
  character(len=20) :: file_name='false_position.csv'
  integer :: u, ios
  open(unit=u, iostat=ios, file=file_name, status='replace', action='write')
  eps = 1.0e-7
  c = (equation(b)*a-equation(a)*b)/(equation(b)-equation(a))
  icount = 1
  dif = b - c
  !print *, dif
  do while (abs(dif) > eps)
    alk = sign(1.0,equation(b)) * sign(1.0,equation(c))
    !print *, equation(0.0)
    if (ios == 0) then
      write(u,'(i3, f8.5)') icount, c
    else
      print *, 'File error'
    end if
    if (alk <= 0) then
      a=c
    else
      b=c
    end if
    !print *, a,b
    c = (equation(b)*a-equation(a)*b)/(equation(b)-equation(a))
    dif = b - c
    !print *, a,b,c
    !print *, dif
    icount = icount + 1
  end do
  print *, 'false_position took ', icount, ' iterations.'
  close(u)
end subroutine false_position



program num6
  implicit none
  real :: a,b,c,c1,equation
  a = -2.0
  b = 2.0
  call bisection(a,b,c)
  a = -2.0
  b = 2.0
  call false_position(a,b,c1)
  print *, "the root by bisection is", c
  print *, "the root by false position is", c1
end program num6
