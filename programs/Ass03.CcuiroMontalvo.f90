logical function dis(x,y) 
  real(8), intent(in) :: x, y 
  real(8) :: d
  d=sqrt(x**2+y**2)
  if (d <= 1) then
    dis = .true.
  else
    dis = .false.
  end if
end function dis

program cal_pi
  implicit none
  integer :: n, i, circle=0, square=0
  real(8) :: x,y,r,pi,dif,pi_exact
  logical, external :: dis
  do
    call random_number(x)
    call random_number(y)
    if (dis) then
      circle=circle+1
    end if
    square=square+1
    pi_exact=4*atan(1.0)
    pi=circle/(square/4.0)
    dif=abs(pi-pi_exact)
    if (dif<1.0e-7) exit
    end if
  end do
  print*, pi
end program cal_pi
