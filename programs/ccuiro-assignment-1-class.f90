MODULE assig_1

CONTAINS

REAL(8) FUNCTION f(x) RESULT(res)
  implicit none
  real(8), intent(in) :: x
  res = exp(x)
END FUNCTION f

REAL(8) FUNCTION int_rectangles(a, b, n) RESULT(res)
  implicit none
  real(8) :: a, b, h
  integer :: n, i
  h = (b-a)/n
  res = 0
  do i=1,n
    res = res + f(a)*h
    !print *, a
    a = a + h
  end do
END FUNCTION int_rectangles

END MODULE assig_1


PROGRAM assignment_int_1
  use assig_1
  implicit none
  real(8) :: a, b, ao, bo
  real(8) :: e, res, res2, dif 
  integer :: init
  write(*,'(a,/,a)') 'The interval is [a,b]','Input a'
  read *, a
  ao = a
  write(*,'(a)') 'Input b'
  read *, b
  bo = b
  write(*,'(a)') 'Input the precision'
  read *, e
  init = 10
  res = int_rectangles(a,b,init)
  do
    init = init * 2
    a = ao
    b = bo
    res2 = int_rectangles(a,b,init)
    dif = abs(res2-res)
    res = res2
    write(*,'(f10.7,i10)') res2, init
    if (dif <= e) exit
  end do
  write(*,'(f9.6)') res2
END PROGRAM assignment_int_1
