PROGRAM series
  IMPLICIT NONE
  INTEGER :: m, j !declare my integer variables
  !REAL :: pi_num, pi, dif !declare my float variables
  REAL(8) :: pi_num, pi, dif
  !this is a loop to ensure that m has to be greater than 1
  DO
    PRINT*, "Input the integer value:" !instructions for the user
    READ*, m !read the integer number and save it in m
    !this conditional ensures that m is greater than 1, if is not then will ask the user to input again the number
    IF (m<1) THEN
      PRINT*, "The real has to be greater than 1" !an instruction for the user to clarify that the number has to be greater than 1
    ELSE
            EXIT !if m is greater than 1 then the exit of the loop
    END IF
  END DO

  !this loop compare the diference between the numerical pi and the pi
  DO
    pi_num=0 !start the variable for the loop

    !this loop make the sum
    DO j = 0,m
      pi_num = 4*(-1)**j/(2.0*j+1)+pi_num 
    END DO

  pi=4*ATAN(1.0) !define the value of pi

  dif=ABS(pi_num-pi) !compare the diference between numerical pi and the pi

  IF (dif < 1.0e-4 .OR. m > 90000) EXIT !if the diference is lower than 1.0e-2  or m is greater than 100 then will exit of this loop

  m=m+1 !this statement to increase m to repeat the sum
  END DO

  !finally, print statements for the user
  PRINT *, "The value of"
  PRINT *, "m=",m
  PRINT *,  pi
  PRINT *, "pi=",pi_num
END PROGRAM series
