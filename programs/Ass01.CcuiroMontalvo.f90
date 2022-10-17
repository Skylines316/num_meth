PROGRAM readnum
  IMPLICIT NONE
  REAL :: a,b,res !floatings points
  INTEGER :: i !declare my integer variable
  PRINT*, "Input the real numbers: " !just an instruction for the user
  READ*, a !read the first real number into variable a
  PRINT*, "now, the second one" !instruction for the user
  READ*, b !read the second real number into variable b
  PRINT*, "Input the integer number: " !instruction to input the integer number
  READ*, i !read the integer number into variable
  res = (a+b)**i !make the operation with the numbers
  PRINT*, "The result is: ", res !print out the result

END PROGRAM readnum
