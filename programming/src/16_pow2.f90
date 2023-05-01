! Write a function next_pow2(n) that returns the next integer that is larger or equal than n and a
! power of two.

PROGRAM funct
  USE iso_fortran_env, ONLY: int32
  IMPLICIT NONE
  INTEGER(int32) :: n, result

  DO n = 1, 5
    result = next_pow2(n)
    WRITE(*,*) result
  END DO

  CONTAINS


  FUNCTION next_pow2(n) RESULT(result)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    INTEGER :: p, result

    IF (n <= 0) THEN ! n is negative or zero
      result = 1 ! return 1
    ELSE 
      p = 1 
      DO WHILE (p < n) 
        p = 2 * p ! double p
      END DO
      result = p
    END IF

  END FUNCTION next_pow2

END PROGRAM funct

