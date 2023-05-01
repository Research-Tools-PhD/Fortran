
! pick two randomly chosen elements in array 'dat'
! and swap them. do this 'count' times.
SUBROUTINE swap(dat,count)
  IMPLICIT NONE
  REAL, DIMENSION(:),INTENT(inout) :: dat
  INTEGER, INTENT(in) :: count
  REAL,DIMENSION(2) :: rval
  INTEGER :: i,num,i1,i2
  REAL :: tmp

  num = SIZE(dat,1)
  DO i=1,count
      ! pick two elements at random
      CALL RANDOM_NUMBER(rval)
      rval = rval*REAL(num)+0.5
      i1 = INT(rval(1))
      i2 = INT(rval(2))
      ! paranoia check to avoid out-of-bounds access
      IF ((i1 < 1) .OR. (i1 > num) .OR. (i2 < 1) .OR. (i2 > num)) CYCLE
      ! swap the elements
      tmp = dat(i1)
      dat(i1) = dat(i2)
      dat(i2) = tmp
  END DO
END SUBROUTINE swap


PROGRAM real_sort
  USE sorting
  IMPLICIT NONE

  INTEGER :: num, i, n
  REAL,ALLOCATABLE,DIMENSION(:) :: dat
  REAL :: time1, time2, rv
  INTEGER,PARAMETER,DIMENSION(9) :: sizes = (/ &
      500,1000,2000,5000,10000,20000,50000,100000,200000 /)

  INTERFACE
      SUBROUTINE swap(dat,count)
        IMPLICIT NONE
        REAL, DIMENSION(:),INTENT(inout) :: dat
        INTEGER, INTENT(in) :: count
      END SUBROUTINE swap
  END INTERFACE

  ! initialize pseudo random number generator
  CALL RANDOM_SEED()

  ! loop over various pre-defined array dimensions for sorting benchmarks.
  DO n=1,SIZE(sizes,1)
      num = sizes(n)
      ALLOCATE(dat(num))

      ! fill array with uniform distributed random numbers
      DO i=1,num
          CALL RANDOM_NUMBER(rv)
          dat(i) = (ISHFT(HUGE(i),-7)*0.000001*rv)
      END DO

      ! call sort algorithm and measure the time spent on it.
      CALL CPU_TIME(time1)
      CALL simplesort(dat)
      CALL CPU_TIME(time2)
      WRITE(*,FMT=666) num, 'unsorted random', time2-time1

      ! call sort again on the already sorted data
      CALL CPU_TIME(time1)
      CALL simplesort(dat)
      CALL CPU_TIME(time2)
      WRITE(*,FMT=666) num, 'already sorted', time2-time1

      ! swap a few elements of the sorted array and sort one more time
      CALL swap(dat,INT(LOG(REAL(num))))
      CALL CPU_TIME(time1)
      CALL simplesort(dat)
      CALL CPU_TIME(time2)
      WRITE(*,FMT=666) num, 'mostly sorted', time2-time1

      ! release storage
      DEALLOCATE(dat)
  END DO

666 FORMAT (' Sorting',I8,1X,A15,1X,'elements took:',F12.6,' seconds')
END PROGRAM real_sort
