! Integrate the provided files 09_simplesort.f90 and sorting.f90 into your build system from part 1.
! 09_simplesort.f90 provides a framework for testing subroutines that sort arrays of real numbers,
! and sorting.f90 provides a module containing two implementations of sorting algorithms, a very
! simple one and a quicksort implementation. Now take the swap subroutine and move it into your
! list_tools module instead. Furthermore add calls to is_sorted() from the same module after each
! sort is completed and print out a warning, if an array is not sorted. Since you validated the
! is_sorted() function with the data files in part 1, you can now validate sorting algorithms with it.
! Run the resulting executable and extract the timing information for the various problem set
! sizes

PROGRAM simple_sort
    USE sorting
    USE list_tools
    IMPLICIT NONE
    
    ! Declare variables
    INTEGER :: num, i, j
    REAL, ALLOCATABLE, DIMENSION(:) :: dat
    REAL :: start_time, end_time, random_value
    INTEGER, DIMENSION(9) :: sizes = (/ 500,1000,2000,5000,10000,20000,50000,100000,200000 /)
    INTEGER :: testing
    
    ! Check if program is running tests
    testing = COMMAND_ARGUMENT_COUNT()
    IF (testing > 0) THEN
        sizes = (/ 100, 250, 500,1000,2000,5000,10000,15000,20000 /)
    END IF
    
    ! Initialize random number generator
    CALL RANDOM_SEED()
    
    ! Loop over various array sizes for sorting benchmarks
    DO j = 1, SIZE(sizes, 1)
        num = sizes(j)
        ALLOCATE(dat(num))
        
        ! Fill array with uniformly distributed random numbers
        DO i=1,num
            CALL RANDOM_NUMBER(random_value)
            dat(i) = (ISHFT(HUGE(i),-7)*0.000001*random_value)
        END DO
        
        ! Call sorting algorithm and measure the time spent on it
        CALL CPU_TIME(start_time)
        CALL simplesort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'unsorted random', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by simplesort'
        END IF
        
        ! Call sorting algorithm again on the already sorted data
        CALL CPU_TIME(start_time)
        CALL simplesort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'already sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by simplesort'
        END IF
        
        ! Swap a few elements of the sorted array and sort one more time
        CALL swap(dat, INT(LOG(REAL(num))))
        
        CALL CPU_TIME(start_time)
        CALL simplesort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'mostly sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by simplesort'
        END IF
        
        ! Release storage
        DEALLOCATE(dat)
    END DO
    
    ! Format for printing time
    666 FORMAT (' Sorting',I8,1X,A15,1X,'elements took:',F12.6,' seconds')
END PROGRAM simple_sort
  
