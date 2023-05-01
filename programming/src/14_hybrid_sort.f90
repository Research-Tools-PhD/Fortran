! Well scaling sorting algorithms often have significant overhead and thus are less efficient for
! small arrays. Thus, the fastest generic implementations of sort algorithms are often hybrids that
! combine multiple algorithms for different problem set sizes. Implement such a hybrid sort from
! insertion sort and merge sort: rather than starting the merge with lists of length 1, do a loop over
! the data in chunks of 32 elements and sort each of them with insertion sort; then continue with
! merge sort on these pre-sorted sublists


PROGRAM hybrid_sort
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
        CALL hybridsort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'unsorted random', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by hybridsort'
        END IF
        
        ! Call sorting algorithm again on the already sorted data
        CALL CPU_TIME(start_time)
        CALL hybridsort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'already sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by hybridsort'
        END IF
        
        ! Swap a few elements of the sorted array and sort one more time
        CALL swap(dat, INT(LOG(REAL(num))))
        
        CALL CPU_TIME(start_time)
        CALL hybridsort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'mostly sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by hybridsort'
        END IF
        
        ! Release storage
        DEALLOCATE(dat)
    END DO
    
    ! Format for printing time
    666 FORMAT (' Sorting',I8,1X,A15,1X,'elements took:',F12.6,' seconds')
END PROGRAM hybrid_sort