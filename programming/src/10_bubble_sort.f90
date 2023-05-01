! Implement a bubblesort algorithm into the sorting module and create a new main program
! based on the executable from section 09 that now calls bubblesort instead. Again, record
! benchmark data provided by the executable.

! The bubblesort algorithm is a simple sorting algorithm that repeatedly steps through the list
! to be sorted, compares each pair of adjacent items and swaps them if they are in the wrong
! order. The pass through the list is repeated until no swaps are needed, which indicates that
! the list is sorted. The algorithm gets its name from the way smaller elements "bubble" to the
! top of the list. Because it only uses comparisons to operate on elements, it is a comparison
! sort. Although the algorithm is simple, it is too slow and impractical for most problems
! even when compared to insertion sort.

PROGRAM bubble_sort
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
        CALL bubblesort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'unsorted random', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by bubblesort'
        END IF
        
        ! Call sorting algorithm again on the already sorted data
        CALL CPU_TIME(start_time)
        CALL bubblesort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'already sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by bubblesort'
        END IF
        
        ! Swap a few elements of the sorted array and sort one more time
        CALL swap(dat, INT(LOG(REAL(num))))
        
        CALL CPU_TIME(start_time)
        CALL bubblesort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'mostly sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by bubblesort'
        END IF
        
        ! Release storage
        DEALLOCATE(dat)
    END DO
    
    ! Format for printing time
    666 FORMAT (' Sorting',I8,1X,A15,1X,'elements took:',F12.6,' seconds')
END PROGRAM bubble_sort
  