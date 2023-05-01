! Use  the provided   quicksort   implementation  to  build  a  corresponding   test   and  benchmark
! executable for quicksort. As noted in the lecture, the choice of the pivot element is arbitrary as
! far   as  the   algorithm   as   such   is   concerned,   but   it   can  have   a  significant   impact   on  the
! performance of quicksort. In the  sorting.f90  file is a commented out alternative strategy for
! choosing the pivot element. Use this as well and collect benchmark data for both variants of
! quicksort. Since this implementation of quicksort uses recursions, it may use large amounts of
! stack space, thus you may need to increase the available stack size using ‘ulimit -s unlimited’ to
! avoid crashes.


PROGRAM quick_sort_bench
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
        CALL quicksort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'unsorted random', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by quicksort'
        END IF
        
        ! Call sorting algorithm again on the already sorted data
        CALL CPU_TIME(start_time)
        CALL quicksort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'already sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by quicksort'
        END IF
        
        ! Swap a few elements of the sorted array and sort one more time
        CALL swap(dat, INT(LOG(REAL(num))))
        
        CALL CPU_TIME(start_time)
        CALL quicksort(dat)
        CALL CPU_TIME(end_time)
        WRITE(*,FMT=666) num, 'mostly sorted', end_time - start_time
        
        ! Check if simplesort worked correctly
        IF (.NOT. is_sorted(dat)) THEN
            WRITE(*,*) 'Array was not sorted by quicksort'
        END IF
        
        ! Release storage
        DEALLOCATE(dat)
    END DO
    
    ! Format for printing time
    666 FORMAT (' Sorting',I8,1X,A15,1X,'elements took:',F12.6,' seconds')
END PROGRAM quick_sort_bench