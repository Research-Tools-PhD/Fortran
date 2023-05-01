! Now put the definition of the derived type “pair” info a new module “list_types”. Import this
! module into “list_tools” and write a variant of “is_sorted()” for the pair type. This function shall
! have a second optional argument, a logical indicating whether the check should be performed
! on the key or the value of the pair. For that two new constants, “bykey” and “byvalue” shall be
! added to the module as well. Finally write a main program, similar to that from section 06 that
! will read the pair data files and perform the sorted check. Test data files if they are ordered by
! ascending value and descending keys.

PROGRAM modules
    USE list_types
    USE list_tools
    IMPLICIT NONE

    INTEGER :: n, i, ios
    TYPE(pair), ALLOCATABLE :: pairs(:)
    REAL :: sum = 0, sumcheck, tol
    CHARACTER(LEN=100) :: filename

    ! Read in the number of pairs
    ! define maximum length of array
    CALL GET_COMMAND_ARGUMENT(1, filename)

    ! From data/d2_1.dat
    OPEN(5, FILE=filename, STATUS='OLD', IOSTAT=ios, ACTION='READ') 
    IF (ios /= 0) THEN
        WRITE(*,*) 'Error opening file ', filename
        STOP
    END IF
    
    READ(5, *) n
    ALLOCATE(pairs(n))

    ! Read in the pairs
    READ (5, *) pairs(:)

    READ(5, *) sumcheck

    ! Check the checksum
    DO i = 1, n
        sum = sum + pairs(i)%val
    END DO

    tol = MAX(1e-5 * MAX(ABS(sum), ABS(sumcheck)), 1e-5)

    IF (ABS(sum - sumcheck) > tol) THEN
        WRITE(*, *) 'There is error in the data'
        WRITE(*, *) 'Checksum is ', sum
        WRITE(*, *)'Checksum should be ', sumcheck
        WRITE(*, *)'Difference is ', ABS(sum - sumcheck)
    ELSE
        WRITE(*, *) 'Checksum is correct'
    END IF

    ! Check if the list is sorted
    IF (is_sorted(pairs, ascending, forvalue)) THEN
        WRITE(*, *) 'Values are sorted in ascending order'
    ELSE
        WRITE(*, *) 'Values are not sorted in ascending order'
    END IF

    IF (is_sorted(pairs, descending, forkey)) THEN
        WRITE(*, *) 'Keys are sorted in descending order'
    ELSE
        WRITE(*, *) 'Keys are not sorted in descending order'
    END IF

    DEALLOCATE(pairs)
END PROGRAM modules
