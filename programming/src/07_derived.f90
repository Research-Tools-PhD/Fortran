! Write a program similar to the programs of sections 01 and 02 that can read in pairs of numbers
! as key-value pairs, where the key is integer and the value a real number. The corresponding
! data files have names “d3_#.dat” and the format is similar to previous cases: one line with the
! number of items (i.e. pairs), then the integer-real pairs, and then a single line with the
! checksum, that is is the sum of all real values. To implement this create a derived type “pair”
! with two entries: an integer “key” and a real “val” and store the data from the files in an array of
! this derived type

PROGRAM derived
    IMPLICIT NONE

    ! Creating a derived type  pair with two entries: an integer “key” and a real “val” and store the data from the files in an array of this derived type
    TYPE pair
        INTEGER :: key
        REAL :: val
    END TYPE pair

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
    CLOSE(5)

    ! Print the data
    DO i = 1, n
        WRITE(*, *) pairs(i)%key, pairs(i)%val
    END DO
    WRITE(*, *) 'Checksum is ', sumcheck

    ! Check the checksum
    DO i = 1, n
        sum = sum + pairs(i)%val ! sum of all real values
    END DO

    ! Here we define the tolerance because the sum of the real values is not an integer and therefore we cannot use the same tolerance as in the previous examples
    ! we use the maximum of 1e-5 * max(abs(sum), abs(sumcheck)) and 1e-5 as tolerance because we want to make sure that the tolerance is not too small
    tol = MAX(1e-5 * MAX(ABS(sum), ABS(sumcheck)), 1e-5)

    IF (ABS(sum - sumcheck) < tol) THEN
        WRITE(*, *) 'Checksum is correct'
    ELSE
        WRITE(*, *) 'Checksum is incorrect'
    END IF

    DEALLOCATE(pairs)

END PROGRAM derived
