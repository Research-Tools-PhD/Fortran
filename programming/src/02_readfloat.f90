! WAP, similar to the one from section 01, which handles real data (i.e. 32-bit floating
! point numbers) instead of integer. The corresponding files have names following the pattern
! “d2_#.dat”. Real numbers may be provided in fixed or exponential format. Please note that
! floating point numbers may be truncated or rounded on output, so do not test for identify when
! comparing with the checksum. Rather use a relative accuracy of 10-5.

PROGRAM readfloat
    IMPLICIT NONE

    INTEGER :: i, n, ios
    REAL, ALLOCATABLE :: a(:) 
    REAL :: sum=0, sumcheck 
    CHARACTER(LEN=100) :: filename

    ! define maximum length of array
    CALL GET_COMMAND_ARGUMENT(1, filename)

    ! From data/d2_1.dat
    OPEN(5, FILE=filename, STATUS='OLD', IOSTAT=ios, ACTION='READ') 
    IF (ios /= 0) THEN
        WRITE(*,*) 'Error opening file ', filename
        STOP
    END IF

    ! read length of array
    READ (5, *) n

    ! allocate array
    ALLOCATE(a(n))

    ! read array
    READ (5, *) a 

    ! read checksum and close file again
    READ (5, *) sumcheck
    CLOSE(5)

    ! compute checksum and compare with expected value
    DO i = 1, n
        sum = sum + a(i)
    END DO
    
    ! output results
    WRITE(*,*) 'Length of array: ', n

    ! check if checksum is correct and output result
    IF (sum - sumcheck .LT. 1e-5) THEN
        WRITE(*,*) 'Checksum matches!', sum
    ELSE
        WRITE(*,*) 'Checksum incorrect'
    END IF

    DEALLOCATE(a) ! free memory

END PROGRAM readfloat