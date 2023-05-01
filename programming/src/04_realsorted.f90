! Now write a program similar to the one from section 03, which handles real numbers instead of
! integers and also the “is_sorted()” function is provided in a separate file as part of a “list_tools”
! module.

PROGRAM readint
    USE list_tools
    IMPLICIT NONE

    INTEGER :: i, n, ios! loop index, length of array, sum of array elements, checksum
    REAL, ALLOCATABLE :: a(:) ! array
    CHARACTER(LEN=100) :: filename ! file name

    CALL GET_COMMAND_ARGUMENT(1, filename)
    ! From data/d2_1.dat
    OPEN(5, FILE=filename, STATUS='OLD', IOSTAT=ios, ACTION='READ') 
    IF (ios /= 0) THEN
        WRITE(*,*) 'Error opening file ', filename
        STOP
    END IF

    ! Read length of array
    READ (5,*) n

    ! Allocate array
    ALLOCATE(a(n))

    READ (5,*) a

    ! Close file
    CLOSE(5)

    ! Print array
    WRITE(*,*) 'Array:'
    DO i=1,n
        WRITE(*,*) a(i)
    END DO

    ! Print result
    IF (is_sorted(a)) THEN
        WRITE(*,*) 'Array is sorted'
    ELSE
        WRITE(*,*) 'Array is not sorted'
    END IF

    DEALLOCATE(a)

END PROGRAM readint




