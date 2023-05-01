! Now rename the “is_sorted()” function in “list_module” to “is_sorted_real()” and implement a
! version for integers called “is_sorted_int()” and define an interface for “is_sorted()” so that either
! of the two functions is being called, depending on the datatype of the arguments. The programs
! from sections 04 and 05 have to remain functional without change. In addition write a variant of
! the main program from section 03 that now imports “is_sorted()” from the “list_tools” module and
! checks integer arrays for being sorted.

PROGRAM function_overload
    USE list_tools
    IMPLICIT NONE

    INTEGER :: i, n, ios! loop index, length of array, sum of array elements, checksum
    INTEGER, ALLOCATABLE :: a(:) ! array
    CHARACTER(LEN=100) :: filename ! file name

    CALL GET_COMMAND_ARGUMENT(1, filename)
    ! From data/d1_1.dat
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
        WRITE (*,*) 'Array is sorted in ascending order'
    ELSE
        WRITE (*,*) 'Array is not sorted in ascending order'
    END IF

    DEALLOCATE(a)

END PROGRAM function_overload


