! Expand the “list_tools” module so that it contains two constants (parameters), “ascending” and
! “descending”, both are of type logical and have the value .true. and .false., respectively. Now
! add to the “is_sorted()” function an optional argument so that the direction of the order can be
! indicated. Do this in such a fashion that the program from 04 remains functional without
! modification, yet uses the same module. The main program shall differ from the version in
! section 04 only by explicitly requesting the sort order to check for (ascending).

PROGRAM readint
    USE list_tools
    IMPLICIT NONE

    INTEGER :: i, n, ios
    REAL, ALLOCATABLE :: a(:) 
    CHARACTER(LEN=100) :: filename 

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
        WRITE (*,*) 'Array is sorted in ascending order'
    ELSE
        WRITE (*,*) 'Array is not sorted in ascending order'
    END IF

    DEALLOCATE(a)

END PROGRAM readint




