! Write a program, similar to the one from section 01, where you add a function “is_sorted()” in 
! the same source file  to test if the array elements are sorted in ascending order. This function 
! shall take the integer array as argument and return a logical as result. After reading the data, 
! call the function and output a line of text indicating whether the array is sorted or not.

MODULE sorted
    IMPLICIT NONE

    CONTAINS

    FUNCTION is_sorted(a, n) RESULT(sorted)
        INTEGER, INTENT(IN) :: n
        INTEGER, INTENT(IN) :: a(n)
        INTEGER :: i
        LOGICAL :: sorted

        sorted = .TRUE.
        DO i=1,SIZE(a)-1
            IF (a(i) > a(i+1)) THEN
                sorted = .FALSE.
                EXIT
            END IF
        END DO

    END FUNCTION is_sorted

END MODULE sorted

PROGRAM readint
    USE sorted
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
    IF (is_sorted(a, n)) THEN
        WRITE(*,*) 'Array is sorted'
    ELSE
        WRITE(*,*) 'Array is not sorted'
    END IF

    DEALLOCATE(a)

END PROGRAM readint




