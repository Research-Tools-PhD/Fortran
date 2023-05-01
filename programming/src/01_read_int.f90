!WAP that can read in a list of integer numbers into an array from a provided file,
!reading from standard input (channel 5) via i/o redirection. The files have filenames of the
!pattern “d1_1a.dat” and the following format: the first line has a single integer number signaling
!the length of the array; then the array elements spread over multiple lines, and finally a single
!number on the final line containing the sum of all elements, which should be used to check
!whether the reading was done correctly. The program should read the first line, then allocate
!sufficient storage and read in the array data and finally read and store the “checksum”. It shall
!output the length of the array and whether the checksum matches or not. If the checksum does
!not match, both, the expected and the computed values should be printed as well.

PROGRAM readint
    IMPLICIT NONE

    INTEGER :: i, n, sum=0, ios, sumcheck 
    INTEGER, ALLOCATABLE :: a(:) 
    CHARACTER(LEN=100) :: filename 

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

    READ (5,*) a ! Read array 

    READ (5,*) sumcheck ! Read checksum

    CLOSE(5) ! Close file so we can open it again later

    ! Compute checksum
    DO i=1,n
        sum = sum + a(i)
    END DO

    ! Print results
    WRITE(*,*) 'Length of array: ', n

    ! Check if checksum matches
    IF (sum /= sumcheck) THEN
        WRITE(*,*) 'Checksum does not match!'
        WRITE(*,*) 'Expected: ', sumcheck
        WRITE(*,*) 'Computed: ', sum
    ELSE
        WRITE(*,*) 'Checksum matches!'
    END IF

    DEALLOCATE(a) ! Deallocate array

END PROGRAM readint



