! is_sorted for real and integer

MODULE list_tools

    USE list_types
    IMPLICIT NONE
  
    LOGICAL, PARAMETER :: ascending = .true., descending = .false.
    LOGICAL, PARAMETER :: forkey = .true., forvalue = .false.

    ! Check if an array is sorted in the requested direction
    ! If no direction is specified, the array is assumed to be sorted in ascending order
    ! Interface for real and integer arrays
    INTERFACE is_sorted
      MODULE PROCEDURE is_sorted_real
      MODULE PROCEDURE is_sorted_int
      MODULE PROCEDURE is_sorted_pair
    END INTERFACE is_sorted

    CONTAINS

   ! pick two randomly chosen elements in array 'dat'
! and swap them. do this 'count' times.
SUBROUTINE swap(dat,count)
  IMPLICIT NONE
  REAL, DIMENSION(:),INTENT(inout) :: dat
  INTEGER, INTENT(in) :: count
  REAL,DIMENSION(2) :: rval
  INTEGER :: i,num,i1,i2
  REAL :: tmp

  num = SIZE(dat,1)
  DO i=1,count
      ! pick two elements at random
      CALL RANDOM_NUMBER(rval)
      rval = rval*REAL(num)+0.5
      i1 = INT(rval(1))
      i2 = INT(rval(2))
      ! paranoia check to avoid out-of-bounds access
      IF ((i1 < 1) .OR. (i1 > num) .OR. (i2 < 1) .OR. (i2 > num)) CYCLE
      ! swap the elements
      tmp = dat(i1)
      dat(i1) = dat(i2)
      dat(i2) = tmp
  END DO
END SUBROUTINE swap

   FUNCTION is_sorted_real(a, direction) RESULT(sorted)
      IMPLICIT NONE
      LOGICAL :: sorted
      INTEGER :: i
      REAL, INTENT(IN) :: a(:)
      LOGICAL, OPTIONAL, INTENT(IN) :: direction
      
      ! Assume the array is sorted
      sorted = .true.

      ! Check if the array is sorted
      DO i = 1, SIZE(a)-1
        IF (PRESENT(direction)) THEN

          ! Check if the array is sorted in the requested direction
          IF (direction .EQV. ascending) THEN
            IF (a(i) > a(i+1)) THEN
              sorted = .false.
              EXIT
            END IF
          ELSE ! descending
            IF (a(i) < a(i+1)) THEN
              sorted = .false.
              EXIT
            END IF
          END IF
        ELSE ! No direction specified
          IF (a(i) > a(i+1)) THEN
            sorted = .false.
            EXIT
          END IF
        END IF
      END DO

    END FUNCTION is_sorted_real

    FUNCTION is_sorted_int(a, direction) RESULT(sorted)
      IMPLICIT NONE
      LOGICAL :: sorted
      INTEGER :: i
      INTEGER, INTENT(IN) :: a(:)
      LOGICAL, OPTIONAL, INTENT(IN) :: direction
          
      ! Assume the array is sorted
      sorted = .true.
    
      ! Check if the array is sorted
      DO i = 1, SIZE(a)-1
        IF (PRESENT(direction)) THEN
          ! Check if the array is sorted in the requested direction
          IF (direction .EQV. ascending) THEN
            IF (a(i) > a(i+1)) THEN
              sorted = .false.
              EXIT
            END IF
          ELSE
            IF (a(i) < a(i+1)) THEN
              sorted = .false.
              EXIT
            END IF
          END IF
        ELSE
          IF (a(i) > a(i+1)) THEN
            sorted = .false.
            EXIT
          END IF
        END IF
      END DO
    
    END FUNCTION is_sorted_int


    FUNCTION is_sorted_pair(a, direction, for) RESULT(sorted)
      IMPLICIT NONE
      LOGICAL :: sorted
      INTEGER :: i
      TYPE(pair), INTENT(IN) :: a(:)
      LOGICAL, OPTIONAL, INTENT(IN) :: direction
      LOGICAL, OPTIONAL, INTENT(IN) :: for

      ! Assume the array is sorted
      sorted = .true.

      ! Check if the array is sorted

      IF (PRESENT(for)) THEN
        IF (for .EQV. forkey) THEN
          DO i = 1, SIZE(a)-1
            IF (PRESENT(direction)) THEN

              ! Check if the array is sorted in the requested direction
              IF (direction .EQV. ascending) THEN
                IF (a(i)%key > a(i+1)%key) THEN
                  sorted = .false.
                  EXIT
                END IF
              ELSE
                IF (a(i)%key < a(i+1)%key) THEN
                  sorted = .false.
                  EXIT
                END IF
              END IF
            ELSE
              IF (a(i)%key > a(i+1)%key) THEN
                sorted = .false.
                EXIT
              END IF
            END IF
          END DO
        ELSE ! forvalue
          DO i = 1, SIZE(a)-1
            IF (PRESENT(direction)) THEN

              ! Check if the array is sorted in the requested direction
              IF (direction .EQV. ascending) THEN
                IF (a(i)%val > a(i+1)%val) THEN
                  sorted = .false.
                  EXIT
                END IF
              ELSE
                IF (a(i)%val < a(i+1)%val) THEN
                  sorted = .false.
                  EXIT
                END IF
              END IF
            ELSE
              IF (a(i)%val > a(i+1)%val) THEN
                sorted = .false.
                EXIT
              END IF
            END IF
          END DO
        END IF
      ELSE ! No direction specified
        DO i = 1, SIZE(a)-1
          IF (a(i)%key > a(i+1)%key) THEN
            sorted = .false.
            EXIT
          END IF
        END DO
      END IF

    END FUNCTION is_sorted_pair


END MODULE list_tools

