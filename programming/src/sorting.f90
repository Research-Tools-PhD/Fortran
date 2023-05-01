MODULE sorting
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: simplesort, quicksort, bubblesort, insertionsort, mergesort, hybridsort
CONTAINS

  ! pathetically bad sorting algorithm:
  ! loop over all unique pairs and swap the values
  ! if the left element is larger than the right one.
  SUBROUTINE simplesort(dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, i, j
    REAL :: tmp

    num = SIZE(dat,1)
    IF (num < 2) RETURN
    DO i=1,num-1
        DO j=i+1,num
            IF (dat(i) > dat(j)) THEN
                tmp = dat(i)
                dat(i) = dat(j)
                dat(j) = tmp
            END IF
        END DO
    END DO
  END SUBROUTINE simplesort

  ! quicksort implementation via recursion
  ! top-level takes whole array, recursions work on subsets.
  ! pick pivot element and recursively sort the two sublists.
  SUBROUTINE quicksort(dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, p

    num = SIZE(dat,1)
    IF (num < 2) RETURN

    p = select_pivot(dat,1,num)
    CALL quicksort_recurse(dat,1,p-1)
    CALL quicksort_recurse(dat,p+1,num)
  END SUBROUTINE quicksort

  RECURSIVE SUBROUTINE quicksort_recurse(dat,left,right)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER,INTENT(in) :: left, right
    INTEGER :: p

    IF (left < right) THEN
        p = select_pivot(dat,left,right)
        CALL quicksort_recurse(dat,left,p-1)
        CALL quicksort_recurse(dat,p+1,right)
    END IF
  END SUBROUTINE quicksort_recurse

  ! core step in quicksort. pick pivot value. then swap
  ! array elements so that smaller values are to the left of
  ! it and all larger values to the right. store pivot in
  ! the remaining spot. this element is now in its final location.
  ! return the index of the pivot element.
  ! The choice of the pivot is arbitrary, but crucial for getting
  ! good performance with presorted data.
  RECURSIVE FUNCTION select_pivot(dat,left,right) RESULT(i)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: i, j, right, left
    REAL :: tmp, pivot

    ! this is the classic choice of pivot element, assuming random data
    pivot = dat(right)
    ! an element in the middle is a much better choice for presorted data
    ! swap it with the rightmost element.
    ! pivot = dat((left+right)/2)
    ! dat((left+right)/2) = dat(right)
    i = left
    DO j=left,right-1
        IF (pivot > dat(j)) THEN
            tmp = dat(i)
            dat(i) = dat(j)
            dat(j) = tmp
            i = i+1
        END IF
    END DO
    dat(right) = dat(i)
    dat(i) = pivot
  END FUNCTION select_pivot

  ! This is an implementation of the insertion sort algorithm.
  ! It repeatedly steps through the list, compares adjacent elements and swaps them if they are in the wrong order.
  SUBROUTINE insertionsort(dat)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(INOUT) :: dat
    INTEGER :: i, j, num
    REAL :: temp
  
    ! Check for base case (i.e., array is already sorted or has only one element)
    num = SIZE(dat, 1)

    IF (num < 2) THEN
      RETURN
    END IF
    
    DO i = 2, num
      temp = dat(i)
      j = i - 1
      DO WHILE (j >=1 .AND. dat(j) > temp)
        dat(j+1) = dat(j)
        j = j - 1
      END DO
      dat(j+1) = temp
    END DO
  END SUBROUTINE insertionsort

  ! This is an implementation of the bubble sort algorithm.
  ! It repeatedly steps through the list, compares adjacent elements and swaps them if they are in the wrong order.
  SUBROUTINE bubblesort(dat)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(INOUT) :: dat
    INTEGER :: i, j, num
    REAL :: temp
    LOGICAL :: swapped
  
    num = SIZE(dat, 1)
    IF (num < 2) RETURN
    ! swapped is a flag to check if any swaps were made in the last iteration
    ! It is set to .TRUE. at the beginning of each iteration
    swapped = .TRUE.
    j = 1
    DO WHILE (swapped)
      ! Set swapped to .FALSE. at the beginning of each iteration
      ! Because if no swaps are made, the array is sorted
      swapped = .FALSE.
      DO i = 1, num - j
        IF (dat(i) > dat(i+1)) THEN
          temp = dat(i)
          dat(i) = dat(i+1)
          dat(i+1) = temp
          swapped = .TRUE.
        END IF
      END DO
      ! Increment j by 1 so that the last j elements are sorted
      j = j + 1
    END DO
  
  END SUBROUTINE bubblesort

! This is an implementation of the merge sort algorithm using bottom up approach.
! It divides the array into two halves, sorts the two halves and then merges them.
  SUBROUTINE mergesort(dat)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(INOUT) :: dat
    INTEGER :: num, i, j, k, chunk_size, left_start, right_end
    REAL, DIMENSION(:), ALLOCATABLE :: temp
  
    num = SIZE(dat, 1)
    IF (num < 2) RETURN
    ! Allocate memory for temporary array
    ALLOCATE(temp(num))
    ! chunk_size is the size of the subarray to be sorted
    chunk_size = 1
    DO WHILE (chunk_size < num)
      ! left_start is the starting index of the left subarray
      left_start = 1
      DO WHILE (left_start < num)
        ! mid is the ending index of the left subarray
        ! mid+1 is the starting index of the right subarray
        ! right_end is the ending index of the right subarray
        right_end = MIN(left_start + 2*chunk_size - 1, num)
        i = left_start
        j = left_start + chunk_size
        k = 1
        DO WHILE (i <= left_start + chunk_size - 1 .AND. j <= right_end)
          IF (dat(i) <= dat(j)) THEN
            temp(k) = dat(i)
            i = i + 1
          ELSE
            temp(k) = dat(j)
            j = j + 1
          END IF
          k = k + 1
        END DO
        DO WHILE (i <= left_start + chunk_size - 1)
          temp(k) = dat(i)
          i = i + 1
          k = k + 1
        END DO
        DO WHILE (j <= right_end)
          temp(k) = dat(j)
          j = j + 1
          k = k + 1
        END DO
        DO i = 1, right_end - left_start + 1
          dat(left_start+i-1) = temp(i)
        END DO
        left_start = right_end + 1
      END DO
      chunk_size = chunk_size * 2
    END DO
    ! Deallocate memory for temporary array
    DEALLOCATE(temp)
  END SUBROUTINE mergesort

!This is an implementation of the hybrid sort algorithm.
! It uses first insertion sort and then mergesort.
! It is a combination of the two algorithms so that it is faster than both of them.

  SUBROUTINE hybridsort(dat)
    IMPLICIT NONE           ! Explicit declaration of all variables
    REAL, DIMENSION(:), INTENT(INOUT) :: dat  ! Declare input array 'dat' as a real number array of unknown size 
    INTEGER :: num, i, chunk_size = 32       ! Declare variables for array size, index and chunk size
    REAL, DIMENSION(:), ALLOCATABLE :: temp  ! Declare temporary array 'temp' as a real number array of unknown size
  
    num = SIZE(dat, 1)      ! Get the number of elements in the array 'dat'
  
    IF (num < 2) THEN       ! If there is only one element in the array, there is nothing to sort
      RETURN
    END IF
    
    DO i = 1, num, chunk_size
      CALL insertionsort(dat(i:min(i+chunk_size-1, num)))
    END DO

    ALLOCATE(temp(num))     ! Allocate memory for the temporary array 'temp'
    CALL mergesort(dat)
    DEALLOCATE(temp)        ! Deallocate memory for the temporary array 'temp'
  END SUBROUTINE hybridsort



END MODULE sorting

