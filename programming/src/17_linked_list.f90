! Integrate the provided file  17_array_lookup.f90  file into your build system from parts 1 and 2.
! 17_array_lookup.f90  provides a framework for testing and benchmarking data structures and
! looking up the previously stored data in random order. The file expects to read d3_#.dat files
! with data pairs. You need to implement operations 1) to initialize the data structure, 2) to add
! items to the list, 3) to look up items by value (i.e. the “key” element of the pair type); this should
! be a function returning the pair, and 4) to deallocate the data structure completely.

! This module defines the linked list data structure and its associated subroutines and functions

MODULE linked_list
  USE list_types 
  IMPLICIT NONE

  TYPE, PUBLIC :: list
    TYPE(pair) :: data 
    TYPE(list), POINTER :: next => NULL() 
  END TYPE list

  TYPE(list), POINTER :: head => NULL() 
  TYPE(list), POINTER :: tail => NULL()

  PUBLIC :: init_list, add_list, lookup_list, free_list ! make these subroutines/functions public so they can be called from other parts of the code

CONTAINS

  ! Subroutine to initialize an empty linked list
  SUBROUTINE init_list()
    head => NULL() ! set the head and tail pointers to NULL to indicate an empty list
    tail => NULL()
  END SUBROUTINE init_list

  ! Subroutine to add a new node to the end of the linked list
  SUBROUTINE add_list(p)
    TYPE(pair), INTENT(IN) :: p ! the data to be added to the linked list
    TYPE(list), POINTER :: new_node ! pointer to the new node being created

    ALLOCATE(new_node) ! allocate memory for the new node
    new_node = list(p, NULL()) ! initialize the new node with the input data and a NULL pointer

    IF (.NOT.ASSOCIATED(head)) THEN ! if the list is empty, set the head and tail pointers to the new node
      head => new_node
      tail => new_node
    ELSE ! otherwise, add the new node to the end of the list and update the tail pointer
      tail%next => new_node
      tail => new_node
    END IF
  END SUBROUTINE add_list

  ! Function to look up a node in the linked list based on its key
  FUNCTION lookup_list(key) result(p)
    INTEGER, INTENT(IN) :: key ! the key value to search for
    TYPE(list), POINTER :: node ! pointer to the current node being examined
    TYPE(pair) :: p ! the data to be returned if the key is found

    node => head ! start at the head of the list
    DO WHILE (ASSOCIATED(node))! loop over each node in the list
      IF (node%data%key == key) THEN ! if the current node's key matches the input key, return its data and exit the loop
        p = node%data
        EXIT
      END IF
      node => node%next ! move to the next node in the list
    END DO
  END FUNCTION lookup_list

  ! Subroutine to deallocate all nodes in the linked list
  SUBROUTINE free_list()
    TYPE(list), POINTER :: node, next ! pointers to the current and next nodes being deallocated

    node => head ! start at the head of the list
    DO WHILE (ASSOCIATED(node)) ! loop over each node in the list
      ! pointer is not NULL, it is safe to dereference it
      next => node%next ! store the next node in the list so we can deallocate the current one and move on
      DEALLOCATE(node) ! deallocate memory for the current node
      node => next ! move to the next node in the list
    END DO
  END SUBROUTINE free_list

END MODULE linked_list


PROGRAM array_lookup
  USE list_types
  USE linked_list

  IMPLICIT NONE

  INTEGER :: num, i, j, k
  REAL :: chk, rv, time1, time2
  INTEGER, ALLOCATABLE, DIMENSION(:) :: idx
  TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat
  TYPE(pair) :: p
  INTEGER, PARAMETER :: nlook = 5000
  INTEGER :: error
  REAL :: test

  CHARACTER(LEN=256) :: filename
  
  ! Check if the file name is provided
  error = COMMAND_ARGUMENT_COUNT()  
  ! If no file name is provided, stop the program and print an error message
  IF (error < 1) THEN
      WRITE(*,*) "No filename provided"
      STOP
  ENDIF

  ! Read the file name
  CALL GET_COMMAND_ARGUMENT(1, filename)

  ! Open the file in channel 5
  OPEN(UNIT=5, FILE=filename, STATUS='old', ACTION='read')


  READ(5,*) num
  ALLOCATE(dat(num))
  READ(5,*) (dat(i),i=1,num)
  READ(5,*) chk
  CLOSE(5)

  ! initialize the linked list
  CALL init_list()

  ! add the data to the linked list
  DO i=1,num
      CALL add_list(dat(i))
  END DO

  ! fill idx array with randomly selected keys
  CALL RANDOM_SEED()
  ALLOCATE(idx(nlook))
  DO i=1,nlook
      CALL RANDOM_NUMBER(rv)
      j = INT(rv*num)+1
      idx(i) = dat(j)%key
  END DO

  chk = 0
  CALL CPU_TIME(time1)
  DO k=1,1000
      DO i=1,nlook
          DO j=1,num
              IF (dat(j)%key == idx(i)) THEN
                  p = dat(j)
                  EXIT
              END IF
          END DO
          chk=chk+p%key
      END DO
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'array value lookups', (time2-time1)
  WRITE(*,*) 'Checksum:', chk
  test = chk ! save the checksum for later comparison

  chk = 0
  CALL CPU_TIME(time1)
  DO k=1,1000
      DO i=1,nlook
          ! XXX do linked list or hash table lookups here
          p = lookup_list(idx(i))
          chk = chk + p%key
      END DO
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'Linked list lookups', (time2-time1)
  WRITE(*,*) 'Checksum:', chk

  ! Checking if checksum matches with the one obtained from array lookup
  IF (ABS(chk - test) < 1.0E-6) THEN
      WRITE(*,*) "Checksum is correct"
  ELSE
      WRITE(*,*) "Checksum does not match"
  ENDIF

  ! XXX free all allocated data
  CALL free_list()
  DEALLOCATE(dat,idx)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM array_lookup
