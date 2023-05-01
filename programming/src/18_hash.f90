! Now implement a hash table using linked lists as buckets. Use the  next_pow2(n)  function to
! determine the actual number of buckets based on a suggested minimum value. Implement a
! function hashfunc(val, size) that takes the value to be hashed and the (power of two) number of
! buckets as an argument.  This function shall contain an interface to the provided C function
! inthash(key, size)  and  the  corresponding  inthash.c  source   file  needs   to  be  added   to  the
! support library sources. To get it correctly compiled, the CmakeLists.txt file also needs to be
! changed to enable C as programming language besides Fortran.
! You may alternately choose to re-implement the hashing function in Fortran and skip interfacing
! the C function. It must produce identical hash values as the C version.
! Inside   the   hash   table   functions   you   can   call   hashfunc()   to   compute   a   hash   based   on   the
! (integer) key of any data pair and it shall return the index of the bucket the data shall be stored
! in. Experiment with multiple minimum numbers of buckets (50, 100, 200). Add this to the testing
! and benchmarking framework as in section 17 and implement the same 4 types of operations.
! Discuss the performance of looking up elements by value in the three data structures

MODULE hash_table
  USE list_types ! import the list_types module, which defines the 'pair' type used in the linked list
  USE iso_c_binding, ONLY : C_INT ! import the C_INT type from the iso_c_binding module
  IMPLICIT NONE

  TYPE, PUBLIC :: node ! define a new type to store a node in the linked list
    TYPE(pair) :: data ! the data stored in this node of the linked list
    TYPE(node), POINTER :: next => NULL() ! pointer to the next node in the linked list
  END TYPE node

  TYPE, PUBLIC :: list ! define a new type to store the linked list
    TYPE(node), POINTER :: head => NULL() ! pointer to the first node in the linked list
    TYPE(node), POINTER :: tail => NULL() ! pointer to the last node in the linked list
  END TYPE list

  TYPE, PUBLIC :: hash ! define a new type to store the hash table
    TYPE(list), POINTER :: buckets(:) ! array of pointers to the linked lists that make up the buckets
    INTEGER :: size ! the number of buckets in the hash table
  END TYPE hash

  PUBLIC :: init_htable, add_htable, lookup_htable, free_htable ! make these subroutines/functions public so they can be called from other parts of the code

  ! Subroutine to initialize an empty hash table with a given number of buckets
  ! It is a C interface to the Fortran subroutine init_htable(n)

  INTERFACE
    FUNCTION inthash(value, size) BIND(C, name="inthash")
      IMPORT
      INTEGER(C_INT), value :: value, size
      INTEGER(C_INT) :: inthash
    END FUNCTION inthash
  END INTERFACE

 CONTAINS
  
 FUNCTION next_pow2(n) RESULT(result)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: p, result

  IF (n <= 0) THEN ! n is negative or zero
    result = 1 ! return 1
  ELSE 
    p = 1 
    DO WHILE (p < n) 
      p = 2 * p ! double p
    END DO
    result = p
  END IF

 END FUNCTION next_pow2

  FUNCTION hashfunc(key, size) result(ph)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: key, size ! the value to hash and the number of buckets in the hash table
    INTEGER :: ph ! the index of the bucket to store the value in

    ph = inthash(key, size) + 1! call the C function to hash the value and if we are using 1-based indexing, add 1 to the result else leave it as is
  END FUNCTION hashfunc


  ! Subroutine to initialize an empty hash table with a given number of buckets
  SUBROUTINE init_htable(htable, n)
    TYPE(hash), INTENT(INOUT) :: htable ! the hash table to initialize
    INTEGER, INTENT(IN) :: n ! the number of buckets to initialize the hash table with
    
    htable%size = next_pow2(n) ! find the next power of 2 greater than or equal to the input number of buckets 
    ALLOCATE(htable%buckets(htable%size)) ! allocate memory for the array of buckets
    htable%buckets = list(NULL(), NULL()) ! set the head and tail of each bucket to NULL
  END SUBROUTINE init_htable




  SUBROUTINE add_htable(htable, pe)
    TYPE(hash), INTENT(INOUT) :: htable ! the hash table to add the data to
    TYPE(pair), INTENT(IN) :: pe ! the data to add to the hash table
    TYPE(node), POINTER :: new_node ! pointer to the new node being created
    INTEGER :: index ! the index of the bucket to add the data to

    index = hashfunc(pe%key, htable%size) ! find the index of the bucket to add the data to

    ALLOCATE(new_node) ! allocate memory for the new node
    new_node = node(pe, NULL()) ! initialize the new node with the input data and a NULL pointer

    IF (.NOT.ASSOCIATED(htable%buckets(index)%head)) THEN ! if the bucket is empty, set the head and tail pointers to the new node
      htable%buckets(index)%head => new_node
      htable%buckets(index)%tail => new_node
    ELSE ! otherwise, add the new node to the end of the list and update the tail pointer
      htable%buckets(index)%tail%next => new_node
      htable%buckets(index)%tail => new_node
    END IF
  END SUBROUTINE add_htable




  FUNCTION lookup_htable(htable, key) result(p)
    TYPE(hash), INTENT(IN) :: htable ! the hash table to search
    INTEGER, INTENT(IN) :: key ! the key value to search for
    TYPE(pair) :: p ! the data to be returned if the key is found
    TYPE(node), POINTER :: n_node ! pointer to the current node being examined
    INTEGER :: index ! the index of the bucket to search

    index = hashfunc(key, htable%size) ! find the index of the bucket to search

    n_node => htable%buckets(index)%head ! start at the head of the list
    
    DO WHILE (ASSOCIATED(n_node))! loop over each node in the list
      IF (n_node%data%key == key) THEN ! if the current node's key matches the input key, return its data and exit the loop
        p = n_node%data
        EXIT
      END IF
      n_node => n_node%next ! move to the next node
    END DO
  END FUNCTION lookup_htable

  SUBROUTINE free_htable(htable)
    TYPE(hash), INTENT(INOUT) :: htable ! the hash table to deallocate
    TYPE(node), POINTER :: c_node, n_node ! pointers to the current and next nodes being deallocated
    INTEGER :: i ! loop index

    DO i = 1, htable%size ! loop over each bucket in the hash table
      c_node => htable%buckets(i)%head ! start at the head of the list
      
      DO WHILE (ASSOCIATED(c_node)) ! loop over each node in the list
        n_node => c_node%next ! save a pointer to the next node
        DEALLOCATE(c_node) ! deallocate the current node
        c_node => n_node ! move to the next node
      END DO
      htable%buckets(i)%head => NULL() ! set the head and tail pointers to NULL to indicate an empty bucket
      htable%buckets(i)%tail => NULL()
    END DO
    DEALLOCATE (htable%buckets) ! deallocate the array of buckets
  END SUBROUTINE free_htable

END MODULE hash_table


PROGRAM array_lookup
  USE list_types
  USE hash_table
  IMPLICIT NONE

  INTEGER :: num, i, j, k
  REAL :: chk, rv, time1, time2, test
  INTEGER, ALLOCATABLE, DIMENSION(:) :: idx
  TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat
  TYPE(pair) :: p
  TYPE(hash) :: htable
  INTEGER, PARAMETER :: nlook = 5000
  INTEGER :: error


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
  CALL init_htable(htable, num)

  ! add the data to the linked list
  DO i=1,num
      CALL add_htable(htable, dat(i))
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
          ! hash table lookups here
          p = lookup_htable(htable, idx(i))
          chk = chk + p%key
      END DO
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'htable lookups', (time2-time1)
  WRITE(*,*) 'Checksum:', chk

  ! Checking if checksum matches with the one obtained from array lookup
  IF (ABS(chk - test) < 1.0E-6) THEN
      WRITE(*,*) "Checksum is correct"
  ELSE
      WRITE(*,*) "Checksum does not match"
  ENDIF

  ! XXX free all allocated data
  CALL free_htable(htable)
  DEALLOCATE(dat,idx)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM array_lookup
