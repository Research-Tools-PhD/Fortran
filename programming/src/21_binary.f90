! Implement a binary search tree (BST) and test and benchmark it in a similar fashion to the
! linked list and hash table sections (17 and 18). Since tree traversal is most easily done with
! recursions,   use   recursive   subroutines   and   functions   for   that.   Implement   functionality   to   add
! items to the tree, locate items by value (the “key” entry of the pair data type), and to free all
! allocated storage. Compare lookup performance to array, linked list and hash table.

MODULE bst_module

    USE list_types
    IMPLICIT NONE

    PRIVATE 

    TYPE :: node 
        TYPE(node), POINTER :: left => NULL() ! Left child node is less than the current node
        TYPE(node), POINTER :: right => NULL() ! Right child node is greater than the current node
        TYPE(pair) :: dat ! Data stored in the node where pair%key is the key and pair%val is the value
    END TYPE node

    TYPE, PUBLIC :: bst
        TYPE(node), POINTER :: root => NULL() ! Root node of the BST
        INTEGER :: size ! Number of nodes in the BST
    END TYPE bst 

    PUBLIC :: init_bst, add_bst, lookup_bst, free_bst ! Public interface for the BST module

 CONTAINS

   ! Initializes an empty BST
    SUBROUTINE init_bst(tree)
        IMPLICIT NONE
        TYPE(bst), INTENT(INOUT) :: tree ! BST to initialize
        tree%root => NULL() ! Set the root node to NULL to indicate an empty BST
        tree%size = 0 ! Set the size to 0 to indicate an empty BST
    END SUBROUTINE init_bst 

   ! Adds a pair to the BST
    SUBROUTINE add_bst(tree, item)
         IMPLICIT NONE
         TYPE(bst), INTENT(INOUT) :: tree  ! BST to add the pair
         TYPE(pair), INTENT(IN) :: item ! Pair to add to the BST

         CALL recursion_add(tree%root, item) ! Recursively add the pair to the BST
    END SUBROUTINE add_bst

    ! Recursively adds a pair to the BST
    RECURSIVE SUBROUTINE recursion_add(curr_node, item)
         IMPLICIT NONE
         TYPE(node), POINTER, INTENT(INOUT) :: curr_node ! Current node in the BST to add the pair
         TYPE(pair), INTENT(IN) :: item ! Pair to add to the BST
         TYPE(node), POINTER :: new_node => NULL() ! New node to store the pair
         
         IF (.NOT. ASSOCIATED(curr_node)) THEN ! If the current node is NULL, create a new node and store the pair
            ALLOCATE(new_node) ! Allocate memory for the new node
            new_node%dat = item ! Store the pair in the new node
            curr_node => new_node ! Set the current node to the new node

            ELSEIF (item%key < curr_node%dat%key) THEN ! If the key of the pair is less than the key of the current node, add the pair to the left child node
                CALL recursion_add(curr_node%left, item) ! Recursively add the pair to the left child node

            ELSEIF (item%key > curr_node%dat%key) THEN ! If the key of the pair is greater than the key of the current node, add the pair to the right child node
                CALL recursion_add(curr_node%right, item) ! Recursively add the pair to the right child node

            ELSE ! If the key of the pair is equal to the key of the current node, update the value of the key in the BST
                curr_node%dat%val = item%val ! Update the value of the key in the BST

        END IF
    END SUBROUTINE recursion_add

    ! Looks up a key in the BST and returns the value
    SUBROUTINE lookup_bst(tree, key, item)
        IMPLICIT NONE
        TYPE(bst), INTENT(IN) :: tree ! BST to lookup the key
        INTEGER, INTENT(IN) :: key ! Key to lookup in the BST
        TYPE(pair) :: item ! Pair to store the key and value of the key in the BST

        item%key = key ! Set the key of the pair to the key to lookup in the BST

        CALL recursion_lookup(tree%root, item) ! Recursively lookup the key in the BST
    END SUBROUTINE lookup_bst

    ! Recursively looks up a key in the BST and returns the value
    RECURSIVE SUBROUTINE recursion_lookup(curr_node, item)
        IMPLICIT NONE
        TYPE(node), POINTER :: curr_node ! Current node in the BST to lookup the key
        TYPE(pair), INTENT(INOUT) :: item ! Pair to store the key and value of the key in the BST

        IF (ASSOCIATED(curr_node)) THEN ! If the current node is not NULL, lookup the key in the left and right child nodes
            IF (item%key < curr_node%dat%key) THEN ! If the key of the pair is less than the key of the current node, lookup the key in the left child node
                CALL recursion_lookup(curr_node%left, item) ! Recursively lookup the key in the left child node

            ELSEIF (item%key > curr_node%dat%key) THEN ! If the key of the pair is greater than the key of the current node, lookup the key in the right child node
                CALL recursion_lookup(curr_node%right, item) ! Recursively lookup the key in the right child node

            ELSE ! If the key of the pair is equal to the key of the current node, store the value of the key in the pair
                item%val = curr_node%dat%val ! Store the value of the key in the pair
            END IF
        END IF
    END SUBROUTINE recursion_lookup
        

    ! Frees all allocated storage in the BST
    SUBROUTINE free_bst(tree)
        IMPLICIT NONE
        TYPE(bst), INTENT(INOUT) :: tree ! BST to free all allocated storage

        CALL recursion_free(tree%root) ! Recursively free all allocated storage in the BST
        tree%root => NULL() ! Set the root node to NULL to indicate an empty BST
        tree%size = 0 ! Set the size to 0 to indicate an empty BST
    END SUBROUTINE free_bst

    ! Recursively frees all allocated storage in the BST
    RECURSIVE SUBROUTINE recursion_free(curr_node)
        IMPLICIT NONE
        TYPE(node), POINTER, INTENT(INOUT) :: curr_node ! Current node in the BST to free all allocated storage

        IF (ASSOCIATED(curr_node)) THEN ! If the current node is not NULL, free all allocated storage in the left and right child nodes
            CALL recursion_free(curr_node%left) ! Recursively free all allocated storage in the left child node
            CALL recursion_free(curr_node%right) ! Recursively free all allocated storage in the right child node
            DEALLOCATE(curr_node) ! Deallocate the memory for the current node
        END IF

    END SUBROUTINE recursion_free

END MODULE bst_module

PROGRAM bst_test

    USE bst_module
    USE list_types

    IMPLICIT NONE

    INTEGER :: num, i, j, k ! Number of pairs to add to the BST
    REAL :: chk, rv, time1, time2 ! Checksum and random value
    INTEGER, ALLOCATABLE, DIMENSION(:) :: idx ! Array of indices to add to the BST
    TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat ! Array of pairs to add to the BST
    TYPE(pair) :: p ! Pair to lookup in the BST
    INTEGER, PARAMETER :: nlook = 5000
    INTEGER :: error ! Error code
    REAL :: test ! Test value
    TYPE(bst) :: tree ! BST to test

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

    ! Read the number of pairs to add to the BST
    READ(5,*) num
    ALLOCATE(dat(num))
    READ(5,*) (dat(i),i=1,num)
    READ(5,*) chk
    CLOSE(5)

    ! Initialize the BST
    CALL init_bst(tree)

    ! add the data to the linked list
    DO i=1,num
        CALL add_bst(tree, dat(i))
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
          CALL lookup_bst(tree, idx(i), p)
          chk = chk + p%key
      END DO
    END DO
   CALL CPU_TIME(time2)
   WRITE(*,FMT=666) nlook, 'Binary tree lookups', (time2-time1)
   WRITE(*,*) 'Checksum:', chk

   ! Checking if checksum matches with the one obtained from array lookup
   IF (ABS(chk - test) < 1.0E-6) THEN
      WRITE(*,*) "Checksum is correct"
   ELSE
      WRITE(*,*) "Checksum does not match"
   ENDIF

  ! XXX free all allocated data
  CALL free_bst(tree)
  DEALLOCATE(dat,idx)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM bst_test




   


