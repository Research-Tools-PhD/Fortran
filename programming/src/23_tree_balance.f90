! Implement the following strategy to rebalance the tree: 1) extract the content of the tree into a
! sorted array; 2) allocate a new root node; 3) determine the middle of the array and assign the
! data at this location to the new root node; 4) rebuild the tree by calling a function that takes an
! array of data elements with the part of the array before the middle and the part of the array after
! the middle element; this function determines the middle of the passed in array and adds it to the
! tree and then recursively calls itself on the two remaining parts. 5) free the old tree and assign
! the root node pointer with the location of the new tree.
! Benchmark the tree lookup performance after the rebalancing and compare depth and number
! of open leafs before and after the rebalancing.

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

    PUBLIC :: init_bst, add_bst, lookup_bst, free_bst, count_nodes, extract_tree, &
              print_depth_and_nodes_with_one_leaf, rebalance_tree, find_depth

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

    ! Counts the number of nodes in the BST
    FUNCTION count_nodes(tree) RESULT(num_nodes)
        IMPLICIT NONE
        TYPE(bst), INTENT(IN) :: tree ! BST to count the nodes in
        INTEGER :: num_nodes ! Number of nodes in the BST
    
        num_nodes = 0 ! Initialize the number of nodes to 0
        CALL recursion_count(tree%root, num_nodes) ! Recursively count the nodes in the BST
    END FUNCTION count_nodes

    ! Recursively counts the number of nodes in the BST
    RECURSIVE SUBROUTINE recursion_count(curr_node, num_nodes)
        IMPLICIT NONE
        TYPE(node), POINTER :: curr_node ! Current node in the BST to count the nodes in
        INTEGER, INTENT(INOUT) :: num_nodes ! Number of nodes in the BST

        IF (ASSOCIATED(curr_node)) THEN ! If the current node is not NULL, count the nodes in the left and right child nodes
            num_nodes = num_nodes + 1 ! Increment the number of nodes by 1
            CALL recursion_count(curr_node%left, num_nodes) ! Recursively count the nodes in the left child node
            CALL recursion_count(curr_node%right, num_nodes) ! Recursively count the nodes in the right child node
        END IF
    END SUBROUTINE recursion_count

    ! Extracts the content of the tree, ordered by value, into an array
    SUBROUTINE extract_tree(tree, array)
        IMPLICIT NONE
        TYPE(bst), INTENT(IN) :: tree ! BST to extract the content from
        TYPE(pair), DIMENSION(:), POINTER, INTENT(OUT) :: array ! Array to store the content of the tree
        INTEGER :: index ! Current index in the array
    
        index = 1 ! Set the current index to 1
        CALL recursion_extract(tree%root, array, index) ! Recursively extract the content of the tree
    END SUBROUTINE extract_tree
    
    ! Recursively extracts the content of the tree, ordered by value, into an array
    RECURSIVE SUBROUTINE recursion_extract(curr_node, array, index)
        IMPLICIT NONE
        TYPE(node), POINTER :: curr_node ! Current node in the BST to extract the content from
        TYPE(pair), DIMENSION(:), POINTER, INTENT(OUT) :: array ! Array to store the content of the tree
        INTEGER, INTENT(INOUT) :: index ! Current index in the array
    
        IF (ASSOCIATED(curr_node)) THEN ! If the current node is not NULL, extract the content of the left and right child nodes
            CALL recursion_extract(curr_node%left, array, index) ! Recursively extract the content of the left child node
            array(index) = curr_node%dat ! Store the data of the current node in the array
            index = index + 1 ! Increment the index
            CALL recursion_extract(curr_node%right, array, index) ! Recursively extract the content of the right child node
        END IF
    END SUBROUTINE recursion_extract


    ! Find the depth of the BST
    FUNCTION find_depth(tree) RESULT(depth)
        IMPLICIT NONE
        TYPE(bst), INTENT(IN) :: tree ! BST to find the depth of
        INTEGER :: depth ! Depth of the BST

        depth = 0 ! Initialize the depth to 0
        CALL recursion_depth(tree%root, depth) ! Recursively find the depth of the BST
    END FUNCTION find_depth

    ! Recursively find the depth of the BST
    RECURSIVE SUBROUTINE recursion_depth(curr_node, depth)
        IMPLICIT NONE
        TYPE(node), POINTER :: curr_node ! Current node in the BST to find the depth of
        INTEGER, INTENT(INOUT) :: depth ! Depth of the BST
        INTEGER :: left_depth ! Depth of the left child node
        INTEGER :: right_depth ! Depth of the right child node

        IF (ASSOCIATED(curr_node)) THEN ! If the current node is not NULL, find the depth of the left and right child nodes
            left_depth = 0 ! Initialize the depth of the left child node to 0
            right_depth = 0 ! Initialize the depth of the right child node to 0
            CALL recursion_depth(curr_node%left, left_depth) ! Recursively find the depth of the left child node
            CALL recursion_depth(curr_node%right, right_depth) ! Recursively find the depth of the right child node
            depth = MAX(left_depth, right_depth) + 1 ! Set the depth of the BST to the maximum depth of the left and right child nodes plus 1
        END IF
    END SUBROUTINE recursion_depth

    ! function for  number of nodes with only one leaf

    FUNCTION count_nodes_with_one_leaf(tree) RESULT(num_nodes)
        IMPLICIT NONE
        TYPE(bst), INTENT(IN) :: tree ! BST to count the nodes in
        INTEGER :: num_nodes ! Number of nodes in the BST

        num_nodes = 0 ! Initialize the number of nodes to 0
        CALL recursion_count_nodes_with_one_leaf(tree%root, num_nodes) ! Recursively count the nodes in the BST
    END FUNCTION count_nodes_with_one_leaf

    ! Recursively counts the number of nodes in the BST
    RECURSIVE SUBROUTINE recursion_count_nodes_with_one_leaf(curr_node, depth)
        IMPLICIT NONE
        TYPE(node), POINTER :: curr_node ! Current node in the BST to count the nodes in
        INTEGER, INTENT(INOUT) :: depth ! Depth of the BST

        IF (ASSOCIATED(curr_node)) THEN ! If the current node is not NULL, count the nodes in the left and right child nodes
            IF (ASSOCIATED(curr_node%left) .AND. .NOT. ASSOCIATED(curr_node%right)) THEN
                depth = depth + 1 ! Increment the number of nodes by 1
            ELSE IF (.NOT. ASSOCIATED(curr_node%left) .AND. ASSOCIATED(curr_node%right)) THEN
                depth = depth + 1 ! Increment the number of nodes by 1
            END IF
            CALL recursion_count_nodes_with_one_leaf(curr_node%left, depth) ! Recursively count the nodes in the left child node
            CALL recursion_count_nodes_with_one_leaf(curr_node%right, depth) ! Recursively count the nodes in the right child node
        END IF
    END SUBROUTINE recursion_count_nodes_with_one_leaf

    ! Subroutine to print depth of the tree and number of nodes with only one leaf

    SUBROUTINE print_depth_and_nodes_with_one_leaf(tree)
        IMPLICIT NONE
        TYPE(bst), INTENT(IN) :: tree ! BST to count the nodes in

        WRITE(*,*) "Depth of the tree is: ", find_depth(tree) ! Initialize the number of nodes to 0
        WRITE(*,*) "Number of nodes with only one leaf: ", count_nodes_with_one_leaf(tree) ! Recursively count the nodes in the BST
    END SUBROUTINE print_depth_and_nodes_with_one_leaf

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

        IF (ASSOCIATED (curr_node)) THEN ! If the current node is not NULL, free all allocated storage in the left and right child nodes
            CALL recursion_free(curr_node%left) ! Recursively free all allocated storage in the left child node
            CALL recursion_free(curr_node%right) ! Recursively free all allocated storage in the right child node
            DEALLOCATE(curr_node) ! Deallocate the current node
        END IF
    END SUBROUTINE recursion_free

    SUBROUTINE rebalance_tree(tree)
        IMPLICIT NONE
        TYPE(bst), INTENT(INOUT) :: tree ! BST to rebalance

        INTEGER :: num_nodes ! Number of nodes in the BST
        TYPE(pair), POINTER:: keys(:)

        num_nodes = count_nodes(tree) ! Get the number of nodes in the BST
        ALLOCATE(keys(num_nodes)) ! Allocate an array of keys
        CALL extract_tree(tree, keys)

        CALL free_bst(tree) ! Free all allocated storage in the BST
        CALL init_bst(tree) ! Initialize the BST
        CALL recursive_rebalance_tree(tree, keys, 1, num_nodes) ! Recursively rebalance the BST using the keys

        DEALLOCATE(keys) ! Deallocate the array of keys
    END SUBROUTINE rebalance_tree

    ! Recursively rebalances the BST
    RECURSIVE SUBROUTINE recursive_rebalance_tree(tree, keys, start, end)
        IMPLICIT NONE
        TYPE(bst), INTENT(INOUT) :: tree ! BST to rebalance
        TYPE(pair), POINTER, INTENT(IN) :: keys(:) ! Array of keys to rebalance the BST with
        INTEGER, INTENT(IN) :: start ! Start index of the keys array
        INTEGER, INTENT(IN) :: end ! End index of the keys array

        INTEGER :: mid ! Middle index of the keys array

        IF (start <= end) THEN ! If the start index is less than or equal to the end index, rebalance the BST
            mid = (start + end) / 2 ! Get the middle index of the keys array
            CALL add_bst(tree, keys(mid)) ! Add the middle key to the BST
            CALL recursive_rebalance_tree(tree, keys, start, mid - 1) ! Recursively rebalance the left child node
            CALL recursive_rebalance_tree(tree, keys, mid + 1, end) ! Recursively rebalance the right child node
        END IF
    END SUBROUTINE recursive_rebalance_tree


END MODULE bst_module

PROGRAM bst_test

    USE bst_module
    USE list_types

    IMPLICIT NONE

    INTEGER :: num, i, j, k! Number of pairs to add to the BST
    REAL :: chk, rv, time1, time2 ! Checksum and random value
    INTEGER, ALLOCATABLE, DIMENSION(:) :: idx ! Array of indices to add to the BST
    TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat !Array of pairs to add to the BST
    TYPE(pair) :: p ! Pair to lookup in the BST
    INTEGER, PARAMETER :: nlook = 5000
    INTEGER :: error ! Error code
    REAL :: test ! Test value
    TYPE(bst) :: tree ! BST to add the data to
    TYPE(pair), POINTER:: keys(:)


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

    !extract the keys from the BST
    ALLOCATE(keys(num))
    CALL extract_tree(tree, keys)
    WRITE (*,*) "The keys in the BST are: "
    DO i=1,num
      WRITE (*,*) keys(i)%key
    END DO


    ! fill idx array with randomly selected keys
    CALL RANDOM_SEED()
    ALLOCATE(idx(nlook))
    DO i=1,nlook
      CALL RANDOM_NUMBER(rv)
      j = INT(rv*num)+1
      idx(i) = dat(j)%key
    END DO

    WRITE (*,*) "Tree before rebalancing: "
    !count no. of nodes with in the tree
    WRITE(*,*) "Number of nodes in the tree: ", count_nodes(tree)

    CALL print_depth_and_nodes_with_one_leaf(tree) ! Print the depth of the tree and the number of nodes with only one leaf

    ! do array value lookups
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
    WRITE(*,FMT=666) nlook, 'Binary search tree lookups', (time2-time1)
    WRITE(*,*) 'Checksum:', chk
    test = chk ! save the checksum for later comparison

    ! rebalance the tree
    CALL CPU_TIME (time1)
    CALL rebalance_tree(tree)
    CALL CPU_TIME (time2)
    WRITE(*,FMT=666) nlook, 'Rebalancing the tree', (time2-time1)

    WRITE (*,*) "Tree after rebalancing: "
    !count no. of nodes with in the tree
    WRITE(*,*) "Number of nodes in the tree: ", count_nodes(tree)

    CALL print_depth_and_nodes_with_one_leaf(tree) ! Print the depth of the tree and the number of nodes with only one leaf

    ! do BST lookups
    chk = 0
    CALL CPU_TIME(time1)
    DO k=1,1000
      DO i=1,nlook
          ! Binary tree lookups here
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
  DEALLOCATE(dat,idx,keys)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM bst_test