! Program another stack class with the same API as in section 19 and use a linked list as backing
! storage this time. Discuss benefits and disadvantages of these two kinds of stack classes.

!Advantages:

!In the array-based implementation, the size of the stack is fixed at the time of initialization. 
!However, in the linked list implementation, the size can be increased or decreased dynamically as needed by adding or removing nodes. 
!This can be useful in cases where the maximum size of the stack is unknown or may change during the program execution.

!Disadvantages:

!In the array-based implementation, the stack is stored in a contiguous block of memory.
!This means that the stack can be accessed quickly and efficiently.
!However, in the linked list implementation, the stack is stored in a non-contiguous block of memory.
  
MODULE list_linked_list

    IMPLICIT NONE
    
    TYPE list_node  ! Node of a linked list of integers 
        INTEGER :: data
        TYPE(list_node), POINTER :: next => NULL()
    END TYPE list_node

    TYPE list      ! Linked list of integers 
        PRIVATE
        TYPE(list_node), POINTER :: head => NULL()
        INTEGER :: size = 0
    END TYPE list

    PUBLIC :: default_list, copy_list, free_list, push_list, pop_list, length_list  ! Public interface

    CONTAINS

    ! Initializes a list
    SUBROUTINE default_list(l)
        TYPE(list), INTENT(INOUT) :: l
        l%head => NULL()  ! Initialize head pointer to NULL so that we can check if the list is empty
        l%size = 0
    END SUBROUTINE default_list

    ! Copies the contents of one list to another
    SUBROUTINE copy_list(l1, l2)
        TYPE(list), INTENT(IN) :: l1
        TYPE(list), INTENT(INOUT) :: l2
        TYPE(list_node), POINTER :: curr_node, prev_node => NULL() ! prev_node is the node that will be added to l2 and curr_node is the node that will be traversed in l1
        l2%size = l1%size
        curr_node => l1%head
        DO WHILE (ASSOCIATED(curr_node))  ! Traverse l1 and add each node to l2 in reverse order
            ALLOCATE(prev_node)  ! Allocate memory for the new node
            prev_node%data = curr_node%data   ! Copy the data from the current node to the new node
            prev_node%next => l2%head  ! Set the next pointer of the new node to the current head of l2
            l2%head => prev_node  ! Add the new node to the front of l2
            curr_node => curr_node%next  ! Move to the next node in l1
        END DO
    END SUBROUTINE copy_list

    ! Frees the memory used by a list
    SUBROUTINE free_list(l)
        TYPE(list), INTENT(INOUT) :: l
        TYPE(list_node), POINTER :: curr_node, next_node => NULL()  ! curr_node is the node that will be freed and next_node is the node that will be traversed in l
        curr_node => l%head
        DO WHILE (ASSOCIATED(curr_node))  ! Traverse l and free each node
            next_node => curr_node%next  ! Store the next node in l
            DEALLOCATE(curr_node) ! Free the current node
            curr_node => next_node ! Move to the next node in l
        END DO
        l%head => NULL()
        l%size = 0
    END SUBROUTINE free_list

    ! Pushes an element onto the front of a list
    SUBROUTINE push_list(l, x)
        TYPE(list), INTENT(INOUT) :: l  ! l is the list that we are pushing to
        INTEGER :: x
        TYPE(list_node), POINTER :: new_node  ! new_node is the node that we are adding to l
        ALLOCATE(new_node)
        new_node%data = x  ! Set the data of the new node to x
        new_node%next => l%head  ! Set the next pointer of the new node to the current head of l
        l%head => new_node  ! Add the new node to the front of l
        l%size = l%size + 1  ! Increment the size of l
    END SUBROUTINE push_list

    ! Pops the front element off of a list and returns its value
    SUBROUTINE pop_list(l, x)
        TYPE(list), INTENT(INOUT) :: l
        INTEGER :: x
        TYPE(list_node), POINTER :: old_node
        IF (l%size == 0) THEN
            WRITE(*,*) "List is empty"
            STOP
        END IF
        old_node => l%head  ! old_node is the node that we are removing from l
        x = old_node%data  ! Set x to the data of the old node
        l%head => old_node%next  ! Set the head of l to the next node in l
        DEALLOCATE(old_node)  ! Free the old node
        l%size = l%size - 1 ! Decrement the size of l by 1 so that it is accurate
    END SUBROUTINE pop_list

    ! Returns the number of elements in a list
    FUNCTION length_list(l) result (n)
        TYPE(list), INTENT(IN) :: l
        INTEGER :: n
        n = l%size      ! Return the size of l which is the number of elements in l
    END FUNCTION length_list

END MODULE list_linked_list


! Test program for list module l1 and l2 are two list objects also ass timing for the two different list types

PROGRAM test_list
    USE list_linked_list
    IMPLICIT NONE
    
    TYPE(list) :: l1, l2
    INTEGER :: i, x
    
    ! fill s1 with data
    CALL default_list(l1)

    DO i = 1, 5
        CALL push_list(l1, i)
        WRITE (*,*) "Pushed to l1: ", i
    END DO

    
    ! print the length of l1
    WRITE (*,*) "Length of l1: ", length_list(l1)

    ! create l2 as a copy of l1
    CALL copy_list(l1, l2)

    ! print the length of l2
    WRITE (*,*) "Length of l2: ", length_list(l2)
    
    ! add more data to both s1 and s2
    DO i = 6, 10
        CALL push_list(l1, i)
        WRITE (*,*) "Pushed to l1 more data: ", i
        CALL push_list(l2, i**2)
        WRITE (*,*) "Pushed to l2 more data: ", i**2
    END DO
    
    ! empty both stacks
    DO WHILE (length_list(l1) > 0)
        CALL pop_list(l1, x)
        WRITE(*,*) "Popped from l1: ", x
    END DO
    
    DO WHILE (length_list(l2) > 0)
        CALL pop_list(l2, x)
        WRITE(*,*) "Popped from l2: ", x 
    END DO
    
    ! free allocated memory
    CALL free_list(l1)
    CALL free_list(l2)

    WRITE (*,*) "Done"
    
END PROGRAM test_list







