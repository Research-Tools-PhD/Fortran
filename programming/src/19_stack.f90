! Implement a stack class for integers using a dynamically allocated array as backing storage.
! Implement a default constructor and a copy constructor, a free() function, and push(), pop() and
! length() methods. Write a test program that instantiates one stack object, fills it with data, then
! instantiates a second via the copy constructor from the first, and adds more data to both. Empty
! both stacks in a while loop each. Finally free all allocated storage and end.

MODULE stack_module

    IMPLICIT NONE
    
    TYPE stack     
        PRIVATE
        INTEGER, dimension(:), ALLOCATABLE :: dat ! backing storage
        INTEGER :: size                ! number of elements in stack
    END TYPE stack

    PUBLIC :: default_stack, copy_stack, free_stack, push_stack, pop_stack, length_stack

    CONTAINS

    ! Initializes a stack with a default size of 10 or a user-specified size
    SUBROUTINE default_stack(s, n)
        TYPE(stack), INTENT(INOUT) :: s
        INTEGER, OPTIONAL, INTENT(IN) :: n
        ! allocate the array with a default size of 10
        IF (PRESENT(n)) THEN
            ALLOCATE(s%dat(1:n))
        ELSE
            ALLOCATE(s%dat(1:10))
        END IF
        ! initialize the size to zero (empty stack)
        s%size = 0
    END SUBROUTINE default_stack

    ! Copies the contents of one stack to another
    SUBROUTINE copy_stack(s1, s2)
        TYPE(stack), INTENT(IN) :: s1
        TYPE(stack), INTENT(INOUT) :: s2
        s2%size = s1%size
        ALLOCATE (s2%dat(1:s1%size))
        s2%dat(1:s1%size) = s1%dat(1:s1%size)
    END SUBROUTINE copy_stack

    ! Frees the memory used by a stack
    SUBROUTINE free_stack(s)
        TYPE(stack), INTENT(INOUT) :: s
        IF (ALLOCATED(s%dat)) THEN
            DEALLOCATE(s%dat)
        END IF
    END SUBROUTINE free_stack

    ! Pushes an element onto the top of a stack
    SUBROUTINE push_stack(s, x)
        TYPE(stack), INTENT(INOUT) :: s
        INTEGER, INTENT(IN) :: x
        INTEGER :: i, n_size, o_size
        INTEGER, dimension(:), ALLOCATABLE :: n_dat, o_dat

        IF (s%size == SIZE(s%dat)) THEN
            ! allocate a new array with double the size
            o_dat = s%dat
            o_size = SIZE(o_dat)
            n_size = 2*o_size
            ALLOCATE(n_dat(n_size))
            DO i = 1, o_size
                n_dat(i) = o_dat(i)
            END DO

            ! deallocate the old array and assign the new one
            DEALLOCATE(o_dat)
            s%dat = n_dat
        END IF
        s%size = s%size + 1
        s%dat(s%size) = x
    END SUBROUTINE push_stack

    ! Pops the top element off of a stack and returns its value
    SUBROUTINE pop_stack(s, x)
        TYPE(stack), INTENT(INOUT) :: s
        INTEGER, INTENT(OUT) :: x
        INTEGER :: i, n_size, o_size
        INTEGER, dimension(:), ALLOCATABLE :: n_dat, o_dat

        IF (s%size == 0) THEN
            WRITE(*,*) "Stack is empty"
            STOP
        END IF

        x = s%dat(s%size)
        s%size = s%size - 1

        IF (s%size < SIZE(s%dat)/4) THEN
            ! allocate a new array with half the size
            o_dat = s%dat
            o_size = SIZE(o_dat)
            n_size = o_size/2
            ALLOCATE(n_dat(n_size))
            DO i = 1, n_size
                n_dat(i) = o_dat(i)
            END DO

            ! deallocate the old array and assign the new one
            DEALLOCATE(o_dat)
            s%dat = n_dat
        END IF
    END SUBROUTINE pop_stack

    function length_stack(s) result (n)
        TYPE(stack), INTENT(IN) :: s
        INTEGER :: n
        n = s%size
    end function length_stack

END MODULE stack_module


! Test program for stack module s1 and s2 are two stack objects

PROGRAM test_stack
    USE stack_module
    IMPLICIT NONE
    
    TYPE(stack) :: s1, s2
    INTEGER :: i, x
    
    ! fill s1 with data
    CALL default_stack(s1)
    DO i = 1, 5
        CALL push_stack(s1, i)
        WRITE (*,*) "Pushed to s1: ", i
    END DO
    
    ! print the length of s1
    WRITE (*,*) "Length of s1: ", length_stack(s1)

    ! create s2 as a copy of s1
    CALL copy_stack(s1, s2)

    ! print the length of s2
    WRITE (*,*) "Length of s2: ", length_stack(s2)
    
    ! add more data to both s1 and s2
    DO i = 6, 10
        CALL push_stack(s1, i)
        WRITE (*,*) "Pushed to s1 more data: ", i
        CALL push_stack(s2, i**2)
        WRITE (*,*) "Pushed to s2 more data: ", i**2
    END DO
    
    ! empty both stacks
    DO WHILE (length_stack(s1) > 0)
        CALL pop_stack(s1, x)
        WRITE(*,*) "Popped from s1: ", x
    END DO
    
    DO WHILE (length_stack(s2) > 0)
        CALL pop_stack(s2, x)
        WRITE(*,*) "Popped from s2: ", x
    END DO
    
    ! free allocated memory
    CALL free_stack(s1)
    CALL free_stack(s2)

    WRITE (*,*) "Done"
    
END PROGRAM test_stack







