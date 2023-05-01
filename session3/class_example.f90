MODULE nodes
  IMPLICIT NONE
  ! all module elements are made private by default
  ! except for those exported explicitly
  PRIVATE

  ! define derived type with embedded procedures, i.e. class
  TYPE node
      INTEGER :: val
      TYPE(node),POINTER :: next
    CONTAINS
      PROCEDURE :: get
      PROCEDURE :: set
      PROCEDURE :: append
  END TYPE node
  
  ! interface for overloaded constructor functions
  INTERFACE node
      MODULE PROCEDURE node_default
      MODULE PROCEDURE node_copy
      MODULE PROCEDURE node_val
  END INTERFACE node

  ! we export the derived type and its interface only
  PUBLIC :: node

CONTAINS
  ! all procedures to be used as constructor
  ! have to init and return an element of the derived type
  ! no arguments -> default constructor
  TYPE(node) FUNCTION node_default()
    node_default%next => NULL()
    node_default%val = -1
  END FUNCTION node_default
  
  ! copy constructor
  TYPE(node) FUNCTION node_copy(n)
    TYPE(node) :: n
    node_copy%next => n%next
    node_copy%val = n%val
  END FUNCTION node_copy

  ! custom constructor (changes initialization of payload)
  TYPE(node) FUNCTION node_val(i)
    INTEGER,INTENT(in) :: i
    node_val%next => NULL()
    node_val%val = i
  END FUNCTION node_val

  ! member function of derived type (i.e. class)
  ! first argument must be of the derived type and must be declared as CLASS not type
  INTEGER FUNCTION get(self)
    CLASS(node) :: self
    get = self%val
  END FUNCTION get

  ! member subroutine of derived type
  ! also here the first argument must be of the derived type and declared as CLASS 
  SUBROUTINE set(self,i)
    CLASS(node) :: self
    INTEGER,INTENT(in) :: i
    self%val = i
  END SUBROUTINE set

  ! another member subroutine. this will "connect" an element to the end of the list
  SUBROUTINE append(self,n)
    CLASS(node),TARGET :: self
    TYPE(node),POINTER :: n,ptr
    ptr => self
    DO WHILE (ASSOCIATED(ptr%next))
        ptr => ptr%next
    END DO
    ptr%next => n
  END SUBROUTINE append

END MODULE nodes


PROGRAM test_node
  USE nodes
  IMPLICIT NONE
  TYPE(node),POINTER :: head,nn,ptr
  TYPE(node),TARGET :: n
  INTEGER :: i

  ALLOCATE(head,nn)
  ! initialize class instances with different overloaded constructors
  head = node(1)
  nn = node()
  n = node()
  ptr => n
  ! make use of instances
  CALL n%set(20)
  CALL head%append(nn)
  CALL head%append(ptr)

  PRINT*,'val=', head%get(), n%get(), nn%get()

  ! loop through list from the head
  ptr => head
  i = 1
  DO WHILE (ASSOCIATED(ptr))
      PRINT*,i,' -> ',ptr%get()
      ptr => ptr%next
      i = i+1
  END DO

  ! need to free dynamically allocated storage
  DEALLOCATE(head,nn)
  PRINT*,'done'
END PROGRAM test_node
