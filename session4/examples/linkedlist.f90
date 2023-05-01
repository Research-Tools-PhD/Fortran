PROGRAM linkedlist
  TYPE llitem
      TYPE(llitem), POINTER :: next => NULL()
      INTEGER :: val
  END TYPE llitem
  TYPE(llitem), POINTER :: head, item

  ALLOCATE(item)
  item%val = 1
  head => item
  ALLOCATE(item)
  item%val = 2
  item%next => head
  head => item
  ALLOCATE(item)
  item%val = 3
  item%next => head
  head => item

  item => head
  DO WHILE (ASSOCIATED(item))
      PRINT*, item%val
      item => item%next
  END DO

  DO WHILE (ASSOCIATED(head))
      item => head%next
      DEALLOCATE(head)
      head => item
  END DO

END PROGRAM linkedlist
