PROGRAM pointers
  REAL, POINTER :: q => NULL()
  REAL, TARGET  :: c = 0.0, d = -1.0

  PRINT*,ASSOCIATED(q)  ! prints F
  q => c
  PRINT*,ASSOCIATED(q)  ! prints T
  PRINT*,c,q            ! prints 0.0  0.0
  c = 1.0
  PRINT*,c,q            ! prints 1.0  1.0
  q = 2.0
  PRINT*,c,q            ! prints 2.0  2.0
  q => d
  PRINT*,c,q            ! prints 2.0 -1.0
END PROGRAM pointers
