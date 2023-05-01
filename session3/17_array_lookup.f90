

PROGRAM array_lookup
  USE list_types
  IMPLICIT NONE

  INTEGER :: num, i, j, k
  REAL :: chk, rv, time1, time2
  INTEGER, ALLOCATABLE, DIMENSION(:) :: idx
  TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat
  TYPE(pair) :: p
  INTEGER, PARAMETER :: nlook = 5000

  READ(5,*) num
  ALLOCATE(dat(num))
  READ(5,*) (dat(i),i=1,num)
  READ(5,*) chk

  ! XXX fill linked list or hash table with items from dat() here

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

  chk = 0
  CALL CPU_TIME(time1)
  DO k=1,1000
      DO i=1,nlook
          ! XXX do linked list or hash table lookups here
          chk = chk + p%key
      END DO
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'XXXX XXXX lookups', (time2-time1)
  WRITE(*,*) 'Checksum:', chk

  ! XXX free all allocated data
  DEALLOCATE(dat,idx)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM array_lookup
