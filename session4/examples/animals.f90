MODULE zoo
  IMPLICIT NONE
  PRIVATE

  TYPE animal
      CHARACTER(len=10) :: word
    CONTAINS
      PROCEDURE :: say
      FINAL :: theend
  END TYPE animal

  TYPE, EXTENDS(animal) :: dog
      LOGICAL :: left = .true.
    CONTAINS
      PROCEDURE :: wag
      PROCEDURE :: tail
  END TYPE dog

  TYPE cat
      CHARACTER(len=10) :: word
    CONTAINS
      PROCEDURE :: meow
  END TYPE cat

  INTERFACE animal
      MODULE PROCEDURE init_animal
  END INTERFACE animal

  INTERFACE dog
      MODULE PROCEDURE init_dog
  END INTERFACE dog

  PUBLIC :: animal, dog

CONTAINS

  TYPE(animal) FUNCTION init_animal(w)
    CHARACTER(len=10), INTENT(in) :: w
    init_animal%word = w
  END FUNCTION init_animal

  SUBROUTINE theend(this)
    TYPE(animal) :: this
    PRINT*,'the end of me, ',this%word,'!'
  END SUBROUTINE theend

  SUBROUTINE say(self)
    CLASS(animal), INTENT(inout) :: self
    PRINT*, 'This animal says ',TRIM(self%word)
  END SUBROUTINE say

  SUBROUTINE wag(this)
    CLASS(dog), INTENT(inout) :: this
    this%left = .not. this%left
  END SUBROUTINE wag

  CHARACTER(len=10) FUNCTION tail(self)
    CLASS(dog), INTENT(inout) :: self
    IF (self%left) THEN
        tail = 'left'
    ELSE
        tail = 'right'
    END IF
  END FUNCTION tail

  TYPE(dog) FUNCTION init_dog(w,t)
    CHARACTER(len=10), INTENT(in) :: w
    LOGICAL, OPTIONAL, INTENT(in) :: t
    init_dog%word = w
    IF (PRESENT(t)) THEN
        init_dog%left = t
    END IF
  END FUNCTION init_dog

  SUBROUTINE meow(this)
    CLASS(cat) :: this
    PRINT*,'Meow!',this%word
  END SUBROUTINE meow

END MODULE zoo

PROGRAM sounds
  USE zoo
  IMPLICIT NONE
  TYPE(animal) :: one
  TYPE(dog)    :: two
  CLASS(dog), ALLOCATABLE :: three

  one = animal('meow')
  two = dog('woof')
  ALLOCATE(three)
  three = dog('bark',.false.)
  CALL one%say
  CALL two%say
  CALL three%say
  DEALLOCATE(three)
  PRINT*,'tail points to the ',two%tail()
  CALL two%wag
  PRINT*,'tail points to the ',two%tail()
  CALL two%wag
  PRINT*,'tail points to the ',two%tail()
END PROGRAM sounds

