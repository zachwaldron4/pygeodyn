!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_VECTOR ( U, V, COUNT, R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, V, R dimensioned COUNT:                                         *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! * ### 10-JUL-2003  VEC_MULT_VECTOR  v1.0 (c) L. Petrov 10-JUL-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  COUNT, J1
      DOUBLE PRECISION U(COUNT), V(COUNT), R(COUNT)
!
      IF ( COUNT .LE. 0 ) RETURN
      DO 410 J1=1,COUNT
         R(J1) = U(J1)*V(J1)
 410  CONTINUE
! @ #endif
!
      RETURN
      END
