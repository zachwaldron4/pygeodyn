!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_CONSTANT ( U, COUNT, A, R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, R dimensioned COUNT and constant A:                             *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! * ### 10-JUL-2003 VEC_MULT_CONSTANT v1.0 (c) L. Petrov 10-JUL-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  COUNT, J1
      DOUBLE PRECISION U(COUNT), R(COUNT), A
!
      IF ( COUNT .LE. 0 ) RETURN
      DO 410 J1=1,COUNT
         R(J1) = A*U(J1)
 410  CONTINUE
! @ #endif
!
      RETURN
      END
