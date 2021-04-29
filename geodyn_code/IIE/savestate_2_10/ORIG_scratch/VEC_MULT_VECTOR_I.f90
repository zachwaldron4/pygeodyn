!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_VECTOR_I ( U, STRIDE_U, V, STRIDE_V, COUNT, &
     &                               R, STRIDE_R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, V, R dimensioned COUNT:                                         *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! *   EAch vector has its own stride.                                    *
! *                                                                      *
! * ### 10-JUL-2003 VEC_MULT_VECTOR_I v1.0 (c) L. Petrov 19-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  COUNT, STRIDE_U, STRIDE_V, STRIDE_R
      INTEGER  IND_U, IND_V, IND_R, J1
      DOUBLE PRECISION U(*), V(*), R(*)
!
      IF ( COUNT .LE. 0 ) RETURN
      IND_U = 1
      IND_V = 1
      IND_R = 1
      DO 410 J1=1,COUNT
         R(IND_R) = U(IND_U)*V(IND_V)
         IND_U = IND_U + STRIDE_U
         IND_V = IND_V + STRIDE_V
         IND_R = IND_R + STRIDE_R
 410  CONTINUE
!
      RETURN
      END
