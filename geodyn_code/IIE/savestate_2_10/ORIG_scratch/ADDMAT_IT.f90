!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADDMAT_IT ( M1, M2, A, B )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_IT  add to the matrix A the matrix to be transposed   *
! *   with respect to B:    A = A + B(T).                                *
! *                                                                      *
! * ________________________ INPUT PARAMETERS: _________________________ *
! *                                                                      *
! *   M ( INTEGER*4 ) -- the first dimension of the matrix A.            *
! *   N ( INTEGER*4 ) -- the second dimension of the matrix A.           *
! *   B ( INTEGER*4 ) -- the second matrix. Dimension: M2*M1 .           *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *   A ( REAL*8    ) -- The first matrix. Dimension: M1*M2 .            *
! *                                                                      *
! *  ###  21-Jan-97   ADDMAT_IT    v1.1  (c)  L. Petrov  23-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  M1, M2, J1, J2
      DOUBLE PRECISION A(M1,M2), B(M2,M1)
!
      DO 410 J1=1,M1
         DO 420 J2=1,M2
            A(J1,J2) = A(J1,J2) + B(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END
