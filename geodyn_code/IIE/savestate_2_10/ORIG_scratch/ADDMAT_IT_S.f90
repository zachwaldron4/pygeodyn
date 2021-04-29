!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADDMAT_IT_S ( M, A, B )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_IT_S  add to the quadratic matrix A the matrix to be  *
! *   transposed with respect to A and put the result in symmetric       *
! *   matrix B:   B = A + A(T).                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   M ( INTEGER*4 ) -- Dimension of the matrix A.                      *
! *   A ( INTEGER*4 ) -- Input quadratic matrix. Dimension: M*M          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   B ( REAL*8    ) -- Output symmetric matrix in the upper triangular *
! *                      representation.                                 *
! *                                                                      *
! *  ###  16-JUL-99  ADDMAT_IT_S   v1.0  (c)  L. Petrov  16-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  M, J1, J2, LC
      DOUBLE PRECISION A(M,M), B(*)
!
      LC = 0
      DO 410 J1=1,M
         DO 420 J2=1,J1
            LC=LC+1
            B(LC) = A(J1,J2) + A(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END
