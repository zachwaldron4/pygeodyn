      SUBROUTINE SPROD(A,B,DOTAB)
!********1*********2*********3*********4*********5*********6*********7**
! SPROD            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  THIS SUBROUTINE COMPUTES THE SCALAR PRODUCT   DOTAB = A.B
!            OF THE TWO 3-D VECTORS A AND B.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I    A    INPUT VECTOR A
!   B        I    A    INPUT VECTOR B
!   DOTAB    O    A    OUTPUT DOT PRODUCT A*B
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL (L)
      SAVE
      DIMENSION A(3),B(3)
      DOTAB = 0.D0
      DO 10   I = 1,3
   10 DOTAB = DOTAB+A(I)*B(I)
      RETURN
      END
