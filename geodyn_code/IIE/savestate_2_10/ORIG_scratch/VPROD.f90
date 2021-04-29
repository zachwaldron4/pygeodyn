      SUBROUTINE VPROD(A,B,VP)
!********1*********2*********3*********4*********5*********6*********7**
! VPROD            00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  THIS SUBROUTINE COMPUTES THE 3-D VECTOR PRODUCT VP = B X A.
!
! I/O PARAMETERS:
!
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I    A    INPUT VECTOR A
!   B        I    A    INPUT VECTOR B
!   VP       O    A    OUTPUT PRODUCT BXA
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL (L)
      SAVE
      DIMENSION A(3),B(3),VP(3)
      VP(1) = A(2)*B(3)-A(3)*B(2)
      VP(2) = A(3)*B(1)-A(1)*B(3)
      VP(3) = A(1)*B(2)-A(2)*B(1)
      RETURN
      END
