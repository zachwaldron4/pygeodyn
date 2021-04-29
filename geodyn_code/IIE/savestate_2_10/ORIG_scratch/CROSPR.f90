!$CROSPR
      SUBROUTINE CROSPR( A, B, C )
!********1*********2*********3*********4*********5*********6*********7**
! CROSPR           00/00/00            0000.0    PGMR - JJM
!
!
! FUNCTION:  COMPUTES VECTOR CROSS PRODUCT OF 2 LENGTH = 3 VECTORS A AND
!            B - STORE RESULT IN C
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I    A    FIRST VECTOR - LENGTH = 3
!   B        I    A    SECOND VECTOR - LENGTH = 3
!   C        O    A    RESULTING VECTOR = A X B
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION A(3),B(3),C(3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!     ....THIS SUBROUTINE COMPUTES THE CROSS PRODUCT
!     ....OF VECTORS A AND B AND STORES THE RESULT IN C
!
      C(1) = A(2) * B(3) - A(3) * B(2)
      C(2) = A(3) * B(1) - A(1) * B(3)
      C(3) = A(1) * B(2) - A(2) * B(1)
!
      RETURN
      END
