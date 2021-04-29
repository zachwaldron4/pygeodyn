!$CROPDT
      SUBROUTINE CROPDT(A,B,C)
!********1*********2*********3*********4*********5*********6*********7**
! CROPDT          07/07/92        0000.0  WRITTEN BY LUCIA TSAOUSSI
!
! FUNCTION:        COMPUTE THE CROSS PRODUCT OF TWO VECTORS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!     A      I    A    DYNAMIC ARRAY FOR REAL VARIABLES.
!     B      I    A    DYNAMIC ARRAY FOR INTEGER VARIABLES
!     C      O    A    FOR A GIVEN INTERNAL STA NO (1,2,3) INDSTA
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      DIMENSION A(3),B(3),C(3)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE
!***********************************************************************
!
      C(1) = A(2) * B(3) - A(3) * B(2)
      C(2) = A(3) * B(1) - A(1) * B(3)
      C(3) = A(1) * B(2) - A(2) * B(1)
      RETURN
      END
