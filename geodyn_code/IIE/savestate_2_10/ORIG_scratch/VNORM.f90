      SUBROUTINE VNORM(V,VN,RNORM)
!********1*********2*********3*********4*********5*********6*********7**
! VNORM            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  THIS SUBROUTINE FINDS THE UNIT 3-D VECTOR  VN  POINTING
!            WITH THE DIRECTION AND SENSE OF  V .
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   V        I    A    INPUT VECTOR V
!   VN       O    A    OUTPUT UNIT VECTOR
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION   (A-H,O-Z),LOGICAL (L)
      SAVE
      DIMENSION V(3),VN(3)
      RNORMQ = 0.D0
      DO 10    I = 1,3
   10 RNORMQ= RNORMQ+V(I)**2
      RNORM = SQRT(RNORMQ)
      RNORMI = 1.D0/RNORM
      DO 20    I = 1,3
   20 VN(I) = V(I)*RNORMI
      RETURN
      END
