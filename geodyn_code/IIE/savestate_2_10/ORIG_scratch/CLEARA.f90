!$CLEARA
      SUBROUTINE CLEARA(A,N)
!********1*********2*********3*********4*********5*********6*********7**
! CLEARA           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CLEAR A REAL ARRAY WITH DIMENSION N
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A       I/O   A    REAL ARRAY TO BE CLEARED
!   N        I    S    DIMENSION OF THE ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION A(1)
      DATA ZERO/0.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(N.LE.0) RETURN
      K1 = 1
      K2 = N
      DO 1000 K=K1,K2
         A(K)=ZERO
 1000 END DO
      RETURN
      END
