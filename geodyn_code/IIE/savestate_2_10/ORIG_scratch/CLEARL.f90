!$CLEARL
      SUBROUTINE CLEARL(L,N)
!********1*********2*********3*********4*********5*********6*********7**
! CLEARL           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CLEAR A LOGICAL ARRAY WITH DIMENSION N
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   L       I/O   A    LOGICAL ARRAY TO BE CLEARED
!   N        I    S    DIMENSION OF THE ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION L(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(N.LE.0) RETURN
      DO 2000 K1=1,N,32768
      K2=MIN(K1+32767,N)
      DO 1000 K=K1,K2
      L(K)=.FALSE.
 1000 END DO
 2000 END DO
      RETURN
      END
