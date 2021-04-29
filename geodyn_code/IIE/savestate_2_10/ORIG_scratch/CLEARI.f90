!$CLEARI
      SUBROUTINE CLEARI(I,N)
!********1*********2*********3*********4*********5*********6*********7**
! CLEARI           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CLEAR AN INTEGER ARRAY WITH DIMENSION N
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   I       I/O   A    INTEGER ARRAY TO BE CLEARED
!   N        I    S    DIMENSION OF THE ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION I(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(N.LE.0) RETURN
      DO 2000 K1=1,N,32768
      K2=MIN(K1+32767,N)
      DO 1000 K=K1,K2
      I(K)=0
 1000 END DO
 2000 END DO
      RETURN
      END
