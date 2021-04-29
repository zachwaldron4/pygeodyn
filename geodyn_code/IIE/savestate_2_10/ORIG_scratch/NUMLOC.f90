!$NUMLOC
      FUNCTION NUMLOC(N,IA,NA)
!********1*********2*********3*********4*********5*********6*********7**
! NUMLOC           00/00/00            0000.0    PGMR - ?
!
!
!
! FUNCTION:  LOCATE A NUMBER N IN AN INTEGER ARRAY IA
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N        I    S    NUMBER TO BE LOCATED
!   IA       I    A    INTEGER ARRAY TO BE SEARCHED
!   NA       I    S    DIMENSION OF IA
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION IA(NA)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      NUMLOC=0
      IF(NA.LE.0) RETURN
      DO 1000 I=1,NA
      IF(N.NE.IA(I)) GO TO 1000
      NUMLOC=I
      RETURN
 1000 END DO
      RETURN
      END
