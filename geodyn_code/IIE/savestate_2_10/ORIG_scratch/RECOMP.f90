!$RECOMP
      SUBROUTINE RECOMP(LPRE,KPRE,NPRE)
!********1*********2*********3*********4*********5*********6*********7**
! RECOMP           84/10/10            8410.0    PGMR - TOM MARTIN
!
! FUNCTION:  RECOMPOSE GEODYN-II BINARY TRACKING DATA FORMAT
!            PREPROCESSING WORD FROM LOGICAL ARRAY
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LPRE     I    A    GEODYN-II BINARY TRACKING DATA PREPRO WORD
!                      DECOMPOSED INTO LOGICAL ARRAY
!   KPRE     O    S    GEODYN-II BINARY TRACKING DATA PREPRO WORD
!   NPRE     I    S    NUMBER OF LEAST SIGNIFICANT BITS TO BE
!                      RECOMPOSED
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION LPRE(NPRE)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      INDEX=NPRE
      KPRE=0
      DO 1000 I=1,NPRE
      KPRE=KPRE*2
      IF(LPRE(INDEX)) KPRE=KPRE+1
      INDEX=INDEX-1
 1000 END DO
      RETURN
      END
