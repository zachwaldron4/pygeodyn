!
!$DECOMP
      SUBROUTINE DECOMP(KPRE,LPRE,NPRE)
!********1*********2*********3*********4*********5*********6*********7**
! DECOMP           84/10/10            8410.0    PGMR - TOM MARTIN
!
! FUNCTION:  DECOMPOSE PREPROCESSING WORD FROM
!            GEODYN-II BINARY TRACKING DATA FORMAT
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   KPRE     I    S    GEODYN-II BINARY TRACKING DATA PREPRO WORD
!   LPRE     O    A    GEODYN-II BINARY TRACKING DATA PREPRO WORD
!                      DECOMPOSED INTO LOGICAL ARRAY
!   NPRE     I    S    NUMBER OF LEAST SIGNIFICANT BITS TO BE
!                      DECOMPOSED
!
! COMMENTS:
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
      JPRE=KPRE
      DO 1000 I=1,NPRE
      IPRE=JPRE/2
      IODD=JPRE-IPRE*2
      LPRE(I)=IODD.EQ.1
      JPRE=IPRE
 1000 END DO
      RETURN
      END
