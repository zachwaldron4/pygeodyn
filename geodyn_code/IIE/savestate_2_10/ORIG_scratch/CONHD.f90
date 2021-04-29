!$CONHD
      SUBROUTINE CONHD
!********1*********2*********3*********4*********5*********6*********7**
! CONHD
!
!
! FUNCTION:  WRITES LOCATION DATA HEADER RECORD FOR ALT. CONSTRAINTS AND
!            IMAGE DATA TO BINARY RESIDUAL FILE ON UNIT-19
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION  XPSEQ(20)
!
      DATA ZERO/0.D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!
      DO 100 I=1,20
      XPSEQ(I)=ZERO
  100 END DO
!
      XPSEQ(1)=3.0
! WRITE RECORD
      WRITE(IUNT19) XPSEQ
      RETURN
      END
