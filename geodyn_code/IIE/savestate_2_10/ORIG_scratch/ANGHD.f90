!$ANGHD
      SUBROUTINE ANGHD(INDSAT,INDSTA)
!********1*********2*********3*********4*********5*********6*********7**
! ANGHD            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  WRITES LOCATION DATA HEADER RECORD FOR ANGULAR DATA
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INDSAT   I    A    Internal satellite numbers
!   INDSTA   I    A    Internal station numbers
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBINRI/NSATB,NSTAB,INMSAT(3),INMSTA(3)
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION  INDSAT(3,4),INDSTA(3,4),XPSEQ(20),                     &
     &           ILWSTA(3),ILWSAT(3),SUB(3)
!
      CHARACTER(8)      :: SUB
      DATA SUB/'MTSTST','MTSSST','MTPTT '/
      DATA MSATB/3/,MSTAB/3/
      DATA ILWSTA /11,13,15/,ILWSAT/12,14,16/
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
      KPSEQ=2
      XPSEQ(1)=DBLE(ILWSTA(1))
      XPSEQ(2)=DBLE(ILWSAT(1))
! WRITE RECORD
      WRITE(IUNT19) XPSEQ
      RETURN
      END
