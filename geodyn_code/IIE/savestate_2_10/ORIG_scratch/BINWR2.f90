!$BINWR2
      SUBROUTINE BINWR2(IUNT,ARRAY,ISIZE)
! *******1*********2*********3*********4*********5*********6*********7**
! BINWR2           00/00/00            0000.0    PGMR - WFE
!                                                PGMR - SBL
!
! FUNCTION:  WRITE OUT VARIABLE LENGTH DOUBLE WORD RECORDS TO
!            THE SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO WRITE ON
!   ARRAY    I    A    DOUBLE WORD ARRAY TO BE WRITTEN OUT
!   ISIZE    I    S    NUMBER OF DOUBLE WORDS OF ARRAY TO OUTPUT
!
! COMMENTS:  IF AN OUTPUT RECORD OF ZERO LENGTH IS REQUESTED
!            A RECORD CONTAINING ONE DOUBLE WORD (ZERO) IS OUTPUT
!
!
! *******1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
!
      DIMENSION ARRAY(1)
!
      DATA ZERO/0.0D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(LASCII) THEN
      IF(ISIZE.GT.0) GO TO 100
      WRITE(IUNT,17)  ZERO
   17 FORMAT(D24.16)
      RETURN
  100 CONTINUE
      CALL FRMWR2(IUNT,ARRAY,ISIZE)
      RETURN
      ELSE
      IF(ISIZE.GT.0) GO TO 200
      WRITE(IUNT)  ZERO
      RETURN
  200 CONTINUE
      CALL UNFWR2(IUNT,ARRAY,ISIZE)
      RETURN
      ENDIF
      END
