!$BINWRT
      SUBROUTINE BINWRT(IUNT,IARRAY,ISIZE)
! *******1*********2*********3*********4*********5*********6*********7**
! BINWRT           00/00/00            0000.0    PGMR - WFE
!                                                PGMR - SBL
!
!
! FUNCTION:  WRITE OUT VARIABLE LENGTH BINARY RECORDS TO
!            THE SPECIFIED UNIT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S     UNIT NUMBER TO WRITE ON
!   IARRAY   I    A     ARRAY TO BE WRITTEN OUT
!   ISIZE    I    S     NUMBER OF WORDS OF ARRAY TO OUTPUT
!
! COMMENTS:  IF AN OUTPUT RECORD OF ZERO LENGTH IS REQUESTED
!            A RECORD CONTAINING ONE INTEGER WORD (ZERO) IS OUTPUT
!
! *******1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION IARRAY(1)
!
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
!
      DATA IZERO/0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      IF(LASCII) THEN
      IF(ISIZE.GT.0) GO TO 100
      WRITE(IUNT,17) IZERO
   17 FORMAT(I25)
      RETURN
  100 CONTINUE
      CALL FRMWRT(IUNT,IARRAY,ISIZE)
      RETURN
      ELSE
      IF(ISIZE.GT.0) GO TO 300
      WRITE(IUNT) IZERO
      RETURN
  300 CONTINUE
      CALL UNFWRT(IUNT,IARRAY,ISIZE)
      RETURN
      ENDIF
      END
