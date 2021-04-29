!$FRMWRT
      SUBROUTINE FRMWRT(IUNT,IARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! FRMWRT           00/00/00            0000.0    PGMR - SBL
!
! FUNCTION:  WRITE OUT FORMATTED RECORDS TO THE SPECIFIED UNIT.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO WRITE ON
!   IARRAY   I    A    ARRAY TO BE WRITTEN OUT
!   ISIZE    I    S    NUMBER OF WORDS OF ARRAY TO OUTPUT
!
! COMMENTS:
!                  THIS ROUTINE DIFFERS FROM BINWRT IN THAT NO TEST
!                  IS MADE FOR ZERO LENGTH RECORDS AND NO DUMMY RECORD
!                  IS OUTPUT.
!
! RESTRICTIONS:
!                  ISIZE MUST BE GREATER THAN 0.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION IARRAY(ISIZE)
!
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(LARECL) THEN
      WRITE (IUNT,10) IARRAY
   10 FORMAT(3I25)
      ELSE
      WRITE (IUNT,11) IARRAY
   11 FORMAT(5I25)
      ENDIF
      RETURN
      END
