!$FRMWTL
      SUBROUTINE FRMWTL(IUNT,LARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! FRMWTL           00/00/00            0000.0    PGMR - SBL
!
! FUNCTION:  WRITE OUT LOGICAL CHARACTER FORMATTED RECORDS TO THE
!            SPECIFIED UNIT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO WRITE ON
!   LARRAY   I    A    ARRAY TO BE WRITTEN OUT
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
      DIMENSION LARRAY(ISIZE)
!
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(LARECL) THEN
      WRITE (IUNT,10) LARRAY
   10 FORMAT(80L1)
      ELSE
      WRITE (IUNT,11) LARRAY
!     WRITE (6,11) LARRAY
   11 FORMAT(132L1)
      ENDIF
      RETURN
      END
