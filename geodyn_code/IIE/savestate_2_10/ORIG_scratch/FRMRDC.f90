!$FRMRDC
      SUBROUTINE FRMRDC(IUNT,ARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! FRMRDC           00/00/00            0000.0    PGMR - SBL
!
! FUNCTION:  WRITE OUT FORMATTED CHARACTER RECORDS TO THE
!            SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO WRITE ON
!   ARRAY    I    A    DOUBLE WORD ARRAY TO BE WRITTEN OUT
!   ISIZE    I    S    NUMBER OF DOUBLE WORDS OF ARRAY TO OUTPUT
!
! COMMENTS:
!                  THIS ROUTINE DIFFERS FROM BINWR2 IN THAT NO TEST
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
      DIMENSION ARRAY(ISIZE)
!
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(LARECL) THEN
      READ(IUNT,10) ARRAY
   10 FORMAT(BZ,10A8)
      ELSE
      READ(IUNT,11) ARRAY
   11 FORMAT(BZ,12A8)
      ENDIF
      RETURN
      END
