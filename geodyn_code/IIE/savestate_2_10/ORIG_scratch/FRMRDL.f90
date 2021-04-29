!$FRMRDL
      SUBROUTINE FRMRDL(IUNT,LARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! FRMRDL           00/00/00            0000.0    PGMR - SBL
!
! FUNCTION:  READ VARIABLE LENGTH LOGICAL CHARACTER FORMATTED RECORDS
!            FROM A SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO READ FROM
!   LARRAY   I    A    ARRAY TO BE READ IN TO
!   ISIZE    I    S    NUMBER OF WORDS TO BE READ
!
! COMMENTS:
!                  THIS ROUTINE DIFFERS FROM BINRD IN THAT NO TEST
!                  IS MADE FOR ZERO LENGTH RECORDS AND NO DUMMY RECORD
!                  IS READ FOR ZERO LENGTH RECORDS.
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
      READ(IUNT,10) LARRAY
   10 FORMAT(BZ,80L1)
      ELSE
      READ(IUNT,11) LARRAY
   11 FORMAT(BZ,132L1)
      ENDIF
      RETURN
      END
