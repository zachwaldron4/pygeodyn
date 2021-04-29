!$FRMRD2
      SUBROUTINE FRMRD2(IUNT,ARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! FRMRD2           00/00/00            0000.0    PGMR - SBL
!
! FUNCTION:  READ FORMATTED DOUBLE WORD RECORDS FROM A SPECIFIED
!            UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO READ FROM
!   ARRAY    I    A    ARRAY TO BE READ IN TO
!   ISIZE    I    S    NUMBER OF DOUBLE WORDS TO BE READ
!
! COMMENTS:
!                  THIS ROUTINE DIFFERS FROM BINRD2 IN THAT NO TEST
!                  IS MADE FOR ZERO LENGTH RECORDS AND NO DUMMY RECORD
!                  IS READ FOR ZERO LENGTH RECORDS.
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
   10 FORMAT(BZ,3D24.16)
      ELSE
      READ(IUNT,11) ARRAY
   11 FORMAT(BZ,5D24.16)
      ENDIF
      RETURN
      END
