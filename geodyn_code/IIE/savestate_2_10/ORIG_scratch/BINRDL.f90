!$BINRDL
      SUBROUTINE BINRDL(IUNT,IARRAY,ISIZE)
! *******1*********2*********3*********4*********5*********6*********7**
! BINRDL           00/00/00             0000.0   PGMR - B. EDDY
!                                                       SBL
!
! FUNCTION:  READ VARIABLE LENGTH SINGLE WORD BINARY RECORDS
!            FROM A SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO READ FROM
!   IARRAY   I    A    ARRAY TO BE READ IN TO
!   ISIZE    I    S    NUMBER OF WORDS OF ARRAY TO BE READ
!
! COMMENTS:  IF AN INPUT RECORD OF ZERO LENGTH IS REQUESTED
!            A RECORD CONTAINING ONE INTEGER WORD IS READ
!            INTO A DUMMY VARIABLE
! *******1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      COMMON/CLASCI/LASCII,LIFCYB,LARECL,NXLASC
!
      DIMENSION IARRAY(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(LASCII) THEN
      IF(ISIZE.GT.0) GO TO 100
      READ(IUNT,17) IDUM
   17 FORMAT(BZ,I25)
      RETURN
  100 CONTINUE
      CALL FRMRDL(IUNT,IARRAY,ISIZE)
      RETURN
      ELSE
      IF(ISIZE.GT.0) GO TO 300
      READ(IUNT) IDUM
      RETURN
  300 CONTINUE
      CALL UNFRD(IUNT,IARRAY,ISIZE,.FALSE.)
      RETURN
      ENDIF
      END
