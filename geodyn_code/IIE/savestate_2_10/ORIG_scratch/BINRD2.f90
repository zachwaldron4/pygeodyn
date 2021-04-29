!$BINRD2
      SUBROUTINE BINRD2(IUNT,ARRAY,ISIZE)
! *******1*********2*********3*********4*********5*********6*********7**
! BINRD2           00/00/00            0000.0    PGMR - WFE
!                                                PGMR - SBL
!
!
! FUNCTION:  READ VARIABLE LENGTH DOUBLE WORD BINARY RECORDS
!            FROM A SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    UNIT NUMBER TO READ FROM
!   ARRAY    I    A    ARRAY TO BE READ IN TO
!   ISIZE    I    S    NUMBER OF WORDS OF ARRAY TO BE READ
!
! COMMENTS:  IF AN INPUT RECORD OF ZERO LENGTH IS REQUESTED
!            A RECORD CONTAINING ONE DOUBLE WORD IS READ
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
      DIMENSION ARRAY(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IF(LASCII) THEN
      IF(ISIZE.GT.0) GO TO 200
      READ(IUNT,17) DUM
   17 FORMAT(BZ,D24.16)
      RETURN
  200 CONTINUE
      CALL FRMRD2(IUNT,ARRAY,ISIZE)
      RETURN
      ELSE
      IF(ISIZE.GT.0) GO TO 400
      READ(IUNT) DUM
      RETURN
  400 CONTINUE
      CALL UNFRD2(IUNT,ARRAY,ISIZE)
      RETURN
      ENDIF
      END
