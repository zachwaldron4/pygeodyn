!$PDFWRT
      SUBROUTINE PDFWRT(IUNTD,NMRC,AREA)
!********1*********2*********3*********4*********5*********6******
! PDFWRT           99/05/28            0000.0    PGMR - SFL
!
!                  ENTRY POINT DREAD
! FUNCTION:  DIRECT ACCESS READING OR WRITING OF UNIT 42
!
!   I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   --------------------------------------------
!   IUNTD   I/O        UNIT NUMBER
!   NMRC    I/O        THE RECORD NUMBER
!   AREA    I/O        AREA TO BE WRITTEN OR READ INTO
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6******
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      CHARACTER*5 FILNAM
! AREA IS DIMENSIONED BY 3002 BECAUSE WRITING OUT 12008 BYTES
!     DIMENSION AREA(3002)
      REAL AREA(3002)
      DATA LOPEN/.TRUE./
!
!*****************************************************************
! START OF EXECQTABLE CODE ***************************************
!*****************************************************************
      IF(LOPEN) THEN
      IU1=IUNTD/10
      IU2=IUNTD-IU1*10
      WRITE(FILNAM,FMT='(A3,I1,I1)')'ftn',IU1,IU2
      OPEN(UNIT=IUNTD,FILE=FILNAM,STATUS='NEW',      &
     &ACCESS='DIRECT',RECL=12008)
      LOPEN=.FALSE.
      ENDIF
      WRITE(IUNTD,REC=NMRC) AREA
      RETURN
      ENTRY PDREAD(IUNTD,NMRC,AREA)
      READ(IUNTD,REC=NMRC) AREA
      RETURN
      END
