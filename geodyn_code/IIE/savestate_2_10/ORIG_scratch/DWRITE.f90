!$DREAD
      SUBROUTINE DWRITE(IUNTD,NMRC,AREA)
!CC      SUBROUTINE DWRITE(IUNTD,NMRC,AREA,*)
!********1*********2*********3*********4*********5*********6******
! DWRITE           89/05/23            0000.0    PGMR - SBL
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
! AREA IS DIMENSIONED BY 2368 BECAUSE WRITING OUT 18944 BYTES
      DIMENSION AREA(2368)
      DATA LOPEN/.TRUE./
!
!*****************************************************************
! START OF EXECUTABLE CODE ***************************************
!*****************************************************************
      IF(LOPEN) THEN
      IU1=IUNTD/10
      IU2=IUNTD-IU1*10
      WRITE(FILNAM,FMT='(A3,I1,I1)')'ftn',IU1,IU2
      OPEN(UNIT=IUNTD,FILE=FILNAM,STATUS='NEW',       &
     &ACCESS='DIRECT',RECL=18944)
      LOPEN=.FALSE.
      ENDIF
      WRITE(IUNTD,REC=NMRC) AREA
      RETURN
      ENTRY DREAD(IUNTD,NMRC,AREA)
!CC      ENTRY DREAD(IUNTD,NMRC,AREA,*)
      READ(IUNTD,REC=NMRC) AREA
      RETURN
      END
