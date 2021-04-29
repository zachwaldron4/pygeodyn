!$BUFIN
      SUBROUTINE BUFIN (OBSBUF,IBUF)
!********1*********2*********3*********4*********5*********6*********7**
! BUFIN            00/00/00            0000.0    PGMR - TOM MARTIN
!
!
! FUNCTION: READ AN OBSERVATION BUFFER FROM UNIT 20
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   OBSBUF  I/O   A    OBSERVATION BUFFER
!   IBUF     I    S    BUFFER NUMBER
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION OBSBUF(512)
      DATA INDATA/20/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!     READ (INDATA,REC=IBUF) OBSBUF
      RETURN
      END
