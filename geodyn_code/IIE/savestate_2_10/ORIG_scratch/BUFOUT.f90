!$BUFOUT
      SUBROUTINE BUFOUT(OBSBUF,IBUF)
!********1*********2*********3*********4*********5*********6*********7**
! BUFOUT           00/00/00            0000.0    PGMR - TOM MARTIN
!
!
! FUNCTION:  WRITE AN OBSERVATION BUFFER ON UNIT 20.
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   OBSBUF  I/O   A    OBSERVATION BUFFER
!   IBUF     I    S    NUMBER OF BUFFER
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
!     WRITE(INDATA,REC=IBUF) OBSBUF
      RETURN
      END
