!$UNFWR2
      SUBROUTINE UNFWR2(IUNT,ARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! UNFWR2         00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  WRITE OUT VARIABLE LENGTH DOUBLE WORD RECORDS TO
!            THE SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I         UNIT NUMBER TO WRITE ON
!   ARRAY    I    A    DOUBLE WORD ARRAY TO BE WRITTEN OUT
!   ISIZE    I         NUMBER OF DOUBLE WORDS OF ARRAY TO OUTPUT
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
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      WRITE(IUNT) ARRAY
      RETURN
      END
