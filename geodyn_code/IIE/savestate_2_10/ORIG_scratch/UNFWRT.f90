!$UNFWRT
      SUBROUTINE UNFWRT(IUNT,IARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! UNFWRT           00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  WRITE OUT VARIABLE LENGTH BINARY RECORDS TO
!            THE SPECIFIED UNIT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I         UNIT NUMBER TO WRITE ON
!   IARRAY   I         ARRAY TO BE WRITTEN OUT
!   ISIZE    I         NUMBER OF WORDS OF ARRAY TO OUTPUT
!
! COMMENTS:
!                  THIS ROUTINE DIFFERS FROM BINWRT IN THAT NO TEST
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
      DIMENSION IARRAY(ISIZE)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      WRITE (IUNT) IARRAY
      RETURN
      END
