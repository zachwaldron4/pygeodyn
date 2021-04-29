!$UNFRD2
      SUBROUTINE UNFRD2(IUNT,ARRAY,ISIZE)
!********1*********2*********3*********4*********5*********6*********7**
! UNFRD2           00/00/00            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  READ VARIABLE LENGTH DOUBLE WORD BINARY RECORDS
!            FROM A SPECIFIED UNIT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I         UNIT NUMBER TO READ FROM
!   ARRAY    I         ARRAY TO BE READ IN TO
!   ISIZE    I         NUMBER OF DOUBLE WORDS TO BE READ
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
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      READ(IUNT) ARRAY
      RETURN
      END
