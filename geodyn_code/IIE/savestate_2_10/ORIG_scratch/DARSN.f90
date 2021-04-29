!$DARSN
      FUNCTION DARSN(ARG)
!********1*********2*********3*********4*********5*********6*********7**
! DARCS          09/13/91         LUCIA TSAOUSSI
!
! FUNCTION: DARCS WILL COMPUTE THE DOUBLE PRECISION ARCSIN OF ARG.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!    ARG     I    S    THE NUMBER WHOSE ARCCOS IS TO BE TAKEN.
!
!   DARSN    O    S    THE ARCSIN OF ARG.
!
!
! COMMENTS:
!     ***  ADOPTED FROM THE CALC FUNCTION DARSN ***
!       CALLED SUBROUTINES - ATAN2,SQRT
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! 1.    COMPUTE THE OTHER ARGUMENT
!
      ARG2 = SQRT( 1.0D0 - (ARG * ARG))
!
! 2.    COMPUTE THE ARCSIN
!
      DARSN = ATAN2(ARG,ARG2)
!
! 3.    THE END
!
      RETURN
      END
