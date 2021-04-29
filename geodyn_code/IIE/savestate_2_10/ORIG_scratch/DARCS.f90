!$DARCS
      FUNCTION DARCS(ARG)
!********1*********2*********3*********4*********5*********6*********7**
! DARCS          08/13/91         LUCIA TSAOUSSI
!
! FUNCTION: DARCS WILL COMPUTE THE DOUBLE PRECISION ARCCOS OF ARG.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!    ARG     I    S    THE NUMBER WHOSE ARCCOS IS TO BE TAKEN.
!
!   DARCS    O    S    THE ARCCOS OF ARG.
!
!
! COMMENTS:
!     ***  ADOPTED FROM THE CALC FUNCTION DARCS ***
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
! 2.    COMPUTE THE ARCCOS
!
      DARCS = ATAN2(ARG2,ARG)
!
! 3.    THE END
!
      RETURN
      END
