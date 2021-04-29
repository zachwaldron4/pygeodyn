!$UVCOMP
      SUBROUTINE UVCOMP (U, POS1, LSW)
!********1*********2*********3*********4*********5*********6*********7**
! UVCOMP           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   U
!   POS1
!   LSW
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!-------------------------------------------------------------------
!
!     ....THIS SUBROUTINE CALCULATES A ROTATION MATRIX TO TRANSFORM
!     ....FROM XYZ TO HCL OR FROM HCL TO XYZ
!
!     ....UVCOMP HAS BEEN MODIFIED TO USE THE GEODYN DEFINITION
!     ....OF HCL COORDINATES, WHICH IS:
!     ....  H = RADIAL
!     ....  L = VELOCITY
!     ....  C = L X H
!
!     ....U = ROTATION MATRIX FROM XYZ TO HCL
!     ....POS1 = SATELLITE POSITION AND VELOCITY VECTOR
!     ....LSW = SWITCH -- IF TRUE, DO NO ROTATION
!-------------------------------------------------------------------
!
      DOUBLE PRECISION POS1,  R, POS, U, DOTPDT, ZERO, ONE
      DIMENSION  POS1(1), POS(6), U (3, 3)
      DATA ZERO/0.D0/, ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!     ....CHECK THE COORDINATE SYSTEM
!
      DO 2 I = 1, 6
      POS(I) = POS1(I)
    2 END DO
!
!     ....IF LSW = TRUE,  THE U MATRIX IS THE IDENTITY MATRIX
!
      IF ( LSW ) GOTO 50
!
      R = SQRT (DOTPDT (POS, POS))
      R = ONE / R
      DO 20 I = 1, 3
      U (I, 1) = POS(I) *R
   20 END DO
!
      V = SQRT( DOTPDT ( POS(4),POS(4)) )
      V = ONE / V
      DO 30 I = 1, 3
      U (I, 3) = POS (I+3) * V
   30 END DO
!
      U (1, 2) = U (3, 1) * U (2, 3) - U (2, 1) * U (3, 3)
      U (2, 2) = U (1, 1) * U (3, 3) - U (3, 1) * U (1, 3)
      U (3, 2) = U (2, 1) * U (1, 3) - U (1, 1) * U (2, 3)
!
!     ....THIS SECTION CAN BE FINISHED LATER
!     ....XNN IS THE MAGNITUDE OF THE CROSS PRODUCT LXH
!     ....THIS IS IDENTICALLY ONE IF L IS PERPENDICULAR TO H
!     ....IN THIS FORMULATION THIS IS ONLY APPROXIMATELY TRUE
!     ....BUT FOR NOW IT WILL BE ASSUMED THAT XNN=1
!
!     XNN = ONE
!     DO 40 I = 1, 3
!     U (I, 2) = U (I, 2) / XNN
!40    CONTINUE
!
      GOTO 60
!
!---------------------------------------------------------------
!
!     ....SECTION FOR THE IDENTITY MATRIX OPTION
!
   50 CONTINUE
!
      DO 51 J=1,3
      DO 51 I=1,3
      U (I, J) = ZERO
   51 CONTINUE
!
      U (1, 1) = ONE
      U (2, 2) = ONE
      U (3, 3) = ONE
!
!----------------------------------------------------------------
!
   60 CONTINUE
      RETURN
      END
