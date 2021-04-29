
!$CCOR
      FUNCTION  CCOR86( ALT, R, H1, ZH )
!********1*********2*********3*********4*********5*********6*********7**
! CCOR             91/01/15            0000.0    PGMR - A.HEDIN
!
! FUNCTION:  CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ALT
!   R
!   H1
!   ZH
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( D70  = +70.D0 )
      PARAMETER ( DM70 = -70.D0 )
      PARAMETER ( ONE  = 1.D0 )
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!     Eq. A20a or Eq. A21
!
      E=(ALT-ZH)/H1
      IF( E .GT. D70 ) THEN
         CCOR86 = ONE
         RETURN
      ELSE IF( E .LT. DM70 ) THEN
          CCOR86 = EXP(R)
          RETURN
      ELSE
          EX=EXP(E)
          CCOR86 = EXP( R / (ONE + EX) )
      ENDIF
      RETURN
      END
