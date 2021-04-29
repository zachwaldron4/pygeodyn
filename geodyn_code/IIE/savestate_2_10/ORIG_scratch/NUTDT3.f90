!$NUTDT3
       BLOCK DATA NUTDT3
!********1*********2*********3*********4*********5*********6*********7
!                                               PGMR - S.LUO
!
! PARAMETERS
!   PV = PV0 + DPV0 * D
!
!   PV : MOTION OF THE PRIME MERIDIAN OF MARS IN J2000
!   D     : TIME INTERVAL IN DAY FROM J2000.0
!
!
! FUNCTION
!     CALCULATE THE MARS ROTATION
!
! REFERENCE
!     L.BASS AND R.CESARONE,  MARS OBSERVER: PLANETARY CONSTANTS
!                 AND MODELS
!                        JPL D-3444(DRAFT) (NOVEMBER 1990)
!
!*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
!
      COMMON/CROTAT/PV0,DPV0
!     DATA PV0/319.0779911D0/
      DATA PV0/319.2169911D0/
      DATA DPV0/350.8919857D0/
      END
