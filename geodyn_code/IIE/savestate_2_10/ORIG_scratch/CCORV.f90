!$CCORV
      FUNCTION CCORV(ALT, R,H1,ZH)
!
!     ....called by VTS3 for Venus Atmosphere Model of Hedin
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO = 0.0D0 )
      PARAMETER ( HALF = 0.5D0 )
      PARAMETER ( ONE  = 1.0D0 )
      PARAMETER ( D170 = 170.D0 )
!
!
!        CHEMISTRY/DISSOCIATION CORRECTION
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      E=(ZH-ALT)/H1
      IF( E .GT.  D170 ) GO TO 10
      IF( E .LT. -D170 ) GO TO 20
        EX=EXP(E)
        CCORV=(ONE+R*EX)/(ONE+EX)
        GO TO 50
   10   CCORV=R
        GO TO 50
   20   CCORV=ONE
        GO TO 50
   50 CONTINUE
!CC    WRITE(6,*) 'CCORV: E, CCORV ',E, CCORV
       RETURN
      END
