!$DNETV
      FUNCTION DNETV(DD,DM,ZHM,XMM,XM)
!
!     ....called by VTS3 for Venus Atmosphere Model of Hedin
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO = 0.0D0 )
      PARAMETER ( HALF = 0.5D0 )
      PARAMETER ( ONE  = 1.0D0 )
      PARAMETER ( TEN  = 10.D0 )
!
!
!       8/20/80;7/25/83
!
!       TURBOPAUSE CORRECTION
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      A=ZHM/(XMM-XM)
      ADM=ZERO
      IF(DM.GT.ZERO) ADM=LOG(DM)
      ADD=ZERO
      IF(DD.GT.ZERO) ADD=LOG(DD)
      YLOG=A*(ADM-ADD)
      IF(YLOG.LT.-TEN) GO TO 10
      IF(YLOG.GT.TEN)    GO TO 20
        DNETV=DD*(ONE+EXP(YLOG))**(1/A)
        GO TO 50
   10 CONTINUE
        DNETV=DD
        GO TO 50
   20 CONTINUE
        DNETV=DM
        GO TO 50
   50 CONTINUE
      RETURN
      END
