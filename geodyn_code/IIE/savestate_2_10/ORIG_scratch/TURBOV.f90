!$TURBOV
      SUBROUTINE TURBOV(DD,DM,ZB,ZH,XM,XMM,TZ)
!
!     ....called by VTS3 for Venus Atmosphere Model of Hedin
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO = 0.0D0 )
      PARAMETER ( HALF = 0.5D0 )
      PARAMETER ( ONE  = 1.0D0 )
!
!
!        ESTIMATE TURBOPAUSE HEIGHT
!
      COMMON/VPARMB/GSURFV,REV
!
      DATA RGAS/831.4D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!CC   WRITE(6,*) 'TURBOV: GSURFV, REV ', GSURFV, REV
      GZB=GSURFV/(ONE+ZB/REV)**2
      ZH=ZB+RGAS*TZ/GZB/(XM-XMM)*LOG(DD/DM)
!CC   WRITE(6,*) 'TURBOV: GZB, ZH ', GZB, RH
      RETURN
      END
