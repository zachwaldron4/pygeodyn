!$SASTW
      SUBROUTINE SASTW(RH,TC,RHDOT,TCDOT,RLAT,SITEHT,ZW,ZWDOT)
!********1*********2*********3*********4*********5*********6*********7**
! SASTW           12.11.91            9110.4   LUCIA TSAOUSSI
!
!
! FUNCTION:  COMPUTES ZENITH DELAY DUE TO WET ( NON-"HYDROSTATIC")
!            COMPONENT OF THE ATMOSPHERE USING SAASTAMOINEN FORMULATION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   RH       I    S    RELATIVE HUMIDITY ( 0< RH < 1)
!   RHDOT    I    S    TIME DERIVATIVE OF RH
!   TC       I    S    TEMPERATURE IN CELCIUS
!   TCDOT    I    S    TIME DERIVATIVE OF TC
!   RLAT     I    S    SITE LATITUDE IN RADIANS
!   SITEHT   I    S    SITE GEOID HEIGHT IN METERS
!   ZW       O    S    ZENITH PATH DELAY OF WET COMPONENT IN METERS
!   ZWDOT    O    S    TIME DERIVATIVE OF ZENITH DELAY IN METERS/SEC
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
!
      DATA TWO/2.0D0/,ONE/1.0D0/,TEN/10.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!  CALCULATE THE SATURATION VAPOR PRESSURE AND TIME DERIVATIVE
!
      TEMP=TC+237.3D0
      ESAT=6.11D0*EXP(17.269D0*TC/TEMP)
!
!  CALCULATE THE VARIATION OF GRAVITY WITH STATION POSITION
      F=ONE-0.00266D0*COS(TWO*RLAT)-0.00028D0*SITEHT/1000.D0
      ESATDOT=ESAT*(17.269D0/TEMP-17.269D0*TC/(TEMP*TEMP))*TCDOT/F
!
!  CALCULATE ZENITH PATH DELAY
      ZW=0.002277*(1255.D0/(TC+273.15D0)+0.05D0)*RH*ESAT/F
!
      TEMP=TC+273.15D0
      ZWDOT=-0.002277D0*(TCDOT*1255.D0/(TEMP*TEMP))*RH*ESAT/F           &
     &      +0.002277D0*(1255.D0/TEMP+0.05D0)*RHDOT*ESAT/F              &
     &      +ZW*ESATDOT*F/ESAT
!
      RETURN
      END
