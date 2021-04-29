!$SASTD
      SUBROUTINE SASTD(P,PDOT,RLAT,SITEHT,ZD,ZDDOT)
!********1*********2*********3*********4*********5*********6*********7**
! SASTD           12.11.91            9110.4   LUCIA TSAOUSSI
!
!
! FUNCTION:  COMPUTES ZENITH DELAY FOR "HYDROSTATIC" COMPONENT
!            OF THE ATMOSPHERE USING SAASTAMOINEN FORMULATION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   P        I    S    SURFACE PRESSURE,MBAR
!   PDOT     I    S    TIME RATE OF CHANGE OF SURFACE PRESSURE,MBAR/SEC
!   RLAT     I    S    SITE LATITUDE IN RADIANS
!   SITEHT   I    S    SITE GEOID HEIGHT IN METERS
!   ZD       O    S    ZENITH DELAY IN METERS
!   ZDDOT    O    S    RATE OF CHANGE OF ZENITH DELAY IN METERS/SEC
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
      DATA TWO/2.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!  CALCULATE THE VARIATION OF GRAVITY WITH STATION POSITION
!
      F=ONE-0.00266D0*COS(TWO*RLAT)-0.00028D0*SITEHT/1000.D0
      ZD=0.0022768*P/F
      ZDDOT=ZD*PDOT/P
!
      RETURN
      END
