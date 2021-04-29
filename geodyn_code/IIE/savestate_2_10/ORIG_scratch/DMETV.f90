      SUBROUTINE DMETV(DMET,P,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
!
!           *CMET  * ATMOSPHERIC PRESSURE(MBARS), TEMPERATURE(DEG K),
!                    AND PARTIAL PRESSURE(MBARS) OF WATER VAPOR USED BY
!                    REFRACTION MODELS
!
!            P0     -  ATMOSPHERIC PRESSURE (MBARS)
!            T0     -  TEMPERATURE (DEGREES KELVIN)
!            RELHUM -  RELATIVE HUMIDITY
!            E0     -  WATER VAPOR PRESSURE (MBARS)
!            WAVLEN -
!
!
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
!
!           *CSTA/R* STATION GEODETIC INFORMATION USED IN MEASUREMENT
!                    CORRECTIONS
!           **NOTE**  /CSTA/ is equivalenced to the STAINF array
!                     if words are added to the STAINF array, the common
!                     block /CSTA/ must be extended by the same number
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      HHOLD=HEIGHT
      HEIGHT=0.D0
      CALL METDAT(DMET)
      T=T0-273.15D0
      P=P0
      HEIGHT=HHOLD
      RETURN
      END
