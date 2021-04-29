!$THERMPAR
      Subroutine Thermpar(RAU,FBARR,Clat,ALS,sunlat,TINF0,TF0,ZF0,      &
     &  SCALE)
!********1*********2*********3*********4*********5*********6*********7**
! THERMPAR         00/00/00            0000.0    PGMR - ?
!....................................................................
!
!...  Thermospheric parameters, revised from the orginal Stewart
!     parameterizations:
!     SMA = 1.523691
!     ZF0 = 124.4 * (SMA/RAU)
!     TINF0 = 4.11 * (11.0 + FBARR)
!     TF0 = 170.0 * (SMA/RAU)
!     SCALE = TF0 / 9.20
!
!     The new parameterizations are based on four data sets from the
!     University of Arizona Mars Thermospheric Global Circulation
!     Model (MTGCM), cases MGS97L, MGS98L, MANC00, and MGS97E. For
!     a description of the MTGCM model and its output, see Bougher,
!     et al., Journal of Geophysical Research, vol. 95 (B9), pp.
!     14,811 - 14,827, August 30, 1990.
!
!...................................................................
!
!     Inputs:
!       RAU    = orbital position radius (AU)
!       FBARR  = 10.7 cm solar flux at Mars position
!       clat    = latitude for evaluation of parameters (degrees)
!       ALS    = local solar time (Mars hours) at evaluation point
!       sunlat = latitude of sun (degrees)
!     Outputs:
!       TINF0  = Exospheric temperature (K)
!       TF0    = Temperature at base of thermosphere (K)
!       ZF0    = Height of base of thermosphere (km)
!       SCALE  = Scale height for temperature variations (km)
!
!     Output values are un-corrected for Stewart (ES array) variations,
!     pressure and dust effects.  These factors are accounted for in
!     the Stewart2 subroutine.  Adjustment factors deltaTEX, deltaTF
!     and deltaZF are also added after computation of these values.
!....................................................................
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      Data XLATMA/25.4/
!
!...  Degrees to radians conversion factor
      pi180 = ATAN(1.)/45.
!...  Global mean exospheric temperature (K) versus 10.7 cm flux
      Tbar = 156.3 + 0.9427*FBARR
!...  Zonal average exospheric temperature (K) versus latitude
      Tavg = Tbar*(1. + 1.369D-4*sunlat*clat)
!...  Phase angles (hours) for local solar time variation
      t1 = 13.2 - 0.00119*sunlat*clat
      t2 = 9.4 - 0.00231*sunlat*clat
!...  Amplitude factor for local solar time variation
      cphi = COS(pi180*(clat + sunlat)/(1. + XLATMA/90.))
!...  Exospheric temperature (K) versus local solar time
      TINF0 = Tavg*(1. + 0.22*cphi*COS(pi180*15.0*(ALS-t1)) +          &
     &  0.04*cphi*COS(pi180*30.0*(ALS-t2)))
!...  Global mean height of thermosphere base (km)
      Zbar = 197.94 - 49.058*RAU
!...  Latitude variation factor
      factlat = (sunlat/XLATMA)*(clat/77.5)**3
!...  Zonal average base height (km) versus latitude
      Zavg = Zbar + 4.3*factlat
!...  Amplitudes for local solar time variation
      A1 = 1.5 - COS(pi180*4.0*clat)
      A2 = 2.3*(COS(pi180*(clat + 0.5*sunlat)))**3
!...  Phase angles (hours) for local solar time variation
      t1 = 16.2 - (sunlat/XLATMA)*ATAN(pi180*10.0*clat)
      t2 = 11.5
!...  Base height of thermosphere (km) versus local solar time
      ZF0 = Zavg + A1*COS(pi180*15.0*(ALS-t1)) +                       &
     & A2*COS(pi180*30.0*(ALS-t2))
!...  Global mean temperature (K) at thermosphere base, versus FBARR
      Tbar = 113.7 + 0.5791*FBARR
!...  Zonal average temperature at thermosphere base (K) vs. latitude
      Tavg = Tbar*(1. + 0.186*factlat)
!...  Amplitudes for local solar time variation
      A1 = 0.06 - 0.05*COS(pi180*4.0*clat)
      A2 = 0.1*(COS(pi180*(clat + 0.5*sunlat)))**3
!...  Phase angles (hours) for local solar time variation
      t1 = 17.5 - 2.5*(sunlat/XLATMA)*ATAN(pi180*10.0*clat)
      t2 = 10.0 + 2.0*(clat/77.5)**2
!...  Thermosphere base temperature (K) versus local solar time
      TF0 = Tavg*(1.0 + A1*COS(pi180*15.0*(ALS-t1)) +                  &
     & A2*COS(pi180*30.0*(ALS-t2)))
!...  Global mean scale height (km) of thermospheric temperature
      SCALE = 8.38 + 0.09725*FBARR
!...  Zonal average temperature scale height (km) vs. latitude
      SCALE = SCALE*(1.14 - 0.18*COS(pi180*clat))
      Return
      END
