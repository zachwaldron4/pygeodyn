!$TEMPS
      Subroutine Temps(T0bar,Tsurf,gam,alat,dlon,dustM,dustA,als,T,CF,  &
     &   thgt)
!********1*********2*********3*********4*********5*********6*********7**
! TEMPS            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    alat    I    S    SPACECRAFT LATITUDE (DEGREES)
!    dlon    I    S
!    als     I    S    AREOCENTRIC LONGITUDE OF SUN ORBIT
!    dustM   I    S    dust storm magnitude for average T and p effect
!                      (1 = full magnitude, 0 = no dust storm)
!    dustA   I    S    dust storm magnitude for daily amplitude T and p
!                      effect(1 = full magnitude, 0 = no dust storm)
!    T0bar   I    S
!    Tsurf   I    S
!    CF      I    A    Climate adjustment factors CF(5)
!    thgt    I    S    local terrain height
!    T       O    A    TEMPERATURE(K).vs. Height at the significant
!                      altitude
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!     Subroutine to compute temperatures T(i) (K), vs. height at
!     the significant altitudes for given latitude, alat,
!     and West longitude, alon (degrees).  Input is T0bar, the
!     average surface temperature (K) without dust storm, Tsurf,
!     the local surface temperature (K) including dust storm effect
!     (if any), gam(i) are the input, non-dust-storm lapse
!     rates (deg./km), which are modified to the actual lapse rates
!     by this subroutine, dlon is the site-sun West longitude differ-
!     ence (degrees), dustM is the magnitude of dust storm effect
!     on daily average temperature, dustA is magnitude of dust
!     storm effect on daily amplitude (1 indicates full dust-storm
!     effect, 0 indicates no dust storm in progress), and als is
!     the areocentric longitude of the sun (degrees).
!
      Dimension gam(5),T(0:5),dz(5),z(5),factor(5),dfactor(5),afact(5)  &
     & ,A25(0:6),B25(0:6),C25(0:6),CF(0:5)
!...  Coefficients for T(25 km) correction to gammas
      Data A25/171.0696,-2.8478,17.2147,2.1852,-0.4919,-0.8862,         &
     & -3.3907/
      Data B25/-11.0360,16.2759,-1.9340,-10.8421,0.2430,3.5283,0.9585/
      Data C25/-2.8552,0.6307,-1.8903,0.6098,1.7360,-0.7366,-0.6614/
!...  Increments between height levels
      Data dz/5.,10.,15.,20.,25./
!...  Height levels for evaluating temperatures and pressures
      Data z/5.,15.,30.,50.,75./
!...  Height scaling factors for diurnal amplitude (relative to
!     amplitude at 30 km) in non-dust storm conditions
      Data Factor/0.67,0.70,1.00,1.30,1.34/
!...  Height scaling factors for diurnal amplitudes in dust-storm
!     conditions
      Data dfactor/0.396,0.749,1.00,1.00,0.919/
!...  factor to convert degrees to radians
      pi180 = ATAN(1.)/45.
!...  factor for 240 degree period in T(25 km)
      pi120 = 1.5*pi180
!...  Phase hour for maximum diurnal perturbation at surface
      thour0 = 14.
!...  Compute T(25 km) for given latitude and Ls values
      AT25 = A25(0)
      BT25 = B25(0)
      CT25 = C25(0)
      Do 5 i = 1,5,2
        sinlat = SIN(pi120*(i+1.)*alat/2.)
        coslat = COS(pi120*(i+1.)*alat/2.)
        AT25 = AT25 + A25(i)*sinlat
        AT25 = AT25 + A25(i+1)*coslat
        BT25 = BT25 + B25(i)*sinlat
        BT25 = BT25 + B25(i+1)*coslat
        CT25 = CT25 + C25(i)*sinlat
    5 CT25 = CT25 + C25(i+1)*coslat
      T25 = AT25 + BT25*SIN(pi180*als) + CT25*COS(pi180*als)
!...  Compute average temperatures vs height from given average
!     surface temperature and lapse rates
      dTsrf = gam(1)*thgt*0.5
      T(0) = T0bar - dTsrf
      Do 10 i = 1,3
   10 T(i) = T(i-1) - gam(i)*dz(i)
!...  Difference between T(25 km) and value from gammas
      DT25 = T25 - (T(2) + 2.*T(3))/3. - gam(3)*thgt
!...  Adjust T(1-3) array values with T(25 km) model values
      Do 15 i = 1,3
        T(i) = T(i) + z(i)*DT25/25.
   15 gam(i) = (T(i-1) - T(i))/dz(i)
!...  Compute T(4-5) array values with gamma values
      Do 20 i = 4,5
   20 T(i) = T(i-1) - gam(i)*dz(i)
      abslat = ABS(alat)
!...  Compute non-dust effects if dustM = 0
      If (dustM .eq. 0)then
!...    Latitude dependence for diurnal factor at 5 km height
        If (abslat .le. 25)then
          Factor(1) = 0.79
        Else If (abslat .ge. 45)then
          Factor(1) = 0.43
        Else
          Factor(1) = 1.24 - 0.018*abslat
        Endif
        Do 25 i = 1,5
   25   afact(i) = factor(i)
      Endif
!...  Relative temperature amplitude at 30 km height
      If (abslat .lt. 90.)then
        dtr30 = 0.0141*(90. - abslat)**0.2
      Else
        dtr30 = 0.0
      Endif
!...  Phase hour for maximum at 26 km height
      If (alat .le. -70.)then
        thour26 = 21.
      Else If (alat .ge. -20.)then
        thour26 = 11.5
      Else
        thour26 = 7.7 - 0.19*alat
      Endif
      If (dustM .gt. 0.0)then
!...    Compute dust-storm effects on average and diurnal temperature
!...    Adjust hour of diurnal temperature maximum at 26 km
        thour26 = (1. - dustA)*thour26 + dustA*16.7
!...    Adjust height factors for diurnal amplitudes
        Do 30 i = 1,5
   30   afact(i) = (1. - dustA)*factor(i) + dustA*dfactor(i)
!...    Adjust average temperatures and diurnal temperature amplitude
        If (abslat .le. 50.)then
          dtr26 = 0.0465 + 1.90D-6*abslat**2
          T(1) = T(1) + 13.*dustM
          T(2) = T(2) + 36.*dustM
          T(3) = T(3) + (48. + 0.0024*abslat**2)*dustM
          T(4) = T(4) + 30.*dustM
          T(5) = T(5) + 26.*dustM
        Else
          alatp = 90. - abslat
          dtr26 = ((1.597D-6*alatp - 1.417D-4)*alatp +                  &
     &     4.394D-3)*alatp
          T(1) = T(1) + (28. - 0.3*abslat)*dustM
          T(2) = T(2) + (11. + 0.015625*alatp**2)*dustM
          T(3) = T(3) + (25. + 0.018125*alatp**2)*dustM
          T(4) = T(4) + (50. - 0.4*abslat)*dustM
          T(5) = T(5) + (46. - 0.4*abslat)*dustM
        Endif
!...    Adjust diurnal amplitude from 26 to 30 km
        dtr30 = (1. - dustA)*dtr30 + dustA*1.072*dtr26
      Endif
!...  Spep through heights
      Do 40 i = 1,5
!...    Compute diurnal adjustment to average temperatures.
!...    Interpolate (extrapolate) for phase hour of maximum diurnal
!       perturbation
        thour = thour0 + (thour26 - thour0)*z(i)/26.
!...    Make sure phase hour is in range 0 < thour < 24
        If (thour .gt. 24.)thour = thour - 24.
        If (thour .lt. 0.)thour = thour + 24.
!...    Longitude where maximum phase occurs
        alonmax = 15.*(thour - 12.)
!...    Evaluate temperature including diurnal perturbation
        T(i) = T(i)*(1. + dtr30*afact(i)*COS(pi180*(dlon + alonmax)))
   40 Continue
      T(0) = Tsurf - dTsrf
!...  Apply climate adjustment factors
      Do 45 i = 0,5
       T(i) = T(i)*CF(i)
   45 Continue
!...  Re-evaluate lapse rates (deg./km), based on actual temperatures
      Do 50 i = 1,5
   50 gam(i) = (T(i-1) - T(i))/dz(i)
      Return
      END
