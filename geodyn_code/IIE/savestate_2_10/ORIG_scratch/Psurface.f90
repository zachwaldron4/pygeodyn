!$PSURFACE
      Subroutine Psurface(sitla,sitlo,als,Tavg,Rbar,pavg,psurf,dustM,   &
     & dustA,thgt,gzero,gamma,sunlon)
!********1*********2*********3*********4*********5*********6*********7**
! PSURFACE         00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   sitla    I    S    SITE LATITUDE (DEGREES)
!   sitlo    I    S    WEST LONGITUDE OF SPACECRAFT (DEGREES)
!     als    I    S    AREOCENTRIC LONGITUDE OF SUN ORBIT
!    Tavg    I    S
!    Rbar    I    S    Gas constant (m**2/(s**2 K))
!    thgt    I    S    LOCAL TERRAIN HEIGHT
!   gzero    I    S    ACCELERATION OF GRAVITY AT SURFACE
!   gamma    I    A    LAPSE RATES
!   sunlon   I    S    WEST LONGITUDE OF THE SUN (DEGREES)
!   dustM    I    S    dust storm magnitude for average T and p effect
!                      (1 = full magnitude, 0 = no dust storm)
!   dustA    I    S    dust storm magnitude for daily amplitude T and p
!                      effect (1 = full magnitude, 0 = no dust storm)
!   pavg     O    S    Daily average surface pressure ( in N/m**2)
!   Psurf    O    S    Local surface pressure ( N/m**2)
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DOUBLE PRECISION npls,nmls,nmals
!...  Computes daily average surface pressure (pavg, in N/m**2),
!...  local surface pressure (psurf, in N/m**2) at site latitude
!...  longitude (sitla and sitlo, in degrees), for given areocentric
!...  longitude of sun (als, in degrees), and daily average surface
!...  temperature (Tavg, in K).  Rbar is the gas constant (m**2/
!...  (s**2 K)); dustM and dustA are dust storm effects on magnitude
!...  of mean or diurnal amplitude for temperature or pressure
!...  (0 = no effect, 1 = full effect); thgt is input terrain height
!...  (km), relative to the reference ellipsoid, at the site latitude
!...  and longitude; gzero (m/s**2), is the local surface gravity for
!...  the given site latitude and longitude; gamma is the local
!...  temperature lapse rate (K/km); and sunlon is the longitude of
!...  the sun (degrees).
      Data C0,C1,C2/2.276672,2.347012D-5,1.634902D-9/
!
      pi180 = ATAN(1.)/45.
!
      npls = (90. + sitla)**2
      nmls = (90. - sitla)**2
      abslat = ABS(sitla)
      nmals = (90. - abslat)**2
!...  Pressure/temperature ratio (annual average)
      potavg = 0.5*(2.*C0 + C1*(npls+nmls) + C2*(npls**2                &
     & + nmls**2))
!...  Annual average Temperature vs latitude
      Tbar = (((0.525161D-6*sitla - 0.263317D-5)*sitla                  &
     & - 0.0116584)*sitla + 0.0334196)*sitla + 216.54
!...  Seasonal and latitudinal variation of ratio p/pbar(annual)
      Call PRSEAS(als,sitla,pr)
!...  Latitude and seasonally-dependent daily average pressure
!...  at reference ellipsoid height level
      factor = 1.3854 - 3.59437D-5*sitla**2
      pref = factor*pr*potavg*Tbar
!...  Convert daily average pressure to terrain surface level
      If (ABS(gamma) .lt. 0.001) then
        pavg = pref*EXP(-1000.*gzero*thgt/(Rbar*Tavg))
      Else
        Tref = Tavg + gamma*thgt
        exponent = -1000.*gzero/(Rbar*gamma)
        pavg = pref*((Tref/Tavg)**exponent)
      Endif
!...  Dust storm effect on diurnal average pressure
      pavg = pavg + (61.082*SIN(2.*pi180*sitla)                         &
     & - 10.837*SIN(4.*pi180*sitla))*dustM
!...  Local solar time (Mars hours)
      tlocal = 12. + (sunlon - sitlo)/15.
      If (tlocal .lt. 0.) tlocal = tlocal + 24.
      If (tlocal .gt. 24.)tlocal = tlocal - 24.
!...  Amplitude (N/m**2) and phase (hours) of diurnal variation
      sinLs = SIN(pi180*als)
      A11 = 12.60 - 0.118*sitla
      A12 = -0.60 - 0.063*sitla
      amp1 = A11 + A12*sinLs
      H11 = 7.769 - 0.05769*sitla
      H12 = 3.077 + 0.001923*sitla
      ph1 = H11 - H12*sinLs
!...  Amplitude (N/m**2) and phase (hours) of semi-diurnal variation
      A21 = 7.982 - 0.022*sitla
      A22 = -5.219 + 0.033*sitla
      amp2 = A21 + A22*sinLs
      H21 = 11.635 - 0.02885*sitla
      H22 = -0.135 + 0.02885*sitla
      alsm60 = pi180*(als-60.)
      cosLs = COS(alsm60)
      ph2 = H21 + H22*cosLs
!...  Dust storm effect on diurnal and semidiurnal amplitudes
      xlat = pi180*sitla
      coslat = COS(xlat)
      amp1 = amp1 + (1.32 + 13.56*coslat)*dustA
      amp2 = amp2 + (33.33*EXP(-.618727D-3*sitla**2))*dustA
      freq = 15.*pi180
!...  Diurnal plus semidiurnal variation of pressure about daily
!...  average
      psurf = pavg + amp1*COS(freq*(tlocal-ph1))                       &
     & + amp2*COS(2.*freq*(tlocal-ph2))
      Return
      END
