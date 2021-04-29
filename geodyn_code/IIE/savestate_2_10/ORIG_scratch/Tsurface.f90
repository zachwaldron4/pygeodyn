!$TSURFACE
      Subroutine Tsurface(sitla,sitlo,sunla,sunlo,als,au,Tsurf,         &
     & T0bar,dustT,Tavg)
!********1*********2*********3*********4*********5*********6*********7**
! TSURFACE         00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    sitla   I    S    SPACECRAFT LATITUDE (DEGREES)
!    sitlo   I    S    WEST LONGITUDE OF SPACECRAFT (DEGREES)
!    au      I    S    MARS ORBITAL RADIUS (AU)
!    sunla   I    S    AREOCENTRIC LATITUDE OF SUN (DEGREES)
!    sunlo   I    S    MARS WEST LONGITUDE OF SUN (DEGREES)
!     ALS    I    S    AREOCENTRIC LONGITUDE OF SUN ORBIT
!    dustT   I    S    dust storm magnitude ( 1 = full effect,0=none)
!    Tsurf   O    S
!    T0bar   O    S
!    Tavg    O    S
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  computes surface temperature (K) versus site latitude and
!...  longitude (degrees) for given solar latitude and longitude
!...    (degrees), solar areocentric longitude Ls (degrees), and orbital
!...    radius (AU). Also computes diurnal average temperature for given
!...  site latitude. Parameter dustT is the magnitude of the dust
!...  storm  maximum Temperature effect amplitude (dustT = 1 means
!...  full temperature effect; dustT = 0 means no dust storm). Tsurf
!...    is the local value of surface temperature, including dust effect
!...  (if any). T0bar is the diurnal average surface temperature
!...  without dust effect.
      pi = 4.D0*ATAN(1.D0)
!...  factor for degrees to radians
      dtr = pi/180.D0
      sunlat = sunla*dtr
      clats = COS(sunlat)
      slats = SIN(sunlat)
      tlats = slats/clats
!...  Solar constant at Mars orbital radius
      F0 = 1367.D0/(au**2)
      sitelat = sitla*dtr
      clat = COS(sitelat)
      slat = SIN(sitelat)
      tlat = SIGN(99999.9D0,slat)
      If(clat .ne. 0.0D0)tlat = slat/clat
!...  dlon = length of solar day at site latitude
      cosdlon = -tlat*tlats
!...  polar = temperature adjustment factor in polar region, at
!     locations where daily insolation is zero
      polar = 0.D0
      crlat = 0.D0
      If (ABS(cosdlon) .gt. 1.D0)then
        If(sitla*sunla.lt.0.D0)Then
!...      crlat = latitude at which daily insolation = 0
          If (sunla.gt.0.0D0)Then
            crlat = sunla - 90.D0
          Else
            crlat = sunla + 90.D0
          Endif
!...      polar = factor for polar temperature correction
          polar = (ABS(sitla-crlat))/25.19D0
          dlon = 0.D0
        Else
          dlon = pi
        Endif
        sindlon = 0.D0
      Else
        sindlon = SQRT(1.D0 - cosdlon**2)
        dlon = pi/2.D0
        If(cosdlon.ne.0.D0)dlon = ATAN(sindlon/cosdlon)
        If (dlon .le. 0.0D0)dlon = pi + dlon
      Endif
!...  avcos = daily average cosine solar zenith angle at site latitude
      avcos = (slats*slat*dlon + clats*clat*sindlon)/pi
      If (avcos .lt. 0.0D0)avcos = 0.D0
!...  Set dust optical depth and single-scatter albedo
      delta = 0.3D0
      ssalb = 0.85D0
      atrans = ssalb/2.D0
!...  avmu = cosine solar zenith angle at noon
      avmu = slat*slats + clat*clats
!...  Compute daily-average solar transmittance
      If (avmu.le.0.0D0)Then
        avmu = 0.0D0
        taubar = 0.0D0
      Else
        taubar = atrans + (1.D0 - atrans)*EXP(-delta*(1.D0/avmu))
      Endif
!...  Get surface albedo
      albedo = ALBMS(als,sitla)
!...  Daily solar radiation absorbed at surface
      Absorb = taubar*(1.D0 - albedo)*F0*avcos
!...  Mimimum cosine solar zenith angle
      pmmu = slat*slats - clat*clats
      If (pmmu.lt.0.0D0)pmmu = 0.0D0
!...  Amplitude of diurnal heating
      Qa = taubar*(1.D0 - albedo)*F0*(avmu - pmmu)/2.D0
!...  Determine limits of northern and southern polar caps
      capnorth = 90.D0 - polecap(90.D0,als)
      capsouth = -90.D0 + polecap(-90.D0,als)
!...  Compute daily average surface temperature
      If (sitla.gt.capnorth.or.sitla.lt.capsouth)Then
        Tavg = 140.5D0 + 0.2336D0*Absorb - 8.5D0*polar
      Else
        Tavg = 140.5D0 + 0.8221D0*Absorb - 0.001425D0*(Absorb**2)
      Endif
!...  Compute amplitude of daily surface temperature variation
      Tamp = 0.16D0*Qa
!...  minimum and maximum daily temperatures at site latitude
      Tmax = Tavg + Tamp
      Tmin = Tavg - Tamp
!...  Daily average surface temperature, without dust storm
      T0bar = Tavg
!...  Compute effects of dust storm (if any) on Tavg, Tmax and Tmin
      If (dustT .gt. 0.0D0)then
        abslat = ABS(sitla)
        If (abslat .le. 50.D0) then
          Tavg = Tavg - (7.D0 - 1.28D-6*abslat**4)*dustT
        Else
          Tavg = Tavg + (7.5D0 - 0.13D0*abslat)*dustT
        Endif
        Tamp = (0.5D0 - 0.25D0*dustT)*(Tmax - Tmin)/T0bar
        Tmax = Tavg*(1.D0 + Tamp)
        Tmin = Tavg*(1.D0 - Tamp)
      Endif
!...  longitude difference from sun to site (equal solar hour angle,
!...  in degrees, measured from solar noon)
      dlo = sunlo - sitlo
      if (dlo .gt. 180.D0)dlo = dlo - 360.D0
      if (dlo .lt. -180.D0)dlo = dlo + 360.D0
      Tsurf = Tdiurnal(dlo,Tmin,Tmax)
      Return
      END
