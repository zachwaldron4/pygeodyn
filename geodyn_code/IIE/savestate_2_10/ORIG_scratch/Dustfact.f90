

!$DUSTFACT
      Subroutine Dustfact(CLAT,CLON,HGT,als,dustM,dustA)
!********1*********2*********3*********4*********5*********6*********7**
! DUSTFACT         00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: CALCULATE RELATIVE DUST STORM INTENSITY FACTORS DUSTM
!           AND DUSTA AS A FUNCTION OF TIME SINCE START OF STORM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!      HGT   I    S    SPACECRAFT HEIGHT
!     CLAT   I    S    SPACECRAFT LATITUDE (DEGREES)
!     CLON   I    S    WEST LONGITUDE OF SPACECRAFT (DEGREES)
!     ALS    I    S    AREOCENTRIC LONGITUDE OF SUN
!     dustM  O    S    dust storm magnitude for average T and p effect
!                      (1 = full magnitude, 0 = no dust storm)
!     dustA  O    S    dust storm magnitude for daily amplitude T and
!                      p effect (1 = full magnitude, 0 = no dust storm)
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  Computes relative dust storm intensity factors dustM and
!...  dustA as a function of the time since start of the storm,
!...  (als - als0), measured in Ls angle (degrees), and as a
!...  function of the storm intensity, intens.  dustM is for
!...  relative magnitude of effect on daily average temperature
!...  and pressure.  dustA is for relative magnitude of effect on
!...  diurnal (and semidiurnal) amplitudes for temperature and
!...  pressure.
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!      COMMON /DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,
!     & RREF,als0,SNTENS,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,
!     & CFPMS,SPOPT,F107,STDLMS,AUMARS,gzerom,gzms
      COMMON/DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,   &
     & RREF,als0,INTENS,idum1,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,     &
     & CFPMS,IPOPT,idum2,F107,STDLMS,AUMARS,gzerom,gzms
      dls = als - als0
!...  Return dust factor of 0 if Ls-Ls0 < 0 or > 48 degrees
      If (dls .le. 0.0D0 .or. dls .gt. 48.0D0)then
        dustM = 0.0D0
        dustA = 0.0D0
        Return
      Endif
!...  Compute initial dustM factor (0-1) from time (Ls) profile
      If (dls .le. 6.0D0)then
        dustM = dls/6.D0
      Else If (dls .ge. 24.D0)then
        dustM = 2.D0 - dls/24.D0
      Else
        dustM = 1.0D0
      Endif
!...  Compute initial dustA factor (0-1) from time (Ls) profile
      If (dls .le. 9.0D0)then
        dustA = dls/9.0D0
      Else If (dls .ge. 18.D0)then
        dustA = (48.D0 - dls)/30.D0
      Else
        dustA = 1.0D0
      Endif
      sizefact = 1.0D0
!...  Compute parameters of local storm if radmax is not 0
      If (radmax .ne. 0.0D0)Then
        sizefact = 0.0D0
!...    dns,dew,rad = horizontal coordinates from center of dust storm
        dns = DEGRAD*Rref*(CLAT - dustlat)
        dew = DEGRAD*Rref*COS(DEGRAD*CLAT)*(CLON - dustlon)
        rad = SQRT(dns**2 + dew**2)
!...    raddust, hgtdust = actual horizontal and vertical size of storm
        raddust = dustM*radmax
        hgtdust = dustM*dusthgt
!...    sizefact = position-dependent measure of relative storm effect
        If (rad .lt. 2.0D0*raddust .and. HGT .lt. 2.0D0*hgtdust)        &
     &   sizefact = 0.25D0*(1.0D0 + COS(90.D0*DEGRAD*rad/raddust))*    &
     &   (1.0D0 + COS(90.D0*DEGRAD*HGT/hgtdust))
      Endif
!...  Final factors dependent on position and on storm intensity
      dustM = sizefact*dustM*DBLE(intens)/3.D0
      dustA = sizefact*dustA*DBLE(intens)/3.D0
      Return
      END
