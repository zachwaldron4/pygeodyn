!$PRESSURE
      Subroutine Pressure(psurf,thgt,gzero,rref,Rbar,T,gam,p,z)
!********1*********2*********3*********4*********5*********6*********7**
! PRESSURE         00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: CALCULATE PRESSURE AND AREOGRAPHIC HEIGHTS AT SIGNIFICANT
!           HEIGHT LEVELS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   psurf    I    S    SURFACE PRESSURE
!   thgt     I    S    LOCAL TERRAIN HEIGHT (KM)
!   gzero    I    S    SURFACE GRAVITY
!   Rbar     I    S    GAS CONSTANT
!   rref     I    S    LOCAL RADIUS OF REFERENCE ELLIPSOID (KM)
!   T        I    A    TEMPERATURE AT SIGNIFICANT LEVEL ( T(5))
!   gam      I    A    LAPSE RATES ( gam(5))
!   p        O    A    PRESSURE AT SIGNIFICANT HEIGHT LEVELS
!   z        O    A    AREOGRAPHIC ALTITUDE AT SIGNIFICANT LEVELS
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  Computes pressures, p(i) (N/m**2), at significant height
!...  levels from surface pressure, psurf (N/m**2), terrain
!...  height, thgt (areopotential km), surface gravity, gzero
!...  (m/s**2), local radius of reference ellipsoid, rref (km),
!...  Gas Constant, Rbar m**2/(s**2 K), temperatures, T(i) (K),
!...  and lapse rates, gam(i) (deg./km).
!...  Also computes the areographic altitudes, z(i) (km), relative to
!...   the reference ellipsoid, corresponding to the significant levels
      Dimension T(0:5),p(0:5),dh(5),gph(5),gam(5),z(0:5)
!...  Height increments between significant levels (areopotential km)
      Data dh/5.,10.,15.,20.,25./
!...  Heights of significant levels (areopotential km)
      Data gph/5.,15.,30.,50.,75./
!...  surface gravity/gas constant in mks units
      goR = 1000.*gzero/Rbar
!...  areographic height from areopotential height
      z(0) = thgt/(1. - thgt/rref)
      p(0) = psurf
!...  Step through the significant levels
      Do 10 i = 1,5
        z(i) = (gph(i)+thgt)/(1. - (gph(i)+thgt)/rref)
!...    pressures from constant lapse rate between significant levels
        If (ABS(gam(i)) .gt. 0.001)then
          p(i) = p(i-1)*EXP(-goR*LOG(T(i-1)/T(i))/gam(i))
        Else
          p(i) = p(i-1)*EXP(-goR*dh(i)/T(i-1))
        Endif
   10 Continue
      Return
      END
