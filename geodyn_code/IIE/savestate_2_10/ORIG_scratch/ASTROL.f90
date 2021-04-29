!===================================================================
      SUBROUTINE ASTROL( TIME, SHPN )
!
!  Computes the basic astronomical mean longitudes  s, h, p, N.
!  Note N is not N', i.e. N is decreasing with time.
!  These formulae are for the period 1990 - 2010, and were derived
!  by David Cartwright (personal comm., Nov. 1990).
!  TIME is UTC in decimal MJD.
!  All longitudes returned in degrees.
!  R. D. Ray    Dec. 1990
!
!  Non-vectorized version.
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION  SHPN(4)
      PARAMETER  (CIRCLE=360.0D0)
!
      T = TIME - 51544.4993D0
!
!     mean longitude of moon
!     ----------------------
      SHPN(1) = 218.3164D0 + 13.17639648D0 * T
!
!     mean longitude of sun
!     ---------------------
      SHPN(2) = 280.4661D0 +  0.98564736D0 * T
!
!     mean longitude of lunar perigee
!     -------------------------------
      SHPN(3) =  83.3535D0 +  0.11140353D0 * T
!
!     mean longitude of ascending lunar node
!     --------------------------------------
      SHPN(4) = 125.0445D0 -  0.05295377D0 * T

      SHPN(1) = MOD(SHPN(1),CIRCLE)
      SHPN(2) = MOD(SHPN(2),CIRCLE)
      SHPN(3) = MOD(SHPN(3),CIRCLE)
      SHPN(4) = MOD(SHPN(4),CIRCLE)

      IF (SHPN(1).LT.0.D0) SHPN(1) = SHPN(1) + CIRCLE
      IF (SHPN(2).LT.0.D0) SHPN(2) = SHPN(2) + CIRCLE
      IF (SHPN(3).LT.0.D0) SHPN(3) = SHPN(3) + CIRCLE
      IF (SHPN(4).LT.0.D0) SHPN(4) = SHPN(4) + CIRCLE
      RETURN
      END
