      SUBROUTINE UCL_JASON_panel_TRR(distance,theta,accn)
!-----------------------------------
!
!     Model built by Sima Adhya 09/05/03
!     Translated to FORTRAN77 by Ant Sibthorpe 09/05/03
!
!-----------------------------------
!
!     Returns the magnitude of the acceleration perpendicular
!     to JASON's solar panels due to thermal re-radiation when the
!     satellite is in full phase only!
!     Accounts for non-nominal pointing angles
!
!     Function simply evaluates a quadratic
!
!     Inputs: sun-satellite distance (km),
!             [non-nominal incidence] angle (radians), between the
!             incoming radiation and the solar panel normal
!     Outputs: accn(m/s^2), use this to scale the unit panel normal
!
!     Model built by Sima Adhya 09/05/03
!     Translated to FORTRAN77 by Ant Sibthorpe 09/05/03
!
!-----------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

      DOUBLE PRECISION distance,theta,accn
      DOUBLE PRECISION flux
      DOUBLE PRECISION oneAU,solarI,mass
!     value of one Astronomical Unit in km
      PARAMETER(oneAU=149597870.691)

!     solar irradiance at one AU
      PARAMETER(solarI=1368.2)
!     mass [nominal] of JASON spacecraft
      PARAMETER(mass=489.1)
      distance=distance/1000.D0

      IF(ABS(theta)>PI/2.0)THEN
          accn=0.0
      ELSE
!         evaluate quadratic
          flux=(oneAU**2/distance**2)*solarI*COS(theta)
          accn=-3.540D-13*flux**2-3.06D-10*flux+1.11D-7
!         divide by mass of spacecraft
          accn=accn/mass

      ENDIF
      RETURN
      END
