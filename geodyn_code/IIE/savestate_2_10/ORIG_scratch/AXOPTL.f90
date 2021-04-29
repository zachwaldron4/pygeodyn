!$AXOPTL
      SUBROUTINE AXOPTL(CTCSTR, TCAXIS, DAXOP )
!********1*********2*********3*********4*********5*********6*********7**
! AXOPTL          08/14/91        0000.0  WRITTEN BY LUCIA TSAOUSSI
!
! FUNCTION:       COMPUTES THE PARTIAL DERIVATIVES OF THE VLBI DELAY
!                 WRT THE ANTENNA AXIS OFFSET AT SITE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!  CTCSTR   I/O   A    -  THE SOURCE UNIT VECTOR IN THE
!                         TOPOCENTRIC REFERENCE SYSTEM
!                         CORRECTED FOR ATMOSPHERIC REFRACTION.
!                         (UNITLESS)
!
!  TCAXIS    I    A    -  THE UNIT VECTOR REPRESENTING THE
!                         ANTENNA FIXED AXIS IN THE TOPOCENTRIC
!                         REFERENCE SYSTEM.  (UNITLESS)
!
!  DAXOP     O    S    -  THE PARTIAL DERIVATIVES OF THE DELAY
!                         AND OF THE DELAY RATE WITH RESPECT TO
!                         THE ANTENNA AXIS OFFSETS AT OBSERVATION SITE.
!                         (SEC/M)
!
! COMMENTS: VARIABLES USED IN THIS ROUTINE ARE:
!
! AZMUTH         -  THE AZIMUTH OF THE TOPOCENTRIC SOURCE
!                   UNIT VECTOR.  (RAD)
! TANEL          -  THE TANGENT OF THE ELEVATION ANGLE.
!                   (UNITLESS)
! XI             -  THE ANGLE BETWEEN THE FIXED ANTENNA
!                   AXIS AND THE CORRECTED SOURCE UNIT
!                   VECTOR.  (RAD)
! ZEN            -  THE ZENITH ANGLE OF THE SOURCE UNIT
!                   VECTOR.  (RAD)
! ZENCOR         -  THE CORRECTION ANGLE TO THE SOURCE
!                   UNIT VECTOR DUE TO ATMOSPHERIC
!                   REFRACTION.  (RAD)
! SURREF         -  THE SURFACE REFRACTIVITY AT RADIO WAVE-
!                   LENGTHS IN N UNITS.
!                   N = (1 - REFRACTIVITY) * 10**6
! DCOMP          -  THE PARTIAL DERIVATIVES OF THE
!                   COMPONENT OF THE ANTENNA AXIS
!                   OFFSET IN THE APPARENT DIRECTION OF THE SOURCE
!                   WITH RESPECT TO THE ANTENNA AXIS OFFSETS AT SITE
!                   (UNITLESS)
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION  CTCSTR(3), TCAXIS(3)
      PARAMETER ( SURREF=313.0D0,ZERO=0.D0,ONE=1.D0,TWO=2.D0 )
      DATA D1M6/1.0D-06/,KAXOD/0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!   *****  START EXECUTABLE CODE  ***********
!
!         Compute the topocentric source unit vector corrected for
!         atmospheric refraction.  (NOTE: Due to atmospheric refraction,
!         the antenna does not point at the geometric position of the
!         source thus the component of the antenna axis offset in
!         the direction of the apparent source position is the parameter
!         of interest.)
!         The refraction correction which we are implementing is a "High
!         Elevation  (Zenith distance less than 80 degrees) model.
!         however, at low elevation (zenith distance greater than 80
!         degrees) the error relative to a ray tracing program is
!         a maximum of 10 arcseconds.
!
!         Compute the zenith angle of the source.
          ZEN = DARCS (CTCSTR(1) )
!         Compute the tangent of the elevation angle.
          HALFPI= PI/TWO
          TANEL = TAN(HALFPI - ZEN)
!
!         Compute the azimuth of the source.
          AZMUTH = ATAN2 (CTCSTR(2),CTCSTR(3) )
!
!         Compute the correction angle.
          ZENCOR = SURREF * D1M6 * (ONE/TANEL)
!
!         Compute the corrected topocentric star unit vector.
!         (NOTE: This corrects the vector so that it is pointing
!         nearer to the zenith).
          CTCSTR(1) = COS ( ZEN - ZENCOR )
          CTCSTR(2) = SIN ( ZEN - ZENCOR ) * SIN ( AZMUTH )
          CTCSTR(3) = SIN ( ZEN - ZENCOR ) * COS ( AZMUTH )
!
!         Compute the angle between the apparent direction to the
!         source and that of the antenna fixed axis.
          DOTP=ZERO
          DO 100 I=1,3
          DOTP = DOTP + TCAXIS(I)*CTCSTR(I)
  100     CONTINUE
          XI = DARCS ( DOTP )
!
!         Complete the calculation of the partial derivative of the
!         component of the antenna axis offset in the apparent direction
!         of the source with respect to the antenna azis offsets at
!         each observing site.
          DCOMP = SIN ( XI )
!
!     Compute the partial derivatives of the delay and rate with
!     respect to the antenna axis offsets at each site.  (NOTE:
!     The minus sign appears dud to the definition of the
!     baseline vector being the site#1 vector minus the site#2
!     vector.)
!
           DAXOP = + DCOMP / VLIGHT
!
!
!  ****  THE FOLLOWING CODE FOR FUTURE USE  ******
!         Compute the partial derivative of the CT time derivative of
!         the component of the antenna axis offset in the apparent
!         direction of the soruce with respect to the antenna axis
!         offsets at each observation site.
!         (NOTE: The assumption is made here that the only contribution
!         to the time derivative is the rotation of the earth, i.e., the
!         effects of temporal changes in the atmosphere are ignored.
!
!           Compute the CT time derivative of the source unit vector
!           in the topocentric reference system.
!
!           CALL MTRAN ( R2000(1,1,2), TR2000(1,1,2) )
!           CALL VECRT ( TR2000(1,1,2), STAR, CFSTAR(1,2) )
!           CALL VECRT ( CFTOTC, CFSTAR(1,2), TCSTAR(1,2) )
!
!           Complete the calculation.
!           DCOMP(L,2) = - DOTP ( TCAXIS, TCSTAR(1,2) ) / DTAN ( XI )
!
!           DAXOP(2,K) = - DCOMP(2,K) / VLIGHT
!
!     *****  END OF FUTURE USE CODE   **********
!
!     See if debug is needed.
      IF ( KAXOD .EQ. 0 )  GO TO 1000
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, 'Debug output for subroutine AXOPTL.' )
      WRITE(6,8)' AZMUTH  ',AZMUTH
    8 FORMAT(A,5D25.16/(9X,5D25.16))
      WRITE(6,8)' CTCSTR  ',CTCSTR
      WRITE(6,8)' TANEL   ',TANEL
      WRITE(6,8)' TCAXIS  ',TCAXIS
      WRITE(6,8)' XI      ',XI
      WRITE(6,8)' ZEN     ',ZEN
      WRITE(6,8)' ZENCOR  ',ZENCOR
      WRITE(6,8)' SURREF  ',SURREF
      WRITE(6,8)' HALFPI  ',HALFPI
      WRITE(6,8)' DCOMP  ',DCOMP
      WRITE(6,8)' DAXOP  ',DAXOP
!
 1000 RETURN
      END
