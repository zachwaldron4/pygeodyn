!$AXOFF
      SUBROUTINE AXOFF( TCSTAR, CFSTAR, NM, DAXOC )
!********1*********2*********3*********4*********5*********6*********7**
! AXOFF          08/07/91        9107.3  WRITTEN BY LUCIA TSAOUSSI
!
! FUNCTION:        COMPUTES THE CORRECTION TO VLBI DELAY MEASUREMENTS
!                  DUE TO ANTENNA AXIS OFFSET DISPLACEMENT
!
! * SOME COMMENTS WERE TAKEN FROM CALC
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!   CFSTAR   I    A    -  THE SOURCE UNIT VECTOR IN THE CRUST
!                         FIXED GEOCENTRIC REFERENCE SYSTEM
!                         AT THE NM TIMES OF OBSERVATIONS (UNITLESS)
!
!   TCSTAR   O    A    -  THE SOURCE UNIT VECTOR IN THE
!                         TOPOCENTRIC REFERENCE SYSTEM (UNITLESS)
!
!   DAXOC     O   A    -  THE CONTRIBUTIONS TO THE DELAY DUE TO ANTENNA
!                         AXIS OFFSETS AT THE OBSERVATION SITE. (SEC)
!
! COMMENTS:   *** THE FOLLOWING VARIABLES ARE USED *****
!
!   MOUNT         -  THE ANTENNA AXIS MOUNT TYPE AT
!                    THE OBSERVATION SITE.  (UNITLESS)
!   TCTOCF(3,3)   -  THE ROTATION MATRIX WHICH ROTATES
!                    THE TOPOCENTRIC REFERENCE SYSTEM
!                    TO THE CRUST FIXED GEOCENTRIC
!                    REFERENCE SYSTEM (UNITLESS)
!   RICHM(2)      -  THE AXES ORIENTATION FOR THE RICHMOND
!                    ANTENNA SYSTEM. THE AXES ARE ORIENTED
!                    AS FOR AN EQUATORIAL MOUNT AT LATITUDE
!                    39.06 DEGREES AND ROTATED 0.12 DEGREES
!                    WEST OF NORTH.
!   CFTOTC(3,3)   -  THE 3x3 ROTATION MATRIX WHICH ROTATES
!                    THE GEOCENTRIC CRUST FIXED REFERENCE
!                    SYSTEM TO THE TOPOCENTRIC REFERENCE
!                    SYSTEM.  (UNITLESS)
!   TCAXIS(3)     -  THE UNIT VECTOR REPRESENTING THE
!                    ANTENNA FIXED AXIS IN THE TOPOCENTRIC
!                    REFERENCE SYSTEM.  (UNITLESS)
!   DISP          -  THE ANTENNA AXIS OFFSET AT THE OBSERVATION SITE (M)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
!
      DIMENSION  CFSTAR(NM,3), CFTOTC(3,3), RICHM(2),                   &
     &           TCAXIS(3),TCSTAR(NM,3), TCTOCF(3,3),DAXOC(NM)
!
      PARAMETER ( ZERO=0.D0,ONE=1.D0,TWO=2.D0 )
      DATA RICHM /39.06D0, 0.12D0/
      DATA KAXOD/0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!     Sections 1-8  for the calculation of the
!     partial derivatives of the component of the antenna axis
!     offsets in the apparent direction of the source and the CT
!     time derivative of that component with respect ot the antenna
!     axis offsets at site.
!
!         Identify the unit vector representing the antenna fixed axis
!         in a topocentric reference system.  The topocentric reference
!         system sits on the earth's surface at the observation station
!         with axes pointing 1) radially up (X-AXIS), 2) east (Y-axis),
!         3) North (Z-axis).
!
!          Identify the antenna axis type in order to obtain the
!          correct algorithm.  (NOTE: If the antenna axis type is
!          invalid, a message is written and the program stops.)
           MOUNT=TMOUNT
           IF ( MOUNT .EQ. 4 ) GO TO 220
           IF ( MOUNT .EQ. 2 ) GO TO 230
           IF ( MOUNT .EQ. 3 ) GO TO 240
           IF ( MOUNT .EQ. 1 ) GO TO 250
           IF ( MOUNT .EQ. 5 ) GO TO 260
           IF ( ( MOUNT .EQ. 0 ) .OR. ( MOUNT .GT. 5 ) )                &
     &     GO TO 1100
!
!          Handle the equatorial mount.  (MOUNT = 1)
!          (i.e. The antenna fixed axis points at the north celestial
!          pole.)
  220      TCAXIS(1) = SINLAT
           TCAXIS(2) = 0.D0
           TCAXIS(3) = COSLAT
           GO TO 310
!
!          Handle the an X-Y mount with the antenna fixed axis
!          pointed north-south.  (MOUNT = 2)
!          (i.e. the antenna fixed axis is in the plane of the horizon
!          pointed north-south.)
  230      TCAXIS(1) = ZERO
           TCAXIS(2) = ZERO
           TCAXIS(3) = ONE
           GO TO 310
!
!          Handle an alt-ax mount.  (MOUNT = 3)
!          (i.e. The antenna fixed axis points at the zenith.)
  240      TCAXIS(1) = ONE
           TCAXIS(2) = ZERO
           TCAXIS(3) = ZERO
           GO TO 310
!
!          Handle an X-Y mount with the antenna fixed axis pointed
!          east-west.  (MOUNT = 4)
!          (i.e. The antenna fixed axis is in the plane of the horizon
!          pointed east-west.)
  250      TCAXIS(1) = ZERO
           TCAXIS(2) = ONE
           TCAXIS(3) = ZERO
           GO TO 310
!
!          Handle the case of the Richmond antenna system with axies
!          oriented as for an equatorial mount at latitude +39.06
!          degrees and rotated 0.12  degrees west of north.
  260      ZC        =     COS(RICHM(1)*DEGRAD)
           TCAXIS(1) =     SIN(RICHM(1)*DEGRAD)
           TCAXIS(2) = -ZC*SIN(RICHM(2)*DEGRAD)
           TCAXIS(3) =  ZC*COS(RICHM(2)*DEGRAD)
!
  310   CONTINUE
!          The J2000.0 source unit vector (EHAT) has been rotated to the
!          earth fixed geocentric reference system (CFSTAR).
!
!          Compute the rotation matrix which rotates from the
!          topocentric to the geocentric crust fixed reference system
!
           CALL TOPECF ( RLAT, RLON, TCTOCF )
!
!          Compute the rotation matrix which rotates from the
!          geocentric crust fixed reference system to the topocentric
!          reference system.
!
!     Transpose the matrix TCTOCF
      DO 320  J = 1,3
        DO 300  I = 1,3
          CFTOTC(I,J) = TCTOCF(J,I)
  300   CONTINUE
  320 END DO
!
!          Rotate the source unit vector to the topocentric system
!
      DO 330 I=1,NM
         DO 330 J=1,3
         TCSTAR(I,J)=CFTOTC(J,1)*CFSTAR(I,1)+CFTOTC(J,2)*CFSTAR(I,2)    &
     &              +CFTOTC(J,3)*CFSTAR(I,3)
  330 CONTINUE
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
      DO 350 I=1,NM
      CALL AXOPTL(TCSTAR(I,1),TCAXIS,DAXOP)
      DAXOC(I)=DAXOP*DISP
  350 END DO
!
!     See if debug is needed.
      IF ( KAXOD .EQ. 0 )  GO TO 1000
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, 'Debug output for subroutine AXOFF.' )
    8 FORMAT(A,5D25.16/(9X,5D25.16))
      WRITE(6,8)' CFSTAR  ',(CFSTAR(1,J),J=1,3)
      WRITE(6,8)' CFTOTC  ',CFTOTC
      WRITE(6,8)' TCSTAR  ',(TCSTAR(1,J),J=1,3)
      WRITE ( 6, 9200 )  MOUNT,SINLAT, TCTOCF
 9200 FORMAT (1X, 'MOUNT  = ',  ( I2, 10X ), /, 1X,                     &
     &            'SINLAT = ',  ( D30.16, 10X ), /, 1X,                 &
     &            'TCTOCF = ',  ( 3 ( D30.16, 10X ), /, 1X ) )
      WRITE ( 6, 9300 )  DISP, DAXOC
 9300 FORMAT (1X, 'DISP = ',  ( D30.16, 10X ), /, 1X,                   &
     &            'DAXOC = ', ( 2 ( D30.16, 10X ), /, 1X ) )
      GO TO 1000
!     Abnormal termination.
 1100 WRITE ( 6, 9500 )  MOUNT
 9500 FORMAT (1X, 'THE PROGRAM HAS TERMINATED IN SUBROUTINE AXOFF  ',   &
     &            'MOUNT (', I2, ') = ', I2, '.' )
!     Normal conclusion.
 1000 RETURN
!
      END
