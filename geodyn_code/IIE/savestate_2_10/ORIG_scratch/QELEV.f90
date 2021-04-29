!$QELEV
      SUBROUTINE QELEV( TCSTAR,CFSTAR,NM,ELEVSC,ELEV,NELEV,RLAT,  &
     &                  RLON )
!********1*********2*********3*********4*********5*********6*********7**
! QELEV          09/13/91        9107.3  WRITTEN BY LUCIA TSAOUSSI
!
! FUNCTION:        COMPUTES THE SOURCE ELEVATION ANGLES FOR THE TWO
!                  SITES OF THE VLBI DELAY MEASUREMENT
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
!   ELEVSC  I/O   A    -  ELEVATION OF SOURCE WRT STATION
!
!   ELEV     O    S    -  ELEVATION IN RADIANS
!
!   LNELEV   I    S    -  FLAG SET TO .TRUE. WHEN ELEVATION IS NOT TO BE
!                         COMPUTED
!   NM       I    S    -  NUMBER OF MEASUREMENTS IN THIS BLOCK
!
! COMMENTS:   *** THE FOLLOWING VARIABLES ARE USED *****
!
!   TCTOCF(3,3)   -  THE ROTATION MATRIX WHICH ROTATES
!                    THE TOPOCENTRIC REFERENCE SYSTEM
!                    TO THE CRUST FIXED GEOCENTRIC
!                    REFERENCE SYSTEM (UNITLESS)
!   CFTOTC(3,3)   -  THE 3x3 ROTATION MATRIX WHICH ROTATES
!                    THE GEOCENTRIC CRUST FIXED REFERENCE
!                    SYSTEM TO THE TOPOCENTRIC REFERENCE
!                    SYSTEM.  (UNITLESS)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO=0.D0,ONE=1.D0,TWO=2.D0 )
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!     COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT, &
!    &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
!
      DATA KELEV/0/
!
      DIMENSION  CFSTAR(NM,3), CFTOTC(3,3),ELEVSC(NM), &
     &           TCSTAR(NM,3), TCTOCF(3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      IF (LNELEV)  GO TO 1000
!
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
  320 CONTINUE
!
!          Rotate the source unit vector to the topocentric system
!
      DO 330 I=1,NM
         DO 330 J=1,3
         TCSTAR(I,J)=CFTOTC(J,1)*CFSTAR(I,1)+CFTOTC(J,2)*CFSTAR(I,2) &
     &              +CFTOTC(J,3)*CFSTAR(I,3)
         ELEVSC(I)=DARSN(TCSTAR(I,1))/DEGRAD
         ELEV=ELEVSC(I)*DEGRAD
  330 CONTINUE
!
!
!     See if debug is needed.
      IF ( KELEV .EQ. 0 )  GO TO 1000
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, 'Debug output for subroutine QELEV.' )
    8 FORMAT(A,5D25.16/(9X,5D25.16))
      WRITE(6,8)' CFSTAR  ',(CFSTAR(1,J),J=1,3)
      WRITE(6,8)' CFTOTC  ',CFTOTC
      WRITE(6,8)' TCSTAR  ',(TCSTAR(1,J),J=1,3)
      WRITE ( 6, 9300 )  ELEV,ELEVSC
 9300 FORMAT (1X, 'ELEVSC = ', 2 ( D30.16, 10X ), /, 1X )
!     Normal conclusion.
 1000 RETURN
!
      END
