!$ANTCOR
      SUBROUTINE ANTCOR(CTCSTR, SINEL, COSEL, HSCALE, ZCORR)
!********1*********2*********3*********4*********5*********6*********7**
! ANTCOR          12/30/91        0000.0  WRITTEN BY LUCIA TSAOUSSI
!
! FUNCTION:  CALCULATES THE CORRECTION TO THE DRY ZENITH TROPOSPH. DELAY
!            DUE TO AXIS OFFSETS AND MOTION OF FEED RELATIVE TO THE
!            INTERSECTION OF AXES. CORRECTION DEPENDS ON THE MOUNT TYPE,
!            THE SOURCE POSITION AND THE AXIS OFFSET VALUE.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!  CTCSTR    I    A    -  THE SOURCE UNIT VECTOR IN THE
!                         TOPOCENTRIC REFERENCE SYSTEM
!                         CORRECTED FOR ATMOSPHERIC REFRACTION.
!                         (UNITLESS)
!
!  SINEL     I    S    -  THE SIN OF THE ELEVATION ANGLE.
!                         (UNITLESS)
!  COSEL     I    S    -  THE COSIN OF THE ELEVATION ANGLE.
!                         (UNITLESS)
!  HSCALE    I    S    -  THE SCALE HEIGHT OF THE TROPOSPHERE
!
!  ZCORR     O    S    -  THE CORRECTION TO THE DRY ZENITH
!                         TROPOSPHERIC DELAY DUE TO AXIS OFFSET AND
!                         THE THE FEED MOTION AT OBSERVATION SITE.
!                         (M)
!
! COMMENTS: VARIABLES USED IN THIS ROUTINE ARE:
!
! AZMUTH         -  THE AZIMUTH OF THE TOPOCENTRIC SOURCE
!                   UNIT VECTOR.  (RAD)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      DIMENSION  CTCSTR(3)
      DATA ZERO/0.0D0/,ONE/1.0D0/, KAXOD/0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!   *****  START EXECUTABLE CODE  ***********
!
!
!         Compute the azimuth of the source.
          AZMUTH = ATAN2 (CTCSTR(2),CTCSTR(3) )
!
!         Compute the correction angle.
      MOUNT=TMOUNT
      IF (MOUNT.EQ.4) THEN
!         Compute the SOURCE HOUR angle first.
!
      SINDEC=SINEL*SINLAT+COSEL*COSLAT*COS(AZMUTH)
      COSDEC=SQRT(ONE-SINDEC**2)
      COSHA=(SINEL-SINDEC*SINLAT)/(COSDEC*COSLAT)
      ZCORR=(DISP/HSCALE)*COSLAT*COSHA
      GO TO 999
      END IF
      IF (MOUNT.EQ.2) THEN
!         Compute the correction angle.
      SINAZ=SIN(AZMUTH)
      XY=SQRT(SINAZ**2+SINEL**2-(SINAZ*SINEL)**2)
      ZCORR=(DISP/HSCALE)*SINEL/XY
      GO TO 999
      END IF
!  FOR OTHER MOUNT TYPES
      ZCORR=ZERO
!
  999 CONTINUE
!     See if debug is needed.
      IF ( KAXOD .EQ. 0 )  GO TO 1000
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, 'Debug output for subroutine ANTCOR.' )
      WRITE(6,8)' AZMUTH  ',AZMUTH
    8 FORMAT(A,5D25.16/(9X,5D25.16))
      WRITE(6,8)' CTCSTR  ',CTCSTR
      WRITE(6,8)' SINEL   ',SINEL
      WRITE(6,8)' COSEL   ',COSEL
      WRITE(6,8)' HSCALE  ',HSCALE
      WRITE(6,8)' TMOUNT  ',TMOUNT
      WRITE(6,8)' ZCORR  ',ZCORR
      WRITE(6,8)' DISP  ',DISP
!
 1000 RETURN
      END
