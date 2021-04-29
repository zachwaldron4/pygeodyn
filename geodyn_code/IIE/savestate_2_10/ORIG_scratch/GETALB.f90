!$GETALB
          SUBROUTINE GETALB(P,ACS,MD,MO,MMO,KM,MAXDEG,MAXORD,DLAT,      &
     &                      DLON,ALBEDO,MXD,MXO)
!********1*********2*********3*********4*********5*********6*********7**
!  GETALB          93/03/23            0000.0    PGMR: FGL
!
!  FUNCTION:  EVALUATE THE SPHERICAL HARMONIC MODEL OF THE ALBEDO OR
!             EMMISIVITY FOR THE PLANET MARS
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  P         O    A    LEGENDRE AND ASSOCIATED LEGENDRE FUNCTIONS
!  ACS       I    A    C AND S COEFFICIENT ARRAY
!  KM        I    S    ALB OR EMMIS INDICATOR
!  MAXDEG    I    S    MAXIMUM DEGREE OF MODEL
!  MAXORD    I    S    MAXIMUM ORDER OF MODEL
!  DLAT      I    S    LATITUDE (RADIANS)
!  DLON      I    S    LONGITUDE (RADIANS)
!  ALBEDO    O    S    ALBEDO (OR EMMISSIVITY) AT THE SURFACE POINT
!                      DEFINED BY DLAT , DLON
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION ACS(MD+1,MO+1,2,MMO), P(MXD+1,MXO+1)

      DIMENSION DCOSML(MAXDEG), DSINML(MAXDEG)

      SINX = SIN(DLAT)
      ALBEDO = ACS(1,1,1,KM)

      ! PRECOMPUTE COS AND SIN TERMS
      DO MM = 1, MAXDEG
          DCOSML(MM) = COS(MM*DLON)
          DSINML(MM) = SIN(MM*DLON)
      END DO

      ! FIRST EVALUATE THE ZONAL TERMS
      ! GET THE LEGENDRE FUNCTIONS FIRST: USE SUBROUTINE FROM POHOP
      CALL LEGFGL(MAXDEG, MAXORD, SINX, P, MXD, MXO)
      DO LL = 1, MAXDEG
          ALBEDO = ALBEDO + P(LL+1,1)*ACS(LL+1,1,1,KM)
      END DO

      IF (MAXORD-1 <= 0) RETURN

      ! NOW EVALUATE THE NON-ZONAL TERMS
      DO LL = 1, MAXDEG
          MEND = MIN(LL, MAXORD)
          DO MM = 1, MEND
              T1 = DCOSML(MM) * P(LL+1,MM+1) * ACS(LL+1,MM+1,1,KM)
              T2 = DSINML(MM) * P(LL+1,MM+1) * ACS(LL+1,MM+1,2,KM)
              ALBEDO = ALBEDO + T1 + T2
          END DO
      END DO

      END SUBROUTINE
