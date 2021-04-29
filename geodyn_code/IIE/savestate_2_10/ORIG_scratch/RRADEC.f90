!$RRADEC
      SUBROUTINE RRADEC(NM, OBS, OBS2, REFMT, OBSMR1, OBSMR2)
!********1*********2*********3*********4*********5*********6*********7**
! RRADEC           89/04/24            0000.0    PGMR - JJM
!
!  FUNCTION:  TRANSFORM RA DEC OBSERVATIONS FROM EARTH MEAN
!             EQUATOR AND EQUINOX OF 1950.0 TO THE TRUE OF
!             REFERENCE MARS FRAME TO COMPUTE RESIDUALS CORRECTLY
!             FOR THE MARINER/VIKING OBSERVATIONS
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF MEASUREMENTS IN THE BLOCK
!   INDSAT   O    A    FOR A GIVEN INTERNAL SAT NO. (1,2,3) INDSAT GIVES
!                      THE ACTUAL LOCATION IN THE SATELLITE RELATED
!                      ARRAYS FOR THIS SATELLITE
!   OBS      I    A    R.A. OF MOON IN EARTH MEAN EQEQX OF 1950.0
!   OBS2     I    A    DECLINATION OF MOON IN EARTH MEAN EQEQX OF 1950.0
!   REFMT    I    A    MATRIX TO ROTATE A VECTOR FROM EARTH MEAN
!                      EQUATOR AND EQUINOX OF 1950.0 TO THE TRUE
!                      OF REFERENCE MARS EQUATOR AND EQUINOX (IAU) FRAME
!   OBSMR1   O    A    R.A. OF MOON IN TRUE OF REF MARS FRAME
!   OBSMR2   O    A    DECLINATION OF MOON IN TRUE OF REF MARS FRAME
!
!
! COMMENTS:  USED WITH THE MARINER/VIKING RA/DEC MEASUREMENTS
!
! REFERENCES:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION OBS(NM),    OBS2(NM)
      DIMENSION OBSMR1(NM), OBSMR2(NM)
      DIMENSION XTEMP1(3), XTEMP2(3),  REFMT(9)
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
!
!
      DO 100 I=1,NM
!
!
!-----------------------------------------------------------------------
!
!     ....CONVERT OBS, OBS2 TO A VECTOR
!
      XTEMP1(1) = COS(OBS2(I)) * COS(OBS(I))
      XTEMP1(2) = COS(OBS2(I)) * SIN(OBS(I))
      XTEMP1(3) = SIN(OBS2(I))
!
!-----------------------------------------------------------------------
!
!     ....ROTATE XTEMP1 VECTOR FROM EARTH EQ & EQX TO
!     ....TOR MARS EQ EQX BY MULTIPLYING BY (REFMT)**T
!
      XTEMP2(1)=REFMT(1)*XTEMP1(1)+REFMT(2)*XTEMP1(2)+REFMT(3)*XTEMP1(3)
      XTEMP2(2)=REFMT(4)*XTEMP1(1)+REFMT(5)*XTEMP1(2)+REFMT(6)*XTEMP1(3)
      XTEMP2(3)=REFMT(7)*XTEMP1(1)+REFMT(8)*XTEMP1(2)+REFMT(9)*XTEMP1(3)
!
!-----------------------------------------------------------------------
!
!     ....CONVERT XTEMP2 TO RA DEC
!
      XYVEC = SQRT( XTEMP2(1)**2 + XTEMP2(2)**2 )
      OBSMR1(I) = ATAN2( XTEMP2(2), XTEMP2(1) )
      OBSMR2(I) = ATAN2( XTEMP2(3) , XYVEC )
!
!
  100 END DO
!
      RETURN
      END
