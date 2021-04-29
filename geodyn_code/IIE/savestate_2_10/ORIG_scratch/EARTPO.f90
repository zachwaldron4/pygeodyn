!$EARTPO
      SUBROUTINE EARTPO(NM, MINTIM, MJDSBL, FSECN, REFMT,               &
     &           XSAT, EPRAE, EPDECE,AA,II )
!********1*********2*********3*********4*********5*********6*********7**
! EARTPO           89/06/10            0000.0    PGMR - JJM
!
!  FUNCTION: TO GET THE EARTH'S RA AND DECLINATION AS SEEN BY A
!            MARS ORBITING SATELLITE FOR COMPARISON WITH INFORMATION
!            ON THE DATA RECORD
!            USED ONLY FOR DEBUG AT THE PRESENT TIME.
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF MEASUREMENTS IN THE DATA BLOCK
!   MINTIM   I    S    MAXIMUM VALUE FOR NM, DEFINED IN IIS
!   MJDSBL   I    S    TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!   FSECN    I    A    TIME IN FRACTIONAL SECONDS FOR EACH MEASUREMENT
!   REFMT    I    A    ROTATION MATRIX TO GO FROM 1950.0 EARTH EQUATOR
!                      AND EQUINOX (EQEQNX) SYSTEM TO TRUE OF
!                      REFERENCE MARS EQUATOR AND NODE ON EARTH 1950.
!                      EQUATOR (IAU) SYSTEM
!   XSAT     I    A    MARS CENTERED COORDINATES OF THE SATELLITE IN THE
!                      IAU MARS COORDINATE SYSTEM
!   EPRAE    O    A    RIGHT ASCENSION OF THE EARTH IN THE IAU
!                      COORDINATE SYSTEM AS SEEN BY THE SATELLITE AT
!                      POSITION 'XSAT'
!   EPDECE   O    A    DECLINATION  OF THE EARTH IN THE IAU COORDINATE
!                      SYSTEM AS SEEN BY THE SATELLITE AT POSITION
!                      'XSAT'
!
! COMMENTS:  USED ONLY FOR DEBUG AT THE PRESENT TIME
!
! REFERENCES:  DUXBURY, T.C. AND J. D. CALLAHAN, "PHOBOS AND DEIMOS
!              MEASUREMENTS FROM VIKING", ASTRON. ASTROPHYS., V.201,
!              PP. 169-176, 1988
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CBARYC/CBODY(6),TBODY(6),SUNRF(6)
!
      DIMENSION FSECN(NM), EPRAE(500), EPDECE(500), XSAT(MINTIM,3),     &
     &          REFMT(9), XES(3)
      DIMENSION AA(1),II(1)
!
      DATA ONE/1.D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
!C    WRITE(6,*) 'EARTPO: NM, MJDSBL, FSECN(1) ', NM, MJDSBL, FSECN(1)
!C    WRITE(6,*) 'EARTPO: XSAT(1,123) ', XSAT(1,1),XSAT(1,2),XSAT(1,3)
!
      DO 10 K=1,NM
!----------------------------------------------------------------------
!
!     ....GET EARTH POSITION IN 1950.0 E EQEQNX FROM EPHEMERIS
!
      CALL PLANPO( MJDSBL, FSECN(K),.FALSE., .FALSE.,AA,II )
!
!C    WRITE(6,*) 'EARTPO: EARTH VEC FROM MARS', BDSTAT(1,9),
!C   1             BDSTAT(2,9), BDSTAT(3,9)
!----------------------------------------------------------------------
!
!     ....COMPUTE ITS RA, DEC AND ROTATE TO MARS FRAME
!
      XYVEC     = SQRT( BDSTAT(1,9)**2 + BDSTAT(2,9)**2 )
      REARTH    = SQRT( XYVEC**2 + BDSTAT(3,9)**2 )
      EPRAE(K)  = ATAN2( BDSTAT(2,9), BDSTAT(1,9) )
      EPDECE(K) = ATAN2( BDSTAT(3,9), XYVEC )
      EPRAE(K)  = MOD( EPRAE(K) + TWOPI, TWOPI )
!
!C    WRITE(6,*) 'EARTPO: K, 1950 EPRAE, EPDECE IN RAD FROM MARS'
!C    WRITE(6,*) K, EPRAE(K), EPDECE(K)
!
      RADEG  = EPRAE(K)  / DEGRAD
      DECDEG = EPDECE(K) / DEGRAD
!
!C    WRITE(6,*) 'EARTPO: K, 1950 EPRAE, EPDECE IN DEG FROM MARS'
!C    WRITE(6,*) K, RADEG, DECDEG
!----------------------------------------------------------------------
!
!     ....CONVERT TO MARS FRAME COORDINATES
!
      CALL RRADEC(1, EPRAE(K), EPDECE(K), REFMT, EPRAE(K), EPDECE(K) )
!
!     ....COMPUTE VECTOR FROM SPACECRAFT TO EARTH AND COMPUTE RA
!
      XES(1) = REARTH * COS(EPDECE(K)) * COS(EPRAE(K)) - XSAT(K,1)
      XES(2) = REARTH * COS(EPDECE(K)) * SIN(EPRAE(K)) - XSAT(K,2)
      XES(3) = REARTH * SIN(EPDECE(K))                  - XSAT(K,3)
!
!C    WRITE(6,*) 'EARTPO: EARTH VEC FROM S/C', XES(1), XES(2), XES(3)
!
      XYVEC     = SQRT( XES(1)**2 + XES(2)**2 )
      EPRAE(K)  = ATAN2( XES(2), XES(1) )
      EPDECE(K) = ATAN2( XES(3), XYVEC )
      EPRAE(K)  = MOD( EPRAE(K) + TWOPI, TWOPI )
!
      RADEG  = EPRAE(K)  / DEGRAD
      DECDEG = EPDECE(K) / DEGRAD
!
      WRITE(6,*) 'EARTPO: K, 1950 EPRAE, EPDECE IN DEG FROM SAT '
      WRITE(6,*)  K, RADEG, DECDEG
!
!----------------------------------------------------------------------
   10 END DO
      RETURN
      END
