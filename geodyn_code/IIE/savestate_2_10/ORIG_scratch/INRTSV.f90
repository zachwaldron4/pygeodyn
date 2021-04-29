!$INRTSV
      SUBROUTINE INRTSV(COSTHG,SINTHG,XI,VI,NM,NDIMI)
!********1*********2*********3*********4*********5*********6*********7**
! INRTSV           83/03/28            8303.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE INERTIAL VELOCITY VECTORS FOR EARTH FIXED
!            TRACKING STATIONS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES  OF RIGHT ASCENSION OF GREENWICH
!   XI       I    A    INERTIAL CARTESIAN POSITION VECTORS
!   VI       O    A    INERTIAL CARTESIAN VELOCITY VECTORS
!   NM       I    S    NUMBER OF VECTORS TO BE CONVERTED
!   NDIMI    I    S    MAXIMUM # OF MEASUREMENTS IN ANY BLOCK
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CTHDOT/THDOT
      DIMENSION COSTHG(NM),SINTHG(NM),XI(NDIMI,3),VI(NDIMI,3)
      DATA ZERO/0.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      VI(N,1)=        -XI(N,2)*THDOT
      VI(N,2)=        +XI(N,1)*THDOT
      VI(N,3)=         ZERO
 1000 END DO
      RETURN
      END
