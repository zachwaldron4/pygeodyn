!$ECFIXV
      SUBROUTINE ECFIXV(VI,COSTHG,SINTHG,XE,VE,NDIMI,NDIME,NM)
!********1*********2*********3*********4*********5*********6*********7**
! ECFIXV           83/03/28            8303.0    PGMR - TOM MARTIN
!
! FUNCTION:  CONVERT INERTIAL CARTESIAN VELOCITY VECTORS
!            TO EARTH CENTERED FIXED CARTESIAN
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   VI       I    A    INERTIAL CARTESIAN VELOCITY VECTORS
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES  OF RIGHT ASCENSION OF GREENWICH
!   XE       I    A    EARTH CENTERED FIXED POSITION VECTORS
!   VE       O    A    EARTH CENTERED FIXED VELOCITY VECTORS
!   NDIMI    I    S    SAME AS NM
!   NDIME    I    S    SAME AS NM
!   NM       I    S    NUMBER OF VECTORS TO BE CONVERTED
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CTHDOT/THDOT
      DIMENSION VI(NDIMI,3),COSTHG(NM),SINTHG(NM),XE(NDIME,3),          &
     &          VE(NDIME,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      CALL ECFIXP(VI,COSTHG,SINTHG,VE,NDIMI,NDIME,NM)
      DO 1000 N=1,NM
      VE(N,1)= VE(N,1)+XE(N,2)*THDOT
      VE(N,2)= VE(N,2)-XE(N,1)*THDOT
 1000 END DO
      RETURN
      END
