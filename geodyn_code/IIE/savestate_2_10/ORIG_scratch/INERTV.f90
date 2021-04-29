!$INERTV
      SUBROUTINE INERTV(VE,COSTHG,SINTHG,XI,VI,NDIME,NDIMI,NM)
!********1*********2*********3*********4*********5*********6*********7**
! INERTV           83/03/28            8303.0    PGMR - TOM MARTIN
!
! FUNCTION:  CONVERT EARTH CENTERED FIXED VELOCITY VECTORS
!            TO INERTIAL CARTESIAN
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   VE       I    A    EARTH CENTERED FIXED VELOCITY VECTORS
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES  OF RIGHT ASCENSION OF GREENWICH
!   XI       I    A    INERTIAL CARTESIAN POSITION VECTORS
!   VI       O    A    INERTIAL CARTESIAN VELOCITY VECTORS
!   NDIME    I    S    DIMENSION OF VE (SAME AS NM)
!   NDIMI    I    S    DIMENSION OF XI (SAME AS NM)
!   NM       I    S    NUMBER OF VECTORS TO BE CONVERTED
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CTHDOT/THDOT
      DIMENSION VE(NDIME,3),COSTHG(NM),SINTHG(NM),XI(NDIMI,3),          &
     &          VI(NDIMI,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      CALL INERTP(VE,COSTHG,SINTHG,VI,NDIME,NDIMI,NM)
      DO 1000 N=1,NM
      VI(N,1)= VI(N,1)-XI(N,2)*THDOT
      VI(N,2)= VI(N,2)+XI(N,1)*THDOT
 1000 END DO
      RETURN
      END
