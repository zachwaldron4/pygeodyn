!$ECFIXP
      SUBROUTINE ECFIXP(XI,COSTHG,SINTHG,XE,NDIMI,NDIME,NM)
!********1*********2*********3*********4*********5*********6*********7**
! ECFIXP           83/03/28            8303.0    PGMR- TOM MARTIN
!
! FUNCTION:  CONVERT INERTIAL CARTESIAN POSITION VECTORS
!            TO EARTH CENTERED FIXED CARTESIAN
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XI       I    A    INERTIAL CARTESIAN POSITION VECTORS
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES  OF RIGHT ASCENSION OF GREENWICH
!   XE       O    A    EARTH CENTERED FIXED POSITION VECTORS
!   NDIMI    I    S    SAME AS NM
!   NDIME    I    S    SAME AS NM
!   NM       I    S    NUMBER OF VECTORS TO BE CONVERTED
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XI(NDIMI,3),COSTHG(NM),SINTHG(NM),XE(NDIME,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      XE(N,1)= XI(N,1)*COSTHG(N)+XI(N,2)*SINTHG(N)
      XE(N,2)=-XI(N,1)*SINTHG(N)+XI(N,2)*COSTHG(N)
      XE(N,3)= XI(N,3)
 1000 END DO
      RETURN
      END
