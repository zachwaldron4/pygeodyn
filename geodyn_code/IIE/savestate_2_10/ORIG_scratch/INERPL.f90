!$INERPL
      SUBROUTINE INERPL(XE,COSTHG,SINTHG,XI,NM,NDIMI)
!********1*********2*********3*********4*********5*********6*********7**
! INERPL
!
! FUNCTION:  CONVERT PLANET  CENTERED FIXED POSITION VECTORS
!            TO INERTIAL CARTESIAN
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XE       I    A    EARTH CENTERED FIXED POSITION VECTORS
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES  OF RIGHT ASCENSION OF GREENWICH
!   XI       O    A    INERTIAL CARTESIAN POSITION VECTORS
!   NDIMI    I    S    DIMENSION OF XI (SAME AS NM)
!   NM       I    S    NUMBER OF VECTORS TO BE CONVERTED
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XE(3),COSTHG(NM),SINTHG(NM),XI(NDIMI,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      XI(N,1)= XE(1)*COSTHG(N)-XE(2)*SINTHG(N)
      XI(N,2)= XE(1)*SINTHG(N)+XE(2)*COSTHG(N)
      XI(N,3)= XE(3)
 1000 END DO
      RETURN
      END
