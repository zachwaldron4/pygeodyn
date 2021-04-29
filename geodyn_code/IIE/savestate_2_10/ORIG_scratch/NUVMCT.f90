!$NUVMCT
      SUBROUTINE NUVMCT(MJDSEC,FSEC,DPSI,EPST,EPSM,NM,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7
! NUVMCT           08/26/92            0000.0    PGMR - S.LUO
!
! FUNCTION:  CALL CNMARS FOR GETTING MARS NUTATION
!
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    EPHEMERIS SECONDS SINCE GEODYN REFERENCE TIME
!   FSEC     I    A    EPHEMERIS SECONDS SINCE MJDSC
!   DPSI     O    A    MARS NUTATION IN LONGITUDE
!   EPSI     O    A    MARS NUTATION IN OBLIQUITY
!   EPST     O    A    MARS TRUE OBLIQUITY
!   EPSM     I    S    MARS MEAN OBLIQUITY
!   NM       I    S    NUMBER OF OBSERVATION
!   SCRTCH        A    SCRTCH 5*NM IN LENGTH
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
          IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL (L)
      SAVE
!
      DIMENSION FSEC(NM),DPSI(NM),EPST(NM),EPSM(NM),SCRTCH(NM,5)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!
      CALL CNMARS(MJDSEC,FSEC,DPSI,EPST,EPSM,NM,SCRTCH)
!
      RETURN
      END
