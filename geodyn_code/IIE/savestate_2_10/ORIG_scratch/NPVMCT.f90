!$NPVMCT
      SUBROUTINE NPVMCT(MJDSEC,FSEC,DPSI,EPST,EPSM,PDPSI,PEPST,PEPSM,   &
     &              NM,SCRTCH,ROTMAT,EQN,NMA,LONLEQ)
!********1*********2*********3*********4*********5*********6*********7**
! NPVMCT           93/03/31            0000.0    PGMR - S. LUO
!
!  FUNCTION:  CALL SUBROUTINE DMQN WITH A VECTOR OF TIMES
!             SELECTED FROM THE VECTOR FSEC.
!
!
! I/O PARAMATERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSC    I    S    NUMBER OF SECONDS SINCE XXX 243000.5.
!                      ALL TIMES IN FSEC ARE TIMES SINCE MJDSC
!   FSEC     I    A    TIME SINCE MJDSC
!   DPSI     I    A    NUTATION IN LONGITUDE
!   EPST     I    A    TRUE OBLIQUITY AT T
!   PDPSI    I    A    MARS PRECESSION ELEMENT IN LONGITUDE
!   PEPSM    I    A    MEAN OBLIQUITY AT J2000
!   DEPST    I    A    MEAN OBLIQUITY AT T
!   NM       I    S    NUMBER OF ROTMAT AND EQUATION OF
!                      EQUINOX REQUESTED.
!   SCRTCH        A    5 TIMES NM
!   ROTMAT   O    A    MARS NP ROTATION MATRIX, MEAN OF J2000 TO TRUE
!                      OF FSEC.
!   EQN      O    A    EQUATION OF EQUINOX AT FSEC
!   NMA      I    S    ACTUAL FIRST DIMENSION OF ROTMAT
!   LONLEQ   I         .TRUE. IF EQUATION OF EQUINOX REQUEST
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION FSEC(NM),EPST(NM),PDPSI(NM),PEPST(NM),PEPSM(NM),        &
     & EPSM(NM),SCRTCH(NM,5),ROTMAT(NMA,9),EQN(NM),DPSI(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
   15 CONTINUE
!
      CALL DMQN(MJDSEC,FSEC,PDPSI,PEPSM,PEPST,                          &
     &   EPST,DPSI,ROTMAT(1,1),                                         &
     &   ROTMAT(1,2),ROTMAT(1,3),ROTMAT(1,4),ROTMAT(1,5),               &
     &   ROTMAT(1,6),ROTMAT(1,7),ROTMAT(1,8),ROTMAT(1,9),               &
     &   EQN,SCRTCH,NM,LONLEQ)
!
      RETURN
      END
