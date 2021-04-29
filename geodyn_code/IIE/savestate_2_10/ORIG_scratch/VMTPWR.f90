!$VMTPWR
      SUBROUTINE VMTPWR(WORD,NEQN,PXPFNM,PXPFNP,PVPFNP,NPTS,IPT)
!********1*********2*********3*********4*********5*********6*********7**
! VMTPWR           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   WORD
!   NEQN
!   PXPFNM
!   PXPFNP
!   PVPFNP
!   NPTS
!   IPT
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CESTML/LNPNM ,NXCSTL
!     COMMON/UNITS/
      DIMENSION WORD(12),PXPFNM(NPTS,NEQN,3,2),PXPFNP(NEQN,3)
      DIMENSION PVPFNP(NEQN,3)
      DATA IUNT80/80/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(NPTS.GT.1) GO TO 100
      WRITE(IUNT80) WORD,PXPFNM
      RETURN
  100 CONTINUE
      IF(.NOT.LNPNM) GO TO 200
      WRITE(IUNT80) WORD,PXPFNP,PVPFNP
      RETURN
  200 CONTINUE
      WRITE(IUNT80) WORD,(PXPFNM(IPT,N,1,1),N=1,NEQN),                  &
     &  (PXPFNM(IPT,N,2,1),N=1,NEQN),(PXPFNM(IPT,N,3,1),N=1,NEQN),      &
     &  (PXPFNM(IPT,N,1,2),N=1,NEQN),(PXPFNM(IPT,N,2,2),N=1,NEQN),      &
     &  (PXPFNM(IPT,N,3,2),N=1,NEQN)
      RETURN
      END
