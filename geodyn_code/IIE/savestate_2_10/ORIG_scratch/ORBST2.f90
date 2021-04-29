!$ORBST2
      SUBROUTINE ORBST2(NSAT,IVSAT,LSETS,ISAT,ISETO,LSTOP)
!********1*********2*********3*********4*********5*********6*********7**
! ORBST2           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  DETERMINES WHICH SPACECRAFT PARAMETERS ARE REQUIRED TO BE
!            INTERPOLATED
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NSAT     I    A    ID OF SATELLITE TO BE CHECKED FOR INTERPOLATION
!   IVSAT    O    A    ARRAY FILLED WITH SATELLITES TO BE INTERPOLATED
!   LSETS    O    A    LOGICAL FLAGS FOR THE SATELLITES TO BE
!                      INTERPOLATED
!   ISAT     I    S    INTERNAL SATELLITE INDEX
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      DIMENSION NSAT(NSETA),IVSAT(NSATA),LSETS(NSETA)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IVSAT(1)=ISAT
      JSAT=0
      ISETO=-99
      LSTOP=.FALSE.
      DO 1000 ISET=1,NSETA
      JSAT0=JSAT
      JSAT=JSAT+NSAT(ISET)
      LSETS(ISET)=(ISAT.GT.JSAT0).AND.(ISAT.LE.JSAT)
      IF(LSETS(ISET)) ISETO=ISET
 1000 END DO
      IF(ISETO.LT.0) THEN
         WRITE(6,45678)
         WRITE(6,45679)
         STOP
      ENDIF
      IF(NSAT(ISETO).GT.1) LSTOP=.TRUE.
      RETURN
45678 FORMAT(' EXECUTION TERMINATING.')
45679 FORMAT(' NO SATELLITE MATCH FOUND IN OUTST2')
      END
