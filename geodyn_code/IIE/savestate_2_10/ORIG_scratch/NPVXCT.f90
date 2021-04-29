!$NPVXCT
      SUBROUTINE NPVXCT(MJDSC,FSEC,DUM1,DUM2,DUM3,RAP,DUM4,DCP,NM,      &
     &                  SCRTCH,ROTMAT,DUM5,NMA,LDUM)
!********1*********2*********3*********4*********5*********6*********7**
! NPVXCT           86/12/24            8701.0    PGMR - D. ROWLANDS
!
! FUNCTION:   CALL SUBROUTINE DXQN WITH A VECTOR OF TIMES.
!             AT PRESENT,DXQN CALCULATES EACH ELEMENT OF THE
!             PRECESSION-(NUTATION) MATRIX IN A STRAITFORWARD
!             WAY. THEREFORE, IT IS UNNECESSARY FOR NPVXCT TO
!             BREAK UP THE INPUT VECTOR OF TIMES INTO 12 HR
!             PACKAGES. NPVXCT EXITS SIMPLY TO PARAELLEL NPVECT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSC    I    S    NUMBER OF SECONDS SINCE XXX 243000.5.
!                      ALL TIMES IN FSEC ARE TIMES SINCE MJDSC
!   FSEC     I    A    TIME SINCE MJDSC
!   DUM1
!   DUM2
!   DUM3
!   RAP      I    A    PRECESSION IN RIGHT ASCENSION
!   DUM4
!   DCP      I    A    PRECESSION IN DECLINATION
!   NM       I    S    NUMBER OF ROTMAT AND EQUATION OF
!                      EQUINOX REQUESTED.
!   SCRTCH        A    4 TIMES NM
!   ROTMAT   O    A    NP ROTATION MATRIX, MEAN OF 50 TO TRUE
!                      OF FSEC.
!   DUM5
!   NMA      I    S    ACTUAL FIRST DIMENSION OF ROTMAT
!   LDUM
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION FSEC(NM),RAP(NM),DCP(NM),SCRTCH(NM,5),ROTMAT(NMA,9)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! CALL DXQN
      CALL DXQN(MJDSC,FSEC,RAP,DCP,ROTMAT(1,1),ROTMAT(1,2),ROTMAT(1,3), &
     &   ROTMAT(1,4),ROTMAT(1,5),ROTMAT(1,6),ROTMAT(1,7),ROTMAT(1,8),   &
     &   ROTMAT(1,9),SCRTCH,NM)
      RETURN
      END
