!$PRECSS
      SUBROUTINE PRECSS(MJDSC,FSEC,EF,ZF,TF,SCRTCH,NM)
!********1*********2*********3*********4*********5*********6*********7**
! PRECSS           83/04/12            8303.0    PGMR - D. ROWLANDS
!
! FUNCTION: OBTAIN ELEMENTS OF PRECESSION SINCE 1950.0 (:EF,ZF,TF IN
!           RADIANS)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSC    I    S    INTEGER ET SECONDS SINCE GEODYN REFERENCE TIME
!   FSEC     I    A    REMAINING SECONDS SINCE MJDSC
!   EF       O    A    EPSILON ELEMENT OF PRECESSION
!   ZF       O    A    ZETA ELEMENT OF PRECESSION
!   TF       O    A    THETA ELEMENT OF PRECESSION
!   SCRTCH        A    MUST BE AN ARRAY OF LENGTH NM
!   NM       I    S    # TIMES AT WHICH ELEMENTS ARE REQUESTED
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION FSEC(NM),SCRTCH(NM),EF(NM),ZF(NM),TF(NM)
      COMMON/PRECON/PRECON(3,3)
      COMMON/EOPRAT/ETAG,THTAG,ZTAG,EPSMG,DPSIG,EPSTG,TETAG,XPOLE,   &
     &     YPOLE,ETAR,THTAR,ZTAR,EPSMR,DPSIR,EPSTR,THETAGR,XPOLER,   &
     &     YPOLER,SP2,DSP2
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      OFFSET=MJDSC-FSCINP(1)
      DO 100 I=1,NM
      SCRTCH(I)=FSEC(I)+OFFSET
      EF(I)=PRECON(1,1)+(PRECON(1,3)*SCRTCH(I)+PRECON(1,2))             &
     &*SCRTCH(I)
      ZF(I)=PRECON(2,1)+(PRECON(2,3)*SCRTCH(I)+PRECON(2,2))             &
     &*SCRTCH(I)
      TF(I)=PRECON(3,1)+(PRECON(3,3)*SCRTCH(I)+PRECON(3,2))             &
     &*SCRTCH(I)
      ETAG=EF(I)
      THTAG=TF(I)
      ZTAG=ZF(I)
  100 END DO
      RETURN
      END
