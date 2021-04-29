!$PMPTRP
      SUBROUTINE PMPTRP(PMPA,NDIM1,NDIM2,INDEXB,NM,DRYT,WETT,MTYPE,    &
     &                       LNPNM)
!********1*********2*********3*********4*********5*********6*********7**
! PMPTRP           00/00/00            8804.00   PGMR - TVM
!
! FUNCTION: COMPUTE THE TROPOSPHERIC REFRACTION SCALE BIAS PARTIALS AND
!           LOAD INTO THE APPROPRIATE LOCATIONS IN THE A-MATRIX.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX = PARTIAL DERIVATIVES OF MEASUREMENTS
!                      W.R.T. ADJUSTED PARAMETERS.
!   NDIM1    I    S    FIRST DIMENSION OF PMPA.
!   NDIM2    I    S    SECOND DIMENSION OF PMPA.
!   INDEXB   I    S    INDEX INTO THE A-MATRIX FOR THE TROPO PARTIALS.
!   NM       I    S    NUMBER OF MEASUREMENTS IN THIS OBSERVATION BLOCK.
!   DRYT     I    A    DRY TROPOSPHERIC CORRECTION.
!   WETT     I    A    WET TROPOSPHERIC CORRECTION.
!   LNPNM    I    S   .TRUE.  IF NDIM1 .EQ. NUMBER OF PARAMETERS.
!                     .FALSE. IF NDIM1 .EQ. NUMBER OF MEASUREMENTS.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      DIMENSION PMPA(NDIM1,NDIM2),DRYT(NM),WETT(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(LNPNM) GO TO 5000
! PARTIAL DERVIATIVE ARRAY DIMENSIONED (NM,NADJST)
      IF(MODESC(MTYPE).EQ.0) THEN
      DO 1000 N=1,NM
      PMPA(N,INDEXB)=DRYT(N)+WETT(N)
 1000 END DO
      ELSE
      DO 1100 N=1,NM
      PMPA(N,INDEXB)=WETT(N)
 1100 END DO
      ENDIF
      RETURN
! PARTIAL DERVIATIVE ARRAY DIMENSIONED (NADJST,NM)
 5000 CONTINUE
      IF(MODESC(MTYPE).EQ.0) THEN
      DO 6000 N=1,NM
      PMPA(INDEXB,N)=DRYT(N)+WETT(N)
 6000 END DO
      ELSE
      DO 6100 N=1,NM
      PMPA(INDEXB,N)=WETT(N)
 6100 END DO
      ENDIF
      RETURN
      END
