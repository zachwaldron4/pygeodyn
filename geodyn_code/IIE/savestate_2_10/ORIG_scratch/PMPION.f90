!$PMPION
      SUBROUTINE PMPION(PMPA,NDIM1,NDIM2,INDEXB,NM,XION,LNPNM)
!********1*********2*********3*********4*********5*********6*********7**
! PMPION           00/00/00            8804.00   PGMR - TVM
!
! FUNCTION: COMPUTE THE IONOSPHERIC REFRACTION SCALE BIAS PARTIALS AND
!           LOAD INTO THE APPROPRIATE LOCATIONS IN THE A-MATRIX.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX = PARTIAL DERIVATIVES OF MEASUREMENTS
!                      W.R.T. ADJUSTED PARAMETERS.
!   NDIM1    I    S    FIRST  DIMENSION OF PMPA.
!   NDIM2    I    S    SECOND DIMENSION OF PMPA.
!   INDEXB   I    S    INDEX INTO THE A-MATRIX FOR THE IONO PARTIALS.
!   NM       I    S    NUMBER OF MEASUREMENTS IN THIS OBSERVATION BLOCK.
!   XION     I    A    IONOSPHERIC CORRECTION.
!   LNPNM    I    S    .TRUE.  IF NDIM1 .EQ. NUMBER OF PARAMETERS.
!                      .FALSE. IF NDIM1 .EQ. NUMBER OF MEASUREMENTS.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PMPA(NDIM1,NDIM2),XION(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(LNPNM) GO TO 5000
! PARTIAL DERVIATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 1000 N=1,NM
      PMPA(N,INDEXB)=XION(N)
 1000 END DO
      RETURN
! PARTIAL DERVIATIVE ARRAY DIMENSIONED (NADJST,NM)
 5000 CONTINUE
      DO 6000 N=1,NM
      PMPA(INDEXB,N)=XION(N)
 6000 END DO
      RETURN
      END
