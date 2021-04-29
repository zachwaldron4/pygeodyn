!$PMPB
      FUNCTION PMPB(PMPA,NDIM1,NDIM2,N,INDEXB,LNPNM)
!********1*********2*********3*********4*********5*********6*********7**
! PMPB             00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  EXTRACTS A SINGLE PARTIAL DERIVATIVE FROM THE OBSERVATION
!            BLOCK PARTIAL DERIVATIVE MATRIX (A-MATRIX)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX - PARTIAL DERIVATIVES OF MEASUREMENTS
!                      WRT ADJUSTED PARAMETERS
!   NDIM1    I    S    FIRST DIMENSION OF PMPA
!   NDIM2    I    S    SECOND DIMENSION OF PMPA
!   N        I    S    ROW OF THE SINGLE PARTIAL DERIVATIVE
!   INDEXB   I    S    COLUMN OF THE SINGLE PARTIAL DERIVATIVE
!   LNPNM    I    S    .FALSE. FOR TRANSPOSED PMPA
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PMPA(NDIM1,NDIM2)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(LNPNM) GO TO 1000
      PMPB=PMPA(N,INDEXB)
      RETURN
 1000 CONTINUE
      PMPB=PMPA(INDEXB,N)
      RETURN
      END
