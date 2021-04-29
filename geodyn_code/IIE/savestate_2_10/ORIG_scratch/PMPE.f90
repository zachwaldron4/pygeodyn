!$PMPE
      SUBROUTINE PMPE(XPARTL,PMPA,NDIM1,NDIM2,N,INDEX1,INDEX2,LNPNM)
!********1*********2*********3*********4*********5*********6*********7**
! PMPE             00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  EXTRACTS ALL PARTIAL DERIVATIVES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XPARTL   O    A
!   PMPA    I/O   A    A-MATRIX = PARTIAL DERIVATIVES OF MEASUREMENTS
!                      WRT ADJUSTED PARAMETERS
!   NDIM1    I    S    FIRST DIMENSIONS OF PMPA
!   NDIM2    I    S    SECOND DIMENSIONS OF PMPA
!   N        I    S    ROW OF THE SPECIFIC PARTIAL DERIVATIVES
!   INDEX1   I    S    COLUMN OF THE SPECIFIC PARTIAL DERIVATIVES
!   INDEX2   I    S    LAST COLUMN OF THE SPECIFIC PARTIAL DERIVATIVES
!   LNPNM    I    S    .FALSE. FOR TRANSPOSED PMPA
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PMPA(NDIM1,NDIM2),XPARTL(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      I=0
      IF(LNPNM) GO TO 5000
      DO 1000 INDEXP=INDEX1,INDEX2
      I=I+1
      XPARTL(I)=PMPA(N,INDEXP)
 1000 END DO
      RETURN
 5000 CONTINUE
      DO 6000 INDEXP=INDEX1,INDEX2
      I=I+1
      XPARTL(I)=PMPA(INDEXP,N)
 6000 END DO
      RETURN
      END
