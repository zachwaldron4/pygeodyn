!$PMNPNM
      SUBROUTINE PMNPNM(PMPA  ,NDIM1 ,NDIM2 ,PARTL ,NM    ,INDEXP)
!********1*********2*********3*********4*********5*********6*********7**
! PMNPNM           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  LOADS ONE PARTIAL DERIVATIVE FOR A BLOCK OF OBSERVATIONS
!            INTO THE OBSERVATION BLOCK PARTIAL DERIVATIVE MATRIX - A
!            MATRIX
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX - PARTIAL DERIVATIVES OF MEASUREMENTS
!                      WRT ADJUSTED PARAMETERS
!   NDIM1    I    S    FIRST DIMENTION OF PMPA
!   NDIM2    I    S    SECOND DIMENTION OF PMPA
!   PARTL    I    A    PARTIAL DERIVATIVES FOR A BLOCK OF OBSERVATION
!   NM       I    S    NUMBER OF OBSERVATIONS IN A BLOCK
!   INDEXP   I    S    COLUMN INDEX FOR PMPA
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      DIMENSION PMPA(NDIM1,NDIM2),PARTL(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 1000 N=1,NM
      PMPA(INDEXP,N)=PARTL(N)
 1000 END DO
      RETURN
      END
