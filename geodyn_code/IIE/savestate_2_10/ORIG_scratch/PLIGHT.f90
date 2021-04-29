!$PLIGHT
      SUBROUTINE PLIGHT(PMPA,NDIM1,NDIM2,OBS ,NM  ,INVLIT,LNPNM)
!********1*********2*********3*********4*********5*********6*********7**
! PLIGHT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  COMPUTES SPEED OF LIGHT PARTIALS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX - PARTIAL DERIVATIVES OF MEASUREMENTS
!                      WRT ADJUSTED PARAMETERS
!   NDIM1   I     S    FIRST DIMENSION OF PMPA ARRAY
!   NDIM2   I     S    SECOND DIMENSION OF PMPA ARRAY
!   OBS     I     A    OBSERVATION VALUES
!   NM      I     S    NUMBER OF OBSERVATIONS IN THE BLOCK
!   INVLIT  I     S    POINTER TO THE SPEED OF LIGHT
!   LNPNM   I     S    .TRUE. IF THE FIRST DIMENSION IS THE NUMBER OF
!                      MEASUREMENTS IN THE BLOCK
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      DIMENSION PMPA(NDIM1,NDIM2),OBS(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(LNPNM) GO TO 2000
       DO 1000 N=1,NM
       PMPA(N,INVLIT)=-OBS(N)/VLIGHT
 1000  CONTINUE
      RETURN
 2000 CONTINUE
      DO 3000 N=1,NM
      PMPA(INVLIT,N)=-OBS(N)/VLIGHT
 3000 END DO
      RETURN
      END
