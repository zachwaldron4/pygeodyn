!$PLITEM
      SUBROUTINE PLITEM(PMPA  ,OBS   ,NM    ,INVLIT)
!********1*********2*********3*********4*********5*********6*********7**
! PLITEM           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  COMPUTES SPEED OF LIGHT PARTIALS WHEN PARTIALS ARE ORDERED
!            BY MEASUREMENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX - PARTIAL DERIVATIVE OF MEASUREMENTS
!                      WRT ADJUSTED PARAMETERS
!   OBS      I    A    OBSERVATION VALUES
!   NM       I    S    NUMBER OF OBSERVATIONS IN THE BLOCK
!   INVLIT   I    S    POINTER TO THE SPEED OF LIGHT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      DIMENSION PMPA(NM,NADJST),OBS(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
       DO 1000 N=1,NM
       PMPA(N,INVLIT)=-OBS(N)/VLIGHT
 1000  CONTINUE
      RETURN
      END
