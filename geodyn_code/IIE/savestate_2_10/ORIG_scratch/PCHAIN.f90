!$PCHAIN
      SUBROUTINE PCHAIN(PMPX,NM,I3,IPV,PXPF,NEQN,N3,IND,IL,NPVECT,PMPF)
!********1*********2*********3*********4*********5*********6*********7**
! PCHAIN           08/27/82            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  CHAIN THE PARTIALS OF THE MEASUREMENTS W.R.T. THE
!            S/C POS-VEL WITH THE PARTIALS OF THE S/C POS-VEL
!            W.R.T. THE ADJUSTED FORCE MODEL PARAMETERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPX     I    A    PARTIALS OF MEASUREMENTS W.R.T. POS-VEL
!   NM       I    S    NUMBER OF MEASUREMENTS
!   I3       I    S    NUMBER OF SATELLITES CHAINED TIMES 3
!   IPV      I    S    =1, IMPLIES POSITION ONLY
!                      =2, IMPLIES VELOCITY ONLY
!   PXPF     I    A    PARTIALS OF POS-VEL W.R.T. ADJUSTED FORCE
!                      MODEL PARAMETERS
!   NEQN     I    S    NUMBER OF FORCE MODEL PARAMETERS
!   N3       I    S    NUMBER OF SATELLITES TIMES 3
!   IND      I    A    STARTING INDICES OF DESTINATIONS
!                      SUBSETS OF CHAINED PARTIALS
!   IL       I    A    LENGTH OF SUBSET
!   NPVECT   I    S    NUMBER OF PARTIAL DERIVATIVE SUBSETS
!   PMPF     O    A    PARTIALS OF MEASUREMENTS W.R.T. ADJUSTED
!                      FORCE MODEL PARAMETERS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      DIMENSION PMPX(NM,I3),PXPF(NM,NEQN,N3,2),                         &
     &   PMPF(NM,NADJST),IND(NPVECT),IL(NPVECT)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!***** DEBUG *****
!     PRINT 12345,NM,I3,IPV,NEQN,N3,NPVECT
!2345 FORMAT(' ** PCHAIN **  NM,I3,IPV,NEQN,N3,NPVECT='/(1X,6I11))
!     PRINT 12346,IND
!2346 FORMAT(' ** PCHAIN **  IND='/(1X,5I12))
!     PRINT 12347,IL
!2347 FORMAT(' ** PCHAIN **  IL='/(1X,5I12))
!     PRINT 12348,PMPX
!2348 FORMAT(' ** PCHAIN **  PMPX='/(1X,5G15.8))
!     PRINT 12349,PXPF
!2349 FORMAT(' ** PCHAIN **  PXPF='/(1X,5G15.8))
!***** END DEBUG *****
! THRU 4000 LOOPS OVER THREE CARTESIAN COMPONENTS
      DO 4000 I=1,I3
! THRU 3000 LOOPS OVER FORCE MODEL PARAMETER SUBSETS
      N=0
      DO 3000 IPVECT=1,NPVECT
      INDS=IND(IPVECT)
      INDF=INDS+IL(IPVECT)-1
! THRU 2000 LOOPS OVER FORCE MODEL PARAMETERS WITHIN SUBSETS
      DO 2000 INDX=INDS,INDF
      N=N+1
! THRU 1000 LOOPS OVER MEASUREMENTS
      DO 1000 M=1,NM
      PMPF(M,INDX)=PMPF(M,INDX)+PMPX(M,I)*PXPF(M,N,I,IPV)
 1000 END DO
 2000 END DO
 3000 END DO
 4000 END DO
!***** DEBUG *****
!     PRINT 22345,PMPF
!2345 FORMAT(' ** PCHAIN **  PMPF='/(1X,5G15.8))
!***** END DEBUG *****
      RETURN
      END
