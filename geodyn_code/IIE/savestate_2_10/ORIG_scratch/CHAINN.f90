!$CHAINN
      SUBROUTINE CHAINN(PMPX,NM,I3,IPV,PXPF,NEQN,N3,IND,IL,NPVECT,PMPF)
!********1*********2*********3*********4*********5*********6*********7**
! CHAINN           82/08/27            8208.0    PGMR - TOM MARTIN
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
!   IPV      I    S    1, IMPLIES POSITION ONLY
!                      2, IMPLIES VELOCITY ONLY
!   PXPF     I    A    PARTIALS OF POS-VEL W.R.T. ADJUSTED FORCE
!                      MODEL PARAMETERS
!   NEQN     I    S    NUMBER OF FORCE MODEL PARAMETERS
!   N3       I    S    NUMBER OF SATELLITES TIMES 3
!   IND      I    A    STARTING INDICES OF DESTINATIONS
!                      SUBSETS OF CHAINED PARTIALS
!   IL       I    A    LENGTH OF SUBSETS
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
      DIMENSION PMPX(NM,I3),PXPF(NEQN,N3,NM,2),                         &
     &   PMPF(NADJST,NM),IND(NPVECT),IL(NPVECT)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! THRU 4000 LOOPS OVER THREE CARTESIAN COMPONENTS PRESENTLY ONLY ONE
! SAT PER CALL(I3=3)
      DO 4000 I=1,I3
      DO 3000 M=1,NM
      NN=0
      DO 2000 IPVECT=1,NPVECT
      INDS=IND(IPVECT)-1
      ILS=IL(IPVECT)
! THRU 2000 LOOPS OVER FORCE MODEL PARAMETERS WITHIN SUBSETS
! THRU 1000 LOOPS OVER MEASUREMENTS
      DO 1000 N=1,ILS
      PMPF(INDS+N,M)=PMPF(INDS+N,M)-PMPX(M,I)*PXPF(NN+N,I,M,IPV)
 1000 END DO
      NN=NN+ILS
 2000 END DO
 3000 END DO
 4000 END DO
      RETURN
      END
