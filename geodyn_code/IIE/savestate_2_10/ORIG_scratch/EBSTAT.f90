!$EBSTAT
      SUBROUTINE EBSTAT(TMEAN ,TRMS  ,WTMEAN,WTRMS ,TYMEAN,TYPRMS,      &
     &                  WTMTYP,WTYRMS,STATEB,BDELTA,                    &
     &                  ATWA  ,ATWD  ,BTWB  ,BTWD  ,ETWE  ,ETWD  ,      &
     &                  INDEXX,NINDEX,NTYPE ,ISTATS)
!********1*********2*********3*********4*********5*********6*********7**
! EBSTAT            90/03/00            90??.0    PGMR - W. EDDY
!                   90/03/00            90??.0    PGMR - J. MCCARTHY
!
! FUNCTION:
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   TMEAN   I/O   A    RESIDUAL MEANS GROUPED BY STATIONS
!   TRMS    I/O   A    RESIDUAL RMS   GROUPED BY STATIONS
!   WTMEAN  I/O   A    RESIDUAL WEIGHTED MEANS GROUPED BY STATIONS
!   WTRMS   I/O   A    RESIDUAL WEIGHTED RMS   GROUPED BY STATIONS
!   TYMEAN  I/O   A    RESIDUAL MEANS GROUPED BY MEASUREMENT TYPES
!   TYPRMS  I/O   A    RESIDUAL RMS   GROUPED BY MEASUREMENT TYPES
!   WTMTYP  I/O   A    RESIDUAL WEIGHTED MEANS GROUPED BY MEAS. TYPES
!   WTYRMS  I/O   A    RESIDUAL WEIGHTED RMS   GROUPED BY MEAS. TYPES
!   STATEB
!   BDELTA
!   ATWA
!   ATWD
!   BTWB          A    NORMAL MATRIX OF E-BIAS PARAMETERS
!   BTWD          A    Right Hand Side of E-bias solution
!   ETWE
!   ETWD
!   INDEXX
!   NINDEX
!   NTYPE
!   ISTATS   I    S    NUMBER OF UNIQUE TRACKING/MTYPE CONFIGURATIONS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO = 0.0D0 )
!
      COMMON/CEBGLB/MEPARM,MEDIM ,MEBIAS,MBIASE,NEBGRP,IEBGRP(4),NXCEBG
      COMMON/CSTATA/RMSTOT,RMSEDT,RMSGLB
      COMMON/CSTATS/MSTATS,MTYPES,NXCSTT
!
      DIMENSION TMEAN (MSTATS),TRMS  (MSTATS)
      DIMENSION WTMEAN(MSTATS),WTRMS (MSTATS)
      DIMENSION TYMEAN(MTYPES),TYPRMS(MTYPES)
      DIMENSION WTMTYP(MTYPES),WTYRMS(MTYPES)
      DIMENSION STATEB(MEPARM,2),                                       &
     &          ATWA  (MEDIM)   ,ATWD  (MEPARM),                        &
     &          BTWB  (MEDIM)   ,BTWD  (MEPARM),                        &
     &          ETWE  (MEDIM)   ,ETWD  (MEPARM),                        &
     &          BDELTA(NINDEX)  ,INDEXX(NINDEX)
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! CORRECT SUMMATIONS FOR MEAN
!
      DO 1000 I=1,NINDEX
      J=INDEXX(I)
      DELTA =BDELTA(I)
      DELTA1=DELTA *STATEB(J,1)
      STATEB(J,1)=ZERO
      TMEAN (ISTATS)=TMEAN (ISTATS)-DELTA1
      TYMEAN(NTYPE )=TYMEAN(NTYPE )-DELTA1
      DELTA2=DELTA *STATEB(J,2)
      STATEB(J,2)=ZERO
      WTMEAN(ISTATS)=WTMEAN(ISTATS)-DELTA2
      WTMTYP(NTYPE )=WTMTYP(NTYPE )-DELTA2
 1000 END DO
!
! CORRECT SUMMATIONS FOR RMS
!
      CALL EXPECT(ATWA,ATWD,BDELTA,MEDIM,MEPARM,NINDEX,DELRMS)
      TRMS  (ISTATS)=TRMS  (ISTATS)+DELRMS
      TYPRMS(NTYPE )=TYPRMS(NTYPE )+DELRMS
      CALL EXPECT(BTWB,BTWD,BDELTA,MEDIM,MEPARM,NINDEX,DELRMS)
      WTRMS (ISTATS)=WTRMS (ISTATS)+DELRMS
      WTYRMS(NTYPE )=WTYRMS(NTYPE )+DELRMS
      CALL EXPECT(ETWE,ETWD,BDELTA,MEDIM,MEPARM,NINDEX,DELRMS)
      RMSEDT        =RMSEDT        +DELRMS
      RETURN
      END
