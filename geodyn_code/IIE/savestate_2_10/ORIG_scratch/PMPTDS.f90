!$PMPTDS
      SUBROUTINE PMPTDS(PMPA,NDIM1,NDIM2,NDXS,KLKS,OBSTIM,DTIME,PRMLBL, &
     &   LNPNM,OBSTRL)
!********1*********2*********3*********4*********5*********6*********7**
! PMPTDS           00/00/00            9403.00   PGMR - SBL
!
! FUNCTION: COMPUTE THE TDRSS 1-WAY LONG LINK SCALE BIAS QUADRATIC PARTI
!           AND LOAD APPROPRIATE LOCATIONS IN THE A-MATRIX.  This is the
!           bias of the open-loop TDRSS to take care of s/c oscillator
!           frequency uncertainty.
!
! NOTE:     This routine utilizes all information from satellite pmpclk
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX = PARTIAL DERIVATIVES OF MEASUREMENTS
!                      W.R.T. ADJUSTED PARAMETERS.
!   NDIM1    I    S    FIRST  DIMENSION OF PMPA.
!   NDIM2    I    S    SECOND DIMENSION OF PMPA.
!   NDXS     I    A    INDICES INDICATING TO WHICH STATION THE CLOCK
!                      PARAMETERS BELONG AND ALSO THE ALGEBRAIC SIGN.
!   KLKS     I    A    INDICES IN PMPA OF THE LOCATIONS OCCUPIED BY THE
!                      CLOCK POLYNOMIAL PARTIALS.
!   OBSTIM   I    A    OBSERVATION TIME IN ELAPSED SECONDS FROM MJDSBL
!                      (/CBLOKI/).
!   DTIME    I    A    AVERAGING INTERVAL FOR AVERAGE RANGE RATE DATA.
!   PRMLBL   I    A    PARAMETER LABELS. FOR BIASES, PRMLBL(2,*)
!                      CONTAINS THE UTC START TIME IN MJDS FORM FOR
!                      THE BIAS APPLICATION INTERVAL.
!   LNPNM    I    S    .TRUE.  IF NDIM1 .EQ. NUMBER OF PARAMETERS.
!                      .FALSE. IF NDIM1 .EQ. NUMBER OF MEASUREMENTS.
!   OBSTRL   I    A    TDRSS 1-WAY LONG LINK RANGE DIFFERENCE
!                      NOT YET DIVIDED BY DOPPLER COUNTING INTERVAL
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
!
      DIMENSION OBSTIM(NM),DTIME(NM),PRMLBL(3,1),OBSTRL(NM)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION NDXS  (2,35),KLKS  (4,2)
!
      DATA ONE/1.0D0/,TEN10/1.0D+10/,C3600/3.6D3/
      DATA METRIX/37/
      DATA LFIRST/.TRUE./
!     DATA HALF/0.50D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! COMPUTE INDEX INTO NDXS ARRAY
!
      INDEXT=(MTYPE-31)/2+1
      J1    =MOD(MTYPE-1,2)+1
! LOOP OVER EACH POLYNOMIAL
      DO 13000 I=1,2
      MSIGNB=NDXS  (I,INDEXT)
      IF(MSIGNB.EQ.0) GO TO 13000
      INDEXB=KLKS  (1,I)
      IF(INDEXB.LE.0) GO TO 4000
! LOAD PARTIALS FOR CONSTANT TERM
      IF(LNPNM) GO TO 2000
      DO 1000 N=1,NM
!
! Negative sign on OBSTRL is to take into account range diff. is
! range at t2 - range at t1.  This is same as negative sign in
! AVGRR.  ROBSUM does things t1 - t2, and OBSTRL is not passed through
! AVGRR so it has to be divided by -DTIME.
!
      PMPA  (N,INDEXB)=-ONE*((-OBSTRL(N)/DTIME(N))-VLIGHT)
 1000 END DO
      GO TO 4000
 2000 CONTINUE
      DO 3000 N=1,NM
       PMPA  (INDEXB,N)=-ONE*((-OBSTRL(N)/DTIME(N))-VLIGHT)
 3000 END DO
 4000 CONTINUE
! LOOP OVER HIGHER ORDER TERMS
      DO 12000 J=2,4
      K     =J     -1
      INDEXB=KLKS  (J,I)
      IF(INDEXB.LE.0) GO TO 12000
      T0    = MOD(PRMLBL(2,INDEXB),TEN10)
      DT0   =MJDSBL-T0
! LOAD PARTIALS FOR HIGHER ORDER TERMS
      IF(LNPNM) GO TO 10000
      DO 9000 N=1,NM
      DT    =OBSTIM(N)+DT0
      PMPA  (N,INDEXB)=(-ONE*((-OBSTRL(N)/DTIME(N))-VLIGHT))*(DT**K)
 9000 END DO
      GO TO 12000
10000 CONTINUE
      DO 11000 N=1,NM
      DT    =OBSTIM(N)+DT0
      PMPA  (INDEXB,N)=(-ONE*((-OBSTRL(N)/DTIME(N))-VLIGHT))*(DT**K)
11000 END DO
12000 END DO
13000 END DO
      RETURN
      END
