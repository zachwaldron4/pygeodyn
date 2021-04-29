!$ADOOD
      FUNCTION ADOOD(COSEPH,SINEPH,NN,MM,KK,HH,JJ,IBDY,SIGN1,FI,GE)
!***********************************************************************
! VERSION 8308  DATE 830817      PGMMR (1)DEMOS CHRISTODOULIDIS
!                                      (2)D. ROWLANDS (MOD FOR GII)
!                                      (3)A. MARSHALL
!
! FUNCTION - ADOOD CALCULATES DOODSON'S COEFFICIENTS USED IN THE
!            TIDAL EXPANSION MODEL FOR SOLID EARTH TIDES (SEE
!            GEODYN DOCUMENTATION VOLUME 1).
!
!            THIS PART OF CODE ASSUMES DEGREE=2.
!
!            THIS WAS NOT THE CASE WITH DEMOS' ORGINAL
!            ALTHOUGH THE EARTH TIDES WERE LIMITED TO DEGREE 2
!
!  INPUT PARAMETERS
!
!             NN - TIDAL DEGREE IN HARMONIC EXPANSION
!             MM - TIDAL EXPANSION ARGUMENT
!             KK - TIDAL EXPANSION ARGUMENT
!             HH - TIDAL EXPANSION ARGUMENT
!             JJ - TIDAL EXPANSION ARGUMENT
!          SIGN1 - SIGN ASSOCIATED WITH EXPANSION ARGUMENTS
!           IBDY - PERTURBING BODY
!             FI - KAULA'S INCLINATION FUNCTION
!             GE - KAULA'S ECCENTICITY FUNCTION
!
! REFERENCES:
!            "OBSERVED TIDAL BRAKING IN THE EARTH/MOON/SUN SYSTEM"
!             BY CHRISTODOULIDIS, ET AL., JGR, VOL 93, P. 6216-6236,
!             JUNE 10,1988.
! **********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER( ZERO  = 0.0D0 )
      PARAMETER( ONE   = 1.0D0 )
      PARAMETER( TWO   = 2.0D0 )
      PARAMETER( THREE = 3.0D0 )
      PARAMETER( FOUR  = 4.0D0 )
!
      INTEGER SIGN1,HH
!
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
!
      DIMENSION FI(2),GE(2),SFCRAT(3),CDBLU(3,3),XJPSI(5,3),SPSI(3),    &
     &          DCOEF(5,3),XDERIV(5,5)
!
      DATA SFCRAT/-.1333333333333333D1,+.6666666666666667D0,            &
     &            +.3333333333333333D0/
      DATA CDBLU/+.5D0,-1.D0,-1.D0,-.5D0,+1.D0,+1.D0,-.5D0,+1.D0,+1.D0/
      DATA XJPSI/12.D0,3.D0,.5D0,.8333333333333333D-1,                  &
     &          .2083333333333333D-1,                                   &
     &           24.D0,6.D0,1.D0,.1666666666666667D0,                   &
     &          .4166666666666667D-1,                                   &
     &           24.D0,6.D0,1.D0,.1666666666666667D0,                   &
     &          .4166666666666667D-1/
      DATA SPSI/1.D0,-.1666666666666667D0,.4166666666666667D-1/
      DATA DCOEF/1.D0,-2.D0,1.D0, 0.D0,0.D0,                            &
     &           1.D0,-3.D0,3.D0,-1.D0,0.D0,                            &
     &           1.D0,-4.D0,6.D0,-4.D0,1.D0/
      DATA XDERIV/1.D0,1.D0,1.D0,1.D0,1.D0,                             &
     &            0.D0,1.D0,2.D0,3.D0,4.D0,                             &
     &            0.D0,0.D0,2.D0,6.D0,12.D0,                            &
     &            0.D0,0.D0,0.D0,6.D0,24.D0,                            &
     &            0.D0,0.D0,0.D0,0.D0,24.D0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      IDM=MM+1
!
!     WRITE(6,*) 'ADOOD: ENTRY  COSEPH,SINEPH,NN,MM,KK ',
!    1                          COSEPH,SINEPH,NN,MM,KK
!     WRITE(6,*) 'ADOOD: ENTRY  HH,JJ,IBDY,SIGN1 ',
!    1                          HH,JJ,IBDY,SIGN1
!     WRITE(6,*) 'ADOOD: ENTRY  FI,GE ',
!    1                          FI,GE
!
! IF TIDE IS GREATER THAN DEGREE TWO, USE THE MORE INEFFICIENT
! GEODYN 1 METHOD OF EVALUATION
!
      IF (NN.GT.2) GOTO 2000
!
! START DBLU CALCULATION
      IDK=KK+1
      IR=KK*SIGN1
!
!    START PSI CALCULATION
      MPR=MM+IR
      IRLM=IR-MM
      ISK=IR+3
      PSI=(COSEPH**MPR)*(SINEPH**IRLM)
      PSI=PSI*XJPSI(ISK,IDM)
      T=COSEPH*COSEPH
!
!       START F2NLR COMPUTATION
      IORDER=2+IR
      I3=3+MM
      XF2NLR=ZERO
      IDEG=5
      DO 220 I=1,I3
      IDEG=IDEG-1
!
!          START DERIV COMPUTATION
      DERIV=ZERO
      IF(IORDER.GT.IDEG) GO TO 200
      IEXP=IDEG-IORDER
      DERIV=XDERIV(IDEG+1,IORDER+1)*(T**IEXP)
  200 CONTINUE
!          END   DERIV COMPUTATION
!
      XF2NLR=XF2NLR+DCOEF(I,IDM)*DERIV
  220 END DO
!       END F2NLR COMPUTATION
!
      PSI=PSI*XF2NLR
      IF(SIGN1.LT.0) PSI=PSI*SPSI(IDK)
!    END PSI CALCULATION
!
      W=CDBLU(IDK,IDM)*PSI
! END   DBLU CALCULATION
!
!
!
      INDEX=ABS(JJ)+KK
      IF(HH.EQ.1.AND.INDEX.EQ.0) GO TO 1000
      SUM=B(IBDY)*FI(IBDY)*GE(IBDY)
      GO TO 1100
 1000 CONTINUE
      W=TWO*W
      SUM=B(1)*FI(1)*GE(1)+B(2)*FI(2)*GE(2)
 1100 CONTINUE
!
!     WRITE(6,*) 'ADOOD: AFTER 1100  '
!
      ADOOD=W*SUM*SFCRAT(IDM)
      GOTO 3000
!
 2000 CONTINUE
!
!     WRITE(6,*) 'ADOOD: AFTER 2000  '
!
! THIS CALL FOR DEGREE >2 IS INEFFICIENT.  IN THE FUTURE, THIS SHOULD
! BE REPLACED STORAGE ARRAYS WHICH ARE CALCULATED ONCE PER RUN AND
! ACCESSED HERE
!
      DM=0.0D0
      IF(MM.EQ.0) DM=1.0D0
      INDEX=ABS(JJ)+KK
      MM1=2-MM
      MM2=2+MM
      S=2.0D0*MM-4.0D0*DM/3.0D0
      W=DBLU(2,MM,KK,SIGN1,EPSLON)
      IF(HH.EQ.1.AND.INDEX.EQ.0) GO TO 2100
      SUM=B(IBDY)*FI(IBDY)*GE(IBDY)
      GO TO 2200
 2100 CONTINUE
      W=2.0D0*W
      SUM=B(1)*FI(1)*GE(1)+B(2)*FI(2)*GE(2)
 2200 CONTINUE
      ADOOD=W*(2.0D0-DM)*S*FACT(MM1)*SUM/FACT(MM2)
 3000 CONTINUE
      RETURN
      END
