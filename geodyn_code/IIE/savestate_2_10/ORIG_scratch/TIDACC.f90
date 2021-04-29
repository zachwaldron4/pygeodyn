!$TIDACC
      SUBROUTINE TIDACC(AT,BT,R,P,COEF,LL1,XQQ,IPT,XIEXP,XMM,XLL1,      &
     & XSIGN1,XSN2QQ,QQ,X2M2HH,X2M2HJ,XKK,JJBDY,SCRTCH,LSATID,JSGRP,    &
     & NEQN,XSIGN2,AAA,BBB,D,IIPPTT,ACCTRF,GRPAR,AORN,IQNDEX,UNORM)
!********1*********2*********3*********4*********5*********6*********7**
! TIDACC           89/06/01            0000.0    PGMR - BEN ROSEN
!                                                PGMR - D. CHRISTODOULID
!                                                PGMR - D. ROWLANDS (GII
!                                                PGMR - D. MORSBERGER
!                                                PGMR - A. MARSHALL
!
! FUNCTION:  TO COMPUTE THE TIDAL ACCELERATION ON A SATELITE DUE TO
!            THE TIDAL EXPANSION MODEL (SEE VOLUME 1). ALSO COMPUTE
!            THE GRPAR ARRAY TO ALLOW FOR THE ESTIMATION OF THE TID
!            AMPLITUDES AND PHASES.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AT       I         A TIDE COEFFCIENT
!   BT       I         B TIDE COEFFCIENT
!   R        I         SATELLITE RADIUS
!   P        I         LGENDR POLYNONIALS ASSOCIATED WITH SATELLITE
!   COEF     I         TIDAL COEFFICIENT. COMPUTED FROM DOODSON
!                      COEFFICIENT FOR SOLID EARTH TIDES AND FROM
!                      LOADING CONSTANTS FOR OCEAN TIDES.
!   LL1      I         DEGREE OF TIDAL HARMONIC + 1
!                      = 3 FOR SOLID EARTH TIDES
!   XQQ      I         DFLOAT OF ORDER OF TIDAL HARMONIC
!                      = XMM FOR SOLID EARTH TIDES
!   IPT      I         POINTER TO LGENDR POLYNOMIAL OF LL,QQ
!   XIEXP    I         +1 IF XMM EVEN;-1 IF XMM ODD
!   XMM      I         DFLOAT OF EXPANSION ARGUMENT MM
!   XLL1     I         DFLOAT OF LL+1
!   XSIGN1   I         SIGN ASSOCIATED WITH EXPANSION ARGUMENTS
!                      COLUMNS 16-17 OF THE OTIDES/ETIDES CARD
!                      MULTIPLIES -(LINEAR COMB NODE, PER, MA)
!   XSN2QQ   I         SIGN ASSOCIATED WITH HARMONIC TERM TIMES QQ
!                      = + FOR SOLID EARTH TIDES
!   QQ       I         ORDER OF TIDAL HARMONIC
!                      = MM FOR SOLID EARTH TIDES
!   X2M2HH   I         2-2HH WHERE HH IS A TIDAL EXPAMSION TERM
!   X2M2HJ   I         X2M2HH+JJ WHERE JJ IS AN EXPANSION ARGUMENT
!   XKK      I         DFLOAT OF EXPANSION ARGUMENT KK
!   JJBDY    I         PERTURBING BODIES
!                      1 - MOON AND SUN OR JUST MOON
!                      2 - SUN
!                      NOTE THAT IF MOON& SUN,THEN X2M2HH,X2M2HJ,XKK
!                      HAVE ALL BEEN SET TO ZERO
!   SCRTCH   I         SCRATCH SPACE TO FORM ACCELERATIONS IN RFL
!   LSATID   I         TRUE IF FORMINGING PARTIALS ON THIS CALL
!   JSGPR    I         POINTER TO FIRST LOCATION IN PARTIAL ARRAY
!                      FOR TIDES
!   NEQN     I         NUMBER OF FORCE MODEL EQUATIONS
!   XSIGN2   I         DFLOAT OF SIGN OF HARMONIC EXPANSION
!                      PROGRADE RETROGRADE (COLUMN 7-8 OF OTIDES)
!                      = +1 FOR SOLID EARTH TIDES
!   AAA      I         SIDE BANDS AMPLITUDE
!   BBB      I         SIDE BANDS AMPLITUDE
!   D        I         NORMALIZED POLYNIMIAL EQN FROM EGRAV
!   IIPPTT   I         POINTER ARRAY FOR MULTIPLE FREQUENCIES
!   ACCTRF   O         TRUE OF REF ACCELERATION DUE TO TIDES
!   GRPAR    O         PARTIALS OF ACCELERATION   W.R.T. A& BCOEFS
!   AORN     I         GMR*(A/R)**N
!   IQNDEX   I         POINTER FOR Q*LAMDA TERM; QQ-IQMIN+1
!   UNORM    I         DENORMALIZING FACTORE FOR LEGENDRE POLY.
!
! COMMENTS:
!
! REFERENCES:
!            "OBSERVED TIDAL BRAKING IN THE EARTH/MOON/SUN SYSTEM"
!             BY CHRISTODOULIDIS, ET AL., JGR, VOL 93, P. 6216-6236,
!             JUNE 10,1988.
!
!             NASA TECH MEMO 86180, "ALTIMETRY, ORBITS AND TIDES",
!             OSCAR L. COLOMBO
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
      INTEGER LL1,QQ
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMBI/RMBI(9)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/QQTIDE/IQMAX,IQMIN,IQDIF,NUMFRQ,NBAND
      COMMON/TDELEM/XMEAN(2),XNODE(2),OMEGA(2),A(2),XINCL(2),E(2),      &
     &   EPSLON,RMASS,XLONG,TANPSI,THETG,RHO3,CC(3,3),B(2)
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      DIMENSION AT(1),BT(1),P(1),COEF(1),XQQ(1),IPT(1),                 &
     &          XIEXP(1),XMM(1),XLL1(1),XSIGN1(1),XSN2QQ(1),            &
     &          X2M2HH(1),X2M2HJ(1),XKK(1),JJBDY(1),SCRTCH(NTIDE,10),   &
     &          ACCTRF(3),GRPAR(NEQN,3),AORN(1)
      DIMENSION IIPPTT(NTIDE),IQNDEX(NTIDE),LL1(NTIDE)
      DIMENSION XSIGN2(NTIDE)
      DIMENSION ACCRFL(3),QQ(1)
      DIMENSION AAA(1),BBB(1),D(1)
      DIMENSION UNORM(NTIDE)
      DATA ZERO/0.D0/,HALF/.5D0/,ONE/1.D0/,TWO/2.D0/,                   &
     &     THREE/3.D0/,SIX/6.D0/
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      RRCIP=ONE/R
      RGMR=ONE/GMR
      THGPI2=THETG+PI*HALF
      TANPSI=SINPSI/COSPSI
!
! COMPUTE ANGULAR ARGUMENT FOR DISTINCT FREQ ONLY (NUMFREQ)
      DO 100 I=1,NUMFRQ
      SCRTCH(I,1)=OMEGA(JJBDY(I))
      SCRTCH(I,2)=XMEAN(JJBDY(I))
      SCRTCH(I,3)=XNODE(JJBDY(I))
      SCRTCH(I,4)=-(X2M2HH(I)*SCRTCH(I,1)                               &
     &             +X2M2HJ(I)*SCRTCH(I,2)                               &
     &             +XKK(I)*SCRTCH(I,3))                                 &
     &             *XSIGN1(I)+ XMM(I)*THGPI2
  100 END DO
!
! FIND SIN AND COSINE OF ANGULAR ARGUMENT (DISTINCT FREQ ONLY)
! SCRTCH(1,7)=SIN SCRTCH(1,8)=COS
      DO 200 I=1,NUMFRQ
      SCRTCH(I,7)=SIN(SCRTCH(I,4))
      SCRTCH(I,8)=COS(SCRTCH(I,4))
  200 END DO
!
! MATCH ANGULAR ARGUMENT SIN AND COS WITH CORRESPONDING TIDE CARDS
! SCRTCH(1,9)=SIN SCRTCH(1,10)=COS
      DO 300 I=1,NTIDE
      SCRTCH(I,9) =SCRTCH(IIPPTT(I),7)
      SCRTCH(I,10)=SCRTCH(IIPPTT(I),8)
  300 END DO
!
! COMPUTE Q*XLONG FOR EVERY Q BETWEEN QMAX & QMIN
      DO 400 I=1,IQDIF
      SCRTCH(I,7)=XLONG*(IQMIN+I-1)
  400 END DO
!
! STORE SIN AND COS OF QLAMBDA
! SCRTCH(1,8)=SIN SCRTCH(1,7)=COS
      DO 500 I=1,IQDIF
      SCRTCH(I,8)=SIN(SCRTCH(I,7))
      SCRTCH(I,7)=COS(SCRTCH(I,7))
  500 END DO
!
! MATCH SIN AND COS OF QLAMBDA WITH CORRESPONDING TIDE CARD
! SCRTCH(1,4)=SIN  SCRTCH(1,5)=COS
      DO 600 I=1,NTIDE
      SCRTCH(I,4)=SCRTCH(IQNDEX(I),8)*XSIGN2(I)
      SCRTCH(I,5)=SCRTCH(IQNDEX(I),7)
  600 END DO
!
! USING TRIG IDENTITY FOR ANGLE SUMMATION, COMPUTE SIN AND COS OF
! ANGULAR ARGUMENT +- QLAMBDA
! INPUT:
!        SCRTCH(1,9)=SIN(ANGARG) SCRTCH(1,10)=COS(ANGARG)
!        SCRTCH(1,4)=SIN(QLAMBDA) SCRTCH(1,5)=COS(QLAMBDA)
! OUTPUT:
!        SCRTCH(1,3) = SIN(ANG ARG + QLAMBDA)
!        SCRTCH(1,4) = COS(ANG ARG + QLAMBDA)
      DO 700 I=1,NTIDE
      SCRTCH(I,3)=SCRTCH(I,9)*SCRTCH(I,5)+SCRTCH(I,10)*SCRTCH(I,4)
      SCRTCH(I,4)=SCRTCH(I,10)*SCRTCH(I,5)-SCRTCH(I,9)*SCRTCH(I,4)
  700 END DO
!
! MULTIPLY ANGLE BY SIDEBAND COEFFICIENTS
! SCRTCH(1,1)=SIN OF TOTAL ARGUMENT  SCRTCH(1,2)=COS OF TOTAL ARGUMENT
      DO 800 I=1,NTIDE
      SCRTCH(I,2)=AAA(I)*SCRTCH(I,4)+BBB(I)*SCRTCH(I,3)
      SCRTCH(I,1)=AAA(I)*SCRTCH(I,3)-BBB(I)*SCRTCH(I,4)
  800 END DO
!
! GET LGENDR POLYNOMIALS
      DO 900 I=1,NTIDE
      SCRTCH(I,6)=P(IPT(I))
      SCRTCH(I,5)=D(IPT(I))
  900 END DO
!
! USE UNNORMALIZED LEGENDRE POLYNOMIALS
      IF(NUNPLY .EQ. 1 )THEN
      DO 950 I=1,NTIDE
      SCRTCH(I,5)=SCRTCH(I,5)/UNORM(I)
      SCRTCH(I,6)=SCRTCH(I,6)/UNORM(I)
  950 END DO
      ENDIF
!
! GET COEFFICIENTS
      DO 1000 I=1,NTIDE
      SCRTCH(I,3)=AORN(LL1(I))*RGMR*COEF(I)*XIEXP(I)
 1000 END DO
! COMBINE LEGENDRE POLYNOMIALS AND COEFFICIENTS
      DO 1100 I=1,NTIDE
      SCRTCH(I,5)=-SCRTCH(I,5)*SCRTCH(I,3)
      SCRTCH(I,6)= SCRTCH(I,6)*SCRTCH(I,3)
 1100 END DO
!
! COMBINE THIS AMPLITDE WITH ANGULAR ARG AND SIDE BAND AMP
      DO 1200 I=1,NTIDE
      SCRTCH(I,4)=SCRTCH(I,1)*SCRTCH(I,6)
      SCRTCH(I,6)=SCRTCH(I,2)*SCRTCH(I,6)
 1200 END DO
!
! FILL IN  ARRAY OF PARTIALS IN RFL
      DO 1300 I=1,NTIDE
      SCRTCH(I,2)=SCRTCH(I,5)*SCRTCH(I,2)
      SCRTCH(I,5)=SCRTCH(I,5)*SCRTCH(I,1)
      SCRTCH(I,3)=SCRTCH(I,4)*XSN2QQ(I)
      SCRTCH(I,1)=RRCIP*XLL1(I)
      SCRTCH(I,4)=SCRTCH(I,4)*SCRTCH(I,1)
      SCRTCH(I,1)=SCRTCH(I,1)*SCRTCH(I,6)
      SCRTCH(I,6)=-XSN2QQ(I)*SCRTCH(I,6)
 1300 END DO
!
!
!OMPUTE TIDAL ACCELERATIONS (AT&BT FROM INPUT OT&ET CARDS)
      ACCRFL(1)=ZERO
      ACCRFL(2)=ZERO
      ACCRFL(3)=ZERO
      DO 1400 I=1,NTIDE
      ACCRFL(1)=ACCRFL(1)+AT(I)*SCRTCH(I,1)+BT(I)*SCRTCH(I,4)
      ACCRFL(2)=ACCRFL(2)+AT(I)*SCRTCH(I,2)+BT(I)*SCRTCH(I,5)
      ACCRFL(3)=ACCRFL(3)+AT(I)*SCRTCH(I,3)+BT(I)*SCRTCH(I,6)
 1400 END DO
!
!     TRANSFORM TO TRUE OF REF
      ACCTRF(1)=RMBI(1)*ACCRFL(1)+RMBI(4)*ACCRFL(2)+RMBI(7)             &
     &         *ACCRFL(3)
      ACCTRF(2)=RMBI(2)*ACCRFL(1)+RMBI(5)*ACCRFL(2)+RMBI(8)             &
     &         *ACCRFL(3)
      ACCTRF(3)=RMBI(3)*ACCRFL(1)+RMBI(6)*ACCRFL(2)+RMBI(9)             &
     &         *ACCRFL(3)
      IF(.NOT.LSATID) RETURN
      IF(NETADJ.LE.0) GO TO 1700
!
! STORE PARTIALS
      DO 1500 I=1,NETADJ
      GRPAR(JSGRP+I-1,1)=RMBI(1)*SCRTCH(I,1)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(4)*SCRTCH(I,2)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(7)*SCRTCH(I,3)
      GRPAR(JSGRP+I-1,2)=RMBI(2)*SCRTCH(I,1)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(5)*SCRTCH(I,2)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(8)*SCRTCH(I,3)
      GRPAR(JSGRP+I-1,3)=RMBI(3)*SCRTCH(I,1)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(6)*SCRTCH(I,2)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(9)*SCRTCH(I,3)
 1500 END DO
      JSGRP=JSGRP+NETADJ
      DO 1600 I=1,NETADJ
      GRPAR(JSGRP+I-1,1)=RMBI(1)*SCRTCH(I,4)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(4)*SCRTCH(I,5)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(7)*SCRTCH(I,6)
      GRPAR(JSGRP+I-1,2)=RMBI(2)*SCRTCH(I,4)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(5)*SCRTCH(I,5)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(8)*SCRTCH(I,6)
      GRPAR(JSGRP+I-1,3)=RMBI(3)*SCRTCH(I,4)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(6)*SCRTCH(I,5)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(9)*SCRTCH(I,6)
 1600 END DO
 1700 CONTINUE
      IF(NOTADJ.LE.0) RETURN
      JSGRP=JSGRP+NETADJ
      ISSTRT=NETADJ+1
      DO 1800 I=1,NOTADJ
      GRPAR(JSGRP+I-1,1)=RMBI(1)*SCRTCH(ISSTRT+I-1,1)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(4)*SCRTCH(ISSTRT+I-1,2)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(7)*SCRTCH(ISSTRT+I-1,3)
      GRPAR(JSGRP+I-1,2)=RMBI(2)*SCRTCH(ISSTRT+I-1,1)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(5)*SCRTCH(ISSTRT+I-1,2)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(8)*SCRTCH(ISSTRT+I-1,3)
      GRPAR(JSGRP+I-1,3)=RMBI(3)*SCRTCH(ISSTRT+I-1,1)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(6)*SCRTCH(ISSTRT+I-1,2)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(9)*SCRTCH(ISSTRT+I-1,3)
 1800 END DO
      JSGRP=JSGRP+NOTADJ
      DO 1900 I=1,NOTADJ
      GRPAR(JSGRP+I-1,1)=RMBI(1)*SCRTCH(ISSTRT+I-1,4)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(4)*SCRTCH(ISSTRT+I-1,5)
      GRPAR(JSGRP+I-1,1)=GRPAR(JSGRP+I-1,1)                             &
     &                  +RMBI(7)*SCRTCH(ISSTRT+I-1,6)
      GRPAR(JSGRP+I-1,2)=RMBI(2)*SCRTCH(ISSTRT+I-1,4)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(5)*SCRTCH(ISSTRT+I-1,5)
      GRPAR(JSGRP+I-1,2)=GRPAR(JSGRP+I-1,2)                             &
     &                  +RMBI(8)*SCRTCH(ISSTRT+I-1,6)
      GRPAR(JSGRP+I-1,3)=RMBI(3)*SCRTCH(ISSTRT+I-1,4)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(6)*SCRTCH(ISSTRT+I-1,5)
      GRPAR(JSGRP+I-1,3)=GRPAR(JSGRP+I-1,3)                             &
     &                  +RMBI(9)*SCRTCH(ISSTRT+I-1,6)
 1900 END DO
      RETURN
      END
