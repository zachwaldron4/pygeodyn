!$TDCONR
      SUBROUTINE TDCONR(MJDSEC,FSEC,CONNAM,ANGFAC,SCRTCH,GRDANG,        &
     &                  UANG,FAMP,IMP2FR,ISIDE,TIDES,                   &
     &                  OFACT,EXPRFL,IPRESP,IMP2RS,IDOODN,VRARAY,       &
     &                  CORPAR,XM,XNP1,P,COSLAM,SINLAM,AORN,WORK,SPEED, &
     &                  XSIGN,EXPART,NEQN,ACC,INCP,NTDDOO)
!********1*********2*********3*********4*********5*********6*********7**
! TDCONR           00/00/00            8604.0    PGMR - ?
!
! FUNCTION:  CONTROLS TIDE COMPUTATIONS FOR THE FORCE MODEL FOR THE
!            CASE WHEN THE STANDARD (NOT CHRISTODOULIDIS) TIDE
!            APPROACH IS BEING USED.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    INTEGER TIME
!   FSEC     I    A    FRACTIONAL SECONDS FROM MJDSEC
!   ANGFAC   I    A    FACTORS TO MULTIPLY EACH OF THE 5 ARGUMENTS BY
!                      PLUS A -/+ PHASE FOR CERTAIN DOODSON NUMBERS
!   SCRTCH  I/O   A    SCRATCH ARRAY FOR INTERMEDIATE CALCULATION
!   GRDANG   O    A    ANGLE COMPUTED FROM THE LINEAR COMBINATION
!                      OF ASTRONOMICAL ANGLES (GIVEN AT GRID TIMES)
!   UANG     O    A    ANGLE TO BE ADDED TO GRDANG TO COMPENSATE
!                      FOR SIDE BANDS (GIVEN AT GRID TIMES)
!   FAMP     O    A    FACTOR TO MULTIPLLT AMPLITUDE OF THE TIDE TO
!                      COMPENSATE FOR SIDE BANDS (GIVEN AT GRID TIMES)
!   IMP2FR   I    A    ARRAY WHICH MAPS FROM TIDE PAIR TO A LIST OF
!                      UNIQUE DOODSON NUMBERS
!   ISIDE    I    A    ARRAY WHICH HAS FLAGS FOR WHETHER TO COMPUTE
!                      SIDEBANDS OR NOT
!   TIDES    I    A    CURRENT VALUES OF THE FREQUENCY DEPENDENT
!                      EARTH AND OCEAN (AND ATMOSPHERIC) TIDES. THESE
!                      ARE A AND B COEFFICIENTS. IN THE CASE OF
!                      UNADJUSTED OTIDES THESE HAVE BEEN SCALED TO TAKE
!                      INTO ACCOUNT SEA WATER DENSITY AND LOADING
!   OFACT    I    A    FACTORS TO TAKE INTO ACCOUNT LOADING AND SEA
!                      WATER DENSITY. THESE ARE ONLY FOR THE ADJUSTED
!                      OTIDES (UNADJUSTED OTIDES HAVE THIS FACTOR
!                      BUILT INTO TIDES ARRAY)
!   EXPRFL   I    A    PARTIAL COMPUTATIONS FOR GRAVITY ACCELERATIONS
!                      FROM SUBROUTINE EGRAV
!   IPRESP   I    A    POINTER TO THE NRESP LOCATIONS IN THE EXPRFL
!                      ARRAY THAT ARE USED FOR TIDES.
!   IMP2RS   I    A    MAP FROM THE FREQUENCY DEPENDENT TIDE PAIRS TO
!                      THE NRESP UNIQUE RESPONSE DEGREE AND ORDERS
!   VRARAY   I    A    ACCELERATIONS (IN RFL) DUE TO GEOPOTENTIAL,
!                      R,RSQ,XYSQ,RTXYSQ,GM/R (BODY FIXED)
!   CORPAR        A    TRANSFORMATION MATRIX FOR BODY FIXED R,PHI,LAMBDA
!   XM       I    A    ARRAY OF PRECOMPUTED ORDERS TO MATCH C AND S
!                      ARRAYS
!   XNP1     I    A    ARRAY OF PRECOMPUTED DEGREES PLUS 1 TO MATCH C
!                      AND S ARRAYS
!   P        I    A    LEGENDRE POLYNOMIAL ARRAY FOR SATELLITE
!   COSLAM   I    A    COSINE M*L ARRAY FOR SATELLITE
!   SINLAM   I    A    SINE M*L ARRAY FOR SATELLITE
!   AORN     I    A    (AE/R)**N ARRAY FOR SATELLITE
!   WORK          A    WORKING ARRAY USED TO HOLD RESPONSE EFFECT
!                      INFORMATION. THIS ARRAY IS EQUIVALENT TO
!                      THE EXPLICIT PARTIALS OF ACCELERATION WITH
!                      RESPECT TO GEOPOTENTIAL COEFFICIENTS
!   SPEED    I    A    RATE OF CHANGE OF ANGULAR FORCING ARGUMENT
!   XSIGN    I    A    1.D0 IF PROGRADE ; -1.D0 IF RETROGRADE
!   EXPART   O    A    ARRAY OF EXPLICIT PARTIALS OF ACCELERATION
!                      WRT FORCE MODEL PARAMETERS (TRUE OF REF)
!   NEQN     I    S    NUMBER OF ADJUSTING FORCE MODEL PARAMETERS
!                      ASSOCIATED WITH CURRENT SATELLLITE
!   ACC      O    A    TRUE OF STEP ACELERATION DUE TO TIDES
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CRMI/RMI(9)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/SETADJ/LSADRG(4),LSASRD(4),LSAGA(4),LSAGP,LSATID,LSAGM,    &
     &              LSADPM,LSAKF,LSADNX,LSADJ2,LSAPGM
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
      COMMON/TIDE2I/ISCRT(5),INTRT,NTMRT,JMP2RS(10)
      COMMON/TIDE2R/TIDESS(20)
      DIMENSION ANGFAC(NDOODN,6)
      DIMENSION CONNAM(NDOODN)
      DIMENSION IDOODN(NDOODN,3)
      DIMENSION IMP2FR(NTIDE),ISIDE(NTIDE),SCRTCH(NTDDOO,6)
      DIMENSION GRDANG(NDOODN,MXTMRT)
      DIMENSION UANG(NDOODN,MXTMRT,3)
      DIMENSION FAMP(NDOODN,MXTMRT,3)
      DIMENSION SPEED(NDOODN)
      DIMENSION TIDES(NTIDE,2),XSIGN(NTIDE)
      DIMENSION OFACT(NOTADJ)
      DIMENSION INCP(NTIDE)
      DIMENSION EXPRFL(NP,6)
      DIMENSION IPRESP(NRESP),WORK(NRESP,6)
      DIMENSION IMP2RS(NTIDE)
      DIMENSION VRARAY(8),CORPAR(9)
      DIMENSION XM(NP),XNP1(NP),P(NP),COSLAM(NMAXP1),                   &
     &          SINLAM(NMAXP1),AORN(NMAX)
      DIMENSION EXPART(NEQN,3)
      DIMENSION BFRTTR(9)
      DIMENSION ACC(3)
!
      DATA EPSP1/1.0001D0/,EPSM1/-.9999D0/
      DATA ETA1/32.1496183D0/
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
      PI2=ACOS(-1.D0)/2.D0
      ACC(1)=0.D0
      ACC(2)=0.D0
      ACC(3)=0.D0

      IF(NRESP.LE.0) RETURN

      ISTART=JSATID+NSTADJ
!
! START OFF WITH RESPONSE EFFECT COMPUTATIONS FROM GRAVITY FIELD
!
!   GET MATRIX TO GO FROM R,PSI,LAMDA TO TRUE OF REFERENCE
!
      BFRTTR(1)=RMB(1)*CORPAR(1)+RMB(2)*CORPAR(4)+RMB(3)*CORPAR(7)
      BFRTTR(2)=RMB(4)*CORPAR(1)+RMB(5)*CORPAR(4)+RMB(6)*CORPAR(7)
      BFRTTR(3)=RMB(7)*CORPAR(1)+RMB(8)*CORPAR(4)+RMB(9)*CORPAR(7)
      BFRTTR(4)=RMB(1)*CORPAR(2)+RMB(2)*CORPAR(5)+RMB(3)*CORPAR(8)
      BFRTTR(5)=RMB(4)*CORPAR(2)+RMB(5)*CORPAR(5)+RMB(6)*CORPAR(8)
      BFRTTR(6)=RMB(7)*CORPAR(2)+RMB(8)*CORPAR(5)+RMB(9)*CORPAR(8)
      BFRTTR(7)=RMB(1)*CORPAR(3)+RMB(2)*CORPAR(6)+RMB(3)*CORPAR(9)
      BFRTTR(8)=RMB(4)*CORPAR(3)+RMB(5)*CORPAR(6)+RMB(6)*CORPAR(9)
      BFRTTR(9)=RMB(7)*CORPAR(3)+RMB(8)*CORPAR(6)+RMB(9)*CORPAR(9)
!
!   DIVIDE FIRST COLUMN BY R TO ACCOUNT FOR THE FACT THAT
!   R PARTIALS ARE TOO LARGE BY R
!
      T1=BFRTTR(1)/VRARAY(4)
      T2=BFRTTR(2)/VRARAY(4)
      T3=BFRTTR(3)/VRARAY(4)
!
!   PUT PARTIALS WRT C INTO WORK
!
      DO 100 I=1,NRESP
      ICPX=IPRESP(I)
      XORDP1=XM(ICPX)+EPSP1
      IORDP1=XORDP1
      XDEG=XNP1(ICPX)+EPSM1
      IDEG=XDEG
      IF(IDEG.GT.NMAX) THEN
         WRITE(6,7000)
         WRITE(6,7001) IDEG
         WRITE(6,7002) NMAX
         STOP
      ENDIF
      WW1=-AORN(IDEG)*XNP1(IPRESP(I))*P(IPRESP(I))
      WW2=AORN(IDEG)*EXPRFL(IPRESP(I),2)
      WW3=AORN(IDEG)*XM(IPRESP(I))*P(IPRESP(I))
      W1=WW1*COSLAM(IORDP1)
      W2=WW2*COSLAM(IORDP1)
      W3=-WW3*SINLAM(IORDP1)
      WORK(I,1)=T1*W1+BFRTTR(4)*W2+BFRTTR(7)*W3
      WORK(I,2)=T2*W1+BFRTTR(5)*W2+BFRTTR(8)*W3
      WORK(I,3)=T3*W1+BFRTTR(6)*W2+BFRTTR(9)*W3
      IF(IORDP1.EQ.1) THEN
      WORK(I,4)=0.D0
      WORK(I,5)=0.D0
      WORK(I,6)=0.D0
      ELSE
      W1=WW1*SINLAM(IORDP1)
      W2=WW2*SINLAM(IORDP1)
      W3=WW3*COSLAM(IORDP1)
      WORK(I,4)=T1*W1+BFRTTR(4)*W2+BFRTTR(7)*W3
      WORK(I,5)=T2*W1+BFRTTR(5)*W2+BFRTTR(8)*W3
      WORK(I,6)=T3*W1+BFRTTR(6)*W2+BFRTTR(9)*W3
      ENDIF
  100 END DO
!
!     NOW WORK ON FORCING PART
!
      IF(NTIDE.LE.0) GO TO 510
      IF(ICBDGM.NE.ITBDGM) GO TO 510
!
!
!  OFFU IS AN APPRX ET-UT1
      OFFU=A1UT(32)+ETA1
      FSECX=FSEC-OFFU
       CALL RAYGRD (MJDSEC,FSECX,CONNAM,ANGFAC,SCRTCH,GRDANG,           &
     &              UANG,FAMP,IDOODN,ISLP,ISRP,NTDDOO)
      DIST=DBLE(MJDSEC-ISCRT(ISLP))+FSECX
      DIST2=DBLE(MJDSEC)+FSECX
      DIST2=MOD(DIST2,86400.D0)
      FRAC2=DIST/DBLE(INTRT)
      FRAC1=1.D0-FRAC2
      IS=ISTART-1
!
!
!  SIDE BAND ALTERATIONS
!
!
      DO 250 IBAND=1,3
      ITBAND=IBAND-1
!
!    COMPUTE UP ALL THE COS AND SIN TERMS FOR FORCING ASSUMING SIDE BAND
!
      DO 200 I=1,NDOODN
!!!   SCRTCH(I,4)=FRAC1*GRDANG(I,ISLP)+FRAC2*GRDANG(I,ISRP)             &
!
! GRDANG DOES NOT NEED TO BE INTERPLOATED. IT IS THE CORRECY ANGLE
! AT 0HR UT1. SPEED TAKES UP THE DIFFEREBCE AFTER 0 HRS
      SCRTCH(I,4)=GRDANG(I,ISLP)                                        &
     &           +FRAC1*UANG(I,ISLP,IBAND)                              &
     &           +FRAC2*UANG(I,ISRP,IBAND)+DIST2*SPEED(I)
      SCRTCH(I,3)=FRAC1*FAMP(I,ISLP,IBAND)+FRAC2*FAMP(I,ISRP,IBAND)
      SCRTCH(I,5)=COS(SCRTCH(I,4))*SCRTCH(I,3)
      SCRTCH(I,6)=SIN(SCRTCH(I,4))*SCRTCH(I,3)
  200 END DO
!
!    LOAD ALL THE COS AND SIN TERMS FOR FORCING FOR TIDES WTTH SIDE BAND
!
      DO 210 I=1,NTIDE
      IF(ISIDE(I).NE.ITBAND) GO TO 210
      PA=DBLE(INCP(I))*PI2
      SCRTCH(I,1)=COS(SCRTCH(IMP2FR(I),4)+PA)*SCRTCH(IMP2FR(I),3)
      SCRTCH(I,2)=SIN(SCRTCH(IMP2FR(I),4)+PA)*SCRTCH(IMP2FR(I),3)
  210 END DO
  250 CONTINUE
!
!
!  END SIDE BAND ALTERATIONS
!
!
! ADJUSTED ETIDES
!
      IF(NETADJ.LE.0) GO TO 320
      IF(LSATID) GO TO 310
!  NOT ON LAST ITERATION (PARTIALS NOT CUT)
      DO 305 I=1,NETADJ
      ACC(1)=ACC(1)+TIDES(I,1)                                          &
     &  *(SCRTCH(I,1)*WORK(IMP2RS(I),1)                                 &
     &  -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),4))                        &
     &             +TIDES(I,2)                                          &
     &  *(SCRTCH(I,2)*WORK(IMP2RS(I),1)                                 &
     &  +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),4))
      ACC(2)=ACC(2)+TIDES(I,1)                                          &
     &  *(SCRTCH(I,1)*WORK(IMP2RS(I),2)                                 &
     &  -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),5))                        &
     &             +TIDES(I,2)                                          &
     &  *(SCRTCH(I,2)*WORK(IMP2RS(I),2)                                 &
     &  +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),5))
      ACC(3)=ACC(3)+TIDES(I,1)                                          &
     &  *(SCRTCH(I,1)*WORK(IMP2RS(I),3)                                 &
     &  -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),6))                        &
     &             +TIDES(I,2)                                          &
     &  *(SCRTCH(I,2)*WORK(IMP2RS(I),3)                                 &
     &  +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),6))
  305 END DO
      GO TO 320
  310 CONTINUE
!  ON LAST ITERATION (PARTIALS CUT)
      ISA=ISTART-1
      ISB=ISA+NETADJ
      DO 315 I=1,NETADJ
      EXPART(ISA+I,1)=SCRTCH(I,1)*WORK(IMP2RS(I),1)                     &
     &               -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),4)
      EXPART(ISA+I,2)=SCRTCH(I,1)*WORK(IMP2RS(I),2)                     &
     &               -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),5)
      EXPART(ISA+I,3)=SCRTCH(I,1)*WORK(IMP2RS(I),3)                     &
     &               -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),6)
      EXPART(ISB+I,1)=SCRTCH(I,2)*WORK(IMP2RS(I),1)                     &
     &               +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),4)
      EXPART(ISB+I,2)=SCRTCH(I,2)*WORK(IMP2RS(I),2)                     &
     &               +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),5)
      EXPART(ISB+I,3)=SCRTCH(I,2)*WORK(IMP2RS(I),3)                     &
     &               +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),6)
!
      ACC(1)=ACC(1)+TIDES(I,1)*EXPART(ISA+I,1)                          &
     &             +TIDES(I,2)*EXPART(ISB+I,1)
      ACC(2)=ACC(2)+TIDES(I,1)*EXPART(ISA+I,2)                          &
     &             +TIDES(I,2)*EXPART(ISB+I,2)
      ACC(3)=ACC(3)+TIDES(I,1)*EXPART(ISA+I,3)                          &
     &             +TIDES(I,2)*EXPART(ISB+I,3)
  315 END DO
  320 CONTINUE
!
!
! ADJUSTED OTIDES
!
      IF(NOTADJ.LE.0) GO TO 420
      IS1=NETADJ+1
      IS2=NETADJ+NOTADJ
      IF(LSATID) GO TO 410
!  NOT ON LAST ITERATION (PARTIALS NOT CUT)
      DO 405 I=IS1,IS2
      ACC(1)=ACC(1)+OFACT(I-NETADJ)*(TIDES(I,1)                         &
     &    *(SCRTCH(I,1)*WORK(IMP2RS(I),1)                               &
     &     -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),4))                     &
     &                              +TIDES(I,2)                         &
     &    *(SCRTCH(I,2)*WORK(IMP2RS(I),1)                               &
     &     +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),4)))
      ACC(2)=ACC(2)+OFACT(I-NETADJ)*(TIDES(I,1)                         &
     &    *(SCRTCH(I,1)*WORK(IMP2RS(I),2)                               &
     &     -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),5))                     &
     &                              +TIDES(I,2)                         &
     &    *(SCRTCH(I,2)*WORK(IMP2RS(I),2)                               &
     &     +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),5)))
      ACC(3)=ACC(3)+OFACT(I-NETADJ)*(TIDES(I,1)                         &
     &    *(SCRTCH(I,1)*WORK(IMP2RS(I),3)                               &
     &     -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),6))                     &
     &                              +TIDES(I,2)                         &
     &    *(SCRTCH(I,2)*WORK(IMP2RS(I),3)                               &
     &     +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),6)))
  405 END DO
      GO TO 420
  410 CONTINUE
!  ON LAST ITERATION (PARTIALS CUT)
      ISA=ISTART-1+NETADJ
      ISB=ISA+NOTADJ
      DO 415 I=IS1,IS2
      EXPART(ISA+I,1)=(SCRTCH(I,1)*WORK(IMP2RS(I),1)                    &
     &               -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),4))           &
     &               *OFACT(I-NETADJ)
      EXPART(ISA+I,2)=(SCRTCH(I,1)*WORK(IMP2RS(I),2)                    &
     &               -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),5))           &
     &               *OFACT(I-NETADJ)
      EXPART(ISA+I,3)=(SCRTCH(I,1)*WORK(IMP2RS(I),3)                    &
     &               -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),6))           &
     &               *OFACT(I-NETADJ)
      EXPART(ISB+I,1)=(SCRTCH(I,2)*WORK(IMP2RS(I),1)                    &
     &               +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),4))           &
     &               *OFACT(I-NETADJ)
      EXPART(ISB+I,2)=(SCRTCH(I,2)*WORK(IMP2RS(I),2)                    &
     &               +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),5))           &
     &               *OFACT(I-NETADJ)
      EXPART(ISB+I,3)=(SCRTCH(I,2)*WORK(IMP2RS(I),3)                    &
     &               +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),6))           &
     &               *OFACT(I-NETADJ)
!
      ACC(1)=ACC(1)+TIDES(I,1)*EXPART(ISA+I,1)                          &
     &             +TIDES(I,2)*EXPART(ISB+I,1)
      ACC(2)=ACC(2)+TIDES(I,1)*EXPART(ISA+I,2)                          &
     &             +TIDES(I,2)*EXPART(ISB+I,2)
      ACC(3)=ACC(3)+TIDES(I,1)*EXPART(ISA+I,3)                          &
     &             +TIDES(I,2)*EXPART(ISB+I,3)
  415 END DO
  420 CONTINUE
!
      NREST=NETUN+NOTUN
      IF(NREST.LE.0) GO TO 510
      IS1=NETADJ+NOTADJ+1
      IS2=NETADJ+NOTADJ+NREST
      DO 500 I=IS1,IS2
      ACC(1)=ACC(1)+TIDES(I,1)                                          &
     &  *(SCRTCH(I,1)*WORK(IMP2RS(I),1)                                 &
     &  -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),4))                        &
     &             +TIDES(I,2)                                          &
     &  *(SCRTCH(I,2)*WORK(IMP2RS(I),1)                                 &
     &  +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),4))
      ACC(2)=ACC(2)+TIDES(I,1)                                          &
     &  *(SCRTCH(I,1)*WORK(IMP2RS(I),2)                                 &
     &  -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),5))                        &
     &             +TIDES(I,2)                                          &
     &  *(SCRTCH(I,2)*WORK(IMP2RS(I),2)                                 &
     &  +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),5))
      ACC(3)=ACC(3)+TIDES(I,1)                                          &
     &  *(SCRTCH(I,1)*WORK(IMP2RS(I),3)                                 &
     &  -XSIGN(I)*SCRTCH(I,2)*WORK(IMP2RS(I),6))                        &
     &             +TIDES(I,2)                                          &
     &  *(SCRTCH(I,2)*WORK(IMP2RS(I),3)                                 &
     &  +XSIGN(I)*SCRTCH(I,1)*WORK(IMP2RS(I),6))
  500 END DO
  510 CONTINUE
!
!
      CALL TIDALR(TIDESS,JMP2RS,WORK,EXPART,NEQN,ACC)
!
      A1=RMI(1)*ACC(1)+RMI(4)*ACC(2)+RMI(7)*ACC(3)
      A2=RMI(2)*ACC(1)+RMI(5)*ACC(2)+RMI(8)*ACC(3)
      A3=RMI(3)*ACC(1)+RMI(6)*ACC(2)+RMI(9)*ACC(3)
      ACC(1)=A1
      ACC(2)=A2
      ACC(3)=A3
      RETURN
 7000 FORMAT(' EXECUTION TERMINATING IN SUBROUTINE TIDCONR')
 7001 FORMAT(' TIDAL RESPONSE DEGREE ',I5,' GREATER THAN')
 7002 FORMAT(' GEOPOTENTIAL DEGREE ',I5)
      END
