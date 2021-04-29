!$PRMCSS
      SUBROUTINE PRMCSS(MJDSEC,FSEC,PDPSI,PEPST,PEPSM,SCRTCH,NM)
!
!********1*********2*********3*********4*********5*********6*********7
! PRMCSS           01/11/93            0000.0    PGMR - S.LUO
!
! FUNCTION:  CALCULATE MARS PRECESSION IN MARS ORBIT PLANE REFRENCE
!            FRAME
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T        I         INTERVAL TIME IN DAY FROM J2000.0(JD2451545)
!   PDPSI    O    A    MARS PRECESSION IN LONGITUDE (0."001)
!   PEPSM    O    A    MARS MEAN EQUILITY BETWEEN ORBIT AND EQUATOR
!                      AT EPOCH J2000
!   EPSMT    O    A    MARS MEAN EQUILITY BETWEEN ORBIT AND EQUATOR
!                      AT T
!   ...................................................................
!
! REFERENCES:
!
! 1. R.D.REASENBERG AND R.W.KING,   THE ROTATION OF MARS
!                  J.G.R. VO.84, NO.B11, P6231 (OCT., 1979)
!
! 2. J.L.HILTON,   THE MOTION OF MAR'S POLE I. RIGID BODY PRECESSION
!                  AND NUTATION
!          THE ASTRONOMICAL JOURNAL  VO.102, NO.4,P1510 (OCT.,1990)
!
! 3. L.BASS AND R.CESARONE, MARS OBSERVER:PLANETARY CONSTANT AND
!                           MODELS
!          JPL D-3444 (DRAFT)  (NOVEMBER, 1990)
!
! 4. C.A. MURRAY, VECTORIAL ASTROMETRY
!          ADAM HILGER LTD, BRISTOL (1983)
!
! 5. P.K.SELDELMANN, 1980 IAU THEORY OF NUTATION: THE FINAL REPORT
!                    OF THE IAU WORKING GROUP ON NUTATION
!          CELESTIAL MECHANICS VO.27, P79-106(1982)
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
      DIMENSION FSEC(NM),PDPSI(NM),                                     &
     &       PEPST(NM),PEPSM(NM),SCRTCH(NM,5)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!
! GET THE MEAN OBLIQUITY OF MARS EQUATOR W.R.T ORBIT PLANE
! AT J2000 AND T
!
      CALL EPSMRR(0.D0,ARMN0,EPSMM)
!
      PEPSM(1)=EPSMM
! FOR DEBUG OUT
      ARMN0=ARMN0/DEGRAD
      EPSMM=EPSMM/DEGRAD
!
      DO 20 I = 1,NM
      T = (DBLE(MJDSEC)+FSEC(I))/86400.D0 + TMGDN1
      T=(T-2451545.0)
!
      CALL EPSMRR(T,ARMNT,EPSMT)
!
! CALCULATE MARS PRECESSION IN LONGITUDE AND OBLIQUITY IF OPTION
! (DR. HILTON'S MODEL)
!
         T1  = T / 36525.D0
! ****** TEST NEW VALUE ****
!     DLTPSI = 756.6 * T1/3600.D0
      DLTPSI = 729.6 * T1/3600.D0
!
!  GET THE MARS PRECESSION METRIX, PRSMRS(3,3)
!
      DLTPSI = DLTPSI * DEGRAD
      DLTEPS = EPSMT-PEPSM(1)
!
      PDPSI(I)=DLTPSI
      PEPST(I)=EPSMT
      PEPSM(I)=PEPSM(1)
!
! ..FOR DEGUT OUT
      EPSMT=PEPST(I)/DEGRAD
      ARMNT=ARMNT/DEGRAD
!C
      DLTEPS = (DLTEPS/DEGRAD)*3600
!C
!C    WRITE(6,1000)T,EPSMM,EPSMT,ARMN0,ARMNT,DLTEPS
   20 END DO
!
 1000 FORMAT(1X,F10.1,/5((1X,F15.8)/))
      RETURN
      END
