!$CNMARS
      SUBROUTINE CNMARS(MJDSEC,FSEC,DPSI,EPST,EPSM,NM,SCRTCH)
!********1*********2*********3*********4*********5*********6*********7
! CNMARS           08/26/92            0000.0    PGMR - S.LUO
!
! FUNCTION:  CALL NUTMAR FOR GETTING MARS NUTATION
!
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    EPHEMERIS SECONDS SINCE GEODYN REFERENCE TIME
!   FSEC     I    A    EPHEMERIS SECONDS SINCE MJDSC
!   DPSI     O    A    MARS NUTATION IN LONGITUDE
!   EPSI     O    A    MARS NUTATION IN OBLIQUITY
!   EPST     O    A    MARS TRUE OBLIQUITY AT T
!   EPSM     O    A    MARS MEAN OBLIQUITY AT T
!   NM       I    S    NUMBER OF OBSERVATION
!   SCRTCH        A    WORK ARRAY, 5*NM IN LENGTH
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
          IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL (L)
      SAVE
!
      DIMENSION FSEC(NM),DPSI(NM),EPST(NM),EPSM(NM),SCRTCH(NM,5)
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
      T0=2451545.0D0-TMGDN1
!
      DO 10 I=1,NM
!
! GET OBSERVATION TIME IN DAYS SINCE J2000.0 (JD2451545.0)
!
      T = (DBLE(MJDSEC)+FSEC(I))/86400.D0 - T0
!
      CALL NUTMAR(T,DPSIM,DEPSM,EPSMA)
      DPSI(I)=DPSIM/36.D5
      EPSM(I)=DEPSM/36.D5
      EPST(I)=EPSM(I) + EPSMA
!.. DEBUG OUT
!     WRITE(6,99) I,EPSMA
!  99 FORMAT(1X,I4, F15.8)
!
! CONVERT DEGREE TO RANDIAN
      DPSI(I) = DPSI(I)*DEGRAD
      EPSM(I) = EPSM(I)*DEGRAD
      EPST(I) = EPST(I)*DEGRAD
   10 END DO
!
      RETURN
      END
