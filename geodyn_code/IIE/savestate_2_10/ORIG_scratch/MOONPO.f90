!$MOONPO
      SUBROUTINE MOONPO(IMN, GM, MJDS, FSEC, XPHO, XKPHO,               &
     &                   AA, I6)
!
!********1*********2*********3*********4*********5*********6*********7**
! MOONPO           04/29/90            9004.0    PGMR - J. McCarthy
!                  10/01/91            9110.0         - S.B. LUTHCKE
!
! FUNCTION         CALCULATE PLANETARY MOON POSITION FROM AN
!                  EXTERNAL EPHEMERIS FILE.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IMN      I    S    MOON INDEX IN IMNNUM ARRAY IN COMMON PLMONI
!   GM       I    S    CENTRAL BODY GM
!   MJDS     I    S    MJD SECONDS AFTER GEODYN REFERENCE TIME
!                      FOR SELECTED TIME
!   FSEC     I    A    FRACTIONAL SECONDS OF SELECTED TIME
!   XPHO     O    A    OUTPUT CARTESIAN ELEMENTS OF MOON
!   XKPHO    O    A    OUTPUT KEPLERIAN ELEMENTS OF MOON
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES
!   I6       I    S    PRINTOUT UNIT
!
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION   (A-H,O-Z),LOGICAL (L)
      SAVE
!
      CHARACTER*6 FILNAM
!
      PARAMETER (  ZERO = 0.D0 )
      PARAMETER (  D360 = 360.D0)
      PARAMETER (  DP01 = 0.01D0)
      PARAMETER (  ONE  = 1.00D0)
!
      COMMON/CITER /NINNER,NARC,NGLOBL
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
      COMMON/PLMONI/IMNNUM(5,2),NXPLMI
      COMMON/PLMINI/ IYMDP0(5),IHMP0(5),MJDP0(5),IMNUNT(5)
      COMMON/PLMINR/ RMNMIN(5),SECP0(5),FSECP0(5),                      &
     &               XMNCHG(5),XINT(5),XJDT1(5),XJDT2(5),               &
     &               XKEP1(6,5),XKEP2(6,5)
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
!
!
      DIMENSION XPHO(6), XKPHO(6)
      DIMENSION AA(1)
!
!*************************************************************
!
      DATA LFIRST/.TRUE./
      DATA LDEBUG/.FALSE./
      DATA XJDT10/-1.D10/
      DATA XJDT20/-1.D10/
      DATA ITERP/0/
      DATA IBACK/0/
      DATA KBACK/0/
      DATA JBACK/0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!
      IBACK=0
      KBACK=0
      JBACK=0
!
!     ....REWIND TRAJ FILE AND RESET TIMES AT EACH ITERATION
!
      IF (NINNER.NE.ITERP) THEN
!
         IF (NINNER.EQ.1.AND.NGLOBL.EQ.1  ) THEN
          DO 10 IMX=IMN,NPVAL(IXPMGM)
!         DO 10 IMX=1,NPVAL(IXPMGM)
            XMNCHG(IMX) = 0.0D0
            WRITE(FILNAM,FMT='(A3,I3)') 'MON',IMNNUM(IMX,1)
            INQUIRE(FILE=FILNAM,EXIST=LEXIST)
            IF(LEXIST) THEN
               IMNUNT(IMX)=39+IMX
!         PRINT *,'MOONPO: UNIT AND FILNAM ARE: ',IMNUNT(IMX),FILNAM
               OPEN(UNIT=IMNUNT(IMX),FILE=FILNAM,FORM='FORMATTED',      &
     &              BLANK='ZERO',STATUS='OLD')
               READ(IMNUNT(IMX),7700,END=7701) XMNCHG(IMX)
 7700          FORMAT(F10.2)
               GO TO 7702
 7701          CONTINUE
               WRITE(6,*) 'MOONPO: EOF READING FILNAM=',FILNAM
               STOP 69
 7702          CONTINUE
!               WRITE(6,*) 'MOONPO: READ XMNCHG ', XMNCHG(IMX)
            ELSE
               WRITE(6,*) 'MOONPO: ',FILNAM,' DOES NOT EXIST'
               STOP 69
            ENDIF
            IF(ABS(XMNCHG(IMX)).GT.361.D0 ) STOP 16
   10 END DO
         ENDIF
!
         ITERP = NINNER
      DO 20 IMX=IMN,NPVAL(IXPMGM)
!     DO 20 IMX=1,NPVAL(IXPMGM)
         XJDT1(IMX) = XJDT10
         XJDT2(IMX) = XJDT20
         REWIND IMNUNT(IMX)
         READ(IMNUNT(IMX),*)
!
         READ(IMNUNT(IMX),20400,END=9997) IYMD, IHM, SEC,               &
     &      (XKEP1(JJ,IMX),JJ=1,6)
!        WRITE(6,*) 'IYMD IHM SEC ', IYMD, IHM, SEC
!        WRITE(6,*) 'XKEP ', XKEP1
         IHMS = IHM * 100
         CALL YMDTIS( IYMD, IHMS, MJDSEC )
         XJDS1  = DBLE( MJDSEC ) +  SEC
         IF(LDEBUG)WRITE(6,*) 'INITIAL READ MJDSEC, SEC, XKEP1 ',       &
     &                            MJDSEC, SEC, XKEP1
!
         READ(IMNUNT(IMX),20400,END=9997) IYMD, IHM, SEC,               &
     &      (XKEP2(JJ,IMX),JJ=1,6)
!        WRITE(6,*) 'IYMD IHM SEC ', IYMD, IHM, SEC
!        WRITE(6,*) 'XKEP ', XKEP2
         IHMS = IHM * 100
         CALL YMDTIS( IYMD, IHMS, MJDSEC )
         XJDS2  = DBLE( MJDSEC ) +  SEC
         IF(LDEBUG)WRITE(6,*) 'INITIAL READ MJDSEC, SEC, XKEP2 ',       &
     &                            MJDSEC, SEC, XKEP2
         XINT(IMX) = XJDS2 - XJDS1
         IF(LDEBUG)WRITE(6,*) 'INITIAL READ XJDS1, XJDS2, XINT ',       &
     &                            XJDS1,XJDS2,XINT(IMX)
         REWIND IMNUNT(IMX)
         READ(IMNUNT(IMX),*)
   20 END DO
      ENDIF
!
!
!     ....COMPUTE T0 = TIME OF REQUESTED TIME
!
      T0 = DBLE( MJDS ) + FSEC
!
!
!
!     ....DO KEPLERIAN INTERPOLATION HERE
!
!     ....FIND TIMES IN EPHEMERIS WHICH STRADDLE REQUESTED TIME
!
!     ....CHECK TO SEE IF REQUESTED TIME ALREADY IN INTERP INTERVAL
!
      IF(T0.GE.XJDT1(IMN).AND.T0.LT.XJDT2(IMN)) GO TO 3000
!
!     ....READ AND CHECK TIMES WITHOUT CALLING UTCET IN ORDER TO
!     ....GET NEAR THE REQUESTE@ TIME
!     ....DO THIS TO AVOID PROBLEMS IN UTCET FOR TIMES BEFORE
!     ....START TIME OF RUN
!
 2100 READ(IMNUNT(IMN),20400,END=2198) IYMD, IHM, SEC,                  &
     &      (XKEP1(JJ,IMN),JJ=1,6)
      IHMS = IHM * 100
      CALL YMDTIS( IYMD, IHMS, MJDSEC )
      XJDSEC = DBLE( MJDSEC ) +  SEC
      T0DIF = T0 - XJDSEC
      IF (  T0DIF .GE. ZERO .AND. T0DIF .LT. XINT(IMN) ) GO TO 2103
      IF (  T0 .GT. XJDSEC ) GO TO 2100
!
!
!     ....NEAR THE REQUESTED TIME SO CALL UTCET TO FIND TIME
!     ....IN ET TIME
!
      NBACK = ABS( T0DIF ) / XINT(IMN)  + 4
      DO 2105 I=1,NBACK
      BACKSPACE IMNUNT(IMN)
 2105 END DO
         IBACK = IBACK + 1
         IF(IBACK.GT.30 ) THEN
            WRITE(6,*) 'MOONPO: STOP BECAUSE IBACK = ', IBACK
            STOP 16
         ENDIF
      GO TO 2100
!
 2104 CONTINUE
!
         JBACK = JBACK + 2
         IF(JBACK.GT.30) THEN
            WRITE(6,*) 'MOONPO: STOP BECAUSE JBACK = ', JBACK
            STOP 69
         ENDIF
      BACKSPACE IMNUNT(IMN)
      BACKSPACE IMNUNT(IMN)
      READ(IMNUNT(IMN),20400,END=2198) IYMD, IHM, SEC,                  &
     &      (XKEP1(JJ,IMN),JJ=1,6)
      IHMS = IHM * 100
      CALL YMDTIS( IYMD, IHMS, MJDSEC )
 2103 CONTINUE
      CALL UTCET(.TRUE.,1, MJDSEC, SEC, FSECE, AA(KA1UT) )
!
      XJDSEC = DBLE( MJDSEC ) +  FSECE
!
! IF CONVERTING FROM UTC TO ET CAUSES THE EPHEMERIS POINT TO BE AFTER T0
! go to 2104
      IF(XJDSEC.GT.T0) GOTO 2104
!
      XJDT1(IMN) = XJDSEC
!
      READ(IMNUNT(IMN),20400,END=2198) IYMD, IHM, SEC,                  &
     &      (XKEP2(JJ,IMN),JJ=1,6)
!
!
      IHMS = IHM * 100
      CALL YMDTIS( IYMD, IHMS, MJDSEC )
      CALL UTCET(.TRUE.,1, MJDSEC, SEC, FSECE, AA(KA1UT) )
!
      XJDT2(IMN) = DBLE( MJDSEC )  + FSECE
!
!
      GO TO 3000
!
!-------------------------------------------------------------------
!
 2198 CONTINUE
!
!     ....THIS SECTION HANDLES THE CASE OF AN END-OF-FILE READ
!     ....ON THE EPHEMERIS
!     ....SET THE LAST READ POSITION TO XKEP2, BACKSPACE 2 AND
!     ....SET THE NEXT-TO-LAST POSITION TO XKEP1
!     ....THEN EXTRAPOLATE FOR T0 POSITION
!
!
      DO 2190 I=1,6
         XKEP2(I,IMN) = XKEP1(I,IMN)
 2190 END DO
!
      IHMS = IHM * 100
      CALL YMDTIS( IYMD, IHMS, MJDSEC )
      XJDT2(IMN) = DBLE( MJDSEC )  + SEC
!
      BACKSPACE IMNUNT(IMN)
      BACKSPACE IMNUNT(IMN)
      BACKSPACE IMNUNT(IMN)
      READ(IMNUNT(IMN),20400,END=2198) IYMD, IHM, SEC,                  &
     &      (XKEP1(JJ,IMN),JJ=1,6)
!
      IHMS = IHM * 100
      CALL YMDTIS( IYMD, IHMS, MJDSEC )
      XJDSEC = DBLE( MJDSEC ) +  SEC
      XJDT1(IMN) = XJDSEC
!
!
!
!-------------------------------------------------------------------
!
 3000 CONTINUE
!
!     ....COMPUTE INTERPOLATION FOR PHOBOS POSITION
!
      S =  ( T0 - XJDT1(IMN) ) / ( XJDT2(IMN) - XJDT1(IMN) )
      IF(S.LT.ZERO ) THEN
         KBACK = KBACK + 3
         IF(KBACK.GT.30) THEN
            WRITE(6,*) 'MOONPO: STOP BECAUSE KBACK = ', KBACK
            STOP 16
         ENDIF
         BACKSPACE IMNUNT(IMN)
         BACKSPACE IMNUNT(IMN)
         BACKSPACE IMNUNT(IMN)
         GO TO 2100
      ENDIF
!
!     ....INTERPOLATE A, E, I
!
      DO 250 I=1,3
         XKPHO(I) = XKEP1(I,IMN) + S * ( XKEP2(I,IMN) - XKEP1(I,IMN) )
  250 END DO
!
!
!     ....INTERPOLATE NODE AND ARG PERIGEE
!
      DO 260 I=4,5
         IF(XKEP2(I,IMN).LT.XKEP1(I,IMN) ) THEN
            DIF1 = XKEP2(I,IMN) - XKEP1(I,IMN)
            DIF2 = MOD( DIF1 + D360, D360 )
            IF(ABS(DIF1).LT.ABS(DIF2)  ) THEN
               XKPHO(I) = XKEP1(I,IMN) + S * DIF1
            ELSE
               XKPHO(I) = XKEP1(I,IMN) + S * DIF2
            ENDIF
         ELSE
            XKPHO(I) = XKEP1(I,IMN) + S * ( XKEP2(I,IMN) - XKEP1(I,IMN))
         ENDIF
            XKPHO(I) = MOD( XKPHO(I) + D360, D360 )
  260 END DO
!
!      COMPUTE MEAN MOTION IN DEGREES/SEC
!
      XMMOTN=ONE/(SQRT(XKPHO(1)**3/GM)*DEGRAD)
!
!      COMPUTE MEAN ANOMALY FROM MEAN MOTION
!
      XKPHO(6)=XMMOTN*(T0-XJDT1(IMN)) + XKEP1(6,IMN)
!
      XKPHO(6) = XKPHO(6) + XMNCHG(IMN)
!
      IF(XKPHO(6).LT.ZERO ) THEN
         XKPHO(6) = MOD( XKPHO(6) + D360, D360 )
      ELSE
         XKPHO(6) = MOD( XKPHO(6), D360 )
      ENDIF
!
!     ....CONVERT KEPLERIAN ELEMENTS TO CARTESIAN
!
      CALL POSVEL(XPHO,XKPHO,1,GM)
!
!
      RETURN
!
!
 9997 CONTINUE
      WRITE(6,*) 'MOONPO: EOF ON FIRST READ OF EPHEMERIS '
      STOP 16
!
10400 FORMAT(1X,I6,I5,F10.6,3F13.2,3F12.5)
20400 FORMAT(1X,I6,I5,F10.6,F16.3,F15.11,F16.9,3F17.9)
!
      END
