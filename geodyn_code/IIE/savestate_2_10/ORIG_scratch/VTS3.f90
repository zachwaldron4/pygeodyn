!$VTS3
      SUBROUTINE VTS3(ALT,YRD,SLAT,CLAT,TLOC,F107A,F107,MAS ,D,T)
!********1*********2*********3*********4*********5*********6*********7**
! VTS3             08/13/90            9007.1    PGMR - ? (MOD. JJM)
!
!
! FUNCTION:        VENUS ATMOSPHERE MODEL (03/05/81)
!                  returns density and temperature
!
!                  USES LOCAL TIME AND LATITUDE TO ESTIMATE SOLAR ZENITH
!                  ANGLE FOR SYMMETRICAL TERMS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ALT      I    S    Satellite altitude in Km
!   YRD      -    S    Not used
!   SLAT     I    S    Sine of satellite latitude
!   CLAT     I    S    Cosine of satellite latitude
!   TLOC     I    S    Local time in hours
!   F107A    I    S    Average solar flux  (81 day centered average)
!   F107     I    S    Solar flux for Venus at current time
!   MAS      I    S    ? - constant = 48
!   D        O    A    Array of mass and number densitites
!   T        O    A    Temperature at altitude and exospheric temp.
!
! COMMENTS
!
!    Hedin's original comments:
!
!    INPUT VARIABLES
!
!      ALT   - ALTITUDE (KM)
!      SLAT  - SINE OF LATITUDE
!      CLAT  - COSINE OF LATITUDE
!      TLOC  - LOCAL HOUR ANGLE (HRS)
!      MAS   - MASS REQUIRED, 48 FOR ALL
!      YRD            NOT CURRENTLY USED
!      F107A - 3 month avg 10.7
!      F107  - daily 10.7 shifted in phase to venus (but not correcting
!              for change in distance)
!
!    OUTPUT VARIABLES
!
!      D(1) - TOTAL MASS DENSITY (GM/CM3)
!        2  - CO2 NUMBER DENSITY (CM-3)
!        3  - O
!        4  - CO
!        5  - HE
!        6  - N
!        7  - N2
!      T(1) - EXOSPHERIC TEMPERATURE
!        2  - TEMPERATURE AT ALTITUDE
!
!  REFERENCE:
!
!     Hedin, A. E., et al., "Global Empirical Model of the Venus
!            Thermosphere", JGR, Vol. 88, No.A1, pp 73-83, Jan. 1, 1983
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      PARAMETER ( ZERO = 0.0D0 )
      PARAMETER ( D16M24 = 1.66D-24 )
      PARAMETER ( D1M3 = 1.D-3 )
      PARAMETER ( HALF = 0.5D0 )
      PARAMETER ( ONE  = 1.0D0 )
      PARAMETER ( TWO  = 2.D0  )
      PARAMETER ( THREE= 3.D0 )
      PARAMETER ( FOUR = 4.0D0 )
      PARAMETER ( FIVE = 5.D0 )
      PARAMETER ( EIGHT= 8.D00 )
      PARAMETER ( TEN  = 10.D0 )
      PARAMETER ( D14  = 14.D0 )
      PARAMETER ( D15  = 15.D0 )
      PARAMETER ( D16  = 16.D0 )
      PARAMETER ( D20  = 20.D0 )
      PARAMETER ( D28  = 28.D0 )
      PARAMETER ( D30  = 30.D0 )
      PARAMETER ( D35  = 35.D0 )
      PARAMETER ( D44  = 44.D0 )
      PARAMETER ( D63  = 63.D0 )
      PARAMETER ( D70  = 70.D0 )
      PARAMETER ( D105 = 105.D0)
      PARAMETER ( D170 = 170.D0 )
      PARAMETER ( D200 = 200.D0 )
      PARAMETER ( D945 = 945.D0)
!
      DIMENSION D(7),T(2),MT(7)
!
      COMMON/VPARM/PTV(50),PDV(50,6)
      COMMON/VLOWER/PTMV(8),PDMV(8,6)
      COMMON/VVTS1C/TLB,S,DB44,DB16,DB29,DB04,DB14,DB28
      COMMON/VPARMB/GSURFV,REV
      COMMON/VVFIT/ZAA,FRR
!
      DATA IFL/0/,MT/48,44,16,29,4,14,28/
!CC   DATA KENTRY /0/
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!CC   KENTRY = KENTRY + 1
!CC   IF( KENTRY .GT. 20 ) STOP
!
!CC   WRITE(6,*) 'VTS3: ALT,YRD ', ALT, YRD
!CC   WRITE(6,*) 'VTS3: SLAT, CLAT, TLOC ', SLAT,CLAT, TLOC
!CC   WRITE(6,*) 'VTS3: F107A,F107 ', F107A, F107
!CC   WRITE(6,*) 'VTS3: MAS ', MAS
!
      MASS=ABS(MAS)
      TB=0
      IF(MAS.EQ.-48) TB=1
      IF(IFL.EQ.1) GOTO 10
      CALL PARMSV
      IFL=1
   10 CONTINUE
!
!       TEMPERATURE STRUCTURE
!
      TINFV = GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PTV(1))
!
      TINF = PTMV(1)*(ONE+TINFV)*PTV(1)
!CC   WRITE(6,*) 'VTS3: TINFV, TINF ', TINFV, TINF
      T(1)=TINF
      ZA=PTMV(5)*PTV(31)
      ZAA=ZA
      T0=PTMV(3)*PTV(47)
      S=PTMV(4)*PTV(46)
      SP=PTV(49)
!
!      SET TA BETWEEN T0 AND TINF
!
      FR=PTV(16)*(ONE+GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PTV(16)))
!
      FRR=FR
      FR=ONE/(ONE+EXP(-FOUR * (FR - HALF)))
      TA=TINF+FR*(T0-TINF)
      ZM=PTMV(7)*PTV(48)
      AM=PTMV(8)*PDV(31,1)*                                             &
     &        (ONE+GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(31,1)))
!
      IF(MASS.EQ.0) GO TO 50
!
!
      ENTRY VTSBV(D,T)
!
!
      XMR=PDMV(2,2)*PDV(16,2)*(ONE+GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,&
     &   PDV(16,2)) )
      IF(XMR.LT.D1M3) XMR=D1M3
      H1=PDMV(7,2)*PDV(47,2)
      ZHO=PDMV(6,2)*PDV(46,2)
      YMR=XMR*CCORV(PDMV(6,1),PDMV(5,2),H1,ZHO)
!
!       CALCULATE MEAN MASS
!
      XMM=(D44+D44*YMR+PDMV(2,6)*D28)/(ONE+TWO*YMR+PDMV(2,6))
      PDMV(5,1)=XMM
      ZHM=PDMV(4,1)/PDV(31,2)
   13 CONTINUE
      DO 15 J=1,7
      IF(MASS.EQ.MT(J)) GO TO 20
   15 END DO
      GO TO 90
!
!       CO2 DENSITY
!
   20 CONTINUE
      DB44=PDMV(1,1)*                                                   &
     &  EXP(GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(1,1)))*PDV(1,1)
      D(2)=DENSSV(ALT,DB44,TINF,TLB,D44,ZERO,T(2),PTMV(6),S,T0,ZA,      &
     &    TA, ZM, AM, SP)
      ZH44=PDMV(3,1)*PDV(16,1)
!        GET MIXING DENSITY AT ZLB
      XMD=D44-XMM
      B44=DENSSV(ZH44,DB44,TINF,TLB,XMD,-ONE,TZ,PTMV(6),S,T0,ZA,TA,ZM,  &
     &AM,SP)
      DM44=DENSSV(ALT,B44,TINF,TLB,XMM,ZERO,TZ,PTMV(6),S,T0,ZA,TA,ZM,   &
     &   AM,SP)
      D(2)=DNETV(D(2),DM44,ZHM,XMM,D44)
   22 CONTINUE
!
!       O DENSITY
!
   25 DB16=PDMV(1,2)*                                                   &
     &  EXP(GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(1,2)))*PDV(1,2)
      D(3)=DENSSV(ALT,DB16,TINF,TLB,D16,ZERO,T(2),PTMV(6),S,T0,ZA,TA,ZM,&
     &   AM, SP)
      DM16=DM44*XMR
      D(3)=DNETV(D(3),DM16,ZHM,XMM,D16)
      D(3)=D(3)*CCORV(ALT,PDMV(5,2),H1,ZHO)
      IF(MASS.EQ.44 .OR. MASS.EQ.16 .OR. MASS.EQ.04) GO TO 27
!
!        GET O TURBOPAUSE ESTIMATE
!
      DD16=DENSSV(ZH44,DB16,TINF,TLB,D16,ZERO,TZ,PTMV(6),S,T0,ZA,TA,ZM, &
     &   AM, SP)
      DMZ44=DENSSV(ZH44,B44,TINF,TLB,XMM,ZERO,TZ,PTMV(6),S,T0,ZA,TA,ZM, &
     &   AM, SP)
!
      CALL TURBOV(DD16,DMZ44*XMR,ZH44,ZH16,D16,XMM,TZ)
!
      PDMV(3,2)=ZH16
   27 CONTINUE
!
      GO TO (30,90,90,30,35,40,45),J
!
!       CO DENSITY
!
   30 DB29=PDMV(1,3)*                                                   &
     &  EXP(GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(1,3)))*PDV(1,3)
      D(4)=DENSSV(ALT,DB29,TINF,TLB,D28,ZERO,T(2),PTMV(6),S,T0,ZA,TA,ZM,&
     &   AM, SP)
      DM29=DM44*XMR
      D(4)=DNETV(D(4),DM29,ZHM,XMM,D28)
      D(4)=D(4)*CCORV(ALT,PDMV(5,2),H1,ZHO)
      IF(TB.EQ.ZERO) GO TO 32
      DD29=DENSSV(ZH44,DB29,TINF,TLB,D28,ZERO,TZ,PTMV(6),S,T0,ZA,TA,    &
     & ZM,AM,SP)
!
      CALL TURBOV(DD29,DMZ44*XMR,ZH44,PDMV(3,3),D28,XMM,TZ)
!
   32 CONTINUE
      IF(MASS.EQ.29) GO TO 40
      IF(MASS.NE.48) GO TO 90
!
!      HE DENSITY
!
   35 DB04=PDMV(1,4)*                                                   &
     &  EXP(GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(1,4)))*PDV(1,4)
      D(5)=DENSSV(ALT,DB04,TINF,TLB,FOUR,-0.6D0,T(2),PTMV(6),S,T0,ZA,TA,&
     &   ZM,AM, SP)
      DM04=DM44*PDMV(2,4)*PDV(16,4)
      D(5)=DNETV(D(5),DM04,ZHM,XMM,FOUR)
      IF(TB.EQ.ZERO) GO TO 39
      DD04=DENSSV(ZH44,DB04,TINF,TLB,FOUR,-0.6D0,TZ,PTMV(6),S,T0,ZA,TA, &
     & ZM,AM,SP)
!
      CALL TURBOV(DD04,DMZ44*PDMV(2,4)*PDV(16,4),ZH44,PDMV(3,4),FOUR,   &
     &            XMM,TZ)
!
   39 CONTINUE
      IF(MASS.NE.48) GO TO 90
!
!      N DENSITY
!
   40 DB14=PDMV(1,5)*                                                   &
     &  EXP(GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(1,5)))*PDV(1,5)
      D(6)=DENSSV(ALT,DB14,TINF,TLB,D14,ZERO,T(2),PTMV(6),S,T0,ZA,TA,   &
     &   ZM,AM, SP)
      ZH14=ZH16
      PDMV(3,5)=ZH14
      XMD=D14-XMM
      B14=DENSSV(ZH14,DB14,TINF,TLB,XMD,-ONE,TZ,PTMV(6),S,T0,ZA,TA,ZM,  &
     &     AM,SP)
      DM14=DENSSV(ALT,B14,TINF,TLB,XMM,ZERO,TZ,PTMV(6),S,T0,ZA,TA,ZM,   &
     &   AM,SP)
      D(6)=DNETV(D(6),DM14,ZHM,XMM,D14)
      D(6)=D(6)*CCORV(ALT,PDMV(5,5),PDMV(7,5),PDMV(6,5))
   42 CONTINUE
      IF(MASS.NE.48) GO TO 90
!
!       N2 DENSITY
!
   45 DB28=PDMV(1,6)*                                                   &
     &   EXP(GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,PDV(1,6)))*PDV(1,6)
      D(7)=DENSSV(ALT,DB28,TINF,TLB,D28,ZERO,T(2),PTMV(6),S,T0,ZA,TA,ZM,&
     &   AM, SP)
      DM28=DM44*PDMV(2,6)
      D(7)=DNETV(D(7),DM28,ZHM,XMM,D28)
      IF(TB.EQ.ZERO) GO TO 47
      DD28=DENSSV(ZH44,DB28,TINF,TLB,D28,ZERO,TZ,PTMV(6),S,T0,ZA,TA,    &
     & ZM,AM,SP)
!
      CALL TURBOV(DD28,DMZ44*PDMV(2,6),ZH44,PDMV(3,6),D28,XMM,TZ)
!
   47 CONTINUE
      IF(MASS.EQ.28) GO TO 40
      IF(MASS.NE.48) GO TO 90
!
!      TOTAL MASS DENSITY
!
      D(1) = D16M24*(D44*D(2)+D16*D(3)+D28*D(4)+                        &
     & FOUR*D(5)+D14*D(6)+D28*D(7))
!CC   WRITE(6,*) 'VTS3: D(1) ', D(1)
      GO TO 90
   50 DD=DENSSV(ALT,ONE,TINF,TLB,ZERO,ZERO,T(2),PTMV(6),S,T0,ZA,TA,ZM,  &
     &   AM,SP)
!
   90 CONTINUE
      DO 91 I=1,7
!
!      MULTIPLICATION FACTOR FOR ONMS DENSITIES
!
   91 D(I)=D(I)*PDMV(8,1)
!CC   WRITE(6,*) 'VTS3: D, T ', D, T
      RETURN
      END
