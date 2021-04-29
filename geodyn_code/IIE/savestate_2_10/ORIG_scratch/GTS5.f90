!$GTS5
      SUBROUTINE GTS5(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
!********1*********2*********3*********4*********5*********6*********7**
! GTS5             00/00/00            0000.0    PGMR - A. HEDIN
!
! FUNCTION:   MSIS-86/CIRA 1986 NEUTRAL THERMOSPHERE MODEL
!             A.E.HEDIN 3/15/85;2/26/87 (VARIABLE NAMES SHORTENED)
!             10/14/87 INCREASE ALTITUDE LIMIT OF O MIXING CALCULATION
!             ALTL(2) FROM 300.0 TO 400.0 KM .
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IYD      I         YEAR AND DAY AS YYDDD
!   SEC      I         UT(SEC)
!   ALT      I         ALTITUDE(KM) (GREATER THAN 85 KM)
!   GLAT     I         GEODETIC LATITUDE(DEG)
!   GLONG    I         GEODETIC LONGITUDE(DEG)
!   STL      I         LOCAL APPARENT SOLAR TIME(HRS)
!   F107A    I         3 MONTH AVERAGE OF F10.7 FLUX
!   F107     I         DAILY F10.7 FLUX FOR PREVIOUS DAY
!   AP -     I         MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
!                      ARRAY CONTAINING:
!                        (1) DAILY AP
!                        (2) 3 HR AP INDEX FOR CURRENT TIME
!                        (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
!                        (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
!                        (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
!                        (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM
!                            12 TO 33 HRS PR TO CURRENT TIME
!                        (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM
!                            36 TO 59 HRS PR TO CURRENT TIME
!   MASS     I         MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
!                      CALCULATED. MASS 0 IS TEMPERATURE.
!                      MASS 48 FOR ALL.
!   D        O         D(1) - HE NUMBER DENSITY(CM-3)
!                      D(2) - O NUMBER DENSITY(CM-3)
!                      D(3) - N2 NUMBER DENSITY(CM-3)
!                      D(4) - O2 NUMBER DENSITY(CM-3)
!                      D(5) - AR NUMBER DENSITY(CM-3)
!                      D(6) - TOTAL MASS DENSITY(GM/CM3)
!                      D(7) - H NUMBER DENSITY(CM-3)
!                      D(8) - N NUMBER DENSITY(CM-3)
!   T                  T(1) - EXOSPHERIC TEMPERATURE
!                      T(2) - TEMPERATURE AT ALT
!
! COMMENTS:
!      TO GET OUTPUT IN M-3 and KG/M3:   CALL METERS(.TRUE.)
!
!          ADDITIONAL COMMENTS
!           (1) LOWER BOUND QUANTITIES NO LONGER IN COMMON/GTS3C/ FOR
!               GEODYN II VERSION, BECAUSE THIS COMMON BLOCK IS NOT
!               NEEDED.  (2/13/90)
!           (2) TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SW)
!               WHERE SW IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1.
!               FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
!               FOR THE FOLLOWING VARIATIONS
!               1 - F10.7 EFFECT ON MEAN  2 - TIME INDEPENDENT
!               3 - SYMMETRICAL ANNUAL    4 - SYMMETRICAL SEMIANNUAL
!               5 - ASYMMETRICAL ANNUAL   6 - ASYMMETRICAL SEMIANNUAL
!               7 - DIURNAL               8 - SEMIDIURNAL
!               9 - DAILY AP             10 - ALL UT/LONG EFFECTS
!              11 - LONGITUDINAL         12 - UT AND MIXED UT/LONG
!              13 - MIXED AP/UT/LONG     14 - TERDIURNAL
!              15 - DEPARTURES FROM DIFFUSIVE EQUILIBRIUM
!              16 - ALL TINF VAR         17 - ALL TLB VAR
!              18 - ALL T0 VAR           19 - ALL S VAR
!              20 - ALL Z0 VAR           21 - ALL NLB VAR
!              22 - ALL TR12 VAR         23 - TURBO SCALE HEIGHT VAR
!
!              To get current values of SW: CALL TRETRV86(SW)
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
!      LOGICAL LMETER
!
      COMMON/CSW/SW(25),SWC(25),ISW
      COMMON/LOWER5/PTM(8),PDM(8,7)
      COMMON/PARM5/PT(150),PD(150,7),PS(150),PDL(25,2)
      COMMON/TTTEST/TINFG,GB,ROUT,TTT(15)
!
      DIMENSION D(8),T(2),MT(10),AP(7),ALTL(8)
!
      DATA MT/48,0,4,16,28,32,40,1,49,14/,IFL/0/
      DATA ALTL/200.D0,400.D0,150.D0,200.D0,240.D0,450.D0,320.D0,450.D0/
      DATA IMR/0/
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      IF(IFL.EQ.0) THEN
!        PRINT 601
!  601   FORMAT(/,2X,'==> READING IN MSIS MODEL COEFFICIENTS.')
!        CALL PRMSG5
         IFL=1
!        WRITE(6,*) 'GTS5: SW VALUES ', SW
!
         ZA=PTM(5)*PDL(16,2)
         XMM=PDM(5,3)
         ZH28=PDM(3,3)
         ZHM28=PDM(4,3)*PDL(6,2)
         XMD=28.0D0-XMM
         ZH04=PDM(3,1)
         ZHM04=ZHM28
         ZC04=PDM(5,1)*PDL(1,2)
         HC04=PDM(6,1)*PDL(2,2)
         ZH16=PDM(3,2)
         ZHM16=ZHM28
         HC16=PDM(6,2)*PDL(4,2)
         ZC16=PDM(5,2)*PDL(3,2)
         HCC16=PDM(8,2)*PDL(14,2)
         ZCC16=PDM(7,2)*PDL(13,2)
         RC16=PDM(4,2)*PDL(15,2)
         ZH32=PDM(3,4)
         ZHM32=ZHM28
         HC32=PDM(6,4)*PDL(8,2)
         ZC32=PDM(5,4)*PDL(7,2)
         ZH40=PDM(3,5)
         ZHM40=ZHM28
         HC40=PDM(6,5)*PDL(10,2)
         ZC40=PDM(5,5)*PDL(9,2)
         ZH01=PDM(3,6)
         ZHM01=ZHM28
         HC01=PDM(6,6)*PDL(12,2)
         ZC01=PDM(5,6)*PDL(11,2)
         HCC01=PDM(8,6)*PDL(20,2)
         ZCC01=PDM(7,6)*PDL(19,2)
         RC01=PDM(4,6)*PDL(21,2)
         ZH14=PDM(3,7)
         ZHM14=ZHM28
         HC14=PDM(6,7)*PDL(2,1)
         ZC14=PDM(5,7)*PDL(1,1)
         HCC14=PDM(8,7)*PDL(5,1)
         ZCC14=PDM(7,7)*PDL(4,1)
         RC14=PDM(4,7)*PDL(6,1)
      ENDIF
!
      YRD=IYD
!       Eq. A7
      TINF=PTM(1)*(1.D0+GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,       &
     & AP,PT))*PT(1)
!       Eq. A9
      T0=PTM(3)*PD(76,3)*(1.D0+GLOB5L(PD(76,3)))
!       Eq. A8
      TLB=PTM(2)*(1.D0+GLOB5L(PD(26,3)))*PD(26,3)
!       Eq. A10
      Z0=PTM(7)*(1.D0+GLOB5L(PD(51,3)))*PD(51,3)
!       Eq. A6
      G0=PTM(4)*PS(1)                                                   &
     & *(1.D0+GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,                 &
     & AP,PS))
!       Eq. A5
      S=G0/(TINF-TLB)
!       Eq. A11
      TR12=PD(101,3)*(1.D0+GLOB5L(PD(101,3)))
      T(1)=TINF
!CCCC IF(MASS.EQ.0) GO TO 50
!       Eq. A18  N2
      G28=GLOB5L(PD(1,3))
      YRD=IYD
      T(1)=TINF
!
!* CHANGE BY J. RIDGWAY, 2/22/90:  Since mass is set = 48 in calling
!* routine, it need not be checked in "DO 10" loop. COMMENT OUT.
!*    DO 10 J = 1,10
!*    IF(MASS.EQ.MT(J))   GO TO 15
!* 10 CONTINUE
!*    WRITE(6,100) MASS
!*    GO TO 90
!     ....also comment out this test which is always false if mass=48
!**   15 IF(ALT.GT.ALTL(6).AND.MASS.NE.28.AND.MASS.NE.48) GO TO 17
!
!       **** N2 DENSITY ****
!
!       Eq. A18
      DB28 = PDM(1,3)*EXP(G28)*PD(1,3)
!       Eq. A13 - A17
      D(3)=DENSS(ALT,DB28,TINF,TLB, 28.D0,0.D0,T(2),PTM(6),S,T0,        &
     &      ZA,Z0,TR12)
      DD=D(3)
!       Eq. A19
      B28=DENSS(ZH28,DB28,TINF,TLB,XMD,-1.D0,TZ,PTM(6),S,T0,ZA,Z0,TR12)
      IF(ALT.GT.ALTL(3)) GO TO 17
      DM28=DENSS(ALT,B28,TINF,TLB,XMM,0.D0,TZ,PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(3)=DNET86(D(3),DM28,ZHM28,XMM,28.D0)
   17 CONTINUE
!CCC  GO TO (20,50,20,25,90,35,40,45,25,48),  J
   20 CONTINUE
!
!       **** HE DENSITY ****
!
!       Eq. A18
      G4 = GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,1))
      DB04 = PDM(1,1)*EXP(G4)*PD(1,1)
!       Eq. A13 - A17
      D(1)=DENSS(ALT,DB04,TINF,TLB, 4.D0,-.4D0,T(2),PTM(6),S,T0,        &
     &           ZA,Z0,TR12)
      DD=D(1)
      IF(ALT.GT.ALTL(1)) GO TO 24
!       Eq. A19
      B04=DENSS(ZH04,DB04,TINF,TLB,4.D0-XMM,-1.4D0,                     &
     &  T(2),PTM(6),S,T0,ZA,Z0,TR12)
      DM04=DENSS(ALT,B04,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(1)=DNET86(D(1),DM04,ZHM04,XMM,4.D0)
!       Eq. A20b
      RL=LOG(B28*PDM(2,1)/B04)
!       Eq. A20a
      D(1)=D(1)*CCOR86(ALT,RL,HC04,ZC04)
   24 CONTINUE
!CCCC IF(MASS.NE.48)   GO TO 90
   25 CONTINUE
!
!      **** O DENSITY ****
!
!       Eq. A18
      G16= GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,2))
      DB16 =  PDM(1,2)*EXP(G16)*PD(1,2)
!       Eq. A13 - A17
      D(2)=DENSS(ALT,DB16,TINF,TLB, 16.D0,0.D0,T(2),PTM(6),S,T0,        &
     &           ZA,Z0,TR12)
      DD=D(2)
      IF(ALT.GT.ALTL(2)) GO TO 34
!  Corrected from PDM(3,1) to PDM(3,2)  12/2/85
!       Eq. A19
      B16=DENSS(ZH16,DB16,TINF,TLB,16-XMM,-1.D0,                        &
     &  T(2),PTM(6),S,T0,ZA,Z0,TR12)
      DM16=DENSS(ALT,B16,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(2)=DNET86(D(2),DM16,ZHM16,XMM,16.D0)
!       Eq. A20b
      RL=LOG(B28*PDM(2,2)*ABS(PDL(17,2))/B16)
!       Eq. A20a
      D(2)=D(2)*CCOR86(ALT,RL,HC16,ZC16)
!       Eq. A21
      D(2)=D(2)*CCOR86(ALT,RC16,HCC16,ZCC16)
   34 CONTINUE
!CCCC IF(MASS.NE.48 .AND. MASS.NE.49) GO TO 90
   35 CONTINUE
!
!       **** O2 DENSITY ****
!
!       Eq. A18
      G32= GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,4))
      DB32 = PDM(1,4)*EXP(G32)*PD(1,4)
!       Eq. A13 - A17
      D(4)=DENSS(ALT,DB32,TINF,TLB, 32.D0,0.D0,T(2),PTM(6),S,T0,        &
     &           ZA,Z0,TR12)
!CCCC IF(MASS.EQ.49) THEN
!CCCC    DD=DD+2.D0*D(4)
!CCCC ELSE
         DD=D(4)
!CCCC ENDIF
      IF(ALT.GT.ALTL(4)) GO TO 39
!       Eq. A19
      B32=DENSS(ZH32,DB32,TINF,TLB,32.D0-XMM,-1.D0,                     &
     &  T(2),PTM(6),S,T0,ZA,Z0,TR12)
      DM32=DENSS(ALT,B32,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(4)=DNET86(D(4),DM32,ZHM32,XMM,32.D0)
!       Eq. A20b
      RL=LOG(B28*PDM(2,4)/B32)
!       Eq. A20a
      D(4)=D(4)*CCOR86(ALT,RL,HC32,ZC32)
   39 CONTINUE
!CCCC IF(MASS.NE.48)   GO TO 90
   40 CONTINUE
!
!       **** AR DENSITY ****
!
!       Eq. A18
      G40= GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,5))
      DB40 = PDM(1,5)*EXP(G40)*PD(1,5)
!       Eq. A13 - A17
      D(5)=DENSS(ALT,DB40,TINF,TLB, 40.D0,0.D0,T(2),PTM(6),S,T0,ZA,Z0,  &
     &           TR12)
      DD=D(5)
      IF(ALT.GT.ALTL(5)) GO TO 44
!       Eq. A19
      B40=DENSS(ZH40,DB40,TINF,TLB,40.D0-XMM,-1.D0,                     &
     &  T(2),PTM(6),S,T0,ZA,Z0,TR12)
      DM40=DENSS(ALT,B40,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(5)=DNET86(D(5),DM40,ZHM40,XMM,40.D0)
!       Eq. A20b
      RL=LOG(B28*PDM(2,5)/B40)
!       Eq. A20a
      D(5)=D(5)*CCOR86(ALT,RL,HC40,ZC40)
   44 CONTINUE
!CCCC IF(MASS.NE.48)   GO TO 90
   45 CONTINUE
!
!        **** HYDROGEN DENSITY ****
!
!       Eq. A18
      G1 = GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,6))
      DB01 = PDM(1,6)*EXP(G1)*PD(1,6)
!       Eq. A13 - A17
      D(7)=DENSS(ALT,DB01,TINF,TLB,1.D0,-.4D0,T(2),PTM(6),S,T0,         &
     &           ZA,Z0,TR12)
      DD=D(7)
      IF(ALT.GT.ALTL(7)) GO TO 47
!       Eq. A19
      B01=DENSS(ZH01,DB01,TINF,TLB,1.D0-XMM,-1.4D0,                     &
     &  T(2),PTM(6),S,T0,ZA,Z0,TR12)
      DM01=DENSS(ALT,B01,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(7)=DNET86(D(7),DM01,ZHM01,XMM,1.D0)
!       Eq. A20b
      RL=LOG(B28*PDM(2,6)*ABS(PDL(18,2))/B01)
!       Eq. A20a
      D(7)=D(7)*CCOR86(ALT,RL,HC01,ZC01)
!       Eq. A21
      D(7)=D(7)*CCOR86(ALT,RC01,HCC01,ZCC01)
   47 CONTINUE
   48 CONTINUE
!
!        **** ATOMIC NITROGEN DENSITY ****
!
!       Eq. A18
      G14 = GLOBE5(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,7))
      DB14 = PDM(1,7)*EXP(G14)*PD(1,7)
!       Eq. A13 - A17
      D(8)=DENSS(ALT,DB14,TINF,TLB,14.D0,0.D0,T(2),PTM(6),S,T0,         &
     &           ZA,Z0,TR12)
      DD=D(8)
      IF(ALT.GT.ALTL(8)) GO TO 49
!       Eq. A19
      B14=DENSS(ZH14,DB14,TINF,TLB,14.D0-XMM,-1.D0,                     &
     &  T(2),PTM(6),S,T0,ZA,Z0,TR12)
      DM14=DENSS(ALT,B14,TINF,TLB,XMM,0.D0,T(2),PTM(6),S,T0,ZA,Z0,TR12)
!       Eq. A12
      D(8)=DNET86(D(8),DM14,ZHM14,XMM,14.D0)
!       Eq. A20b
      RL=LOG(B28*PDM(2,7)*ABS(PDL(3,1))/B14)
!       Eq. A20a
      D(8)=D(8)*CCOR86(ALT,RL,HC14,ZC14)
!       Eq. A21
      D(8)=D(8)*CCOR86(ALT,RC14,HCC14,ZCC14)
   49 CONTINUE
!CCCC IF(MASS.NE.48) GO TO 90
!
!       TOTAL MASS DENSITY
!
      D(6) = 1.66D-24*(4.D0*D(1)+16.D0*D(2)+28.D0*D(3)                  &
     &        +32.D0*D(4)+40.D0*D(5)+                                   &
     &         D(7)+14.D0*D(8))
      DB48=1.66D-24*(4.D0*DB04+16.D0*DB16+28.D0*DB28                    &
     &     +32.D0*DB32+40.D0*DB40+DB01+                                 &
     &        14.D0*DB14)
      GO TO 90
!CC  50 DDUM  = DENSS(ALT,1.D0, TINF,TLB,0.D0,0.D0,T(2),PTM(6),
!CC    &              S,T0,ZA,Z0,TR12)
!CC   GO TO 90
   90 CONTINUE
      IF(IMR.EQ.1) THEN
        DO 95 I=1,8
          D(I)=D(I)*1.D6
   95   CONTINUE
        D(6)=D(6)/1000.D0
      ENDIF
      RETURN
  100 FORMAT(1X,'MASS', I5, '  NOT VALID')
!     ENTRY METERS(LMETER)
!     IMR=0
!     IF(LMETER) IMR=1
      END
