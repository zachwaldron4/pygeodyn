!$GLOBE5
      FUNCTION GLOBE5(YRD,SEC,GLAT,GLONG,TLOC,F107A,F107,AP,P)
!********1*********2*********3*********4*********5*********6*********7**
! GLOBE5           00/00/00            0000.0    PGMR - A. HEDIN
!
! FUNCTION:  CALCULATE G(L) FUNCTION FOR MSIS-86/CIRA 1986
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   YRD
!   SEC
!   GLAT
!   GLONG
!   TLOC
!   F107A
!   F107
!   AP
!   P
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( HALF = 0.5D0 )
      PARAMETER ( ONE  = 1.0D0 )
      PARAMETER ( TWO  = 2.0D0 )
      PARAMETER ( THREE  = 3.0D0 )
      PARAMETER ( FOUR  = 4.0D0 )
      PARAMETER ( FIVE = 5.0D0 )
      PARAMETER ( SIX  = 6.0D0 )
      PARAMETER ( D8 = 8.0D0 )
      PARAMETER ( D1OV3 = ONE / THREE )
      PARAMETER ( D1OV4 = ONE / FOUR )
      PARAMETER ( D1OV5 = ONE / FIVE )
      PARAMETER ( D1OV6 = ONE / SIX )
      PARAMETER ( D1OV8 = ONE / D8 )
!
      COMMON/CSW/SW(25),SWC(25),ISW
      COMMON/LPOLY/PLG(9,4),CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,    &
     &             DAY,DF,DFA,APD,APDF,APT(4),IYR
      COMMON/TTTEST/TINFG,GB,ROUT,TTT(15)
!
      DIMENSION P(150),SV(25),AP(7)
!
      DATA DGTR/1.74533D-2/,DR/1.72142D-2/, XL/1000.D0/,TLL/1000.D0/    &
     &  DAYL/-1.D0/,P14/-1000.D0/,P18/-1000.D0/,P32/-1000.D0/           &
     &  HR/0.2618D0/,SR/7.2722D-5/,SV/25*1.D0/,NSW/14/,P39/-1000.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! Eq. A24d
!
      G0(A)=(A-4.D0+(P(26)-1.D0)*(A-4.D0+(EXP(-ABS(P(25))               &
     &     *(A-4.D0))-1.D0)/ ABS(P(25))))
! Eq. A24c
!
      SUMEX(EX)=1.D0+(1.D0-EX**19)/(1.D0-EX)*EX**(0.5D0)
!
! Eq. A24a.  Uses 3-hourly Ap values.
!
!     SG0(EX)=(G0(AP(2))+(G0(AP(3))*EX+G0(AP(4))*EX*EX+G0(AP(5))*EX**3
!    $ +(G0(AP(6))*EX**4+G0(AP(7))*EX**12)*(1.D0-EX**8)/(1.D0-EX)))
!    $ /SUMEX(EX)
!
      SG0(EX)=                                                          &
     & ( G0(AP(2))+                                                     &
     &   ( EX * ( EX * ( EX * G0(AP(5))+G0(AP(4)) ) + G0(AP(3)) )       &
     & +(G0(AP(6))+G0(AP(7))*EX**8)*EX**4 *                             &
     &  (1.D0-EX**8)/(1.D0-EX) ) ) /SUMEX(EX)
!
!**********************************************************************
!* STARP OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!     write(6,*) ' entry GLOBE5 yrd ', yrd
!     write(6,*) ' entry GLOBE5 sec ', sec
!     write(6,*) ' entry GLOBE5 glat ', glat
!     write(6,*) ' entry GLOBE5 glong ', glong
!     write(6,*) ' entry GLOBE5 tloc ', tloc
!     write(6,*) ' entry GLOBE5 f107a ', f107a
!     write(6,*) ' entry GLOBE5 f107 ', f107
!     write(6,*) ' entry GLOBE5 ap ', ap
!     write(6,*) ' entry GLOBE5 p ', p
!
      IF(ISW.NE.64999) CALL TSELEC86(SV)
      TTT(10) = 0.D0
      TTT(11) = 0.D0
      TTT(12) = 0.D0
      TTT(13)=0.D0
   10 CONTINUE
      IYR = YRD/1000.D0
      DAY = YRD - IYR*1000.D0
!
! Eq. A22 (remainder of code)
!
      IF(XL.EQ.GLAT)  GO TO 15
! CALCULATE LEGENDRE POLYNOMIALS
      C = SIN(GLAT*DGTR)
      S = COS(GLAT*DGTR)
      C2 = C*C
      C4 = C2*C2
      S2 = S*S
      PLG(2,1) = C
      PLG(3,1) = HALF *(THREE*C2 -ONE )
!     PLG(4,1) = 0.5D0*(FIVE*C*C2-THREE*C)
      PLG(4,1) = HALF *(FIVE*C2-THREE)*C
      PLG(5,1) = (35.D0*C4 - 30.D0*C2 + THREE) * D1OV8
!     PLG(6,1) = (63.D0*C2*C2*C - 70.D0*C2*C + 15.D0*C)/8.D0
      PLG(6,1) = ((63.D0*C2 - 70.D0)*C2 + 15.D0)*C * D1OV8
      PLG(7,1) = (11.D0*C*PLG(6,1) - FIVE*PLG(5,1)) * D1OV6
      PLG(2,2) = S
      PLG(3,2) = THREE * C * S
      PLG(4,2) = 1.5D0*(5.D0*C2-ONE )*S
!     PLG(5,2) = 2.5D0*(7.D0*C2*C-3.D0*C)*S
      PLG(5,2) = 2.5D0*(7.D0*C2-THREE)*C*S
      PLG(6,2) = 1.875D0*(21.D0*C4 - 14.D0*C2 +ONE )*S
      PLG(7,2) = (11.D0*C*PLG(6,2)-SIX*PLG(5,2)) * D1OV5
      PLG(3,3) = THREE*S2
      PLG(4,3) = 15.D0*S2*C
      PLG(5,3) = 7.5D0*(7.D0*C2 -ONE )*S2
      PLG(6,3) = THREE*C*PLG(5,3)-TWO*PLG(4,3)
      PLG(7,3)=(11.D0*C*PLG(6,3)-7.D0*PLG(5,3))* D1OV4
      PLG(8,3)=(13.D0*C*PLG(7,3)-D8  *PLG(6,3))*D1OV5
      PLG(4,4) = 15.D0*S2*S
      PLG(5,4) = 105.D0*S2*S*C
      PLG(6,4)=(9.D0*C*PLG(5,4)-7.D0*PLG(4,4))* HALF
      PLG(7,4)=(11.D0*C*PLG(6,4)-D8*PLG(5,4))*D1OV3
      XL=GLAT
   15 CONTINUE
      IF(TLL.EQ.TLOC)   GO TO 16
      HRTLOC = HR * TLOC
      HRTLO2 = TWO * HRTLOC
      HRTLO3 = THREE * HRTLOC
      STLOC = SIN(HRTLOC)
      CTLOC = COS(HRTLOC)
      S2TLOC = SIN(HRTLO2)
      C2TLOC = COS(HRTLO2)
      S3TLOC = SIN(HRTLO3)
      C3TLOC = COS(HRTLO3)
      TLL = TLOC
   16 CONTINUE
      IF(DAY.NE.DAYL.OR.P(14).NE.P14) THEN
         CD14=COS(DR*(DAY-P(14)))
         C2D14=COS(TWO * DR*(DAY-P(14)))
      ENDIF
      IF(DAY.NE.DAYL.OR.P(18).NE.P18) CD18=COS(TWO*DR*(DAY-P(18)))
      IF(DAY.NE.DAYL.OR.P(32).NE.P32) CD32=COS(DR*(DAY-P(32)))
      IF(DAY.NE.DAYL.OR.P(39).NE.P39) CD39=COS(TWO*DR*(DAY-P(39)))
      DAYL = DAY
      P14 = P(14)
      P18 = P(18)
      P32 = P(32)
      P39 = P(39)
!         F10.7 EFFECT
      DF = F107 - F107A
      DFA=F107A-150.D0
!     TTT(1) = P(20)*DF + P(21)*DF*DF + P(22)*DFA
!    $ + P(30)*DFA**2
      TTT(1) = ( P(20) + P(21)*DF )*DF + ( P(22) + P(30)*DFA ) *DFA
      TEMP1 = (P(20)*DF+P(21)*DF*DF)
!     F1 = 1.D0 + (P(48)*DFA +P(20)*DF+P(21)*DF*DF)*SWC(1)
!     F2 = 1.D0 + (P(50)*DFA +P(20)*DF+P(21)*DF*DF)*SWC(1)
      F1 = ONE  + P(48)*DFA  + TEMP1
      F2 = ONE  + P(50)*DFA  + TEMP1
!
!        TIME INDEPENDENT
      TTT(2) =                                                          &
     &  (P(2)*PLG(3,1) + P(3)*PLG(5,1)+P(23)*PLG(7,1))                  &
     & +(P(15)*PLG(3,1))*DFA                                            &
     & +P(27)*PLG(2,1)
!
!        SYMMETRICAL ANNUAL
      TTT(3) =                                                          &
     & (P(19) )*CD32
!
!        SYMMETRICAL SEMIANNUAL
      TTT(4) =                                                          &
     & (P(16)+P(17)*PLG(3,1))*CD18
!
!        ASYMMETRICAL ANNUAL
      TTT(5) = F1*                                                      &
     &  (P(10)*PLG(2,1) + P(11)*PLG(4,1))*CD14
!
!         ASYMMETRICAL SEMIANNUAL
      TTT(6) =  P(38)*PLG(2,1)*CD39
!
!        DIURNAL
      TEMP1 = CD14
      T71 = (P(12)*PLG(3,2) + P(36)*PLG(2,2))*TEMP1
      T72 = (P(13)*PLG(3,2) + P(37)*PLG(2,2))*TEMP1
      TTT(7) = F2*                                                      &
     & ((P(4)*PLG(2,2) + P(5)*PLG(4,2) + P(28)*PLG(6,2)                 &
     & + T71)*CTLOC                                                     &
     & + (P(7)*PLG(2,2) + P(8)*PLG(4,2) +P(29)*PLG(6,2)                 &
     & + T72)*STLOC)
!
!        SEMIDIURNAL
      T81 = (P(24)*PLG(4,3))*TEMP1
      T82 = (P(34)*PLG(4,3))*TEMP1
      TTT(8) = F2*                                                      &
     & ((P(6)*PLG(3,3) + P(42)*PLG(5,3) + T81)*C2TLOC                   &
     & +(P(9)*PLG(3,3) + P(43)*PLG(5,3) + T82)*S2TLOC)
!
!        TERDIURNAL
      TTT(14) = F2*                                                     &
     & ((P(40)*PLG(4,4)+(P(94)*PLG(5,4)+P(47)*PLG(7,4))*TEMP1)*         &
     & S3TLOC                                                           &
     & +(P(41)*PLG(4,4)+(P(95)*PLG(5,4)+P(49)*PLG(7,4))*TEMP1)*         &
     & C3TLOC)
!
!          MAGNETIC ACTIVITY BASED ON DAILY AP
      IF(SW(9).EQ.-1.D0 .AND. P(52).NE.0.D0) GO TO 30
      APD=(AP(1)-FOUR)
      P44=P(44)
      P45=P(45)
      IF(P44.LT.0) P44=1.D-5
      APDF = (APD+(P45-ONE)*(APD+(EXP(-P44  *APD)-ONE)/P44  ))
      TTT(9)=APDF*(P(33)+P(46)*PLG(3,1)+P(35)*PLG(5,1)+                 &
     & (P(101)*PLG(2,1)+P(102)*PLG(4,1)+P(103)*PLG(6,1))*TEMP1+         &
     & (P(122)*PLG(2,2)+P(123)*PLG(4,2)+P(124)*PLG(6,2))*               &
     & COS(HR*(TLOC-P(125))))
      GO TO 40
   30 CONTINUE
      EXP1 = EXP(-10800.D0*ABS(P(52))/(ONE+P(139)*(45.D0-ABS(GLAT))))
      IF(EXP1.GT..99999D0) EXP1=.99999D0
      EXP2 = EXP(-10800.D0*ABS(P(54)))
      IF(EXP2.GT..99999D0) EXP2=.99999D0
      IF(P(25).LT.1.D-4) P(25)=1.D-4
      APT(1)=SG0(EXP1)
      APT(3)=SG0(EXP2)
!
      TTT(9) = APT(1)*(P(51)+P(97)*PLG(3,1)+P(55)*PLG(5,1)+             &
     & (P(126)*PLG(2,1)+P(127)*PLG(4,1)+P(128)*PLG(6,1))*TEMP1+         &
     & (P(129)*PLG(2,2)+P(130)*PLG(4,2)+P(131)*PLG(6,2))*               &
     & COS(HR*(TLOC-P(132))))
   40 CONTINUE
!
      IF(GLONG.LE.-1000.D0) GO TO 49
!        LONGITUDINAL
      TTT(11)= (ONE +P(90)*PLG(2,1))*(ONE+P(81)*DFA)*                   &
     &((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,2)                    &
     & +P(104)*PLG(2,2)+P(105)*PLG(4,2)+P(106)*PLG(6,2)                 &
     & +TEMP1*(P(110)*PLG(2,2)+P(111)*PLG(4,2)+P(112)*PLG(6,2)))*       &
     &     COS(DGTR*GLONG)                                              &
     & +(P(91)*PLG(3,2)+P(92)*PLG(5,2)+P(93)*PLG(7,2)                   &
     & +P(107)*PLG(2,2)+P(108)*PLG(4,2)+P(109)*PLG(6,2)                 &
     & +TEMP1*(P(113)*PLG(2,2)+P(114)*PLG(4,2)+P(115)*PLG(6,2)))*       &
     &  SIN(DGTR*GLONG))
!        UT AND MIXED UT,LONGITUDE
      TTT(12)=(ONE+P(96)*PLG(2,1))*(ONE+P(82)*DFA)*                     &
     &(1.D0+P(120)*PLG(2,1)*TEMP1)*                                     &
     &((P(69)*PLG(2,1)+P(70)*PLG(4,1)+P(71)*PLG(6,1))*                  &
     &     COS(SR*(SEC-P(72))))
      TTT(12)=TTT(12)+                                                  &
     & (P(77)*PLG(4,3)+P(78)*PLG(6,3)+P(79)*PLG(8,3))*                  &
     &     COS(SR*(SEC-P(80))+2.D0*DGTR*GLONG)*(1.D0+P(138)*DFA)
!        UT,LONGITUDE MAGNETIC ACTIVITY
      IF(SW(9).EQ.-1.D0 .AND. P(52).NE.0.D0) GO TO 45
      TTT(13)= APDF*(ONE+P(121)*PLG(2,1))*                              &
     &((P( 61)*PLG(3,2)+P( 62)*PLG(5,2)+P( 63)*PLG(7,2))*               &
     &     COS(DGTR*(GLONG-P( 64))))                                    &
     & +APDF*                                                           &
     & (P(116)*PLG(2,2)+P(117)*PLG(4,2)+P(118)*PLG(6,2))*               &
     &     CD14*COS(DGTR*(GLONG-P(119)))                                &
     & + APDF*                                                          &
     & (P( 84)*PLG(2,1)+P( 85)*PLG(4,1)+P( 86)*PLG(6,1))*               &
     &     COS(SR*(SEC-P( 76)))
      GOTO 48
   45 CONTINUE
      TTT(13)=APT(1)*(ONE+P(133)*PLG(2,1))*                             &
     &((P(53)*PLG(3,2)+P(99)*PLG(5,2)+P(68)*PLG(7,2))*                  &
     &     COS(DGTR*(GLONG-P(98))))                                     &
     & +APT(1)*                                                         &
     & (P(134)*PLG(2,2)+P(135)*PLG(4,2)+P(136)*PLG(6,2))*               &
     &     CD14*COS(DGTR*(GLONG-P(137)))                                &
     & +APT(1)*                                                         &
     & (P(56)*PLG(2,1)+P(57)*PLG(4,1)+P(58)*PLG(6,1))*                  &
     &     COS(SR*(SEC-P(59)))
   48 CONTINUE
!  PARMS NOT USED: 60,83,100,140-150
   49 TINF = 0.D0
      IF( SW(9) .EQ. -1.0D0 )TINF=P(31)
      DO 50 I = 1,NSW
!ccc50 TINF = TINF + ABS(SW(I))*TTT(I)
      TINF = TINF + TTT(I)
   50 END DO
      GLOBE5 = TINF
      RETURN
      END
