!$DENSSV
      FUNCTION DENSSV(ALT,DLB,TINF,TLB,XM,ALPHA,TZ,ZLB,S2,T0,ZA,TA,     &
     &                ZM,AM, SP)
!
!     ....CALLED BY VTS3 FOR VENUS ATMOSPHERE MODEL OF HEDIN
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      PARAMETER ( ZERO = 0.0D0 )
      PARAMETER ( D16M24 = 1.66D-24 )
      PARAMETER ( D1M3 = 1.D-3 )
      PARAMETER ( D1M4 = 1.D-4 )
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
!        CALCULATE DENSITY (12/12/80)
!         TLB CALCULATED FROM TA
!
      COMMON/VPARMB/GSURFV,REV
      COMMON/VFIT/TAF
!
      DATA RGAS/831.4D0/
!
!     ....INLINE FUNCTION
      ZETA(ZZ)=(ZZ-ZLB)*(REV+ZLB)/(REV+ZZ)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!CC   WRITE(6,*) 'DENSSV: INPUT VALUES '
!CC   WRITE(6,*) ALT,DLB,TINF,TLB,XM,ALPHA,TZ,ZLB,S2,T0,ZA,TA,ZM,AM,
!CC  & SP
      DENSSV=DLB
        TAF=(TA-T0)/(TINF-T0)
        IF(TAF.GT.0.AND.TAF.LT.ONE)   GO TO 7
!CCC    WRITE(6,110) TA,TINF,TLB,S2,ALT,ZA,T0
          IF(TAF.LE.ZERO)TA=T0+ D1M4 *(TINF-T0)
          IF(TAF.GE.ONE)TA=TINF-D1M4 *(TINF-T0)
    7   CONTINUE
!
!CC   WRITE(6,*) 'DENSSV: TA,TINF ', TA, TINF
!CC   WRITE(6,*) 'DENSSV: TLB,S2 ', TLB, S2
!CC   WRITE(6,*) 'DENSSV: ALT,ZA ', ALT, ZA
!CC   WRITE(6,*) 'DENSSV: T0 ', T0
!
  110   FORMAT(' DENSSV ',1P,7D11.2)
    5   Z=MAX(ALT,ZA)
        ZG2=ZETA(Z)
        ZGA=ZETA(ZA)
      ZG=ZETA(ALT)
        TLB=TINF-(TINF-TA)*EXP(S2*ZGA)
        T2=TINF-(TINF-TLB)*EXP(-S2*ZG2)
        TT=T2
        IF(ALT.GE.ZA) GO TO 10
          S1=-S2*(TINF-TA)/(TA-T0)
          ZG1=ZG-ZGA
!           CALCULATE TEMPERATURE BELOW ZA
          IF(S1*ZG1.LT. TEN) GO TO 8
            T1=T0
            GO TO 9
    8     T1=T0-(T0-TA)*EXP(-S1*ZG1)
    9     TT=T1
   10   CONTINUE
        TZ=TT
        TZL=TLB
        IF(AM.EQ.ZERO) GO TO 15
          ZGM=ZETA(ZM)
          EXZ=EXP(-(SP*(ZG -ZGM)))
          DIT=FOUR*AM*EXZ/(ONE+EXZ)**2/TINF
          TZ=TT/(ONE+TT*DIT)
          EXM=EXP(SP*ZGM)
          DITL=FOUR*AM*EXM/(ONE+EXM)**2/TINF
          TZL=TLB/(ONE+TLB*DITL)
   15   IF(XM.EQ.ZERO) GO TO 350
!
!          CALCULATE DENSITY ABOVE ZA
!
          GLB=GSURFV/(ONE+ZLB/REV)**2
          GAMMA2=XM*GLB/(S2*RGAS*TINF)
          DENS2=DLB*(TLB/T2)**(GAMMA2)*EXP(-S2*GAMMA2*ZG2)
          DENSSV=DENS2
          IF(ALT.GE.ZA) GO TO 300
!            CALCULATE DENSITY BELOW ZA
            GAMMA1=XM*GLB/(S1*RGAS*T0)
            DENS1=DENS2*(TA/TT)**(GAMMA1)*EXP(-S1*GAMMA1*ZG1)
            DENSSV=DENS1
  300    CONTINUE
         IF(AM.EQ.ZERO) GO TO 340
          GAMMAP=XM*GLB/(SP*RGAS*TINF)
           DENSM=EXP(GAMMAP*FOUR*AM*(EXZ/(ONE+EXZ)-EXM/(ONE+EXM)))
           DENSSV=DENSM*DENSSV
  340    CONTINUE
         DENSSV=DENSSV*(TZL/TZ)**(ONE+ALPHA)
  350 CONTINUE
      RETURN
      END
