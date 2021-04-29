!$DENSS
      FUNCTION DENSS(ALT,DLB,TINF,TLB,XM,ALPHA,TZ,ZLB,S2,T0,ZA,Z0,TR12)
!********1*********2*********3*********4*********5*********6*********7**
! DENSS            00/00/00            0000.0    PGMR - A. HEDIN
!
!
! FUNCTION:  CALCULATE TEMPERATURE AND DENSITY PROFILES FOR MSIS MODELS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ALT
!   DLB
!   TINF
!   TLB
!   XM
!   ALPHA
!   TZ
!   ZLB
!   S2
!   T0
!   ZA
!   Z0
!   TR12
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/PARMB/GSURF,RE
!
!CC
!
      DATA RGAS/831.4D0/
!
      ZETA(ZZ,ZL)=(ZZ-ZL)*(RE+ZL)/(RE+ZZ)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      DENSS=1.D0
      T0I = 1.0D0 / T0
      Z=MAX(ALT,ZA)
!      Eq. A4a
      ZG2=ZETA(Z,ZLB)
!      Eq. A1a
      TT=TINF-(TINF-TLB)*EXP(-S2*ZG2)
      TA=TT
      TAI = 1.D0 / TA
      TZ=TT
      DENSS=TZ
      IF(ALT.GE.ZA) GO TO 10
!      Eq. A4b
      ZG0=ZETA(Z0,ZA)
!      Eq. A2b
      DTA=(TINF-TA)*S2*((RE+ZLB)/(RE+ZA))**2.D0
!      Eq. A3e
      T12=T0+TR12*(TA-T0)
!      Eq. A4b
      ZG1=ZETA(ALT,ZA)
!       CALCULATE TEMPERATURE BELOW ZA
!      Eq. A3a
      DD=0.666666D0*ZG0*DTA*TAI**2.D0 - 3.11111D0*(TAI-T0I)+            &
     & 7.11111D0*(1.D0/T12-T0I)
!      Eq. A3b
      CC=ZG0*DTA/(2.D0*TA*TA) - (TAI-T0I) - 2.D0*DD
!      Eq. A3c
      BB=(TAI-T0I) - CC - DD
!      Eq. A3d
      X=(-(ZG1-ZG0)/ZG0)
!      Eq. A1b
      X2=X*X
!CCC  TZ=1.D0/(1.D0/T0+BB*X2+CC*X2*X2+DD*X2*X2*X2)
      TZ=1.D0/(T0I+ X2*(BB + X2*( CC + X2*DD ) ) )
      DENSS=TZ
      TAF=(T12-T0)/(TA-T0)
   10 IF(XM.EQ.0.D0) GO TO 50
      IF(TA.GT.0.D0 .AND. TZ.GT.0.D0) GO TO 20
!         WRITE(6,*)ALT,XM,TINF,TLB,T0,TA,II,JG,N,IFUN,S2,ZG0,TZ
         TT=TLB
         TA=TLB
         TAI = 1.0D0 / TA
         TZ=TLB
   20 CONTINUE
!      CALCULATE DENSITY ABOVE ZA
!      Eq. A17a
      GLB=GSURF/(1.D0+ZLB/RE)**2.D0
!      Eq. A16a
      GAMMA=XM*GLB/(S2*RGAS*TINF)
!      Eq. A13, A14a, & A15
      DENSA=DLB*(TLB/TT)**(1.D0+ALPHA+GAMMA)*EXP(-S2*GAMMA*ZG2)
      DENSS=DENSA
      IF(ALT.GE.ZA) GO TO 50
!      CALCULATE DENSITY BELOW ZA
!      Eq. A17b
      GLB=GSURF/(1.D0+ZA/RE)**2.D0
!      Eq. A16b
      GAMM=XM*GLB*ZG0/RGAS
!      Eq. A13, A14b, & A15
      DENSS=DENSA*(TA/TZ)**(1.D0+ALPHA)*                                &
     & EXP(GAMM*((X-1.D0)*T0I                                           &
     & +BB*(X*X2-1.D0)/3.D0+CC*(X2*X2*X-1.D0)/5.D0+                     &
     & DD*(X2*X2*X2*X-1.D0)/7.D0))
   50 CONTINUE
!CCCCCWRITE(6,100)CXM,ALT,ZA,TINF,TLB,S2,T0,S1,TA,TZ,DLB,DENSA,DENSS
!C100 FORMAT(' D',1P13E10.2)
      RETURN
      END
