!$THERMOS
      SUBROUTINE THERMOS (ES, TINF, TF, LAT, LST, ZF, RF, ZZF,          &
     & TOTALPRZ, TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ,         &
     & SCALE,tgrad)
!********1*********2*********3*********4*********5*********6*********7**
! THERMOS          00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!      ES    I    A    STD DEVIATIONS FROM NOMINAL VALUES
!                      ( Long TERM and Short TERM)
!     TINF   I    S
!      TF    I    S    TEMPERATURE AT ZF
!      LAT   I    S    SPACECRAFT LATITUDE
!      LST   I    S    TIME (MARS)
!      ZF    I    S
!      RF    I    S
!     ZZF    I    S    ALTITUDE DIFFERENCE FROM ZF
!  TOTALPRZ  O    S    PRESSURE
!      TZ              TEMPRETURE AT Z
!  TOTALMDZ  O    S    DENSITY
!   MOLWTG         S   AVERAGE MOLECULAR WEIGHT (43.49)
!     PRZ    O    S    PRESSURE AT Z
!     NDZ    O    S    NUMBER DENSITY AT Z
!     MDZ    O    S    MASS DENSITY AT Z
!   SCALE    I    S
!   tgrad    O    S    TEMPERATURE GRANDIENT
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION ES(0:11),PRZ(0:11),NDZ(0:11),MDZ(0:11)
      DOUBLE PRECISION LAT,LST,MOLWTG,NDZ,MDZ
!.... RETURNS TEMP & COMPOSITION VS ALTITUDE ABOVE ZF
!.... ES (EPS*SIG)   DEVIATIONS FROM NOMINAL VALUES
!.... 0,2,....,10 LONG-TERM
!.... 1,3,....11   SHORT-TERM
!.... FOR   FBAR, TINF, FOXY, ADXY,ZF,DZDUST
!.... ZZF  = INPUT ALTITUDES ABOVE ZF
!.... TZ  = TEMPERATURE VS ALTITUDE, DEG K
!.... NDZ= # DENSITY VS ALTITUDE, #/cc
!.... MDZ= MASS DENSITY VS ALTITUDE, gm/cc
!.... PRZ= PRESSURE VS ALTITUDE, BARS
      DOUBLE PRECISION M
      DIMENSION HH(0:11), XHH(0:11), DM(0:11), XDM(0:11), FF(0:11),     &
     & XFF(0:11), M(0:11)
      BK = 1.3804D-16
!
      A = 3394.67
      B = 3393.21
!...  C = polar radius (km)
      C = 3376.78
      AB = SQRT(A * B)
!...
!.... CONSTANT FOR NUMBER DENSITY CALCULATIONS
!...  GM = gravitational constant * mass of Mars
      GMMS = 4.28282D7
!...  J2 = coefficient of 1st non-spherical gravity term
      XJ2 = 0.001965D0
!...  P2 = latitude part of 1st non-spherical gravity term
      P2 = 1.5*SIN(LAT*DEGRAD)**2 - 0.5
!...  acceleration of gravity at height ZF
      GF = (GMMS/RF**2)*(1. - 3.*XJ2*((AB/RF)**2)*P2)
      GF = 100.D0*GF
!.... ACC. OF GRAVITY AT ZF
!.... PRESSF = 1.24E-9   (as originally in Stewart's model)
      PRESSF = 1.26D-9
!.... 1.26E-3 dynes/cm**2 = 1.26 nbar AT ZF, BASE OF THERMOSPHERE
!.... P1BAR = 1.013E6   (as originally in Stewart's model)
      P1BAR = 1.0D6
!.... MARS  SURFACE PRESSURE
      AO = 0.18 * (1.0 + ES(7))
!.... AO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT
      FO = 0.01 * EXP(ES(4) + ES(5))
!.... FO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT I
      M(0) = 44.011
!.... CO2 MOLECULAR WEIGHT
      M(1) = 28.016
!.... N2
      M(2) = 39.944
!.... ARGON
      M(3) = 32.00
!.... MOLECULAR OXYGEN
      M(4) = 28.011
!.... CARBON MONOXIDE
      M(5) = 16.00
!.... ATOMIC OXYGEN
      M(6) = 4.003
!.... HELIUM
      M(7) = 2.016
!.... MOLECULAR HYDROGEN
      M(8) = 1.008
!.... ATOMIC HYDROGEN MOLECULAR WEIGHT
      DM(0) = 7.31D-23
!.... CO2 MOLECULAR MASS
      DM(1) = 4.65D-23
!.... N2 MOLECULAR MASS
      DM(2) = 6.63D-23
!.... ARGON MOLECULAR MASS
      DM(3) = 5.31D-23
!.... 02 MOLECULAR MASS
      DM(4) = 4.65D-23
!.... CARBON MONOXIDE MOLECULAR MASS
      DM(5) = 2.66D-23
!.... ATOMIC OXYGEN MOLECULAR MASS
      XDM(0) = 6.65D-24
!.... HELIUM MOLECULAR MASS
      XDM(1) = 3.32D-24
!.... H2 MOLECULAR MASS
      XDM(2) = 1.66D-24
!.... H   MOLECULAR MASS
!.... THE FOLLOWING IS THE COMPOSITION OF THE HETEROSPHERE
      FF(0) = 0.932
!.... CO2
      FF(1) = 0.027
!.... NITROGEN
      FF(2) = 0.016
!.... ARGON
      FF(3) = 0.002
!.... MOLECULAR OXYGEN
      FF(4) = 0.013
!.... CARBON MONOXIDE
      FF(5) = 0.010
!.... ATOMIC OXYGEN
      FF(5) = FO*(1.0-AO*SIN(15.0*LST*DEGRAD)*COS(LAT*DEGRAD))
      PFHE = 3.3D-16 * TINF
!.... EXOBASE (ZF) HELIUM PARTIAL PRESSURE
      PFH2 = 2.4D-15
!.... EXOBASE (ZF) H2 PARTIAL PRESSURE
      if (TINF .LE. 330.0)PFH=5.2D-16*TINF*EXP(-TINF/70.0)
!.... EXOBASE (ZF) H PARTIAL PRESSURE
      if (TINF .GT. 330.0) then
        RATIO = 1440.0 / TINF
        PFH=5.8D-18*SQRT(TINF)*EXP(RATIO)/(1.0+RATIO)
!....   EXOBASE (ZF) H PARTIAL PRESSURE
      ENDIF
      XFF(0) = PFHE / PRESSF
      XFF(1) = PFH2 / PRESSF
      XFF(2) = PFH / PRESSF
      MOLWTG = 0.0D0
      TOTALPRZ = 0.0D0
      TOTALMDZ = 0.0D0
      YSC = ZZF * RF / (RF + ZZF)
      DEXPS =EXP(-YSC/SCALE)
      TZ = TINF - (TINF - TF) * DEXPS
!.... Temperature gradient, K/km
      tgrad = (TINF - TF)*(RF + YSC)/(SCALE*(RF + ZZF))*DEXPS
      DO 200 I = 0, 5
        HH(I) = BK * TINF / (GF * DM(I)) / 1.0D5
!....   SCALE HEIGHT
      PRZ(I)=PRESSF*FF(I)*EXP(-YSC/HH(I)-(SCALE/HH(I))*LOG(TZ/TF))
        NDZ(I) = P1BAR * PRZ(I) / (BK * TZ)
!....   NUMBER DENSITY HEAVY GASES
        MDZ(I) = NDZ(I) * DM(I)
        TOTALMDZ = TOTALMDZ + MDZ(I)
        TOTALPRZ = TOTALPRZ + PRZ(I)
        MOLWTG = MOLWTG + PRZ(I) * M(I)
  200 CONTINUE
      DO 210 J = 0, 2
        XHH(J) = BK * TINF / (GF * XDM(J)) / 1.0D5
!....   SCALE HEIGHT
        PRZ(J+6)=PRESSF*XFF(J)*EXP(-YSC/XHH(J)-(SCALE/XHH(J))*  &
     &   LOG(TZ/TF))
!
        NDZ(J+6) = P1BAR * PRZ(J + 6) / (BK * TZ)
!....   NUMBER DENSITY LIGHT GASES
        MDZ(J + 6) = NDZ(J + 6) * XDM(J)
        TOTALMDZ = TOTALMDZ + MDZ(J + 6)
        TOTALPRZ = TOTALPRZ + PRZ(J + 6)
        MOLWTG = MOLWTG + PRZ(J + 6) * M(J + 6)
  210 CONTINUE
      MOLWTG = MOLWTG / TOTALPRZ
      TOTALNDZ = P1BAR * TOTALPRZ / (BK * TZ)
      RETURN
      END
