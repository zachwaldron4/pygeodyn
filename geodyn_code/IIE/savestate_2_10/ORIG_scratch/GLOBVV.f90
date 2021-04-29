!$GLOBVV
      FUNCTION GLOBVV(YRD,SLAT,CLAT,TLOC,F107A,F107,P)
!
!     ....called by VTS3 for Venus Atmosphere Model of Hedin
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
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
!        CALCULATE GLOBAL VARIATIONS (12/12/80)
!        USES SOLAR ZENITH ANGLE FOR SYMMETRICAL TERMS
!
      DIMENSION PLG(6,6),P(*),SV(15),T(15)
!
      COMMON/VCSW/SWV(15),ISWV
!
      DATA DGTR/1.74533D-2/, XL/-1000.D0/,SV/15*1.0D0/,                 &
     &     TLL/1000.D0/,HR/.2618D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!CC   WRITE(6,*) 'GLOBVV:YRD,SLAT,CLAT ',YRD, SLAT, CLAT
!CC   WRITE(6,*) 'GLOBVV:TLOC,F107A,F107,P ',TLOC, F107A, F107, P
!
      IF(ISWV.NE.64999) CALL TSELEV(SV)
!
      IF( XL.EQ.SLAT.AND.TLL.EQ.TLOC) GO TO 15
!
!      CALCULATE LEGENDRE POLYNOMIALS
!
      C = CLAT
      S = SLAT
!        COSINE OF SOLAR ZENITH ANGLE
      C=CLAT*COS(HR*(TLOC-12.D0))
!CC   WRITE(6,*) 'GLOBVV:C,S ',C, S
      S2=S*S
      C2=C*C
      C4=C2*C2
      PLG(2,1)=C
      PLG(3,1)=HALF*(THREE*C2-ONE)
      PLG(4,1)=HALF*(FIVE*C2-THREE)*C
      PLG(5,1)=(D35*C4-D30*C2+THREE)/EIGHT
      PLG(6,1)=(D63*C4-D70*C2+D15)*C/EIGHT
      PLG(2,2)=S
      PLG(3,3)=THREE*S2
      PLG(4,4)=D15*S2*S
      PLG(5,5)=D105*S2*S2
      PLG(6,6)=D945*S2*S2*S
      XL=SLAT
   15 CONTINUE
      IF(TLL.EQ.TLOC) GO TO 16
!
!     ....RE-CODE SINE TERMS FOR SPEED
!      STLOC=SIN(HR*TLOC)
!      S2TLOC=SIN(2.*HR*TLOC)
!      S3TLOC=SIN(3.*HR*TLOC)
!      S4TLOC=SIN(4.*HR*TLOC)
!      S5TLOC=SIN(5*HR*TLOC)
!>>>>>
      TLOCHR = HR * TLOC
!CC   WRITE(6,*) 'GLOBVV: tloc, hr, tlochr ', tloc, hr, tlochr
      STLOC = SIN( TLOCHR )
      CTLOC = COS( TLOCHR )
      STLOC2 = STLOC**2
      S2TLOC = TWO * STLOC * CTLOC
      S3TLOC = ( THREE - FOUR * STLOC2 ) * STLOC
      S4TLOC = CTLOC * STLOC * ( FOUR - EIGHT * STLOC2 )
      S5TLOC = STLOC * ( FIVE - D20 * STLOC2 + D16 * STLOC2**2 )
!CC   WRITE(6,*) 'GLOBVV: STLOC, CTLOC, STLOC2 ', STLOC, CTLOC, STLOC2
!CC   WRITE(6,*) 'GLOBVV: S2TLOC, S3TLOC, S4TLOC ', S2TLOC,
!CC  1                         S3TLOC, S4TLOC
!CC   WRITE(6,*) 'GLOBVV: S5TLOC ', S5TLOC
!<<<<<
   16 CONTINUE
      TLL=TLOC
      T(1)=P(14)*(F107A-D200)+P(15)*(F107-F107A)
!CC   WRITE(6,*) 'GLOBVV:TLL, T(1) ',TLL, T(1)
!
!  THE FOLLOWING TEST INADVERTENTLY TREATS HELIUM DIFFERENTLY THAN THE
!  OTHER SPECIES WITH REGARD TO THE F107 EFFECT.
!  THE .GT. SHOULD BE .NE. BUT IS NOT CHANGED HERE TO BE CONSISTENT WITH
!  THE COEFFICIENTS AND TABLES AS ORIGINALLY DERIVED AND PUBLISHED.
!     7/25/83
!
      IF(P(2).GT.ZERO) GO TO 18
        F=ONE
        GO TO 20
   18 F=ONE+T(1)/(P(2)-P(4)+P(6)-P(8)+P(10))
!CC   WRITE(6,*) 'GLOBVV:TLL, T(1) ',TLL, T(1)
   20 CONTINUE
!CC   WRITE(6,*) 'GLOBVV: P(2), F  ', P(2), F
      T(2)=(P(2)*PLG(2,1)                                               &
     &     +P(3)*PLG(2,2)*STLOC)*F
      T(3)=(P(4)*PLG(3,1)                                               &
     &     +P(5)*PLG(3,3)*S2TLOC)*F
      T(4)=(P(6)*PLG(4,1)                                               &
     &     +P(7)*PLG(4,4)*S3TLOC)*F
      T(5)=(P(8)*PLG(5,1)       +P(9)*PLG(5,5)*S4TLOC)*F
      T(6)=(P(10)*PLG(6,1)       +P(11)*PLG(6,6)*S5TLOC)*F
      G=ZERO
      DO 50 I=1,6
   50 G=G+SWV(I)*T(I)
      GLOBVV=G
!CC   WRITE(6,*) 'GLOBVV: GLOBVV = ', GLOBVV
      RETURN
      END
