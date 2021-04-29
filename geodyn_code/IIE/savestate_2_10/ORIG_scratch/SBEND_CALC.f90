!$SBEND_CALC
! ------------------------------------------------------------------------
!
      FUNCTION SBEND_CALC ( EL_RAD, TEMP_K, HUMID_F, PRESS_HG )
! ************************************************************************
! *                                                                      *
! *   Enigmatic routine. Nobody knows what is its origin. According to   *
! *   rumours it computes refercation angle. This rroutine was used by   *
! *   Calc package.                                                      *
! *                                                                      *
! *  ### 08-OCT-2004   SBEND_CALC  v1.0 (c)  L. Petrov  08-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
!
! input:
!   El_rad   -- elevation angle in radians
!   Press_Hg -- Pressure in mm of Mercury (Hg)
!   TEMP_K   -- Temperature in Kelvins
!   HUMID_F  -- relative humidity (percent)
!
! output   --
!   Sbend  -- bending angle in radians.
!
      DOUBLE PRECISION SBEND_CALC
      DOUBLE PRECISION  EL_RAD, TEMP_K, HUMID_F, PRESS_HG
!
      DOUBLE PRECISION  e(12),wp1(4),d3
      DOUBLE PRECISION fp,ft,fw,u,x,ad1,ad2,bd1,bd2,zd2,r,delta
      DOUBLE PRECISION a1,a2,b1,b2,c1,c2,e1,e2,e3,e4, &
     &       e5,e6,e7,e8,e9,e10,e11,e12
      DOUBLE PRECISION p1,p2,t1,t2,z1,z2,w0,w1,w2,w3
      Integer*4 I
!
      equivalence (e( 1), e1),(e( 2), e2),(e( 3), e3),(e( 4), e4), &
     &            (e( 5), e5),(e( 6), e6),(e( 7), e7),(e( 8), e8), &
     &            (e( 9), e9),(e(10),e10),(e(11),e11),(e(12),e12)
      equivalence (wp1(1),w0),(wp1(2),w1),(wp1(3),w2),(wp1(4),w3)
!
      data a1, a2 /     0.40816D0, 112.30D0  /
      data b1, b2 /     0.12820D0, 142.88D0  /
      data c1, c2 /     0.80000D0,  99.344D0 /
      data e  / 46.625D0  ,  45.375D0 ,     4.1572D0,  1.4468D0  , &
     &          0.25391D0,   2.2716D0,    -1.3465D0, -4.3877D0  , &
     &          3.1484D0 ,   4.5201D0,    -1.8982D0,  0.89000D0 /
      data p1 /   760.0D0 /
      data t1 /   273.0D0 /
      data wp1 / 22000.0D0    ,  17.149D0 ,  4684.1D0,    38.450D0   /
      data z1 /  91.870D0 /
!
!      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!           VARIABLES 'FROM':
!              1. HALFPI - THE VALUE OF PI/2
!              2. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS
!                          (RAD/DEG)
!
! STATEMENT FUCNTION
!
      DOUBLE PRECISION CONVD
      DELTA(AD1,AD2,BD1,BD2,ZD2)=(AD2-AD1)*EXP(BD1*(ZD2-BD2))
!          write ( 6, * ) ' sbend: ', temp_k, humid_f, press_hg ! %%%%%%%%%%
!
! CONVERT UNITS
! --- Zenith angle in degrees
!
      Z2 = 90.0D0 - EL_RAD*57.295779512D0
!         write ( 6, * ) ' z2=',z2 ! %%%%%%%%%
!
! --- Temperature in Kelvins
!
      T2 = TEMP_K
!
! --- Fractional humidity (0.0 -> 1.0)
!
      R = HUMID_F
!
! --- Pressure in mm of Hg
!
      P2 = PRESS_HG
!
! --- Calculate corrections for pres, temp, and wetness
!
      D3 = 1.0D0 + DELTA(Z1,Z2,C1,C2,Z2)
      FP = (P2/P1) * ( 1.0D0 - DELTA(P1,P2,A1,A2,Z2)/D3 )
      FT = (T1/T2) * ( 1.0D0 - DELTA(T1,T2,B1,B2,Z2)/D3 )
      FW = 1.0D0 + W0*R*EXP((W1*T2-W2)/(T2-W3)) / (T2*P2)
!
! --- Calculate optical refraction
!
      U=(Z2-E1)/E2
      X=E11
      DO 10 I=1,8
        X = E(11-I) + U*X
  10  CONTINUE
!
! --- Combine factors and finish optical factor
!
      SBEND_CALC = FT*FP*FW*( EXP(X/D3) - E12 )
!
! --- Back to radians from arc seconds
!@      sbend=(sbend/3600.0d0)*CONVD
!
      SBEND_CALC = SBEND_CALC/206264.806D0
!
      RETURN
      END
