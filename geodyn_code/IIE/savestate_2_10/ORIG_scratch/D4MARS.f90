!$D4MARS
      SUBROUTINE D4MARS(XSC,MJDSEC,FSEC,FLXAVG,DENST)

!********1*********2*********3*********4*********5*********6*********7**
! D4MARS           08/23/13            0000.0    PGMR - ?
!
!
! FUNCTION: COMPUTE ATMOSPHERIC DENSITY OF MARS USING THE REVISED
!           DTM-Mars 2001 MODEL (Bruinsma and Lemoine).
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSC(I)  I          POSITION VECTOR OF THE SPACECRAFT (KM)
!   MJDSEC  I          NUMBER OF SECONDS CORRESPONDING TO MJD
!   FSEC    I          FRACTIONAL SECOND CORRESPONDING TO THE ABOVE
!   FLXAVG  I          AVERAGED SOLAR FLUX FOR THIS DATE AND TIME.
!   DENST   O          MASS DENSITY (KG/M**3)
!   A                  SEMIMAJOR AXIS OF MARS ORBIT (KM)
!   AUOA               INVERSE OF A IN ASTRONOMICAL UNITS
!   AVOG               AVOGADRO'S NUMBER
!   AVOGDR             RECIPROCAL OF AVOGADRO'S NUMBER (KG-MOLE)
!   CSCLAT             COSINE OF THE SPACECRAFT LATITUDE
!   DEGRAD             CONSTANT TO CONVERT DEGREES TO RADIANS
!   DELR61             CORRECTION TO R61MB DUE TO PRESSURE VARIATION
!   DL                 LONGITUDE DIFFERENCE BETWEEN SPACECRAFT AND SUN
!   DT                 DIFFERENCE OF DATE BETWEEN PRESENT AND REFERENCE
!   EAU                DEFINITION OF ASTRONOMICAL UNIT
!   PROPR              PRESSURE OVER AVERAGE PRESSURE, SEASONAL VARIATION
!   PERIOD             SIDERIAL PERIOD OF MARS (DAYS)
!   R61MB              AEROCENTRIC DISTANCE OF 6.1 MILLIBAR SPHEROID (KM)
!   REFDAT             DATE OF MARS PERIHELION
!   RSC                AEROCENTRIC DISTANCE OF SPACECRAFT (KM)
!   alat               LATITUDE OF SPACECRAFT (RAD.) (ROTATING
!                      COORDINATES)
!   d(I)               CONCENTRATION OF EACH GAS
!                      WHERE     I = 1 REPRESENTS CO2
!                                    2     "      O
!                                    3     "      N2
!                                    4     "      AR
!                                    5     "      CO
!                                    6     "      O2
!                                    7     "      HE
!                                    8     "      H
!                                    9     "      H2
!   ddh                DENSITY GRADIENT WITH ALTITUDE, K/km
!   fbar               SOLAR FLUX AT 10.7 CM WAVELENGTH (AT MARS)
!   hl                 LOCAL TIME AT SPACECRAFT POSITION (RAD.)
!   wmm                MEAN MOLECULAR MASS
!   tz                 TEMPERATURE AT ALTITUDE alti
!   tinf               EXOSPHERIC TEMPERATURE
!   t120               TEMPERATURE AT REFERENCE ALTITUDE (138 km)
!   tp120              TEMPERATURE GRADIENT
!
!
! COMMENTS:
!
!   THIS ROUTINE IS PROGRAMMED IN GEODYN TO IMPLEMENT THE
!   UPDATED MARS ATMOSPHERE MODEL OF DTM-Mars PUBLISHED IN 2001
!   BY SEAN BRUINSMA AND FRANK LEMOINE. THE ORIGINAL FORTRAN SUBROUTINE
!   WAS WRITTEN BY ANNAIG PEDRONO AND SEAN BRUINSMA.
!
!                                       A. GENOVA
!                                       AUGUST, 2013
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

!***********************************************************************
!  COMMON PARAMETERS INTO DTM SUBROUTINES*******************************
!***********************************************************************

      INCLUDE 'COMMON_DECL.inc'
      COMMON/PARDTM/tau_atm,taubg,xlsdeb,xlsfin
      COMMON/HLOCAL/c2h,c3h,ch,hl0,s2h,s3h,sh
      COMMON/PLGDTM/p10_atm,p10mg,p11_atm,p20_atm,p20mg,p21_atm,p22_atm,&
     &              p30_atm,p31_atm,p32_atm,p33_atm,p40_atm,p40mg,p41, &
     &              p42,p50_atm,p51_atm,p52,p60,p62

!***********************************************************************
!  COMMON PARAMETERS INTO DTM GEODYN************************************
!***********************************************************************

      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/MNSNSP/XMNSNS(7,2)
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/PMARS /DTM74 ,XPMARS

!***********************************************************************
!  LOCAL PARAMETERS*****************************************************
!***********************************************************************
      DOUBLE PRECISION, DIMENSION(74) :: az2,h,he,o,o2,tp,            &
     &                                   tt,t0_atm,co2,co,ar,h2,ttedit

      DIMENSION XSNI(3),XSC(3),XSN(3)
      DOUBLE PRECISION :: day_m
      DOUBLE PRECISION :: fbar
      DOUBLE PRECISION :: alti
      DOUBLE PRECISION :: hl
      DOUBLE PRECISION :: alat
      DOUBLE PRECISION :: ls
      DOUBLE PRECISION :: tz
      DOUBLE PRECISION :: tinf
      DOUBLE PRECISION :: ro
      DOUBLE PRECISION :: wmm
      DOUBLE PRECISION :: ddh

      INTEGER :: i
      DOUBLE PRECISION, PARAMETER :: gsurf_l = 372.7D0
      DOUBLE PRECISION, PARAMETER :: re_l = 3376.78D0
      DOUBLE PRECISION, PARAMETER :: rgas = 831.4D0
      DOUBLE PRECISION, PARAMETER :: zlb = 138.D0
      DOUBLE PRECISION :: c_l,c2_l,c4,dt120,dtinf,dtp120,dzeta,dzeta2, &
     &              expsz,gamma_l,gdelaz,gdelaz2,gdelh,gdelhe,gdelo, &
     &                    gdelco2,gdelco,gdelar,gdelh2,gdelo2,gdelt, &
     &                    gdelt0,gdeltp,glb,sbc,s2,sigma,sigzeta,sp, &
     &                    t120,t120tz,tinftz,tp120,upapg,zeta_l,avo

      INTEGER, DIMENSION(9) :: ma
      DOUBLE PRECISION, DIMENSION(9) :: alefa,cc_l,dbc,dbase,fz,vma
      DOUBLE PRECISION, DIMENSION(74) :: daz2,dh,dhe,do2,dt0,dtp,dtt, &
     &                                   do,dar,dh2,dco2,dco

      DATA alefa/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,-0.38D0,-0.40D0,0.D0/
      DATA DP0145/.0145D0/,DP001/.001D0/
      DATA REFDAT,SIGMA,SOLDAY,U/2443951.D0,1.D0,365.25636D0,           &
     &                          .42828443D10/
      DATA XMBAR,NC,PERIOD,MYEAR/43.3D0,9,686.98D0,668.60D0/
      DATA DJ3935/2443935.D0/,D26P35/26.35D0/,D360/360.D0/,D250/250.D0/
      DATA EMO/.09338D0/
      DATA ZERO/0.D0/,HALF/.5D0/,ONE/1.D0/,THREE/3.D0/
      DATA P6,RA,RB,RC/610.D0,3394.67D0,3393.21D0,3376.78D0/
      DATA A,AF,AVOGDR,ED,ES/2.279D8,60.D0,0.1660081676D-20,.1D0,.1D0/
      DATA SCLNGA/1.893682238D0/
! DEFINITION OF AU IN METERS
      DATA EAU /1.4959787866D+11/
! HARD CODE REFERENCE RADII AND MARS FLATTENING FOR OUTPUT
      DATA  DMARSAE/3397000.0D0/
      DATA  DMARSF/6.123050038D-03/

! HARD CODE TABLE VALUES (12 columns of 74 values)

      DATA  tt/                                                         &
     & 0.226932D+03,   -0.372422D-05,   -0.580413D-03,     &
     &-0.111180D-04,    0.721520D-02,    0.401038D-03,     &
     &-0.635205D-01,    0.100000D-03,   -0.135000D-02,     &
     &-0.839578D-05,    0.575336D-05,   -0.393925D-01,     &
     & 0.212435D+00,    0.903626D-03,   -0.206559D-03,     &
     & 0.000000D+00,    0.000000D+00,    0.479901D-02,     &
     & 0.249381D-01,    0.567840D-03,   -0.168823D-03,     &
     & 0.175620D+00,    0.175214D+00,    0.157895D-04,     &
     &-0.318785D-02,    0.759020D-02,    0.447412D-02,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.204434D-03,   -0.874504D-03,   -0.413575D-04,     &
     &-0.792217D-05,    0.129316D-04,    0.717451D-05,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.300000D+00,    0.300000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.900000D-03,    0.450000D-02,     &
     &-0.302798D-04,    0.137452D-01,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.300000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA  t0_atm/                                                     &
     & 0.169763D+03,   -0.750896D-01,    0.236530D-02,     &
     &-0.535410D-02,    0.138965D-02,   -0.135127D-01,     &
     & 0.269928D-01,    0.344571D-04,    0.120614D-03,     &
     & 0.492368D-02,   -0.696624D-02,    0.185870D-02,     &
     & 0.134456D+00,   -0.650429D-02,   -0.182243D-01,     &
     & 0.881042D-04,   -0.803250D-04,    0.106393D-02,     &
     & 0.288038D-01,   -0.969317D-02,   -0.336558D-02,     &
     & 0.135820D+00,    0.958600D-01,    0.220022D-02,     &
     &-0.375800D-02,    0.115979D-01,    0.916790D-02,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.885327D-02,   -0.544968D-03,   -0.120295D-02,     &
     & 0.158907D-02,   -0.372751D-03,   -0.126953D-02,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     &-0.500000D-04,    0.400000D+00,    0.400000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.648545D-02,     &
     & 0.207435D-02,    0.500768D-04,   -0.677926D-03,     &
     & 0.000000D+00,    0.000000D+00,   -0.125640D-03,     &
     &-0.815167D-05,    0.000000D+00,    0.000000D+00,     &
     & 0.400000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/


      DATA  tp/                                                         &
     & 0.407032D+01,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.650000D-05,    0.800000D-05,     &
     &-0.880000D-06,    0.490000D-06,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA co2/                                                         &
     & 0.110065D+11,   -0.132629D+00,   -0.598009D+00,     &
     &-0.876863D-03,    0.112690D-02,   -0.116190D+00,     &
     &-0.559003D+00,   -0.345337D-03,   -0.236417D-02,     &
     &-0.244419D-01,   -0.119775D-01,   -0.378425D-01,     &
     & 0.793940D+00,    0.885329D-01,    0.120209D-02,     &
     &-0.181154D-02,    0.626019D-03,   -0.845423D-01,     &
     & 0.882971D+00,   -0.550530D-01,   -0.444001D-02,     &
     & 0.161327D+00,    0.366189D+00,   -0.161210D-02,     &
     & 0.168313D-01,    0.444246D-01,    0.131215D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.635223D-01,   -0.339925D-01,   -0.437459D-02,     &
     &-0.781535D-02,    0.433640D-02,   -0.103457D-01,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.650000D+01,    0.650000D+01,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,   -0.250000D-01,     &
     &-0.100000D+00,    0.000000D+00,    0.000000D+00,     &
     &-0.345225D-01,    0.140686D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.596750D-01,   -0.343652D-01,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA   o/                                                         &
     & 0.501110D+09,   -0.299834D-08,    0.000000D+00,     &
     &-0.200000D-08,    0.500000D-02,   -0.300363D+00,     &
     & 0.500660D-01,    0.336334D-08,    0.220899D-08,     &
     & 0.960000D-04,    0.000000D+00,   -0.250000D-08,     &
     &-0.150000D-08,    0.100914D+00,   -0.507080D-01,     &
     &-0.141006D-08,    0.504522D-08,   -0.600000D-01,     &
     & 0.400000D+00,    0.000000D+00,    0.000000D+00,     &
     &-0.100992D+00,    0.300887D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.743494D-09,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.200000D+00,   -0.800000D-01,     &
     & 0.240000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA az2/                                                         &
     & 0.128146D+09,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.347342D-02,    0.110692D+00,     &
     & 0.719648D+00,    0.615105D-04,    0.381507D-03,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.679487D-01,    0.269512D-01,     &
     &-0.140584D-02,    0.493685D-03,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.477270D-05,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA  ar/                                                         &
     & 0.133527D+09,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.177664D-02,    0.147606D+00,     &
     & 0.967759D+00,   -0.232877D-03,   -0.163206D-02,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.856673D-01,    0.332451D-01,     &
     &-0.170650D-02,    0.603064D-03,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA  co/                                                         &
     & 0.254731D+09,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.348087D-02,    0.110932D+00,     &
     & 0.720995D+00,    0.428137D-04,    0.271390D-03,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.656797D-01,    0.261134D-01,     &
     &-0.159889D-02,    0.563851D-03,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA  o2/                                                         &
     & 0.355178D+08,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.290544D-02,    0.123159D+00,     &
     & 0.802556D+00,   -0.503435D-04,   -0.370080D-03,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.720365D-01,    0.284242D-01,     &
     &-0.164344D-02,    0.578833D-03,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/


      DATA  he/                                                         &
     & 0.661880D+07,    0.200000D+00,    0.000000D+00,     &
     &-0.100000D+00,    0.200000D-02,   -0.200000D-01,     &
     &-0.600000D-01,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.400000D+00,     &
     &-0.900000D+00,    0.150000D+00,    0.400000D-01,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,   -0.200000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.306000D-09,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.110000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA   h/                                                         &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

      DATA  h2/                                                         &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00,    0.000000D+00,     &
     & 0.000000D+00,    0.000000D+00/

!***********************************************************************
!  ATOMIC MASS  CO2, O, N2, Ar, CO, O2, HE, H, H2***********************
!***********************************************************************

      DATA ma/44.D0,16.D0,28.D0,40.D0,28.D0,32.D0,4.D0,1.D0,2.D0/

!***********************************************************************
!  AVOGADRO'S NUMBER****************************************************
!***********************************************************************

      DATA avo/6.022D+23/

!***********************************************************************
!  ATOMIC MASS / AVOGADRO'S NUMBER (6.022e+23 /mole)********************
!***********************************************************************

      DO i=1,9
            vma(i)=ma(i)/avo
      END DO

!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! *** RECONSTITUTE THE JULIAN DATE
!

        tau_atm  = tt(74-3)                                          ! MA
        xlsdeb= tt(74-2)                                             ! Ls
        xlsfin= tt(74-1)                                             ! Ls
!       taubg = tt(74)                                               ! BACKGROUN
        taubg = DTM74


        IF(taubg.lt.1.5D-01) taubg=0.D0                                 !


!      IF (DTM74.NE.0.0D0) THEN
!        DO JJ=1,73
!         ttedit(JJ)=tt(JJ)
!        END DO
!         ttedit(74)=DTM74
!         taubg = ttedit(74)
!         IF(taubg.lt.1.5E-01) taubg=0.d0
!      END IF

      DATE=TMGDN1+(DBLE(MJDSEC)+FSEC)/SECDAY
      DMJD=TMGDN1-2400000.50D0

!
! *** PULL QUANTITIES FOR SUN FROM XMNSNS
!

      RSN=XMNSNS(4,2)
      XSNI(1)=RSN*XMNSNS(1,2)
      XSNI(2)=RSN*XMNSNS(2,2)
      XSNI(3)=RSN*XMNSNS(3,2)
      RSN=RSN*DP001

!
! *** CONVERT COORDINATE SYSTEM
!

      XSN(1)=RMB(1)*XSNI(1)+RMB(4)*XSNI(2)+RMB(7)*XSNI(3)
      XSN(2)=RMB(2)*XSNI(1)+RMB(5)*XSNI(2)+RMB(8)*XSNI(3)
      XSN(3)=RMB(3)*XSNI(1)+RMB(6)*XSNI(2)+RMB(9)*XSNI(3)
      XYSC=XSC(1)*XSC(1)+XSC(2)*XSC(2)
      XYSN=XSN(1)*XSN(1)+XSN(2)*XSN(2)
      RSC=SQRT(XYSC+XSC(3)*XSC(3))*DP001
      SCLONG=ATAN2(XSC(2),XSC(1))
      SCLAT=ATAN2(XSC(3),SQRT(XYSC))
      SNLONG=ATAN2(XSN(2),XSN(1))
      SNLAT=ATAN2(XSN(3),SQRT(XYSN))
      alat=SCLAT                                                        !

!
! *** CALCULATE LOCAL TIME
!

      DL=SCLONG-SNLONG
      hl=PI+DL                                                          !
      IF(hl.LT.ZERO) hl=hl+TWOPI
      IF(hl.GE.TWOPI) hl=hl-TWOPI
      SCLONG=SCLONG+SCLNGA

!
! *** CALCULATE THE FRACTION OF HELIOCENTRIC PERIOD
!

      DT=DATE-REFDAT
      FP=MOD(DT,PERIOD)/PERIOD
      IF(FP.LT.ZERO) FP=FP+ONE


!
! *** CALCULATE THE AEROCENTRIC SOLAR LONGITUDE ls (DEG)
!

      AOR=A/RSN
      AC=(AOR*(ONE-EMO*EMO)-ONE)/EMO
      IF(ABS(AC).GT.ONE) AC=INT(AC)
      ALM=ACOS(AC)/DEGRAD
      IF(FP.GT.HALF) ALM=D360-ALM
      ls = ALM + D250
      IF(ls.GT.D360) ls=ls-D360

! *** CALCULATE MARS DAY-OF-YEAR
!

      day_m = MYEAR/D360*ls
!
! *** CALCULATE RADIUS OF REF SPHEROID
!

      CSCLAT=COS(SCLAT)
      CSCLNG=COS(SCLONG)
      SSCLAT=SIN(SCLAT)
      SSCLNG=SIN(SCLONG)
      RSQ=(CSCLAT*CSCLNG/RA)**2+(CSCLAT*SSCLNG/RB)**2+(SSCLAT/RC)**2
      R61MB=SQRT(ONE/RSQ)

!
! *** CORRECTION TO 6.1 MILLIBAR ELLIPSOID FOR SEASONAL GLOBAL PRESSURE
! *** VARIATION
!
!  AM1 and AM2 REPARAMETERIZED WITH VARIABLES ALS1, ALS2 BY FGL ON 7/10/2000
!     AM1 = (DCOS(0.75D0*DEGRAD*(ALS-270.D0)))**2
!     AM2 = (DCOS((6.D0/7.D0)*DEGRAD*(ALS-45.D0)))**2
! THESE SIX LINES ADDED BY FGL ON 7/10/2000.

      ALS1=ls
      IF(ls.LE.30.0D0)ALS1=ls+360.0D0
      ALS2=ls
      IF(ls.GE.300.0D0)ALS2=ls-360.0D0
      AM1 = (COS(0.75D0*DEGRAD*(ALS1-270.D0)))**2.D0
      AM2 = (COS((6.D0/7.D0)*DEGRAD*(ALS2-45.D0)))**2.D0

      IF (ls.GE.30.D0 .AND. ls.LE.150.D0)  AM1 = ZERO
      IF (ls.GE.150.D0 .AND. ls.LE.300.D0) AM2 = ZERO
      PROPR = (1.D0 + 0.310D0*AM1 + 0.225D0*AM2) / 1.169D0
      T0_ELL = 220.D0 *AOR
      DEL61 = (T0_ELL/19.51D0) * LOG(PROPR)
      R61MB = R61MB + DEL61

!
! *** CALCULATE THE HEIGHT OF THE SPACECRAFT ABOVE THE SPHEROID
! *** SURFACE
!

      alti = RSC - re_l
      Z_GEODYN = RSC - R61MB

!
! *** COMPUTE 10.7 CM FLUX WITH SHORT PERIOD TERM IGNORED AND
! *** ADJUSTED TO THE DISTANCE OF MARS FROM THE SUN (TAKEN FROM
! *** OLD CODE)
!
! FLXAVG HAS BEEN SCALED BY EARTH ECCENTRICITY; THEREFORE
! IT MUST BE 'UNSCALED' BACK TO THE 1 AU VALUE.

      DMJD=DATE-2400000.50D0
      EARAU=EDIST(DMJD)
      FLXSCL=FLXAVG*EARAU*EARAU
      DMAAU = EAU*DP001/RSN
      fbar = FLXSCL * (DMAAU*DMAAU)

!***********************************************************************
!  INITIALIZATION PARAMETERS********************************************
!***********************************************************************

      ro=0.D0
      ddh=0.D0
      dtinf=0.D0
      dt120=0.D0
      dtp120=0.D0
      fz(1)=0.D0
      fz(2)=0.D0
      fz(3)=0.D0
      fz(4)=0.D0
      fz(5)=0.D0
      fz(6)=0.D0
      fz(7)=0.D0
      fz(8)=0.D0
      fz(9)=0.D0

      DO i=1,74
            dtt(i)=0.D0
            dco2(i)=0.D0
            do(i)=0.D0
            daz2(i)=0.D0
            dar(i)=0.D0
            dco(i)=0.D0
            do2(i)=0.D0
            dhe(i)=0.D0
            dh(i)=0.D0
            dh2(i)=0.D0
            dt0(i)=0.D0
            dtp(i)=0.D0
      END DO

!***********************************************************************
!  CALCULATE LEGENDRE POLYNOMIALS***************************************
!***********************************************************************

      c_l = SIN(alat)
      c2_l = c_l * c_l
      c4 = c2_l * c2_l
      sbc = COS(alat)
      s2 = sbc * sbc

      p10_atm = c_l
      p20_atm = 1.5D0*c2_l - 0.5D0
      p30_atm = c_l * (2.5D0*c2_l-1.5D0)
      p40_atm = 4.375D0*c4 - 3.75D0*c2_l + 0.375D0
      p50_atm = c_l * (7.875D0*c4-8.75D0*c2_l+1.875D0)
      p11_atm = sbc
      p21_atm = 3.D0 * c_l * sbc
      p31_atm = sbc * (7.5D0*c2_l-1.5D0)
      p41 = c_l * sbc * (17.5D0*c2_l-7.5D0)
      p51_atm = sbc * (39.375D0*c4-26.25D0*c2_l+1.875D0)
      p22_atm = 3.D0 * s2
      p32_atm = 15.D0 * c_l * s2
      p42 = s2 * (52.5D0*c2_l-7.5D0)
      p52 = 3.D0*c_l*p42 - 2.D0*p32_atm
      p33_atm = 15.D0 * sbc * s2

!***********************************************************************
!  LOCAL TIME***********************************************************
!***********************************************************************

      hl0 = hl + PI
      ch = COS(hl0)
      sh = SIN(hl0)
      c2h = ch*ch - sh*sh
      s2h = 2.D0 * ch * sh
      c3h = c2h*ch - s2h*sh
      s3h = s2h*ch + c2h*sh

!***********************************************************************
!  CALCULATE FUNCTION G(l) FOR tinf, t120, tp120************************
!***********************************************************************



!      IF (DTM74.NE.0.0D0) THEN
!
!      call GLDTMARS(fbar,day_m,ttedit,dtt,gdelt,1.d0,ls)
!      dtt(1)=1.d0 + gdelt
!      tinf= ttedit(1) * dtt(1)
!!      write(6,*)'gdelt,ttinf',gdelt,ttinf
!
!      ELSE
!
      call GLDTMARS(fbar,day_m,tt,dtt,gdelt,1.D0,ls)
      dtt(1)=1.D0 + gdelt
      tinf= tt(1) * dtt(1)
!
!      ENDIF


      call GLDTMARS(fbar,day_m,t0_atm,dt0,gdelt0,1.D0,ls)
      dt0(1)=1.D0+gdelt0
      t120= t0_atm(1)*dt0(1)

      call GLDTMARS(fbar,day_m,tp,dtp,gdeltp,1.D0,ls)
      dtp(1)=1.D0+gdeltp
      tp120=tp(1)*dtp(1)

!***********************************************************************
!  CHECK tinf > t120****************************************************
!***********************************************************************

      IF(tinf.lt.t120) THEN

         tinf = 2.D0*(t120 - tinf) + tinf

      END IF

!***********************************************************************
!  CALCULATE CONCENTRATIONS n(z): H, HE, O, N2, O2, N*******************
!***********************************************************************

      sigma=tp120/(tinf-t120)
      dzeta=(re_l+zlb)/(re_l+alti)
      zeta_l=(alti-zlb)*dzeta
      dzeta2=dzeta*dzeta
      sigzeta=sigma*zeta_l
      expsz=EXP(-sigzeta)
      tz=tinf-(tinf-t120)*expsz

!***********************************************************************
!  CALCULATE FUNCTION G(l) FOR CO2, O, N2, Ar, CO, O2, He, H, H2********
!***********************************************************************

      call GLDTMARS(fbar,day_m,co2,dco2,gdelco2,1.D0,ls)
      dco2(1)=EXP(gdelco2)
      dbase(1)=co2(1)*dco2(1)

      call GLDTMARS(fbar,day_m,o,do,gdelo,1.D0,ls)
      do(1)=EXP(gdelo)
      dbase(2)=o(1)*do(1)

      call GLDTMARS(fbar,day_m,az2,daz2,gdelaz2,1.D0,ls)
      daz2(1)=EXP(gdelaz2)
      dbase(3)=az2(1)*daz2(1)

      call GLDTMARS(fbar,day_m,ar,dar,gdelar,1.D0,ls)
      dar(1)=EXP(gdelar)
      dbase(4)=ar(1)*dar(1)

      call GLDTMARS(fbar,day_m,co,dco,gdelco,1.D0,ls)
      dco(1)=EXP(gdelco)
      dbase(5)=co(1)*dco(1)

      call GLDTMARS(fbar,day_m,o2,do2,gdelo2,1.D0,ls)
      do2(1)=EXP(gdelo2)
      dbase(6)=o2(1)*do2(1)

      call GLDTMARS(fbar,day_m,he,dhe,gdelhe,1.D0,ls)
      dhe(1)=EXP(gdelhe)
      dbase(7)=he(1)*dhe(1)

      call GLDTMARS(fbar,day_m,h,dh,gdelh,0.D0,ls)
      dh(1)=EXP(gdelh)
      dbase(8)=h(1)*dh(1)

      call GLDTMARS(fbar,day_m,h2,dh2,gdelh2,0.D0,ls)
      dh2(1)=EXP(gdelh2)
      dbase(9)=h2(1)*dh2(1)

      glb=gsurf_l/(1.D0+zlb/re_l)**2.D0
      glb=glb/(sigma*rgas*tinf)
      t120tz=t120/tz

      DO i=1,9
         gamma_l=ma(i) * glb
         upapg = 1.D0 + alefa(i) + gamma_l
         fz(i) = t120tz**upapg * EXP(-sigzeta*gamma_l)

         !   concentrations H, HE, O, N2, O2, N
         cc_l(i) = dbase(i) * fz(i)
         !   partial densities of H, HE, O, N2, O2, N
         dbc(i) = cc_l(i) * vma(i)

!***********************************************************************
!  TOTAL DENSITY********************************************************
!***********************************************************************

         ro = ro + dbc(i)


         ddh = ddh + dbc(i)*sigma*dzeta2*(1.+alefa(i)-upapg*tinftz)

      END DO
!      write(6,*)'partial densities'
!      write(6,*)dbc(1),dbc(2),dbc(3),dbc(4),dbc(5),dbc(6)
      DENST = ro * 1.D3                                                 !


!***********************************************************************
!  MEAN MOLECULAR MASS**************************************************
!***********************************************************************

      wmm = ro*avo / (cc_l(1)+cc_l(2)+cc_l(3)+cc_l(4)+cc_l(5)+cc_l(6)   &
     &                                       +cc_l(7)+cc_l(8)+cc_l(9))


!
!  Printout for DTM-Stewart model comparison

!      IF(LSTINR)THEN
!        CALL REFELL2(XSC, DMARSAE, DMARSF,PHI, DLAM,DMH)
!        IF((DMH/1.0D+03).LT.425.0D0)THEN
!          write(99,9876)DMJD,DENST,PHI/DEGRAD,hl/DEGRAD,                &
!     &                DMH/1.0D+3,ls,fbar,Z_GEODYN,alat,FLXAVG
!        END IF
!
! 9876   format(f14.7,E16.8,2F12.6,1X,F8.2,F8.2,F9.3,F9.3,F9.3,F9.3)
!
!      END IF

      END
