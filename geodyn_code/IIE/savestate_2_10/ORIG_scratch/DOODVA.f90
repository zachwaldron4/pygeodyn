!$DOODVA
      SUBROUTINE DOODVA(T,BETA,BETAP,U,RI,RNIU,RP)
!********1*********2*********3*********4*********5*********6*********7**
! DOODVA           07/08/94            9408.0    PGMR - N. PAVLIS
!
!  FUNCTION  :
!  S/R TO EVALUATE THE DOODSON VARIABLES, THEIR RATES AND THE ARGUMENTS
!  NEEDED TO EVALUATE NODAL ANGLES AND NODAL FACTORS FOR THE LUNAR
!  TIDES.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T       I    S    TIME IN JULIAN CENTURIES SINCE J2000.0
!                     (J2000.0 = MJD 51544.5)
!  BETA     O    A    DOODSON VARIABLES ........RADIANS
!  BETAP    O    A    RATES ....................RADIANS/JULIAN CENTURY
!                     (1 JULIAN CENTURY = 36525 DAYS)
!  U        O    A    NODAL ANGLE ARGUMENTS.....RADIANS
!  RI
!  RNIU
!  RP       O    S    NODAL FACTOR ARGUMENTS....RADIANS
!
! COMMENTS
!
!  THE FUNDAMENTAL ARGUMENTS OF THE IAU 1980 THEORY OF NUTATION ARE
!  COMPUTED ACCORDING TO THE EQS. IN: (IERS TECHNICAL NOTE 13, IERS
!  STANDARDS (1992), JULY 1992, PG. 32). THE DEFINITION OF THE
!  DOODSON VARIABLES CAN BE FOUND IN (IBID, PP. 53-54).
!
!  THE ARGUMENTS FOR THE NODAL ANGLES ARE COMPUTED ACCORDING TO THE
!  FORMULATION IN:'MANUAL OF HARMONIC ANALYSIS AND PREDICTION OF TIDES'
!  BY P. SCHUREMAN (U.S. COAST AND GEODETIC SURVEY SPECIAL PUBLICATION
!  NO. 98).
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      DIMENSION    A(6,4),  R(6,3)
      DIMENSION ARGM(6), RATE(6)
      DIMENSION BETA(6),BETAP(6),U(6)
!-----------------------------------------------------------------------
      DATA PI     /3.14159265358979323846D+00/
      DATA A/ 0.23555483935439406835D+01, 0.62400359393260227988D+01,   &
     &        0.16279019339719610992D+01, 0.51984695135799227465D+01,   &
     &        0.21824386243609943354D+01, 0.17533685592332655129D+01,   &
     &        0.83286914228838956956D+04, 0.62830195602418415834D+03,   &
     &        0.84334661583184542444D+04, 0.77713771461706416006D+04,   &
     &       -0.33757045933753511924D+02, 0.62833197068884092720D+03,   &
     &        0.15179516355539571959D-03,-0.27973749400020226830D-05,   &
     &       -0.64271749704691186670D-04,-0.33408510765258125318D-04,   &
     &        0.36142859926715908322D-04, 0.67707139449033358721D-05,   &
     &        0.31028075591010303590D-06,-0.58177641733144319231D-07,   &
     &        0.53329504922048959295D-07, 0.92114599410811838782D-07,   &
     &        0.38785094488762879487D-07,-0.45087672343186847404D-09/
      DATA R/ 0.83286914228838956956D+04, 0.62830195602418415834D+03,   &
     &        0.84334661583184542444D+04, 0.77713771461706416006D+04,   &
     &       -0.33757045933753511924D+02, 0.23012167531542315185D+06,   &
     &        0.30359032711079143919D-03,-0.55947498800040453660D-05,   &
     &       -0.12854349940938237334D-03,-0.66817021530516250637D-04,   &
     &        0.72285719853431816644D-04, 0.13541484217399397774D-04,   &
     &        0.93084226773030910769D-06,-0.17453292519943295769D-06,   &
     &        0.15998851476614687788D-06, 0.27634379823243551635D-06,   &
     &        0.11635528346628863846D-06,-0.13517157923004855929D-08/
!      SAVE  ! original
!-----------------------------------------------------------------------
!  1  MEAN ANOMALY OF THE MOON (L)
!  2  MEAN ANOMALY OF THE SUN (L')
!  3  MEAN ARGUMENT OF LATITUDE OF THE MOON (F)
!  4  MEAN ELONGATION OF THE MOON FROM THE SUN (D)
!  5  MEAN LONGITUDE OF THE ASC. NODE OF THE MOON (OMEGA)
!  6  UT1 TO GMST(0H UT1) CONVERSION
!  6R RATIO OF UNIVERSAL TO SIDEREAL TIME
!-----------------------------------------------------------------------
      DO 10 I=1,6
      ARGM(I)=((A(I,4)*T+ A(I,3))*T+A(I,2))*T+A(I,1)
      RATE(I)=(           R(I,3) *T+R(I,2))*T+R(I,1)
   10 END DO
       BETA(2)=          ARGM(3)+ARGM(5)
       BETA(3)=  BETA(2)-ARGM(4)
       BETA(4)=  BETA(2)-ARGM(1)
       BETA(5)=         -ARGM(5)
       BETA(6)=  BETA(2)-ARGM(4)-ARGM(2)
       BETA(1)= -BETA(2)+ARGM(6)+PI
      BETAP(2)=          RATE(3)+RATE(5)
      BETAP(3)= BETAP(2)-RATE(4)
      BETAP(4)= BETAP(2)-RATE(1)
      BETAP(5)=         -RATE(5)
      BETAP(6)= BETAP(2)-RATE(4)-RATE(2)
      BETAP(1)=-BETAP(2)+RATE(6)
!-----------------------------------------------------------------------
!  FROM HERE ON ALL EQUATION REFERENCES REFER TO U.S. C. & G. SURVEY
!  SPECIAL PUBLICATION NO. 98.
!-----------------------------------------------------------------------
!  FIND 'I', 'XI', 'NIU' (EQS. ON PAGE 156)
!-----------------------------------------------------------------------
      RN=-BETA(5)
      X02=COS(RN)
      X03=TAN(RN/2.D0)
      X08=0.91370D0-0.03569D0*X02
      RI=ACOS(X08)
      X07=SIN(RI)
      X27=X07*X07
      Y01=ATAN(1.01883D0*X03)
      Y02=ATAN(0.64412D0*X03)
       RXI=RN-(Y01+Y02)
      RNIU=   (Y01-Y02)
!-----------------------------------------------------------------------
!  FIND 'P', 'Q' (EQS. 191 AND 203)
!-----------------------------------------------------------------------
      RP=BETA(4)-RXI
      TWOP=2.D0*RP
      X04=SIN(TWOP)
      X05=COS(TWOP)
      X06=X04/(1.D0+X05)
      RQ=ATAN(0.483D0*X06)
!-----------------------------------------------------------------------
!  FIND "NIU'", 'R', "NIU''" (EQS. 224, 214 AND 232)
!-----------------------------------------------------------------------
      X09=2.D0*X07*X08
      X10=SIN(RNIU)
      X11=COS(RNIU)
      X12=(1.D0+X08)/X07
      X13=2.D0*X10*X11
      X14=1.D0-2.D0*X10*X10
       RNIUP=ATAN(X09*X10/(X09*X11+0.3347D0))
          RR=ATAN(X04/(1.D0/6.D0*X12*X12-X05))
      RNIUPP=0.5D0*ATAN(X27*X13/(X27*X14+0.0727D0))
!-----------------------------------------------------------------------
      U(1)=   RXI
      U(2)=  RNIU
      U(3)=    RQ
      U(4)= RNIUP
      U(5)=    RR
      U(6)=RNIUPP
!-----------------------------------------------------------------------
      RETURN
      END
