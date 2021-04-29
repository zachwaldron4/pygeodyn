!$GAMMA
      Subroutine gamma1(alat,als,gam)
!********1*********2*********3*********4*********5*********6*********7**
! GAMMA            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    alat    I    S    SPACECRAFT LATITUDE (DEGREES)
!     als    I    S    AREOCENTRIC LONGITUDE OF SUN ORBIT
!     gam    O    S    LAPSE RATES (DEGREES/KM)
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      Dimension gam(5)
!...  Computes lapse rates (-dT/dz) over geopotential height
!     intervals, from input site latitude (alat in degrees)
!     and areocentric longitude of the sun (Ls in degrees)
!     gam(1) = 0-5 km,  gam(2) = 5-15 km, gam(3) = 15-30 km,
!     gam(4) = 30-50 km, and gam(5) = 50-75 km.
!     Lapse rates gam(i) are in degrees/km
      Dimension A1(0:8),A2(0:8),A3(0:8),B1(8),B2(8),B3(8),C1(4),        &
     & C2(4),C3(4),f180(8),f240(8)
!...  A1,A2,A3 = coefficients for gamma dependence on latitude
!...  B1,B2,B3 = coefficients for gamma dependence on sine of Ls
!...  C1,C2,C3 = coefficients for gamma dependence on cosine of Ls
      Data A1/.98490781,-.36313842D-01,.27767563,-.29067342D-01,        &
     & .33237874,.10428713D-01,-.96889817D-01,-.24491553D-01,           &
     & -.12063783/
      Data B1/1.5759294,.96351525D-01,.11954050,-.37783196D-01,         &
     & -.14430163,-.38031515D-02,-.19010408,.10531591D-01/
      Data C1/-.25120403,.28816971D-02,-.18253198D-01,-.25117670D-02/
      Data A2/1.3406685,.32901300D-01,.38667874,.11147321D-02,          &
     & .95668988D-01,.15771285D-02,-.48561389D-01,.86152441D-03,        &
     & -.40206002D-01/
      Data B2/1.0556523,-.68349887D-01,-.28712926D-01,.70384158D-01,    &
     & -.73340675D-01,-.37801823D-01,-.10859144,.31166811D-01/
      Data C2/-.31909428,.30018072D-02,.46023855D-01,.71909687D-02/
      Data A3/1.1604253,0.0,.34303967,0.0,.69740185D-02,0.0,            &
     & -.57031699D-01,0.0,.39832610D-02/
      Data B3/.59719365,0.0,-.59718639D-01,0.0,-.80399468D-01,0.0,      &
     & .13167478D-01,0.0/
      Data C3/-.17950859,0.0,.15223419,0.0/
      pi180 = ATAN(1.)/45.
!...  sine and cosine of Ls
      sals = SIN(pi180*als)
      cals = COS(pi180*als)
!...  latitude frequency pi/90
      pi90 = 2.*pi180
!...  f180 = Fourier terms for pi/90 frequency harmonics
      f180(1) = SIN(pi90*alat)
      f180(2) = COS(pi90*alat)
      f180(3) = SIN(2.*pi90*alat)
      f180(4) = COS(2.*pi90*alat)
      f180(5) = SIN(3.*pi90*alat)
      f180(6) = COS(3.*pi90*alat)
      f180(7) = SIN(4.*pi90*alat)
      f180(8) = COS(4.*pi90*alat)
!...  latitude frequency pi/120
      pi120 = 1.5*pi180
!...  f240 = Fourier terms for pi/120 frequency harmonics
      f240(1) = SIN(pi120*alat)
      f240(2) = COS(pi120*alat)
      f240(3) = SIN(2.*pi120*alat)
      f240(4) = COS(2.*pi120*alat)
      f240(5) = SIN(3.*pi120*alat)
      f240(6) = COS(3.*pi120*alat)
      f240(7) = SIN(4.*pi120*alat)
      f240(8) = COS(4.*pi120*alat)
!...  Compute gam(1) = lapse rate for 0-5 km
      A = A1(0)
      B = 0.
      C = 0.
      Do 10 i = 1,8
        A = A + A1(i)*f180(i)
   10 B = B + B1(i)*f240(i)
      Do 20 i = 1,4
   20 C = C + C1(i)*f240(i)
      gam(1) = A + B*sals + C*cals
!...  Compute gam(2) = lapse rate for 5-15 km
      A = A2(0)
      B = 0.
      C = 0.
      Do 30 i = 1,8
        A = A + A2(i)*f180(i)
   30 B = B + B2(i)*f240(i)
      Do 40 i = 1,4
   40 C = C + C2(i)*f240(i)
      gam(2) = A + B*sals + C*cals
!...  Compute gam(3) = lapse rate for 15-30 km
      A = A3(0)
      B = 0.
      C = 0.
      Do 50 i = 1,8
        A = A + A3(i)*f180(i)
   50 B = B + B3(i)*f240(i)
      Do 60 i = 1,4
   60 C = C + C3(i)*f240(i)
      gam(3) = A + B*sals + C*cals
!...  Compute gam(4) and gam(5) = lapse rates 30-50 km and 50-75 km
      gam(4) = 1.19
      gam(5) = 0.44
      Return
      END
