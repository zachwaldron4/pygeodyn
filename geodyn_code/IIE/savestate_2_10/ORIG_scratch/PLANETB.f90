!$CEBODY
!********1*********2*********3*********4*********5*********6*********7**
! CEBODY           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: Block data routine for planetary information
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
       BLOCK DATA PLANETB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CEBODY/APL(11),APLSQ,FINVPL(11),FGP,EGSQP,EGSQPP,C1P,C2P,  &
     &              FOURCP,TWOC2P,WREFP(11),RPMEAN,RFPC20,              &
     &              RFPC40,RFPC60,BEGP,CBLTCP,                          &
     &              WAE2P,GAMMAP,FSTARP,F4P
! 1978 IUGG ADOPTED REFERENCE ELLIPSOID FOR THE EARTH
! APL IS LOADED ALSO IN DCOMFL FROM IIS DYNAMIC ARRAY AA
! FLATTENINGS AND PLANET ROTATION RATES ARE INITIALIZED HERE
!
!
!     Planetary Semi-major Axes
!
      DATA APL/                                                         &
     &            2439700.D0,                                           &
     &            6051000.D0,                                           &
     &            6378137.D0,                                           &
     &            1737400.D0,                                           &
     &            3397200.D0,                                           &
     &            71492000.D0,                                          &
     &            60268000.D0,                                          &
     &            25559000.D0,                                          &
     &            25269000.D0,                                          &
     &            1162000.D0,                                           &
     &            6.96D8 /
!      Mercury
!      Venus
!      Earth
!      Earth's Moon
!      Mars
!      Jupiter
!      Saturn
!      Uranus
!      Neptune
!      Pluto
!      Sun
!
!
!     Planetary Inverse Flattening
!
      DATA FINVPL/                                                      &
     &            1.0D15,                                               &
     &            1.0D15,                                               &
     &            298.257D0,                                            &
     &            1.0D15,                                               &
     &            192.808252D0,                                         &
     &            15.4144D0,                                            &
     &            10.2080D0,                                            &
     &            43.6160D0,                                            &
     &            53.8785D0,                                            &
     &            1.0D15,                                               &
     &            1.0D15 /
!      Mercury    (NOTE: IAU flattening is ZERO)
!      Venus      (NOTE: IAU flattening is ZERO)
!      Earth
!      Earth's Moon (NOTE: IAU flattening is ZERO)
!      Mars
!      Jupiter
!      Saturn
!      Uranus
!      Neptune
!      Pluto      (NOTE: IAU flattening is ZERO)
!      Sun
!
!
!     Planetary Rotation Rate in radians per second
!
      DATA WREFP/                                                       &
     &            1.2400125D-6,                                         &
     &            -7.3331054D-6,                                        &
     &            0.72921151D-4,                                        &
     &            2.6616994D-6,                                         &
     &            0.7088218D-4,                                         &
     &            1.7585323D-4,                                         &
     &            1.6378499D-4,                                         &
     &            1.0123719D-4,                                         &
     &            9.7722802D-5,                                         &
     &            1.1386D-5,                                            &
     &            2.8653296D-6 /
!      Mercury
!      Venus
!      Earth
!      Earth's Moon
!      Mars
!      Jupiter
!      Saturn
!      Uranus
!      Neptune
!      Pluto
!      Sun
!
      END
