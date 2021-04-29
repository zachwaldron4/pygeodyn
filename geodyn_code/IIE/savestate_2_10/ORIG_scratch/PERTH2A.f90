!$PERTH2A
      SUBROUTINE PERTH2A( TIME,TIDE,H12 )
!********1*********2*********3*********4*********5*********6*********7**
! PERTH2A          97/08/27            9708.0    PGMR -
!
!
! FUNCTION:    to compute the ocean tidal height at a given time
!              from a set of tidal harmonic constants.
!              Current version uses 26 largest constituents in the
!              semidiurnal & diurnal bands, with other tides inferred.
!              (Long period tides are NOT computed by this routine.)
!
!                   Name derivation - PREdict Tidal Heights
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   TIME    I          desired UTC time, in (decimal) MJD
!                      e.g., 1 Jan 1990 noon = 47892.5
!
!   TIDE    O          computed tidal height.  The units will be
!                      the same as the 'amplitude' units on the
!                      input tidal grids (usually cm).
!
!   H12     I          inphase & quadrature harmonic constants
!                      for 8 major tides and 18 minor tides
!                      Loaded in order of frequency (i.e.:
!                      Q1,O1,P1,K1,N2,M2,S2,K2; then minors).
!
! COMMENTS:
!
!  Important local variables -
!     SHPN - holds the mean astronomical longitudes of interest;
!        this array is equivalenced to S, H, P, & OMEGA
!
!  Technical references -
!     A. T. Doodson & H. Warburg, Admiralty Manual of Tides, HMSO, 1941.
!
!  History -
!   version   date    programmer        change description
!   -------   ----    ----------        ------------------
!     1.0    6/03/93     R Ray    Initial version.
!     2.0    5/19/95     R Ray    Infer minor tides; remove SELECT argum
!                                 Bypass nodal calculations if time < 30
!     2a     1/15/96     R Ray    Highly modified for use with PFTIDE.
!     2a.1   5/30/96     R Ray    Fixed bug that left out 5 small tides.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
      DOUBLE PRECISION  NINETY
      PARAMETER (NT=26)
      DIMENSION  H12(2,NT)
      DIMENSION  SHPN(4), ARG(NT), F(NT), U(NT)
      EQUIVALENCE (SHPN(1),S),(SHPN(2),H),(SHPN(3),P),(SHPN(4),OMEGA)
      PARAMETER (UNDEF=99999.0, PI=3.141592654D0, RAD=PI/180.D0)
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0, FOUR=4.D0)
      PARAMETER (FIFTEN=15.D0, THIRTY=30.D0, NINETY=90.D0, PP=282.8D0)
      DATA       TIME0,TIME1/2*999.D0/
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!     determine equilibrium tidal arguments
!     -------------------------------------
      IF (TIME.NE.TIME0) THEN
         TIME0 = TIME
         HOUR = (TIME - INT(TIME))*24.D0
         T1 = FIFTEN*HOUR
         T2 = THIRTY*HOUR
         CALL ASTROL( TIME, SHPN )
                                                  ! Q1
         ARG(1) = T1 + H - THREE*S + P - NINETY
                                                  ! O1
         ARG(2) = T1 + H - TWO*S - NINETY
                                                  ! P1
         ARG(3) = T1 - H - NINETY
                                                  ! K1
         ARG(4) = T1 + H + NINETY
                                                  ! N2
         ARG(5) = T2 + TWO*H - THREE*S + P
                                                  ! M2
         ARG(6) = T2 + TWO*H - TWO*S
                                                  ! S2
         ARG(7) = T2
                                                  ! K2
         ARG(8) = T2 + TWO*H
                                                       ! 2Q1
         ARG( 9) = T1 - FOUR*S + H + TWO*P - NINETY
                                                       ! sigma1
         ARG(10) = T1 - FOUR*S + THREE*H - NINETY
                                                       ! rho1
         ARG(11) = T1 - THREE*S + THREE*H - P - NINETY
                                                       ! M1
         ARG(12) = T1 - S + H - P + NINETY
                                                       ! M1
         ARG(13) = T1 - S + H + P + NINETY
                                                       ! chi1
         ARG(14) = T1 - S + THREE*H - P + NINETY
                                                       ! pi1
         ARG(15) = T1 - TWO*H + PP - NINETY
                                                       ! phi1
         ARG(16) = T1 + THREE*H + NINETY
                                                       ! theta1
         ARG(17) = T1 + S - H + P + NINETY
                                                       ! J1
         ARG(18) = T1 + S + H - P + NINETY
                                                       ! OO1
         ARG(19) = T1 + TWO*S + H + NINETY
                                                       ! 2N2
         ARG(20) = T2 - FOUR*S + TWO*H + TWO*P
                                                       ! mu2
         ARG(21) = T2 - FOUR*S + FOUR*H
                                                       ! nu2
         ARG(22) = T2 - THREE*S + FOUR*H - P
                                                       ! lambda2
         ARG(23) = T2 - S + P + 180.D0
                                                       ! L2
         ARG(24) = T2 - S + TWO*H - P + 180.D0
                                                       ! L2
         ARG(25) = T2 - S + TWO*H + P
                                                       ! T2
         ARG(26) = T2 - H + PP
      ENDIF
!
!     determine nodal corrections f and u
!        Note: Update this code next iteration of model  -RDR
!     -----------------------------------
      IF (ABS(TIME-TIME1).GT.30.D0) THEN
         TIME1 = TIME
         SINN = SIN(OMEGA*RAD)
         COSN = COS(OMEGA*RAD)
         SIN2N = SIN(TWO*OMEGA*RAD)
         COS2N = COS(TWO*OMEGA*RAD)
         F(1) = 1.009 + 0.187*COSN - 0.015*COS2N
         F(2) = F(1)
         F(3) = ONE
         F(4) = 1.006 + 0.115*COSN - 0.009*COS2N
         F(5) = 1.000 - 0.037*COSN
         F(6) = F(5)
         F(7) = ONE
         F(8) = 1.024 + 0.286*COSN + 0.008*COS2N
         F( 9) = SQRT((1.0 + 0.189*COSN - 0.0058*COS2N)**2 +            &
     &                (0.189*SINN - 0.0058*SIN2N)**2)
         F(10) = F(9)
         F(11) = F(9)
         F(12) = SQRT((1.0 + 0.185*COSN)**2 + (0.185*SINN)**2)
         F(13) = SQRT((1.0 + 0.201*COSN)**2 + (0.201*SINN)**2)
         F(14) = SQRT((1.0 + 0.221*COSN)**2 + (0.221*SINN)**2)
         F(15) = ONE
         F(16) = ONE
         F(17) = ONE
         F(18) = SQRT((1.0 + 0.198*COSN)**2 + (0.198*SINN)**2)
         F(19) = SQRT((1.0 + 0.640*COSN + 0.134*COS2N)**2 +             &
     &                (0.640*SINN + 0.134*SIN2N)**2 )
         F(20) = SQRT((1.0 - 0.0373*COSN)**2 + (0.0373*SINN)**2)
         F(21) = F(20)
         F(22) = F(20)
         F(23) = ONE
         F(24) = F(20)
         F(25) = SQRT((1.0 + 0.441*COSN)**2 + (0.441*SINN)**2)
         F(26) = ONE
         U(1) = 10.8*SINN - 1.3*SIN2N
         U(2) = U(1)
         U(3) = ZERO
         U(4) = -8.9*SINN + 0.7*SIN2N
         U(5) = -2.1*SINN
         U(6) = U(5)
         U(7) = ZERO
         U(8) = -17.7*SINN + 0.7*SIN2N
         U(9) = ATAN2(0.189*SINN - 0.0058*SIN2N,                        &
     &                1.0 + 0.189*COSN - 0.0058*SIN2N)/RAD
         U(10) = U(9)
         U(11) = U(9)
         U(12) = ATAN2( 0.185*SINN, 1.0 + 0.185*COSN)/RAD
         U(13) = ATAN2(-0.201*SINN, 1.0 + 0.201*COSN)/RAD
         U(14) = ATAN2(-0.221*SINN, 1.0 + 0.221*COSN)/RAD
         U(15) = ZERO
         U(16) = ZERO
         U(17) = ZERO
         U(18) = ATAN2(-0.198*SINN, 1.0 + 0.198*COSN)/RAD
         U(19) = ATAN2(-0.640*SINN - 0.134*SIN2N,                       &
     &                 1.0 + 0.640*COSN + 0.134*COS2N)/RAD
         U(20) = ATAN2(-0.0373*SINN, 1.0 - 0.0373*COSN)/RAD
         U(21) = U(20)
         U(22) = U(20)
         U(23) = ZERO
         U(24) = U(20)
         U(25) = ATAN2(-0.441*SINN, 1.0 + 0.441*COSN)/RAD
         U(26) = ZERO
      ENDIF
!
!     sum over all tides
!     ------------------
      SUM = 0.D0
      DO 100 I=1,NT
         H1 = H12(1,I)
         H2 = H12(2,I)
         CHIU = (ARG(I) + U(I))*RAD
         SUM = SUM + H1*F(I)*COS(CHIU) + H2*F(I)*SIN(CHIU)
  100 END DO
!
      TIDE = SUM
      RETURN
      END
