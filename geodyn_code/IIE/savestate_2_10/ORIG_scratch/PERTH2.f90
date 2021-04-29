      SUBROUTINE PERTH2( DLAT,DLON,TIME,TIDE,ISDATA,ldeall)
!
!
!
!  Function -  to compute the ocean tidal height at a given time
!              and location from grids of harmonic constants.
!              Current version uses the 8 largest constituents in the
!              semidiurnal & diurnal bands, with other tides inferred.
!              (Long period tides are NOT computed by this routine.)
!
!  Language - Fortran 77
!
!  Arguments -
!     name      type  I/O               description
!     ----      ----  ---               -----------
!     DLAT       D     I    north latitude (in degrees) for desired
!                           location.
!
!     DLON       D     I    east longitude (in degrees).
!
!     TIME       D     I    desired UTC time, in (decimal) Modified Juli
!                           e.g., 1 Jan 1990 noon = 47892.5
!
!     TIDE       D     O    computed tidal height.  The units will be
!                           the same as the 'amplitude' units on the
!                           input tidal grids (usually cm).
!
!     ISDATA     L     O    logical denoting whether tide data exist at
!                           desired location.  If FALSE, then TIDE is
!                           not modified.
!
!
!  Usage notes -
!     All 8 input tide files should be concatenated, in order of frequen
!     e.g., do:
!         cat q1.d o1.d p1.d k1.d n2.d m2.d s2.d k2.d > fort.30
!     (or the equivalent for your operating system).
!
!  Processing logic -
!     Tidal constants at desired location are found by bilinear
!     interpolation from input grid files.  The astronomical mean
!     longitudes (determined by ASTROL) are most accurate for the
!     period 1990 - 2010.  Nodal corrections are applied to all
!     lunar tides.  Sixteen minor tides are inferred from major tides.
!
!  File references -
!     Input tidal grids are read on first call via unit LU (set in DATA)
!
!  Important local variables -
!     ARRAY - holds inphase & quadrature tidal constants as follows:
!        ARRAY(i,j,k,l) where
!        i,j are longitude/latitude indices.
!        k = 1,2  for Hcos(G) or Hsin(G).
!        l = 1,...,8 (max) - for each tidal constituent.
!     SHPN - holds the mean astronomical longitudes of interest;
!        this array is equivalenced to S, H, P, & OMEGA
!
!     Programming note: This routine is written for general use, which
!     is not necessarily efficient for all applications.
!     If it is desired to compute tidal heights at, e.g., every 1-sec
!     observation along an arc, the program could be speeded up by
!     use of several approximations.  Contact author for details.
!     For machines like the Cray, the DOUBLE PRECISION should be removed
!
!  Error processing -
!     An attempt is made is ensure that the input grids are read in the
!     correct order; this procedure looks for the constituent names
!     in the TITLE part of the input files.
!
!  Technical references -
!     A. T. Doodson & H. Warburg, Admiralty Manual of Tides, HMSO, 1941.
!
!  History -
!   version   date    programmer        change description
!   -------   ----    ----------        ------------------
!     1.0    6/03/93     R Ray    Initial version.
!     1.1    1/10/95     R Ray    Fixed missing declaration of NINETY.
!     1.2    3/15/95     R Ray    TLOAD ensures consistent nulls in all
!     1.3    5/05/95     R Ray    Keep last ISDATA internally.
!     2.0    5/19/95     R Ray    Infer minor tides; remove SELECT argum
!                                 Bypass nodal calculations if time < 30
!     2.1    1/13/98     R Ray    Fixed bad argument to "pi1" tide.
!
!
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NINETY
      PARAMETER (MX=720, MY=361, NG=8, NT=26)
      LOGICAL    INIT,ISDATA,WRAP,ISDATA0,LDEALL

      DOUBLE PRECISION, ALLOCATABLE :: ARRAY(:,:,:,:)
      LOGICAL, ALLOCATABLE :: SELECT(:)
      ALLOCATABLE ARG(:), F(:), U(:)

      DOUBLE PRECISION       LATMIN,LATMAX,LONMIN,LONMAX,UNDEF,H12(2,NT)
      DOUBLE PRECISION       SINN,SIN2N,COSN,COS2N
      DIMENSION  SHPN(4)
      EQUIVALENCE (SHPN(1),S),(SHPN(2),H),(SHPN(3),P),(SHPN(4),OMEGA)

      PARAMETER (UNDEF=99999.0, PI=3.141592654D0, RAD=PI/180.D0)
      PARAMETER (TWO=2.D0, THREE=3.D0, FOUR=4.D0)
      PARAMETER (FIFTEN=15.D0, THIRTY=30.D0, NINETY=90.D0, PP=282.8D0)
      DATA       DLAT0,DLON0,TIME0,TIME1/4*999.D0/
      DATA       INIT/.TRUE./, LU/30/

      SAVE

!     DEALLOCATE ARRAYS IF REQUESTED
!     -----------------------------------------------
      IF(LDEALL) THEN

       print *,'deallocating in perth2_new'

      DEALLOCATE(ARRAY,SELECT,ARG,                                      &
     &         F,U,STAT=memgot)

      print *,'perth2: memgot: ',memgot
      IF(MEMGOT.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN PERTH2'
       WRITE(6,*) 'TROUBLE DEALLOCATING MEMORY FOR GOT99 GRID'
       WRITE(6,*) 'MEMGOT IS: ', MEMGOT
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

      RETURN

      ENDIF

!     on first call, read grids of harmonic constants
!     -----------------------------------------------

      IF (INIT) THEN

         INIT = .FALSE.

       print *,'allocating in perth2_new',NT

      ALLOCATE(ARRAY(MX,MY,2,NG),SELECT(NG),ARG(NT),                    &
     &         F(NT),U(NT),STAT=memgot)

      print *,'perth2: memgot: ',memgot
      IF(MEMGOT.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN PERTH2'
       WRITE(6,*) 'TROUBLE ALLOCATING MEMORY FOR GOT99 GRID'
       WRITE(6,*) 'MEMGOT IS: ', MEMGOT
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!...OPEN GOT99 GRID FILE
      OPEN(unit=LU,file='got99_grid.dat',status='old',action='read',    &
     &     form='formatted',iostat=ioerror)
      IF(ioerror > 0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN LDGOT9'
       WRITE(6,*) 'TROUBLE OPENING got99_grid.dat'
       WRITE(6,*) 'ioerror IS: ', ioerror
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!...LOAD ARRAYS

      DO I=1,NG
       SELECT(I) = .TRUE.
      END DO

      print *,'call to tload:  '
      print *,'mx,my,nx,ny,lu,undef: ',mx,my,nx,ny,lu,undef
      print *,'ngrids,select: ',ngrids,select
      print *,'latmin,latmax,lonmin,lonmax:',latmin,latmax,lonmin,lonmax

         CALL TLOAD( ARRAY,MX,MY, SELECT, LU, UNDEF,                    &
     &               NX,NY,LATMIN,LATMAX,LONMIN,LONMAX,NGRIDS )
         DX = (LONMAX - LONMIN)/(NX - 1)
         WRAP = ABS(LONMAX - LONMIN - 360.) .LT. 2.0*DX
         DO 10 I=1,NT
            F(I) = 1.D0
            U(I) = 0.D0
   10    CONTINUE

       CLOSE(30)
      ENDIF
!
!     determine tidal constants at this location
!     ------------------------------------------
      IF (DLAT.NE.DLAT0 .OR. DLON.NE.DLON0) THEN
          CALL GRSINT( ARRAY,MX,MY,2*NGRIDS,NX,NY,UNDEF,                &
     &                 LATMIN,LATMAX,LONMIN,LONMAX,WRAP,                &
     &                 DLAT,DLON,H12,ISDATA )
          DLAT0 = DLAT
          DLON0 = DLON
          ISDATA0 = ISDATA
!
!         infer minor tides at this location
!         ----------------------------------
          H12(1, 9) = 0.263 *H12(1,1) - 0.0252*H12(1,2)
          H12(2, 9) = 0.263 *H12(2,1) - 0.0252*H12(2,2)
          H12(1,10) = 0.297 *H12(1,1) - 0.0264*H12(1,2)
          H12(2,10) = 0.297 *H12(2,1) - 0.0264*H12(2,2)
          H12(1,11) = 0.164 *H12(1,1) + 0.0048*H12(1,2)
          H12(2,11) = 0.164 *H12(2,1) + 0.0048*H12(2,2)
          H12(1,12) = 0.0140*H12(1,2) + 0.0101*H12(1,4)
          H12(2,12) = 0.0140*H12(2,2) + 0.0101*H12(2,4)
          H12(1,13) = 0.0389*H12(1,2) + 0.0282*H12(1,4)
          H12(2,13) = 0.0389*H12(2,2) + 0.0282*H12(2,4)
          H12(1,14) = 0.0064*H12(1,2) + 0.0060*H12(1,4)
          H12(2,14) = 0.0064*H12(2,2) + 0.0060*H12(2,4)
          H12(1,15) = 0.0030*H12(1,2) + 0.0171*H12(1,4)
          H12(2,15) = 0.0030*H12(2,2) + 0.0171*H12(2,4)
          H12(1,16) =-0.0015*H12(1,2) + 0.0152*H12(1,4)
          H12(2,16) =-0.0015*H12(2,2) + 0.0152*H12(2,4)
          H12(1,17) =-0.0065*H12(1,2) + 0.0155*H12(1,4)
          H12(2,17) =-0.0065*H12(2,2) + 0.0155*H12(2,4)
          H12(1,18) =-0.0389*H12(1,2) + 0.0836*H12(1,4)
          H12(2,18) =-0.0389*H12(2,2) + 0.0836*H12(2,4)
          H12(1,19) =-0.0431*H12(1,2) + 0.0613*H12(1,4)
          H12(2,19) =-0.0431*H12(2,2) + 0.0613*H12(2,4)
          H12(1,20) = 0.264 *H12(1,5) - 0.0253*H12(1,6)
          H12(2,20) = 0.264 *H12(2,5) - 0.0253*H12(2,6)
          H12(1,21) = 0.298 *H12(1,5) - 0.0264*H12(1,6)
          H12(2,21) = 0.298 *H12(2,5) - 0.0264*H12(2,6)
          H12(1,22) = 0.165 *H12(1,5) + 0.00487*H12(1,6)
          H12(2,22) = 0.165 *H12(2,5) + 0.00487*H12(2,6)
          H12(1,23) = 0.0040*H12(1,6) + 0.0074*H12(1,7)
          H12(2,23) = 0.0040*H12(2,6) + 0.0074*H12(2,7)
          H12(1,24) = 0.0131*H12(1,6) + 0.0326*H12(1,7)
          H12(2,24) = 0.0131*H12(2,6) + 0.0326*H12(2,7)
          H12(1,25) = 0.0033*H12(1,6) + 0.0082*H12(1,7)
          H12(2,25) = 0.0033*H12(2,6) + 0.0082*H12(2,7)
          H12(1,26) = 0.0585*H12(1,7)
          H12(2,26) = 0.0585*H12(2,7)
      ELSE
          ISDATA = ISDATA0
      ENDIF
      IF (.NOT.ISDATA) RETURN
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
         F(4) = 1.006 + 0.115*COSN - 0.009*COS2N
         F(5) = 1.000 - 0.037*COSN
         F(6) = F(5)
         F(8) = 1.024 + 0.286*COSN + 0.008*COS2N
         F( 9) = SQRT((1.0 + 0.189*COSN - 0.0058*COS2N)**2 +            &
     &                (0.189*SINN - 0.0058*SIN2N)**2)
         F(10) = F(9)
         F(11) = F(9)
         F(12) = SQRT((1.0 + 0.185*COSN)**2 + (0.185*SINN)**2)
         F(13) = SQRT((1.0 + 0.201*COSN)**2 + (0.201*SINN)**2)
         F(14) = SQRT((1.0 + 0.221*COSN)**2 + (0.221*SINN)**2)
         F(18) = SQRT((1.0 + 0.198*COSN)**2 + (0.198*SINN)**2)
         F(19) = SQRT((1.0 + 0.640*COSN + 0.134*COS2N)**2 +             &
     &                (0.640*SINN + 0.134*SIN2N)**2 )
         F(20) = SQRT((1.0 - 0.0373*COSN)**2 + (0.0373*SINN)**2)
         F(21) = F(20)
         F(22) = F(20)
         F(24) = F(20)
         F(25) = SQRT((1.0 + 0.441*COSN)**2 + (0.441*SINN)**2)

         U(1) = 10.8*SINN - 1.3*SIN2N
         U(2) = U(1)
         U(4) = -8.9*SINN + 0.7*SIN2N
         U(5) = -2.1*SINN
         U(6) = U(5)
         U(8) = -17.7*SINN + 0.7*SIN2N
         U(9) = ATAN2(0.189*SINN - 0.0058*SIN2N,                        &
     &                1.0 + 0.189*COSN - 0.0058*SIN2N)/RAD
         U(10) = U(9)
         U(11) = U(9)
         U(12) = ATAN2( 0.185*SINN, 1.0 + 0.185*COSN)/RAD
         U(13) = ATAN2(-0.201*SINN, 1.0 + 0.201*COSN)/RAD
         U(14) = ATAN2(-0.221*SINN, 1.0 + 0.221*COSN)/RAD
         U(18) = ATAN2(-0.198*SINN, 1.0 + 0.198*COSN)/RAD
         U(19) = ATAN2(-0.640*SINN - 0.134*SIN2N,                       &
     &                 1.0 + 0.640*COSN + 0.134*COS2N)/RAD
         U(20) = ATAN2(-0.0373*SINN, 1.0 - 0.0373*COSN)/RAD
         U(21) = U(20)
         U(22) = U(20)
         U(24) = U(20)
         U(25) = ATAN2(-0.441*SINN, 1.0 + 0.441*COSN)/RAD
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
