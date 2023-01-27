!$JB2008_call
      SUBROUTINE JB2008_call(MJDSEC,FSEC,    &
        &      ALTM,PHI,XLAMB, RHO, DRHODZ,  &
        &      COSPSI, n_dens_temp )
      
      
!********1*********2*********3*********4*********5*********6*********7**
! JB2008_call             2022/02/09                by Zach Waldron
!
! FUNCTION:
!    THIS ROUTINE SERVES AS A SHELL FOR CALLING THE JB2008 DENSITY MODEL
!
!
! I/O PARAMETERS:
!
!   NAME        I/O  A/S          DESCRIPTION OF PARAMETERS
!   ------      ---  ---          ------------------------------------------------
!   MJDSEC       I    S           TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!   FSEC         I    S           FRACTIONAL REMAINING SECONDS
!   ALTM         I    S           ALTITUDE IN METERS.
!   PHI          I    S           GEODETIC LATITUDE IN RADIANS.
!   XLAMB        I    S           GEODETIC LONGITUDE IN RADIANS.
!   RHO         I/0   S           DENSITY OF ATMOSPHERE AS GIVEN BY MSIS MODEL
!   DRHODZ      I/0   S           PARTIAL DERIVATIVE OF RHO WITH HEIGHT.
!   COSPSI       I    S           Cosine of GEOCENTRIC LATITUDE
!   n_dens_temp  I    A(alloc.)   Array containing the various outputs from JB2008
!                                    nden(N2,O2,O,Ar,He,H), RHO, TEMP(x2)
!   -------------------------------------------------------------------

! COMMENTS:
!
! REFERENCES:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2
      
      !!!*CEARTH* GEOMETRICAL EARTH CONSTANTS USED FOR MEAS. PROCESSI CONSTANTS  ADOPTED IN GRS80
!            AEG    -  SEMI MAJOR AXIS OF TRACKING BODY (EARTH)
!            AEGSQ  -  AEG * AEG
!            FG     -  FLATTENING OF EARTH ELLIPSOID
!            EGSQ   -  SQUARE OF ECCENTRICITY OF ELLIPSOID
      !COMMON/CEARTH/AEG,FG,EGSQ

      DIMENSION SUN(2),SAT(3),TEMP_jb2008(2)
      REAL(8), dimension(6) :: ALN
      REAL(8) :: RHO_jb2008
      PARAMETER (PI = 3.1415927D0)
      PARAMETER (DG2RD = 3.1415927D0 / 180.D0)
      PARAMETER ( ONE = 1.D0 )

      parameter ( xjd0 = 2400000.5d0 )
      parameter ( half =  0.5d0 )
      parameter ( d122  =   122.1d0 )
      parameter ( d36525 =  365.25d0 )
      parameter ( d30600 =  30.6001d0 )
      DATA RADDEG/57.29577951D0/               


     !Parameters for calculating DRHODZ
      COMMON/PARMB/GSURF,RE
      DATA RGAS/831.4D0/,ZL/120.D0/
     !% amu's or molar mass:g/mol; He, O, N2, O2, Ar, H, N,O
      DATA He_amu/4.002602D0/
      DATA O_amu/15.9994D0/
      DATA N2_amu/28.0134D0/
      DATA O2_amu/31.9988D0/
      DATA Ar_amu/39.948D0/
      DATA H_amu/1.0079D0/
      !DATA N_amu/14.0067D0/



      !!!  Try to fix the timing stuff
      REAL(8) :: IJDSEC
      REAL(8) :: AMJD
      CHARACTER(len = 6) i_YYMMDD
      CHARACTER(len = 6) i_HHMMSS
      CHARACTER(len = 12) i_YYMMDDHHMMSS

      REAL(8), DIMENSION(9) ::  n_dens_temp


!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      kin = kin+1


      IJDSEC=float(MJDSEC)+FSEC     
      AMJD=(IJDSEC/86400.0D0)+(TMGDN2+ 0.10D0)



      !!!!   USE THE CALCULATIONS IN MJDYMD.f90 to calcualte the integer
      !!!!   VALUES FOR YEAR, MONTH, DAY, HOUR, MIN, SEC
      !!!!    ---- but  we have to actually use DOY(day of year) instead of Month&Day
      iDOY=DOY(MJDSEC,FSEC)   !-ONE    ----- zach removed subtract one...
      MODJD=MJDSEC+FSEC
      IDY=(MODJD/86400)+INT(TMGDN2+0.1)  ! CONVERT FROM MJDS TO MJD
      xjd = IDY + xjd0
      c  = INT( xjd + half ) + 1537
      nd = INT( (c - d122) / d36525 )
      e  = INT( d36525 * nd )
      nf = INT( ( c - e ) / d30600 )
      iday   = c - e - INT( d30600 * nf ) + frac( xjd + half )
      imonth = nf - 1 - 12 * INT( nf / 14 )
      iyear  = nd - 4715 - INT(  ( 7 + imonth ) / 10 )
      isecs  = MOD(MODJD,86400)
      ihour  = isecs/3600
      imin   = (isecs-ihour*3600)/60
      isec   = isecs-ihour*3600-imin*60


      !C     CONVERT TIME TO DAYS SINCE 1950 JAN 0 (Jan 31,1949 0 UT) AND MJD
      IF (iyear.GT.1900) iyear = (iyear-2000) + 100
      IF (iyear.LT.50) iyear = iyear + 100
      IYY = ((iyear-1)/4-12)
      IYY = (iyear-50)*365 + IYY
      D1950 = IYY*1.D0 + iDOY*1.D0+ihour/24.D0+imin/1440.D0+isec/86400.D0
      !AMJD2 = D1950 + 33281.0D0



      !     READ SOLAR INDICES
      !     USE 1 DAY LAG FOR F10 AND S10 FOR JB2008
      DLAG = 1.
      T1950 = D1950 - DLAG
      CALL SOLFSMY (T1950,F10,F10B,S10,S10B,XMXX,XMXXB,XYXX,XYXXB)
      


      !C     USE 2 DAY LAG FOR M10 FOR JB2008
      DLAG = 2.
      T1950 = D1950 - DLAG
      CALL SOLFSMY (T1950,XFXX,XFXXB,XSXX,XSXXB,XM10,XM10B,XYXX,XYXXB)
      
      !C     USE 5 DAY LAG FOR Y10 FOR JB2008
      DLAG = 5.
      T1950 = D1950 - DLAG
      CALL SOLFSMY (T1950,XFXX,XFXXB,XSXX,XSXXB,XMXX,XMXXB,Y10,Y10B)

      !C     READ GEOMAGNETIC STORM DTC VALUE
      CALL DTCVAL (D1950,IDTCVAL)
      DSTDTC = IDTCVAL
      
      
      !!!! if COSPSI is greater than one by any amount (like even in the 12th decimal point)
      !!! it will return a nan and result in cascading errors
      if (COSPSI.GE.1) then
          WRITE(6,*) 'COSPSI too large: ', COSPSI
          WRITE(6,*) '    setting equal to 1.D0 '
          COSPSI=1.D0
      endif 
       
      ! Convert GEODETIC Lat to GEOCENTRIC
      XLAT_geodetic = PHI     !  GEODETIC in rads
      XLON_geodetic = XLAMB   !  GEODETIC in rads
      XLAT_geocentric = ACOS(COSPSI)
      XLON_geocentric = XLON_geodetic
      
 
      XLAT = XLAT_geocentric
      XLON = XLON_geocentric

      !     CONVERT POINT OF INTEREST LOCATION (RADIANS AND KM)
      GWRAS  = THETA(D1950)    !! Calcualte Greenwich right ascension angle at desired time  
      !     CONVERT LONGITUDE TO RA
      SAT(1) = DMOD(GWRAS + XLON + 2.D0*PI, 2.D0*PI)
      SAT(2) = XLAT 
      SAT(3) = ALTM/1000.D0                                   ! Satellite altitude in KM
      
       
      !     GET SUN LOCATION (RAD)
      CALL SUNPOS(AMJD,SOLRAS,SOLDEC)
      SUN(1) = SOLRAS
      SUN(2) = SOLDEC

            
      ! if(kin.eq.1) WRITE(6,*) '*******************************************************'
      ! if(kin.eq.1) WRITE(6,*) '****************** [JB2008_call.f90] ******************'
      ! if(kin.eq.1) WRITE(6,*) ' '
      ! if(kin.eq.1) WRITE(6,*) 'INPUTS to CALL JB2008()     '
      ! if(kin.eq.1) WRITE(6,*) '   AMJD      ', AMJD
      ! if(kin.eq.1) WRITE(6,*) '   SUN       ', SUN
      ! if(kin.eq.1) WRITE(6,*) '   SAT       ', SAT
      ! if(kin.eq.1) WRITE(6,*) '   F10       ', F10
      ! if(kin.eq.1) WRITE(6,*) '   F10B      ', F10B
      ! if(kin.eq.1) WRITE(6,*) '   S10       ', S10
      ! if(kin.eq.1) WRITE(6,*) '   S10B      ', S10B
      ! if(kin.eq.1) WRITE(6,*) '   XM10      ', XM10
      ! if(kin.eq.1) WRITE(6,*) '   XM10B     ', XM10B
      ! if(kin.eq.1) WRITE(6,*) '   Y10       ', Y10
      ! if(kin.eq.1) WRITE(6,*) '   Y10B      ', Y10B
      ! if(kin.eq.1) WRITE(6,*) '   DSTDTC    ', DSTDTC
      ! if(kin.eq.1) WRITE(6,*) '   TEMP_jb2008      ', TEMP_jb2008
      ! if(kin.eq.1) WRITE(6,*) '   RHO_jb2008       ', RHO_jb2008

            ! Computes density in units of kg/m^3 -- Returns density through RHO
            !           AMJD   : Date and Time, in modified Julian Days
            !                    and Fraction (MJD = JD-2400000.5)
            !           SUN(1) : Right Ascension of Sun (radians)
            !           SUN(2) : Declination of Sun (radians)
            !           SAT(1) : Right Ascension of Position (radians)
            !           SAT(2) : Geocentric Latitude of Position (radians)
            !           SAT(3) : Height of Position (km)
            !           F10    : 10.7-cm Solar Flux (1.0E-22*Watt/(M**2*Hertz))
            !                    (Tabular time 1.0 day earlier)
            !           F10B   : 10.7-cm Solar Flux, ave.
            !                    81-day centered on the input time
            !                    (Tabular time 1.0 day earlier)
            !           S10    : EUV index (26-34 nm) scaled to F10
            !                    (Tabular time 1.0 day earlier)
            !           S10B   : EUV 81-day ave. centered index
            !                    (Tabular time 1.0 day earlier)
            !           XM10   : MG2 index scaled to F10
            !                    (Tabular time 2.0 days earlier)
            !           XM10B  : MG2 81-day ave. centered index
            !                    (Tabular time 2.0 days earlier)
            !           Y10    : Solar X-Ray & Lya index scaled to F10
            !                    (Tabular time 5.0 days earlier)
            !           Y10B   : Solar X-Ray & Lya 81-day ave. centered index
            !                    (Tabular time 5.0 days earlier)
            !           DSTDTC : Temperature change computed from Dst index
      CALL JB2008(AMJD,SUN,SAT,                   &
                &   F10,F10B,S10,S10B,XM10,XM10B, &
                &   Y10,Y10B,DSTDTC,              &
                &   TEMP_jb2008,RHO_jb2008,ALN)
      
                !   TEMP(1): Exospheric Temperature above Input Position (deg K)
                !   TEMP(2): Temperature at Input Position (deg K)
                !   RHO    : Total Mass-Desnity at Input Position (kg/m**3)
                !   ALN(1) : Natural Log Number Density [N2] (ln(molecules/m**3))
                !   ALN(2) : Natural Log Number Density [O2] (ln(molecules/m**3))
                !   ALN(3) : Natural Log Number Density [O]  (ln(molecules/m**3))
                !   ALN(4) : Natural Log Number Density [Ar] (ln(molecules/m**3))
                !   ALN(5) : Natural Log Number Density [He] (ln(molecules/m**3))
                !   ALN(6) : Natural Log Number Density [H]  (ln(molecules/m**3))


      n_dens_temp(1) = exp(ALN(1)) ! N2 !molecules/m**3)
      n_dens_temp(2) = exp(ALN(2)) ! O2 !molecules/m**3)
      n_dens_temp(3) = exp(ALN(3)) ! O !molecules/m**3)
      n_dens_temp(4) = exp(ALN(4)) ! Ar !molecules/m**3)
      n_dens_temp(5) = exp(ALN(5)) ! He !molecules/m**3)
      n_dens_temp(6) = exp(ALN(6)) ! H !molecules/m**3)
      n_dens_temp(7) = RHO_jb2008     !   RHO    : Total Mass-Desnity at Input Position (kg/m**3)
      n_dens_temp(8) = TEMP_jb2008(1) !   TEMP(1): Exospheric Temperature above Input Position (deg K)
      n_dens_temp(9) = TEMP_jb2008(2) !   TEMP(2): Temperature at Input Position (deg K)
      
      if(kin.eq.1) WRITE(6,*) '     * JB2008 Outputs '
      if(kin.eq.1) WRITE(6,*) '          - N2            ', n_dens_temp(1)
      if(kin.eq.1) WRITE(6,*) '          - O2            ', n_dens_temp(2)
      if(kin.eq.1) WRITE(6,*) '          - O             ', n_dens_temp(3)
      if(kin.eq.1) WRITE(6,*) '          - Ar            ', n_dens_temp(4)
      if(kin.eq.1) WRITE(6,*) '          - He            ', n_dens_temp(5)
      if(kin.eq.1) WRITE(6,*) '          - H             ', n_dens_temp(6)
      ! if(kin.eq.1) WRITE(6,*) ' '
      if(kin.eq.1) WRITE(6,*) '          - RHO_jb2008    ', n_dens_temp(7)
      if(kin.eq.1) WRITE(6,*) '          - Exosph. Temp. ', n_dens_temp(8)
      if(kin.eq.1) WRITE(6,*) '          - Ambient Temp  ', n_dens_temp(9)
      ! if(kin.eq.1) WRITE(6,*) ' '

       ! Return the density back to the DRAG routine
       RHO = RHO_jb2008



    !!!!  CHECK IF RHO is a NAN.  Nan will never equal itself so this method checks for nan
       if (RHO.NE.RHO) then
           WRITE(6,*) '*****   ERROR in JB2008_call.f90   *****'
           WRITE(6,*) '        JB2008 returned RHO as NAN      '
           WRITE(6,*) '           RHO = ', RHO_jb2008
           
           
           WRITE(6,*) 'INPUTS to CALL JB2008()     '
           WRITE(6,*) '   AMJD      ', AMJD
           WRITE(6,*) '   SUN       ', SUN
           WRITE(6,*) '   SAT       ', SAT
           WRITE(6,*) '   COSPSI (sat(2))  ', COSPSI
           WRITE(6,*) '   ACOS(COSPSI)     ',  ACOS(COSPSI)
           WRITE(6,*) '   F10       ', F10
           WRITE(6,*) '   F10B      ', F10B
           WRITE(6,*) '   S10       ', S10
           WRITE(6,*) '   S10B      ', S10B
           WRITE(6,*) '   XM10      ', XM10
           WRITE(6,*) '   XM10B     ', XM10B
           WRITE(6,*) '   Y10       ', Y10
           WRITE(6,*) '   Y10B      ', Y10B
           WRITE(6,*) '   DSTDTC    ', DSTDTC
           WRITE(6,*) '   TEMP_jb2008      ', TEMP_jb2008
           WRITE(6,*) '   RHO_jb2008       ', RHO_jb2008
           WRITE(6,*) 'OUTPUTS ------'
           WRITE(6,*) '   N2      ', n_dens_temp(1)
           WRITE(6,*) '   O2      ', n_dens_temp(2)
           WRITE(6,*) '   O       ', n_dens_temp(3)
           WRITE(6,*) '   Ar      ', n_dens_temp(4)
           WRITE(6,*) '   He      ', n_dens_temp(5)
           WRITE(6,*) '   H       ', n_dens_temp(6)
           WRITE(6,*) ' '
           WRITE(6,*) '   RHO_jb2008    ', n_dens_temp(7)
           WRITE(6,*) '   Exosph. Temp. ', n_dens_temp(8)
           WRITE(6,*) '   Ambient Temp  ', n_dens_temp(9)
           WRITE(6,*) '*********************************************'
       endif
           
           
           

           
           
           
       !!!!   CALCUALTE DRHODZ
       GSURF_ZL = GSURF*(RE/(RE + ZL) )**2
       TERM1 = ((-1.66D-24*GSURF_ZL)/RGAS)    
       ! !## convert from 1/m^3 to 1/cm^3
       TERM_sp = (n_dens_temp(1)*0.0000010D0*N2_amu    &   
    &           + n_dens_temp(2)*0.0000010D0*O2_amu    &   
    &           + n_dens_temp(3)*0.0000010D0*O_amu     &   
    &           + n_dens_temp(4)*0.0000010D0*Ar_amu    &   
    &           +  n_dens_temp(5)*0.0000010D0*He_amu    &   
    &           +  n_dens_temp(6)*0.0000010D0*H_amu     &   
    &           + 0.0D0 )*(1.D0/TEMP_jb2008(2))
       TERMnorm1 = 1.D0/(1.D0 + ZL/RE)**2
       TERMnorm2 = ((RE+ZL)/(RE+(ALTM/1000.D0)))**2    
       DRHODZ =TERM1*(TERM_sp)*TERMnorm1*TERMnorm2

!       DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.









   99 RETURN
      END    
