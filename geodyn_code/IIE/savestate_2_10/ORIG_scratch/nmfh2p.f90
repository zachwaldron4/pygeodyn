!$NMFH2P
      subroutine nmfh2p(doy,latitu,height,elev,hmf)
!***********************************************************************
! NMFH2P                      12/00/96     9611.02      PRGM - D.D ROWLA
!                                                     UPDATE - S. LUO
!
!    FUNCTION:
!
!     Routine to compute the hydrostatic mapping function nhmf2 which
!     depends on DOY (day of year) and station position (latitude
!     and height above geoid; use ellipsoid height for now).
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   doy      I    S    - days since Dec 31
!   height   I    S    - height of site above geoid (meters)
!   latitu   I    S    - latitude (degrees)
!   elev     I    S    - elevation (degrees)
!   hmf(1)   O         - delay mapping function
!   hmf(2)   O         - d_mapping_function/d_elevation (dhmf2/d_el)
!
! comments:
!
! 931007 aen NEW nhmf2
! 951129 aen MOD nmfh2p1 Add derivative of height correction wrt elevati
!                to hmf(2).
!                NOTE change in spelling of subroutine from nhmf to nmfh
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!   a,b,c       - the a,b,and c coeffiecents in the continued fraction
!                 form of Marini
!   beta        - intermediate term in calculation
!   gamma       - intermediate term in calculation
!   sine        - sine of elevation angle
!   cose        - cos of elevation angle
!   topcon      - constant of top of mapping function to ensure
!                 that value is 1.0000 at zenith
      DOUBLE PRECISION a,b,c, beta, cose, hmf(2), gamma, sine, topcon

!   height     - height of site above geoid (meters)
!   hskm      - Height of site in kms.
!   latitu     - latitude (degrees)
!   l          - absolute latitude
!   dl         - incremental latitude from last lathmf
!   elev       - elevation (degrees)
!   epoch      - if Julian date of observation is known for the observat
!              - then epoch can be used to get day of year.
!              - (if epoch  is passed as argument, then un-comment the
!                 line converting epoch to doy.)
!   doy        - days since Dec 31
!   doyatm    - doy for atmosphere relative to Jan 28.
!   dyatmr   - doyatm in radians;
!   cost       - cosine(day of year)
!   doy2rad    - convert doy to radians

      DOUBLE PRECISION doy, latitu, height, elev
      DOUBLE PRECISION hskm, l, dl, doyatm, dyatmr, cost
      DOUBLE PRECISION doy2rad, deg2rad

!   lathmf     - latitudes at which coefficients are defined (5).
!   abcavg     - continued fraction coefficients at latitudes lathmf
!   abcamp     - amplitude of annual variation of abcavg
!   daavg, daamp, etc - incremental values for interpolation
!   aavg,  aamp,  etc - average and amplitude at latitude

      DOUBLE PRECISION lathmf(5)
      DOUBLE PRECISION abcavg(5,3), abcamp(5,3)
      DOUBLE PRECISION daavg, daamp, dbavg, dbamp, dcavg, dcamp
      DOUBLE PRECISION aavg,  aamp,  bavg,  bamp,  cavg,  camp

!   aht, bht, cht - parameters for continued fraction for height corr'n.
!   dhcdel    - derivative of height correction coefficient with elevati
!   dhtcdel - derivative of height correction with elevation

      DOUBLE PRECISION aht, bht, cht, htcrcf, htcorr
      DOUBLE PRECISION dhcdel, dhtcdel

!   define parameters used for calculating coefficients.

      data lathmf / 15, 30, 45, 60, 75/

      data abcavg /                                                     &
     &1.2769934D-3,1.2683230D-3,1.2465397D-3,1.2196049D-3,1.2045996D-3, &
     &2.9153695D-3,2.9152299D-3,2.9288445D-3,2.9022565D-3,2.9024912D-3, &
     &62.610505D-3,62.837393D-3,63.721774D-3,63.824265D-3,64.258455D-3/

      data abcamp /                                                     &
     &  0.0,   1.2709626D-5, 2.6523662D-5, 3.4000452D-5, 4.1202191D-5,  &
     &  0.0,   2.1414979D-5, 3.0160779D-5, 7.2562722D-5, 11.723375D-5,  &
     &  0.0,   9.0128400D-5, 4.3497037D-5, 84.795348D-5, 170.37206D-5/

      data aht / 2.53D-5/                                               &
     &     bht / 5.49D-3/                                               &
     &     cht / 1.14D-3/

!   conversions:

      doy2rad = 2*3.14159265/365.25
      deg2rad = 3.14159265/180.

!   convert height in meters to kilometers

      hskm  = height/1000.D0

!   If Julian date is used for epoch, then calculate day of year;
!      use 1980 Jan 0 as reference epoch.

!     doy = epoch - 2444238.5

!   to account for the six month difference in seasons between hemispher
!   add 365.25/2 days to doy if station is in the southern hemisphere.

      l = ABS(latitu)
      if (latitu .lt. 0) doy = doy + 365.25/2

! mod aen 930517 Use phase of 28 days (winter extremum corresponds to Ja
!                based on least-square fit to
!                raytrace of radiosonde data for DRT, ELP, ALB, CHH, FAI
!                MUN, and LIH.
!

      doyatm  = doy - 28.
      dyatmr = doyatm * doy2rad

!aen  debug
!     write(*,'("doy, doyatm, dyatmr = ", 3f15.6)') doy, doyatm, dyatmr
      cost = COS(dyatmr)

!   Coefficients for the continued fraction expansion for each latitude.

!   for latitudes less than 15 degrees:

      if (l .le. lathmf(1)) then
         a = abcavg(1,1)
         b = abcavg(1,2)
         c = abcavg(1,3)
      endif

!   for latitudes between 15 and 75  degrees:

      do i = 1,4
          if (l .gt. lathmf(i) .and. l .le. lathmf(i+1)) then
             dl = (l-lathmf(i))/(lathmf(i+1)-lathmf(i))
             daavg =   abcavg(i+1,1)-abcavg(i,1)
             daamp =   abcamp(i+1,1)-abcamp(i,1)
             aavg  =   abcavg(i,1) + dl*daavg
             aamp  =   abcamp(i,1) + dl*daamp
             a     = aavg - aamp*cost
!     write(*,'(" dl,daavg,daamp,aavg,aamp,a ",6e15.6)')
!    .            dl,daavg,daamp,aavg,aamp,a

             dbavg =   abcavg(i+1,2)-abcavg(i,2)
             dbamp =   abcamp(i+1,2)-abcamp(i,2)
             bavg  =   abcavg(i,2) + dl*dbavg
             bamp  =   abcamp(i,2) + dl*dbamp
             b     = bavg - bamp*cost
!     write(*,'(" dl,dbavg,dbamp,bavg,bamp,b ",6e15.6)')
!    .            dl,dbavg,dbamp,bavg,bamp,b

             dcavg =   abcavg(i+1,3)-abcavg(i,3)
             dcamp =   abcamp(i+1,3)-abcamp(i,3)
             cavg  =   abcavg(i,3) + dl*dcavg
             camp  =   abcamp(i,3) + dl*dcamp
             c     = cavg - camp*cost
!     write(*,'(" dl,dcavg,dcamp,cavg,camp,c ",6e15.6)')
!    .            dl,dcavg,dcamp,cavg,camp,c

          endif
      end do

!   for latitudes greater than 75 degrees:

      if (l .ge. lathmf(5)) then
         a = abcavg(5,1)
         b = abcavg(5,2)
         c = abcavg(5,3)
      endif

!   Now the coefficients exist; calculate for the sea level part
!   the mapping function, hmf(1), and the derivative wrt elevation
!   dhmf/d_el = hmf(2).

!   To get delay-rate correction d_tau/dt:
!      d_tau/dt = d_tau-zen/dt*hmf(1) + tau-zen*hmf(2)*d_el/dt
!      where  hmf(2)=dhmf/d_el

      sine   = SIN(elev * deg2rad)
      cose   = COS(elev * deg2rad)
      beta   = b/( sine + c )
      gamma  = a/( sine + beta)
      topcon = (1.D0 + a/(1.D0 + b/(1.D0 + c)))

      hmf(1) =     topcon / ( sine + gamma )

      hmf(2) =     -topcon*cose / ( sine + gamma )**2 *                 &
     &            ( 1.D0 - a/ ( sine + beta)**2 *                       &
     &            ( 1.D0 - b/ ( sine + c   )**2 ) )

!     write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
!    .           sine, cose, beta, gamma, topcon
!     write(*,'("hmf(1), hmf(2) = ", 2f10.4)') hmf(1), hmf(2)
!     write(*,'("hmf(1), hmf(2) = ", 2f10.4)') hmf

!   Apply height correction to mapping function and derivative wrt eleva
!
!      1) height correction coefficient is
!         1/sine(elev) - continued fraction(aht,bht,cht).
!      2) height correction is htcrcf times height in km.
!      3) height correction to derivative wrt elevation is (derivative o
!         height correction coefficient wrt elevation)Cheight in km.

      beta   = bht/( sine + cht )
      gamma  = aht/( sine + beta)
      topcon = (1.D0 + aht/(1.D0 + bht/(1.D0 + cht)))
      htcrcf = 1/sine - topcon/(sine + gamma)
      htcorr      = htcrcf * hskm
      hmf(1)       = hmf(1)     + htcorr

!    951129 The derivative of the height correction wrt elevation is add
!    to hmf(2) after Chris Jacobs pointed out the magnitude of the term.

      dhcdel   = -cose/sine**2                                          &
     &             +topcon*cose / ( sine + gamma)**2 *                  &
     &            ( 1.D0 - aht/ ( sine + beta)**2 *                     &
     &            ( 1.D0 - bht/ ( sine + cht)**2) )
      dhtcdel = dhcdel * hskm
      hmf(2)       = hmf(2)     + dhtcdel

!     write(*,'("htcrcf, htcorr, hskm, hmf(1) = ", 4f15.6)')
!    .           htcrcf, htcorr, hskm, hmf(1)

      return
      END
