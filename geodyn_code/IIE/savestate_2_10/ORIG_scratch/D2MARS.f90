!$D2MARS
      SUBROUTINE D2MARS(XSC,MJDSEC,FSEC,DENST)
!********1*********2*********3*********4*********5*********6*********7**
! D2MARS           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CALCULATE MARS MEAN DENSITY AND OTHER ATMOSPHERE PARAMETERS
!            (model FROM MARSGRAM 3.7 VERSION)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSC(I)   I    A    POSITION VECTOR OF THE SPACECRAFT (KM)
!   MJDSEC   I    S    NUMBER OF SECONDS CORRESPONDING TO MJD
!   FSEC     I    S    FRACTIONAL SECOND CORRESPONDING TO MJDSEC
!   DENST    O    S    MASS DENSITY (KG/M**3) (OUTPUT)
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
!    RMB(I)            ROTATION MATRIX (COMMON)
!    XMNSNS            (1-3,2) POSITION VECTOR OF THE SUN (M) (COMMON)
!
!    A                 SEMIMAJOR AXIS OF MARS ORBIT (KM)
!    AL                = TF / 9.2
!    AUOA              INVERSE OF A IN ASTRONOMICAL UNITS
!    AVGDEN            AVERAGE DENSITY AROUND THE ORBIT
!    AVOG              AVOGADROS NUMBER
!    ALS               AEROCENTRIC LONGITUDE OF THE SUN
!    AVOGDR            RECIPROCAL OF AVOGADROS NUMBER (KG-MOLE)
!    CSCLAT            COSINE OF THE SPACECRAFT LATITUDE
!    DEGRAD            CONSTANT TO CONVERT DEGREES TO RADIANS
!    DELR61            CORRECTION TO R61MB DUE TO PRESSURE VARIATION
!    DENSTI            DENSITY PER MOLECULAR SPECIES
!    DL                LONGITUDE DIFFERENCE BETWEEN SPACECRAFT AND SUN
!    DT                DIFFERENCE OF DATE BETWEEN PRESENT AND REFERENCE
!    DZDS              INCREMENT OF ALTITUDE DUE TO DUST STORMS (KM)
!    F107             SOLAR FLUX AT 10.7 CM WAVELENGTH
!                     (1/KM)
!    PF               PRESSURE AT ZF
!    SCLAT            LATITUDE OF SPACECRAFT (RAD.) (ROTATING
!                     COORDINATES)
!    SCLON            LONGITUDE OF SPACECRAFT (RAD.)
!    SNLAT            LATITUDE OF SUN (RAD.) (ROTATING COORDINATES)
!    SNLON            LONGITUDE OF SUN (RAD.)
!    SOLDAY           DAYS IN EARTH YEAR
!    SPCLT            THE QUANTITY 1.219D-2 / T
!    T                TEMPERATURE AT SPACECRAFT POSITION
!    U                UNIVERSAL GAS CONSTANT (CM*KM**2/SEC*2)
!    Z                ALTITUDE OF SPACECRAFT ABOVE REFERENCE
!                     SURFACE (KM)
!    ZF               ALTITUDE OF 1.24 NANOBAR LEVEL (KM)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,   &
     & RREF,als0,INTENS,idum1,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,     &
     & CFPMS,IPOPT,idum2,F107,STDLMS,AUMARS,gzerom,gzms
      COMMON/MNSNSP/XMNSNS(7,2)
      COMMON/STARTT/ESSTRT,FSSTRT
      DIMENSION XSNI(3)
      DIMENSION XSC(3),XSN(3),F(9)
      DIMENSION XMW(9)
!     DATA BDAT/2430000.5D0/
      DATA A,AF,AVOGDR,ED,ES/2.279D8,60.D0,0.1660081676D-20,.1D0,.1D0/
      DATA AUOA/0.656300975D0/, PF/1.24D-9/, AVOG /6.022045D23/
      DATA F/.932D0,.01D0,.027D0,.016D0,.013D0,.002D0,5.0D-5,1.D-6,     &
     &        4.D-6/
      DATA XK,XMW/8314.39D0,44.01D0,16.D0,28.012D0,39.948D0,28.01D0,    &
     &           32.D0,4.D0,1.D0,2.D0/
      DATA XMBAR,NC,PERIOD/43.3D0,9,686.98D0/
      DATA P6,RA,RB,RC/610.D0,3394.67D0,3393.21D0,3376.78D0/
      DATA REFDAT,SIGMA,SOLDAY,U/2443951.D0,1.D0,365.25636D0,           &
     &                          .42828443D10/
      DATA SCLNGA/1.893682238D0/
      DATA DJ3935/2443935.D0/,D26P35/26.35D0/,D360/360.D0/,D250/250.D0/
      DATA DP0145/.0145D0/,DP001/.001D0/
      DATA EMO/.09338D0/
      DATA ZERO/0.D0/,HALF/.5D0/,ONE/1.D0/,THREE/3.D0/
      DATA ICOUNT/0/, SUMDAY/0.0/
!!!      ON EXTERNAL ERROR CALL TRAP
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!     THERE ARE NO CHANGES TO THE OLD MODEL CODE
!     FOR COMPUTING SPACECRAFT OR SUN COORDINATES,
!     LOCAL TIME, FRACTION OF HELIOCENTRIC PERIOD, OR RADIUS OF THE
!     REFERENCE SPHEROID.
!
! *** RECONSTITUTE THE JULIAN DATE
!
      DATE=TMGDN1+(DBLE(MJDSEC)+FSEC)/SECDAY
!
! *** CORIOL FACTOR
!
      CORFAC= 0.2D0 * DEGRAD /SLDAY
!
! *** PULL QUANTITIES FOR SUN FRMO XMNSNS
!
      RSN=XMNSNS(4,2)
      XSNI(1)=RSN*XMNSNS(1,2)
      XSNI(2)=RSN*XMNSNS(2,2)
      XSNI(3)=RSN*XMNSNS(3,2)
      RSN=RSN*DP001
!
      AUMARS=RSN*1000.D0/AU
!
! *** CONVERT COORDINATE SYSTEM
!
      XSN(1)=RMB(1)*XSNI(1)+RMB(4)*XSNI(2)+RMB(7)*XSNI(3)
      XSN(2)=RMB(2)*XSNI(1)+RMB(5)*XSNI(2)+RMB(8)*XSNI(3)
      XSN(3)=RMB(3)*XSNI(1)+RMB(6)*XSNI(2)+RMB(9)*XSNI(3)
!
      XYSC=XSC(1)*XSC(1)+XSC(2)*XSC(2)
      XYSN=XSN(1)*XSN(1)+XSN(2)*XSN(2)
!
      RSC=SQRT(XYSC+XSC(3)*XSC(3))*DP001
!
      SCLON=ATAN2(XSC(2),XSC(1))
      SCLAT=ATAN2(XSC(3),SQRT(XYSC))
      SNLON=ATAN2(XSN(2),XSN(1))
      SNLAT=ATAN2(XSN(3),SQRT(XYSN))
!
!...  Acceleration of gravity, gz, at height z, computed by method of
!...    Seiff, Adv Space Res., 2, 1982, pp 3-17
!...  GMMS = gravitational constant * mass of Mars
      GMMS = 4.28282D7
!...  J2 = coefficient of 1st non-spherical gravity term
      XJ2 = 0.001965D0
!...  P2 = latitude part of 1st non-spherical gravity term
      P2 = 1.5D0*SIN(SCLAT)**2 - 0.5D0
!...  Rz = total radius to current height SCHGT
      Rz = RSC
!...  acceleration of gravity at height z
      RAB= SQRT(RA*RB)
      gzms = (GMMS/Rz**2)*(1. - 3.D0*XJ2*((RAB/Rz)**2)*P2)
!
! *** CALCULATE LOCAL TIME
!
      DL=SCLON-SNLON
      TIME=PI+DL
      IF(TIME.LT.ZERO) TIME=TIME+TWOPI
      IF(TIME.GE.TWOPI) TIME=TIME-TWOPI
      SCLON=SCLON+SCLNGA
      IF(SCLON.LT.0.D0)SCLON=SCLON+TWOPI
!
! *** CALCULATE THE FRACTION OF HELIOCENTRIC PERIOD
!
      DT=DATE-REFDAT
      FP=MOD(DT,PERIOD)/PERIOD
      IF(FP.LT.ZERO) FP=FP+ONE
      DAYS=MOD(DATE-DJ3935,D26P35)
      IF(DAYS.LT.ZERO) DAYS=DAYS+D26P35
!
! *** CALCULATE RADIUS OF REF SPHEROID
!
      AOR=A/RSN
      AC=(AOR*(ONE-EMO*EMO)-ONE)/EMO
      IF(ABS(AC).GT.ONE) AC=INT(AC)
      ALM=ACOS(AC)/DEGRAD
      IF(FP.GT.HALF) ALM=D360-ALM
      ALS = ALM + D250
      IF(ALS.GT.D360) ALS=ALS-D360
!........
      CSCLAT=COS(SCLAT)
      CSCLNG=COS(SCLON)
      SSCLAT=SIN(SCLAT)
      SSCLNG=SIN(SCLON)
!
      RSQ=(CSCLAT*CSCLNG/RA)**2+(CSCLAT*SSCLNG/RB)**2+(SSCLAT/RC)**2
      R61MB=SQRT(ONE/RSQ)
!
! *** CORRECTION TO 6.1 MILLIBAR ELLIPSOID FOR SEASONAL GLOBAL PRESSURE
! *** VARITION
!
      AM1 = (COS(0.75D0*DEGRAD*(ALS-270.D0)))**2
      AM2 = (COS((6.D0/7.D0)*DEGRAD*(ALS-45.D0)))**2
      IF (ALS.GE.30.D0 .AND. ALS.LE.150.D0)  AM1 = ZERO
      IF (ALS.GE.150.D0 .AND. ALS.LE.300.D0) AM2 = ZERO
      PROPR = (1.D0 + 0.310D0*AM1 + 0.225D0*AM2) / 1.169D0
      T0 = 220.D0 *AOR
      DEL61 = (T0/19.51D0) * LOG(PROPR)
      R61MB = R61MB + DEL61
      RREF=R61MB
!...  acceleration of gravity at the surface
      gzerom= (GMMS/RREF**2)*(1. - 3.*XJ2*((RAB/RREF)**2)*P2)
!
! *** CALCULATE THE HEIGHT OF THE SPACECRAFT ABOVE THE 6.1 MILLIBAR
! *** SURFACE
!
      SCHGT = RSC - RREF
!
! *** COMPUTE 10.7 CM FLUX WITH SHORT PERIOD TERM IGNORED AND
! *** ADJUSTED TO THE DISTANCE OF MARS FROM THE SUN (TAKEN FROM
! *** OLD CODE)
!
      Y = (DATE-2442870.D0)*TWOPI / (11.04D0*SOLDAY)
      F107 = 75.D0 + 60.D0*(ONE-COS(Y+30.D0*DEGRAD*(ONE-COS(Y))))
      F107 = F107 * AOR**2 * AUOA**2
!
      IF(F107.LT.50.D0)F107=50.D0
      IF(F107.GT.450.D0)F107=450.D0
!C
      SCLON=SCLON/DEGRAD
      SCLAT=SCLAT/DEGRAD
      SNLON=360.D0-SNLON/DEGRAD
      SNLAT=SNLAT/DEGRAD
!
      IF(SNLON.GT.360.D0)SNLON=SNLON-360.D0
      IF(SNLON.LT.0.D0)SNLON=SNLON+360.D0
!
      IF(SCLON.LT.0.D0)SCLON=SCLON+360.D0
      IF(SCLON.GE.360.D0)SCLON=SCLON-360.D0
!
! NEW MARS ATMOSPHERE MODEL STARTS HERE:
!
!...  Evaluate local terrain height
      thgt = Terrain(SCLAT,SCLON)
!...  Evaluate factors for dust storm model
      CHGT=SCHGT-THGT
      Call Dustfact(SCLAT,SCLON,CHGT,als,dustM,dustA)
!...  Evaluate atmospheric parameters
      CALL ATMOS2(SCHGT,SCLAT,SCLON,SNLAT,SNLON,ALS,                    &
     & dustM,dustA,HSCALE,TEMP,DENS,PRES,THGT,                          &
     & TAVG,Bruntf,densurf,ZF,Texos,Tbase,Hrho)
      Zbase = ZF
!... density
      DENST=DENS
!
!...  VLL = vertical scale for wind viscosity factor, km
!...  Use pressure scale height for VLL
      VLL = HSCALE
!...  VLS = vertical scale for density perturbations, km,
!...  estimated from structure function analysis of VL-1 and VL-2
!...  relative density perturbations
      VLS = 8.0D0
!...  HLS = horizontal scale for density perturbations, km,
!...  estimated from 1.5 times Earth-GRAM values
      HLS = 30.D0 + .01875D0*CHGT**2
!...  Atmospheric parameters at +/- 2.5 degrees longitude, for
!...  computing northward areostrophic wind component
  490 CLONP = SCLON + 2.5D0
      IF (CLONP .gt. 360.D0)CLONP = CLONP - 360.D0
      CALL ATMOS2(SCHGT,SCLAT,CLONP,SNLAT,SNLON,ALS,                    &
     & dustM,dustA,HLONP,TLONP,DLONP,PLONP,RLONP,TAVP,bvf,den0,         &
     & zt,TINF,TF,Hrlp)
      CLONM = SCLON - 2.5D0
      IF (CLONM .lt. 0.0D0)CLONM = CLONM + 360.D0
      CALL ATMOS2(SCHGT,SCLAT,CLONM,SNLAT,SNLON,ALS,                    &
     & dustM,dustA,HLONM,TLONM,DLONM,PLONM,RLONM,                       &
     & TAVM,bvf,den0,zt,TINF,TF,Hrlm)
!...  First two heights for area average height
      areahgt = Terrain(SCLAT,CLONP) + Terrain(SCLAT,CLONM)
      CLATP = SCLAT + 2.5D0
      CLONP = SCLON
      IF(CLATP.LE.90.D0)GO TO 570
      CLATP = 180.D0 - CLATP
      CLONP = CLONP + 180.D0
!...  Atmospheric parameters at +/- 2.5 degrees latitude, for
!...  computing eastward areostrophic wind component
  570 Continue
      CALL ATMOS2(SCHGT,CLATP,CLONP,SNLAT,SNLON,ALS,                    &
     & dustM,dustA,HLATP,TLATP,DLATP,PLATP,RLATP,                       &
     & TAVP,bvf,den0,zt, TINF,TF,HRlp)
      CLATM = SCLAT - 2.5D0
      CLONM = SCLON
      IF(CLONM.GE.-90.D0)GO TO 580
      CLATM = -180.D0 - CLATM
      CLONM = CLONM + 180.D0
  580 Continue
      CALL ATMOS2(SCHGT,CLATM,CLONM,SNLAT,SNLON,ALS,                    &
     & dustM,dustA,HLATM,TLATM,DLATM,PLATM,RLATM,                       &
     & TAVM,bvf,den0,zt,TINF,TF,Hrlm)
!...  Area average height
      areahgt = 0.25D0*(areahgt + Terrain(CLATM,CLONM) +                &
     & Terrain(CLATP,CLONP))
!...  DELNS = distance corresponding to 5 degrees of latitude
      DELNS = DEGRAD*RSC*5.0D3
!...  WLAT = Latitude for wind calculations (2.5 - 87.5 in Abs value)
      WLAT = SCLAT
      ABSWLAT = ABS(WLAT)
      IF(ABSWLAT.GT.87.5D0)WLAT = SIGN(87.5D0,WLAT)
      IF(ABSWLAT.LT.2.5D0)WLAT = SIGN(2.5D0,WLAT)
      COSWLAT = COS(DEGRAD*WLAT)
      SINWLAT = SIN(DEGRAD*WLAT)
!...  DELEW = distance corresponding to 5 degrees of longitude
      DELEW = -DEGRAD*RSC*COSWLAT*5.0D3
!...  Coriolis factor for computing areostrophic wind
      CORIOL = CORFAC*SINWLAT
!...  Viscous modifications to areostrophic wind
      VISC = BETAMS*TEMP**1.5D0/(TEMP + SVAL)
!...  Use multiplier to scale wind magnitude
      VISCFAC = 0.04D0*VISC/(1.0D6*DENS*VLL**2)
!...  FUG, FVG = Coriolis parameter times components of areostrophic
!...  wind
      FUG = -(PLATP-PLATM)/(DELNS*DENS)
      FVG = (PLONP-PLONM)/(DELEW*DENS)
      If (ABSWLAT .lt. 15.0D0)then
!...    Low latitude wind model
        FUGP = -4000.D0*RSC*SINWLAT*(PLATP + PLATM - 2.D0*PRES)/        &
     &   (DENS*COSWLAT*DELNS*DELNS)
        FVGP = 0.0D0
        If (ABSWLAT .gt. 7.5D0) then
          FUG = FUGP + (ABSWLAT-7.5D0)*(FUG-FUGP)/7.5D0
          FVG = FVGP + (ABSWLAT-7.5D0)*(FVG-FVGP)/7.5D0
        Else
          FUG = FUGP
          FVG = FVGP
        Endif
      Endif
      If (ABSWLAT .gt. 75.D0)then
!...    High latitude wind model
        Cosfac = COSWLAT/COS(DEGRAD*75.D0)
        FUG = FUG*COSWLAT*Cosfac
        FVG = FVG*COSWLAT*Cosfac
      Endif
      DENOM = CORIOL**2 + VISCFAC**2
!...  Viscous-corrected areostrophic wind components
      EWWIND = (CORIOL*FUG - VISCFAC*FVG)/DENOM
      NSWIND =  (CORIOL*FVG + VISCFAC*FUG)/DENOM
!...  Use boundary layer wind model for heights close to surface
      If (SCHGT .lt. thgt + 1.0D0)then
!...    Height above ground level, meters
        z = 1000.D0*(CHGT - thgt)
!...    "Surface" winds at Viking lander 1.6 meter height
        if (z .lt. 1.6D0)z = 1.6D0
!...    z0 = surface roughness, meters
        z0 = 0.03
!...    CD = geostrophic drag coefficient; von Karman constant = 0.4
        CD = 1.5D-3
        factor = (SQRT(CD)/0.4D0)*LOG(z/z0)
        if (factor .gt. 1.0D0) factor = 1.0D0
        EWWIND = factor*EWWIND
        NSWIND = factor*NSWIND
      Endif
!...  Local time in "Martian hours" (1/24th Sols)
!!    ....CLON is undefined                   ! jjm 9/98
!!    ....SUNLON is undefined                 ! jjm 9/98
!!      TLOCAL = 12.D0 + (SUNLON - CLON)/15.D0
                                                  ! best guess jjm 9/98
      TLOCAL = 12.D0 + (SNLON - SCLON)/15.D0

      IF (TLOCAL .LT. 0.D0)TLOCAL = TLOCAL + 24.D0
      IF (TLOCAL .GT. 24.D0)TLOCAL = TLOCAL - 24.D0
!
  999 RETURN
      END
