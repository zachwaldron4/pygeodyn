!$ATMOS2
      SUBROUTINE ATMOS2 (CHGT,CLAT,CLON,SUNLAT,SUNLON,ALS,              &
     & dustM,dustA,H,TEMP,DENST,PRES,Z0,TAVG,Bruntf,densurf,ZF,         &
     & Texos,Tbase,Hrho)
!********1*********2*********3*********4*********5*********6*********7**
! ATMOS2           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: EVALUATES MARS ATMOSPHERIC PARAMETERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!     CHGT     SPACECRAFT HEIGHT ABOVE REFERENCE SURFACE (KM)(INPUT)
!     CLAT     SPACECRAFT LATITUDE (DEGREES) (INPUT)
!     CLON     WEST LONGITUDE OF SPACECRAFT (DEGREES) (INPUT)
!     SUNLAT   AREOCENTRIC LATITUDE OF SUN (DEGREES) (INPUT)
!     SUNLON   MARS WEST LONGITUDE OF SUN (DEGREES) (INPUT)
!     ALS      AREOCENTRIC LONGITUDE OF SUN ORBIT (INPUT)
!     dustM    dust storm magnitude for average T and p effect
!              (1 = full magnitude, 0 = no dust storm) (INPUT)
!     dustA    dust storm magnitude for daily amplitude T and p effect
!              (1 = full magnitude, 0 = no dust storm) (INPUT)
!     H        SCALE HEIGHT AT SPACECRAFT POSITION (KM) (OUTPUT)
!     TEMP     TEMPERATURE AT SPACECRAFT POSITION (K) (OUTPUT)
!     DENST    MASS DENSITY AT SPACECRAFT POSITION (KG/M**3) (OUTPUT)
!     PRES     PRESSURE AT SPACECRAFT POSITION (N/M**2) (OUTPUT)
!     RSC      AREOCENTRIC RADIUS TO SPACECRAFT (KM) (OUTPUT)
!     Z0       LOCAL TERRAIN HEIGHT RELATIVE TO REFERENCE ELLIPSOID
!              (KM) (INPUT)
!     TAVG     Daily average surface temperature (K) (OUTPUT)
!     Bruntf   Brunt-Vaisala frequency = Sqrt((g/T)*(dT/dZ + g/Cp))
!              (OUTPUT)
!     densurf  Density at surface (kg/m**3) (OUTPUT)
!              (OUTPUT)
!     ZF       Local height of base of thermosphere (OUTPUT)
!     Texos    local exospheric temperature (K) (OUTPUT)
!     Tbase    local temperature for base of exosphere (K) (OUTPUT)
!
!.....................................................................
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!      COMMON/DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,
!     & RREF,als0,SNTENS,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,
!     & CFPMS,SPOPT,F107,STDLMS,AUMARS,gzerom,gzms
      INCLUDE 'COMMON_DECL.inc'
      COMMON/DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,   &
     & RREF,als0,INTENS,idum1,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,     &
     & CFPMS,IPOPT,idum2,F107,STDLMS,AUMARS,gzerom,gzms
      Dimension gam(5),T(0:5),p(0:5),gph(0:5),z(0:5),gam0(5)
      Dimension ES(0:11)
!...  z(i) are significant height levels (areographic km), relative to
!...  reference ellipsoid
!...  Significant height levels (areopotential km)
      Data gph/0.D0,5.D0,15.D0,30.D0,50.D0,75.D0/
!.....................................................................
!
!...  Rstar = Universal gas constant
!...  AMW = average molecular weight up to turbopause
      DATA RSTAR,AMW/8.31439D3,43.49D0/
!...  SMA = semi-major axis of Mars orbit
      Data SMA/1.523691D0/
!...  Rbar = gas constant for Martian atmosphere up to turbopause
      Rbar = RSTAR/AMW
!...  Evaluate surface temperature, K
      Call Tsurface(CLAT,CLON,SUNLAT,SUNLON,ALS,AUMARS,Tsurf,           &
     & T0bar,dustM,TAVG)
!...  areographic radius to altitude CHGT
      RSC = RREF + CHGT
      Call Gamma1(CLAT,ALS,gam0)
!...  gam0 = mean temperature lapse rates, for use in perturbation
!...  model
      Do 10 i = 1,5
   10 GAM(i) = gam0(i)
      dlon = CLON - SUNLON
!...  Evaluate temperatures at significant levels
      Call Temps(T0bar,Tsurf,gam,CLAT,dlon,dustM,dustA,ALS,T,           &
     &CFMARS,Z0)
!...  Evaluate surface pressure, N/m**2
      Call Psurface(CLAT,CLON,ALS,T0bar,Rbar,pavg,psurf,dustM,dustA,    &
     & Z0,gzerom,gam(1),sunlon)
!...  Apply climate adjustment for surface pressure (default 1.0)
      psurf = psurf*CFPMS
!...  Evaluate pressure at significant levels
      Call Pressure(psurf,Z0,gzerom,rref,Rbar,T,gam,p,z)
!...  Surface density and for use in random perturbation model
      densurf = psurf/(Rbar*Tsurf)
      tgrad = -0.001D0*gam0(1)
!...  bvf0 = Brunt Vaisala frequency at surface
      bvf0 = gzerom*(tgrad + gzerom/Cp(Tsurf))/Tsurf
      if (bvf0 .lt. 0.0D0)bvf0 = 1.0D-5
      bvf0 = SQRT(bvf0)
      ZF = 0.0D0
      If (CHGT .le. -5.D0)then
        PRES = psurf
        TEMP = Tsurf
        DENST = psurf/(Rbar*Tsurf)
        H = 0.001D0*Rbar*Tsurf/gzerom
        Hrho = H/(1.D0 + tgrad*Rbar/gzerom)
        Bruntf = bvf0
        Return
      Endif
      If (CHGT .le. z(5))then
!...    heights below 75 areopotential km
        goR = 1000.D0*gzerom/Rbar
        j = 0
        Do 20 i = 0,4
   20   If (CHGT .ge. z(i))j = i
        gphgt = ((CHGT-Z0)/(1.D0 + (CHGT-Z0)/rref)) - gph(j)
        TEMP = T(j) - gam(j+1)*gphgt
        If (ABS(gam(j+1)) .gt. 0.001D0)then
          PRES = p(j)*EXP(goR*LOG(TEMP/T(j))/gam(j+1))
        Else
          PRES = p(j)*EXP(-goR*gphgt/T(j))
        Endif
        DENST = PRES/(Rbar*TEMP)
        H = TEMP/goR
!...    tgrad = temperature gradient, K/m
        tgrad = -0.001D0*gam0(j+1)
        Hrho = H/(1.D0 + tgrad*Rbar/gzerom)
!...    bvf = square of Brunt Vaisala frequency
        bvf = gzms*(tgrad + gzms/Cp(TEMP))/TEMP
        if (bvf .le. 0.0D0)bvf = 1.0D-5
        Bruntf = SQRT(bvf)
      Else
!
!...    Thermosphere or Stratosphere region
!
        TIME = 12.D0 + (SUNLON - CLON)/15.D0
        IF (TIME .LT. 0.0D0)TIME = TIME + 24.D0
        IF (TIME .GT. 24.D0)TIME = TIME - 24.D0
!...    Seasonal relative pressure variation
        Call PRSEAS(ALS,CLAT,PFAC)
        TO = 220.0D0*SMA/AUMARS
!...    Correction to ZF for pressure variation
        DR = (TO/19.51D0)*LOG(PFAC)
!...    Evaluate long-term standard deviations about nominal
        Call EScalc(stdlms,0.D0,ES)
!...    Correction to ZF for dust storm
        Call DZDUST(ALS,als0,INTENS,DUST)
        DUST = DUST * EXP(ES(10))
!...    Height of base of thermosphere
        FBAR = F107*EXP(ES(0))/AUMARS**2
        Call Thermpar(AUMARS,FBAR,CLAT,TIME,SUNLAT,TINF0,TF0,ZF0,SCALE)
        ZF = ZF0*EXP(ES(8)) + DR + DUST + deltaZF
        If (ipopt.eq.1) Then
         TF = TF0 * EXP(ES(8) + ES(9)) + deltaTF
         PF = 1.26D-4
         AMF = AMW
         g100 = gzerom/(1.D0 + 100.D0/rref)**2
         Call Stratos(z(5),T(5),p(5),AMW,ZF,TF,PF,AMF,Rstar,rref,       &
     &   g100,100.D0,T100,P100,D100,H100,dt100,Z(4),T(4),zFp,1,HD100)
        ZF = zFp
        Endif
!...    Use stratosphere model if current height < ZF
      If (CHGT .lt. ZF)then
        Shgt = ZF
!...    Evaluate thermospheric parameters at base of thermosphere
        Call Stewart2(ALS,CLAT,TIME,PRES,TEMP,DENST,Shgt,               &
     &  Rstar,H,AMF,0.D0,sunlat,TINF,TF,zFp,Hrho)
          Tbase = TF
          Texos = TINF
          PF = PRES
          TF = TEMP
          If (ipopt.eq. 1) Then
            ZFS = zFp
          Else
            ZFS = ZF
          Endif
          If(ipopt.eq.1)AMF = AMW
        Call Stratos(z(5),T(5),p(5),AMW,ZFS,TF,PF,AMF,Rstar,rref,       &
     &  gzms,CHGT,TEMP,PRES,DENST,H,tgrad,Z(4),T(4),zFU,ipopt,Hrho)
        Else
          Shgt = CHGT
!...    Evaluate thermospheric parameters at current height
        Call Stewart2(ALS,CLAT,TIME,PRES,TEMP,DENST,Shgt,               &
     &  Rstar,H,AMF,0.D0,sunlat,TINF,TF,zFp,Hrho)
          Texos = TINF
          Tbase = TF
        Endif
      Endif
      Return
      END
