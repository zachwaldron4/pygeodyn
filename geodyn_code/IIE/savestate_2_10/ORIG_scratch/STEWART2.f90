!$STEWART2
      SUBROUTINE STEWART2 ( SUNLON, LAT, LST, TOTALPRZ, TZ,             &
     & TOTALMDZ, CHGT, RSTAR, H, MOLWTG, SIGMA,sunlat,TINF,             &
     & TF,zFp,Hrho)
!********1*********2*********3*********4*********5*********6*********7**
! STEWART2         00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SUNLON             AREOCENTRIC LONGITUDE OF SUN AT LST(TIME)
!      LAT             SPACECRAFT LATITUDE
!      LST             TIME (MARS)
!  TOTALPRZ  O    S    PRESSURE
!      TZ              TEMPRETURE AT Z
!  TOTALMDZ  O    S    DENSITY
!     CHGT             SPACECRAFT HEIGHT ABOVE REFERENCE SURFACE (KM)
!    RSTAR             UNIVERSAL GAS CONSTANT(8.31439E3)
!      H
!   MOLWTG         S   AVERAGE MOLECULAR WEIGHT (43.49)
!    SIGMA         S
!   sunlat    I    S   AREOCENTRIC LATITUDE OF SUN (DEGREES)
!       gz    I    S   Acceleration of Gravity at Z
!      zFp    I    S
!     TINF    O    S
!      TF     O    S
!     Hrho    O    S
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!.....  TIME-DEPENDENT MARS ATMOSPHERE MODEL, FORTRAN VERSION OF PROGRAM
!     BY IAN STEWART, LABORATORY FOR ATMOSPHERIC AND SPACE PHYSICS,
!     UNIV. OF COLORADO. FINAL REPORT JPL PO # NQ-802429
!....................................................................
!      COMMON/DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,
!     & RREF,als0,SNTENS,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,
!     & CFPMS,SPOPT,F107,STDLMS,AUMARS,gzerom,gzms
      INCLUDE 'COMMON_DECL.inc'
      COMMON/DUSTCM/BETAMS,SVAL,SLDAY,DUSTLAT,DUSTLON,DUSTHGT,RADMAX,   &
     & RREF,als0,INTENS,idum1,CFMARS(0:5),DELTAZF,DELTATF,DELTATEX,     &
     & CFPMS,IPOPT,idum2,F107,STDLMS,AUMARS,gzerom,gzms
      DIMENSION ES(0:11), PRZ(0:11), NDZ(0:11), MDZ(0:11)
      DOUBLE PRECISION SUNLON, LAT, LST, MOLWTG, NDZ, MDZ
!.... ES ARE STD DEVIATIONS FROM NOMINAL VALUES
!.... 0,2,....10  LONG - TERM
!.... 1,3.....,11  SHORT-TERM
!.... FOR   FBAR,TINF,FOXY,AOXY,ZF,DZDUST
!.... RETURNS TEMP,#DENS,MDENS,PRESSURE
      Call EScalc(stdlms,SIGMA,ES)
!.... DEVIATIONS FROM NOMINAL VALUES
      SMA = 1.523691D0
!.... SMA = SEMIMAJOR AXIS OF MARS ORBIT
      FBAR = F107 * EXP(ES(0))
!.... 3 MONTH RUNNING MEAN OF 10.7 CM SOLAR FLUX
!.... IN UNITS OF 1.0D-22 W/CM**2
      RAU = AUMARS
!...  Convert solar 10.7 cm flux to Mars position
      FBARR = FBAR / (RAU**2)
!
      CALL PRSEAS(SUNLON, LAT, PFAC)
!...  Evaluate the basic parameters for the thermosphere model
      Call Thermpar(RAU,FBARR,LAT,LST,SUNLAT,TINF0,TF0,ZF0,SCALE)
      TO = 220.0D0 * SMA / RAU
      DR = (TO / 19.51D0) * LOG(PFAC)
!.... SEASONAL CORRECTION TO 6.1 mbar ELLIPSOID
      CALL DZDUST( SUNLON, als0, INTENS, DUST)
      DUST = DUST * EXP(ES(10))
!...  Height of base of thermosphere
      If (ipopt.eq.1)Then
        ZF = zFp
      Else
        ZF = ZF0 * EXP(ES(8) + ES(9)) + DR + DUST + deltaZF
      Endif
!...  Height above base of thermosphere
      ZZF = CHGT - ZF
      RF = RREF + ZF
!...  Exospheric temperature
      TINF = TINF0 * EXP(ES(2) + ES(3)) + deltaTEX
!...  Temperature at base of thermosphere
      TF = TF0 * EXP(ES(8) + ES(9)) + deltaTF
      CALL THERMOS(ES, TINF, TF, LAT, LST, ZF,RF, ZZF, TOTALPRZ,        &
     & TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ , SCALE,tgrad)
!...  SCALE HEIGHT, km
      Rog = RSTAR/(1000.D0*MOLWTG*GZMS)
      H = Rog*TZ
      Hrho = H/(1.D0 + tgrad*Rog)
!...  Convert pressure to N/m**2
      TOTALPRZ = TOTALPRZ*1.0D5
!...  Convert density to kg/m**3
      TOTALMDZ = TOTALMDZ*1000.D0
      RETURN
      END
