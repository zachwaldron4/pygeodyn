!$DTM2020_call
      SUBROUTINE DTM2020_call(MJDSEC,FSEC,                &
              &               FLUXD,FLXAVG,FLUXKP,        &
              &               ALTM,PHI,XLAMB,             &
              &               COSHL,SINHL,                &
              &               RHO,DRHODZ)
      
      
!********1*********2*********3*********4*********5*********6*********7**
! DTM2020_call             2022/02/15                by Zach Waldron
!
!
!  FUNCTION:
!   THIS ROUTINE SERVES AS A SHELL FOR CALLING THE DTM2020 DENSITY MODEL
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
!   I/O PARAMETERS:
!
!   NAME        I/O   A/S          DESCRIPTION OF PARAMETERS
!   ------      ---   ---          ------------------------------------------------
!   MJDSEC       I     S           TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!   FSEC         I     S           FRACTIONAL REMAINING SECONDS
!   FLUXD                          SOLAR FLUX VALUES FROM THE PRECEDING DAY
!
!   FLXAVG                         AVERAGE SOLAR FLUX VALUES WHERE AVERAGING IS
!                                  PERFORMED OVER 3 SOLAR CYCLES
!
!   FLUXKP                         3 HOURLY MAGNETIC FLUX KP VALUES
!
!   ALTM                           ALTITUDE IN METER(M) ABOVE 12O,000 METERS
!   COSHL                          THE COSINE OF HL(LOCAL HOUR)
!   SINHL                          THE SINE OF HL(LOCAL HOUR)
!   COSLAT                         THE COSINE OF LATITUDE IN RADIAN
!   SINLAT                         THE SINE OF LATITUDE IN RADIAN
!   DTOTAL                         TOTAL DENSITY WITH  GRAM/(CM)**3
!   DRHODZ                         DERIVATIVE OF TOTAL DENSITY WRT ALTI
!   MJDSEC   I    S                TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!
!
! COMMENTS:----------------------------------------------------------------
!               from dtm2020 source code:
                  ! ** INPUTS **
                  !     day   =   day of year [1-366]
                  !     f     =   f(1) = instantaneous flux at (t - 24hr)    
                  !               f(2) = 0.
                  !     fbar  =   fbar(1) = mean flux of last 81 days at t  
                  !               fbar(2) = 0.
                  !     akp   =   akp(1) = kp delayed by 3 hours, 
                  !               akp(3) = mean of last 24 hours,
                  !               akp(2) = 0. 
                  !               akp(4) = 0.
                  !     alti  =   altitude (in km) greater than 120 km
                  !     hl    =   local time (in radian: 0-24hr = 0-2pi)
                  !     alat  =   latitude (in radian)
                  !     xlon  =   longitude (in radian)
                  !
                  ! ** OUTPUT **
                  !     tz     =  temperature at altitude -> alti
                  !     tinf   =  exospheric temperature
                  !     d(1)   =  partial density of atomic hydrogen (in gram/cm3)
                  !     d(2)   =  partial density of helium
                  !     d(3)   =  partial density of atomic oxygen
                  !     d(4)   =  partial density of molecular nitrogen
                  !     d(5)   =  partial density of molecular oxygen
                  !     d(6)   =  partial density of atomic nitrogen
                  !     ro     =  total density (in gram/cm3)
                  !     wmm    =  mean molecular mass (in gram)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
   
   
      !DATA RADDEG/57.29577951D0/

   
      REAL(8) :: FLUXD
      REAL(8) :: FLXAVG
      REAL(8) :: FLUXKP
      REAL(8) :: ALTM
      REAL(8) :: COSHL
      REAL(8) :: SINHL

      REAL(4) ::  DOY_real

      REAL(4) ::  ALTKM
      REAL(4) ::  STLOC
      REAL(4) ::  GLAT
      REAL(4) ::  GLON

      REAL(4), DIMENSION(2) ::  flux_daily
      REAL(4), DIMENSION(2) ::  flux_avg
      REAL(4), DIMENSION(4) ::  kp_array

      ! OUTPUTS
      REAL(4)               ::  Temp_z       !temperature at altitude
      REAL(4)               ::  Temp_exo     !exospheric temperature
      REAL(4)               ::  RHO_dtm2020  !total density (in gram/cm3)
      REAL(4)               ::  MBAR_dtm2020 !mean molecular mass (in gram)
      REAL(4), DIMENSION(6) ::  part_dens
                      !     X(1) = part.den atomic hydrogen (in g/cm3)
                      !     X(2) = part.den helium
                      !     X(3) = part.den atomic oxygen
                      !     X(4) = part.den molecular nitrogen
                      !     X(5) = part.den molecular oxygen
                      !     X(6) = part.den atomic nitrogen
      REAL(8), DIMENSION(10) ::  num_dens_temp
                      !     X(1) = n_den atomic hydrogen (in #/m^3)
                      !     X(2) = n_den helium
                      !     X(3) = n_den atomic oxygen
                      !     X(4) = n_den molecular nitrogen
                      !     X(5) = n_den molecular oxygen
                      !     X(6) = n_den atomic nitrogen
                      !     X(7)  = RHO    (kg/m^3)
                      !     X(8)  = temperature at altitude
                      !     X(9)  = exospheric temperature
                      !     X(10) = mean molecular mass  (in kg)

      DATA He_amu/4.002602D0/
      DATA O_amu/15.9994D0/
      DATA N2_amu/28.0134D0/
      DATA O2_amu/31.9988D0/
      DATA Ar_amu/39.948D0/
      DATA H_amu/1.0079D0/
      DATA N_amu/14.0067D0/
      
      
      COMMON/PARMB/GSURF,RE
      DATA RGAS/831.4D0/,ZL/120.D0/

!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
      kin = kin+1     ! count how many times the file is entered


! DATE
!     day   =   day of year [1-366]
      iDOY=DOY(MJDSEC,FSEC)   !-ONE    ----- zach removed subtract one...
      DOY_real = FLOAT(iDOY)
! FLUXES
!     f(1) = instantaneous flux at (t - 24hr)    
!     f(2) = 0.      
      flux_daily(1) = SNGL(FLUXD)
      flux_daily(2) = SNGL(0.0D0)
!     fbar(1) = mean flux of last 81 days at t  
!     fbar(2) = 0.
      flux_avg(1)   = SNGL(FLXAVG)
      flux_avg(2)   = SNGL(0.0D0)
!     Kp Values
      kp_array(1)   = SNGL(FLUXKP)   ! akp(1) = kp delayed by 3 hours,
      kp_array(2)   = SNGL(0.0D0)    ! akp(2) = 0. 
      kp_array(3)   = SNGL(FLUXKP)   ! akp(3) = mean of last 24 hours,
      kp_array(4)   = SNGL(0.0D0)    ! akp(4) = 0.

! ALTITUDE    

      !  Convert Altitude to KM
      ALTKM = SNGL(ALTM/1000.D0)
      !  Check that ALTKM > 120 KM.
      IF(ALTKM .LT. 120.0) THEN
          WRITE(6,*) '*****************************************'
          WRITE(6,*) '********ERROR IN DTM2020_call.f90********'
          WRITE(6,*) '    '
          WRITE(6,*) ' Altitude is below 120 km-- ALTKM=',ALTKM
          STOP 69
      ENDIF

! LOCALTIME      
!     hl    =   local time (in radian: 0-24hr = 0-2pi)
      STLOC = SNGL(ATAN2(SINHL,COSHL))
      
! LONGITUDE AND LATITUDE   (in radian)
      GLAT = SNGL(PHI)
      GLON = SNGL(XLAMB)
        

      if(kin.eq.1) WRITE(6,*) '**********************************************'
      if(kin.eq.1) WRITE(6,*) '************* [DTM2020_call.f90] *************'
      if(kin.eq.1) WRITE(6,*) ' '
      if(kin.eq.1) WRITE(6,*) ' INPUTS to CALL dtm3()'
      if(kin.eq.1) WRITE(6,*) '    iDOY         ', iDOY         
      if(kin.eq.1) WRITE(6,*) '    DOY_real         ', DOY_real         
      if(kin.eq.1) WRITE(6,*) '    flux_daily   ', flux_daily   
      if(kin.eq.1) WRITE(6,*) '    flux_avg     ', flux_avg     
      if(kin.eq.1) WRITE(6,*) '    kp_array     ', kp_array     
      if(kin.eq.1) WRITE(6,*) '    ALTKM        ', ALTKM        
      if(kin.eq.1) WRITE(6,*) '    ALTM        ', ALTM        
      if(kin.eq.1) WRITE(6,*) '    STLOC        ', STLOC        
      if(kin.eq.1) WRITE(6,*) '    GLAT         ', GLAT         
      if(kin.eq.1) WRITE(6,*) '    GLON         ', GLON         
      if(kin.eq.1) WRITE(6,*) ' '


open(146, &
& file='/data/geodyn_proj/geodyn_code/IIE/CD_model_proj/DTM_2020_F107_Kp.dat', &
& status='old') 
      call lecdtm(146)   !read in coefficients
      close (146)

      call dtm3(DOY_real,                           &
          &     flux_daily,flux_avg,kp_array,      &
          &     ALTKM,STLOC,GLAT,GLON,             &
          &     Temp_z,Temp_exo,                   &
          &     RHO_dtm2020,part_dens,           &
          &     MBAR_dtm2020)

      if(kin.eq.1) WRITE(6,*) ' OUTPUTS dtm3()'
      if(kin.eq.1) WRITE(6,*) '    Temp_z       ', Temp_z       
      if(kin.eq.1) WRITE(6,*) '    Temp_exo     ', Temp_exo     
      if(kin.eq.1) WRITE(6,*) '    RHO_dtm2020  ', RHO_dtm2020 
      if(kin.eq.1) WRITE(6,*) '    part_dens    ', part_dens 
      if(kin.eq.1) WRITE(6,*) '    MBAR_dtm2020 ', MBAR_dtm2020


      !!! Convert from g/cm^3 to kg/m^3
      RHO = DBLE(RHO_dtm2020)*1000.D0
      !TEMP = DBLE(Temp_z)

!     X(1) = part.den atomic hydrogen (in g/cm3)
!     X(2) = part.den helium
!     X(3) = part.den atomic oxygen
!     X(4) = part.den molecular nitrogen
!     X(5) = part.den molecular oxygen
!     X(6) = part.den atomic nitrogen

      num_dens_temp(1) = DBLE(part_dens(1)*1000.)/(1.6726D-27*H_amu )
      num_dens_temp(2) = DBLE(part_dens(2)*1000.)/(1.6726D-27*He_amu)
      num_dens_temp(3) = DBLE(part_dens(3)*1000.)/(1.6726D-27*O_amu )
      num_dens_temp(4) = DBLE(part_dens(4)*1000.)/(1.6726D-27*N2_amu)
      num_dens_temp(5) = DBLE(part_dens(5)*1000.)/(1.6726D-27*O2_amu)
      num_dens_temp(6) = DBLE(part_dens(6)*1000.)/(1.6726D-27*N_amu )
     
      num_dens_temp(7)  = RHO
      num_dens_temp(8)  = DBLE(Temp_z)
      num_dens_temp(9)  = DBLE(Temp_exo)
      num_dens_temp(10) = DBLE(MBAR_dtm2020)/1000.D0  !mean molecular mass (in kg)
      
!!!!   CALCUALTE DRHODZ
       GSURF_ZL = GSURF*(RE/(RE + ZL) )**2
       TERM1 = ((-1.66D-24*GSURF_ZL)/RGAS) 
       
       ! !## convert from 1/m^3 to 1/cm^3
       TERM_sp = (num_dens_temp(1)*0.0000010D0*H_amu      &   
    &           + num_dens_temp(2)*0.0000010D0*He_amu     &   
    &           + num_dens_temp(3)*0.0000010D0*O_amu      &   
    &           + num_dens_temp(4)*0.0000010D0*N2_amu     &   
    &           + num_dens_temp(5)*0.0000010D0*O2_amu     &   
    &           + num_dens_temp(6)*0.0000010D0*N_amu      &   
    &           + 0.0D0 )*(1.D0/DBLE(Temp_z))
       TERMnorm1 = 1.D0/(1.D0 + ZL/RE)**2
       TERMnorm2 = ((RE+ZL)/(RE+(ALTM/1000.D0)))**2    
       
       DRHODZ =TERM1*(TERM_sp)*TERMnorm1*TERMnorm2
!       DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.

      
      
      if(kin.eq.1) WRITE(6,*) 'Convert to Double'
      if(kin.eq.1) WRITE(6,*) '    RHO       ', RHO 
      if(kin.eq.1) WRITE(6,*) '    num_dens  ', num_dens_temp 
      if(kin.eq.1) WRITE(6,*) '    DRHODZ    ', DRHODZ 

   99 RETURN
      END    


            !!!! The subroutine for calling DTM2020 is in the dtm2020_F107_Kp-subr.f90 file 
            !!!    i guess it is the 3rd version of dtm (   so dtm3()  )
            !!!    returns density in G/CM^3
