!$DTM2020_call
      SUBROUTINE DTM2020_call(MJDSEC,FSEC,                 &
              &               FLUXD,FLXAVG,FLUXKP,         &
              &               ALTM,PHI,XLAMB,              &
              &               COSHL,SINHL,                 &
              &               RHO,DRHODZ, dtmversion_model,&
              &               XKP,INDEX,IKPAP,I324)
      
      
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


!   XKP(*)   I    A    AN ARRAY CONTAIN 3-HOURLY KP VALUES.  MUST
!                      GO BACK AT LEAST 2.5 DAYS FROM PRESENT TIME. MAY
!                      ALSO CONTAIN AP VALUES, WHICH IS SPECIFIED
!                      BY IKPAP.
!   INDEX    I    S    AN INDEX WHICH TELLS WHERE IN XKP RESIDES
!                      THE PRESENT 3-HOUR KP VALUE.
!   IKPAP    I    S    A FLAG FOR KP/AP.  >0 MEANS KP INPUT IN XKP,
!                      <0 MEANS AP INPUT IN XKP.
!   I324     I    S    =3 MEANS 3-HOURLY VALUES USED, =24 MEANS
!                      24-HOUR AP/KP VALUES STORED IN XKP.

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

      CHARACTER(len=*) dtmversion_model
      CHARACTER(len = 48) global_path
      CHARACTER(len = 26) filename_ap60
      CHARACTER(len = 74) file_ap60
      
      CHARACTER(len = 4) year
      CHARACTER(len = 2) month
      CHARACTER(len = 2) day
      CHARACTER(len = 2) hour
      CHARACTER(len = 2) minute
      CHARACTER(len = 2) sec
      
      
      !integer(8), allocatable :: YYY(:)
      !integer(8), allocatable :: MM(:)
      !integer(8), allocatable :: DD(:)
      !real(8), allocatable    :: hh_h(:)
      !real(8), allocatable    :: hh_m(:)
      CHARACTER(4), allocatable :: YYY(:)
      CHARACTER(2), allocatable :: MM(:)
      CHARACTER(2), allocatable :: DD(:)
      CHARACTER(4), allocatable    :: hh_h(:)
      CHARACTER(5), allocatable    :: hh_m(:)
      !
      real(8), allocatable    :: days(:)
      real(8), allocatable    :: days_m(:)
      real(8), allocatable    :: Hp60(:)
      integer(8), allocatable :: ap60(:)
      integer(8), allocatable :: D(:)

      CHARACTER(len = 12) c_YYMMDDHHMMSS
      CHARACTER(len = 6)  c_YYMMDD
      CHARACTER(len = 6)  c_HHMMSS



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
      real(8) :: pass_24meanKp

      REAL(4), DIMENSION(10) ::  p60_array
      DIMENSION XKP(*)
      DIMENSION AP(7)

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
      global_path = '/data/geodyn_proj/geodyn_code/IIE/CD_model_proj/'
      
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
      

   IF(dtmversion_model=='o') THEN  
      ! FOR OPERATION Vers. use 3-hour Kp
      ! I324 must be = 3        --> fill array with 3hr values
      ! IKPAP must be positive  --> use Kp
      CALL KPTRAN_dtm2020(XKP,INDEX,I324,IKPAP,AP,IERR,dtmversion_model, pass_24meanKp)
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  dtmversion_model ', dtmversion_model         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  kptran, INDEX ', INDEX         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  kptran, I324  ', I324         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  kptran, IKPAP ', IKPAP         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  kptran, IERR  ', IERR         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  kp after  kptran ', XKP(INDEX)         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  kp after  kptran ', XKP(INDEX-1)         
         if(kin.eq.1) WRITE(6,*) 'dtm2020_ops_call:  ap after  kptran ', AP      
         
      kp_array(1) = SNGL(XKP(INDEX))    ! akp(1) = kp delayed by 3 hours,
      kp_array(2) = SNGL(0.0D0)         ! akp(2) = 0. 
      kp_array(3) = SNGL(pass_24meanKp) ! akp(3) = mean of last 24 hours,
      kp_array(4) = SNGL(0.0D0)         ! akp(4) = 0.     

         if(kin.eq.1) WRITE(6,*) '**********************************************'
         if(kin.eq.1) WRITE(6,*) '************* [DTM2020_call.f90] *************'
         if(kin.eq.1) WRITE(6,*) ' '
         if(kin.eq.1) WRITE(6,*) ' INPUTS to CALL dtm3()'
         if(kin.eq.1) WRITE(6,*) '    iDOY         ', iDOY         
         if(kin.eq.1) WRITE(6,*) '    DOY_real     ', DOY_real         
         if(kin.eq.1) WRITE(6,*) '    flux_daily   ', flux_daily   
         if(kin.eq.1) WRITE(6,*) '    flux_avg     ', flux_avg     
         if(kin.eq.1) WRITE(6,*) '    kp_array     ', kp_array     
         if(kin.eq.1) WRITE(6,*) '    ALTKM        ', ALTKM        
         if(kin.eq.1) WRITE(6,*) '    ALTM         ', ALTM        
         if(kin.eq.1) WRITE(6,*) '    STLOC        ', STLOC        
         if(kin.eq.1) WRITE(6,*) '    GLAT         ', GLAT         
         if(kin.eq.1) WRITE(6,*) '    GLON         ', GLON         
         if(kin.eq.1) WRITE(6,*) ' '
!read in coefficients for the operation version (F107 and Kp)
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
      
   ENDIF
   
   
   
   
   
! -------------------- RESEARCH VERSION -------------------- !
   
   
   
   
   
   IF(dtmversion_model == 'r') THEN  ! FOR RESEARCH Vers. use 1-hour Ap60
        
       !!!! Open the ap60 file on the first entry
       if(kin.eq.1)then
          filename_ap60 = 'dtm2020_Hp60_2018_2019.txt'
          file_ap60  = trim(global_path)//trim(filename_ap60)
          if(kin.eq.1) WRITE(6,*) '    AP60 File     ', file_ap60     

          !
          open (123,  file=trim(file_ap60), status='old',action="read")   
            !! Skip the preamble-- 35 lines
            do i=1,35
                read(123,*)
            end do

          !!! Read thru the file once to find how long it is.
          n = 0
          do
            read(123,*,end=1)
            n = n+1
          end do
          
          !!! set the pointer back to the start of file
          1 rewind(123)
            
          !!! Allocate arrays that are length of file
          allocate(YYY(n))
          allocate(MM(n))
          allocate(DD(n))
          allocate(hh_h(n))
          allocate(hh_m(n))
          allocate(days(n))
          allocate(days_m(n))
          allocate(Hp60(n))
          allocate(ap60(n))
          allocate(D(   n))
          
          !! Skip the preamble-- 35 lines
            do i=1,35
                read(123,*)
            end do
          
          do i = 1, n
           !!! Loop thru the file and save as arrays
           read(UNIT=123,                                           &
!    &  FMT="(I4,1x,2(I2,1x),F4.1,1x,F5.2,1x,2(F11.5,1x),F6.3,1x,I4,1x,I1)") &
    &  FMT="(A4,1x,2(A2,1x),A4,1x,A5,1x,2(F11.5,1x),F6.3,1x,I4,1x,I1)") &
            &   YYY(i),       &      
            &   MM(i),        &      
            &   DD(i),        &
            &   hh_h(i),      &      
            &   hh_m(i),      &      
            &   days(i),      &      
            &   days_m(i),    &      
            &   Hp60(i),      &      
            &   ap60(i),      &      
            &   D(i)

          end do
          close(123)
        WRITE(6,*) 'File read in correctly?'
        WRITE(6,*) '     YYY       :',YYY(1)
        WRITE(6,*) '     MM        :',MM(1)
        WRITE(6,*) '     DD        :',DD(1)
        WRITE(6,*) '     hh_h      :',hh_h(1)
        WRITE(6,*) '     hh_m      :',hh_m(1)
        WRITE(6,*) '     days      :',days(1)
        WRITE(6,*) '     days_m    :',days_m(1)
        WRITE(6,*) '     Hp60      :',Hp60(1)
        WRITE(6,*) '     ap60      :',ap60(1)
        WRITE(6,*) '     D         :',D(1)
      endif         
         
      !!!!!  Get the variables containing YYMMDD and HHMMSS
      !!     Gather the input date for this timestep as as string 
      !!     concatenate the two portions
      

      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      !MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)     
      write(c_YYMMDD,'(I0.6)')   IYMD
      write(c_HHMMSS,'(I0.6)')   IHMS
      !i_YYMMDDHHMMSS =  trim(i_YYMMDD//i_HHMMSS)
      
      year  = "20"//c_YYMMDD(1:2)
      month = c_YYMMDD(3:4)
      day   = c_YYMMDD(5:6)
      hour   = c_HHMMSS(1:2)
      minute = c_HHMMSS(3:4)
      sec    = c_HHMMSS(5:6)
         if(kin.eq.1) WRITE(6,*) '               '
         if(kin.eq.1) WRITE(6,*) '     year        :', year
         if(kin.eq.1) WRITE(6,*) '     month       :', month
         if(kin.eq.1) WRITE(6,*) '     day         :', day
         if(kin.eq.1) WRITE(6,*) '     hour        :', hour
         if(kin.eq.1) WRITE(6,*) '     minute      :', minute
         if(kin.eq.1) WRITE(6,*) '     sec         :', sec 
         if(kin.eq.1) WRITE(6,*) '               '
         if(kin.eq.1) WRITE(6,*) '     c_YYMMDD         :',c_YYMMDD
         if(kin.eq.1) WRITE(6,*) '     c_HHMMSS         :',c_HHMMSS
         if(kin.eq.1) WRITE(6,*) '               '

!!!!!  Read the file, looping through the rows and compare the date in each row to the date GEODYN is requesting
!!!!! loop through each column and compare the date values incrementally (year, month, day)
   nrows=n   
   do i_row = 1, nrows    
      !#####  If the date of the row matches the requested date, load the remaining values in that row into the save arrays
      !date = datearray(i_row)
      if (year == YYY(i_row) ) then
       if (month == MM(i_row) ) then
        if (day == DD(i_row) ) then
         if (hour == hh_h(i_row)(1:2) ) then
              WRITE(6,*) '_loop_ DATE match ',  YYY(i_row),MM(i_row),DD(i_row),hh_h(i_row)(1:2)
              WRITE(6,*) '       Hp60/ap60', Hp60(i_row), ap60(i_row)
             
             SUM = 0.0D0
                    DO 20 I=1,24
             20       SUM = SUM + ap60(i_row-I)
                      mean_24hr = SUM/24.0D0
             SUM = 0.0D0
                    DO 21 I=5,7
             21       SUM = SUM + ap60(i_row-I)
                      mean_5_6_7hr = SUM/3.0D0
             SUM = 0.0D0
                    DO 22 I=9,11
             22       SUM = SUM + ap60(i_row-I)
                      mean_9_10_11hr = SUM/3.0D0
             SUM = 0.0D0
                    DO 23 I=14,16
             23       SUM = SUM + ap60(i_row-I)
                      mean_14_15_16hr = SUM/3.0D0
             SUM = 0.0D0
                    DO 24 I=19,21
             24       SUM = SUM + ap60(i_row-I)
                      mean_19_20_21hr = SUM/3.0D0

             p60_array(1)   = SNGL(REAL(ap60(i_row-4))  )  !  ap60(1) = 4hr delayed ap60 at t
             p60_array(2)   = SNGL(REAL(ap60(i_row))    )  !  ap60(2) = 0hr delayed ap60 at t
             p60_array(3)   = SNGL(REAL(ap60(i_row-1))  )  !  ap60(3) = 1hr delayed ap60 at t  
             p60_array(4)   = SNGL(REAL(ap60(i_row-2))  )  !  ap60(4) = 2hr delayed ap60 at t  
             p60_array(5)   = SNGL(REAL(ap60(i_row-3))  )  !  ap60(5) = 3hr delayed ap60 at t  
             p60_array(6)   = SNGL(REAL(mean_24hr)      )  !  ap60(6) = mean of last 24 hours
             p60_array(7)   = SNGL(REAL(mean_5_6_7hr)   )  !  ap60(7) = mean of 5-6-7hr delayed at t       
             p60_array(8)   = SNGL(REAL(mean_9_10_11hr) )  !  ap60(8) = mean of 9-10-11hr delayed at t     
             p60_array(9)   = SNGL(REAL(mean_14_15_16hr))  !  ap60(9) = mean of 14-15-16hr delayed at t     
             p60_array(10)  = SNGL(REAL(mean_19_20_21hr))  !  ap60(10)= mean of 19-20-21hr delayed at t

        !#####  Read the next 8 rows in to form the cube around our ephemeris point on this Time
          !WRITE(6,*) ' '
!          do iloop=1,9
!            dates(iloop) = datearray(  i_row + (iloop-1))
!            lons(iloop)  = lonarray(   i_row + (iloop-1))
!            lats(iloop)  = latarray(   i_row + (iloop-1))
!            alts(iloop)  = altarray(   i_row + (iloop-1))
!            rhos(iloop)  = rho_inarray(i_row + (iloop-1))
!            
!            ndens_O1(iloop)  = nden_O1_inarray(i_row + (iloop-1))
!            ndens_O2(iloop)  = nden_O2_inarray(i_row + (iloop-1))
!            ndens_HE(iloop)  = nden_HE_inarray(i_row + (iloop-1))
!            ndens_N2(iloop)  = nden_N2_inarray(i_row + (iloop-1))
!            Temps(iloop)     = Temp_inarray(i_row + (iloop-1))
!          end do
          
        !########    EXIT the do loop if you got the right date
           exit  !!!! CHANGE HERE
        !########   
           
      endif ! End the year  check
       endif ! End the month check
        endif ! End the day   check
         endif ! End the hour  check

       end do   !!!! CHANGE HERE      ! To fill the P60 array, need to do the following:
      !    1)  read the 60 minute Ap data file, 
      !    2)  Find index of the correct time (to the hour)
      !    3)  Do the calculations to fill the array with correct indicies


   
  
   !read in coefficients for the Research version (Hp and Ap)
open(146, &
& file='/data/geodyn_proj/geodyn_code/IIE/CD_model_proj/DTM_2020_F30_ap60.dat', &
& status='old') 
      call lecdtm_res(146)   !read in coefficients
      close (146)
         
         
     if(kin.eq.1) WRITE(6,*) '**********************************************'
     if(kin.eq.1) WRITE(6,*) '************* [DTM2020_call.f90] *************'
     if(kin.eq.1) WRITE(6,*) ' '
     if(kin.eq.1) WRITE(6,*) ' INPUTS to CALL dtm5()--- research version'
     if(kin.eq.1) WRITE(6,*) '    iDOY         ', iDOY         
     if(kin.eq.1) WRITE(6,*) '    DOY_real     ', DOY_real         
     if(kin.eq.1) WRITE(6,*) '    flux_daily   ', flux_daily   
     if(kin.eq.1) WRITE(6,*) '    flux_avg     ', flux_avg     
     if(kin.eq.1) WRITE(6,*) '    p60_array     ', p60_array     
     if(kin.eq.1) WRITE(6,*) '    ALTKM        ', ALTKM        
     if(kin.eq.1) WRITE(6,*) '    ALTM         ', ALTM        
     if(kin.eq.1) WRITE(6,*) '    STLOC        ', STLOC        
     if(kin.eq.1) WRITE(6,*) '    GLAT         ', GLAT         
     if(kin.eq.1) WRITE(6,*) '    GLON         ', GLON   

      call dtm5(DOY_real,                           &
          &     flux_daily,flux_avg,p60_array,      &
          &     ALTKM,STLOC,GLAT,GLON,             &
          &     Temp_z,Temp_exo,                   &
          &     RHO_dtm2020,part_dens,           &
          &     MBAR_dtm2020)
      
   
   
   ENDIF    !!! End the DTM2020_research version
         

   

        








      if(kin.eq.1) WRITE(6,*) ' OUTPUTS dtm'
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
