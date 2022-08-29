!$KAMODO
      SUBROUTINE KamodoModels_oc(MJDSEC,FSEC,ALTM,XLATD,XLOND,        &
        &     PHI,XLAMB,RHO,DRHODZ, DAY,IYR, kamodo_model, model_path,&
        &            orbitcloud_file, n_dens_temp )
!             
!********1*********2*********3*********4*********5*********6*********7**
! KamodoModels_oc ---------- June, 29, 2021 ----- Written by Zach Waldron
!         (orbit cloud)
!
!
! FUNCTION:
!      THIS ROUTINE SERVES AS A WRAPPER FOR CALLING 
!        THE THE PYTHON KAMODO CODE.
!    
!      In this method we do not call python or kamodo directly from Fortran.
!      Instead we use a file that contains the expected orbit from a
!         pre-initialized run of GEODYN w/ msis2 with a cloud of 
!         uncertainty around that orbit.
!
!
!   I/O PARAMETERS:
!   NAME           I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------         ---  ---   ------------------------------------------------
!   MJDSEC          I    S    Time in integral seconds from geodyn ref time.
!   FSEC            I    S    Fractional remaining seconds
!   ALTM            I         Height of satellite above elipsoid (M)  
!   XLATD           I         Lat of Space station (degrees) 
!   XLOND           I         Lon of Space station (degrees)
!   PHI             I    S    Geodetic latitude  in radians
!   XLAMB           I    S    Geodetic longitude in radians
!   RHO             O    S    Density of atmosphere as given by the REQUESTED model (KG/M3)
!   DRHODZ          O    S    Partial derivative of RHO wrt HEIGHT valid only if IDRV = 1.
!   DAY             I    S    DAY OF YEAR, INCLUDING FRACTION OF DAY (i.e. JAN. 1, 6AM = 0.25, Jan 2 6AM = 1.25 etc.)
!   IYR             I    S    YEAR PAST 1900 (I.E. 78 = 1978 ETC.)
!  kamodo_model     I   CHAR  Name of chosen model output from Kamodo
!  model_path       I   CHAR  Path to orbit cloud file location (and model output)
!  orbitcloud_file  I   CHAR  File name for orbit cloud file, 
!  n_dens_temp      O    A    Array containing the various outputs from Kamodo
!                                    nden(N2,O2,O1,He), RHO, TEMP
!
! COMMENTS:
! Data input for indexing the FILE:
!    Need to construct these:
!        - sat_time     is the satellite timestamp in UTC since
!                       Jan 1 1970 (unix time)
!        - sat_height   is satellite altitude above the ground (in km)
!        - sat_lat      satellite latitude
!        - sat_lon      satellite longitude    
!    String Input:
!        - File_dir     is the directory where the data is located
!    Constants:
!        - rho          is the variable name
!        - ilev         means the variable depends on the pressure 
!                        level for the CTIPe model.
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      COMMON/PARMB/GSURF,RE

      DATA ICNT/0/,RADDEG/57.29577951D0/
      DATA RGAS/831.4D0/,ZL/120.D0/

      CHARACTER(len=*) kamodo_model
      CHARACTER(len = 1)   s 
      INTEGER(8) :: IYYDDD

      CHARACTER(len = 12) i_YYMMDDHHMMSS
      CHARACTER(len = 6)  i_YYMMDD
      CHARACTER(len = 6)  i_HHMMSS
      CHARACTER(len = 300) global_path
      CHARACTER(len = 300) model_path1
      CHARACTER(len = 300) model_path2
      CHARACTER(len = 300) orbit_cloud_path
      
      CHARACTER(len = 200) model_path
      CHARACTER(len = 200) orbitcloud_file
      integer :: i_row
      integer :: inum
      
      CHARACTER(len = 12) :: date
!
      REAL ::  min_lon
      REAL ::  max_lon
      REAL ::  min_lat
      REAL ::  max_lat
      REAL ::  min_alt
      REAL ::  max_alt

      !real, allocatable :: data_input(:,:)
      CHARACTER(len = 12), allocatable :: datearray(:)
      real, allocatable                :: lonarray(:)
      real, allocatable                :: latarray(:)
      real, allocatable                :: altarray(:)
      DOUBLE PRECISION, allocatable    :: rho_inarray(:)
      DOUBLE PRECISION, allocatable    :: nden_O1_inarray(:)
      DOUBLE PRECISION, allocatable    :: nden_O2_inarray(:)
      DOUBLE PRECISION, allocatable    :: nden_HE_inarray(:)
      DOUBLE PRECISION, allocatable    :: nden_N2_inarray(:)
      DOUBLE PRECISION, allocatable    :: Temp_inarray(:)

      CHARACTER(len = 12),dimension(9) :: dates
      REAL,dimension(9) :: lons
      REAL,dimension(9) :: lats
      REAL,dimension(9) :: alts
      DOUBLE PRECISION,dimension(9) :: rhos
      DOUBLE PRECISION,dimension(9) :: ndens_O1
      DOUBLE PRECISION,dimension(9) :: ndens_O2
      DOUBLE PRECISION,dimension(9) :: ndens_HE
      DOUBLE PRECISION,dimension(9) :: ndens_N2
      DOUBLE PRECISION,dimension(9) :: Temps


      REAL(8), dimension(6) :: n_dens_temp 


      !DOUBLE PRECISION :: rho_interpd
      !DOUBLE PRECISION :: ndenO1_interpd
      !DOUBLE PRECISION :: ndenO2_interpd
      !DOUBLE PRECISION :: ndenHE_interpd
      !DOUBLE PRECISION :: ndenN2_interpd
      !DOUBLE PRECISION :: Temp_interpd

!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
      ICNT = ICNT+1
      IERR = 0
      DRHODZ = 0.0D0
!                                                                       &
!! Print to UNIT6 so the user can confirm the correct 
!! subroutine is being used.
      if(ICNT.eq.1)then
          WRITE(6,*) 'CHECK-- KamodoModels_oc.f90: using Kamodo '
          WRITE(6,*) 'kamodo_model:             ', kamodo_model
          
          !!!! Open the file on the first entry
          orbit_cloud_path = trim(orbitcloud_file)
          open (123,  file=trim(orbit_cloud_path), status='old',action="read")   
          
          !!! Read thru the file once to find how long it is.
          n = 0
          do
            read(123,*,end=1)
            n = n+1
          end do
          
          !!! set the pointer back to the start of file
          1 rewind(123)
            
          !!! Allocate arrays that are length of file
          allocate(datearray(n))
          allocate(lonarray(n))
          allocate(latarray(n))
          allocate(altarray(n))
          allocate(rho_inarray(n))
          allocate(nden_O1_inarray(n))
          allocate(nden_O2_inarray(n))
          allocate(nden_HE_inarray(n))
          allocate(nden_N2_inarray(n))
          allocate(Temp_inarray(   n))
          
          do i = 1, n
!        read(UNIT=123,FMT="(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
!            &  FMT="(A12,3x,3(F9.4,3x),D15.6,3x,4(D12.5,3x),F8.4)")  &

           !!! Loop thru the file and save as arrays
           read(UNIT=123,                                           &
            &  FMT="(A12,1x,F10.4,2x,2(F9.4,2x),1x,D15.6,2x,4(D12.5,2x),D11.4)") &
            &   datearray(i),                     &      
            &   lonarray(i),                      &      
            &   latarray(i),                      &
            &   altarray(i),                      &      
            &   rho_inarray(i),                   &      
            &   nden_O1_inarray(i),               &      
            &   nden_O2_inarray(i),               &      
            &   nden_HE_inarray(i),               &      
            &   nden_N2_inarray(i),               &      
            &   Temp_inarray(i)

          end do
          close(123)
        WRITE(6,*) 'File read in correctly?'
        WRITE(6,*) '     datearray       :',datearray(1)
        WRITE(6,*) '     lonarray        :',lonarray(1)
        WRITE(6,*) '     latarray        :',latarray(1)
        WRITE(6,*) '     altarray        :',altarray(1)
        WRITE(6,*) '     rho_inarray     :',rho_inarray(1)
        WRITE(6,*) '     nden_O1_inarray :',nden_O1_inarray(1)
        WRITE(6,*) '     nden_O2_inarray :',nden_O2_inarray(1)
        WRITE(6,*) '     nden_HE_inarray :',nden_HE_inarray(1)
        WRITE(6,*) '     nden_N2_inarray :',nden_N2_inarray(1)
        WRITE(6,*) '     Temp_inarray    :',Temp_inarray(1)

      endif
      
!                                                                       &
!!*********************************************************************
!!    PREPARE THE DATA INPUTS TO KAMODO:
!!        - sat_time     is the satellite timestamp in UTC since
!!                       Jan 1 1970 (unix time)
!!        - sat_height   is satellite altitude above the ground (in km)
!!        - sat_lat      satellite latitude
!!        - sat_lon      satellite longitude    
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!!! Convert PHI and XLAMB TO GLAT, GLON (deg):
      GLAT = PHI*RADDEG
      GLON = XLAMB*RADDEG

!!!!! Put Sat_height in kilometers (km):
      ALTKM = ALTM/1000.D0
!
!!!!!  Get the variables containing YYMMDD and HHMMSS
      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)     
!                                                                       &
      write(i_YYMMDD,'(I0.6)')   IYMD
      write(i_HHMMSS,'(I0.6)')   IHMS
      i_YYMMDDHHMMSS =  trim(i_YYMMDD//i_HHMMSS)

!!!!! Gather the input date for this timestep as as string 
  !!!  concatenate the two portion
      
   

   
   nrows=n
   !#####  Read the file, looping through the rows and compare the date in each row to the date GEODYN is requesting
   do i_row = 1, nrows    
      !#####  If the date of the row matches the requested date, load the remaining values in that row into the save arrays
      date = datearray(i_row)
      if (i_YYMMDDHHMMSS == date ) then
        
        !#####  Read the next 8 rows in to form the cube around our ephemeris point on this Time
          !WRITE(6,*) ' '
          do iloop=1,9
            dates(iloop) = datearray(  i_row + (iloop-1))
            lons(iloop)  = lonarray(   i_row + (iloop-1))
            lats(iloop)  = latarray(   i_row + (iloop-1))
            alts(iloop)  = altarray(   i_row + (iloop-1))
            rhos(iloop)  = rho_inarray(i_row + (iloop-1))
            
            ndens_O1(iloop)  = nden_O1_inarray(i_row + (iloop-1))
            ndens_O2(iloop)  = nden_O2_inarray(i_row + (iloop-1))
            ndens_HE(iloop)  = nden_HE_inarray(i_row + (iloop-1))
            ndens_N2(iloop)  = nden_N2_inarray(i_row + (iloop-1))
            Temps(iloop)     = Temp_inarray(i_row + (iloop-1))

          end do

     !####  Check the mimum reqd values to be sure we are still within the cube.
   !  min_lon = MINVAL(lons)
  !   max_lon = MAXVAL(lons)
 !    min_lat = MINVAL(lats)
!     max_lat = MAXVAL(lats)
     min_alt = MINVAL(alts)
     max_alt = MAXVAL(alts)
!    if(GLON.gt.max_lon)then
!        WRITE(6,*) 'OrbitCloudError-- lon ABOVE bounds:    ', GLON, max_lon
!        STOP 16
!    endif
!    if(GLON.lt.min_lon)then
!        WRITE(6,*) 'OrbitCloudError-- lon BELOW bounds:    ', GLON, min_lon, max_lon
!        WRITE(6,*) ' i_YYMMDDHHMMSS              ', i_YYMMDDHHMMSS
!        WRITE(6,*) ' geodyn_GLON                ', GLON
!        WRITE(6,*) ' geodyn_GLAT                ', GLAT
!        WRITE(6,*) ' geodyn_ALTKM               ', ALTKM
!        WRITE(6,*) ' '
!        WRITE(6,*) ' date                       ', dates
!        WRITE(6,*) ' orbitcloud_lons center     ', lons(1)
!        WRITE(6,*) ' orbitcloud_lats center     ', lats(1)
!        WRITE(6,*) ' orbitcloud_alts center     ', alts(1)
!        STOP 16
!    endif
!    if(GLAT.gt.max_lat)then
!        WRITE(6,*) 'OrbitCloudError-- lat ABOVE bounds:    ', GLAT, max_lat
!        STOP 16
!    endif
!    if(GLAT.lt.min_lat)then
!        WRITE(6,*) 'OrbitCloudError-- lat BELOW bounds:    ', GLAT, min_lat
!        STOP 16
!    endif
    if(ALTKM.gt.max_alt)then
        WRITE(6,*) "OrbitCloudError-- alt ABOVE bounds:    ", ALTKM, max_alt
        STOP 16
    endif
    if(ALTKM.lt.min_alt)then
        WRITE(6,*) "OrbitCloudError-- alt BELOW bounds:    ", ALTKM, min_alt
        WRITE(6,*) ' i_YYMMDDHHMMSS             ', i_YYMMDDHHMMSS
        WRITE(6,*) ' geodyn_GLON                ', GLON
        WRITE(6,*) ' geodyn_GLAT                ', GLAT
        WRITE(6,*) ' geodyn_ALTKM               ', ALTKM
        WRITE(6,*) ' '
        WRITE(6,*) ' date                       ', dates
        WRITE(6,*) ' orbitcloud_lons center     ', lons(1)
        WRITE(6,*) ' orbitcloud_lats center     ', lats(1)
        WRITE(6,*) ' orbitcloud_alts center     ', alts(1)
        STOP 16
    endif

    


        !########    EXIT if you got the right date
           exit  !!!! CHANGE HERE
        !########   
           
           endif

       end do   !!!! CHANGE HERE

    if(kamodo_model=='TIEGCM') then  
        !!!! Interpolate the various FIELDS stored in the "data cube"
        call trilinear_interp(GLON,GLAT,ALTKM,rhos,     n_dens_temp(1))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_O1, n_dens_temp(2))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_O2, n_dens_temp(3))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_HE, n_dens_temp(4))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_N2, n_dens_temp(5))
        call trilinear_interp(GLON,GLAT,ALTKM,Temps,    n_dens_temp(6))
      if(ICNT.eq.1)then
          WRITE(6,*) 'rho_interpd      ',  n_dens_temp(1) ! g/cm^3
          WRITE(6,*) 'ndenO1_interpd   ',  n_dens_temp(2)  ! #/cm^3
          WRITE(6,*) 'ndenO2_interpd   ',  n_dens_temp(3)  ! #/cm^3
          WRITE(6,*) 'ndenHE_interpd   ',  n_dens_temp(4)  ! #/cm^3
          WRITE(6,*) 'ndenN2_interpd   ',  n_dens_temp(5)  ! #/cm^3
          WRITE(6,*) 'Temp_interpd     ',  n_dens_temp(6)  ! Kelvin
      endif
    endif
     
    if(kamodo_model=='CTIPe') then  
        !!!! Interpolate the various FIELDS stored in the "data cube"
        call trilinear_interp(GLON,GLAT,ALTKM,rhos,     n_dens_temp(1))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_O1, n_dens_temp(2))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_O2, n_dens_temp(3))
        call trilinear_interp(GLON,GLAT,ALTKM,ndens_N2, n_dens_temp(5))
        call trilinear_interp(GLON,GLAT,ALTKM,Temps,    n_dens_temp(6))
        if(ICNT.eq.1)then
          WRITE(6,*) 'rho_interpd      ',  n_dens_temp(1) ! g/cm^3
          WRITE(6,*) 'ndenO1_interpd   ',  n_dens_temp(2)  ! #/cm^3
          WRITE(6,*) 'ndenO2_interpd   ',  n_dens_temp(3)  ! #/cm^3
          WRITE(6,*) 'ndenN2_interpd   ',  n_dens_temp(5)  ! #/cm^3
          WRITE(6,*) 'Temp_interpd     ',  n_dens_temp(6)  ! Kelvin
        endif
    endif
   
   
   
    if(kamodo_model=='GITM') then  
        !!!! Interpolate the various FIELDS stored in the "data cube"
        call trilinear_interp(GLON,GLAT,ALTKM,rhos,     n_dens_temp(1))
        !call trilinear_interp(GLON,GLAT,ALTKM,ndens_O1, n_dens_temp(2))
        !call trilinear_interp(GLON,GLAT,ALTKM,ndens_O2, n_dens_temp(3))
        !call trilinear_interp(GLON,GLAT,ALTKM,ndens_N2, n_dens_temp(5))
        call trilinear_interp(GLON,GLAT,ALTKM,Temps,    n_dens_temp(6))
        if(ICNT.eq.1)then
          WRITE(6,*) 'rho_interpd      ',  n_dens_temp(1) ! g/cm^3
        !  WRITE(6,*) 'ndenO1_interpd   ',  n_dens_temp(2)  ! #/cm^3
        !  WRITE(6,*) 'ndenO2_interpd   ',  n_dens_temp(3)  ! #/cm^3
        !  WRITE(6,*) 'ndenN2_interpd   ',  n_dens_temp(5)  ! #/cm^3
          WRITE(6,*) 'Temp_interpd     ',  n_dens_temp(6)  ! Kelvin
        endif
    endif   
   
    if ( kamodo_model == "HASDM" ) then
        call trilinear_interp(GLON,GLAT,ALTKM,rhos,  n_dens_temp(1))
        RHO = n_dens_temp(1)
        DRHODZ = 0.D0 
        n_dens_temp(2)   = 0.D0
        n_dens_temp(3)   = 0.D0
        n_dens_temp(4)   = 0.D0
        n_dens_temp(5)   = 0.D0
        n_dens_temp(6)   = 0.D0
                  
        if(ICNT.eq.1)then
          WRITE(6,*) 'Using hasdm   ', kamodo_model
          WRITE(6,*) '---------------------------'
          WRITE(6,*) 'rho_interpd      ',  n_dens_temp(1) ! g/cm^3
          WRITE(6,*) '   hasdm only provides rho, no constituents'
        endif
      end if
   
   
    RHO = n_dens_temp(1)*1000.D0
    !DRHODZ = 0


!  ZACH WALDRON UPDATED THE DRHO/DZ CALCULATION.
        ! Zach-- added O2 
        ! Zach-- Made the GSURF consistent with ZL
        ! Zach-- Added separate term for the AnomO scale height
        !           to account for the anomolous temperature (4000 K)
        ! Zach-- Added a flag to for easier version call for comparison runs.
!
!
!         
      GSURF_ZL = GSURF*(RE/(RE + ZL) )**2
!
      TERM1 = ((-1.66D-24*GSURF_ZL)/RGAS) 
      
    if(kamodo_model=='TIEGCM') then  
        TERM_species = (16.D0  * n_dens_temp(4)   +   &  ! n_HE
           &            256.D0 * n_dens_temp(2)   +   &  ! n_O1
           &            784.D0 * n_dens_temp(5)   +   &  ! n_N2
           &            1024.D0* n_dens_temp(3)       &  ! n_O2
           &                     )*(1/n_dens_temp(6))    
    endif
    if(kamodo_model=='CTIPe') then  
        TERM_species = (256.D0 * n_dens_temp(2)   +   &  ! n_O1
           &            784.D0 * n_dens_temp(5)   +   &  ! n_N2
           &            1024.D0* n_dens_temp(3)       &  ! n_O2
           &                     )*(1/n_dens_temp(6))    
    endif
!           
      TERMnorm1 = 1.D0/(1.D0 + ZL/RE)**2
      TERMnorm2 = ((RE+ZL)/(RE+ALTKM))**2    
      DRHODZ    = TERM1*(TERM_species)*TERMnorm1*TERMnorm2
!
!       DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.

      if(ICNT.eq.1)then
          write(6,*) '[KAMODO.f90 -- DRHODZ',  DRHODZ
      endif


      
      
      

      
          
!                                                                       &

   99 RETURN
      END





!    ndenO1_interpd   ',  n_dens_temp(2)
!    ndenO2_interpd   ',  n_dens_temp(3)
!    ndenHE_interpd   ',  n_dens_temp(4)
!    ndenN2_interpd   ',  n_dens_temp(5)
!    Temp_interpd     ',  n_dens_temp(6)





