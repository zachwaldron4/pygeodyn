!$Manual_Density
      SUBROUTINE ManualDensityInput(MJDSEC,FSEC,              &
        &                        RHO,DRHODZ, DAY,IYR,      &
        &                        model_path        )
!             
!********1*********2*********3*********4*********5*********6*********7**
! Manual_Density ---------- March 2023 ----- Written by Zach Waldron
!
! FUNCTION:
!         text text text
!         text text text
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
!   RHO             O    S    Density of atmosphere as given by the REQUESTED 
!                             model (KG/M3)
!   DRHODZ          O    S    Partial derivative of RHO wrt HEIGHT valid 
!                             only if IDRV = 1.
!   DAY             I    S    DAY OF YEAR, INCLUDING FRACTION OF DAY 
!                             (i.e. JAN. 1, 6AM = 0.25, Jan 2 6AM = 1.25 etc.)
!   IYR             I    S    YEAR PAST 1900 (I.E. 78 = 1978 ETC.)
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
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      COMMON/PARMB/GSURF,RE

      DATA ICNT/0/,RADDEG/57.29577951D0/
      DATA RGAS/831.4D0/,ZL/120.D0/

      CHARACTER(len = 1)   s 
      INTEGER(8) :: IYYDDD

      CHARACTER(len = 12) i_YYMMDDHHMMSS
      CHARACTER(len = 6)  i_YYMMDD
      CHARACTER(len = 6)  i_HHMMSS

      CHARACTER(len = 255) model_path
      integer :: i_row
      integer :: inum
      
      CHARACTER(len = 12) :: date
      REAL ::  min_lon
      REAL ::  max_lon
      REAL ::  min_lat
      REAL ::  max_lat
      REAL ::  min_alt
      REAL ::  max_alt

      !real, allocatable :: data_input(:,:)
      CHARACTER(len = 12), allocatable :: datearray(:)
      ! real, allocatable                :: lonarray(:)
      ! real, allocatable                :: latarray(:)
      ! real, allocatable                :: altarray(:)
      DOUBLE PRECISION, allocatable    :: rho_inarray(:)
      ! DOUBLE PRECISION, allocatable    :: nden_O1_inarray(:)
      ! DOUBLE PRECISION, allocatable    :: nden_O2_inarray(:)
      ! DOUBLE PRECISION, allocatable    :: nden_HE_inarray(:)
      ! DOUBLE PRECISION, allocatable    :: nden_N2_inarray(:)
      ! DOUBLE PRECISION, allocatable    :: Temp_inarray(:)

      CHARACTER(len = 12),dimension(9) :: dates
      ! REAL,dimension(9) :: lons
      ! REAL,dimension(9) :: lats
      ! REAL,dimension(9) :: alts
      DOUBLE PRECISION  :: rho_in
            ! REAL(8), dimension(6) :: n_dens_temp 



!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
      ICNT = ICNT+1
      IERR = 0
      DRHODZ = 0.0D0

!! Print to UNIT6 so the user can confirm the correct 
!! subroutine is being used.
      if(ICNT.eq.1)then
          ! WRITE(6,*) '     * Drag Coefficient Model (CD)    ', optionsin(4)
          ! WRITE(6,*) '          - Using the Original CD Model in GEODYN'
          WRITE(6,*) '     * ManualDensityInput.f90 '
          
          !! Read densities from file.  At this stage (March2023) we only
          !!   consider the date (ignore position)
          !!!! Open the file on the first entry
          open (123,  file=trim(model_path), status='old',action="read")   
          !!! Read thru the file once to find how long it is.
          n = 0
          do
            read(123,*,end=10)
            n = n+1
          end do

          !!! set the pointer back to the start of file
          10 rewind(unit= 123)
          rewind(unit= 123)

          if(ICNT.eq.1) WRITE(6,*) '          - n               :',n
          
          !!! Allocate arrays that are length of file
          allocate(datearray(n))
          ! allocate(lonarray(n))
          ! allocate(latarray(n))
          ! allocate(altarray(n))
          allocate(rho_inarray(n))
          
          do i = 1, n
           !!! Loop thru the file and save as arrays
           read(UNIT=123,                                           &
            &  FMT="(A12,1x,2x,D15.6)")                             &
            &   datearray(i),                     &      
            ! &   lonarray(i),                      &      
            ! &   latarray(i),                      &
            ! &   altarray(i),                      &      
            &   rho_inarray(i)  
          end do
          close(123)
        
        WRITE(6,*) '     * File read in correctly?'
        WRITE(6,*) '          - datearray       :',datearray(1)
        ! WRITE(6,*) '          - lonarray        :',lonarray(1)
        ! WRITE(6,*) '          - latarray        :',latarray(1)
        ! WRITE(6,*) '          - altarray        :',altarray(1)
        WRITE(6,*) '          - rho_inarray     :',rho_inarray(1)
        WRITE(6,*) ' '
        WRITE(6,*) '          - n               :',n

      endif
      
!! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!!!! Convert PHI and XLAMB TO GLAT, GLON (deg):
      ! GLAT = PHI*RADDEG
      ! GLON = XLAMB*RADDEG

!!!!! Put Sat_height in kilometers (km):
      ! ALTKM = ALTM/1000.D0
!
!!!!!  Get the variables containing YYMMDD and HHMMSS
      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)     
!                                                                       &
      write(i_YYMMDD,'(I0.6)')   IYMD
      write(i_HHMMSS,'(I0.6)')   IHMS
      i_YYMMDDHHMMSS =  trim(i_YYMMDD//i_HHMMSS)
    if(ICNT.eq.1)then
      WRITE(6,*) "   look for: ", i_YYMMDDHHMMSS
    endif


   i_row=0
!!!!! Gather the input date for this timestep as as string 
  !!!  concatenate the two portion    
   nrows=n
   !#####  Read the file, looping through the rows and compare the date in 
   !       each row to the date GEODYN is requesting
   do i_row = 1, nrows    
      !       If the date of the row matches the requested date, load the 
      !       remaining values in that row into the save arrays
      date = datearray(i_row)

      if (i_YYMMDDHHMMSS == date ) then
          if(ICNT.eq.1) WRITE(6,*) "   found date ", i_YYMMDDHHMMSS
          !
          dates = datearray(i_row)                       
          rho_in  = rho_inarray(i_row)
          !!! Exit if you got the right date
           exit       
      endif
   end do   !!!! do loop through the file

! ----------------------------------------------------------------------------

    RHO = rho_in
    DRHODZ = 0

   99 RETURN
      END