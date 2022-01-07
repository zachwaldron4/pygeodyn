!$KAMODO
      SUBROUTINE KamodoModels_oc_ver_readsectionofcsv(MJDSEC,FSEC,ALTM,XLATD,XLOND,        &
        &     PHI,XLAMB,RHO,DRHODZ, DAY,IYR, kamodo_model, model_path,&
        &            orbitcloud_file )
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
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    Time in integral seconds from geodyn ref time.
!   FSEC     I    S    Fractional remaining seconds
!   ALTM     I         Height of satellite above elipsoid (M)  
!   XLATD    I         Lat of Space station (degrees) 
!   XLOND    I         Lon of Space station (degrees)
!   PHI      I    S    Geodetic latitude  in radians
!   XLAMB    I    S    Geodetic longitude in radians
!   RHO      O    S    Density of atmosphere as given by the REQUESTED model
!                           (KG/M3)
!   DRHODZ   O    S    Partial derivative of RHO wrt HEIGHT
!                           valid only if IDRV = 1.
!
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

!!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN

      DATA ICNT/0/,RADDEG/57.29577951D0/
      
      CHARACTER(len=*) kamodo_model
      CHARACTER(len = 1)   s 
      INTEGER(8) :: IYYDDD

      CHARACTER(len = 12) i_YYMMDDHHMMSS
      CHARACTER(len = 6) i_YYMMDD
      CHARACTER(len = 6) i_HHMMSS
      CHARACTER(len = 300) global_path
      CHARACTER(len = 300) model_path1
      CHARACTER(len = 300) model_path2
      CHARACTER(len = 300) orbit_cloud_path
      
      CHARACTER(len = 200) model_path
      CHARACTER(len = 200) orbitcloud_file
      integer :: i_read
      integer :: ii_read
      integer :: date_index
      integer :: inum
      CHARACTER(len = 70) :: results_file
      
      CHARACTER(len = 12) :: date

      CHARACTER(len=70),dimension(9) :: char_ar
      
      CHARACTER(len = 12),dimension(9) :: dates
      REAL,dimension(9) :: lons
      REAL,dimension(9) :: lats
      REAL,dimension(9) :: alts
      DOUBLE PRECISION,dimension(9) :: rhos
!     INTEGER,dimension(9) :: corners
!
      REAL :: lon
      REAL :: lat
      REAL :: alt
      DOUBLE PRECISION :: rho_in
      !INTEGER :: corner

      REAL ::  min_lon
      REAL ::  max_lon
      REAL ::  min_lat
      REAL ::  max_lat
      REAL ::  min_alt
      REAL ::  max_alt

       
        
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
!      WRITE(6,*) '******************'
!      WRITE(6,*) 'MJDSEC:  ', MJDSEC
!      WRITE(6,*) 'FSEC:    ', FSEC
!      WRITE(6,*) 'IJDSEC:  ', IJDSEC
!      WRITE(6,*) 'IYMD:    ', IYMD
!      WRITE(6,*) 'IHMS:    ', IHMS
!      WRITE(6,*) 'TMGDN2:  ', TMGDN2
!      WRITE(6,*) 'IYMD:    ', IYMD
!      WRITE(6,*) 'IHMS:    ', IHMS
!      WRITE(6,*) 'YYMMDDHHMMSS:  ', i_YYMMDDHHMMSS

!!!!! Gather the input date for this timestep as as string 
  !!!  concatenate the two portion
!      write(i_LON,'(F20.14)')   GLON
!      write(i_LAT,'(F20.14)')   GLAT
!      write(i_ALTKM,'(F20.13)') ALTKM
!                                                                       &
!!  GET THE ORBIT DATA
       orbit_cloud_path = trim(orbitcloud_file)
       
       
   nrows=10000000000
   
   !open (123,  file=trim(orbit_cloud_path), status='old', recl=1)                                   
   open (123,  file=trim(orbit_cloud_path), status='old')!!!! CHANGE HERE                                   
     inum = (ICNT + (ICNT-1)*9) - 1
     do 50 ilines = 1, inum    !!!! CHANGE HERE
         READ(123,*)
         
   50  continue
   
   
   !do i_read = 1, nrows    !!!! CHANGE HERE
        
      !WRITE(6,*) 'inum', inum

!                                                                       &
      read(UNIT=123,FMT="(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
           &      date, lon, lat, alt, rho_in  
           
        !read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &   !!!! CHANGE HERE
        !   &       date, lon, lat, alt, rho_in  
        

          if ( i_YYMMDDHHMMSS == date ) then
              !WRITE(6,*) 'i_read', i_read
              !WRITE(6,*) 'i_YYMMDDHHMMSS', i_YYMMDDHHMMSS
              !WRITE(6,*) 'date          ', date

            dates(1)   = date
            lons(1)    = lon
            lats(1)    = lat
            alts(1)    = alt
            rhos(1)    = rho_in
!                                                                       &
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(2),lons(2),lats(2),alts(2),rhos(2)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(3),lons(3),lats(3),alts(3),rhos(3)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(4),lons(4),lats(4),alts(4),rhos(4)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(5),lons(5),lats(5),alts(5),rhos(5)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(6),lons(6),lats(6),alts(6),rhos(6)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(7),lons(7),lats(7),alts(7),rhos(7)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(8),lons(8),lats(8),alts(8),rhos(8)   
         read(123,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,D17.8)") &
            &   dates(9),lons(9),lats(9),alts(9),rhos(9)   


    
         min_lon = MINVAL(lons)
         max_lon = MAXVAL(lons)
         min_lat = MINVAL(lats)
         max_lat = MAXVAL(lats)
         min_alt = MINVAL(alts)
         max_alt = MAXVAL(alts)

    
    if(GLON.gt.max_lon)then
        WRITE(6,*) 'OrbitCloudError-- lon ABOVE bounds:    ', GLON, max_lon
        STOP 16
    endif
    if(GLON.lt.min_lon)then
        WRITE(6,*) 'OrbitCloudError-- lon BELOW bounds:    ', GLON, min_lon, max_lon
        WRITE(6,*) ' i_YYMMDDHHMMSS              ', i_YYMMDDHHMMSS
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
    if(GLAT.gt.max_lat)then
        WRITE(6,*) 'OrbitCloudError-- lat ABOVE bounds:    ', GLAT, max_lat
        STOP 16
    endif
    if(GLAT.lt.min_lat)then
        WRITE(6,*) 'OrbitCloudError-- lat BELOW bounds:    ', GLAT, min_lat
        STOP 16
    endif
    if(ALTKM.gt.max_alt)then
        WRITE(6,*) 'OrbitCloudError-- alt ABOVE bounds:    ', ALTKM, max_alt
        STOP 16
    endif
    if(ALTKM.lt.min_alt)then
        WRITE(6,*) 'OrbitCloudError-- alt BELOW bounds:    ', ALTKM, min_alt
        STOP 16
    endif

    


        !########    EXIT if you got the right 
        !   exit  !!!! CHANGE HERE
        !########   
           
           endif

      ! end do   !!!! CHANGE HERE

      close (123) 
!           WRITE(6,"(A12,1x,F11.4,1x,F11.4,1x,F11.4,1x,E15.8,1x,I3)") &
!               &       date, lon, lat, alt, rho, corner   


!!!!! Once we have our saved values we interpolate the values of the cube 
!!!!!      to a single point (the one requested) by GLAT, GLON, ALTKM

    delta_deg = 2
    delta_m   = 1
    x  = GLON
    x0 = x - delta_deg
    x1 = x + delta_deg

    y  = GLAT
    y0 = y - delta_deg
    y1 = y + delta_deg

    z  = ALTKM
    z0 = z - delta_m
    z1 = z + delta_m
    
   C000 = rhos(6)   ![5]    ! # bottom, front, left
   C100 = rhos(8)  ![7]    ! # bottom, front, right
   C001 = rhos(2)  ![1]    ! # top,    front, left
   C101 = rhos(4)  ![3]    ! # top,    front, right
   C010 = rhos(7)  ![6]    ! # bottom, back,  left
   C110 = rhos(9)  ![8]    ! # bottom, back,  right
   C011 = rhos(3)  ![2]    ! # top,    back,  Left
   C111 = rhos(5)  ![4]    ! # top,    back,  right
    !WRITE(6,*) 'C000:    ', C000



    !##===================================================================
    !##        Manually INTERPOLATE-- Trilinear Interpolation Calculation
    !##===================================================================

    !### On a periodic and cubic lattice, let xd, yd, zd be the differences 
    !### between each of x, y, z and the smaller coordinate related.
    !#
    !###  x0 indicates the lattice point below x 
    !###  x1 indicates the lattice point above x
    xd =  (x-x0)/(x1-x0)
    yd =  (y-y0)/(y1-y0)
    zd =  (z-z0)/(z1-z0)
    !# print(xd,yd,zd)

    !##### First we interpolate along x:   (push one of the X faces of cube towards the opposing face)
    !###   C000 uis the function value (x0, y0, z0)
    C00 = C000*(1-xd) + (C100*xd)
    C01 = C001*(1-xd) + (C101*xd)
    C10 = C010*(1-xd) + (C110*xd)
    C11 = C011*(1-xd) + (C111*xd)

    !#### Next we interpolate along y:  (pushing the values that are now in middle of cube towards the center Y)
    C0 = C00*(1-yd) + C10*yd
    C1 = C01*(1-yd) + C11*yd

    !#### Finally we interpolate along Z   (walk through the line that remains)
    C = C0*(1-zd) + C1*zd


    RHO = C*1000.D0
    DRHODZ = 0
    
   



!                                                                       &

   99 RETURN
      END
