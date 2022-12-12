!$KAMODO
      SUBROUTINE KamodoModels_cl(MJDSEC,FSEC,ALTM,XLATD,XLOND,        &
        &     PHI,XLAMB,RHO,DRHODZ, DAY,IYR, kamodo_model, model_path,&
        &            orbitcloud_file )
!             
!********1*********2*********3*********4*********5*********6*********7**
! KamodoModels_cl ---------- June, 29, 2021 ----- Written by Zach Waldron
!              (command line)
!
!
! FUNCTION:
!      THIS ROUTINE SERVES AS A WRAPPER FOR CALLING THE PYTHON-BASED
!      ATMOSPHERIC MODEL API THAT IS KAMODO.
!    
!      The method of calling python here is thru the command line.
!
!
!
!
!   I/O PARAMETERS:
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!   FSEC     I    S    FRACTIONAL REMAINING SECONDS
!   ALTM     I         Height of satellite above elipsoid (M)  
!   XLATD    I         Lat of Space station (degrees) 
!   XLOND    I         Lon of Space station (degrees)
!   PHI      I    S    GEODETIC LATITUDE IN RADIANS.
!   XLAMB    I    S    "     LONGITUDE "   "
!   RHO      O    S    DENSITY OF ATMOSPHERE AS GIVEN BY MSIS MODEL
!                      (KG/M3)
!   DRHODZ   O    S    PARTIAL DERIVATIVE OF RHO WITH HEIGHT.
!                      VALID ONLY IF IDRV = 1.
!
!
! COMMENTS:
! Data input for calling Kamodo:
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
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/STARTT/ESSTRT,FSSTRT

      DATA ICNT/0/,RADDEG/57.29577951D0/
      DATA jd_unix/-2440587.5D0/
      
      CHARACTER(len=*) kamodo_model
      CHARACTER(len = 1)   s 
      CHARACTER(len = 300) files_path1
      CHARACTER(len = 300) files_path2
      CHARACTER(len = 300) files_path
      CHARACTER(len = 300) kpath1
      CHARACTER(len = 300) kpath2
      CHARACTER(len = 300) kpath3
      CHARACTER(len = 300) kamodo_path 
      CHARACTER(len = 300) A
      CHARACTER(len = 300) B
      CHARACTER(len = 300) BC
      CHARACTER(len = 300) C
      CHARACTER(len = 300) D
      CHARACTER(len = 6) F1
      CHARACTER(len = 6) F2
      CHARACTER(len = 12) F
      CHARACTER(len = 20) G
      CHARACTER(len = 20) H
      CHARACTER(len = 20) II
      CHARACTER(len = 12) J
!      CHARACTER(len = 4) K

      CHARACTER(len=300) command2
      CHARACTER(len=300) command1
      CHARACTER(len=600) command 
!      CHARACTER(len=200) message
      CHARACTER(200) :: message=''

      CHARACTER  model_path

      CHARACTER(len = 12) YYMMDDHHMMSS

      CHARACTER(len = 300) global_path
      !CHARACTER(len = 300) model_path
      CHARACTER(len = 300) path_results

      integer :: exit_val
      integer :: cmd_s
      
      integer :: i_read
      DOUBLE PRECISION, dimension(2) :: results_file

      CHARACTER(len = 15)  var
      !DOUBLE PRECISION ::  rho1
      !DOUBLE PRECISION ::  drhodz1
      CHARACTER(len = 15)  unit
      DOUBLE PRECISION :: RHO_file
      DOUBLE PRECISION :: DRHODZ_file
      DOUBLE PRECISION :: RHO
      DOUBLE PRECISION :: DRHODZ
      real :: start, finish
      INTEGER :: count, count_rate, count_max
      character(8)  :: date
      character(10) :: time
      character(5)  :: zone
      integer,dimension(8) :: values
      integer :: start_t_ms
      integer :: end_t_ms
      real :: time_elapsed_ms
      
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
          WRITE(6,*) 'CHECK-- KamodoModels.f90: using Kamodo '
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
!!      
!!!!! Put Sat_height in kilometers (km):
!      ALTKM = 300000.000D0/1000.D0
       ALTKM = ALTM/1000.D0
!!
!!!!!  Get the variables containing YYMMDD and HHMMSS
      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)     
!!
!!
!!*********************************************************************
!!    PREPARE THE PYTHON COMMAND TO BE SENT TO THE COMMAND LINE:
!!      Prepare the command line entry:
!!        >>> python 
!!            program_path/program_name.py 
!!            model_data_path 
!!            file_dir 
!!            variable_list  
!!            z_dependence 
!!            dz                 # 1 request partial derivative of variable 0 is no calculation
!!            sat_time_YYMMDD 
!!            sat_time_HHMMSS 
!!            sat_height         # altitude in km
!!            sat_lat            # range of -90 to +90 degrees
!!            sat_lon            # range of 0 to 360 degrees
!!            model_dt           # model_dt: half of the time resolution of the model data
!!            high_res           # precision of plevel interp in meters
!!      
!!  Because of the 72 character limit in fortran, this step 
!!  requires some string manipulation coding kungfu
!!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                                                                       &


!!    BUILD STRING FOR DATAPATH     
      files_path1 = "/data/data_geodyn/atmos_models_data/"
    !!    ADD A CONDITIONAL TO POINT TO THE CORRECT DATAFILES.
    !!    This should be updated to be a read-in value from 
    !!    the geodyn_options.txt file
      if(kamodo_model=='CTIPe')then
          files_path2 = "ctipe/2018_Dec_1_15/"
      endif
      if(kamodo_model=='TIEGCM')then
          files_path2 = "tiegcm/2018/Lutz_Rastaetter_072319_IT_1/"
      endif
      !files_path = trim(files_path1)//trim(files_path2)
      files_path = model_path
!!
!!   # TO DO: This needs to be made more modular to be an input from pygeodyn
!!    BUILD STRING FOR PROGRAM_PATH     
      kpath1 = "/data/geodyn_proj/interface_kamodo_geodyn/"
      kpath2 = "Kamodo/kamodo/flythrough/"
      kpath3 = "SingleSatelliteFlythrough.py "
      kamodo_path  = trim(kpath1)//trim(kpath2)//trim(kpath3)
!!
!!    BUILD FIRST HALF OF COMMAND LINE CALL  
      s = ' '
      A = "python"  ! python
!      A = "python -m cProfile -o profileresults.txt"  ! python
      B = kamodo_path
      BC = kamodo_model
      C = files_path
      D = "['rho'] ['1']"
!
!!    BUILD SECOND HALF OF COMMAND LINE CALL  
!     Save the floats as strings using Fortran's internal file system
!     THESE ARE INTERNAL FILES NOT WRITES TO THE UNIT 6 iieout
      write(F1,'(I0.6)')     IYMD
      write(F2,'(I0.6)')     IHMS
      F =  F1//F2  !Combine into a single string: YYMMDDHHMMSS
      write(G,'(F20.14)')   GLON
      write(H,'(F20.14)')   GLAT
      write(II,'(F20.13)')  ALTKM
      J = 'SPH sph 20.0' 
!
!!    TRIM AND COMBINE THE TWO HALFS TO MAKE THE COMMAND LINE CALL.
      command1=trim(A)//s//trim(B)//s//trim(BC)//s//trim(C)//s//trim(D)
      !!  No trimming needed in this one. Vars are set to correct length.
      command2 = s//F//s//G//s//H//s//II//s//J
      command = trim(command1)//command2 

!!*********************************************************************
!!    SEND THE CALL TO THE COMMAND LINE
!!        This command calls the Kamodo python script that indexes the 
!!        chosen model at the time and place of the satellite.
!!        As of now, the Kamodo model interpolates to location of the 
!!        satellite up until the top boundary of the model.
!!- - Need to Check the Following - - - - - - - - - - - - - - - - - - -
!!       - Check the time interpolation?
!!       - Check that the pressure level-> geometric altitude is accurate
!!       - Add a top boundary extrapolation
!!       - Add a DRHODZ calculation
!                                                                       &
    call date_and_time(date,time,zone,values)
    call date_and_time(DATE=date,ZONE=zone)
    call date_and_time(TIME=time)
    call date_and_time(VALUES=values)
    !write(6,*) '  '
    !print '(8i5)', values
    start_t_ms = values(7)*1000+values(8)
    
  call EXECUTE_COMMAND_LINE(command, WAIT=.TRUE.,exitstat=exit_val,     &
                            & CMDSTAT = cmd_s,   CMDMSG = message )



!!*********************************************************************
!!    READ THE OUPUTFILE FROM KAMODO CODE THAT CONTAINS DENSITY
!!       - Read in this file to grab the density.
      
     if(exit_val.eq.0)then

       !global_path = "/data/data_geodyn/atmos_models_data/"
       !model_path=trim(files_path2)//trim(kamodo_model)//'_results.txt'
       !path_results = trim(global_path)//trim(model_path)
       path_results = trim(model_path)//'_results.txt'

        open (122,  file=path_results, status='old')                                               
        !read(122,"(A15,1X, 2(D25.20,1X), A15)") var,rho1,drhodz1,unit
        !read(122,"(A15,1X, D25.20,1X, A15)") var,rho1, unit

        do i_read=1,2
             read(122,"(D25.20)") results_file(i_read)
        end do
        close (122)     
!                                                                        &

       RHO_file    = results_file(1)
       DRHODZ_file = results_file(2)
                
       RHO    =  RHO_file  * 1000.D0
       DRHODZ =  DRHODZ_file
      ! WRITE(6,*) ' RHO     ', RHO 
      ! WRITE(6,*) ' DRHODZ  ', DRHODZ 

       
!                                                                        &
         if(RHO.GT.1.0E-5)then
           WRITE(6,*) ' ********************************************'
           WRITE(6,*) ' Density is way too big ', RHO 
           WRITE(6,*) ' Check the units on density or the Kamodo code'   
           WRITE(6,*) ' STOPPING PROGRAM IN KamodoModels.f90'   
           WRITE(6,*) ' ********************************************'
           STOP 16
         endif
!                                                                       &
     else
        WRITE(6,*) '   '
        WRITE(6,*) ' ---- '
        WRITE(6,*) 'exitstat  ',exit_val  
        WRITE(6,*) 'cmd_s     ',cmd_s  
        WRITE(6,*) 'CMDMSG    ',message  

         
         
         WRITE(6,*) '  ***********************************************'
         WRITE(6,*) '  Bad command line exit status: ',  exit_val 
         WRITE(6,*) '  Did not run Kamodo cmd thru cmdline '   
         WRITE(6,*) '  STOPPING PROGRAM IN SUBROUTINE KamodoModels.f90'   
         write(6,*) ' ', command
         write(6,*) ' ' 
         WRITE(6,*) '------'  
         WRITE(6,*) 'python:                ', trim(A)
         WRITE(6,*) 'kamodo_path:           ', trim(B)
         WRITE(6,*) 'kamodo_model:          ', trim(BC)
         WRITE(6,*) 'files_path:            ', trim(C)
         WRITE(6,*) "['var'] ['dz']'", trim(D)
         WRITE(6,*) '------'  
         WRITE(6,*) 'IYMD:                  ', F1
         WRITE(6,*) 'IHMS:                  ', F2
         WRITE(6,*) 'YYMMDDHHMMSS           ', F
         WRITE(6,*) 'GLON:                  ', G
         WRITE(6,*) 'GLAT:                  ', H
         WRITE(6,*) 'ALTKM:                 ', II
         WRITE(6,*) 'dt:                    ', J
!         WRITE(6,*) 'alt_resolution:        ', K
         WRITE(6,*) '------'  
         WRITE(6,*) '  ***********************************************'
         STOP 16

     endif

!                                                                       &

   99 RETURN
      END
