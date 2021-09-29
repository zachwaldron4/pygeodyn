!$KAMODO-ctipe
      SUBROUTINE KamodoModels_oldold(MJDSEC,FSEC,ALTM,XLATD,XLOND,        &
             &         PHI,XLAMB,RHO,DRHODZ, DAY,IYR, kamodo_model)
!             
!********1*********2*********3*********4*********5*********6*********7**
! KamodoModels ---------- June, 29, 2021 ----- Written by Zach Waldron
!
!
!
! FUNCTION:
!      THIS ROUTINE SERVES AS A WRAPPER FOR CALLING THE PYTHON-BASED
!      ATMOSPHERIC MODEL API THAT IS KAMODO.
!    
!
!
! I/O PARAMETERS:
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!   FSEC     I    S    FRACTIONAL REMAINING SECONDS
!   ALTM     I    ?    Height of satellite above elipsoid (M)  
!   XLATD    I    ?    Lat of Space station (degrees) 
!   XLOND    I    ?    Lon of Space station (degrees)
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
!                        Jan 1 1970
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
      CHARACTER(len = 20) G
      CHARACTER(len = 20) H
      CHARACTER(len = 20) II
      CHARACTER(len = 4) J
      CHARACTER(len = 4) K

      CHARACTER(len=300) command2
      CHARACTER(len=300) command1
      CHARACTER(len=600) command 
      
      CHARACTER(len = 12) YYMMDDHHMMSS

      CHARACTER(len = 300) global_path
      CHARACTER(len = 300) model_path
      CHARACTER(len = 300) path_results

      !real, dimension(2) :: read_data
      integer :: exit_val
      !integer :: i_file
      
      CHARACTER(len = 15)  var
      DOUBLE PRECISION ::  rho1
      DOUBLE PRECISION ::  drhodz1
      CHARACTER(len = 15)  unit
      DOUBLE PRECISION :: RHO
      DOUBLE PRECISION :: DRHODZ
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!                                                                       &
!     

      ICNT = ICNT+1
      IERR = 0
      DRHODZ = 0.0D0
!
!!!!!   Print to UNIT6 so the user can confirm the correct subroutine is being used.
      if(ICNT.eq.1)then
          WRITE(6,*) 'CHECK-- KamodoModels.f90: using Kamodo '
          WRITE(6,*) 'kamodo_model:             ', kamodo_model

      endif
!      
!                                                                       &
!************************************************************************
!
!!!!! Convert PHI and XLAMB TO GLAT, GLON (deg):
      GLAT = PHI*RADDEG
      GLON = XLAMB*RADDEG
!
!      
!!!!! Put Sat_height in kilometers (km):
      ALTKM = 300000.000D0/1000.D0

!                                          JAN 1, 1978 = 78001 ETC.
!!!!! Calculate satellite timestamp since 1970 (around 1543941674):
!  Instead iof :
      !write(6,*) '---------------------------------'
      !WRITE(6,*) 'before mjdymd IYMD: ', IYMD
      !WRITE(6,*) 'before mjdymd IHMS: ', IHMS

      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      !WRITE(6,*) 'after mjdymd IYMD:  ', IYMD
      !WRITE(6,*) 'after mjdymd IHMS:  ', IHMS
      !write(6,*) '---------------------------------'
      MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)     
!      unix = (MJD - 40587.5)*86400 + UTSEC


!!!!!!!!!!   Extract data from the model output.
!!!!!!!!!!   We can do this using the command line.
!!!!!!!!!    ############################################################
!                                                                       &
      files_path1 = "/data/data_geodyn/atmos_models_data/"
      files_path2 = "ctipe/2018_Dec_1_15/"
      files_path = trim(files_path1)//trim(files_path2)
!                                                                       &
      kpath1 = "/data/geodyn_proj/interface_kamodo_geodyn/"
      kpath2 = "Kamodo/kamodo/pygeodyn_connection/"
      kpath3 = "SingleSatelliteFlythrough.py "
      kamodo_path  = trim(kpath1)//trim(kpath2)//trim(kpath3)
!                                                                       &     
      s = ' '
      A = "python"  ! python
!      A = "python -m cProfile -o profileresults.txt"  ! python
      B = kamodo_path
      BC = kamodo_model
      C = files_path
      D = "['rho'] ['ilev'] ['0']"
!                                                                       &
!     Save the floats as strings using Fortran's internal file nonsense
!     THESE ARE INTERNAL FILES NOT WRITES TO THE UNIT 6 iieout
      write(F1,'(I06)')     IYMD
      write(F2,'(I06)')     IHMS
      write(G,'(F20.16)')  ALTKM
      write(H,'(F20.14)')  GLAT
      write(II,'(F20.14)')  GLON
      J = '450.' 
      K = '20.0'
!                                                                       &
      command1=trim(A)//s//trim(B)//s//trim(BC)//s//trim(C)//s//trim(D)
      !!!  No trimming needed in this one. Vars all have correct length
      command2 = s//F1//s//F2//s//G//s//H//s//II//s//J//s//K
      command = trim(command1)//command2
      
!      WRITE(6,*) '------'  
!      WRITE(6,*) 'A:    ', trim(A)
!      WRITE(6,*) 'B:    ', trim(B)
!      WRITE(6,*) 'BC:   ', trim(BC)
!      WRITE(6,*) 'C:    ', trim(C)
!      WRITE(6,*) 'D:    ', trim(D)
!      WRITE(6,*) '------'  
!      WRITE(6,*) 'F1:   ', F1
!      WRITE(6,*) 'F2:   ', F2
!      WRITE(6,*) 'G:    ', G
!      WRITE(6,*) 'H:    ', H
!      WRITE(6,*) 'II:   ', II
!      WRITE(6,*) 'J:    ', J
!      WRITE(6,*) 'K:    ', K
!      WRITE(6,*) '------'  


!      WRITE(6,*) 'command1:   ', command1
!      WRITE(6,*) 'command2:   ', command2
!      WRITE(6,*) 'command:    ', command
!                                                                       &
     call EXECUTE_COMMAND_LINE(command, exitstat=exit_val)
!                                                                       &
!!!!!!!!!    ############################################################
!!!  The above block saves the Density and DRHODZ to a file named results.txt
!    Read in this file to grab the density.
      
     !WRITE(6,*) 'Back in Fortran- KamodoModels.f90 '
      
     if(exit_val.eq.0)then
!                                                                       &
      !  WRITE(6,*) 'Code ran with status 0'
!      WRITE(6,*) '  ***********************************************'
!      write(6,*) 'ICNT:                  ', ICNT
!      write(6,*) '-------------------------------------------------'
!      WRITE(6,*) 'IYMD:                  ', F1
!      WRITE(6,*) 'IHMS:                  ', F2
!      WRITE(6,*) 'ALTKM:                 ', G
!      WRITE(6,*) 'GLAT:                  ', H
!      WRITE(6,*) 'GLON:                  ', II
!      WRITE(6,*) 'dt:                    ', J
!      WRITE(6,*) 'alt_resolution:        ', K
!      WRITE(6,*) '  ***********************************************'
!                                                                       &
      WRITE(6,"(I6,1X,2(A6,1X),3(A20,1X),2(A6,1X))") ICNT,F1,F2,      &
       &                                            G,H,II,J,K


        global_path = "/data/data_geodyn/atmos_models_data/"
       model_path=trim(files_path2)//trim(kamodo_model)//'_results.txt'
        path_results = trim(global_path)//trim(model_path)
       ! WRITE(6,*) 'path_results:    ', path_results

        open (122,  file=path_results, status='old')                                               
!                                                                       &
        !read(122,"(A15,1X, 2(D25.20,1X), A15)") var,rho1,drhodz1,unit
!        read(122,"(A15,1X, D25.20,1X, A15)") var,rho1, unit
        read(122,"(D25.20)") rho1
        close (122)     
!                                                                       &

         !write(RHO,   '(F20.16)')  read_data(2)
         !write(DRHODZ,'(F20.16)')  read_data(4)
         !WRITE(6,*) 'var:      ', var
         !WRITE(6,*) 'rho1:     ', rho1
         !WRITE(6,*) 'drhodz1:  ', drhodz1
         !WRITE(6,*) 'unit:     ', unit

         RHO    = rho1
         DRHODZ = 0.0  !drhodz1 
         !WRITE(6,*) '        '
         !WRITE(6,*) 'DEN:    ', RHO
         !WRITE(6,*) 'DRHODZ: ', DRHODZ
         !WRITE(6,*) '        '
          
         if(RHO.GT.1.0E-5)then
           WRITE(6,*) ' ********************************************'
           WRITE(6,*) ' Density is definitely not correct ', RHO 
           WRITE(6,*) ' Check the units on density'   
           WRITE(6,*) ' STOPPING PROGRAM IN KamodoModels.f90'   
           WRITE(6,*) ' ********************************************'
           STOP 16
         endif
!                                                                       &
     else
         WRITE(6,*) '  ***********************************************'
         WRITE(6,*) '  Bad command line exit status: ',  exit_val 
         WRITE(6,*) '  Did not run CTIPe Kamodo cmd thru cmdline '   
         WRITE(6,*) '  STOPPING PROGRAM IN SUBROUTINE KamodoModels.f90'   
         write(6,*) ' ', command
         write(6,*) ' ' 
         WRITE(6,*) '------'  
         WRITE(6,*) 'python:                ', trim(A)
         WRITE(6,*) 'kamodo_path:           ', trim(B)
         WRITE(6,*) 'kamodo_model:          ', trim(BC)
         WRITE(6,*) 'files_path:            ', trim(C)
         WRITE(6,*) "['rho'] ['ilev'] ['1']'", trim(D)
         WRITE(6,*) '------'  
         WRITE(6,*) 'IYMD:                  ', F1
         WRITE(6,*) 'IHMS:                  ', F2
         WRITE(6,*) 'ALTKM:                 ', G
         WRITE(6,*) 'GLAT:                  ', H
         WRITE(6,*) 'GLON:                  ', II
         WRITE(6,*) 'dt:                    ', J
         WRITE(6,*) 'alt_resolution:        ', K
         WRITE(6,*) '------'  
         WRITE(6,*) '  ***********************************************'
         STOP 16

     endif



!
!
!!!!!!!!!################################################################
!
!
!
   99 RETURN
      END
