!$KAMODO-ctipe
      SUBROUTINE Kamodo_CTIPe(MJDSEC,FSEC,ALTM,XLATD,XLOND,        &
             &         PHI,XLAMB,RHO,DRHODZ, DAY,IYR)
!             
!********1*********2*********3*********4*********5*********6*********7**
! Kamodo_CTIPe()---------- June, 04, 2021 ----- Written by Zach Waldron
!
!
!
! FUNCTION:
!  THIS ROUTINE SERVES AS A WRAPPER FOR CALLING THE PYTHON-BASED
!    ATMOSPHERIC MODEL API THAT IS KAMODO
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

      CHARACTER(len = 36) files_path1
      CHARACTER(len = 20) files_path2
      CHARACTER(len = 56) files_path
      CHARACTER(len = 42) kamodo_path1
      CHARACTER(len = 29) kamodo_path2
      CHARACTER(len = 27) kamodo_path3
      CHARACTER(len = 98) kamodo_path
      CHARACTER(len = 7)  AA
      CHARACTER(len = 98) BB
      CHARACTER(len = 56) CC
      CHARACTER(len = 10) DE
      CHARACTER(len = 6) FF1
      CHARACTER(len = 6) FF2
      CHARACTER(len = 20) GG
      CHARACTER(len = 20) HH
      CHARACTER(len = 20) II
      CHARACTER(len = 1)  sp
      CHARACTER(len = 247) command

      CHARACTER(len = 6) IYMD_str
      CHARACTER(len = 6) IHMS_str
      CHARACTER(len = 12) YYMMDDHHMMSS



      CHARACTER(len = 36) global_path
      CHARACTER(len = 31) model_path
      CHARACTER(len = 67) path_outputfile

      real, dimension(2) :: read_data
      integer :: exit_val
      integer :: i_file
      
!      data half/0.5d0/
!      data one/1.d0/
!      data seven/7.d0/
!      data ten/10.d0/
!      data twelve/12.d0/
!      data d14/14.d0/
!
!      data xjd0/2400000.5d0/
!      data d1537/1537.d0/
!      data d122/122.1d0/
!      data d36525/365.25d0/
!      data d30600/30.6001d0/
!      data d4715/4715.d0/
!      data ib/-15/
!      data d17209/1720996.5d0/
!      data  d3600/3600.d0/
!      data  d60/60.d0/


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
          WRITE(6,*) 'CHECK-- Kamodo_CTIPe.f90: using CTIPe Data '
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
      ALTKM = ALTM/1000.D0

!                                          JAN 1, 1978 = 78001 ETC.
!!!!! Calculate satellite timestamp since 1970 (around 1543941674):
!  Instead iof :
      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      
      MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)     
!      unix = (MJD - 40587.5)*86400 + UTSEC


!     WRITE(6,*) '   '
!     WRITE(6,*) "======== Time variables in Kamodo_CTIPE.f90 ========"


!     WRITE(6,*) "GEODYN2 Defaults"
!     WRITE(6,*) "---------------"
!     WRITE(6,*) 'TGTYMD:      ', TGTYMD
!     WRITE(6,*) 'TMGDN1:      ', TMGDN1
!     WRITE(6,*) 'TMGDN2:      ', TMGDN2
!     WRITE(6,*) 'REPDIF:      ', REPDIF
!     WRITE(6,*) 'XTMGN:       ', XTMGN
!     
!     WRITE(6,*) ''
!     WRITE(6,*) 'Variations of start time'     
!     WRITE(6,*) '------------------------'
!     WRITE(6,*) 'STARTT:      ', STARTT
!     WRITE(6,*) 'ESSTRT:      ', ESSTRT
!     WRITE(6,*) 'FSSTRT:      ', FSSTRT
!
!     WRITE(6,*) ''
!     WRITE(6,*) '------------------------'
!     WRITE(6,*) 'MJDSEC:      ', MJDSEC
!     WRITE(6,*) 'FSEC:        ', FSEC
!     WRITE(6,*) 'MJDSEC+FSEC: ', IJDSEC
     
!     WRITE(6,*) 'MJD:         ' , MJD
!     WRITE(6,*) 'IYMD:         ', IYMD
!     WRITE(6,*) 'IHMS:         ', IHMS
      
!     WRITE(6,*) ' '
!     WRITE(6,*) 'Calc. from IYR and DAY'
!     WRITE(6,*) '------------------------'
!     WRITE(6,*) 'IYR:       ', IYR
!     WRITE(6,*) 'DAY:       ', DAY
!     WRITE(6,*) 'IYYDDD:    ', IYYDDD
!     WRITE(6,*) 'UTSEC:     ', UTSEC
!      
!      WRITE(6,*) '   '
!      WRITE(6,*) "======== Intended Inputs to CTIPe ========"
!      WRITE(6,*) 'YYMMDD:  ', IYMD
!      WRITE(6,*) 'HHMMSS:  ', IHMS
!      WRITE(6,*) 'GLAT:    ', GLAT
!      WRITE(6,*) 'GLON:    ', GLON
      WRITE(6,*) 'ALTM:    ', ALTM
      WRITE(6,*) 'ALTKM:   ', ALTKM


!!!!!!!!!!   Extract data from the model output.
!!!!!!!!!!   We can do this using the command line.
!!!!!!!!!    ############################################################
!                                                                       &
      files_path1  = "/data/data_geodyn/atmos_models_data/"
      files_path2  = "ctipe/2018_Dec_1_15/"
      files_path  = files_path1//files_path2
!                                                                       &
      kamodo_path1 = "/data/geodyn_proj/interface_kamodo_geodyn/"
      kamodo_path2 = "Kamodo-master/kamodo/readers/"
      kamodo_path3 = "CTIPe_wrapper_fortrancmd.py"
      kamodo_path = kamodo_path1//kamodo_path2//kamodo_path3
!                                                                       &     
      AA = "python "
      BB = kamodo_path
      CC = files_path
      DE = " rho ilev "
!                                                                       &

!     Save the floats as strings using Fortran's internal file nonsense
!     THESE ARE INTERNAL FILES NOT WRITES TO THE UNIT 6
      write(FF1,'(I6)') IYMD
      write(FF2,'(I6)') IHMS

      write(GG,'(F20.16)') ALTKM
      write(HH,'(F20.14)') GLAT
      write(II,'(F20.14)') GLON
      
      
      WRITE(6,*) 'GG:         ', GG

!                                                                       &
      sp = " "
!                                                                       &
  command = AA//BB//sp//CC//DE//FF1//sp//FF2//sp//GG//sp//HH//sp//II//sp

!      WRITE(6,*) 'F, time:   ', F
!      WRITE(6,*) 'G, alt:    ', G
!      WRITE(6,*) 'H, lat:    ', H
!      WRITE(6,*) 'I, lon:    ', I
!      write(6,*) '', command

!                                                                       &
     call EXECUTE_COMMAND_LINE(command, exitstat=exit_val)
!                                                                       &
!!!!!!!!!    ############################################################
!!!  The above block saves the Density and DRHODZ to a file named results.txt
!    Read in this file to grab the density.
      
      
      
     if(exit_val.eq.0)then

         global_path = "/data/data_geodyn/atmos_models_data/"
         model_path = "ctipe/2018_Dec_1_15/results.txt"
         path_outputfile = global_path//model_path

         open (122,  file=path_outputfile, status='old')                                               

         do i_file=1,2
            read(122,*) read_data(i_file)
         end do
         close (122)     

         RHO = read_data(1)          
         DRHODZ = read_data(2)       
         
!         WRITE(6,*) 'DEN:    ', RHO
!         WRITE(6,*) 'DRHODZ: ', DRHODZ
!         WRITE(6,*) '   '
      
!                                                                       &
     else
         WRITE(6,*) '  ***********************************************'
         WRITE(6,*) '  Did not run CTIPe Kamodo cmd thru cmdline '   
         WRITE(6,*) '  STOPPING PROGRAM IN SUBROUTINE Kamodo_CTIPe.f90'   
         write(6,*) ' ' 
         write(6,*) ' ', command
         write(6,*) ' ' 

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
