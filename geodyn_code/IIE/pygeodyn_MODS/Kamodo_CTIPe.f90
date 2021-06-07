!$KAMODO-ctipe
      SUBROUTINE Kamodo_CTIPe(MJDSEC,ALTM,XLATD,XLOND,          &
             &         RHO,DRHODZ)
!********1*********2*********3*********4*********5*********6*********7**

! Kamodo_CTIPe             Written by Zach Waldron           June, 04, 2021
!
! FUNCTION:
!
!  THIS ROUTINE SERVES AS A WRAPPER FOR CALLING THE PYTHON-BASED
!    ATMOSPHERIC MODEL API
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!   MJDSEC   I    S    TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME 
!                          ! will need to correct difference in ref times    
!   ALTM     I    ?    Height of satellite above elipsoid (M)  
!   XLATD    I    ?    Lat of Space station (degrees) 
!   XLOND    I    ?    Lon of Space station (degrees)
!
!   RHO      O    S    DENSITY OF ATMOSPHERE AS GIVEN BY MSIS MODEL
!                      (KG/M3)
!   DRHODZ   O    S    PARTIAL DERIVATIVE OF RHO WITH HEIGHT.
!                      VALID ONLY IF IDRV = 1.
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!      COMMON/PARMB/GSURF,RE
!
      DIMENSION XKP(*)
      DIMENSION AP(7),SWI(25),DEN(9),TEMP(2)
!
      DATA RGAS/831.4D0/,ZL/120.D0/
      DATA ICNT/0/,SWI/25*1.0D0/,RADDEG/57.29577951D0/,                 &
     &     DEGHR/0.066666667D0/

      INTEGER(4) :: IYYDDD
      INTEGER(4) :: mass
          
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      ICNT = ICNT+1
      IERR = 0
      DRHODZ = 0.0D0
!
      if(ICNT.eq.1)then
          WRITE(6,*) 'CHECK -- Using MODS_msis00_f90 / MSIS.f90'
      endif

!
!
!
!**************************
!
!  CALCULATE YYDDD QUANTITY.  JAN 1, 1978 = 78001 ETC.
      IYYDDD = IYR*1000 + INT(DAY) + 1
!  **CHECK TO MAKE SURE SEC IS IN UTC.
      UTSEC = ( DAY - INT(DAY) )*86400.D0

!
!  CONVERT PHI, XLAMB TO GLAT, GLON (DEGREES)
      GLAT = PHI*RADDEG
      GLON = XLAMB*RADDEG
!
!  **USE KPTRAN ROUTINE TO: 1) CONVERT KP TO AP,
!                         2) AVERAGE AP ACCORDING TO SPECIFICATIONS
!                            IN GTS5, AS DETERMINED BY I324.
!
!  *** DIAGNOSTIC PRINTOUT FOLLOWS.  COMMENT OUT WHEN NOT NEEDED.
!CC   DAYY = DAY + 1
!CC   PRINT 889,DAYY
  889 FORMAT(2X,'==>DAY = ',F15.8)
!
      CALL KPTRAN(XKP,INDEX,I324,IKPAP,AP,IERR)
!
      IF(IERR .GT. 0) THEN
        WRITE(6,703)
        STOP 16
  703   FORMAT(/,2X,'** FATAL ERROR IN KPTRAN.  EXIT MSIS.')
      ENDIF
!
!
!   AS OF NOW THE MODELS HAVE TO BE CALLED WITH THE COMMAND LINE....     
!      
!
!!!!!!!!!###################################################################################
!                               CALL MSIS
!
      mass=48
      CALL gtd7d(IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,      &
     &          mass,DEN,TEMP)
!
!!!!!!!!!###################################################################################
!
!
!  gtd7d OUTPUTS DENSITY IN G/CC.  CONVERT TO KG/M3.
!      RHO = DEN(6)*1000.D0


!------------------------------------------------------------------------
!
!
!
!
!
   99 RETURN
      END
