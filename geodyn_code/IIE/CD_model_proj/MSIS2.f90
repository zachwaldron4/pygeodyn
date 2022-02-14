!$MSIS2
      SUBROUTINE MSIS2(DAY,IYR,ALTM,PHI,XLAMB,COSHL,SINHL,FLUX,AVGFLX,   &
     &                XKP,INDEX,IKPAP,I324,IDRV,RHO,DRHODZ,IERR,         &
     &                 optionsin,MJDSEC,FSEC, n_dens_temp )
!********1*********2*********3*********4*********5*********6*********7**
! MSIS2             00/00/00            0000.0    Modified from MSIS86 by Zach Waldron
!

    


! FUNCTION:
!
!  THIS ROUTINE SERVES AS A SHELL TO THE GTS5 DRAG ROUTINE (WRITTEN
!  BY AL HEDIN), FOR THE MOST RECENT MSIS ATMOSPHERIC DRAG MODEL.
!  IT IS DESIGNED TO ALLOW FOR A FAIRLY SIMPLE CALL FROM GEODYN,
!  HENCE IT TAKES GEODYN VALUES USED IN THE DRAG SUBROUTINE AND
!  CONVERTS THEM TO VALUES WHICH MAY BE USED IN GTS5.
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DAY      I    S    DAY OF YEAR, INCLUDING FRACTION OF DAY.
!                      (i.e. JAN. 1, 6AM = 0.25, Jan 2 6AM = 1.25 etc.)
!   IYR      I    S    YEAR PAST 1900 (I.E. 78 = 1978 ETC.)
!   ALTM     I    S    ALTITUDE IN METERS.
!   PHI      I    S    GEODETIC LATITUDE IN RADIANS.
!   XLAMB    I    S    "     LONGITUDE "   "
!   COSHL    I    S    COSINE OF LOCAL HOUR OF SUN.
!   SINHL    I    S    SINE   "    "     "  "   " .
!   FLUX     I         DAILY SOLAR FLUX F10.7 VALUE FOR PREVIOUS DAY.
!   AVGFLX   I         AVERAGE SOLAR FLUX F10.7 VALUE OVER 3 MONTHS.
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
!   IDRV     I    S    =0 MEANS DO NOT FIND DRHODZ, THE CHANGE IN
!                      DENSITY WITH HEIGHT.  =1 MEANS FIND DRHODZ.
!   RHO      O    S    DENSITY OF ATMOSPHERE AS GIVEN BY MSIS MODEL
!                      (KG/M3)
!   DRHODZ   O    S    PARTIAL DERIVATIVE OF RHO WITH HEIGHT.
!                      VALID ONLY IF IDRV = 1.
!   IERR     O    S    ERROR FLAG.  IF IERR .NE. 0, THEN A FATAL
!                      ERROR HAS OCCURED.
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
       use msis_init, only          : msisinit
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/PARMB/GSURF,RE
!
      DIMENSION XKP(*)
      DIMENSION AP(7),SWI(25),DEN(9),TEMP(2)
!
      DATA RGAS/831.4D0/,ZL/120.D0/
!      DATA ICNT/0/,SWI/25*1.0D0/,RADDEG/57.29577951D0/,                 &
!     &     DEGHR/0.066666667D0/
      DATA ICNT/0/,RADDEG/57.29577951D0/,                 &
     &     DEGHR/0.066666667D0/
      INTEGER(8) :: IYYDDD
      INTEGER(4) :: mass
      REAL(4)    :: SWI
      INTEGER(4) :: iiun
      CHARACTER(len = 6) i_YYMMDD
      CHARACTER(len = 6) i_HHMMSS
      CHARACTER(len = 12) i_YYMMDDHHMMSS
      integer, dimension(4) :: optionsin

      REAL(8), DIMENSION(11) ::  n_dens_temp

!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      ICNT = ICNT+1
      IERR = 0
      DRHODZ = 0.0D0
      SWI = 1
!
      if(ICNT.eq.1)then
          WRITE(6,*) 'CHECK -- Using MODS_msis2 / MSIS.f90'
      endif

      IF(ICNT .EQ. 1) THEN
!
!       CHECK I324 VALUE.
        IF(I324 .NE. 3 .AND. I324 .NE. 24) THEN
           IERR = 1
           WRITE(6,700) I324
           GO TO 99
  700      FORMAT(/3X,                                                  &
     &     '** FATAL ERROR IN MSIS. I324 UNAUTHORIZED VALUE: ',I9)
        ENDIF
!
!       CALL TSELEC86 PROGRAM (SELECTS VARIOUS VARIABLES TO MODEL IN MSIS)
!       ONLY ON FIRST CALL TO MSIS.
!       I324 = 24 MEANS USE DAILY FORMULA FOR KP; => SWI(9) FLAG = 1.
!       I324 = 3 MEANS SWI(9) = -1, WHICH TELLS HEDIN'S ROUTINES TO USE
!       3-HOURLY KP/AP VALUES
!
        IF(I324 .EQ. 3) SWI(9) = -1.0D0
!        CALL TSELEC86(SWI)
        iiun=117
!
        CALL msisinit(                                                  &
         & parmpath='/data/geodyn_proj/geodyn_code/IIE/CD_model_proj/', &
         & parmfile='msis20.parm',                                      &
!        & iun=iiun,                                                    &
         & switch_legacy=SWI  )
!        
!
      ENDIF
!
!* PRINTOUT FOR TESTING ************************8
!PRINT 805,DAY,INDEX,XKP(INDEX),ALTM,PHI,XLAMB
!  805 FORMAT(2X,'DAY = ',F20.13,1X,'INDEX=',I5,1X,'XKP=',E15.8, &
!             1X,'ALTM=',F12.3,1X,'PHI,XLAMB=',2(E15.8,1X))
!**************************
!
!  CALCULATE YYDDD QUANTITY.  JAN 1, 1978 = 78001 ETC.
      IYYDDD = IYR*1000 + INT(DAY) + 1
!  **CHECK TO MAKE SURE SEC IS IN UTC.
      UTSEC = ( DAY - INT(DAY) )*86400.D0
!  CONVERT ALTITUDE TO KM.
      ALTKM = ALTM/1000.D0
!  CHECK ALTKM > 85 KM.
      IF(ALTKM .LT. 85.0D0) THEN
        WRITE(6,710) ALTKM
        STOP 69
      ENDIF
  710 FORMAT(2X,'** FATAL ERROR IN MSIS:  ALTKM < 85 KM: ',F10.2)
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

      !print *,'msis2:  kp before kptran ', XKP(INDEX)
      !print *,'msis2:  ap before kptran ', AP
      CALL KPTRAN(XKP,INDEX,I324,IKPAP,AP,IERR)
      !print *,'msis2:  kp after  kptran ', XKP(INDEX)
      !print *,'msis2:  ap after  kptran ', AP
!
      IF(IERR .GT. 0) THEN
        WRITE(6,703)
        STOP 16
  703   FORMAT(/,2X,'** FATAL ERROR IN KPTRAN.  EXIT MSIS.')
      ENDIF
!
!
!  CALCULATE LOCAL APPARENT SOLAR TIME( HOURS ).
      STLOC = ATAN2(SINHL,COSHL)*RADDEG*DEGHR + 12.D0
!
!
!!*  PRINTOUT FOR TESTING.
!     PRINT 811,IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,MASS,  &
!                &   DEN, TEMP
! 811 FORMAT(1X, '    ',/,                                        &
!            1X, '     INPUT GTD7:  ',I8,1X, F10.4,/,                 &
!            1X, '     ALTkm: ',F12.3,/,                               &
!            1X, '     LAT,LON: ',2(F9.3,1X),/,                        &
!            1X, '     LOC TIME: ',F8.5,/,                             &
!            1X, '     FLUXAVG = ',F6.2,' VS. ','DAILY FLUX: ',F6.1,/, &
!            1X, '     AP = ',7F6.2,/,                                 &
!            1X, '     MASS = ',I8 ,/,                                 &
!            1X, '     DEN: = ',9(E15.8,1X),/,                         &
!            1X, '     TEMP = ',2(E15.8,1X),/,                         &
!            1X, '     ' )
     
!  CALL GTD7 TO CALCULATE DENSITY, TEMP, ETC.  OUT OF THIS ONLY
!  OUTPUT TOTAL DENSITY OF ATMOSPHERE FOR NOW (=DEN(6)), IN G/CC.
!
!      if(ICNT.eq.1)then
!          WRITE(6,*) 'CHECK -- MSIS.f90 BEFORE GT7D call'
!      
!

!===========PRINT to a file to see INPUTS ===============================
      

      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      MJD=(IJDSEC/86400)+INT(TMGDN2+0.1)
      
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
!      WRITE(6,*) 'i_YYMMDD:    ', i_YYMMDD
!      WRITE(6,*) 'i_HHMMSS:    ', i_HHMMSS

!      WRITE(6,*) 'YYMMDDHHMMSS:  ', i_YYMMDDHHMMSS
!                                                                       &

!###### Write the inputs to MSIS out to a file
        WRITE(98,7001) i_YYMMDD,i_HHMMSS,ALTKM,      &
            & GLAT,GLON
 7001   FORMAT(A6,2X,A6, 2X, F12.3,1X, 2(F9.3,1X) )
        
        WRITE(101,7002) i_YYMMDD,i_HHMMSS,      &
            & AVGFLX,FLUX,AP, XKP(INDEX)
 7002   FORMAT(A6,1X,A6, 1X,       &
            &  F6.2,1X, F6.2,1X,7(F6.2,1x), F6.2   )

!=======================================================================


!!!!!!!!!###################################################################################
!                               CALL MSIS
!
!      WRITE(6,*) 'ALTM:    ', ALTM
!      WRITE(6,*) 'ALTKM:   ', ALTKM


      mass=48
      CALL gtd8d(IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,     &
     &          mass,DEN,TEMP)
     
     
     !!!!! Save out the number densities to be used in the DRIA model
     n_dens_temp(1) = DEN(1)     ! D(1)     He number density (cm-3)
     n_dens_temp(2) = DEN(2)     ! D(2)     O number density (cm-3)
     n_dens_temp(3) = DEN(3)     ! D(3)     N2 number density (cm-3)
     n_dens_temp(4) = DEN(4)     ! D(4)     O2 number density (cm-3)
     n_dens_temp(5) = DEN(5)     ! D(5)     Ar number density (cm-3)
     n_dens_temp(6) = DEN(6)     ! D(6)     Total mass density (g/cm3)
     n_dens_temp(7) = DEN(7)     ! D(7)     H number density (cm-3)
     n_dens_temp(8) = DEN(8)     ! D(8)     N number density (cm-3)
     n_dens_temp(9) = DEN(9)     ! D(9)     Anomalous oxygen number density (cm-3)
     n_dens_temp(10) = TEMP(1)   ! T(1)  Exospheric temperature (K)
     n_dens_temp(11) = TEMP(2)   ! T(2)  Temperature at altitude (K)
 
!
!!!!!!!!!###################################################################################
!    IJDSEC=MJDSEC+FSEC
!    CALL MJDYMD(IJDSEC,IYMD,IHMS,4)

!    WRITE(6,"(I6,1X,2(I6,1X),3(F12.5,1X))") ICNT,IYMD,IHMS,      &
!       &                                            ALTKM,GLAT,GLON


!
!  gtd8d OUTPUTS DENSITY IN G/CC.  CONVERT TO KG/M3.
      RHO = DEN(6)*1000.D0
!
!-----------------------------------------------------
!  UPDATED
!  DRHO/DZ CALCULATION.
!
! Zach-- added O2 and AR to to species sum
! Zach-- Made the GSURF consistent with ZL
! Zach-- Added separate term for the AnomO scale height
!           to account for the anomolous temperature (4000 K)
! Zach-- Added a flag to for easier version call for comparison runs.
!
!
!      open (120, file="/data/geodyn_proj/pygeodyn/geodyn_options.txt", status='old')
!      do i=1,2
!          read(120,*) optionsin(i)
!      end do
!      close (120)     
      new_DRHODZ_ver=optionsin(1)   !the first index contains the drhodz option
!
      IF (new_DRHODZ_ver.EQ.1) THEN
          IF(IDRV .NE. 0) THEN
          
              if(ICNT.eq.1)then
                  write(6,*) 'CHECK: DRHODZ_vers = 1-- updated '
              endif
        
              GSURF_ZL = GSURF*(RE/(RE + ZL) )**2
!
              TERM1 = ((-1.66D-24*GSURF_ZL)/RGAS) 
              TERM_species = (  16.D0*DEN(1)  +  256.D0*DEN(2)          &
           &          + 784.D0*DEN(3)  +    1.D0*DEN(7)                 &
           &          + 196.D0*DEN(8)  + 1024.D0*DEN(4)                 &
           &          + 1600.D0*DEN(5) )*(1/TEMP(2))
              TERM_anomO = (256.D0*DEN(9))*(1/4000.D0)
              TERMnorm1 = 1.D0/(1.D0 + ZL/RE)**2
              TERMnorm2 = ((RE+ZL)/(RE+ALTKM))**2    
              DRHODZ =TERM1*(TERM_species+TERM_anomO)*TERMnorm1*TERMnorm2
!
!       DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.
            ENDIF
      ENDIF
!------------------------------------------------------------------------
! ORIGINAL CALCULATION:

      IF (new_DRHODZ_ver.EQ.0) THEN
          IF(IDRV .NE. 0) THEN
          
            if(ICNT.eq.1)then
                  write(6,*) 'CHECK: DRHODZ_vers = 0 -- original '
            endif
            
            TERM1 = -1.66D-24*(16.D0*DEN(1) + 256.D0*DEN(2)             &
         &            + 784.D0*DEN(3) + DEN(7) + 196.D0*DEN(8) )
            TERM2 = GSURF/(TEMP(2)*RGAS)/(1.D0 + ZL/RE)**2
            TERM3 = ((RE+ZL)/(RE+ALTKM))**2
            DRHODZ = TERM1*TERM2*TERM3
! ! ! ! DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.
          ENDIF
      ENDIF
!
!

!
!
!############## Print to FILES #######################
!
!
!*  PRINTOUT FOR TESTING.
!     PRINT 812,IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,MASS,  &
!                &   DEN, TEMP
! 812 FORMAT(1X, '    ',/,                                        &
!            1X, '     OUTPUT GTD7:  ',I8,1X, F10.4,/,                 &
!            1X, '     ALTkm: ',F12.3,/,                               &
!            1X, '     LAT,LON: ',2(F9.3,1X),/,                        &
!            1X, '     LOC TIME: ',F8.5,/,                             &
!            1X, '     FLUXAVG = ',F6.2,' VS. ','DAILY FLUX: ',F6.1,/, &
!            1X, '     AP = ',7F6.2,/,                                 &
!            1X, '     MASS = ',I8 ,/,                                 &
!            1X, '     DEN: = ',9(E15.8,1X),/,                         &
!            1X, '     TEMP = ',2(E15.8,1X),/,                         &
!            1X, '     ' )
!
!       if(ICNT.eq.1)then
!            write(98,'(a9,a6,a10,a11,a13,2a10,a9,a7,a7,7a6)') &
!                   'IYYDDD','IYR','DAY','UTSEC','ALTKM','GLAT','GLON',  &
!                    & 'STLOC','AVGFLX','FLUX','AP'
!
!            write(101,'(9a15,2a15,2a15)') &
!               'DEN','TEMP','RHO','DRHODZ'
!       endif
!
!       WRITE(101,7002)  DEN,TEMP, RHO, DRHODZ
! 7002   FORMAT(9(E15.8,1X),2(E15.8,1X),E15.8,1X,E15.8,1X )
!
!       WRITE(103,7003)  SWI(9)
! 7003   FORMAT((F15.8,1X) )
!
!
!
!
!
!
   99 RETURN
      END    
