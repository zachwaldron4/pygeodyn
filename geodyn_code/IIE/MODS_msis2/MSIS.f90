!$MSIS
      SUBROUTINE MSIS(DAY,IYR,ALTM,PHI,XLAMB,COSHL,SINHL,FLUX,AVGFLX,   &
     &                XKP,INDEX,IKPAP,I324,IDRV,RHO,DRHODZ,IERR)
!********1*********2*********3*********4*********5*********6*********7**
! MSIS             00/00/00            0000.0    PGMR - J. RIDGEWAY
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
!      use msis_init
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
!
!IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,      &
!     &          mass,DEN,TEMP)
!      REAL(4)    :: UTSEC
!      REAL(4)    :: ALTKM
!      REAL(4)    :: GLAT
!      REAL(4)    :: GLONG
!      REAL(4)    :: STLOC
!      REAL(4)    :: AVGFLX
!      REAL(4)    :: FLUX
!      REAL(4)    :: AP
!      REAL(4)    :: DEN
!      REAL(4)    :: TEMP
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
         &   parmpath='/data/geodyn_proj/geodyn_code/IIE/MODS_msis2/',  &
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
!
      CALL KPTRAN(XKP,INDEX,I324,IKPAP,AP,IERR)
!
!     write(6,*) 'msis: xkp after kptran ', xkp
!     write(6,*) 'msis: ap after kptran ', ap
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
!!!!!!!!!###################################################################################
!                               CALL MSIS
!
      mass=48
      CALL gtd8d(IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,     &
     &          mass,DEN,TEMP)
!
!!!!!!!!!###################################################################################
!
!
!  GTS5 OUTPUTS DENSITY IN G/CC.  CONVERT TO KG/M3.
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
!
      new_DRHODZ_ver=1   ! 0 is original;  1 is updated
!
!
      IF (new_DRHODZ_ver.EQ.1) THEN
          IF(IDRV .NE. 0) THEN

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
              DRHODZ=TERM1*(TERM_species+TERM_anomO)*TERMnorm1*TERMnorm2
!
!       DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.
            ENDIF
      ENDIF
!------------------------------------------------------------------------
! ORIGINAL CALCULATION:

      IF (new_DRHODZ_ver.EQ.0) THEN
          IF(IDRV .NE. 0) THEN
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
       if(ICNT.eq.1)then
            write(98,'(a9,a6,a10,a11,a13,2a10,a9,a7,a7,7a6)') &
                   'IYYDDD','IYR','DAY','UTSEC','ALTKM','GLAT','GLON',  &
                    & 'STLOC','AVGFLX','FLUX','AP'
!
            write(101,'(9a15,2a15,2a15)') &
               'DEN','TEMP','RHO','DRHODZ'
       endif
       WRITE(98,7001) IYYDDD,IYR,DAY,UTSEC,ALTKM,GLAT,GLON,STLOC,      &
            & AVGFLX,FLUX,AP
 7001   FORMAT(I8,1X,I6,1X,F10.4,1X, F10.4,1X, F12.3,1X, 2(F9.3,1X),      &
            &             F8.5,1X, F6.2,1X, F6.2,1X, 7(F6.2,1X) )
!
!
       WRITE(101,7002)  DEN,TEMP, RHO, DRHODZ
 7002   FORMAT(9(E15.8,1X),2(E15.8,1X),E15.8,1X,E15.8,1X )
!
       WRITE(103,7003)  SWI(9)
 7003   FORMAT((F15.8,1X) )

!
!
!
!
!
   99 RETURN
      END    
