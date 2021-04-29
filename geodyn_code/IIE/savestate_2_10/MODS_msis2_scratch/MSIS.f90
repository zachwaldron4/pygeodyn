!$MSIS2
      SUBROUTINE MSIS(DAY,IYR,ALTM,PHI,XLAMB,COSHL,SINHL,FLUX,AVGFLX,  &
     &                XKP,INDEX,IKPAP,I324,IDRV,RHO,DRHODZ,IERR)
!********1*********2*********3*********4*********5*********6*********7**
! MSIS2             00/00/00            0000.0    PGMR - J. RIDGEWAY
!
! FUNCTION:
!
!  THIS ROUTINE SERVES AS A SHELL TO THE GTS5 DRAG ROUTINE (WRITTEN
!  BY AL HEDIN), FOR THE MOST RECENT MSIS2 ATMOSPHERIC DRAG MODEL.
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
      use msis_init!, only          : msisinit
      
      !include 'msis2.0.parm'      

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)

      DIMENSION XKP(*)
!      DIMENSION AP(7),DEN(8),TEMP(2)
      DIMENSION AP(7),DEN(9),TEMP(2)
!
      DATA RGAS/831.4D0/,ZL/120.D0/
!      DATA ICNT/0/,SWI/25*1./,RADDEG/57.29577951D0/,                 &
!     &     DEGHR/0.066666667D0/ 
      DATA ICNT/0/,RADDEG/57.29577951D0/,                 &
     &     DEGHR/0.066666667D0/
      real(4)                     :: SWI(25)
!      character(128)              :: parmpath, parmfile
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      SWI=1.
      ICNT = ICNT+1
      IERR = 0
      DRHODZ = 0.0D0
!
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
!       CALL TSELEC PROGRAM (SELECTS VARIOUS VARIABLES TO MODEL IN MSIS)
!       ONLY ON FIRST CALL TO MSIS.
!       I324 = 24 MEANS USE DAILY FORMULA FOR KP; => SWI(9) FLAG = 1.
!       I324 = 3 MEANS SWI(9) = -1, WHICH TELLS HEDIN'S ROUTINES TO USE
!       3-HOURLY KP/AP VALUES
!
!        IF(I324 .EQ. 3) SWI(9) = -1.0D0
        IF(I324 .EQ. 3) SWI(9) = -1.
        !CALL TSELEC(SWI)

        CALL msisinit(parmpath='/',parmfile='msis2.parm',              &
          & switch_legacy=SWI  ) 

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
!*  PRINTOUT FOR TESTING.
!     PRINT 811,IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX
! 811 FORMAT(1X,'INPUT TO GTS5:',I8,1X,F10.4,1X,'ALTkm:',F12.3,1X,&
!             'LAT,LON: ',2(F9.3,1X),/,1X,'LOC TIME: ',F8.5, &
!               'FLUXAVG = ',F6.2,' VS. ',F6.1,'DAILY FLUX. ')
!
!  CALL GTS5 TO CALCULATE DENSITY, TEMP, ETC.  OUT OF THIS ONLY
!  OUTPUT TOTAL DENSITY OF ATMOSPHERE FOR NOW (=DEN(6)), IN G/CC.
!
      
!          CALL GTS5(IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,      &
!         &          48,DEN,TEMP) 
!                                 
      WRITE(6,*) 'CHECK -- MSIS.f90: Call MSISinit'
      WRITE(6,666) SWI

 666   FORMAT(25F12.4)

!              WRITE(6,*) 'CHECK -- MSIS.f90: Called msisinit'
!
!      CALL msisinit(parmpath='',parmfile='msis2.0.parm',                &
!          & switch_legacy=SWI  )      
!     
!     
!
      CALL gtd8d(IYYDDD,UTSEC,ALTKM,GLAT,GLON,STLOC,AVGFLX,FLUX,AP,     &
          &          48,DEN,TEMP)      
      
!
!  GTD8D OUTPUTS DENSITY IN G/CC.  CONVERT TO KG/M3.
      RHO = DEN(6)*1000.D0
!
!  Calculate drho/dz.
      IF(IDRV .NE. 0) THEN
!
        TERM1 = -1.66D-24*(16.D0*DEN(1) + 256.D0*DEN(2) + 784.D0*DEN(3) &
     &          + DEN(7) + 196.D0*DEN(8) )
        TERM2 = GSURF/(TEMP(2)*RGAS)/(1.D0 + ZL/RE)**2
        TERM3 = ((RE+ZL)/(RE+ALTKM))**2
        DRHODZ = TERM1*TERM2*TERM3
!       DRHODZ IS NOW IN G/CC/KM.  THIS IS DIMENSIONALLY EQUAL TO KG/M4.
!
      ENDIF
!
!     PRINT 812,RHO,TEMP(1),TEMP(2)
! 812 FORMAT(1X,'OUTPUT FR. GTS5: DEN(KG/M3) = ',E15.8,2X,'EXO TEMP=',
!    @       E15.8,2X,'TEMP AT ALT = ',E15.8)
!
   99 RETURN
      END
