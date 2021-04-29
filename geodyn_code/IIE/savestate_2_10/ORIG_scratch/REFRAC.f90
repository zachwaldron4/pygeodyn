!$
      SUBROUTINE REFRAC(COSEL,SINEL,EDOT,RNM,CDRY,CWET,CION,NM,         &
     &   MODELT,LNDRYT,LNWETT,LNIONO,BLKMET,OBSMET,LOBMET,LALTON,       &
     &   RLATA,COSLTA,SINLTA,HTA,MTYPE,MJDSBL,FSEC,NSTA0)
!********1*********2*********3*********4*********5*********6*********7**
! REFRAC           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CONTROLS THE COMPUTATION OF REFRACTION CORRECTIONS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   COSEL    I    A    COSINE OF ELEVATION
!   SINEL    I    A    SINE OF ELEVATION
!   EDOT     I    A    TIME RATE OF CHANGE OF ELEVATION
!   RNM      I    A    RANGE FROM STATION TO S/C
!   CDRY     O    A    DRY TROPOSPHERIC REFRACTION CORRECTION
!   CWET     O    A    WET TROPOSPHERIC REFRACTION CORRECTION
!   CION     O    A    IONOSPHERIC REFRACTION CORRECTION
!   NM       I    S    NUMBER OF OBSERVATION IN THIS LOGICAL BLOCK
!   MODELT   I    S    REFRACTION MODEL INDICATOR
!   LNDRYT   I    S    LOGICAL FLAG TO THE DRY TROPOSPHERIC REFRACTION
!                      CORRECTION
!   LNWETT   I    S    LOGICAL FLAG TO THE WET TROPOSPHERIC REFRACTION
!                      CORRECTION
!   LNIONO   I    S    LOGICAL FLAG TO THE IONOSPHERIC REFRACTION
!                      CORRECTION
!   BLKMET   I    S    METEOROLOGICAL DATA FROM THE BLOCK HEADER RECORDS
!   OBSMET   I    A    METEOROLOGICAL DATA FOR EACH OBSERVATION IN THE
!                      BLOCK
!   LOBMET   I    S    LOGICAL FLAG FOR METEOROLOGICAL DATA
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      DIMENSION COSEL(NM),SINEL(NM),EDOT(NM),RNM(NM),CDRY(NM),CION(NM)
      DIMENSION OBSMET(NM),CWET(NM),FSEC(NM),JYMD(1),JHM(1),FSEC1(1)
      DIMENSION RLATA(NM),COSLTA(NM),SINLTA(NM),HTA(NM)
      DIMENSION HMF(2),WMF(2),IOROGRAPHY_ARRAY(91,145)
!***** FOLLOWING STATEMENT FOR DEBUG ONLY
!     DIMENSION ARGNAM(4)
!***** FOLLOWING STATEMENT FOR DEBUG ONLY
!     DATA ARGNAM/'COSEL   ','SINEL   ','EDOT    ','RNM     '/
      DATA ZERO/0.0D0/,ONE/1.0D0/,HUNDRD/100.0D0/,C1500/1.5D3/
      DATA CFLAM0/0.965D0/,CFLAM2/0.0164D-12/,CFLAM4/0.228D-27/
      DATA CFPOSL/-2.6D-3/,CFPOSH/-3.1D-7/,DPDH/-1.1138D-4/
      DATA P1ATM/1013.5D0/,STDTMP/293.16D0/,VAPOR0/9.35D0/
      DATA kentry/0/

      PARAMETER( HALF = 0.5D0 )
      PARAMETER( C1D2 = 1.0D2 )
      PARAMETER( C1D6 = 1.0D6 )

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!      write(6,*) "I am in REFRAC"
!
!     PRINT 11111,ARGNAM(1),COSEL
!     PRINT 11111,ARGNAM(2),SINEL
!     PRINT 11111,ARGNAM(3),EDOT
!     PRINT 11111,ARGNAM(4),RNM
!11111 FORMAT(' ** REFRAC **  ',A5,' ='/(1X,6D12.5))
!     PRINT 22222,NM,LNDRYT,LNWETT,LNIONO
!22222 FORMAT(' ** REFRAC **  NM,LNDRYT,LNWETT,LNIONO =',I10,
!    1       3(1X,L1))
!      PRINT 33333,RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
!     &   DISP,WAVLEN
!33333 FORMAT(' ** REFRAC **  COMMON/CSTA/ '/                           &
!     &   ' RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,DISP,WAVLEN ='/ &
!     &   (1X,5D15.7))
! IF KENTRY is 1 and if model is 8 read the orography file
      kentry = kentry + 1
!       write(6,*) "REFRAC: KENTRY is :", KENTRY
      IF (kentry.EQ.1 .AND. MODEL(MTYPE).EQ.8) THEN

         CALL READ_OROGRAPHY(IOROGRAPHY_ARRAY)

      ENDIF

      IF(LNDRYT) GO TO 5000
      IF(.NOT.LALTON) THEN
         CALL METDAT(BLKMET)
         IF(P0.GT.HUNDRD.AND.P0.LT.C1500) GO TO 1000
! DEFAULT PRESSURE, TEMPERATURE AND WATER VAPOR PRESSURE
         P0=P1ATM*(ONE+DPDH*HEIGHT)
         T0=STDTMP
         E0=VAPOR0
 1000 CONTINUE
      ENDIF
!     PRINT 44444,MODEL,P0,T0,E0,WAVLEN
!44444 FORMAT(' ** REFRAC **  MODEL(MTYPE,P0,T0,E0,WAVLEN =',I10,4D12.5)
!
! AVAILABLE MODELS ARE: 2=VLBI/GPS,1=MARINI-MURRAY, 0=HOPFIELD
!                3= GPS MODEL USING NEIL MAPPING FUNCTIONS
!                4= MODEL 3 FOR NON-LASER DATA/MARINI-MURRAY FOR LASER D
!                5= PORTO MODEL FOR LASER DATA
!                6= GLOBAL MAPPING FUNCTIONS (GMF)
!
! HOPFIELD MODEL
 1500 CONTINUE
!gps     IF(MODEL(MTYPE).NE.1) GO TO 2000
!     IF(MODEL(MTYPE).EQ.6) GO TO 3000
      IF(MODEL(MTYPE).NE.1) GO TO 2000
!     IF(MODEL(MTYPE).NE.1.AND.IRFRC.EQ.1) GO TO 2000
! REFRACTION MODEL TYPES ARE: 0=ALTIMETER, 1=RANGE, 2=DOPPLER, 3=ELEV.
!     PRINT *,'REFRAC* MODELT=',MODELT
      MTEST=(MODELT+1)/2
!     PRINT *,'REFRAC* MTEST =',MTEST
! MARINI-MURRAY MODEL MAY BE USED FOR LASER RANGE ONLY
      IF(MTEST.NE.1) GO TO 2000
      IF(.NOT.LASER) GO TO 2000
! MARINI-MURRAY MODEL SELECTED
!
! WAVELENGTH FUNCTION
      RLAM2I=ONE/WAVLEN**2
      FLAM=CFLAM0 + CFLAM2*RLAM2I + CFLAM4*(RLAM2I*RLAM2I)
! POSITION FUNCTION
      COSLT2=COSLAT*COSLAT-SINLAT*SINLAT
      FPOS=ONE  + CFPOSL  *COSLT2      + CFPOSH   *HEIGHT
! EVALUATE MODEL FOR EACH MEASUREMENT
      DO 1800 N=1,NM
      IF(LOBMET.AND..NOT.LALTON) CALL METDAT(OBSMET(N))
      IF(LALTON) THEN
        COSLT2=COSLTA(N)*COSLTA(N)-SINLTA(N)*SINLTA(N)
        FPOS=ONE  + CFPOSL  *COSLT2      + CFPOSH   *HTA(N)
        HEIGHT=HTA(N)
        IF(LOBMET) THEN
           CALL METDAT(OBSMET(N))
        ELSE
           CALL METDAT(BLKMET)
           IF(P0.LT.HUNDRD.OR.P0.GT.C1500) THEN
! DEFAULT PRESSURE, TEMPERATURE AND WATER VAPOR PRESSURE
              P0=P1ATM*(ONE+DPDH*HEIGHT)
              T0=STDTMP
              E0=VAPOR0
            ENDIF
        ENDIF
      ENDIF
      CDRY(N)=TROPMM(MODELT,P0,T0,E0,SINEL(N),COSEL(N),EDOT(N),FLAM,    &
     &                COSLT2,FPOS,CWET(N))
 1800 END DO
!     PRINT 55555,CDRY
!5555 FORMAT(' ** REFRAC **  CDRY ='/(1X,6D12.5))
!     PRINT 66666,CWET
!6666 FORMAT(' ** REFRAC **  CWET ='/(1X,6D12.5))

      GO TO 5000
 2000 CONTINUE
! HOPFIELD OR VLBI/GPS MODEL OR GMF MODEL SELECTED
! EVALUATE MODEL FOR EACH MEASUREMENT
      MODE=1
      IF(LASER) MODE=2
      MODELX=MODELT
!      IF(MODEL(MTYPE).EQ.2.OR.MODEL(MTYPE).EQ.3.OR.MODEL(MTYPE).EQ.4.OR.&
!    &    MODEL(MTYPE).EQ.6.OR.MODEL(MTYPE).EQ.7)  MODELX=0
!     IF(MODEL(MTYPE).EQ.3.OR.MODEL(MTYPE).EQ.4) CALL XDOY(DOY)
!      IF(MODEL(MTYPE).EQ.3.OR.MODEL(MTYPE).EQ.4.OR.MODEL(MTYPE).EQ.5.OR.&
!     & MODEL(MTYPE).EQ.6.OR.MODEL(MTYPE).EQ.7)  CALL XDOY(DOY)

       IF( MODEL(MTYPE).EQ.2 .OR. &
          MODEL(MTYPE).EQ.3 .OR. &
          MODEL(MTYPE).EQ.4 .OR. &
          MODEL(MTYPE).EQ.6 .OR. &
          MODEL(MTYPE).EQ.7 .OR. &
          MODEL(MTYPE).EQ.8        )  MODELX=0

      IF( MODEL(MTYPE).EQ.3 .OR.  &
          MODEL(MTYPE).EQ.4 .OR.  &
          MODEL(MTYPE).EQ.5 .OR.  &
          MODEL(MTYPE).EQ.6 .OR.  &
          MODEL(MTYPE).EQ.7 .OR.  &
          MODEL(MTYPE).EQ.8          )  CALL XDOY(DOY)

      RLATD=RLAT*57.295777951D0
! CONVERT WAVELENGTH TI METERS AND LATITUDE IN DEGREES FOR MODEL 5
      W1=WAVLEN*1.D06
      PHI_DEG=RLAT/DEGRAD
! CONVERT WAVELENGTH TI METERS AND LATITUDE IN DEGREES FOR MODEL 5
      DO 2800 N=1,NM
      IF(LOBMET.AND..NOT.LALTON) CALL METDAT(OBSMET(N))
      IF(LALTON) THEN
        HEIGHT=HTA(N)
        IF(LOBMET) THEN
           CALL METDAT(OBSMET(N))
        ELSE
           CALL METDAT(BLKMET)
           IF(P0.LT.HUNDRD.OR.P0.GT.C1500) THEN
! DEFAULT PRESSURE, TEMPERATURE AND WATER VAPOR PRESSURE
              P0=P1ATM*(ONE+DPDH*HEIGHT)
              T0=STDTMP
              E0=VAPOR0
            ENDIF
        ENDIF
      ENDIF
!
! MODEL=6 OR 7, GPT+GMF REQUIRED
!      IF(MODEL(MTYPE).EQ.6.OR.MODEL(MTYPE).EQ.7)  THEN
!
      IF( MODEL(MTYPE).EQ.6 .OR. &
          MODEL(MTYPE).EQ.7 .OR. &
          MODEL(MTYPE).EQ.8) THEN
!       write(6,*) "REFRAC: USING GPT"
! USE GPT TO DETERMINE P0 AND T0.
        CALL GPT(DOY,RLAT,RLON,HEIGHT,P0,T0,DUM)
! CONVERT CELIUS TO KELVIN
          T0=T0+273.15D0

!        write(6,*) "refrac: P0 T0:", P0, T0

        IF (MODEL(MTYPE).EQ.8) GOTO 2700

        GOTO 3000
      ENDIF
! IF MODEL=4 and LASER DATA, USE MARINI-MURRAY MODEL
!
      IF(MODEL(MTYPE).EQ.4.AND.MODE.EQ.2)THEN
!       IF(LALTON) THEN
          COSLT2=COSLTA(N)*COSLTA(N)-SINLTA(N)*SINLTA(N)
          FPOS=ONE  + CFPOSL  *COSLT2      + CFPOSH   *HTA(N)
!       ENDIF
        CDRY(N)=TROPMM(MODELT,P0,T0,E0,SINEL(N),COSEL(N),EDOT(N),FLAM,  &
     &                 COSLT2,FPOS,CWET(N))
        GO TO 2800
       ENDIF
!
      IF(MODEL(MTYPE).EQ.5.AND.MODE.EQ.2)THEN
!CC    TANEL=SINEL(N)/COSEL(N)
!CC    ELEV_DEG=ATAN(TANEL)/DEGRAD

!ecp  BEGIN CHANGED CODE:

!ecp  TZD= SA_ZTD_LASER(PHI_DEG,HEIGHT,P0,E0,W1)      !  This is the ori
!     Saastamoinen formulation
!ecp  IF(METP.EQ.0)  CDRY(N)=FCULA(PHI_DEG,HEIGHT,T0,SINEL(N),TZD)
!ecp  IF(METP.EQ.1)  CDRY(N)=FCULB(PHI_DEG,HEIGHT,DOY,SINEL(N),TZD)
!ecp  CWET(N)=0.D0

!ecp  TZD= SAPC_ZTD_LASER(PHI_DEG,HEIGHT,P0,E0,W1)    !  1st version of
!      Stefan modified to use the P. Ciddor dispersion
!ecp  TZD= FCULB_ZTD_LASER(PHI_DEG,HEIGHT,P0,E0,W1,TZDRY,TZDWET)    !  2
!on of new ZTD. Virgilio's modified version to use the P. Ciddor dispers

!ecp  This is the final, published version of the new ZTD model, broken
!      and WET delay:

      CALL fculzd_hPa(PHI_DEG, HEIGHT, P0, E0, W1, TZD, TZDRY, TZDWET)

      IF(METP(MTYPE).EQ.0)                                              &
     &       CDRY(N)=FCULA(PHI_DEG,HEIGHT,T0,SINEL(N),TZDRY)
      IF(METP(MTYPE).EQ.0)                                              &
     &       CWET(N)=FCULA(PHI_DEG,HEIGHT,T0,SINEL(N),TZDWET)
      IF(METP(MTYPE).EQ.1)                                              &
     &       CDRY(N)=FCULB(PHI_DEG,HEIGHT,DOY,SINEL(N),TZDRY)
      IF(METP(MTYPE).EQ.1)                                              &
     &       CWET(N)=FCULB(PHI_DEG,HEIGHT,DOY,SINEL(N),TZDWET)

!ecp  END CHANGED CODE

        GO TO 2800
       ENDIF
!
      TINV=ONE/T0

      CDRY(N)=TROPRF(MODELX,MODE,P0,TINV,E0,SINEL(N),COSEL(N),EDOT(N),  &
     &                RNM(N),CWET(N))

! USE HOPFIELD TO DETERMINE WET AND DRY CORRECTIONS AND THEN USE
! MAPPING FUNCIONS TO GET VLBI/GPS MODEL
      IF(MODEL(MTYPE).EQ.2) THEN
         CALL TRPGPS(CDRY(N),CWET(N),SINEL(N),COSEL(N),                 &
     &               P0,T0,E0)
      ENDIF
      IF(MODEL(MTYPE).EQ.3.OR.MODEL(MTYPE).EQ.4) THEN
        ELX=ASIN(SINEL(N))*57.29577951D0
        IF(LALTON) THEN
           HEIGHT=HTA(N)
            RLATD=RLATA(N)*57.29577951D0
        ENDIF
        CALL NMFH2P(DOY,RLATD,HEIGHT,ELX,HMF)
        CALL NWMF2(RLATD,ELX,WMF)
        CDRY(N)=CDRY(N)*HMF(1)
        CWET(N)=CWET(N)*WMF(1)
      ENDIF

      !----------------------------------------------------------
 2700 CONTINUE

!      IF (MTYPE .EQ. 40) THEN
       IF( MODEL(MTYPE).EQ. 8  ) THEN
!       write(6,*) "REFRAC: CDRY CWET is:", N, CDRY(N), CWET(N)
!       write(6,*) "MODELX,MODE,P0,TINV:", MODELX,MODE,P0,T0
!       write(6,*) "E0,SINEL(N),COSEL(N):", E0,SINEL(N),COSEL(N)
!       write(6,*) "EDOT(N), RNM(N),CWET(N):", EDOT(N), RNM(N),CWET(N)

        TINV=ONE/T0
        CDRY(N)=TROPRF(MODELX,MODE,P0,TINV,E0,SINEL(N),COSEL(N),&
                       EDOT(N), RNM(N),CWET(N))

!       write(6,*) "REFRACG:GPT W/HOPFIELD: CDRY is:", CDRY(N)
!       write(6,*) "REFRACG:GPT W/HOPFIELD: CWET is:", CWET(N)

      ENDIF
      IF( MODEL(MTYPE).EQ. 8  ) THEN  ! VMF

!           write(6,*) "REFRAC: I am in model 8"

          !   VMF
          !     AH     d      Hydrostatic coefficient a (Note 1)
          !     AW     d      Wet coefficient a (Note 1)
          !     DMJD   d      Modified Julian Date
          !     DLAT   d      Latitude given in radians (North Latitude)
          !     HT     d      Ellipsoidal height given in meters
          !     ZD     d      Zenith distance in radians
          !  Returned:
          !     VMF1H  d      Hydrostatic mapping function (Note 2)
          !     VMF1W  d      Wet mapping function (Note 2)


!          write(6,'(A,1x,i8)') 'refrac: MODELX ', MODELX

          ! dmjd is MJDSBL + FSECBL

          dmjd = DBLE(MJDSBL) + FSEC(NM)

!          write(6,'(A,1x,I20,2x,F10.5)') &
!                'refrac: MJDSBL, FSEC(NM) ', &
!                         MJDSBL, FSEC(NM)

          CALL YMDHMS(MJDSBL,FSEC(NM),JYMD,JHM,FSEC1,1)
          IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
          STRTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC1(1)

!          write(6,*) "refrac: STRTIM:", STRTIM

          !write(6,'(A,8x,F20.2,2x,F12.8)') &
          !      'refrac: dmjd, doy  ', dmjd, doy

          DLAT = RLAT / degrad
          DLON = RLON / degrad

!GET HEIGHT FROM OROGRAPHY_ELL FILE AS THIS IS THE HEIGHT THE
! VMF CORRESPOND TO. NEEDED FOR HEIGHT CORRECTION



          CALL DET_HEIGHTS(DLAT,DLON,IOROGRAPHY_ARRAY,HEIGHT_ORO)

!          write(6,*) "REFRAC: HEIGHT_ORO :", HEIGHT_ORO

          !write(6,'(A,2(1x,F12.8))') &
          !      'refrac: rlon, rlat ', rlon, rlat

          HTS = HEIGHT ! = HTA(N) ? or from common block ?

!          write(6,*) "REFRAC: HTS is :", HTS

          HTG = HEIGHT_ORO

!          write(6,'(A,1x,F12.5)') 'refrac8: HT ', HT

          HT_DIFF = HTS - HTG

          TANEL=SINEL(N)/COSEL(N)
          ELEVAT=ATAN(TANEL)

          !write(6,'(A,1x,F12.8)') 'refrac: ELEVAT      ', &
          !                                 ELEVAT
          !write(6,'(A,1x,F12.8)') 'refrac: ELEV (deg)  ', &
          !                                 ELEVAT / degrad

          ! ZENITH DISTANCE IN RADIANS

          ZENDIST=(PI*0.5D0)-ELEVAT

          zd = zendist

          !write(6,'(A,1x,F12.8)') 'refrac: ZD        ', ZD
          !write(6,'(A,1x,F12.8)') 'refrac: ZD (deg)  ', ZD / degrad

          call vmf_grid( DMJD, RLAT, RLON, HTG, HTS,HT_DIFF,ZD, &
     &                    VMF1H, VMF1W,CDRY(N),CWET(N) )


          ZPDH = CDRY(N)
          ZPDW = CWET(N)

!!          IF (LITER1 .AND. IRFPRT==1) THEN

!!           WRITE(400,'(I8,1x,G24.16,1X,E20.10,1x,E20.10)') &
!!                 NSTA0,STRTIM,ZPDH,ZPDW

!!          ENDIF



          if (nm == 1) then
!
          write(6,'(A,2(1x,E15.7)/)') 'refrac: VMF1H, VMF1W  ', &
                                               VMF1H, VMF1W


              write(6,'(A,1x,I8,2x, 2(1x,E15.7)/)')  &
     &           'refrac: n, CDRY(n), CWET(n)'  ,  &
     &                    n, CDRY(n), CWET(n)

!
          endif

          CDRY(N)=CDRY(N) * VMF1H
          CWET(N)=CWET(N) * VMF1W


        if (NM == 1) then
       write(6,'(A,1x,I8,2x, 2(1x,E15.7)/)')  &
     &        'refrac Total path delay: n, CDRY(n), CWET(n)'  ,  &
     &                 n, CDRY(n), CWET(n)

        endif

!           IF (N == NM) THEN
!              STOP
!           ENDIF
      ENDIF

      !----------------------------------------------------------

 2800 END DO
!     PRINT 55555,CDRY
!     PRINT 66666,CWET

      GO TO 5000

 3000 CONTINUE
!     CALL XDOY(DOY)
      MODELX=0
      MODE=1
!     THE GMF (GLOBAL FUNCTION MODEL) HAS BEEN SELECTED
      IF(MTYPE.EQ.40.OR.LGPS) THEN

! USE HOPFIELD TO DETERMINE WET AND DRY CORRECTIONS AND THEN USE
! MAPPING FUNCIONS TO GET VLBI/GPS MODEL
         DO N=1,NM
      TINV=ONE/T0
      CDRY(N)=TROPRF(MODELX,MODE,P0,TINV,E0,SINEL(N),COSEL(N),EDOT(N),  &
     &                RNM(N),CWET(N))
!        CALL TRPGPS(CDRY(N),CWET(N),SINEL(N),COSEL(N),                 &
!    &               P0,T0,E0)
         ENDDO
! Sass model
      IF(MODEL(MTYPE).EQ.7) THEN
      DSASS=0.0022768D0*P0/(1.D0-0.00266D0*COS(2.D0*RLAT)-             &
     &                      0.00028D-3*HEIGHT)
      ENDIF

!     COMPUTE MODIFIED JULIAN DATE
!     CALL MJDYMD(MJDSBL,IYMD,IHMS,4)
!     CALL MJDYMD(MJDS,IYMD,IHMS,2)
! MJDS MODIFIED JULIAN DATE
! RLAT STATION LATITUDE IN RADIANS
! RLON STATION LONGITUDE IN RADIANS
! HEIGHT STATION HEIGHT IN METERS  (COMMON CSTA FOR ALL THE ABOVE)
       DO N=1,NM
       TANEL=SINEL(N)/COSEL(N)
       ELEVAT=ATAN(TANEL)
! ZENITH DISTANCE IN RADIANS
       ZENDIST=(PI/2.D0)-ELEVAT
       CALL GMF(DOY,RLAT,RLON,HEIGHT,ZENDIST,GMFH,GMFW)
!
!       write(6,*) "REFRACS: CDRY(N) is", CDRY(N)
!       write(6,*) "REFRACS: DSASS is :", DSASS
!       write(6,*) "REFRACS: CWET(N) is", CWET(N)
!       write(6,*) "REFRACS: GMFH is", GMFH
!       write(6,*) "REFRACS: GMFW is", GMFW

       if (nm == 1) then

       write(6,'(A,2(1x,E15.7)/)') 'refrac: GMFH, GMFW  ', &
                                            GMFH, GMFW

            write(6,'(A,1x,I8,2x, 2(1x,E15.7)/)')  &
     &         'refrac: n, CDRY(n), CWET(n)'  ,  &
     &                  n, CDRY(n), CWET(n)


         endif




       CWET(N)=CWET(N)*GMFW
       CDRY(N)=CDRY(N)*GMFH

!
!        if (nm == 1) then
!            write(6,'(A,1x,I8,2x, 2(1x,E15.7)/)')  &
!     &         'refrac Total path delay: n, CDRY(n), CWET(n)'  ,  &
!     &                  n, CDRY(n), CWET(n)
!         endif

       IF(MODEL(MTYPE).EQ.7) CDRY(N)=DSASS*GMFH
       ENDDO

      ELSE
! THIS MODEL APPLIES ONLY TO DORIS AND GPS
      WRITE(6,*)' THE GMF MODEL (MODEL=6) APPLIES TO DORIS AND GPS '
      WRITE(6,*)' AND DORIS ONLY. PLEASE CHECK THE REFRAC OPTION   '
      WRITE(6,*)' EXECUTION STOPS IN SUBROUTINE REFRAC '
      STOP 16
      ENDIF

 5000 CONTINUE
      IF(LNIONO) RETURN
! NO IONOSPHERE MODEL. SET CORRECTION TO ZERO.
! DEP DO NOT ZERO OUT THE IONO CORR
!     DO 5800 N=1,NM
!     CION(N)=ZERO
!5800 CONTINUE
      RETURN
      END
