!$
      SUBROUTINE VMF1(COSEL,SINEL,EDOT,RNM,CDRY,CWET,CION,NM,           &
     &   MODELT,LNDRYT,LNWETT,LNIONO,BLKMET,OBSMET,LOBMET,LALTON,       &
     &   RLATA,COSLTA,SINLTA,HTA,MTYPE,MJDSBL,FSEC,IWPR,NSTA0,ETUTC)
!********1*********2*********3*********4*********5*********6*********7**
! VMF1           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  TROP REFRACTION CORRECTION USING VMF1 MODEL
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
!   LNIONO   I    S    LOGICAL FLAG TO THE IONOSHPERIC REFRACTION
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
      COMMON/LSTRT/LSTART
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      DIMENSION ETUTC(1)
      DIMENSION COSEL(NM),SINEL(NM),EDOT(NM),RNM(NM),CDRY(NM),CION(NM)
      DIMENSION OBSMET(NM),CWET(NM),FSEC(NM),JYMD(1),JHM(1),FSEC1(1)
      DIMENSION RLATA(NM),COSLTA(NM),SINLTA(NM),HTA(NM),AAH(1),AAW(1)
      DIMENSION HMF(2),WMF(2),IOROGRAPHY_ARRAY(91,145)
      DIMENSION JJYMD(NM),JJHM(NM),JJDD(NM),JJHH(NM),ZWET(1),ZDRY(1)
      DIMENSION DMM(1), DHH(1), VMF(2,NM), STIMMP(1)
      DIMENSION VMFCOEFF(3,4),aa(2,4),bb(2,4),cc(2,4),dd(2,4),YY(4)
      DATA ZERO/0.0D0/,ONE/1.0D0/,HUNDRD/100.0D0/,C1500/1.5D3/
      DATA CFLAM0/0.965D0/,CFLAM2/0.0164D-12/,CFLAM4/0.228D-27/
      DATA CFPOSL/-2.6D-3/,CFPOSH/-3.1D-7/,DPDH/-1.1138D-4/
      DATA P1ATM/1013.5D0/,STDTMP/293.16D0/,VAPOR0/9.35D0/
      DATA kentry/0/
      DATA SEC6HR/21600.0D0/

      PARAMETER( HALF = 0.5D0 )
      PARAMETER( C1D2 = 1.0D2 )
      PARAMETER( C1D6 = 1.0D6 )

      INTEGER :: IWPR
      INTEGER :: IVMFCHK

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!      write(6,*) 'jtw I am in VMF1'
!
      kentry = kentry + 1

      IF (kentry.EQ.1) THEN

         CALL READ_OROGRAPHY(IOROGRAPHY_ARRAY)

      ENDIF

!!(1)find midpoint time

      CALL UTCET(.FALSE.,1,MJDSBL,FSEC(NM/2),FSECU,ETUTC)
      Tmid=DBLE(MJDSBL)+FSECU

!!(2)find time block

      CALL YMDHMS(MJDSBL,FSECU,JYMDMP,JHMMP,FSEC1,1)
      IF(JYMDMP.GE.1000000)JYMDMP=JYMDMP-1000000
      STIMMP=DBLE(JYMDMP)*C1D6+DBLE(JHMMP)*C1D2+FSEC1

      IF (JHMMP.GE.300.AND.JHMMP.LT.900)   JFLAG1=1
      IF (JHMMP.GE.900.AND.JHMMP.LT.1500)  JFLAG1=2
      IF (JHMMP.GE.1500.AND.JHMMP.LT.2100) JFLAG1=3
      IF (JHMMP.GE.2100.OR.JHMMP.LT.300)   JFLAG1=4

!!(3a)get two values of delay

      IF (JFLAG1==1) THEN
         IF (JHMMP.GE.600) JFLAG2=1
         IF (JHMMP.LT.600) JFLAG2=2
      ELSE IF (JFLAG1==2) THEN
         IF (JHMMP.GE.1200)JFLAG2=1
         IF (JHMMP.LT.1200) JFLAG2=2
      ELSE IF (JFLAG1==3) THEN
         IF (JHMMP.GE.1800) JFLAG2=1
         IF (JHMMP.LT.1800) JFLAG2=2
      ELSE IF (JFLAG1==4) THEN
         IF (JHMMP.GE.2100) JFLAG2=1
         IF (JHMMP.LE.300) JFLAG2=2
      END IF

!!(3b) get 2 values of Z

      Tmid0=Tmid-SEC6HR
      Tmid2=Tmid+SEC6HR

      RLATD=RLAT*57.295777951D0

      DLAT = RLAT / degrad
      DLON = RLON / degrad

      CALL DET_HEIGHTS(DLAT,DLON,IOROGRAPHY_ARRAY,HEIGHT_ORO)

      HTS = HEIGHT ! = HTA(N) ? or from common block ?
      HTG = HEIGHT_ORO
      HT_DIFF = HTS - HTG

      TANEL=SINEL(NM/2)/COSEL(NM/2)
      ELEVAT=ATAN(TANEL)

      ZENDIST=(PI*0.5D0)-ELEVAT
      zd = zendist

!!VMFCOEFF(3,4)
!hold the values of Zd, Zw, ah, and aw for the three
!interpolation points

          call vmf_grid(Tmid0, RLAT, RLON, HTG, HTS,HT_DIFF,ZD,       &
     &                    VMF1H, VMF1W,CZD,CZW,AH,AW)

        VMFCOEFF(1,1)=CZD
        VMFCOEFF(1,2)=CZW
        VMFCOEFF(1,3)=AH
        VMFCOEFF(1,4)=AW

          call vmf_grid(Tmid, RLAT, RLON, HTG, HTS,HT_DIFF,ZD,       &
     &                    VMF1H, VMF1W,CZD,CZW,AH,AW)

        VMFCOEFF(2,1)=CZD
        VMFCOEFF(2,2)=CZW
        VMFCOEFF(2,3)=AH
        VMFCOEFF(2,4)=AW

          call vmf_grid(Tmid2, RLAT, RLON, HTG, HTS,HT_DIFF,ZD,      &
     &                    VMF1H, VMF1W,CZD,CZW,AH,AW)

        VMFCOEFF(3,1)=CZD
        VMFCOEFF(3,2)=CZW
        VMFCOEFF(3,3)=AH
        VMFCOEFF(3,4)=AW

!!(4) Write values for interpolation

      IF (JFLAG1==1.AND.JFLAG2==1) THEN
         T1=3.0D0
         T2=15.0D0
      ELSE IF (JFLAG1==1.AND.JFLAG2==2) THEN
         T1=21.0D0
         T2=33.0D0
      ELSE IF (JFLAG1==2.AND.JFLAG2==1) THEN
         T1=9.0D0
         T2=21.0D0
      ELSE IF (JFLAG1==2.AND.JFLAG2==2) THEN
         T1=3.0D0
         T2=15.0D0
      ELSE IF (JFLAG1==3.AND.JFLAG2==1) THEN
         T1=15.0D0
         T2=27.0D0
      ELSE IF (JFLAG1==3.AND.JFLAG2==2) THEN
         T1=9.0D0
         T2=21.0D0
      ELSE IF (JFLAG1==4.AND.JFLAG2==1) THEN
         T1=15.0D0
         T2=27.0D0
      ELSE IF (JFLAG1==4.AND.JFLAG2==2) THEN
         T1=21.0D0
         T2=33.0D0
      END IF

!!(5) calculate coefficients for fit
!! a(2,4),b(2,4),c(2,4),d(2,4)
!!
!! aa(1,1)-coeff a1 for zdry
!! aa(2,1)-coeff a2 for zdry
!! aa(1,2)-coeff a1 for zwet
!! aa(2,2)-coeff a2 for zwet
!! aa(1,3)-coeff a1 for ah
!! aa(2,3)-coeff a2 for ah
!! aa(1,4)-coeff a1 for aw
!! aa(2,4)-coeff a2 for aw
!! h (t2-t1,t3-t1) = 6, simplified calculation

        DO i=1,4
           aa(1,i)=(VMFCOEFF(3,i)-2.0D0*VMFCOEFF(2,i)+                &
     &                VMFCOEFF(1,i))/(864.0D0)
           aa(2,i)=-aa(1,i)
           bb(1,i)=0.0D0
           bb(2,i)=(1.0D0/24.0D0)*(VMFCOEFF(3,i)-2.0D0*VMFCOEFF(2,i)+ &
     &                VMFCOEFF(1,i))
           cc(1,i)=((VMFCOEFF(2,i)-VMFCOEFF(1,i))/6.0D0)-             &
     &       (VMFCOEFF(3,i)-2.0D0*VMFCOEFF(2,i)+VMFCOEFF(1,i))/(24.0D0)
           cc(2,i)=((VMFCOEFF(3,i)-VMFCOEFF(2,i))/6.0D0)-             &
     &       (VMFCOEFF(3,i)-2.0D0*VMFCOEFF(2,i)+VMFCOEFF(1,i))/(12.0D0)
         dd(1,i)=VMFCOEFF(1,i)
         dd(2,i)=VMFCOEFF(2,i)
        END DO

!!(6) convert time to fractional hours

      IF (JHMMP.LT.600) JHMMP=JHMMP+2400

      JHH=MOD(JHMMP/100,100)
      JMM=MOD(JHMMP,100)
      DMM=DBLE(JMM)+FSEC1/60.0D0
      DHH=DBLE(JHH)+DMM/60.0D0

!!(7) interpolate

      DUM1=DHH(1)

!!T2 is really T3, use PIV for T2

      PIV=T1+(T2-T1)/2.0D0

      IF (DUM1.LT.PIV) THEN
         jj=1
         DIST=DUM1-T1
      ELSE IF (DUM1.GE.PIV) THEN
         jj=2
         DIST=DUM1-PIV
      END IF

      D2IST=DIST*DIST
      D3IST=DIST*D2IST

      DO i=1,4
         YY(i)=aa(jj,i)*D3IST+bb(jj,i)*D2IST+cc(jj,i)*DIST+dd(jj,i)
      END DO

!!(8) assign to Cdry and Cwet

       ZDRY=YY(1)
       ZWET=YY(2)
       AAH =YY(3)
       AAW =YY(4)

!!(9) get MF values

      DO 2800 N=1,NM

         CALL UTCET(.FALSE.,1,MJDSBL,FSEC(N),FSECU,ETUTC)
         DMJD=DBLE(MJDSBL)+FSECU
          CALL YMDHMS(MJDSBL,FSEC(N),JYMD,JHM,FSEC1,1)
          IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
          STRTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC1(1)

          IF (LITER1 .AND. IWPR==1) THEN

           WRITE(400,'(I8,1x,G24.16,1X,E20.10,1x,E20.10)') &
                 NSTA0,STRTIM,ZDRY,ZWET

          ENDIF

         TANEL=SINEL(N)/COSEL(N)
         ELEVAT=ATAN(TANEL)

         ZENDIST=(PI*0.5D0)-ELEVAT
         zd = ZENDIST

         DMJD = INT(DMJD/86400.0D0)+30000

         call vmf1_ht(AAH,AAW,DMJD,RLAT,HTS,ZD,VMF1H,VMF1W )

         CDRY(N)=ZDRY(1)*VMF1H
         CWET(N)=ZWET(1)*VMF1W

 2800 END DO

 5000 CONTINUE
      IF(LNIONO) RETURN
      RETURN
      END
