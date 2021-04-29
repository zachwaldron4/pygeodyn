!$GPSATT
      SUBROUTINE GPSATT(MJDS,FSEC,SATPOS,SATVEL,BFNRM1,              &
     &           BFNRM2,BFNRM3,TDNRM1,TDNRM2,TDNRM3,CTHETA,          &
     &           NFACE,NMOVE,LFORCE,                                 &
     &           IDSATS,IDATTB,SABIAS,TIMBI1,TIMBI2,                 &
     &           VLOUVS,NSTLOV,ISLVID,TSLOUV,AA,II,ISATID,TOTROT,    &
     &           ATROT,LTUM,ISEQ,LWNDI,IUPDN,WPU)
!*******************************************************************
!  ROUTINE NAME:   GPSATT   DATE: 02/21/92      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE ROTATION FROM GPS BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE GPS
!              ATTITUDE CONTROL LAWS.  ALSO, COMPUTE PARTIAL OF SPF
!              ROTATION MATRIX (HCL TO XYZ) WRT SAT STATE AS NEEDED
!              FOR VMATR PARTIALS
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   MJDS     I      S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC     I      S    FRACTIONAL REMAINING SECONDS
!   SATPOS     A      I    SPACECRAFT POSITION IN TRUE OF DATE SYSTEM (M)
!   SATVEL     A      I    SPACECRAFT VELOCITY IN TRUE OF DATE SYSTEM (M/S
!   BFNRM1   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (X COMP.)
!   BFNRM2   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (Y COMP.)
!   BFNRM3   A      I    SPACECRAFT FLAT PLATE UNIT NORMAL VECTORS IN
!                        SPACECRAFT BODY-FIXED SYSTEM (Z COMP.)
!   TDNRM1   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (X COMP.)
!   TDNRM2   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Y COMP.)
!   TDNRM3   A      O    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Z COMP.)
!   CTHETA   A      O    COS OF ANGLE BETWEEN TOD PLATE NORMAL AND
!                        SATELLITE-SUN VECTOR
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!   LFORCE          I    .TRUE. IF GPSATT CALLED FROM F
!                        .FALSE. IF GPSATT CALLED FROM TRKGPS
!
!************ GPSATT LOCAL VARIABLE DEFFINITIONS************************
!   OFFANG   S      W    OFFSET ANGLE TO OBTAIN LOCAL VERTICAL POINTING
!                        TOPEX REFERENCE ELLIPSOID
!   SAROT    A      W    SOLAR ARRAY TO SBF ROTATION MATRIX
!   SBF      A      W    SBF TO TOD ROTATION MATRIX
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   VORB0    A      W    VECTOR DEFINING LOCATION OF 0 DEG ORBIT ANGLE I
!***********************************************************************
!
!   SOMEGA   S           SPACECRAFT ORBIT ANGLE
!   BETAP    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   SBETAP   S           SIGN OF BETAPRIME ANGLE
!   SGAMMA   S           SOLAR ARRAY PITCH ANGLE
!
!***********************************************************************
!
! NOTES:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  rotate to satellite body-fixed frame (x,y,z)
!            SBF = SATELLITE BODY-FIXED FRAME
!                  apply solar array angle rotations
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!         1) SATELLITE POINTS GEOCENTRICALLY
!         2) SATELLLITE BODY FIXED FRAME:
!             X = Y X Z
!             Y = Z X RSUN
!             Z = -R
!
! REFERENCES:
!            FLIEGEL, H.F. AND T.E. GALLINI, "GLOBAL POSITIONING
!            SYSTEM RADIATION FORCE MODEL FOR GEODETIC APPLICATIONS",
!            JGR, VOL 97, NO B1, PP559-568, JAN 10, 1992.
!
!            LICHTEN, S.M.AND J.S. BORDER, "STRATEGIES FOR HIGH-
!            PRECISION GLOBAL POSITIONING SYSTEM ORBIT DETERMINATION",
!            JGR, VOL 92, NO B12,P 12751-12762, NOV 10,1987.
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      LOGICAL NIGHT,NOON
      SAVE
!
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CINTL/LORBIT,LORBVE,LNDRAG,LNSLRD,LDRADJ,LSRADJ,           &
     &             LBACKW,LSETS
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
      COMMON/CORA04/KABIAS,KABSTR,KABSTP,KCBIAS,KCBSTR,KCBSTP,          &
     &       KRNM  ,KRKM  ,KRKJ  ,KRIJ  ,KRKPM ,                        &
     &       KRRNM ,KRRKM ,KRRKJ ,KRRIJ ,KRRKPM,                        &
     &       KURNM ,KURKM ,KURKJ ,KURIJ ,KURKPM,                        &
     &       KPRRNM,KPRRKM,KPRRKJ,KPRRIJ,KPRRKP,                        &
     &       KPMPXI,KPMPXE,KRA   ,KRELV ,KUHAT ,KWORK ,KTMPCR,KU    ,   &
     &       KS0   ,KS1   ,KXTP  ,KPXSXP,KPXDXP,KXUTDT,KRPOLE,KPXPOL,   &
     &       KXDPOL,KOBBUF,KOBSC ,KRESID,KSIGMA,KRATIO,KELEVS,          &
     &       KPMPA ,KPXSLV,KTPRTL,                                      &
     &       KFRQOF,KXYZOF,KFRQST,KFRQSA,KSTDLY,KSADLY,KXYZCG,KSCMRA,   &
     &       KEBVAL,KEBSIG,KEBNAM,KBTWB ,KTBTWA,KBTWD ,KPMPE ,          &
     &       KFSEB1,KFSEB2,KSTAMT,KPXSPA,KDIAGN,KOBOUT,KGRLT1,KGRLT2,   &
     &       KGRLT3,KGRLT4,KGRLT5,KPSTAT,KRSUNS,KCHEMR,KCHMOR,KEPOSR,   &
     &       KCHEMT,KCHMOT,KEPOST,KCHCBS,KCPOSS,KSCRTM,KXSTT ,KXSTR ,   &
     &       KXSS  ,KRANGT,KRANGR,KCHB1 ,KCHB2 ,KCHBV1,KCHBV2,KCHEB ,   &
     &       KSSTFQ,KSSTCC,KSSTSS,KSSTWT,KRLCOR,KWVLBI,KQUINF,KBRTS ,   &
     &       KBRTSV,KLRARC,KXEPBF,KXEPBR,KPRCBD,KPRTBD,KXPMPA,KXL,      &
     &       KXSIG ,KXEDTS,KXALO1,KXALO2,KXYOF2,KDLATF,KDLATS,KDLONF,   &
     &       KDLONS,KPF   ,KPS   ,                                      &
     &       KXOBSV,KXOBSW,KXEDSW,                                      &
     &       KXSIGW,KPSF  ,KDNUM ,KCNUM ,KFCOR ,KCOSAR,KSINAR,KSABIA,   &
     &       KSBTM1,KSBTM2,KYAWBS,KVLOUV,KACMAG,KOBSTR,                 &
     &       KPRL1 ,KPRL2, KRL1  ,KT1SE ,KTCP  ,                        &
     &       KRATDR,KFQT1S,KFQT1E,KT1STR,KFTTSE,KFRSTR,KFRRAT,KSV1  ,   &
     &       KSV2  ,KTSLOV,                                             &
     &       KGLGR1,KGLGR2,KGLFR1,KGLFR2,                               &
     &       KARGR1,KARGR2,KARFR1,KARFR2,                               &
     &       KRDS1L,KFT1AV,KDFQP ,KFREQ1,KFREQ3,KSAVD1,KSAVD2,          &
     &       KANTOU,KFM3CF,KF2CF,KTMG,KLTMG,KX2TIM,KX2OBS,KXRNDX,KX2SCR,&
     &       KALTWV,KXXBM ,KX2PAR,KATIME,KPMPAT,KPMATT,KX2PAT,          &
     &       KPXEXI,KPXEPA,KPV,KPXEP2,KX2COF,KACOF2,KACOF3,KBCOF,KBCOF2,&
     &       KDDDA ,KX2AUX,KX2VPR,KX2VPA,KEXTRA,KVARAY,KATROT,KATPER,   &
     &       KLTAR ,KXHOLD,KANTBL,KPHC  ,KOFDRV,KGNAME,KGRSIZ,KGRCNT,   &
     &       KGRDAT,KACCDT,KCTBTI,KCTBWE,KCTCTM,KANTUV,KANTOR,KW1PU ,   &
     &       KPYSQH,KSIGSP,KXYOF3,KXVTM1,KXDIST,KXDST0,KTMSE ,KDSDP ,   &
     &       KEXCST,KEXCDT,KEXCGX,KEXCGY,KEXCGZ,KIMNDX,KIMOBS,KIMTIM,   &
     &       KIMSAT,KM2VPA,KDCDD ,KDWNWT,KDPOR ,KC2PAR,KBOUNC,KBPART,   &
     &       NXCA04
      COMMON/CORI04/KNMP  ,KINDP ,KNDPAR,KNPVCT,KISATN,KISET ,KIANTO,   &
     &              KISATO,KIANTD,KISATD,KISTAD,KISATC,KMJDCG,KISTAT,   &
     &              KMJDEB,KMJDBN,KNDBIN,KNWTDB,KIXPAR,KNDBUF,KSSTNM,   &
     &              KSSTNA,KMBSAT,KMBSTA,KXKEY ,KXVKEY,KXFLAG,KIPNTF,   &
     &              KIPNTS,KNCON ,KKF   ,KIDATB,KNSTLV,KSLVID,KLLBIA,   &
     &              KTMRA1,KTMRA2,KIND1 ,KTARID,KATARD,KYAWID,KXOBLK,   &
     &              KDSCWV,KATRSQ,KATSAT,KKVLAS,KPARTP,KLTMSC,KLTASC,   &
     &              KCTBST,KSTBST,KANCUT,KANGPS,KANTYP,                 &
     &              KANTOF,                                             &
     &              KYSAT, KMBDEG,                                      &
     &              KMBNOD,KMBSST,KBSPLN,KSSPLN,KXIOBS,KIDLAS,KIAVP ,   &
     &              KXNAVP,KNEXCG,KIDEXC,KNREXC,KTELEO,KIKEY ,KIMKEY,   &
     &              KIMBLK,KIMPRT,KCN110,KCN111,KC110,KC111,KTDSAT,     &
     &              KTDANT,NXCI04
      COMMON/CRMI/RMI(9)
      COMMON/LSTRT/LSTART
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/YAWTOP/YAWLIM(3),XYAWTP
!
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION SATPOS(3),SATVEL(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3)
      DIMENSION ACROSS(3),ACROSSN(3),ALONGN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),SUNY(3),SUN(3),      &
     &          SATSUN(3),SATN(3) ,SUNBETAV(3),SBPERP(3),SUNX(3)
      DIMENSION XTODOUT(3)
      DIMENSION SAROT(3,3),TOTROT(3,3),                                 &
     &          TEMP(3,3),TEMP1(3,3),SPF(3,3),AA(1),II(1),QAT(4)
      DIMENSION SABIAS(NTPBIA),TIMBI1(NTPBIA),TIMBI2(NTPBIA)
      DIMENSION IDATTB(NTPBIA)
      DIMENSION VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
      DIMENSION HELIOS(3),HELION(3)
      DIMENSION ATROT(3,3)
      DIMENSION WPU(3,2),VWND(3,2)
      DIMENSION TEMPVEC(3)
      DIMENSION XTODTEMP(3), YTODTEMP(3)
      DIMENSION SATSTATE(6)
!
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,CM999/-999.0D0/
      DATA ISTART/1/
      DATA AU/149.D9/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE

      IF(ISTART.EQ.1) THEN
      HPI = PI/2.D0
      RADEG = 1.D0/DEGRAD
      CLOSEANGLE = 15.D0*DEGRAD

      ISTART = 0
      ENDIF

      ! EXTRACT IPRN IBLK
      CALL GPSBLOCK(ISATID,ISVN,IPRN,IBLK)

                                              ! TIME was undefined ! jjm
      TIME = MJDS+FSEC
! COMPUTE TOD SAT. TO SUN UNIT VECTOR

      HELIOS(1)=BDTRUE(1,8)
      HELIOS(2)=BDTRUE(2,8)
      HELIOS(3)=BDTRUE(3,8)

      SUNY(1) = HELIOS(1) - SATPOS(1)
      SUNY(2) = HELIOS(2) - SATPOS(2)
      SUNY(3) = HELIOS(3) - SATPOS(3)

      SUNMAG = SQRT(SUNY(1)**2+SUNY(2)**2+SUNY(3)**2)
      SATSUN(1)=SUNY(1)/SUNMAG
      SATSUN(2)=SUNY(2)/SUNMAG
      SATSUN(3)=SUNY(3)/SUNMAG
!
! COMPUTE THE ROTATION FROM THE SBF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X RSUN)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! ZAXIS ROTATION
      ZMAG = SQRT(SATPOS(1)**2+SATPOS(2)**2+SATPOS(3)**2)
      ZTOD(1) =-SATPOS(1)/ZMAG
      ZTOD(2) =-SATPOS(2)/ZMAG
      ZTOD(3) =-SATPOS(3)/ZMAG
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)
!
! YAXIS ROTATION
      YTOD(1) = ZTOD(2)*SATSUN(3)-ZTOD(3)*SATSUN(2)
      YTOD(2) = ZTOD(3)*SATSUN(1)-ZTOD(1)*SATSUN(3)
      YTOD(3) = ZTOD(1)*SATSUN(2)-ZTOD(2)*SATSUN(1)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
      SPF(1,2)= YTOD(1)
      SPF(2,2)= YTOD(2)
      SPF(3,2)= YTOD(3)
!
! XAXIS ROTATION
      XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
      XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
      XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SPF(1,1)= XTOD(1)
      SPF(2,1)= XTOD(2)
      SPF(3,1)= XTOD(3)

      IF (IBLK == 2 .OR. IBLK == 3) THEN
          RD1 = SQRT(SUNY(1)*SUNY(1)+SUNY(2)*SUNY(2)                    &
     &             +SUNY(3)*SUNY(3))
          RD2 = SQRT(SATPOS(1)*SATPOS(1)+SATPOS(2)*SATPOS(2)            &
     &             +SATPOS(3)*SATPOS(3))
          COSE = -SUNY(1)*SATPOS(1)-SUNY(2)*SATPOS(2)-SUNY(3)*SATPOS(3)
          COSE = COSE/(RD1*RD2)
          SINE = SIN(ACOS(COSE))
          ! FORMULA BELOW (INVOLVING ARCSIN) IS SINGULAR WHEN ANGLE E
          ! IS LESS THAN .5013 DEGREES (OR SINE IS LESS THAN .00875)
          IF (SINE < 0.00876D0) THEN
              SINE = 0.00876D0
          END IF
          XBIAS = 0.5D0
          BBIAS = ASIN(.0175D0*XBIAS/SINE)

          COSY = COS(BBIAS)
          SINY = SIN(BBIAS)

          XTODTEMP(1:3) = XTOD(1:3)
          YTODTEMP(1:3) = YTOD(1:3)

          XTOD(1:3) = XTODTEMP(1:3)*COSY + YTODTEMP(1:3)*SINY
          YTOD(1:3) = -XTODTEMP(1:3)*SINY + YTODTEMP(1:3)*COSY

          SPF(1:3,1) = XTOD(1:3)
          SPF(1:3,2) = YTOD(1:3)
      END IF

      IF (IBLK == 4 .OR. IBLK == 5) THEN
          DO I = 1, 3
              XTOD(I) = -XTOD(I)
              YTOD(I) = -YTOD(I)
              SPF(I,1) = -SPF(I,1)
              SPF(I,2) = -SPF(I,2)
          END DO
      END IF
!
! COMPUTE SOLAR ARRAY PITCH ANGLE
      SABTMP=0.0D0
      DO 90 I=1,NTPBIA
        IF((IDSATS.EQ.IDATTB(I)).AND.                                   &
     &  (TIME.GE.TIMBI1(I)).AND.(TIME.LE.TIMBI2(I)))SABTMP=SABIAS(I)
   90 END DO
      COSGAM = ZTOD(1)*SATSUN(1)+ZTOD(2)*SATSUN(2)+ZTOD(3)*SATSUN(3)
      SGAMMA = ACOS(COSGAM)+SABTMP
      COSGAM = COS(SGAMMA)
      SINGAM = SIN(SGAMMA)
!
! COMPUTE ROTATION FOR SOLAR ARRAY
      SAROT(1,1) = COSGAM
      SAROT(1,2) = ZERO
      SAROT(1,3) = SINGAM
      SAROT(2,1) = ZERO
      SAROT(2,2) = ONE
      SAROT(2,3) = ZERO
      SAROT(3,1) = -SINGAM
      SAROT(3,2) = ZERO
      SAROT(3,3) = COSGAM
!
!

!  COMPUTE BETAP ANGLE
!
      CALL VNORM(SUNY,SUN,XHELIOSMOD)
      CALL VNORM(SATPOS,SATN,SATPOSMOD)
      CALL VNORM(HELIOS,HELION,HELIOSMOD)
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
      CALL VPROD(SATN,SATVEL,ACROSS)
      CALL VNORM(ACROSS,ACROSSN,SACROSS)
      CALL VPROD(ACROSSN,SATN,ALONGN)
      CALL SPROD(HELION,ACROSSN,COSALPHA0)
      GPS_BETAP = HPI-ACOS(COSALPHA0)
      COSBETA = COS(GPS_BETAP)
      SINBETA = SIN(GPS_BETAP)
      BETAPDEG = RADEG*GPS_BETAP

! COMPUTE ORBIT ANGLE
      DO I = 1,3
      SUNX(I) = -(HELION(I)-COSALPHA0*ACROSSN(I))
      ENDDO
      CALL VNORM(SUNX,SUNBETAV,SUNXMOD)
      CALL SPROD(SATN,SUNBETAV,COSMU)
      call vprod(sunbetav,acrossn,sbperp)
      call sprod(satn,sbperp,sgnmu)

      GPS_SOMEGA = ACOS(COSMU)*(-SIGN(1.D0,SGNMU))
      SINMU = SIN(GPS_SOMEGA)


! Find if the satellite is in the penumbra or umbra region
! and modify the solar irradiance constant by a variable
! scale factor "S0FACT" accordingly:
!
      CALL SPROD(ZTOD,SUN,COSEPSILON)
      S0FACT = 1.D0
      EPSILONR = ACOS(COSEPSILON)
      RSAT =SATPOSMOD
      IF(ABS(EPSILONR).LT.CLOSEANGLE.and.COSMU.GT.0.D0) THEN
!
      CALL ECLIPSOL(RSAT,XHELIOSMOD,EPSILONR,AREA,S0FACT)
!
      ENDIF
      ARFR=S0FACT
      S0FACT = S0FACT*au**2/XHELIOSMOD**2 ! Taking into account
!                                the true distance satellite to sun "satsunmod".

!******************************************************************
!
! AT THIS POINT CHECK THE BETA ANGLE AND IF IT IS LESS THAN 14 DEG.
! CALL YAWGPS
!     INITIALIZE IYAWER TO 1 = USE NOMINAL IYAWER FLAG IS OUTPUT FROM
!     YAWGPS. IT WILL BE 0 IF WE NEED TO CORRECT THE YAW DUE TO ECLIPSES
      IYAWER = 1
!     INITIALIZE NOMYAW TO 1 = USE NOMINAL YAW
      NOMYAW = 1
      if(ABS(BETAPDEG).lt.14.D0) then
      NOMYAW = 0
      endif

!
!     CALL VNORM(SATPOS,satn,satposmod)

!          write(6,*)' dbg YAWGPS IBLK IPRN ',IBLK,IPRN
!
!
!     COMPUTE TIME: SECONDS OF START OF GPS WEEK.
      CALL MJDGPS(MJDS,FSEC,NWEEK,TTAG,AA,.TRUE.)

! COMPUTE NOMINAL YAW
! YAW = ATAN2(-DTAN(beta),DSIN(ORBMU)) ! (Bar-Sever, 1995)
! with the additional bias term: asin(0.0175D0*0.5D0/SINEPSILON)
! in the case of block IIA satellites.

      YAWN = ATAN2(-TAN(GPS_BETAP),SINMU)
      YAWNOM = RADEG*YAWN
      YAW=YAWNOM
!     IF(IBLK.EQ.2.OR.IBLK.EQ.3) YAWNOM = YAWNOM +      &
!    & ASIN(0.0175D0*0.5D0/SINGAM)*RADEG

        NOPHI=1
        NIGHT=.FALSE.
        NOON=.FALSE.
        INIGHT=0
        INOON=0

       IF(NOMYAW.EQ.0) THEN  ! that is beta angle< 14 deg
      CALL FNDNUM(ISATID,II(KISATN),NSETA,IRETS)
      SATSTATE(1:3) = SATPOS(:)
      SATSTATE(4:6) = SATVEL(:)
      CALL SATECLIPSE(IPRN,TTAG,HELIOS,SATSTATE,IBLK,XTOD,XTODOUT, &
     &YANGLE,PHI,NIGHT,NOON,NOPHI,YBIAS)

         IF(NOON) INOON=1
         IF(NIGHT) INIGHT=1

      DO JJ=1,3
      XTODOUT(JJ)=XTODOUT(JJ)*(1-NOPHI)+XTOD(JJ)*NOPHI
      ENDDO

!olc      new xyz = outxyz*(1-nophi)+xyz*nophi


!        YANGLE   NOMINAL YAW ANGLE (DEGREES).
!        PHI      ACTUAL YAW ANGLE (DEGREES) DURING ECLIPSE MANEUVERS
!                 (SEE NOTE ON BLOCK IIs).
!        XTODOUT  THE BODY-X UNIT VECTOR ROTATED BY (PHI-YANGLE) RETURNED
!        NOON           LOGICAL FLAG SET TO ".TRUE." DURING A NOON MANEUVER.
!        NIGHT          LOGICAL FLAG SET TO ".TRUE." DURING A MIDNIGHT MANEUVER.
!        NOPHI          SIGNALS THAT THE NOMINAL YAW FROM MAIN PROGRAM SHOULD
!                       BE APPLIED (NOPHI=0)
!
!  The correct yaw is: yawangle = PHI-YANGLE+YAWNOM
!  where  yawnnom is the nominal yaw. It may or may not
!  have an extra 180 degree turn for the IIRs (e.g., GEODYN).

      YAWNU = PHI-YANGLE+YAWNOM
      YAW =    YAWNU*(1-NOPHI)+YAWNOM*NOPHI
!      GPS_YAWANG = YAW
      IYAWER=NOPHI

      IF(IYAWER.GT.0) THEN
!     IF(.NOT.LSTART) WRITE(6,*)'dbg GPSATT USE NOM YAW',YAWNOM,BETAPDEG
      ELSE
!     IF(.NOT.LSTART) WRITE(6,*)'dbg GPSATT USE CORR YAW',YAW,BETAPDEG
      XTOD(1) = XTODOUT(1)
      XTOD(2) = XTODOUT(2)
      XTOD(3) = XTODOUT(3)
      SPF(1,1)= XTOD(1)
      SPF(2,1)= XTOD(2)
      SPF(3,1)= XTOD(3)

! CORRECT THE BODY -Y UNIT VECTOR TO MAKE AN ORTHOGONAL SYSTEM

      CALL CROSPR(ZTOD,XTOD,YTOD)

      ENDIF


 2000 CONTINUE

       ENDIF    ! IF(NOMYAW.EQ.0)
!
      GPS_YAWANG = YAW
!
!******************************************************************

!
! COMPUTE TOTAL ROTATION MATRIX FROM SBF TO TOD FRAME
      DO 120 I=1,3
         TEMP(I,1) = ZERO
         TEMP(I,2) = ZERO
         TEMP(I,3) = ZERO

         TOTROT(I,1) = ZERO
         TOTROT(I,2) = ZERO
         TOTROT(I,3) = ZERO
  120 END DO
!
      DO 220 I=1,3
      TOTROT(I,1) = SPF(I,1)
      TOTROT(I,2) = SPF(I,2)
      TOTROT(I,3) = SPF(I,3)
  220 END DO

      IF (LWNDI) THEN
          IF (IUPDN.EQ.1) THEN
              VWND(1,1) = VHIGH(1,1,ISEQ)
              VWND(2,1) = VHIGH(2,1,ISEQ)
              VWND(3,1) = VHIGH(3,1,ISEQ)
              VWND(1,2) = VHIGH(1,2,ISEQ)
              VWND(2,2) = VHIGH(2,2,ISEQ)
              VWND(3,2) = VHIGH(3,2,ISEQ)
          ELSE
              VWND(1,1) = VLOW(1,1,ISEQ)
              VWND(2,1) = VLOW(2,1,ISEQ)
              VWND(3,1) = VLOW(3,1,ISEQ)
              VWND(1,2) = VLOW(1,2,ISEQ)
              VWND(2,2) = VLOW(2,2,ISEQ)
              VWND(3,2) = VLOW(3,2,ISEQ)
          END IF

          DO JJ = 1, 2
              DO III = 1, 3
                  TEMPVEC(III)  = TOTROT(III,1) * VWND(1,JJ)            &
     &                          + TOTROT(III,2) * VWND(2,JJ)            &
     &                          + TOTROT(III,3) * VWND(3,JJ)
              ENDDO
              DO III = 1, 3
                  WPU(III,JJ) = RMI(3*(III-1)+1) * TEMPVEC(1)           &
     &                        + RMI(3*(III-1)+2) * TEMPVEC(2)           &
     &                        + RMI(3*(III-1)+3) * TEMPVEC(3)
              ENDDO
          ENDDO
      END IF

      DO 150 I=1,NFACE
         TDNRM1(I) = ZERO
         TDNRM2(I) = ZERO
         TDNRM3(I) = ZERO
  150 END DO
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
      DO 500 I=1,NFACE-NMOVE
         TDNRM1(I) =                 SPF(1,1)*BFNRM1(I) +               &
     &                               SPF(1,2)*BFNRM2(I) +               &
     &                               SPF(1,3)*BFNRM3(I)
         TDNRM2(I) =                 SPF(2,1)*BFNRM1(I) +               &
     &                               SPF(2,2)*BFNRM2(I) +               &
     &                               SPF(2,3)*BFNRM3(I)
         TDNRM3(I) =                 SPF(3,1)*BFNRM1(I) +               &
     &                               SPF(3,2)*BFNRM2(I) +               &
     &                               SPF(3,3)*BFNRM3(I)
  500 END DO
!
! IF MOVEABLE PLATES EXIST COMPUTE THEIR ROTATIONS
      IF(NMOVE.LE.0) GOTO 850
      DO 600 I=1,3
      TEMP(I,1) = TOTROT(I,1)
      TEMP(I,2) = TOTROT(I,2)
      TEMP(I,3) = TOTROT(I,3)
  600 END DO
!
      DO 700 I=1,3
      TOTROT(I,1) =               TEMP(I,1)*SAROT(1,1)                 &
     &                          + TEMP(I,2)*SAROT(2,1)                 &
     &                          + TEMP(I,3)*SAROT(3,1)
      TOTROT(I,2) =               TEMP(I,1)*SAROT(1,2)                 &
     &                          + TEMP(I,2)*SAROT(2,2)                 &
     &                          + TEMP(I,3)*SAROT(3,2)
      TOTROT(I,3) =               TEMP(I,1)*SAROT(1,3)                 &
     &                          + TEMP(I,2)*SAROT(2,3)                 &
     &                          + TEMP(I,3)*SAROT(3,3)
  700 END DO
!
      DO 650 I=1,3
      TEMP1(I,1) = TOTROT(I,1)
      TEMP1(I,2) = TOTROT(I,2)
      TEMP1(I,3) = TOTROT(I,3)
  650 END DO

      CALL MATPRD(TEMP1,ATROT,TOTROT,3,3,3)

! ROTATE SBF UNIT NORMAL VECTORS(MOVING PLATES) TO TOD FRAME
      DO 800 I=NFACE-NMOVE+1,NFACE
         TDNRM1(I) =                 TOTROT(1,1)*BFNRM1(I)              &
     &                             + TOTROT(1,2)*BFNRM2(I)              &
     &                             + TOTROT(1,3)*BFNRM3(I)
         TDNRM2(I) =                 TOTROT(2,1)*BFNRM1(I)              &
     &                             + TOTROT(2,2)*BFNRM2(I)              &
     &                             + TOTROT(2,3)*BFNRM3(I)
         TDNRM3(I) =                 TOTROT(3,1)*BFNRM1(I)              &
     &                             + TOTROT(3,2)*BFNRM2(I)              &
     &                             + TOTROT(3,3)*BFNRM3(I)
  800 END DO
!
  850 CONTINUE
! COMPUTE UNIT NORMAL VECTORS IN TOD FRAME
      DO 900 I=1,NFACE
         RSUM = SQRT(TDNRM1(I)**2+TDNRM2(I)**2+TDNRM3(I)**2)
         TDNRM1(I) = TDNRM1(I)/RSUM
         TDNRM2(I) = TDNRM2(I)/RSUM
         TDNRM3(I) = TDNRM3(I)/RSUM
  900 END DO
!
! COMPUTE COSINE OF ANGLE BETWEEN TDNRM# AND SATSUN
      DO 950 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*SATSUN(1)+TDNRM2(I)*SATSUN(2)+              &
     &            TDNRM3(I)*SATSUN(3)
  950 END DO
!
! ROTATE BODY-FIXED LOUVER ACCELERATION VECTOR IF NECESSARY
      CAll FNDNUM(IDSATS,ISLVID,NISLV ,IRET)
      IF(IRET.LE.0)GOTO 1000
       KST=NSTLOV(IRET)
       KNST0=0
       DO K=1,IRET
       KNST0=KNST0+NSTLOV(K)
       ENDDO
      IF(KST .GT. ZERO) THEN
       DO 955 INLV=1,KST
       KNST=KNST0+INLV-KST
         TSLOUV(INLV,1) =        TEMP(1,1)*VLOUVS(1,KNST)               &
     &                         + TEMP(1,2)*VLOUVS(2,KNST)               &
     &                         + TEMP(1,3)*VLOUVS(3,KNST)
         TSLOUV(INLV,2) =        TEMP(2,1)*VLOUVS(1,KNST)               &
     &                         + TEMP(2,2)*VLOUVS(2,KNST)               &
     &                         + TEMP(2,3)*VLOUVS(3,KNST)
         TSLOUV(INLV,3) =        TEMP(3,1)*VLOUVS(1,KNST)               &
     &                         + TEMP(3,2)*VLOUVS(2,KNST)               &
     &                         + TEMP(3,3)*VLOUVS(3,KNST)
         RSUM = SQRT(TSLOUV(INLV,1)**2+TSLOUV(INLV,2)**2+               &
     &               TSLOUV(INLV,3)**2)
         TSLOUV(INLV,1) = TSLOUV(INLV,1)/RSUM
         TSLOUV(INLV,2) = TSLOUV(INLV,2)/RSUM
         TSLOUV(INLV,3) = TSLOUV(INLV,3)/RSUM
  955  CONTINUE
       ENDIF
 1000 CONTINUE
!
! OUTPUT GPS TELEM FILE INFORMATION
      IF(LTXPRT .AND. .NOT. LSTART.AND.LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL MJDYMD(MJDS,IYMD,IHMS,4)
         IHM= IHMS/100
         ISEC=IHMS-IHM*100
         RSEC=DBLE(ISEC)
         SEC= FSECU+RSEC
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
         RSATID=DBLE(ISATID)
         RNOPHI=DBLE(NOPHI)
         RNOON=DBLE(INOON)
         RNIGHT=DBLE(INIGHT)
!....PERTINENT ANGLES
         CALL ROTQAT(TEMP,QAT)
         IF(.NOT.LTUM) THEN
         WRITE(97) DIYMD,DIHM,SEC,CM999,GPS_BETAP/DEGRAD, &
     &       GPS_SOMEGA/DEGRAD, &
     &       CM999,SGAMMA/DEGRAD,CM999,QAT(1),QAT(2),QAT(3), &
     &       QAT(4),SATPOS(1),SATPOS(2),SATPOS(3),SATVEL(1),SATVEL(2), &
     &       SATVEL(3),RSATID

       ELSE
         WRITE(97) DIYMD,DIHM,SEC,GPS_BETAP/DEGRAD, &
     &      GPS_SOMEGA/DEGRAD, &
     &      YAWNOM,YAW,ARFR,S0FACT,RNOPHI,RNOON,RNIGHT,YBIAS, &
     &      SATPOS(1),SATPOS(2),SATPOS(3),SATVEL(1),SATVEL(2),&
     &      SATVEL(3),RSATID
!         WRITE(6,'(A)')'GPSATT:  TELEM INFO '

         ! taylor mods
!         WRITE(6,*) DIYMD,DIHM,SEC,GPS_BETAP/DEGRAD, &
!     &           GPS_SOMEGA/DEGRAD,YAWNOM,YAW,&
!     &           ARFR,S0FRAC,RNOPHI,RNOON,RNIGHT,YBIAS, &
!     &           SATPOS(1),SATPOS(2),SATPOS(3),SATVEL(1),SATVEL(2), &
!     &           SATVEL(3),RSATID
         ! orig &           ARFR,S0FRAC,RNOPHI,RNOON,RNIGHT,CM999, &
         ! taylor mods
      ENDIF

      ENDIF
      RETURN
!      if( iret .le. 0 ) then
!  999    WRITE(6,*)' ID DOES NOT MATCH ANY ID in ISLVID ARRAY'
!         STOP
!      else
!         RETURN
!      endif
       END
