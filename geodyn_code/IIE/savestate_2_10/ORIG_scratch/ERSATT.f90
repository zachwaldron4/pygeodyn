!$ERSATT
      SUBROUTINE ERSATT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,       &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,                    &
     &                  NFACE,NMOVE,LFORCE,IDSATS,IDATTB,               &
     &                  SABIAS,TIMBI1,TIMBI2,                           &
     &                  VLOUVS,NSTLOV,ISLVID,TSLOUV,AA,ISATID,TOTROT,   &
     &                  ATROT)
!*******************************************************************
!  ROUTINE NAME:   ERSATT   DATE: 02/26/92      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE ROTATION FROM ERS-1 BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE ERS-1
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
!   XSAT     A      I    SPACECRAFT POSITION IN TRUE OF DATE SYSTEM (M)
!   VSAT     A      I    SPACECRAFT VELOCITY IN TRUE OF DATE SYSTEM (M/S
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
!   CTHETA   A      O    COS OF ANGLE BETWEEN TOD PLATE NORMAL AN@
!                        SATELLITE-SUN VECTOR
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!   LFORCE          I    .TRUE. IF ERSATT CALLED FROM F
!                        .FALSE. IF ERSATT CALLED FROM TRKERS
!
!************ ERSATT LOCAL VARIABLE DEFFINITIONS************************
!   OFFANG   S      W    OFFSET ANGLE TO OBTAIN LOCAL VERTICAL POINTING
!                        TOPEX REFERENCE ELLIPSOID
!   SAROT    A      W    SOLAR ARRAY TO SBF ROTATION MATRIX
!   SBF      A      W    SBF TO SBF' ROTATION MATRIX
!   SBFP     A      W    SBF' TO SPF ROTATION MATRIX
!   SPF      A      W    SPF TO TOD ROTATION MATRIX
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
!                  rotate to satellite alongtrack, crosstrack, radial sy
!            SPF = SATELLITE ALONGTRACK, CROSSTRACK, RADIAL FRAME
!                  apply yaw steering mode rotation
!            SBF'= SATELLITE BODY-FIXED PRIME FRAME
!                  rotate to satellite body-fixed frame (x,y,z)
!            SBF = SATELLITE BODY-FIXED FRAME
!                  apply solar array angle rotations
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!         1) SATELLITE POINTS GEOCENTRICALLY
!         2) SATELLLITE BODY FIXED FRAME:
!             X = ALONG SA AXIS, POSITIVE IN DIRECTION OF SA
!             Y = ALONG VELOCITY,NEGATIVE IN DIRECTION OF VELOCITY
!             Z = ALONG NADAIR, POSITIVE IN DIRECTION OF GEODETIC CENTER
!
! REFERENCES:
!            ZHU, S.Y. AND CH REIGBER,"THE GERMAN PAF FOR ERS-1:  ERS-1
!            STANDARDS USED AT D-PAF," ERS-D-STD-31101, JAN 25, 1991.
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CELEMX/TRUE,ECC
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CINTL/LORBIT,LORBVE,LNDRAG,LNSLRD,LDRADJ,LSRADJ,           &
     &             LBACKW,LSETS
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
      COMMON/LSTRT/LSTART
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/YAWTOP/YAWLIM(3),XYAWTP
!
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3),AEI(6)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION SPF(3,3),SAROT(3,3),TOTROT(3,3),ATROT(3,3),             &
     &          TEMP(3,3),SBF(3,3),SBFP(3,3),AA(1),QAT(4),TEMP1(3,3)
      DIMENSION SABIAS(NTPBIA),TIMBI1(NTPBIA),TIMBI2(NTPBIA)
      DIMENSION IDATTB(NTPBIA)
      DIMENSION VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,CM999/-999.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE
      PID2=PI/TWO
      TIME=MJDS+FSEC
! COMPUTE TOD EARTH TO SUN UNIT VECTOR
      SUNMAG = SQRT(BDTRUE(1,8)**2+BDTRUE(2,8)**2+BDTRUE(3,8)**2)
      UNTSUN(1)=BDTRUE(1,8)/SUNMAG
      UNTSUN(2)=BDTRUE(2,8)/SUNMAG
      UNTSUN(3)=BDTRUE(3,8)/SUNMAG
!
! COMPUTE THE ROTATION FROM THE SPF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR TANGENT TO ORBIT PLANE (Y = Z X X)
!         XTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (X = VSAT X Z)
!
! ZAXIS ROTATION
      ZMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      ZTOD(1) =XSAT(1)/ZMAG
      ZTOD(2) =XSAT(2)/ZMAG
      ZTOD(3) =XSAT(3)/ZMAG
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)
!
! XAXIS ROTATION
      XTOD(1) = VSAT(2)*ZTOD(3)-VSAT(3)*ZTOD(2)
      XTOD(2) = VSAT(3)*ZTOD(1)-VSAT(1)*ZTOD(3)
      XTOD(3) = VSAT(1)*ZTOD(2)-VSAT(2)*ZTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SPF(1,1)= XTOD(1)
      SPF(2,1)= XTOD(2)
      SPF(3,1)= XTOD(3)
!
! YAXIS ROTATION
      YTOD(1) = ZTOD(2)*XTOD(3)-ZTOD(3)*XTOD(2)
      YTOD(2) = ZTOD(3)*XTOD(1)-ZTOD(1)*XTOD(3)
      YTOD(3) = ZTOD(1)*XTOD(2)-ZTOD(2)*XTOD(1)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
      SPF(1,2)= YTOD(1)
      SPF(2,2)= YTOD(2)
      SPF(3,2)= YTOD(3)
!
!
! COMPUTE UNIT VECTOR DEFINING ZERO ORBIT ANGLE (XTOD X SUN)
!
      VORB0(1) = XTOD(2)*BDTRUE(3,8)-XTOD(3)*BDTRUE(2,8)
      VORB0(2) = XTOD(3)*BDTRUE(1,8)-XTOD(1)*BDTRUE(3,8)
      VORB0(3) = XTOD(1)*BDTRUE(2,8)-XTOD(2)*BDTRUE(1,8)
      VORBMG = SQRT(VORB0(1)**2+VORB0(2)**2+VORB0(3)**2)
      VORB0(1) = VORB0(1)/VORBMG
      VORB0(2) = VORB0(2)/VORBMG
      VORB0(3) = VORB0(3)/VORBMG
!
! COMPUTE NEW ORBIT ANGLE
!     POSITIVE IN COUNTER-CLOCKWISE DIRECTION WHEN LOOKING DOWN ON ORBIT
!     RELATIVE TO VORB0
      VEC1(1) = VORB0(2)*ZTOD(3)-VORB0(3)*ZTOD(2)
      VEC1(2) = VORB0(3)*ZTOD(1)-VORB0(1)*ZTOD(3)
      VEC1(3) = VORB0(1)*ZTOD(2)-VORB0(2)*ZTOD(1)
      SINOMG  =-VEC1(1)*XTOD(1)-VEC1(2)*XTOD(2)-VEC1(3)*XTOD(3)
      COSOMG  = ZTOD(1)*VORB0(1)+ZTOD(2)*VORB0(2)+ZTOD(3)*VORB0(3)
      SOMEGA = ATAN2(SINOMG,COSOMG)
!
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
      VEC1(1) = VORB0(2)*XTOD(3)-VORB0(3)*XTOD(2)
      VEC1(2) = VORB0(3)*XTOD(1)-VORB0(1)*XTOD(3)
      VEC1(3) = VORB0(1)*XTOD(2)-VORB0(2)*XTOD(1)
      VEC1MG = SQRT(VEC1(1)**2+VEC1(2)**2+VEC1(3)**2)
      VEC1(1) = VEC1(1)/VEC1MG
      VEC1(2) = VEC1(2)/VEC1MG
      VEC1(3) = VEC1(3)/VEC1MG
      VEC2(1) = VEC1(2)*BDTRUE(3,8)-VEC1(3)*BDTRUE(2,8)
      VEC2(2) = VEC1(3)*BDTRUE(1,8)-VEC1(1)*BDTRUE(3,8)
      VEC2(3) = VEC1(1)*BDTRUE(2,8)-VEC1(2)*BDTRUE(1,8)
      SINBET  = VEC2(1)*VORB0(1)+VEC2(2)*VORB0(2)+VEC2(3)*VORB0(3)
      COSBET=VEC1(1)*BDTRUE(1,8)+VEC1(2)*BDTRUE(2,8)+VEC1(3)*BDTRUE(3,8)
      BETAP  = ATAN2(SINBET,COSBET)
!
! COMPUTE KEPLER ELEMENTS
      CALL ELEM(XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),        &
     &          AEI,1,PKPX,GM)
! COMPUTE "U"SET FOR SBFP ROTATION MATRIX
! U = TRUE ANOMALY+ARG OF PERIGEE = ARG OF LATITUDE
      OFFSET = MOD(AEI(5) + TRUE,TWOPI)
      SINOFF = SIN(OFFSET)
      COSOFF = COS(OFFSET)
! COMPUTE PITCH, ROLL AND YAW ROTATION ANGLES
      PITCH = -0.335*DEGRAD*SINOFF*COSOFF
      ROLL  =  0.050*DEGRAD*SINOFF
      YAW   =  ATAN(0.0683*COSOFF)
      SINPIT = SIN(PITCH)
      COSPIT = COS(PITCH)
      SINROL = SIN(ROLL)
      COSROL = COS(ROLL)
      SINYAW = SIN(YAW)
      COSYAW = COS(YAW)
!
! SBFP = R1(PITCH)*R2(ROLL)*R3(YAW)
      SBFP(1,1) = COSROL*COSYAW
      SBFP(1,2) =-SINPIT*SINROL*COSYAW-COSPIT*SINYAW
      SBFP(1,3) =-COSPIT*SINROL*COSYAW+SINPIT*SINYAW
      SBFP(2,1) = COSROL*SINYAW
      SBFP(2,2) =-SINPIT*SINROL*SINYAW+COSPIT*COSYAW
      SBFP(2,3) =-COSPIT*SINROL*SINYAW-SINPIT*COSYAW
      SBFP(3,1) = SINROL
      SBFP(3,2) = SINPIT*COSROL
      SBFP(3,3) = COSPIT*COSROL
!
! COMPUTE ROTATION FROM SBF TO SPF FRAMES
      SBF(1,1) = -ONE
      SBF(1,2) = ZERO
      SBF(1,3) = ZERO
      SBF(2,1) = ZERO
      SBF(2,2) = -ONE
      SBF(2,3) = ZERO
      SBF(3,1) = ZERO
      SBF(3,2) = ZERO
      SBF(3,3) = ONE
!
! COMPUTE SOLAR ARRAY PITCH ANGLE
      SABTMP = 0.0D0
      DO 90 I=1,NTPBIA
      IF((TIME.GE.TIMBI1(I)).AND.(TIME.LE.TIMBI2(I)).AND.               &
     & (IDSATS.EQ.IDATTB(I)))SABTMP=SABIAS(I)
   90 END DO
      SGAMMA = SOMEGA + SABTMP
      COSGAM = COS(SGAMMA)
      SINGAM = SIN(SGAMMA)
!
! COMPUTE ROTATION FOR SOLAR ARRAY
      SAROT(1,1) = ONE
      SAROT(1,2) = ZERO
      SAROT(1,3) = ZERO
      SAROT(2,1) = ZERO
      SAROT(2,2) = COSGAM
      SAROT(2,3) = SINGAM
      SAROT(3,1) = ZERO
      SAROT(3,2) = -SINGAM
      SAROT(3,3) = COSGAM
!
! COMPUTE TOTAL ROTATION MATRIX FROM SBF TO TOD FRAME
      DO 100 I=1,3
         TEMP(I,1) = ZERO
         TEMP(I,2) = ZERO
         TEMP(I,3) = ZERO
         TOTROT(I,1) = ZERO
         TOTROT(I,2) = ZERO
         TOTROT(I,3) = ZERO
  100 END DO
!
      DO 150 I=1,NFACE
         TDNRM1(I) = ZERO
         TDNRM2(I) = ZERO
         TDNRM3(I) = ZERO
  150 END DO
!
      DO 200 I=1,3
!     DO 200 J=1,3
!     DO 200 K=1,3
! 200 TOTROT(I,J) = TOTROT(I,J) + SPF(I,K)*SBFP(K,J)
      TOTROT(I,1) =               SPF(I,1)*SBFP(1,1)                    &
     &                          + SPF(I,2)*SBFP(2,1)                    &
     &                          + SPF(I,3)*SBFP(3,1)
      TOTROT(I,2) =               SPF(I,1)*SBFP(1,2)                    &
     &                          + SPF(I,2)*SBFP(2,2)                    &
     &                          + SPF(I,3)*SBFP(3,2)
      TOTROT(I,3) =               SPF(I,1)*SBFP(1,3)                    &
     &                          + SPF(I,2)*SBFP(2,3)                    &
     &                          + SPF(I,3)*SBFP(3,3)
  200 END DO
!
!
      DO 300 I=1,3
      TEMP(I,1) = TOTROT(I,1)
      TEMP(I,2) = TOTROT(I,2)
      TEMP(I,3) = TOTROT(I,3)
  300 END DO
!
!
      DO 400 I=1,3
!     DO 400 J=1,3
!     DO 400 K=1,3
! 400 TOTROT(I,J) = TOTROT(I,J) + TEMP(I,K)*SBF(K,J)
      TOTROT(I,1) =               TEMP(I,1)*SBF(1,1)                    &
     &                          + TEMP(I,2)*SBF(2,1)                    &
     &                          + TEMP(I,3)*SBF(3,1)
      TOTROT(I,2) =               TEMP(I,1)*SBF(1,2)                    &
     &                          + TEMP(I,2)*SBF(2,2)                    &
     &                          + TEMP(I,3)*SBF(3,2)
      TOTROT(I,3) =               TEMP(I,1)*SBF(1,3)                    &
     &                          + TEMP(I,2)*SBF(2,3)                    &
     &                          + TEMP(I,3)*SBF(3,3)
  400 END DO
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
      DO 500 I=1,NFACE-NMOVE
         TDNRM1(I) =                 TOTROT(1,1)*BFNRM1(I) +            &
     &                               TOTROT(1,2)*BFNRM2(I) +            &
     &                               TOTROT(1,3)*BFNRM3(I)
         TDNRM2(I) =                 TOTROT(2,1)*BFNRM1(I) +            &
     &                               TOTROT(2,2)*BFNRM2(I) +            &
     &                               TOTROT(2,3)*BFNRM3(I)
         TDNRM3(I) =                 TOTROT(3,1)*BFNRM1(I) +            &
     &                               TOTROT(3,2)*BFNRM2(I) +            &
     &                               TOTROT(3,3)*BFNRM3(I)
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
!     DO 700 J=1,3
!     DO 700 K=1,3
! 700 TOTROT(I,J) = TOTROT(I,J) + TEMP(I,K)*SAROT(K,J)
      TOTROT(I,1) =               TEMP(I,1)*SAROT(1,1)                  &
     &                          + TEMP(I,2)*SAROT(2,1)                  &
     &                          + TEMP(I,3)*SAROT(3,1)
      TOTROT(I,2) =               TEMP(I,1)*SAROT(1,2)                  &
     &                          + TEMP(I,2)*SAROT(2,2)                  &
     &                          + TEMP(I,3)*SAROT(3,2)
      TOTROT(I,3) =               TEMP(I,1)*SAROT(1,3)                  &
     &                          + TEMP(I,2)*SAROT(2,3)                  &
     &                          + TEMP(I,3)*SAROT(3,3)
  700 END DO
!
! BELOW ROTATE FOR ROLL PITCH AND YAW
!
      DO 650 I=1,3
      TEMP1(I,1) = TOTROT(I,1)
      TEMP1(I,2) = TOTROT(I,2)
      TEMP1(I,3) = TOTROT(I,3)
  650 END DO
!
      CALL MATPRD(TEMP1,ATROT,TOTROT,3,3,3)
!
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
! COMPUTE COSINE OF ANGLE BETWEEN TDNRM# AND UNTSUN
      DO 950 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+              &
     &            TDNRM3(I)*UNTSUN(3)
  950 END DO
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
! OUTPUT ERS-1 TELEM FILE INFORMATION
      IF(LTXPRT .AND. .NOT. LSTART.AND.LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDS,FSECU,IYMD,IHM,SEC,1)
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
       RSATID=DBLE(ISATID)
!....PERTINENT ANGLES
         CALL ROTQAT(TEMP,QAT)
         WRITE(97) DIYMD,DIHM,SEC,CM999,BETAP/DEGRAD,SOMEGA/DEGRAD, &
     &             CM999,SGAMMA/DEGRAD,CM999,QAT(1),QAT(2),QAT(3), &
     &             QAT(4),XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2), &
     &             VSAT(3),RSATID
      ENDIF
      RETURN
!      if( IRET .le. 0 ) then
!999     WRITE(6,*)'SAT ID DOES NOT MATCH ANY ID in ISLVID ARRAY'
!        STOP
!      else
        RETURN
!      endif
      END
