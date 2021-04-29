!$GFOATT
      SUBROUTINE GFOATT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,       &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,                    &
     &                  NFACE,NMOVE,                                    &
     &                  LFORCE,SUNXYZ,AA,ISATID,ISATN,TOTROT,YAWBST,    &
     &                  ALTPT)
!*******************************************************************
!  ROUTINE NAME:   GFOATT   DATE: 01/22/97      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE ROTATION FROM GFO BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE GFO
!              ATTITUDE CONTROL LAWS.  ALSO, COMPUTE PARTIAL OF SPF
!              ROTATION MATRIX (HCL TO XYZ) WRT SAT STATE AS NEEDED
!              FOR VMATR PARTIALS
!
!              MODIFIED CODE ORIGINALLY WRITTEN FOR GIPSY BY
!              YOAZ BAR-SEVER (JPL:  yeb@cobra.jpl.nasa.gov)
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
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!   LFORCE          I    .TRUE. IF TOPATT CALLED FROM F
!                        .FALSE. IF TOPATT CALLED FROM TRKTOP
!   SUNXYZ   A      I    TOD SUN POS
!   ISATID   S      I    SAT. ID
!
!************ TOPATT LOCAL VARIABLE DEFFINITIONS************************
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   VORB0    A      W    VECTOR DEFINING LOCATION OF 0 DEG ORBIT ANGLE I
!***********************************************************************
!
!************ TOPEXA,TOPEXI,TOPEXL COMMON BLOCK DEFINITIONS*************
!   SOMEGG   S           SPACECRAFT ORBIT ANGLE
!   BETAG    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   YAWAGG   S           SPACECRAFT ROTATION ANGLE ABOUT YAW AXIS
!   LTOPX1   L           FIRST CALL TO THIS ROUTINE AFTER STARTER(.TRUE.
!
!***********************************************************************
! NOTES:
!         SBF =   S/C Body-fixed frame
!                  +X pointed toward RA/WVR Antenna parallel to long
!                     axis of solar array (Prop module/antenna axis)
!                  +Y pointed in general direction of SA, completing ZxX
!                  +Z oriented in general direction of Earth perpendicul
!                     to panel with Doppler beacon antenna and horizon
!                     scanners
!         SPITCH= SBF frame with 10 deg. pitch about SC Y Axis to accoun
!                 for misalignment between s/c Z and altimeter borsite
!                  +X completes Y x Z
!                  +Y pointed in general direction of SA (same as SBF)
!                  +Z altimeter boresite
!         SCYAW = SPITCH Frame with appropriate S/C Yaw about altimeter
!                 boresite to try to keep SA aligned with the SUN
!                  (isn't always aligned)
!                  +X completes Y x Z
!                  +Y SBF Y-axis + Yaw angle
!                     Note yaw angle is not always perfect (ala TOPEX).
!                     For B' <15 ACS can't keep up.
!                     Also applies a -6.1928 deg bias.
!                  +Z Through altimeter boresite (unchanged from SPITCH)
!         NADIR = Geodetic Nadir Frame
!                  +X completes Y x Z
!                  +Y SUN cross Geodetic Nadir
!                  +Z Altimeter boresite pointing geodetic nadir
!         SPF =   Satellite alongtrack, crosstrack, radial frame
!         TOD =   GEODYN TRUE OF DATE INERTIAL FRAME
!
! REFERENCES:
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CINTL/LORBIT,LORBVE,LNDRAG,LNSLRD,LDRADJ,LSRADJ,           &
     &             LBACKW,LSETS
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
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/LSTRT/LSTART
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/STARTT/ESSTRT,FSSTRT
!
      DIMENSION AA(1)
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION CTHETA(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION YN(3),ZN(3),SCX(3),SCY(3),SCZ(3)
      DIMENSION TOTROT(3,3)
      DIMENSION SUNXYZ(3)
      DIMENSION QAT(4)
      DIMENSION ISATN(MSATYW),YAWBST(MSATYW,MAXYWB,3)
      DIMENSION ALTPT(3)
!
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D0/,TEN/10.0D0/
      DATA C54P0/54.0D0/,C80P0/80.0D0/,C90P0/90.0D0/,C15P0/15.0D0/
      DATA C104P0/104.0D0/,C260P0/260.0D0/,C284P0/284.0D0/
      DATA C360P0/360.0D0/
      DATA GFOYWB/-6.1928D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE
      PID2=PI/TWO
      TIME=MJDS+FSEC
      GEOPNT=0.D0
!
!
! COMPUTE THE ROTATION FROM THE SPF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! ZAXIS ROTATION
      ZMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      ZTOD(1) =-XSAT(1)/ZMAG
      ZTOD(2) =-XSAT(2)/ZMAG
      ZTOD(3) =-XSAT(3)/ZMAG
!
! YAXIS ROTATION
      YTOD(1) = ZTOD(2)*VSAT(3)-ZTOD(3)*VSAT(2)
      YTOD(2) = ZTOD(3)*VSAT(1)-ZTOD(1)*VSAT(3)
      YTOD(3) = ZTOD(1)*VSAT(2)-ZTOD(2)*VSAT(1)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
!
! XAXIS ROTATION
      XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
      XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
      XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
!
! COMPUTE NADIR POINTING VECTOR (ZN)
      CALL NADIR(XSAT,ZN)
!
! COMPUTE UNIT VECTOR DEFINING ZERO ORBIT ANGLE (YTOD X SUN)
!
      VORB0(1) = YTOD(2)*SUNXYZ(3)-YTOD(3)*SUNXYZ(2)
      VORB0(2) = YTOD(3)*SUNXYZ(1)-YTOD(1)*SUNXYZ(3)
      VORB0(3) = YTOD(1)*SUNXYZ(2)-YTOD(2)*SUNXYZ(1)
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
      SINOMG  = VEC1(1)*YTOD(1)+VEC1(2)*YTOD(2)+VEC1(3)*YTOD(3)
      COSOMG  = -ZTOD(1)*VORB0(1)-ZTOD(2)*VORB0(2)-ZTOD(3)*VORB0(3)
      SOMEGG = ATAN2(SINOMG,COSOMG)/DEGRAD
      IF(SOMEGG.LT.ZERO) SOMEGG=SOMEGG+C360P0
!
! COMPUTE TOD EARTH TO SUN UNIT VECTOR
      SUNMAG = SQRT(SUNXYZ(1)**2+SUNXYZ(2)**2+SUNXYZ(3)**2)
      UNTSUN(1)=SUNXYZ(1)/SUNMAG
      UNTSUN(2)=SUNXYZ(2)/SUNMAG
      UNTSUN(3)=SUNXYZ(3)/SUNMAG
!
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
      DOT=-YTOD(1)*UNTSUN(1)-YTOD(2)*UNTSUN(2)-YTOD(3)*UNTSUN(3)
      BETAG=C90P0-ACOS(DOT)/DEGRAD
!
! If beta' is less 3 deg., set beta'=3 deg and reorient the
! Earth-Sun vector accordingly.
      IF(ABS(BETAG).LT.THREE) THEN
         BETAG=SIGN(THREE,BETAG)
         ANGLE=C90P0+BETAG
         CALL GENROT(YTOD,VORB0,ANGLE,VEC2)
         VMAG = SQRT(VEC2(1)**2+VEC2(2)**2+VEC2(3)**2)
         UNTSUN(1)=VEC2(1)/VMAG
         UNTSUN(2)=VEC2(2)/VMAG
         UNTSUN(3)=VEC2(3)/VMAG
      ENDIF
!
! COMPUTE Y-AXIS IN NADIR FRAME (SUN X ZN)
      YN(1) = UNTSUN(2)*ZN(3)-UNTSUN(3)*ZN(2)
      YN(2) = UNTSUN(3)*ZN(1)-UNTSUN(1)*ZN(3)
      YN(3) = UNTSUN(1)*ZN(2)-UNTSUN(2)*ZN(1)
      YNMAG = SQRT(YN(1)**2+YN(2)**2+YN(3)**2)
      YN(1)=YN(1)/YNMAG
      YN(2)=YN(2)/YNMAG
      YN(3)=YN(3)/YNMAG
!
! COMPUTE YAW ANGLE CORRECTION FOR SC Y-AXIS DEFINITION
! (BETA' < 15 deg, ORBIT ANGLE=80-104 or 260-284)
      IF(ABS(BETAG).LT.C15P0) THEN
        IF( C80P0.LT.SOMEGG .AND. SOMEGG.LT.C104P0 .OR.                 &
     &     C260P0.LT.SOMEGG .AND. SOMEGG.LT.C284P0) THEN
            CALL GFOYAW(BETAG,SOMEGG,YAW)
!  APPLY YAW CORRECTION THROUGH ROTATION ABOUT ZN
            CALL GENROT(YN,ZN,YAW,YN)
        ENDIF
      ENDIF
!
! COMPUTE Y AXIS in SCYAW/SBF FRAME (ZN x YN)
! (ACCOUNT FOR YAW ANGLE)
      SCY(1) = ZN(2)*YN(3)-ZN(3)*YN(2)
      SCY(2) = ZN(3)*YN(1)-ZN(1)*YN(3)
      SCY(3) = ZN(1)*YN(2)-ZN(2)*YN(1)
      SCYMAG = SQRT(SCY(1)**2+SCY(2)**2+SCY(3)**2)
      SCY(1)=SCY(1)/SCYMAG
      SCY(2)=SCY(2)/SCYMAG
      SCY(3)=SCY(3)/SCYMAG
!
! APPLY ADDITIONAL -6.1928 deg. PLUS USER SPECIFIED YAW BIAS
! (YAW ABOUT ZN)
      IF(NYWBIA.LE.0) GO TO 888
      YAWBIA=0.D0
      JRET=0
      if(gfoywb.ne.-6.1928D0.or.yawbia.ne.zero) then
         write(6,*) 'stop in gfoatt, yaw angle ',gfoywb,yawbia
         stop
      endif
      CALL FNDNUM(ISATID,ISATN,MSATYW,IRET)
      IF(IRET.LE.0) GOTO 888
!     IF(IRET.LE.0) THEN
!       WRITE(6,6000)
!       WRITE(6,6001)
!       STOP
 6000 FORMAT(' EXECUTION TERMINATING IN GFOATT')
 6001 FORMAT(' CAN NOT FIND SAT ID ',I10,' IN SATID ARRAY')
!     ENDIF
      DO I=1,MAXYWB
         IF(TIME.GT.YAWBST(IRET,I,2).AND.                               &
     &        TIME.LT.YAWBST(IRET,I,3)) THEN
            JRET=I
            EXIT
         ENDIF
      ENDDO
      IF(JRET.LE.0.AND.JRET.GT.MAXYWB) THEN
         WRITE(6,6000)
         WRITE(6,6001)
         STOP
      ENDIF
      IF(JRET.GT.0) YAWBIA=YAWBST(IRET,JRET,1)
      YAWAGG=GFOYWB+YAWBIA/DEGRAD
      CALL GENROT(SCY,ZN,YAWAGG,SCY)
  888 CONTINUE
!***************DEBUG***********************************
      if(.not.lstart) then
      scx(1) = scy(2)*zn(3)-scy(3)*zn(2)
      scx(2) = scy(3)*zn(1)-scy(1)*zn(3)
      scx(3) = scy(1)*zn(2)-scy(2)*zn(1)
      scxMAG = SQRT(scx(1)**2+scx(2)**2+scx(3)**2)
      scx(1)=scx(1)/scxMAG
      scx(2)=scx(2)/scxMAG
      scx(3)=scx(3)/scxMAG
      yawtot=ACOS(scx(1)*xtod(1)+scx(2)*xtod(2)                         &
     &           +scx(3)*xtod(3))/degrad
      doty=(scx(1)*ytod(1)+scx(2)*ytod(2)+scx(3)*ytod(3))
      if(doty.lt.zero) yawtot=-yawtot
      endif
!***************DEBUG***********************************
! 888 CONTINUE
!
! APPLY 10 DEG ROATION ABOUT S/C Y AXIS TO GO FROM SPITCH
! TO SBF COORDINATES AFTER SAVING ALTIMETER POINTING
      ALTPT(1)=ZN(1)
      ALTPT(2)=ZN(2)
      ALTPT(3)=ZN(3)
      CALL GENROT(ZN,SCY,TEN,SCZ)
!
! COMPLETE SBF FRAME (SCX = SCY x SCZ)
      SCX(1) = SCY(2)*SCZ(3)-SCY(3)*SCZ(2)
      SCX(2) = SCY(3)*SCZ(1)-SCY(1)*SCZ(3)
      SCX(3) = SCY(1)*SCZ(2)-SCY(2)*SCZ(1)
      SCXMAG = SQRT(SCX(1)**2+SCX(2)**2+SCX(3)**2)
      SCX(1)=SCX(1)/SCXMAG
      SCX(2)=SCX(2)/SCXMAG
      SCX(3)=SCX(3)/SCXMAG
!
!
! COMPUTE TOTAL ROTATION MATRIX FROM SBF TO TOD FRAME
      DO 100 I=1,3
         TOTROT(I,1) = SCX(I)
         TOTROT(I,2) = SCY(I)
         TOTROT(I,3) = SCZ(I)
  100 END DO
!
      DO 150 I=1,NFACE
         TDNRM1(I) = ZERO
         TDNRM2(I) = ZERO
         TDNRM3(I) = ZERO
  150 END DO
!
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
      DO 500 I=1,NFACE-NMOVE
!       if(.not.lforce) then
!       print *,'bfnrm#: ',i,bfnrm1(i),bfnrm2(i),bfnrm3(i)
!       endif
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
      WRITE(6,*) 'STOP IN GFOATT.  NO MOVEABLE PLATES ANTCIPATED'
      STOP
  850 CONTINUE
!
! COMPUTE UNIT NORMAL VECTORS IN TOD FRAME
      DO 900 I=1,NFACE
         RSUM = SQRT(TDNRM1(I)**2+TDNRM2(I)**2+TDNRM3(I)**2)
         TDNRM1(I) = TDNRM1(I)/RSUM
         TDNRM2(I) = TDNRM2(I)/RSUM
         TDNRM3(I) = TDNRM3(I)/RSUM
  900 END DO
!
! REDEFINE SUN VECTOR
      UNTSUN(1)=SUNXYZ(1)/SUNMAG
      UNTSUN(2)=SUNXYZ(2)/SUNMAG
      UNTSUN(3)=SUNXYZ(3)/SUNMAG
! COMPUTE COSINE OF ANGLE BETWEEN TDNORM AND UNTSUN
      IF(LFORCE) THEN
      DO 950 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+              &
     &            TDNRM3(I)*UNTSUN(3)
  950 END DO
!     moda=mod(int(fsstrt),60000)
!     if(.not.lstart.and.moda.eq.0) then
!     if(.not.lstart) then
!     write(79,78) time,betag,somegg,yawtot,(ctheta(ij),ij=1,nface)
!  78 format(f16.3,3f9.3,8f8.4)
!     endif
      ENDIF
!     write(79,78) time,betag,somegg,yawtot,(ctheta(ij),ij=1,nface)
!  78 format(f16.3,3f9.3,8f8.4)
!
! CONVERT BETAG AND SOMEGG BACK TO RADIANS
      BETAG=BETAG*DEGRAD
      SOMEGG=SOMEGG*DEGRAD
!
! OUTPUT TOPEX-LIKE TELEM FILE INFORMATION
      IF(LTXPRT.AND..NOT.LSTART.AND.LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDS,FSECU,IYMD,IHM,SEC,1)
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
         RSATID=DBLE(ISATID)
!....YAW ALGORITHM INDICATOR
            YTEMP = 1.0D0
!....PERTINENT ANGLES
         CALL ROTQAT(TOTROT,QAT)
         WRITE(97) DIYMD,DIHM,SEC,YTEMP,BETAG/DEGRAD,   &
     &             SOMEGG/DEGRAD,YAWTOT,(SGAMMA)/DEGRAD,  &
     &             GEOPNT,QAT(1),QAT(2),QAT(3),QAT(4),XSAT(1), &
     &             XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),RSATID
      ENDIF
      RETURN
  999 WRITE(6,*)'SAT ID DOES NOT MATCH ANY ID in ISATID ARRAY'
      STOP 16
      END
