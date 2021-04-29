!$TOPATT
      SUBROUTINE TOPATT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,       &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,                    &
     &                  NFACE,NMOVE,TPSTRT,TPSTOP,ITPMDE,TPFXYW,        &
     &                  LFORCE,TOPXAT,LTPXAT,TPMEAS,LTPMES,SUNXYZ,      &
     &                  IDSATS,IDATTB,SABIAS,TIMBI1,TIMBI2,             &
     &                  YAWBST,ISATNM,VLOUVS,NSTLOV,ISLVID,TSLOUV,      &
     &                  AA,ISATID,TOTROT,IYAWID,ATROT,ITPATS)
!*******************************************************************
!  ROUTINE NAME:   TOPATT   DATE: 08/01/90      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE ROTATION FROM TOPEX BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE TOPEX
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
!   CTHETA   A      O    COS OF ANGLE BETWEEN TOD PLATE NORMAL AND
!                        SATELLITE-SUN VECTOR
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!   NMOVE    S      I    NUMBER OF MOVEABLE FLAT PLATES USED
!   TPSTRT   A      I    YAW STEERING OVERRIDE START TIME
!   TPSTOP   A      I    YAW STEERING OVERRIDE STOP TIME
!   ITPMDE   A      I    YAW STEERING OVERRIDE MODE INDICATOR
!                           1 = SIN
!                           2 = FIX
!                           3 = RAMP UP
!                           4 = RAMP DOWN
!   TPFXYW   A      I    YAW STEERING OVERRIDE FIX YAW ANGLE (MODE=2)
!   LFORCE          I    .TRUE. IF TOPATT CALLED FROM F
!                        .FALSE. IF TOPATT CALLED FROM TRKTOP
!   TOPXAT   A      O    TOPEX ATT. BACK VALUES (BETAPRIME & ORBIT ANGLE
!                           1=BETAPRIME
!                           2=ORBIT ANGLE
!   LTPXAT   A      O    TOPEX ATT. BACK VALUES (RAMPING LOGICALS)
!                           1=LRAMP
!                           2=LRUP
!                           3=LRDOWN
!   TPMEAS   A      I    TOPEX ATT. BACK VALUES (BETAPRIME & ORBIT ANGLE
!                        FOR INTEGRATION STEP PRECEEDING MEASURMENT TIME
!                           1=BETAPRIME
!                           2=ORBIT ANGLE
!   LTPMES   A      I    TOPEX ATT. BACK VALUES (RAMPING LOGICALS)
!                        FOR INTEGRATION STEP PRECEEDING MEASURMENT TIME
!                           1=LRAMP
!                           2=LRUP
!                           3=LRDOWN
!   SUNXYZ   A      I    TOD SUN POS
!   ISATID   S      I    SAT. ID
!   ITPATS   A      I    Unique SAT IDS from TOPATT OPTION
!
!************ TOPATT LOCAL VARIABLE DEFFINITIONS************************
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
!   VALMAX   S      W    VALUE USED TO COMPUTE OFFSET ANGLE AS DICTATED
!                        BY FAIRCHILD DOCUMENT
!***********************************************************************
!
!************ TOPEXA,TOPEXI,TOPEXL COMMON BLOCK DEFINITIONS*************
!   SOMEGA   S           SPACECRAFT ORBIT ANGLE
!   PRVOMG   S           LAST CALCUALTED SPACECRAFT ORBIT ANGLE
!   BETAP    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   PRVBET   S           LAST CALCULATED BETAPRIME ANGLE
!   SBETAP   S           SIGN OF BETAPRIME ANGLE
!   YAWANG   S           SPACECRAFT ROTATION ANGLE ABOUT YAW AXIS
!   YAWLIM   A           YAW LIMITS (BETAPRIME BOUNDARY VALUES, DICTATIN
!                                    YAW ALGORITHM)
!                            1 = BOUNDARY FOR FIXED/SINUSOIDAL YAW AT LO
!                            2 = BOUNDARY FOR 180 DEG YAW FLIPS
!                            3 = BOUNDARY FOR FIXED/SINUSOIDAL YAW AT HI
!   SGAMMA   S           SOLAR ARRAY PITCH ANGLE
!   LRAMP    L           SPACECRAFT IS RAMPING(.TRUE.)
!   LRDOWN   L           SPACECRAFT READY TO RAMP DOWN(.TRUE.)
!   LRUP     L           SPACECRAFT READY TO RAMP UP(.TRUE.)
!   LFIX0    L           SPACECRAFT IN FIXED YAW = 0
!   LFIX90   L           SPACECRAFT IN FIXED YAW = 90
!   LSIN     L           SPACECRAFT IN SINUSOIDAL YAW
!   LTOPX1   L           FIRST CALL TO THIS ROUTINE AFTER STARTER(.TRUE.
!
!***********************************************************************
!
! NOTES:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  rotate to satellite alongtrack, crosstrack, radial sy
!            SPF = SATELLITE ALONGTRACK, CROSSTRACK, RADIAL FRAME
!                  apply offset angle correction for normal pointing to
!            SBF'= SATELLITE BODY-FIXED PRIME FRAME
!                  apply yaw angle rotations
!            SBF = SATELLITE BODY-FIXED FRAME
!                  apply solar array angle rotations
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!         1) VERIFY CORRECT APPLICATION OF OFFSET CORRECTION
!         2) CHECK SOLAR ARRAY PITCH ANGLE (DOES IT UNWIND??)
!         3) APPLY SOLAR ARRAY PITCH BEFORE SPACECRAFT YAW IN SA NORMAL
!            VECTOR ROTATIONS
!         4) ASSUME NOMINAL ALTITUDE OF 1336 KM AND, THEREFORE, A
!            VALMAX OF -2.7720686D-03  FOR OFFSET ANGLE COMPUTATION
!         5) ALL MOVEABLE PLATE INFORMATION STORED AFTER FIXED PLATE INF
!         6) FAIRCHILD PERFORMS CALCULATIONS IN MEAN OF DATE SYSTEM (ECI
!            THIS ROUTINE ASSUMES TRUE OF DATE CALCUALTIOS.  THE EFFECT
!            NUTATION(MEAN OF DATE TO TRUE OF DATE) IS CONSIDERED NEGLIG
!
! REFERENCES:
!            FRIEDER, M., "TOPEX ATTITUDE COMMANDS IN OBC", FAIRCHILD
!            MEMO ACS-87-020, 20 OCTOBER 1987.
!
!            ZIMBELMAN, D., "FINAL VERSION OF TOPEX EULERC SUBROUTINE",
!            FAIRCHILD MEMO GNC:TOPEX:89229, 17 OCTOBER 1989.
!
!            ZIMBELMAN, D., "TOPEX REFERENCE ELLIPSOID AND CALCUALTION
!            OF THE LOCAL VERTICAL OFFSET ANGLE CORRECTION", FAIRCHILD
!            MEMO GNC:TOPEX:90-083A, 19 JUNE 1990.
!
!            PERRYGO, C., "TOPEX SATELLITE YAW MANUEVERS", FAIRCHILD
!            MEMO REF:986:SE:87-074, 11 NOVEMBER 1987.
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER(BUFFER = 1.0D0 * 1.7453292519943296D-2)

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
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/LSTRT/LSTART
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/YAWTOP/YAWLIM(3),XYAWTP
!
      DIMENSION AA(1)
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION SPF(3,3),SBFP(3,3),SBF(3,3),SAROT(3,3),TOTROT(3,3),     &
     &          TEMP(3,3),TEMP1(3,3)
      DIMENSION TPSTRT(NMTPAT,NSATTP), TPSTOP(NMTPAT,NSATTP)
      DIMENSION ITPMDE(NMTPAT,NSATTP), ITPATS(NSATTP)
      DIMENSION SUNXYZ(3), TPFXYW(NMTPAT)
      DIMENSION TOPXAT(2),LTPXAT(3),TPMEAS(2),LTPMES(3)
      DIMENSION QAT(4)
      DIMENSION SABIAS(NTPBIA),TIMBI1(NTPBIA),TIMBI2(NTPBIA)
      DIMENSION YAWBST(MSATYW,MAXYWB,3),IDATTB(NTPBIA)
      DIMENSION ISATNM(NSATID),VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
      DIMENSION IYAWID(1)
      DIMENSION ATROT(3,3),TEMP2(3,3)
!
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
      DATA VALMAX/-2.7720686D-03/
      DATA C54P0/54.0D0/
!
! THE FOLLOWING IS THE CYCLE 6 AND 7 SAPA OVERIDE TIMES HARDCODED
!
      DATA SBTIM1/1636852714.0D0/,SBTIM2/1637344379.0D0/
!
! THE FOLLOWING IS THE CYCLE 33 SAPA OVERIDE TIMES HARDCODED
!
      DATA SBTIM3/1659808682.0D0/,SBTIM4/1659896153.0D0/
!
! THE FOLLOWING IS THE CYCLE 38 SAPA OVERIDE TIMES HARDCODED
!
      DATA SBTIM5/1664491265.0D0/,SBTIM6/1664582460.0D0/
!
! THE FOLLOWING IS THE CYCLE 70 SAPA OVERIDE TIMES HARDCODED
!
      DATA SBTIM7/1691388484.0D0/,SBTIM8/1691405170.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!       print *,'topatt: lforce: ',lforce
! INITIALIZE
      PID2=PI/TWO

! TIME in UTC because overide option times are still in UTC
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
      TIME=MJDS+FSECU
!
      IF(NYWBIA.GT.0) THEN
         YAWBIA=0
         JRET=0
         CALL FNDNUM(IDSATS,IYAWID,MSATYW,IRET)
         IF(IRET.LE.0) GOTO 888
         DO I=1,MAXYWB
            IF(TIME.GT.YAWBST(IRET,I,2).AND.                            &
     &           TIME.LT.YAWBST(IRET,I,3)) THEN
               JRET=I
               EXIT
            ENDIF
         ENDDO
         IF(JRET.LE.0.AND.JRET.GT.MAXYWB) THEN
            GOTO 999
         ELSE
            YAWBIA=YAWBST(IRET,JRET,1)
         ENDIF
      ENDIF
!
  888 CONTINUE
! SAVE INTEGRATION VALUES IF CALLING FROM MEASUREMENT MODEL
! AND INITIALIZE ACCORDINGLY
      IF(.NOT.LFORCE) THEN
         BETASV = BETAP
         PVBTSV = PRVBET
         SOMGSV = SOMEGA
         PROMSV = PRVOMG
         YAWSAV = YAWANG
         SGAMSV = SGAMMA
         LRMPSV = LRAMP
         LRDNSV = LRDOWN
         LRUPSV = LRUP
         LFIXSV = LFIX0
         LFX9SV = LFIX90
         LSINSV = LSIN
!
         PRVBET = TPMEAS(1)
         PRVOMG = TPMEAS(2)
         LRAMP  = LTPMES(1)
         LRUP   = LTPMES(2)
         LRDOWN = LTPMES(3)
!         print *,'prvbet,prvomg,lramp,lrup,lrdown: '
!         print *,prvbet,prvomg,lramp,lrup,lrdown
       ENDIF
! COMPUTE TOD EARTH TO SUN UNIT VECTOR
      SUNMAG = SQRT(SUNXYZ(1)**2+SUNXYZ(2)**2+SUNXYZ(3)**2)
      UNTSUN(1)=SUNXYZ(1)/SUNMAG
      UNTSUN(2)=SUNXYZ(2)/SUNMAG
      UNTSUN(3)=SUNXYZ(3)/SUNMAG
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
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)
!
! YAXIS ROTATION
      YTOD(1) = ZTOD(2)*VSAT(3)-ZTOD(3)*VSAT(2)
      YTOD(2) = ZTOD(3)*VSAT(1)-ZTOD(1)*VSAT(3)
      YTOD(3) = ZTOD(1)*VSAT(2)-ZTOD(2)*VSAT(1)
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
!
! COMPUTE OFFSET ANGLE
      RXY = SQRT(XSAT(1)**2+XSAT(2)**2)
      ALAT = ATAN2(XSAT(3),RXY)
      OFFANG = VALMAX * SIN(TWO*ALAT)
      SINOFF = SIN(OFFANG)
      COSOFF = COS(OFFANG)
!
! COMPUTE ROTATION FROM SBF' TO SPF FRAME
! ROTATE ABOUT VECTOR PERPENDICULAR TO PROJECTION OF POSITION VECTOR (IN
! IN THE EARTH EQUATOR PLANE.  THIS VECTOR DIRECTION IS COMPUTED IN TOD
! THEN NEEDS TO BE ROTATED TO SPF TO USE AS AXIS OF ROTATION TO GO FROM
! SPF .  SINCE THIS VECTOR IS THE ROTATION AXIS IT CAN BE USED AS THE RO
! AXIS TO GO FROM SBF' TO SPF.
!
! NOTE THAT THE FOLLOWING IS THE OPPOSITE DIRECTION OF THE VECTOR DESCRI
! ABOVE SINCE THE OFFSET ANGLE IS NEG. IN THE NORTHERN HEM.
       V0TOD1 = XSAT(2)/RXY
       V0TOD2 =-XSAT(1)/RXY
!
! NOTE THAT UNDER THE FOLLOWING ROTATION THE Z COMPONENET REMAINS ZERO
!
       V01 = SPF(1,1)*V0TOD1+SPF(2,1)*V0TOD2
       V02 = SPF(1,2)*V0TOD1+SPF(2,2)*V0TOD2
       VMAG=SQRT(V01**2+V02**2)
       V01=V01/VMAG
       V02=V02/VMAG
! NSLC is the location of the SAT ID in ITPATS
       IF (NSATTP.LE.0) THEN
         NOVRID=0
       ELSE
         CALL FNDNUM(IDSATS,ITPATS,NSATTP,IRET)
         NSLC=IRET
         IF (IRET.GT.0) THEN
           NOVRID=1
         ELSE
           NOVRID=0
         ENDIF
       ENDIF
! Determine if GEODETIC YAW STEERING
       IF (ITPMDE(NMTPAT,NSLC).EQ.5) THEN
         NGCATT=1
       ELSE
         NGCATT=0
       ENDIF
       GEOPNT=ZERO
      IF(NGCATT.NE.1) THEN
! FORM ROTATION MATRIX TO GO FROM SBFP TO SPF USING UNIT ROTATION AXIS
! CALCULATED ABOVE AND ANGLE OF ROTATION COMPUTED ABOVE
       SBFP(1,1) = COSOFF + (1-COSOFF)*V01*V01
       SBFP(1,2) = V01*V02*(1-COSOFF)
       SBFP(1,3) = -V02*SINOFF
       SBFP(2,1) = SBFP(1,2)
       SBFP(2,2) = COSOFF + (1-COSOFF)*V02*V02
       SBFP(2,3) = V01*SINOFF
       SBFP(3,1) = V02*SINOFF
       SBFP(3,2) = -V01*SINOFF
       SBFP(3,3) = COSOFF
      ELSE
       DO 20 IS=1,NMTPAT
        IF((ITPMDE(IS,NSLC).EQ.5).AND.(TIME.GE.TPSTRT(IS,NSLC)).AND.    &
     &  (TIME.LE.TPSTOP(IS,NSLC))) THEN
         GEOPNT=ONE
         SBFP(1,1) = ONE
         SBFP(1,2) = ZERO
         SBFP(1,3) = ZERO
         SBFP(2,1) = ZERO
         SBFP(2,2) = ONE
         SBFP(2,3) = ZERO
         SBFP(3,1) = ZERO
         SBFP(3,2) = ZERO
         SBFP(3,3) = ONE
         GOTO 30
        ENDIF
   20  CONTINUE
! FORM ROTATION MATRIX TO GO FROM SBFP TO SPF USING UNIT ROTATION AXIS
! CALCULATED ABOVE AND ANGLE OF ROTATION COMPUTED ABOVE
       SBFP(1,1) = COSOFF + (1-COSOFF)*V01*V01
       SBFP(1,2) = V01*V02*(1-COSOFF)
       SBFP(1,3) = -V02*SINOFF
       SBFP(2,1) = SBFP(1,2)
       SBFP(2,2) = COSOFF + (1-COSOFF)*V02*V02
       SBFP(2,3) = V01*SINOFF
       SBFP(3,1) = V02*SINOFF
       SBFP(3,2) = -V01*SINOFF
       SBFP(3,3) = COSOFF
   30  CONTINUE
!....ENDIF FOR NGCATT TEST
      ENDIF
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
! STORE PREVIOUS BETAPRIME AND ORBIT ANGLES
      IF(.NOT.LTOPX1.AND.LFORCE) THEN
       PRVBET = BETAP
       PRVOMG = SOMEGA
      ENDIF
!
! COMPUTE NEW ORBIT ANGLE
!     POSITIVE IN COUNTER-CLOCKWISE DIRECTION WHEN LOOKING DOWN ON ORBIT
!     RELATIVE TO VORB0
      VEC1(1) = VORB0(2)*ZTOD(3)-VORB0(3)*ZTOD(2)
      VEC1(2) = VORB0(3)*ZTOD(1)-VORB0(1)*ZTOD(3)
      VEC1(3) = VORB0(1)*ZTOD(2)-VORB0(2)*ZTOD(1)
      SINOMG  = VEC1(1)*YTOD(1)+VEC1(2)*YTOD(2)+VEC1(3)*YTOD(3)
      COSOMG  = -ZTOD(1)*VORB0(1)-ZTOD(2)*VORB0(2)-ZTOD(3)*VORB0(3)
      SOMEGA = ATAN2(SINOMG,COSOMG)
!
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
      VEC1(1) = VORB0(2)*YTOD(3)-VORB0(3)*YTOD(2)
      VEC1(2) = VORB0(3)*YTOD(1)-VORB0(1)*YTOD(3)
      VEC1(3) = VORB0(1)*YTOD(2)-VORB0(2)*YTOD(1)
      VEC1MG = SQRT(VEC1(1)**2+VEC1(2)**2+VEC1(3)**2)
      VEC1(1) = VEC1(1)/VEC1MG
      VEC1(2) = VEC1(2)/VEC1MG
      VEC1(3) = VEC1(3)/VEC1MG
      VEC2(1) = VEC1(2)*SUNXYZ(3)-VEC1(3)*SUNXYZ(2)
      VEC2(2) = VEC1(3)*SUNXYZ(1)-VEC1(1)*SUNXYZ(3)
      VEC2(3) = VEC1(1)*SUNXYZ(2)-VEC1(2)*SUNXYZ(1)
      SINBET  = VEC2(1)*VORB0(1)+VEC2(2)*VORB0(2)+VEC2(3)*VORB0(3)
      COSBET=VEC1(1)*SUNXYZ(1)+VEC1(2)*SUNXYZ(2)+VEC1(3)*SUNXYZ(3)
      BETAP  = ATAN2(SINBET,COSBET)
!
! IF FIRST CAL TO THIS ROUTINE AFTER STARTER, EQUATE BACK VALUE AND
! CURRENT VALUE
      IF(LTOPX1.AND.LFORCE.AND. .NOT.LSTART) THEN
       PRVBET = BETAP
       PRVOMG = SOMEGA
       LTOPX1=.FALSE.
      ENDIF
!
! COMPUTE YAW ANGLE
!
! IF IN STARTER USE EITHER FIXED OR NOMINAL SIN YAW ONLY
!
      IF(LSTART) THEN
       ABETAP = ABS(BETAP)
!  FIXED + or - 90 DEGREE YAW
       IF(ABETAP.GE.YAWLIM(3)) THEN
        IF(BETAP.GE.ZERO) THEN
         YAWANG = PID2
        ELSE
         YAWANG = -PID2
        ENDIF
        LFIX90=.TRUE.
        LFIX0 =.FALSE.
        LSIN  =.FALSE.
        LRAMP =.FALSE.
        LRUP  =.FALSE.
        LRDOWN=.FALSE.
        GOTO 80
       ENDIF
! FIXED 0 or -180 DEGREE YAW
       IF(ABETAP.LT.YAWLIM(1)) THEN
        IF(BETAP.GE.ZERO) THEN
         YAWANG = ZERO
        ELSE
         YAWANG = -PI
        ENDIF
        LRAMP=.FALSE.
        LFIX90=.FALSE.
        LFIX0=.TRUE.
        LSIN=.FALSE.
        LRUP  =.FALSE.
        LRDOWN=.FALSE.
        GOTO 80
       ENDIF
! NOMINAL SIN YAW
       IF(BETAP.GE.ZERO) THEN
        YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA) + YAWBIA
       ELSE
        YAWANG =-PID2 - (PID2+BETAP)*COS(SOMEGA) + YAWBIA
       ENDIF
       LFIX90=.FALSE.
       LFIX0 =.FALSE.
       LSIN  =.TRUE.
       LRAMP =.FALSE.
       LRUP  =.FALSE.
       LRDOWN=.FALSE.
       GOTO 80
      ENDIF
! ....LSTART
! CHECK FOR NOMINAL YAW ALGORITHM OVERIDE
      IF(NOVRID.LE.0) GOTO 75
!
! FIND APPLICABLE OVERIDE BASED ON START/STOP TIMES
      DO 50 I=1,NMTPAT
      IF(TIME.GE.TPSTRT(I,NSLC) .AND. TIME.LT.TPSTOP(I,NSLC)            &
     &   .AND. ITPMDE(I,NSLC).NE.5) THEN
!
!.......SINUSOIDAL YAW
        IF(ITPMDE(I,NSLC).EQ.1) THEN
           IF(BETAP.GE.ZERO) THEN
              YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA) + YAWBIA
           ELSE
              YAWANG =-PID2 - (PID2+BETAP)*COS(SOMEGA) + YAWBIA
           ENDIF
           LFIX90=.FALSE.
           LFIX0 =.FALSE.
           LSIN  =.TRUE.
           LRAMP =.FALSE.
           LRUP  =.FALSE.
           LRDOWN=.FALSE.
           GOTO 80
        ENDIF
!
!.......FIX YAW
!.......FIX-YAW ANGLE ALREADY SUPPLIED VIA TPFXYW; SPECIFY FIX-YAW HIGH/LOW
!.......REGIME FOR SOLAR ARRAY PITCH ANGLE COMPUTATION
!.......debug check when itpmde=5; if tprxyw ever = +- 90 (npz)
        IF(ITPMDE(I,NSLC).EQ.2) THEN
           YAWANG = TPFXYW(I)
           YAWDEG = TPFXYW(I)/DEGRAD
           IYAWAN = NINT(YAWDEG) ! USE SUPPLIED YAW-ANGLE AS FLAG
           LSIN  =.FALSE.
           IF(IYAWAN.EQ.0 .OR. ABS(IYAWAN).EQ.180) THEN
! FIX-LOW
            LFIX90=.FALSE.
            LFIX0 =.TRUE.
           ENDIF
           IF(ABS(IYAWAN).EQ.90) THEN
! FIX-HIGH
            LFIX90=.TRUE.
            LFIX0 =.FALSE.
           ENDIF
           LRAMP =.FALSE.
           LRUP  =.FALSE.
           LRDOWN=.FALSE.
           GOTO 80
        ENDIF
!
!.......RAMP UP
        IF(ITPMDE(I,NSLC).EQ.3) THEN
           IF(BETAP.GE.ZERO) THEN
                 YAWANG = BETAP/TWO*(ONE-COS(TWO*(SOMEGA+PID2)))        &
     &                    +YAWBIA
           ELSE
                 YAWANG = BETAP/TWO*(ONE-COS(TWO*(SOMEGA+PID2))) - PI   &
     &                    +YAWBIA
           ENDIF
           LFIX90=.FALSE.
           LFIX0 =.FALSE.
           LSIN  =.FALSE.
           LRAMP =.TRUE.
           LRUP  =.TRUE.
           LRDOWN=.FALSE.
           GOTO 80
        ENDIF
!
!.......RAMP DOWN
        IF(ITPMDE(I,NSLC).EQ.4) THEN
           IF(BETAP.GE.ZERO) THEN
                 YAWANG = BETAP/TWO*(ONE-COS(TWO*(SOMEGA+PID2)))        &
     &                    +YAWBIA
           ELSE
                 YAWANG = BETAP/TWO*(ONE-COS(TWO*(SOMEGA+PID2))) - PI   &
     &                    +YAWBIA
           ENDIF
           LFIX90=.FALSE.
           LFIX0 =.FALSE.
           LSIN  =.FALSE.
           LRAMP =.TRUE.
           LRUP  =.FALSE.
           LRDOWN=.TRUE.
           GOTO 80
        ENDIF
!
!.....TIME ENDIF
      ENDIF
!
   50 END DO
!
! APPLY NOMINAL YAW STERRING ALGORITHM
   75 CALL YAW(LRAMP,LRDOWN,LRUP,BETAP,PRVBET,SOMEGA,PRVOMG,            &
     &         YAWLIM,YAWANG,LFIX0,LFIX90,LSIN)
! ADD SPECIFIED YAW BIAS
      YAWANG = YAWANG + YAWBIA
!
   80 COSYAW = COS(YAWANG)
      SINYAW = SIN(YAWANG)
!
! COMPUTE ROTATION FROM SBF TO SBF' FRAMES(ABOUT S/C Z-AXIS (YAW))
      SBF(1,1) = COSYAW
      SBF(1,2) = -SINYAW
      SBF(1,3) = ZERO
      SBF(2,1) = SINYAW
      SBF(2,2) = COSYAW
      SBF(2,3) = ZERO
      SBF(3,1) = ZERO
      SBF(3,2) = ZERO
      SBF(3,3) = ONE
!
! COMPUTE SOLAR ARRAY PITCH ANGLE
!
      DO 90 I=1,NTPBIA
        IF((IDSATS.EQ.IDATTB(I)).AND.                                   &
     &  (TIME.GE.TIMBI1(I)).AND.(TIME.LE.TIMBI2(I)))SABTMP=SABIAS(I)
   90 END DO
!
!
      IF(LFIX0) THEN
         IF(YAWANG.EQ.ZERO)THEN
            SGAMMA = SOMEGA + SABTMP
         ELSE
            SGAMMA = PI - SOMEGA + SABTMP
         ENDIF
      ELSE
         DENOM = COSOMG*COSBET*COSYAW-SINYAW*SINBET
         SGAMMA = PI + ATAN(SINOMG*COSBET/DENOM) + SABTMP
      ENDIF
      COSGAM = COS(SGAMMA)
      SINGAM = SIN(SGAMMA)
!
! COMPUTE ROTATION FROM SBF TO SBF' FOR SOLAR ARRAY
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
! COMPUTE TOTAL ROTATION MATRIX FROM SBF TO TOD FRAME
      DO 100 I=1,3
         TEMP(I,1) = ZERO
         TEMP(I,2) = ZERO
         TEMP(I,3) = ZERO
         TEMP1(I,1) = ZERO
         TEMP1(I,2) = ZERO
         TEMP1(I,3) = ZERO
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
! SAVE SBF to TOD ROTATION MATRIX
      IF(LFORCE) THEN
        DO I=1,3
        DO J=1,3
          TEMP2(I,J) = TOTROT(I,J)
        ENDDO
        ENDDO
      ENDIF
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
!       if(.not.lforce) then
!       print *,'topatt: nface, nmove: ',nface,nmove
!       print *,'topatt:  totrot: '
!       print *,totrot(1,1),totrot(1,2),totrot(1,3)
!       print *,totrot(2,1),totrot(2,2),totrot(2,3)
!       print *,totrot(3,1),totrot(3,2),totrot(3,3)
!       endif
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
!      print *,'bfnrmx i : ',i,bfnrm1(i),bfnrm2(i),bfnrm3(i)
!      print *,'tdnrmx i : ',i,tdnrm1(i),tdnrm2(i),tdnrm3(i)
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
! COMPUTE COSINE OF ANGLE BETWEEN TDNORM AND UNTSUN
      IF(LFORCE) THEN
      DO 950 I=1,NFACE
        CTHETA(I)=TDNRM1(I)*UNTSUN(1)+TDNRM2(I)*UNTSUN(2)+              &
     &            TDNRM3(I)*UNTSUN(3)
  950 END DO
      ENDIF
!
! ROTATE BODY-FIXED LOUVER ACCELERATION VECTOR IF NECESSARY
      CAll FNDNUM(IDSATS,ISLVID,NISLV ,IRET)
      IF(IRET.LE.0)GOTO 1000
      IF(.NOT.LFORCE) GOTO 1000
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
 1000  CONTINUE
!
! OUTPUT TOPEX TELEM FILE INFORMATION
      IF(LTXPRT .AND. .NOT. LSTART .AND. LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDS,FSECU,IYMD,IHM,SEC,1)
         IF(IYMD.LE.999999) GO TO 420
         IYMD=MOD(IYMD,1000000)
         GO TO 430
  420 CONTINUE
  430 CONTINUE
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
         RSATID=DBLE(ISATID)
!....YAW ALGORITHM INDICATOR
         IF(LSIN) THEN
            YTEMP = 1.0D0
         ELSE IF(LFIX0) THEN
            YTEMP = 2.0D0
         ELSE IF(LFIX90) THEN
            YTEMP = 3.0D0
         ELSE IF(LRAMP .AND. LRUP) THEN
            YTEMP = 4.0D0
         ELSE IF(LRAMP .AND. LRDOWN) THEN
            YTEMP = 5.0D0
         ENDIF
!....PERTINENT ANGLES
         CALL ROTQAT(TEMP,QAT)
         WRITE(97) DIYMD,DIHM,SEC,YTEMP,BETAP/DEGRAD,      &
     &             SOMEGA/DEGRAD,(YAWANG)/DEGRAD,(SGAMMA)/DEGRAD, &
     &             GEOPNT,QAT(1),QAT(2),QAT(3),QAT(4),XSAT(1), &
     &             XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),RSATID
      ENDIF
        IF(LFORCE) THEN
! STORE PERTINENT VALUES INTO BACK VALUE ARRAYS
        TOPXAT(1)=BETAP
        TOPXAT(2)=SOMEGA
        LTPXAT(1)=LRAMP
        LTPXAT(2)=LRUP
        LTPXAT(3)=LRDOWN
! RESTORE TOTROT (SBF TO TOD)
          DO I=1,3
          DO J=1,3
            TOTROT(I,J) = TEMP2(I,J)
          ENDDO
          ENDDO
        ENDIF
!
! RETURN FORCE MODEL PARAMTERS TO COMMON BLOCKS
      IF(.NOT.LFORCE) THEN
         BETAP = BETASV
         PRVBET=PVBTSV
         SOMEGA=SOMGSV
         PRVOMG=PROMSV
         YAWANG=YAWSAV
         SGAMMA=SGAMSV
         LRAMP =LRMPSV
         LRDOWN=LRDNSV
         LRUP  =LRUPSV
         LFIX0 =LFIXSV
         LFIX90=LFX9SV
         LSIN  =LSINSV
       ENDIF
      RETURN
  999 WRITE(6,*)'SAT ID DOES NOT MATCH ANY ID in ISATID ARRAY'
      STOP 16
      END
