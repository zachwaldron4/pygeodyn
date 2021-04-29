!$MOATT
      SUBROUTINE MOATT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,        &
     &              TDNRM1,TDNRM2,TDNRM3,CTHETA,NFACE,NMOVE,LFORCE,     &
     &              IDSATS,IDATTB,SABIAS,TIMBI1,TIMBI2,VLOUVS,          &
     &              NSTLOV,ISLVID,TSLOUV,  AA,ISATID)
!*******************************************************************
!  ROUTINE NAME:   MOATT   DATE: 02/26/92      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE ROTATION FROM MO BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE MO
!              ATTITUDE CONTROL LAWS.
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
!   LFORCE          I    .TRUE. IF MOATT CALLED FROM F
!                        .FALSE. IF MOATT CALLED FROM TRKMO
!
!************ MOATT LOCAL VARIABLE DEFFINITIONS************************
!   ACS      A      W    ACS to GCS ROTATION MATRIX
!   GCS      A      W    GCS to SBF ROTATION MATRIX
!   SBF      A      W    SBF TO TOD ROTATION MATRIX
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   VORB0    A      W    VECTOR DEFINING LOCATION OF 0 DEG ORBIT ANGLE I
!   PHID     S      I    GROUND SELECTABLE SA SUN-CONE ANGLE (52 deg)
!   ALPHA    S      I    SA GIMBAL LIMIT ANGLE (56 deg)
!   RHO      S      I    HGA REWIND PARAMETER (50.0-57.7 deg)
!   ETA      S      I    HGA GIMBAL LIMIT ANGLE (110 deg)
!***********************************************************************
!
!   SOMEGA   S           SPACECRAFT ORBIT ANGLE
!   BETAP    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   SGAMMA   S           SOLAR ARRAY PITCH ANGLE
!
!***********************************************************************
!
! NOTES:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  rotate to satellite alongtrack, crosstrack, radial sy
!            SBF = SATELLITE BODY-FIXED FRAME
!                  rotate to gimbal frame (assumes nominal HGA deploymen
!                  gimang=45deg)
!            GCS = GIMBAL COORDIANTE SYSTEM
!                  apply rotations about both gimbal axes
!            ACS = ANTENNA COORDINATE SYSTEM
!
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!
! REFERENCES:
!          Moyer, T.D., "Offset from Center of Mass to High Gain Antenna
!          Phase Center for Mars Observer", JPL Interoffice Memorandum
!          314.5-1617, May 1, 1992.
!
!          Sirlin, S.W. "An algorithm for Correcting Mars Observer Doppl
!          Measurements, JPL Engineering Memo EM 343-1223, March 19, 199
!
!          "Attitude and Articulation Control System Performance
!           Specification", GE Astro Space Document 49671, PS-2631034, 3
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
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/YAWTOP/YAWLIM(3),XYAWTP
!
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3)
      DIMENSION IDATTB(NTPBIA)
      DIMENSION VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION SBF(3,3),QAT(4),SASBF(3,3),TEMP(3,3),ACS(3,3),          &
     &          HGASBF(3,3)
      DIMENSION AA(1)
      DIMENSION SABIAS(NTPBIA),TIMBI1(NTPBIA),TIMBI2(NTPBIA)
!
!
      PARAMETER(PHID=52.0,ALPHA=56.0,RHO=55.0,ETA=110.0)
      PARAMETER(NMOVE1=2,NMOVE2=2)
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,CM999/-999.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      TIME=MJDS+FSEC
!
! COMPUTE TOD MO(MARS) TO SUN UNIT VECTOR
      SUNMAG = SQRT(BDTRUE(1,8)**2+BDTRUE(2,8)**2+BDTRUE(3,8)**2)
      UNTSUN(1)=BDTRUE(1,8)/SUNMAG
      UNTSUN(2)=BDTRUE(2,8)/SUNMAG
      UNTSUN(3)=BDTRUE(3,8)/SUNMAG
!
! COMPUTE THE ROTATION FROM THE SBF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF MARS
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! ZAXIS ROTATION
      ZMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      ZTOD(1) =-XSAT(1)/ZMAG
      ZTOD(2) =-XSAT(2)/ZMAG
      ZTOD(3) =-XSAT(3)/ZMAG
      SBF(1,3)= ZTOD(1)
      SBF(2,3)= ZTOD(2)
      SBF(3,3)= ZTOD(3)
!
! YAXIS ROTATION
      YTOD(1) = ZTOD(2)*VSAT(3)-ZTOD(3)*VSAT(2)
      YTOD(2) = ZTOD(3)*VSAT(1)-ZTOD(1)*VSAT(3)
      YTOD(3) = ZTOD(1)*VSAT(2)-ZTOD(2)*VSAT(1)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
      SBF(1,2)= YTOD(1)
      SBF(2,2)= YTOD(2)
      SBF(3,2)= YTOD(3)
!
! XAXIS ROTATION
      XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
      XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
      XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SBF(1,1)= XTOD(1)
      SBF(2,1)= XTOD(2)
      SBF(3,1)= XTOD(3)
!
!
! COMPUTE UNIT VECTOR DEFINING ZERO ORBIT ANGLE (YTOD X SUN)
!
      VORB0(1) = YTOD(2)*BDTRUE(3,8)-YTOD(3)*BDTRUE(2,8)
      VORB0(2) = YTOD(3)*BDTRUE(1,8)-YTOD(1)*BDTRUE(3,8)
      VORB0(3) = YTOD(1)*BDTRUE(2,8)-YTOD(2)*BDTRUE(1,8)
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
      COSOMG  =-ZTOD(1)*VORB0(1)-ZTOD(2)*VORB0(2)-ZTOD(3)*VORB0(3)
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
      VEC2(1) = VEC1(2)*BDTRUE(3,8)-VEC1(3)*BDTRUE(2,8)
      VEC2(2) = VEC1(3)*BDTRUE(1,8)-VEC1(1)*BDTRUE(3,8)
      VEC2(3) = VEC1(1)*BDTRUE(2,8)-VEC1(2)*BDTRUE(1,8)
      SINBET  = VEC2(1)*VORB0(1)+VEC2(2)*VORB0(2)+VEC2(3)*VORB0(3)
      COSBET=VEC1(1)*BDTRUE(1,8)+VEC1(2)*BDTRUE(2,8)+VEC1(3)*BDTRUE(3,8)
      BETAP  = ATAN2(SINBET,COSBET)
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
      DO 100 I=1,NFACE-NMOVE
         TDNRM1(I) =                 SBF(1,1)*BFNRM1(I) +               &
     &                               SBF(1,2)*BFNRM2(I) +               &
     &                               SBF(1,3)*BFNRM3(I)
         TDNRM2(I) =                 SBF(2,1)*BFNRM1(I) +               &
     &                               SBF(2,2)*BFNRM2(I) +               &
     &                               SBF(2,3)*BFNRM3(I)
         TDNRM3(I) =                 SBF(3,1)*BFNRM1(I) +               &
     &                               SBF(3,2)*BFNRM2(I) +               &
     &                               SBF(3,3)*BFNRM3(I)
  100 END DO
!
      IF(.NOT.LFORCE .OR. NMOVE .EQ. 0) GOTO 99
!
! COMPUTE SOLAR ARRAY CONTRIBUTION
      IF(NMOVE1 .GT. 0) THEN
! ROTATE UNIT SUN VECTOR TO S/C BODY FIXED FRAME
!   (VEC(BODY-FRAME)=(SBF)T * VEC(INERTIAL)) (T=transpose)
        XSUNB=SBF(1,1)*UNTSUN(1)+SBF(2,1)*UNTSUN(2)+SBF(3,1)*UNTSUN(3)
        YSUNB=SBF(1,2)*UNTSUN(1)+SBF(2,2)*UNTSUN(2)+SBF(3,2)*UNTSUN(3)
        ZSUNB=SBF(1,3)*UNTSUN(1)+SBF(2,3)*UNTSUN(2)+SBF(3,3)*UNTSUN(3)
        SMAG=SQRT(XSUNB**2+YSUNB**2+ZSUNB**2)
        XSUNB=XSUNB/SMAG
        YSUNB=YSUNB/SMAG
        ZSUNB=ZSUNB/SMAG
!
! COMPUTE SA CLOCK ANGLE
        THETAS = ATAN2(XSUNB,ZSUNB)
!
! SUBTRACT OFFSET BIAS (NOMINALLY ZERO) SPECIFIED ON TOPBIA CARD
! (USING CONVENTION on PAGE 189 of AACS PERF. SPEC. DOC.)
      SABTMP=0.0D0
      DO 190 I=1,NTPBIA
      IF((TIME.GE.TIMBI1(I)).AND.(TIME.LE.TIMBI2(I)))SABTMP=SABIAS(I)
  190  CONTINUE
        THETAS=THETAS-SABTMP
!
!
! USE CLOCK AND CONE ANGLES TO GET SA BODY FIXED VECTOR
        SPHID = SIN(DEGRAD*PHID)
        CPHID = COS(DEGRAD*PHID)
        SAX = SIN(THETAS)*SPHID
        SAY = CPHID
        SAZ = COS(THETAS)*SPHID
        RSUM = SQRT(SAX**2+SAY**2+SAZ**2)
        SAX=SAX/RSUM
        SAY=SAY/RSUM
        SAZ=SAZ/RSUM
!
! VERIFY GIMBAL ANGLES DO NOT EXCEED MAXIMUM
        CALPHA=COS(DEGRAD*ALPHA)
        IF(SAZ.GT.CALPHA) THEN
           COEF=SQRT(ONE-CALPHA**2)/SQRT(ONE-SAZ**2)
           SAZ=CALPHA
           SAX=SAX*COEF
           SAY=SAY*COEF
           RSUM = SQRT(SAX**2+SAY**2+SAZ**2)
           SAX=SAX/RSUM
           SAY=SAY/RSUM
           SAZ=SAZ/RSUM
        ENDIF
!
! COMPUTE GIMBAL ANGLES
        SAGD1 = ATAN2(SAX,SAY)
        SAGD2 =  ASIN(SAZ)
!
! COMPUTE ROTATION FROM SA to SBF FRAME
        SINGD1 = SAX
        COSGD1 = SAY
        SINGD2 = SAZ
        COSGD2 = ACOS(SAZ)
        SASBF(1,1) = SINGD1*COSGD2
        SASBF(1,2) = -COSGD1
        SASBF(1,3) = -SINGD1*SINGD2
        SASBF(2,1) = COSGD1*COSGD2
        SASBF(2,2) = SINGD1
        SASBF(2,3) = -COSGD1*SINGD2
        SASBF(3,1) = SINGD2
        SASBF(3,2) = ZERO
        SASBF(3,3) = ZERO
!
! COMPUTE ROTATION FROM SA to TOD FRAME
        TEMP(1,1)=SBF(1,1)*SASBF(1,1)+SBF(1,2)*SASBF(2,1)               &
     &           +SBF(1,3)*SASBF(3,1)
        TEMP(1,2)=SBF(1,1)*SASBF(1,2)+SBF(1,2)*SASBF(2,2)               &
     &           +SBF(1,3)*SASBF(3,2)
        TEMP(1,3)=SBF(1,1)*SASBF(1,3)+SBF(1,2)*SASBF(2,3)               &
     &           +SBF(1,3)*SASBF(3,3)
        TEMP(2,1)=SBF(2,1)*SASBF(1,1)+SBF(2,2)*SASBF(2,1)               &
     &           +SBF(2,3)*SASBF(3,1)
        TEMP(2,2)=SBF(2,1)*SASBF(1,2)+SBF(2,2)*SASBF(2,2)               &
     &           +SBF(2,3)*SASBF(3,2)
        TEMP(2,3)=SBF(2,1)*SASBF(1,3)+SBF(2,2)*SASBF(2,3)               &
     &           +SBF(2,3)*SASBF(3,3)
        TEMP(3,1)=SBF(3,1)*SASBF(1,1)+SBF(3,2)*SASBF(2,1)               &
     &           +SBF(3,3)*SASBF(3,1)
        TEMP(3,2)=SBF(3,1)*SASBF(1,2)+SBF(3,2)*SASBF(2,2)               &
     &           +SBF(3,3)*SASBF(3,2)
        TEMP(3,3)=SBF(3,1)*SASBF(1,3)+SBF(3,2)*SASBF(2,3)               &
     &           +SBF(3,3)*SASBF(3,3)
! ROTATE UNIT NORMAL VECTORS(SA MOVING PLATES) TO TOD FRAME
        DO 300 I=NFACE-NMOVE+1,NFACE-NMOVE+NMOVE1
         TDNRM1(I) =                 TEMP(1,1)*BFNRM1(I) +              &
     &                               TEMP(1,2)*BFNRM2(I) +              &
     &                               TEMP(1,3)*BFNRM3(I)
         TDNRM2(I) =                 TEMP(2,1)*BFNRM1(I) +              &
     &                               TEMP(2,2)*BFNRM2(I) +              &
     &                               TEMP(2,3)*BFNRM3(I)
         TDNRM3(I) =                 TEMP(3,1)*BFNRM1(I) +              &
     &                               TEMP(3,2)*BFNRM2(I) +              &
     &                               TEMP(3,3)*BFNRM3(I)
  300 END DO
      ENDIF
! END SOLAR ARRAY COMPUTATION
!
! COMPUTE HGA CONTRIBUTION
      IF(NMOVE2 .GT. 0) THEN
! COMPUTE VECTOR FROM MO TO EARTH
         RMOEX = -XSAT(1)+BDTRUE(1,9)
         RMOEY = -XSAT(2)+BDTRUE(2,9)
         RMOEZ = -XSAT(3)+BDTRUE(3,9)
         RMAG  = SQRT(RMOEX**2+RMOEY**2+RMOEZ**2)
         RMOEX = RMOEX/RMAG
         RMOEY = RMOEY/RMAG
         RMOEZ = RMOEZ/RMAG
! ROTATE UNIT EARTH VECTOR TO S/C BODY FIXED FRAME
!   (VEC(BODY-FRAME)=(SBF)T * VEC(INERTIAL)) (T=transpose)
        XEARB=SBF(1,1)*RMOEX+SBF(2,1)*RMOEY+SBF(3,1)*RMOEZ
        YEARB=SBF(1,2)*RMOEX+SBF(2,2)*RMOEY+SBF(3,2)*RMOEZ
        ZEARB=SBF(1,3)*RMOEX+SBF(2,3)*RMOEY+SBF(3,3)*RMOEZ
        RMAG = SQRT(XEARB**2+YEARB**2+ZEARB**2)
        XEARB=XEARB/RMAG
        YEARB=YEARB/RMAG
        ZEARB=ZEARB/RMAG
!
! CHECK FOR REWIND
        CRHO=COS(DEGRAD*RHO)
        IF(ZEARB.GT.CRHO) THEN
           XEARB=SQRT(ONE-YEARB**2-CRHO**2)
           ZEARB=CRHO
        ENDIF
! CHECK FOR GIMBAL ANGLE CONSTRAINT
        CETA=COS(DEGRAD*ETA)
        IF(YEARB.LT.CETA) THEN
           SETA=SIN(DEGRAD*ETA)
           VAL=SETA/SQRT(XEARB**2+ZEARB**2)
           XEARB=XEARB*VAL
           YEARB=CETA
           ZEARB=ZEARB*VAL
        ENDIF
        RMAG = SQRT(XEARB**2+YEARB**2+ZEARB**2)
        XEARB=XEARB/RMAG
        YEARB=YEARB/RMAG
        ZEARB=ZEARB/RMAG
!
! COMPUTE GIMBAL ANGLES
        HGAGD1 =  ATAN2(XEARB,-YEARB)
        HGAGD2 =  ASIN(-ZEARB)
!
         SINB = -ZEARB
         COSB =  SQRT(ONE-SINB**2)
         SINA =  XEARB
         COSA = -YEARB
!
! COMPUTE ROTATION FROM ANTENNA COORD TO GIMBAL COORD.
         ACS(1,1) =  COSA
         ACS(1,2) = -SINA*COSB
         ACS(1,3) =  SINA*SINB
         ACS(2,1) =  SINA
         ACS(2,2) =  COSA*COSB
         ACS(2,3) = -COSA*SINB
         ACS(3,1) =  ZERO
         ACS(3,2) =  SINB
         ACS(3,3) =  COSB
!
! COMPUTE THE ROTATION FROM THE GCS TO SBF FRAME
! ***NOTE: NOMINAL GIMBAL DEPLOYMENT ANGLE IS 45 deg (21+24, from Ref 2,
! ***Fig 4) THIS MUST BE CHECKED AGAINST ACTUAL DEPLOYMENT
         GIMANG =  45.0D0 * DEGRAD
         COSGIM = COS(GIMANG)
         SINGIM = SIN(GIMANG)
         TEMP(1,1) = -ONE
         TEMP(2,1) = ZERO
         TEMP(3,1) = ZERO
         TEMP(1,2) = ZERO
         TEMP(2,2) = -COSGIM
         TEMP(3,2) = SINGIM
         TEMP(1,3) = ZERO
         TEMP(2,3) = SINGIM
         TEMP(3,3) = COSGIM
!
! FORM ROTATION FROM ANTENNA COORD TO BODY-FIXED COORD
        DO 200 J=1,3
        HGASBF(J,1) =   TEMP(J,1)*ACS(1,1)                              &
     &                + TEMP(J,2)*ACS(2,1)                              &
     &                + TEMP(J,3)*ACS(3,1)
        HGASBF(J,2) =   TEMP(J,1)*ACS(1,2)                              &
     &                + TEMP(J,2)*ACS(2,2)                              &
     &                + TEMP(J,3)*ACS(3,2)
        HGASBF(J,3) =   TEMP(J,1)*ACS(1,3)                              &
     &                + TEMP(J,2)*ACS(2,3)                              &
     &                + TEMP(J,3)*ACS(3,3)
  200 END DO
!
! COMPUTE ROTATION FROM SA to TOD FRAME
        TEMP(1,1)=SBF(1,1)*HGASBF(1,1)+SBF(1,2)*HGASBF(2,1)             &
     &           +SBF(1,3)*HGASBF(3,1)
        TEMP(1,2)=SBF(1,1)*HGASBF(1,2)+SBF(1,2)*HGASBF(2,2)             &
     &           +SBF(1,3)*HGASBF(3,2)
        TEMP(1,3)=SBF(1,1)*HGASBF(1,3)+SBF(1,2)*HGASBF(2,3)             &
     &           +SBF(1,3)*HGASBF(3,3)
        TEMP(2,1)=SBF(2,1)*HGASBF(1,1)+SBF(2,2)*HGASBF(2,1)             &
     &           +SBF(2,3)*HGASBF(3,1)
        TEMP(2,2)=SBF(2,1)*HGASBF(1,2)+SBF(2,2)*HGASBF(2,2)             &
     &           +SBF(2,3)*HGASBF(3,2)
        TEMP(2,3)=SBF(2,1)*HGASBF(1,3)+SBF(2,2)*HGASBF(2,3)             &
     &           +SBF(2,3)*HGASBF(3,3)
        TEMP(3,1)=SBF(3,1)*HGASBF(1,1)+SBF(3,2)*HGASBF(2,1)             &
     &           +SBF(3,3)*HGASBF(3,1)
        TEMP(3,2)=SBF(3,1)*HGASBF(1,2)+SBF(3,2)*HGASBF(2,2)             &
     &           +SBF(3,3)*HGASBF(3,2)
        TEMP(3,3)=SBF(3,1)*HGASBF(1,3)+SBF(3,2)*HGASBF(2,3)             &
     &           +SBF(3,3)*HGASBF(3,3)
! ROTATE SBF UNIT NORMAL VECTORS(HGA MOVING PLATES) TO TOD FRAME
        DO 400 I=NFACE-NMOVE2+1,NFACE
         TDNRM1(I) =                 TEMP(1,1)*BFNRM1(I) +              &
     &                               TEMP(1,2)*BFNRM2(I) +              &
     &                               TEMP(1,3)*BFNRM3(I)
         TDNRM2(I) =                 TEMP(2,1)*BFNRM1(I) +              &
     &                               TEMP(2,2)*BFNRM2(I) +              &
     &                               TEMP(2,3)*BFNRM3(I)
         TDNRM3(I) =                 TEMP(3,1)*BFNRM1(I) +              &
     &                               TEMP(3,2)*BFNRM2(I) +              &
     &                               TEMP(3,3)*BFNRM3(I)
  400 END DO
      ENDIF
! END HGA PROCESSING
!
!
   99 CONTINUE
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
 1000  CONTINUE
! OUTPUT MO TELEM FILE INFORMATION
      IF(LTXPRT .AND. .NOT. LSTART.AND.LFORCE) THEN
!....INTEGRATION STEP TIME
         CALL UTCET(.FALSE.,1,MJDS,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDS,FSECU,IYMD,IHM,SEC,1)
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)
         RSATID=DBLE(ISATID)
!....PERTINENT ANGLES
         CALL ROTQAT(SBF,QAT)
         ! jjm 20120531 "R" is undefined
         WRITE(97) DIYMD,DIHM,SEC,CM999,BETAP/DEGRAD,SOMEGA/DEGRAD, &
     &             CM999,CM999,CM999,QAT(1),QAT(2),QAT(3),QAT(4),&
     &             XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),R
         WRITE(97) DIYMD,DIHM,SEC,CM999,BETAP/DEGRAD,SOMEGA/DEGRAD, &
     &             CM999,CM999,CM999,QAT(1),QAT(2),QAT(3),QAT(4),&
     &             XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),R
      ENDIF
  999 RETURN
      END
