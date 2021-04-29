!$ICEATT2
      SUBROUTINE ICEATT2(MJDS,FSEC,XSAT,VSAT )

!*******************************************************************
!  ROUTINE NAME:   ICEATT2   DATE: 01/19/03      PGMR: S.B. Luthcke
!
!   FUNCTION - TO COMPUTE ROTATION FROM ICESat S/C COORDINATE FRAME
!              TO THE GEODYN TRUE OF REFERENCE FRAME, BASED ON
!              A SIMPLE REPRESENTATION OF THE ICESat CONTROL LAWS.
!              ALSO, COMPUTE PARTIAL OF SPF
!              ROTATION MATRIX (HCL TO XYZ) WRT SAT STATE AS NEEDED
!              FOR VMATR PARTIALS
!
!   *** NOTE: ICESat HAS TWO SBF FRAMES.  ONE IS THE S/C COORDINATE
!             SYSTEM AND THE OTHER IS THE GLAS OR OPTICAL BENCH
!             COORDINATE SYSTEM.  CURRENTLY THIS ROUTINE CONSIDERS
!             SBF TO BE THE BALL SPACRAFT COORDINATE SYSTEM.
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   MJDS     I      S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC     I      S    FRACTIONAL REMAINING SECONDS
!   XSAT     A      I    SPACECRAFT POSITION IN TRUE OF DATE SYSTEM (M)
!   VSAT     A      I    SPACECRAFT VELOCITY IN TRUE OF DATE SYSTEM (M/S

!************ ICEATT2 LOCAL VARIABLE DEFFINITIONS************************
!   OFFANG   S      W    OFFSET ANGLE TO OBTAIN LOCAL VERTICAL POINTING
!                        TOPEX REFERENCE ELLIPSOID
!   SAROT    A      W    SOLAR ARRAY TO SBF ROTATION MATRIX
!   SBF      A      W    SBF TO SBF' ROTATION MATRIX
!   SPF      A      W    SPF TO TOD ROTATION MATRIX
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   VORB0    A      W    VECTOR DEFINING LOCATION OF 0 DEG ORBIT ANGLE I
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
!                  apply offset angle correction for normal pointing to
!            SBF'= SATELLITE BODY-FIXED PRIME FRAME
!                  apply yaw angle rotations
!            SBF = SATELLITE BODY-FIXED FRAME
!                  apply solar array angle rotations
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!   *** NOTE: ICESat HAS TWO SBF FRAMES.  ONE IS THE S/C COORDINATE
!             SYSTEM AND THE OTHER IS THE GLAS OR OPTICAL BENCH
!             COORDINATE SYSTEM (GCS).  CURRENTLY THIS ROUTINE CONSIDERS
!             SBF TO BE THE GCS.  THIS ROUTINE FIRST COMPUTES SCS TO SPF
!             AND THEN COMPUTES GCS TO SPF ( SCS_2_SPF * GCS_2_SCS)
!             GCS_2_SCS IS CONSIDER TO BE A CONSTANT MATRIX
!
!                          0  0  1
!             GCS_2_SCS =  0 -1  0
!                          1  0  0
!
!             ICESat has two attitude modes:
!             1) "airplane mode" for low Betap <= 32 deg..
!                 In this mode the Xscs and Zgcs axes are pointed anti-n
!                              the Zscs and Xgcs axes are pointed veloci
!                              Yscs and Ygcs complete the orthogonal sys
!             2) "sailboat mode" for high Betap > 32 deg..
!                 In this mode the Xscs and Zgcs axes are pointed anti-n
!                              for now assume:
!                              Yscs pointing velocity when B' > 32 deg.
!                              -Yscs pointing velocity when B' < -32 deg
!                              Xscs complete the orthogonal system
!
!   *** NOTE: THIS ROUTINE ASSUMES GEOCENTRIC POINTING, BUT THE S/C IS R
!             POINTING NADIR
!   *** NOTE: SOLAR ARRAY NORMALS ARE ASSUMED TO BE POINTING TOWARDS SUN
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBDTRU/BDTRUE(7,999)
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
      COMMON/LSTRT/LSTART

      ! >>> jjm 20130313
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      ! <<< jjm 20130313

      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
!
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION SPF(3,3),SAROT(3,3),TOTROT(3,3),                        &
     &          TEMP(3,3),SBF(3,3),AA(1),QAT(4),TEMP2(3,3)
      DIMENSION ATROT(3,3)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,CM999/-999.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE
!
      PID2=PI/TWO
      TIME=MJDS+FSEC
!
! COMPUTE TOD EARTH TO SUN UNIT VECTOR
!
      SUNMAG = SQRT(BDTRUE(1,8)**2+BDTRUE(2,8)**2+BDTRUE(3,8)**2)
      UNTSUN(1)=BDTRUE(1,8)/SUNMAG
      UNTSUN(2)=BDTRUE(2,8)/SUNMAG
      UNTSUN(3)=BDTRUE(3,8)/SUNMAG
!
! COMPUTE THE ROTATION FROM THE SPF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! ZAXIS ROTATION
!
      ZMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      ZTOD(1) =-XSAT(1)/ZMAG
      ZTOD(2) =-XSAT(2)/ZMAG
      ZTOD(3) =-XSAT(3)/ZMAG
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)
!
! YAXIS ROTATION
!
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
!
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
      COSOMG  = -ZTOD(1)*VORB0(1)-ZTOD(2)*VORB0(2)-ZTOD(3)*VORB0(3)
      SOMEGA = ATAN2(SINOMG,COSOMG)
      !write(6,'(A,3(1x,E20.10))') 'ICEATT2: sinomg, cosomg, somega ', &
      !                                     sinomg, cosomg, somega
!
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
!
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

      !write(6,'(A,3(1x,E20.10))') 'ICEATT2: VEC1         ', VEC1(1:3)
      !write(6,'(A,3(1x,E20.10))') 'ICEATT2: VEC2         ', VEC2(1:3)
      !write(6,'(A,3(1x,E20.10))') 'ICEATT2: VORB0        ', VORB0(1:3)
      !write(6,'(A,3(1x,E20.10))') 'ICEATT2: BDTRUE(1:3,8)', BDTRUE(1:3,8
      !write(6,'(A,3(1x,E20.10))') 'ICEATT2: sinbet, cosbet, betap ', &
      !                                      sinbet, cosbet, betap

      RETURN
      END
