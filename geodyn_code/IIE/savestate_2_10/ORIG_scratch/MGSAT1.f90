!$MGSAT1
      SUBROUTINE MGSAT1(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,       &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,                    &
     &                  NFACE,NMOVE,LFORCE,IDSATS,                      &
     &                  VLOUVS,NSTLOV,ISLVID,TSLOUV,AA,ISATID,          &
     &                  EARTH,SUNV,SBF)
!*******************************************************************
!  ROUTINE NAME:   MGSAT1   DATE: 04/07/98      PGMR: F. LEMOINE
!
!   FUNCTION - TO COMPUTE ROTATION FROM MGS BODY-FIXED FRAME TO THE
!              GEODYN TRUE OF REFERENCE FRAME, BASED ON THE MGS
!              ATTITUDE CONTROL LAWS DURING SPO-1  (MARCH 27 to MAY 5, 1
!              IN THIS PHASE THE SPACECRAFT HAS TWO ATTITUDE REGIMES
!               IN AN ORBIT; NADIR POINTING NEAR PERIAPSIS
!               (Z AXIS is nadir; X AXIS in along-track direction)
!              ARRAY NORMAL SPIN:
!               X AXIS (HGA) POINTED AT THE EARTH. S/C SPINNING AT
!               1 REV PER 100 MINUTES.
!  NB: AT PRESENT (04/07/98) S/C ROTATION IS IGNORED
!
! NOTE THAT THIS ROUTINE HAS BEEN MODIFIED FROM ROUTINE MOCATT
!  WRITTEN BY J. ANDREW MARSHALL TO DESCRIBE THE ATTITUDE
!  OF MARS OBSERVER DURING CRUISE.
!C
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   MJDS     I      S    INTEGER EPHEMERIS SECONDS SINCE JED 2430000.5
!   FSEC     I      S    FRACTIONAL REMAINING SECONDS
!   XSAT     A      I    SPACECRAFT POSITION IN MARS INERTIAL IAU SYSTEM
!   VSAT     A      I    SPACECRAFT VELOCITY IN MARS INERTIAL IAU SYSTEM
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
!   LFORCE          I    .TRUE. IF MGSAT1 CALLED FROM F
!                        .FALSE. IF MGSAT1 CALLED FROM TRKMO
!   EARTH    A      I    MARS CENTERED IAU COORDINATED OF EARTH
!   SUN      A      I    MARS CENTERED IAU COORDINATED OF SUN
!
!
!************ MGSAT1 LOCAL VARIABLE DEFFINITIONS************************
!   ACS      A      W    ACS to GCS ROTATION MATRIX
!   GCS      A      W    GCS to SBF ROTATION MATRIX
!   SBF      A      W    SBF TO TOD ROTATION MATRIX
!   XTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   ZTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   PHID     S      I    GROUND SELECTABLE SA SUN-CONE ANGLE (52 deg)
!   ALPHA    S      I    SA GIMBAL LIMIT ANGLE (56 deg)
!   RHO      S      I    HGA REWIND PARAMETER (50.0-57.7 deg)
!   ETA      S      I    HGA GIMBAL LIMIT ANGLE (110 deg)
!   DTNADR   S           TIME SPENT IN NADIR MODE (SECONDS).
!   AEIR     A      W    KEPLER ELEMENTS; ANGLES IN RADIANS
!   PKPXL    A      W    PARTIALS of KEP. ELEM. WRT POS. AND VEL.
!                         (NOT USED).
!
!***********************************************************************
!   SOMEGA   S           SPACECRAFT ORBIT ANGLE
!   BETAP    S           BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE
!   SGAMMA   S           SOLAR ARRAY PITCH ANGLE
!
!***********************************************************************
!
! NOTES:
!            IAU = MARS INERTIAL SYSTEM. THIS MAY BE TRUE OF DATE AS
!                  IN THE FORCE MODEL CALL OR TRUE OF REFERENCE AS
!                  IN THE MEASURMENT MODEL CALLS (ROBSUM,ALTPT)
!                  THE VECTORS XSAT,VSAT,EARTH AND SUN MUST BE
!                  IN THE SAME IAU SYSTEM
!            SBF = SATELLITE BODY-FIXED FRAME
!                  rotate to gimbal frame (assumes nominal HGA deploymen
!                  gimang=45deg)
!            GCS = GIMBAL COORDIANTE SYSTEM
!                  apply rotations about both gimbal axes
!            ACS = ANTENNA COORDINATE SYSTEM
!
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
! This routine does not ignores the rotation of the spacecraft about its
! Yaxis as it has virtually no effect on the projected area or exposed
! faces in the force model.  Simialrly, it can be neglected in the Doppl
! measurement model since the HGA remains in a plane perpendicular to th
! axis of rotation.
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
      COMMON/MGSSBF/SBFMGS(3,3)
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/YAWTOP/YAWLIM(3),XYAWTP
!
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3)
      DIMENSION SBF(3,3),QAT(4),AA(1)
      DIMENSION VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
      DIMENSION EARTH(3),SUNV(3)
      DIMENSION FSEC(1)
!
      DIMENSION AEIR(6), PKPXL(6,6)

!
      PARAMETER(PHID=52.0,ALPHA=56.0,RHO=55.0,ETA=110.0)
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,CM999/-999.0D0/
      DATA DTNADR/1080.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!     if(nmove .gt. 0) then
!     write(6,*) 'NO MOVEABLE PLATES IN CRUISE MODE. CHANGE PANEL CARDS'
!     stop
!     endif
! COMPUTE VECTOR FROM MGS TO EARTH
         RMOEX = -XSAT(1)+EARTH(1)
         RMOEY = -XSAT(2)+EARTH(2)
         RMOEZ = -XSAT(3)+EARTH(3)
         RMAG  = SQRT(RMOEX**2+RMOEY**2+RMOEZ**2)
         RMOEX = RMOEX/RMAG
         RMOEY = RMOEY/RMAG
         RMOEZ = RMOEZ/RMAG
! ROTATE UNIT EARTH VECTOR TO S/C BODY FIXED FRAME
! COMPUTE TOD MGS(MARS) TO SUN UNIT VECTOR
      SUNMAG = SQRT(SUNV(1)**2+SUNV(2)**2+SUNV(3)**2)
      UNTSUN(1)=SUNV(1)/SUNMAG
      UNTSUN(2)=SUNV(2)/SUNMAG
      UNTSUN(3)=SUNV(3)/SUNMAG
! GET THE MEAN ANOMALY; THIS WILL DETERMINE WHICH ATTITUDE MODEL TO
!  APPLY
        IDGET = 1
       CALL ELEM(XSAT(1), XSAT(2), XSAT(3), VSAT(1), VSAT(2), VSAT(3),  &
     &           AEIR, IDGET, PKPXL, GM)
! ORBIT PERIOD IS 11 HRS 38 MINUTES 40 SECS (APPROX)
! NADIR REGIME WILL BE for about +/- 20 MINUTES of PERIAPSIS.
!
! COMPUTE TIME OF FLIGHT; IF ORBIT IS HYPERBOLIC FOR WHATEVER
!  REASON; THIS COMPUTATION WILL BE WRONG!

          IF(AEIR(6)/DEGRAD.GT.270.AND.AEIR(6)/DEGRAD.LE.360.0D0)THEN
            AEIR(6)=AEIR(6)-TWOPI
          ENDIF

          TOF = AEIR(6)/SQRT(GM/(AEIR(1)**3))
! IF TOF LT.DTNADR (18 MINUTES OR 1080 SECONDS), S/C IS NADIR POINTING.
!
      IF(ABS(TOF).LT.DTNADR)THEN

!
! COMPUTE THE ROTATION FROM THE SBF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF MARS
!         YTOD = TOD VECTOR NORMAL TO ORBIT PLANE (Y=Z*VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! Z AXIS ROTATION (NADIR DIRECTION IS -RADIAL)
          ZMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
          ZTOD(1) = -XSAT(1)/ZMAG
          ZTOD(2) = -XSAT(2)/ZMAG
          ZTOD(3) = -XSAT(3)/ZMAG
          SBF(1,3)=ZTOD(1)
          SBF(2,3)=ZTOD(2)
          SBF(3,3)=ZTOD(3)


! YAXIS ROTATION; PARALLEL to ORBIT NORMAL OR ANGULAR MOMENTUM VECTOR
! BUT IN OPPOSITE DIRECTION.
! COMPUTE YTOD    =  -H = -R x V
          YTOD(1) = ZTOD(2)*VSAT(3)-ZTOD(3)*VSAT(2)
          YTOD(2) = ZTOD(3)*VSAT(1)-ZTOD(1)*VSAT(3)
          YTOD(3) = ZTOD(1)*VSAT(2)-ZTOD(2)*VSAT(1)
          YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
          YTOD(1)=YTOD(1)/YMAG
          YTOD(2)=YTOD(2)/YMAG
          YTOD(3)=YTOD(3)/YMAG
          SBF(1,2)=YTOD(1)
          SBF(2,2)=YTOD(2)
          SBF(3,2)=YTOD(3)
! X AXIS ROTATION; TOD TANGENT TO ORBIT PLANE (X=Y X Z)
          XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
          XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
          XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
          XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
          XTOD(1)=XTOD(1)/XMAG
          XTOD(2)=XTOD(2)/XMAG
          XTOD(3)=XTOD(3)/XMAG
          SBF(1,1)=XTOD(1)
          SBF(2,1)=XTOD(2)
          SBF(3,1)=XTOD(3)
        ENDIF
! END IF-THEN BLOCK DESCRIBING NADIR POINTING ATTITUDE REGIME
!
! BEGIN IF-THEN BLOCK DESCRIBING ANS ATTITUDE REGIME
      IF(ABS(TOF).GE.DTNADR)THEN
!
! X-AXIS POINTS AT EARTH DURING ARRAY NORMAL SPIN
!
      XTOD(1) = RMOEX
      XTOD(2) = RMOEY
      XTOD(3) = RMOEZ
      SBF(1,1)= XTOD(1)
      SBF(2,1)= XTOD(2)
      SBF(3,1)= XTOD(3)
!
! ZAXIS ROTATION (Assume Z vector is (1,1,Z1), where Z1 is defined such
!                 that ZTOD dot RMOE = 0)
      Z1=-(RMOEX+RMOEY)/RMOEZ
      ZMAG = SQRT(ONE+ONE+Z1**2)
      ZTOD(1) = ONE/ZMAG
      ZTOD(2) = ZTOD(1)
      ZTOD(3) = Z1/ZMAG
      SBF(1,3)= ZTOD(1)
      SBF(2,3)= ZTOD(2)
      SBF(3,3)= ZTOD(3)
!
! YAXIS ROTATION Y = Z xZ
      YTOD(1) = ZTOD(2)*XTOD(3)-ZTOD(3)*XTOD(2)
      YTOD(2) = ZTOD(3)*XTOD(1)-ZTOD(1)*XTOD(3)
      YTOD(3) = ZTOD(1)*XTOD(2)-ZTOD(2)*XTOD(1)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
      SBF(1,2)= YTOD(1)
      SBF(2,2)= YTOD(2)
      SBF(3,2)= YTOD(3)
      ENDIF
      IF(.NOT.LFORCE) RETURN
      IF(LFORCE) THEN
         SBFMGS(1,1)=SBF(1,1)
         SBFMGS(2,1)=SBF(2,1)
         SBFMGS(3,1)=SBF(3,1)
         SBFMGS(1,2)=SBF(1,2)
         SBFMGS(2,2)=SBF(2,2)
         SBFMGS(3,2)=SBF(3,2)
         SBFMGS(1,3)=SBF(1,3)
         SBFMGS(2,3)=SBF(2,3)
         SBFMGS(3,3)=SBF(3,3)
!!!      RETURN
!!!      08/21/06  !!!!! output the TELEM file info.
           GO TO 1000
       ENDIF
! END IF-THEN BLOCK DESCRIBING ARRAY NORMAL SPIN ATTITUDE REGIME
!
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
      IF(NISLV.LE.0) GO TO 1000
!
! ROTATE BODY-FIXED LOUVER ACCELERATION VECTOR IF NECESSARY
      CAll FNDNUM(IDSATS,ISLVID,NISLV ,IRET)
!     IF(IRET.LE.0)GOTO 999
      IF(IRET.LE.0)GOTO 1000
       KST=NSTLOV(IRET)
       KNST0=0
       DO K=1,IRET
       KNST0=KNST0+NSTLOV(K)
       ENDDO
      IF(KST .GT. ZERO) THEN
       DO 955 INLV=1,KST
       KNST=KNST0+INLV-KST
         TSLOUV(INLV,1) =        SBF(1,1)*VLOUVS(1,KNST)                &
     &                         + SBF(1,2)*VLOUVS(2,KNST)                &
     &                         + SBF(1,3)*VLOUVS(3,KNST)
         TSLOUV(INLV,2) =        SBF(2,1)*VLOUVS(1,KNST)                &
     &                         + SBF(2,2)*VLOUVS(2,KNST)                &
     &                         + SBF(2,3)*VLOUVS(3,KNST)
         TSLOUV(INLV,3) =        SBF(3,1)*VLOUVS(1,KNST)                &
     &                         + SBF(3,2)*VLOUVS(2,KNST)                &
     &                         + SBF(3,3)*VLOUVS(3,KNST)
!        WRITE(6,*)'MGSAT1*VLOUVS=',(VLOUVS(K,INLV,IRET),K=1,3)
!        WRITE(6,*)'MGSAT1*TSLOUV=',(TSLOUV(INLV,K),K=1,3)
         RSUM = SQRT(TSLOUV(INLV,1)**2+TSLOUV(INLV,2)**2+               &
     &               TSLOUV(INLV,3)**2)
         TSLOUV(INLV,1) = TSLOUV(INLV,1)/RSUM
         TSLOUV(INLV,2) = TSLOUV(INLV,2)/RSUM
         TSLOUV(INLV,3) = TSLOUV(INLV,3)/RSUM
  955  CONTINUE
      ENDIF
 1000 CONTINUE
!
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
     &             CM999,CM999,CM999,QAT(1),QAT(2),QAT(3),QAT(4), &
     &             XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),R
         WRITE(97) DIYMD,DIHM,SEC,CM999,BETAP/DEGRAD,SOMEGA/DEGRAD, &
     &             CM999,CM999,CM999,QAT(1),QAT(2),QAT(3),QAT(4), &
     &             XSAT(1),XSAT(2),XSAT(3),VSAT(1),VSAT(2),VSAT(3),R
      ENDIF
      RETURN
  999 WRITE(6,*)'SAT ID DOES NOT MATCH ANY ID in ISLVID ARRAY'
      STOP
      END
