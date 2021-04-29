
!$GRLATT
      SUBROUTINE GRLATT(MJDS,FSEC,XSAT,VSAT,BFNRM1,BFNRM2,BFNRM3,       &
     &                  TDNRM1,TDNRM2,TDNRM3,CTHETA,NFACE,NMOVE,        &
     &                  LFORCE,IDSATS,IDATTB,SABIAS,TIMBI1,TIMBI2,      &
     &                  VLOUVS,NSTLOV,ISLVID,TSLOUV,AA,ISATID,          &
     &                  TOTROT,ATROT)
!*******************************************************************
!  ROUTINE NAME:   GRLATT   DATE: 11/06/30      PGMR: M. BLACKBURN
!
!   FUNCTION - TO COMPUTE ROTATION FROM GRAIL 1 and 2 BODY-FIXED FRAME
!              TO THE GEODYN TRUE OF REFERENCE FRAME, BASED ON GRAIL
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
!
!************ GRLATT LOCAL VARIABLE DEFFINITIONS************************
!   OFFANG   S      W    OFFSET ANGLE TO OBTAIN LOCAL VERTICAL POINTING
!                        TOPEX REFERENCE ELLIPSOID
!   SAROT    A      W    SOLAR ARRAY TO SBF ROTATION MATRIX
!   SBF      A      W    SBF TO SBF' ROTATION MATRIX
!   SPF      A      W    SPF TO TOD ROTATION MATRIX
!   XTOD     A      W    TOD VECTOR NORMAL TO ORBIT PLANE
!   YTOD     A      W    TOD VECTOR FROM SPACECRAFT TO EARTH CENTER
!   ZTOD     A      W    TOD VECTOR TANGENT TO ORBIT PLANE
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
!
!         1) SATELLITE POINTS GEOCENTRICALLY
!         2) SATELLLITE BODY FIXED FRAME:
!             X = ALONG SA AXIS, POSITIVE IN DIRECTION OF SA
!             Y = ALONG NADIR, POSITIVE IN DIRECTION OF GEOCENTER
!             Z = ALONG VELOCITY,POSITIVE IN DIRECTION OF VELOCITY
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CBDTRU/BDTRUE(7,999)
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
      COMMON/LSTRT/LSTART
      COMMON/SATPIN/MAXDS,NGSAT1,NXSATP
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/YAWTOP/YAWLIM(3),XYAWTP
!
      DIMENSION iface(8)
      DIMENSION BFNRM1(NFACE),BFNRM2(NFACE),BFNRM3(NFACE)
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XSAT(3),VSAT(3)
      DIMENSION CTHETA(NFACE),UNTSUN(3)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3),VORB0(3),VEC1(3),VEC2(3)
      DIMENSION SPF(3,3),SAROT(3,3),TOTROT(3,3),                        &
     &          TEMP(3,3),SBF(3,3),AA(1),QAT(4),TEMP2(3,3)
      DIMENSION SABIAS(NTPBIA),TIMBI1(NTPBIA),TIMBI2(NTPBIA),           &
     &          IDATTB(NTPBIA)
      DIMENSION VLOUVS(3,NSTLVS)
      DIMENSION NSTLOV(NISLV),ISLVID(NISLV)
      DIMENSION TSLOUV(NSTLVS,3)
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
!         YTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         XTOD = TOD VECTOR NORMAL TO  ORBIT PLANE
!         ZTOD = TOD VECTOR TANGENT TO ORBIT PLANE
!
!  GRAIL satellite with forward velocity is given by NGSAT1
!  y=nadir, Y X Z = X (forward), and  -Z X Y = X (backward)
!
      if(NGSAT1.ne.2012001.and.NGSAT1.ne.2012002) then
      write(6,*)'ERROR IN GRLATT: '
      write(6,*)'FORWARD SATELLITE IS NEITHER 2012001 nor 2012002'
      stop
      endif

      if((NGSAT1.eq.2012001).and.(ISATID.eq.2012002)) GOTO 40
      if((NGSAT1.eq.2012002).and.(ISATID.eq.2012001)) GOTO 40
!
!
!
! LEADING SATELLITE
!
! Y AXIS ROTATION
      YMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      YTOD(1) = -XSAT(1)/YMAG
      YTOD(2) = -XSAT(2)/YMAG
      YTOD(3) = -XSAT(3)/YMAG
      SPF(1,2)= YTOD(1)
      SPF(2,2)= YTOD(2)
      SPF(3,2)= YTOD(3)
! X AXIS ROTATION (X = Y X VSAT)
      XTOD(1) = YTOD(2)*VSAT(3)-YTOD(3)*VSAT(2)
      XTOD(2) = YTOD(3)*VSAT(1)-YTOD(1)*VSAT(3)
      XTOD(3) = YTOD(1)*VSAT(2)-YTOD(2)*VSAT(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SPF(1,1)= XTOD(1)
      SPF(2,1)= XTOD(2)
      SPF(3,1)= XTOD(3)
! Z AXIS ROTATION (Z = X X Y)
      ZTOD(1) = XTOD(2)*YTOD(3)-XTOD(3)*YTOD(2)
      ZTOD(2) = XTOD(3)*YTOD(1)-XTOD(1)*YTOD(3)
      ZTOD(3) = XTOD(1)*YTOD(2)-XTOD(2)*YTOD(1)
      ZMAG = SQRT(ZTOD(1)**2+ZTOD(2)**2+ZTOD(3)**2)
      ZTOD(1) = ZTOD(1)/ZMAG
      ZTOD(2) = ZTOD(2)/ZMAG
      ZTOD(3) = ZTOD(3)/ZMAG
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)
      GOTO 80

40    CONTINUE

!
!
!
! TRAILING SATELLITE
!
! Y AXIS ROTATION
      YMAG = SQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
      YTOD(1) = XSAT(1)/YMAG
      YTOD(2) = XSAT(2)/YMAG
      YTOD(3) = XSAT(3)/YMAG
      SPF(1,2)= YTOD(1)
      SPF(2,2)= YTOD(2)
      SPF(3,2)= YTOD(3)
! X AXIS ROTATION
      XTOD(1) = VSAT(2)*YTOD(3)-VSAT(3)*YTOD(2)
      XTOD(2) = VSAT(3)*YTOD(1)-VSAT(1)*YTOD(3)
      XTOD(3) = VSAT(1)*YTOD(2)-VSAT(2)*YTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SPF(1,1)= XTOD(1)
      SPF(2,1)= XTOD(2)
      SPF(3,1)= XTOD(3)
! Z AXIS ROTATION
      ZTOD(1) = XTOD(2)*YTOD(3)-XTOD(3)*YTOD(2)
      ZTOD(2) = XTOD(3)*YTOD(1)-XTOD(1)*YTOD(3)
      ZTOD(3) = XTOD(1)*YTOD(2)-XTOD(2)*YTOD(1)
      ZMAG = SQRT(ZTOD(1)**2+ZTOD(2)**2+ZTOD(3)**2)
      ZTOD(1) = ZTOD(1)/ZMAG
      ZTOD(2) = ZTOD(2)/ZMAG
      ZTOD(3) = ZTOD(3)/ZMAG
      SPF(1,3)= ZTOD(1)
      SPF(2,3)= ZTOD(2)
      SPF(3,3)= ZTOD(3)

80    CONTINUE

!
!
! COMPUTE UNIT VECTOR DEFINING ZERO ORBIT ANGLE (XTOD X SUN)
!
!     VORB0(1) = XTOD(2)*BDTRUE(3,8)-XTOD(3)*BDTRUE(2,8)
!     VORB0(2) = XTOD(3)*BDTRUE(1,8)-XTOD(1)*BDTRUE(3,8)
!     VORB0(3) = XTOD(1)*BDTRUE(2,8)-XTOD(2)*BDTRUE(1,8)
!     VORBMG = SQRT(VORB0(1)**2+VORB0(2)**2+VORB0(3)**2)
!     VORB0(1) = VORB0(1)/VORBMG
!     VORB0(2) = VORB0(2)/VORBMG
!     VORB0(3) = VORB0(3)/VORBMG
!
! COMPUTE NEW ORBIT ANGLE
!     POSITIVE IN COUNTER-CLOCKWISE DIRECTION WHEN LOOKING DOWN ON ORBIT
!     RELATIVE TO VORB0
!     VEC1(1) = VORB0(2)*YTOD(3)-VORB0(3)*YTOD(2)
!     VEC1(2) = VORB0(3)*YTOD(1)-VORB0(1)*YTOD(3)
!     VEC1(3) = VORB0(1)*YTOD(2)-VORB0(2)*YTOD(1)
!     SINOMG  = VEC1(1)*XTOD(1)+VEC1(2)*XTOD(2)+VEC1(3)*XTOD(3)
!     COSOMG  = -YTOD(1)*VORB0(1)-YTOD(2)*VORB0(2)-YTOD(3)*VORB0(3)
!     SOMEGA = ATAN2(SINOMG,COSOMG)
!
!  COMPUTE BETAPRIME ANGLE (SUN INCLINATION TO ORBIT PLANE)
!
!     VEC1(1) = VORB0(2)*XTOD(3)-VORB0(3)*XTOD(2)
!     VEC1(2) = VORB0(3)*XTOD(1)-VORB0(1)*XTOD(3)
!     VEC1(3) = VORB0(1)*XTOD(2)-VORB0(2)*XTOD(1)
!     VEC1MG = SQRT(VEC1(1)**2+VEC1(2)**2+VEC1(3)**2)
!     VEC1(1) = VEC1(1)/VEC1MG
!     VEC1(2) = VEC1(2)/VEC1MG
!     VEC1(3) = VEC1(3)/VEC1MG
!     VEC2(1) = VEC1(2)*BDTRUE(3,8)-VEC1(3)*BDTRUE(2,8)
!     VEC2(2) = VEC1(3)*BDTRUE(1,8)-VEC1(1)*BDTRUE(3,8)
!     VEC2(3) = VEC1(1)*BDTRUE(2,8)-VEC1(2)*BDTRUE(1,8)
!     SINBET  = VEC2(1)*VORB0(1)+VEC2(2)*VORB0(2)+VEC2(3)*VORB0(3)
!     COSBET=VEC1(1)*BDTRUE(1,8)+VEC1(2)*BDTRUE(2,8)+VEC1(3)*BDTRUE(3,8)
!     BETAP  = ATAN2(SINBET,COSBET)
!
! COMPUTE ROTATION FROM SBF TO SPF FRAMES
      if((NGSAT1.eq.2012001).and.(ISATID.eq.2012002)) GOTO 50
      if((NGSAT1.eq.2012002).and.(ISATID.eq.2012001)) GOTO 50
! leading alongtrack
!      SBF(1,1) = ZERO
!      SBF(1,2) = ZERO
!      SBF(1,3) = ONE
! leading crosstrack
!      SBF(2,1) = ONE
!      SBF(2,2) = ZERO
!      SBF(2,3) = ZERO
! leading radial
!      SBF(3,1) = ZERO
!      SBF(3,2) = -ONE
!      SBF(3,3) = ZERO
!      GOTO 60
50     CONTINUE
! trailing alongtrack
!      SBF(1,1) = ZERO
!      SBF(1,2) = ZERO
!      SBF(1,3) = -ONE
! trailing crosstrack
!      SBF(2,1) = -ONE
!      SBF(2,2) = ZERO
!      SBF(2,3) = ZERO
! trailing radial
!      SBF(3,1) = ZERO
!      SBF(3,2) = ONE
!      SBF(3,3) = ZERO
60     CONTINUE

!
!
!     DO 100 I=1,3
!        TOTROT(I,1) = ZERO
!        TOTROT(I,2) = ZERO
!        TOTROT(I,3) = ZERO
! 100 END DO
!
      DO 150 I=1,NFACE
         TDNRM1(I) = ZERO
         TDNRM2(I) = ZERO
         TDNRM3(I) = ZERO
  150 END DO
!
!
! COMPUTE SOLAR ARRAY PITCH ANGLE
!     SABTMP=0.0d0
!     DO 110 I=1,NTPBIA
!     IF((TIME.GE.TIMBI1(I)).AND.(TIME.LE.TIMBI2(I)))SABTMP=SABIAS(I)
! 110 END DO
!     SGAMMA = SOMEGA + SABTMP
!     COSGAM = COS(SGAMMA)
!     SINGAM = SIN(SGAMMA)
!
! COMPUTE ROTATION FOR SOLAR ARRAY
!     SAROT(1,1) = ONE
!     SAROT(1,2) = ZERO
!     SAROT(1,3) = ZERO
!     SAROT(2,1) = ZERO
!     SAROT(2,2) = SINGAM
!     SAROT(2,3) = COSGAM
!     SAROT(3,1) = ZERO
!     SAROT(3,2) = COSGAM
!     SAROT(3,3) = -SINGAM
!
! COMPUTE TOTAL ROTATION MATRIX FROM SBF TO TOD FRAME
!
!     DO 200 I=1,3
!     TOTROT(I,1) =               SPF(I,1)*SBF(1,1)                     &
!    &                          + SPF(I,2)*SBF(2,1)                     &
!    &                          + SPF(I,3)*SBF(3,1)
!     TOTROT(I,2) =               SPF(I,1)*SBF(1,2)                     &
!    &                          + SPF(I,2)*SBF(2,2)                     &
!    &                          + SPF(I,3)*SBF(3,2)
!     TOTROT(I,3) =               SPF(I,1)*SBF(1,3)                     &
!    &                          + SPF(I,2)*SBF(2,3)                     &
!    &                          + SPF(I,3)*SBF(3,3)
! 200 END DO
!
!
! ROTATE SBF UNIT NORMAL VECTORS(NON-MOVING PLATES) TO TOD FRAME
!     DO 500 I=1,NFACE-NMOVE
!        TDNRM1(I) =                 TOTROT(1,1)*BFNRM1(I) +            &
!    &                               TOTROT(1,2)*BFNRM2(I) +            &
!    &                               TOTROT(1,3)*BFNRM3(I)
!        TDNRM2(I) =                 TOTROT(2,1)*BFNRM1(I) +            &
!    &                               TOTROT(2,2)*BFNRM2(I) +            &
!    &                               TOTROT(2,3)*BFNRM3(I)
!        TDNRM3(I) =                 TOTROT(3,1)*BFNRM1(I) +            &
!    &                               TOTROT(3,2)*BFNRM2(I) +            &
!    &                               TOTROT(3,3)*BFNRM3(I)
! 500 END DO
      DO 501 I=1,NFACE-NMOVE
         TDNRM1(I) =                    SPF(1,1)*BFNRM1(I) +            &
     &                                  SPF(1,2)*BFNRM2(I) +            &
     &                                  SPF(1,3)*BFNRM3(I)
         TDNRM2(I) =                    SPF(2,1)*BFNRM1(I) +            &
     &                                  SPF(2,2)*BFNRM2(I) +            &
     &                                  SPF(2,3)*BFNRM3(I)
         TDNRM3(I) =                    SPF(3,1)*BFNRM1(I) +            &
     &                                  SPF(3,2)*BFNRM2(I) +            &
     &                                  SPF(3,3)*BFNRM3(I)
  501 ENDDO

! No Moving Parts
      if(NMOVE.gt.0) then
        write(6,*)'Not set up for moving parts in GRLATT'
        stop
      endif
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

! No Louver
      CAll FNDNUM(IDSATS,ISLVID,NISLV ,IRET)
      if(IRET.gt.0) then
        write(6,*)'Not set up for Louver in GRLATT'
        stop
      endif
 1000 CONTINUE
!
! OUTPUT GRAIL 1 AND 2 TELEM FILE INFORMATION
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
      END
