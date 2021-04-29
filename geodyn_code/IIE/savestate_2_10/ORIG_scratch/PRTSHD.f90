!$PRTSHD
      SUBROUTINE PRTSHD(MJDSEC,FSEC,ISATID,LCRSUN,LSTSUN,AA)
!********1*********2*********3*********4*********5*********6*********7**
! PRTSHD            92/07/14            9204     PGMR - SHELLEY ROWTON
!
! FUNCTION: PRINTOUT SHADOW CROSSING INFORMATION TO THE TRAJECTORY
!           FILE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC               FRACTIONAL SECONDS
!   ISATID   I         SAT ID ARRAY
!   LSTSUN  I/O   S    .TRUE. WHEN SATELLITE WAS IN SUN LAST CALL AS
!                      INPUT
!   LCRSUN  I/O   S    .TRUE. WHEN SATELLITE WAS IN SUN THIS CALL AS
!                      OUTPUT
!   AA      I     A    REAL DYNAMIC ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      LOGICAL LSTSUN,LCRSUN
!
      COMMON/CONTRL/LSIMDT,LOBS  ,LORB  ,LNOADJ,LNADJL,LORFST,LORLST,   &
     &              LORALL,LORBOB,LOBFST,LOBLST,LOBALL,LPREPO,LORBVX,   &
     &              LORBVK,LACC3D,LSDATA,LCUTOT,LACCEL,LDYNAC,LFRCAT,   &
     &              LNIAU, NXCONT
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
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DIMENSION FSEC(1),ISATID(1),AA(1)
      DIMENSION FSECOT(1),IYMD(1),IHM(1),SEC(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! CONVERT ET TIME TO UT
!
      CALL UTCET(.FALSE.,1,MJDSEC,FSEC,FSECOT,AA(KA1UT))
!
! CONVERT MJDSEC TO YYMMDDHHMMSS
!
      CALL YMDHMS(MJDSEC,FSECOT,IYMD,IHM,SEC,1)
!
! WRITE TO THE TRAJECTORY FILE ONLY WHEN SAT CURRENTLY IN SUN AND
! SAT LAST IN SUN ARE NOT EQUIVALENT, AND THEN DETERMINE WHETHER TO
! WRITE INFORMATION TO UNIT8 FOR CARTESIAN OR UNIT10 FOR KEPPLERIAN
!
      IF(LORBVX.AND.LCRSUN)THEN
         ILINE8=ILINE8+1
         WRITE(IOUT8,1000)ISATID,IYMD,IHM,SEC
      ELSE IF(LORBVX.AND..NOT.LCRSUN)THEN
         ILINE8=ILINE8+1
         WRITE(IOUT8,1001)ISATID,IYMD,IHM,SEC
      ENDIF
!
      IF(LORBVK.AND.LCRSUN)THEN
         ILIN10=ILIN10+1
         WRITE(IOUT10,2000)ISATID,IYMD,IHM,SEC
      ELSE IF(LORBVK.AND..NOT.LCRSUN)THEN
         ILIN10=ILIN10+1
         WRITE(IOUT10,2001)ISATID,IYMD,IHM,SEC
      ENDIF
!
! 1000 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',2I6,1X,F4.1,               &
 1000 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',I9,I6,1X,F4.1,             &
     &       ' IS GOING FROM SHADOW TO SUNLIGHT '/)
! 1001 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',2I6,1X,F4.1,               &
 1001 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',I9,I6,1X,F4.1,             &
     &       ' IS GOING FROM SUNLIGHT TO SHADOW '/)
! 2000 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',2I6,1X,F4.1,               &
 2000 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',I9,I6,1X,F4.1,             &
     &       ' IS GOING FROM SHADOW TO SUNLIGHT '/)
! 2001 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',2I6,1X,F4.1,               &
 2001 FORMAT(1X,'SATELLITE ',I8, ' AT TIME ',I9,I6,1X,F4.1,             &
     &       ' IS GOING FROM SUNLIGHT TO SHADOW '/)
      END
