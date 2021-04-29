      SUBROUTINE ALTRFC(XSN,XBM,OFF,OBS,THETAG,SCR,SCR2,OCDRYT,OCWETT,  &
     &                  OBSMET,NM,MTYPE,LH,MJDSBL,FSECSA,AA)
!********1*********2*********3*********4*********5*********6*********7**
!  ALTRFC          00/00/00         0000.0      PGMR - ?
!
! FUNCTION: COMPUTES ATMOSPHERIC REFRACTION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSN     I     A     TRANSMIT S/C TRACKING POINT TOR COORDS
!   XBM     I     A     APPROXIMATE BOUNCE POINT TOR COORDS (ELLIP. INTER
!   OFF     I     A     TRANSMIT INST. POINTING UNIT VECTOR
!   OBS     O     A     DISTANCE - TRANSMIT TO BOUNCE
!   THETAG        A     RIGHT ASCENSIONS OF GREENWICH
!   SCR           A     SCRATCH SPACE
!   SCR2          A     SCRATCH SPACE
!   OCDRYT  O     A     DRY TROPOSPHERIC CORRECTION
!   OCWETT  O     A     WET TROPOSPHERIC CORRECTION
!   OBSMET        A     METEOROLOGICAL DATA FOR EACH OBSERVATION
!   NM            S     NUMBER OF MEASUREMENTS
!   MTYPE         S     MEASUREMENT TYPE
!   LH            S
!   MJDSBL        S     MODIFIED JULIAN DAY SECONDS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBIASA/BIASE (4),BIASM (2),BSCALE(1)  ,TBIAS (1),          &
     &              BSTROP(3),BSIONO(3),CLKSTA(4,2),CLKSAT(4,2),        &
     &              CLKSTS(2,2),TROPZE(2,2),TROPGR(2,4),BSLBIA(1)
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
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
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      DIMENSION XSN(MINTIM,3),XBM(MINTIM,3),OFF(MINTIM,3),SCR(MINTIM,8)
      DIMENSION OBS(NM),THETAG(NM),SCR2(MINTIM,3)
      DIMENSION OBSMET(NM),OCDRYT(NM),OCWETT(NM)
!
!jtw add CMETI,CORA03,CBLOKL
      IF(LH) GOTO 51
      DO 50 I=1,NM
      OBSCB=OBS(I)+BIASM(1)
! OFF HAS AN INTRINSIC MINUS SIGN
      XBM(I,1)=XSN(I,1)+ABS(OBS(I))*OFF(I,1)
      XBM(I,2)=XSN(I,2)+ABS(OBS(I))*OFF(I,2)
      XBM(I,3)=XSN(I,3)+ABS(OBS(I))*OFF(I,3)
      THETAG(I)=0.D0
   50 END DO
!      xbmag1=sqrt(xbm(1,1)**2+xbm(1,2)**2+xbm(1,3)**2)
!      xbmagnm=sqrt(xbm(nm,1)**2+xbm(nm,2)**2+xbm(nm,3)**2)
!      xsmag1=sqrt(xsn(1,1)**2+xsn(1,2)**2+xsn(1,3)**2)
!      xsmagnm=sqrt(xsn(nm,1)**2+xsn(nm,2)**2+xsn(nm,3)**2)
!      print *,'altrfc: xbmag1,xsmag1 : ',xbmag1,xsmag1
!      print *,'altrfc: xbmagnm,xsmagnm : ',xbmagnm,xsmagnm
   51 CONTINUE
      CALL SUBTRK(XBM,THETAG,SCR(1,1),SCR(1,2),SCR(1,3),SCR(1,4),       &
     &            SCR(1,5),SCR(1,6),NM)
      LHP=.FALSE.
      DO 60 I=1,NM
      SCR(I,3)=SCR(I,4)*DEGRAD
      SCR(I,4)=COS(SCR(I,3))
      SCR(I,4)=COS(SCR(I,3))
      SCR(I,5)=SIN(SCR(I,3))
      SCR(I,5)=SIN(SCR(I,3))
      SCR(I,1)=SQRT(XBM(I,1)*XBM(I,1)+XBM(I,2)*XBM(I,2)                &
     &                    +XBM(I,3)*XBM(I,3))
!
! QUATERNIONS CAN SOMETIMES HAVE SIGNS FLIPPED. MAKE SURE
! SINEL (SCR8) IS A POSITIVE NUMBER
!
! ALSO A FLIP IN QUATERNIONS CAN CAUSE A BAD BOUNCE POINT
! LOCATION. CHECK AGAINST EARTH RADIUS (THIS) ROUTINE
! SHOULD ONLY BE CALLED FOR EARTH. A SIGN FLIP COULD ONLY
! MAKE THE BOUNCE POINT TOO HIGH (NOT TOO LOW).
!
      IF(SCR(I,1).GT.6390000.D0) LHP=.TRUE.
      SCR(I,8)=ABS((XBM(I,1)*OFF(I,1)+XBM(I,2)*OFF(I,2)                &
     &          +XBM(I,3)*OFF(I,3))/SCR(I,1))
      SCR(I,7)=SQRT(ABS(1.D0-SCR(I,8)*SCR(I,8)))
   60 END DO
!
! NEED TO GET WAVLENGTH PASSED BETTER THAN THIS
!
      WAVLEN=VLIGHT/BLKDAT(3,1)
!
      MODELT=1
!      CALL REFRAC(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),   &
!     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
!     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,                  &
!     &            SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL)
!jtw edit, change refrac to split up subroutines:

      IWPR=IRFPRT(MODEL(MTYPE))

      IF(MODEL(MTYPE).EQ.0) THEN

      CALL HOPFLD(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),   &
     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,SCR(1,3),         &
     &            SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,FSECSA,       &
     &            IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.1) THEN

           IF(LASER) THEN

                CALL MARMUR(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),   &
               &    SCR2(1,2),SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),  &
               &    LPRE(6,1),BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,      &
               &    SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,   &
               &    FSECSA,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL HOPFLD(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),   &
               &    SCR2(1,2),SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),  &
               &    LPRE(6,1),BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,      &
               &    SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,   &
               &    FSECSA,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.2) THEN

      CALL VLBGPS(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),   &
     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,SCR(1,3),         &
     &            SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,FSECSA,       &
     &            IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.3) THEN

      CALL GPSNIL(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),   &
     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,SCR(1,3),         &
     &            SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,FSECSA,       &
     &            IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.4) THEN

           IF(LASER) THEN

                CALL MARMUR(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),   &
               &    SCR2(1,2),SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),  &
               &    LPRE(6,1),BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,      &
               &    SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,   &
               &    FSECSA,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL GPSNIL(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),   &
               &    SCR2(1,2),SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),  &
               &    LPRE(6,1),BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,      &
               &    SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,   &
               &    FSECSA,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.5) THEN

           IF(LASER) THEN

                CALL MENDEZ(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),   &
               &    SCR2(1,2),SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),  &
               &    LPRE(6,1),BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,      &
               &    SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,   &
               &    FSECSA,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL HOPFLD(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),   &
               &    SCR2(1,2),SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),  &
               &    LPRE(6,1),BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,      &
               &    SCR(1,3),SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,   &
               &    FSECSA,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.6) THEN

      CALL GMFHOP(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),   &
     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,SCR(1,3),         &
     &            SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,FSECSA,       &
     &            IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.7) THEN

      CALL GMFSAS(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),   &
     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,SCR(1,3),         &
     &            SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,FSECSA,       &
     &            IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.8) THEN

      CALL VMF1(SCR(1,7),SCR(1,8),SCR(1,1),OBS,SCR2(1,1),SCR2(1,2),     &
     &            SCR2(1,3),NM,MODELT,LPRE(3,1),LPRE(4,1),LPRE(6,1),    &
     &            BLKDAT(1,1),OBSMET,LPRE(1,1),.TRUE.,SCR(1,3),         &
     &            SCR(1,4),SCR(1,5),SCR(1,6),MTYPE,MJDSBL,FSECSA,       &
     &            IWPR,NSTA0,AA(KA1UT))

      END IF

      IF(.NOT.LPRE(3,1)) THEN
         DO 70 I=1,NM
         OCDRYT(I)=SCR2(I,1)
   70    CONTINUE
      ENDIF
      IF(.NOT.LPRE(4,1)) THEN
         DO 80 I=1,NM
         OCWETT(I)=SCR2(I,2)
   80    CONTINUE
      ENDIF
!
      IF(LHP) WRITE(6,6000)
      RETURN
 6000 FORMAT(' WARNING: UNREASONABLE BOUNCE PT LOCATION IN ALTRFC')
      END
