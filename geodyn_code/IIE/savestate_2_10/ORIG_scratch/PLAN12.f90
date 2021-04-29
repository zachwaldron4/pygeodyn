!$PLAN12
      SUBROUTINE PLAN12(NREC,MJDSEC,FSEC,LTIDE,AA,II,UTDT)
!********1*********2*********3*********4*********5*********6*********7**
! PLAN12           83/08/23            8308.0    PGMR - D. ROWLANDS
!
! FUNCTION:  CALCULATE PLANET POSITIONS (& IF NECC VELOCITIES)
!            AT 12 HR INTERPOLATION INTERVALS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NREC     I    S    RECORD # TO BE FILLED IN COMMON BLOCK D71BUF
!   MJDSEC   I    S    TIME OF 12 HR PERIOD IN EPHEMERIS SECONDS
!                      SINCE GEODYN REFERENCE TIME
!   FSEC     I    A    FRACTIONAL REMAINING SECONDS
!   LTIDE    I    S    .TRUE. IF EXTENDED TIDE MODEL REQUESTED
!                      REQUIRING PLANET VELOCITIES
!   AA      I/O   A
!   II      I/O   A
!
! COMMENTS: OUTPUT THROUGH COMMON BLOCK MNSNI&D71BF
!
! RESTRICTION:  CAN ONLY CALCULATE THE NEEDED QUANTITIES AT 12 HR
!               INTERVALS CORRESPONDING TO NUTATION INTERPOLATION
!               ENDPOINT TIMES
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION RPN(9),SCRTCH(11),EQN(1),FSEC(1),AA(1),II(1)
      DIMENSION TEMP(1),TEMPC(1),TEMPS(1)
      DIMENSION UTDT(1,1)
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
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
      COMMON/CNUTAT/SINEM(1),COSEM(1),SINDP(1),COSDP(1),SINET(1),       &
     &   COSET(1),SINZ(1),COSZ(1),SINTH(1),COSTH(1),SINEA(1),COSEA(1),  &
     &   SDSE(1),SDCE(1),CDSE(1),CDCE(1),SESD(1),SECD(1),CESD(1),       &
     &   CECD(1),SESE(1),SECE(1),CESE(1),CECE(1),CECT(1),SEST(1),       &
     &   CEST(1),SECT(1),CECZ(1),CESZ(1),SECZ(1),SESZ(1),STSZ(1),       &
     &   CTCZ(1),STCZ(1),CTSZ(1),CTCZCE(1),STSZSE(1),CTCZSE(1),         &
     &   CTSZCE(1),STCZCE(1),STSZCE(1),STCZSE(1),CTSZSE(1),SESDSE(1),   &
     &   SESDCE(1),SECDSE(1),SECDCE(1),CESDSE(1),CESDCE(1),CECDSE(1),   &
     &   CECDCE(1),EPSM(1),EPST(1),DPSI(1),Z(1),THTA(1),ETA(1),AK1(1),  &
     &   AK2(1),AK3(1),AK4(1),AK5(1),AK6(1),AK7(1),AK8(1),SUMMY(198)
      COMMON/D71BUF/DECSUN(4),RASUN(4),TCD71(3),XKPC(3),FSCTC,FSCKP
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)
      COMMON/GRANGL/GHRANG(4),COSGHR(4),SINGHR(4)
      COMMON/MNSNI/XMNSNI(6,2,4)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      N=(NREC-1)*66+1
      NRECC=NREC-1
      NRECC=MAX(NRECC,1)
!  CALCULATE THE MATRX TO GO FROM MEAN 50 TO TRUE EPOCH
      A1=THTA(N)
      A2=Z(N)
      A3=ETA(N)
      A4=EPST(N)
      A5=EPSM(N)
      A6=DPSI(N)
      CALL DEQN(NRECC,.FALSE.,MJDSEC,FSEC,A1,A2,A3,                     &
     &          A4,A5,A6,RPN(1),RPN(2),RPN(3),                          &
     &          RPN(4),RPN(5),RPN(6),RPN(7),RPN(8),RPN(9),              &
     &          EQN,SCRTCH,1)
! GET GREENWICH HOUR ANGLE
      CALL GRHRN2(MJDSEC,FSEC,EQN,SCRTCH,                               &
     &   TEMP,TEMPC,TEMPS,1,AA,UTDT)
      GHRANG(NREC)=TEMP(1)
      COSGHR(NREC)=TEMPC(1)
      SINGHR(NREC)=TEMPS(1)
!  GET MEAN 50 PLANET POSITION
      CALL PLANPO(MJDSEC,FSEC,LTIDE,.FALSE.,AA,II)
!  ROTATE TO TRUE (SUN FIRST)
      XMNSNI(1,2,NREC)=RPN(1)*BDSTAT(1,8)+RPN(4)*BDSTAT(2,8)            &
     &                +RPN(7)*BDSTAT(3,8)
      XMNSNI(2,2,NREC)=RPN(2)*BDSTAT(1,8)+RPN(5)*BDSTAT(2,8)            &
     &                +RPN(8)*BDSTAT(3,8)
      XMNSNI(3,2,NREC)=RPN(3)*BDSTAT(1,8)+RPN(6)*BDSTAT(2,8)            &
     &                +RPN(9)*BDSTAT(3,8)
      RSUN=XMNSNI(1,2,NREC)*XMNSNI(1,2,NREC)+XMNSNI(2,2,NREC)           &
     &    *XMNSNI(2,2,NREC)+XMNSNI(3,2,NREC)*XMNSNI(3,2,NREC)
      RSUN=SQRT(RSUN)
!CC      DECSUN(NREC)=DARSIN(XMNSNI(3,2,NREC)/RSUN)
      DECSUN(NREC)=ASIN(XMNSNI(3,2,NREC)/RSUN)
      RASUN(NREC)=ATAN2(XMNSNI(2,2,NREC),XMNSNI(1,2,NREC))
      IF(.NOT.LTIDE) RETURN
      XMNSNI(4,2,NREC)=RPN(1)*BDSTAT(4,8)+RPN(4)*BDSTAT(5,8)            &
     &                +RPN(7)*BDSTAT(6,8)
      XMNSNI(5,2,NREC)=RPN(2)*BDSTAT(4,8)+RPN(5)*BDSTAT(5,8)            &
     &                +RPN(8)*BDSTAT(6,8)
      XMNSNI(6,2,NREC)=RPN(3)*BDSTAT(4,8)+RPN(6)*BDSTAT(5,8)            &
     &                +RPN(9)*BDSTAT(6,8)
      XMNSNI(1,1,NREC)=RPN(1)*BDSTAT(1,11)+RPN(4)*BDSTAT(2,11)          &
     &                +RPN(7)*BDSTAT(3,11)
      XMNSNI(2,1,NREC)=RPN(2)*BDSTAT(1,11)+RPN(5)*BDSTAT(2,11)          &
     &                +RPN(8)*BDSTAT(3,11)
      XMNSNI(3,1,NREC)=RPN(3)*BDSTAT(1,11)+RPN(6)*BDSTAT(2,11)          &
     &                +RPN(9)*BDSTAT(3,11)
      XMNSNI(4,1,NREC)=RPN(1)*BDSTAT(4,11)+RPN(4)*BDSTAT(5,11)          &
     &                +RPN(7)*BDSTAT(6,11)
      XMNSNI(5,1,NREC)=RPN(2)*BDSTAT(4,11)+RPN(5)*BDSTAT(5,11)          &
     &                +RPN(8)*BDSTAT(6,11)
      XMNSNI(6,1,NREC)=RPN(3)*BDSTAT(4,11)+RPN(6)*BDSTAT(5,11)          &
     &                +RPN(9)*BDSTAT(6,11)
      RETURN
      END
