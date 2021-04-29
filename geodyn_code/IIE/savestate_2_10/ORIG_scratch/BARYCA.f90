!$BARYCX
      SUBROUTINE BARYCA(MJDSEC,FSEC,XPC,VPC,NDXVPC,                     &
     &                 IGRPX,IOR,IORMOX,IORSNX,IOREMX,                  &
     &                 COEF,COEFMO,COEFS,COEFEM,                        &
     &                 NM,NDIM1,NDIM2,LVEL,LTBODY,LSUNV,                &
     &                 PARCBD,PARTBD,PSTAT,PSUN,PMOON,                  &
     &                 CHEB,CHEBV,CHEBS,CHEBSV,CHEBMO,CHBVMO,           &
     &                 TTTMO,TTT,TWOT,AA,II)
!********1*********2*********3*********4*********5*********6*********7**
! BARYCX
!
!
!  FUNCTION:              PUT PLANET CENTERED (TRUE OF SAT REF)
!                         COORDINATES INTO BARYCENTRIC (TRUE SAT REF)
!                         COORDINATES FOR THE CASE WHERE THE 'PLANET'
!                         IS AN ASTEROID WHOSE ORBIT IS ALSO BEING
!                         DETERMINED
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         INTEGER SECONDS SINCE GEODYN REFERENCE TIME
!   FSEC     I         FRACTIONAL REMAINING SECONDS (COORDINATE ET)
!   XPC     I\O        PLANET CENTERD TRUE OF SAT REF SYTEM POS INPUT
!                      BARYCENTERD TRUE OF SAT REF SYTEM POS OUTPUT
!   VPC     I\O        PLANET CENTERD TRUE OF SAT REF SYTEM VEL INPUT
!                      BARYCENTERD TRUE OF SAT REF SYTEM VEL OUTPUT
!   NDXVPC   I         FIRST DIMENSION OF XPC &VPC
!   IGRP     I         CENTRAL BODY GROUP (MERCURY=3, OTHERWISE=1)
!   IOR      I         ORDER OF CHEBYSHEV POLYNOMIALS
!   IORMO    I         ORDER OF CHEBYSHEV POLYNOMIALS (MOON)
!   IORSUN   I         ORDER OF CHEBYSHEV POLYNOMIALS (SUN)
!   COEF     I         CHEBYSHEV POLYNOMIAL COEFFICIENTS
!   COEFMO   I         CHEBYSHEV POLYNOMIAL COEFFICIENTS (MOON)
!   COEFS    I         CHEBYSHEV POLYNOMIAL COEFFICIENTS (SUN)
!   NM       I         NUMBER OF TIMES
!   LVEL     I         TRUE IF VELOCITY CALCULATIOONS REQUIRED
!   LTBODY   I         TRUE IF PLANET IS TRACKING BODY
!   LSUNV    I         TRUE IF VECTOR FROM XPC TO SUN SHOULD BE CALC.
!   PSTAT    O         PLANET STATE IN BARYCENTRIC TRUE SAT REF SYSTEM
!   PSUN     O         SUN    STATE IN BARYCENTRIC TRUE SAT REF SYSTEM
!   PMOON    O         MOON   STATE IN BARYCENTRIC TRUE SAT REF SYSTEM
!   CHEB     O         CHEBYSHEV POLYNOMIALS
!   CHEBV    O         CHEBYSHEV VELOCITY POLYNOMIALS
!   CHEBS    O         CHEBYSHEV POLYNOMIAL (SUN)
!   CHEBSV   O         CHEBYSHEV VELOCITY POLYNOMIAL (SUN)
!   CHEBMO   O         CHEBYSHEV POLYNOMIAL (MOON)
!   CHBVMO   O         CHEBYSHEV VELOCITY POLYNOMIAL (MOON)
!   TTTMO         A    NORMALIZED MOON TIME
!   TTT           A    NORMALIZED TIME (FSEC)
!   TWOT          A    SCRATCH ARRAY
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBARYC/CBODY(6),TBODY(6),SUNRF(6)
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CEPHEP/FSCEPC(1),EPHBFC(1016),                             &
     &              FSCEPT(1),EPHBFT(1016),                             &
     &              SEPBFC(75),SEPBFT(75),XEPHEP
      COMMON/CHCOFX/ COEFX(3,25),COEFXX(3,25,988),RLVAL(988)
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CORA02/KFSCTB,KFSCTC,KFSCTE,KDSECT,                        &
     &              KFSECT,KS    ,KCIP  ,KCIV  ,                        &
     &       KXTN  ,KXSM  ,KXTK  ,KXSJ  ,KXTI  ,KXTKP ,                 &
     &       KVTN  ,KVSM  ,KVTK  ,KVSJ  ,KVTI  ,KVTKP ,                 &
     &       KSATLT,KSATLN,KSATH ,KCOSTH,KSINTH,                        &
     &       KPXPFM,KPXPFK,KPXPFJ,KTRBF ,KFSTRC,                        &
     &       KFSTRE,KDSCTR,KFSCVS,KXSMBF,KXSKBF,KXSJBF,                 &
     &       KRSSV1,KRSSV2,KRSSV3,KTPMES,KACOEF,KACTIM,                 &
     &       KXTNPC,KXSMPC,KXTKPC,KXSJPC,KXTIPC,KXTKPP,                 &
     &       KRLRNG,KASTO,KASTP,NXCA02
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
      COMMON/COREPH/EMJDS0(2),EFSEC0(2),EMODE(2),ECORR(2),              &
     &              PSTAE0(12),DEBC(12),SGMEBC(12),BODYID(2),XCOEPH
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)
      COMMON/EPHMX/FSECSE(10,2)
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
      COMMON/EPHSET/EMFACT,DTENPD,DTINPD,FSC1EN,FSCENP(2),FSCINP(4),    &
     &   FSDINP(4),XEPHST
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/IORCHB/ IORM,IORJ,IORSA,IORSUN,IOREM,IORMO,IORCB,IORTB,    &
     &               IGRP,ICHBOG(2,14),IORSCB(988),NXORCH
      COMMON/LCBODY/LEARTH,LMOON,LMARS
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
      COMMON/PLNETI/IPLNET(999),IPLNIN(999),MPLNGD(999),MPLNGO(999),   &
     &              IPLNZ(999),NXPLNI
      DIMENSION FSEC(NM),COEF(3,IOR),COEFMO(3,IORMO),COEFS(3,IORSUN)
      DIMENSION COEFEM(3,IOREM),DXDEBC(6,6),PSTATC(6),CB(6),TB(6)
      DIMENSION CHEB(NM,IOR),CHEBV(NM,IOR),                             &
     &          CHEBS(NM,IORSUN),CHEBSV(NM,IORSUN),                     &
     &          CHEBMO(NM,IORMO),CHBVMO(NM,IORMO),                      &
     &          PSTAT(NM,6),PMOON(NM,6),PSUN(NM,3),                     &
     &          XPC(NDXVPC,3),VPC(NDXVPC,3),PARCBD(NM,36),PARTBD(NM,36)
      DIMENSION TTT(NM),TTTMO(NM),FNSEC(4),TWOT(NM),VSEC(10)
      DIMENSION AA(1),II(1)
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
      DATA FNSEC/2764800.D0,1382400.D0,691200.D0,345600.D0/
      DATA LEPOCT,LEPOCC/.TRUE.,.TRUE./
      DATA ONETH/1.0D3/

      !******************************************************************
      ! START OF EXECUTABLE CODE ****************************************
      !******************************************************************

      DIF=ABS(ECORR(1)-1.D0)
      LCNVEL=.FALSE.
      IF(DIF.LT..0001D0) LCNVEL=.TRUE.

      ! INITIALIZE
      DO I=1,ICBODY
          VSEC(I)=TWO/RLVAL(I)
      ENDDO
      DO I=1,NM
          CHEB(I,1) = ONE
          CHEBV(I,1)= ZERO
          CHEBV(I,2)= ONE
          CHEBSV(I,1) = ZERO
          CHEBSV(I,2) = ONE
      END DO

      DO I=1,NM
          DO J=1,3
              PSTAT(I,J) = ZERO
          END DO
      END DO
      IF(LVEL.OR.LCNVEL) THEN
          DO I=1,NM
              DO J=4,6
                  PSTAT(I,J) = ZERO
              END DO
          END DO
      ENDIF

      ! CALCULATE CENTER  OF INTEGRATION COORDINATES
      !
      ! FIGURE OUT IN WHICH GROUP IS THE TIME MJDSEC FSEC
      ! MAP MJDSEC,FSEC INTO INTERVAL  -1,1  FOR EACH PERIOD GROUP
      ! FIGURE POINTER WHITHIN 32 DAY RECORD FOR EACH PERIOD GROUP
      ! CENTRAL BODY
      !
      ! CHECK HERE IF THE BLOCK SPANS TWO SUPP EPHEM BUFFERS AND RESET HE
      ! THE TIMES
      T1=DBLE(MJDSEC)+FSEC(1)
      T2=DBLE(MJDSEC)+FSEC(NM)
      !write(6,*)' TIMES IN BARYCX ',T1,T2,FSECSE(ICBDGM-11,2)
      IF(T1.LT.FSECSE(ICBDGM-11,2).AND.T2.GT.FSECSE(ICBDGM-11,2)) THEN
          FSECSE(ICBDGM-11,1)=(FSECSE(ICBDGM-11,1)+FSECSE(ICBDGM-11,2)) &
     &          /2.D0
          FSECSE(ICBDGM-11,2)=FSECSE(ICBDGM-11,1)+RLVAL(ICBDGM-11)
          !write(6,*)' dbg BARYCX split reset ',FSECSE(1,1),FSECSE(1,2)
      ENDIF
      DO I=1,NM
          TTEST=MJDSEC+FSEC(I)-FSECSE(ICBDGM-11,1)
          ! FIGURE OUT IN WHICH GROUP IS THE TIME MJDSEC FSEC
          ! MAP MJDSEC,FSEC INTO INTERVAL  -1,1
          !IGRPT=TTEST/RLVAL(ICBDGM-11)
          !FSECC=FSECSE(ICBDGM-11,1)+IGRPT*RLVAL(ICBDGM-11)
          !DT=((MJDSEC-FSECC)+FSEC(I))/RLVAL(ICBDGM-11)
          DT=TTEST/RLVAL(ICBDGM-11)
          TTT(I)=DT*TWO-ONE
      END DO

      ! COMPUTE CHEBYSHEV POLYNOMIALS AT APPROPRIATE TIMES
      LCHEBV=LVEL.OR.LCNVEL
      CALL CHEBPV(CHEB,CHEBV,TTT,IOR,NM,TWOT,LCHEBV)

      ! COMPUTE POSITION
      DO I=1,3
          DO J=1,IOR
              DO K=1,NM
                  PSTAT(K,I) = PSTAT(K,I)+COEFX(I,J)*CHEB(K,J)
              END DO
          END DO
      END DO

      ! COMPUTE VELOCITY
      IF(LVEL.OR.LCNVEL) THEN
          DO I=1,IOR
              DO J=1,NM
                  CHEBV(J,I) = CHEBV(J,I)*VSEC(ICBDGM-11)
              END DO
          END DO
          DO I=4,6
              DO J=1,IOR
                  DO K=1,NM
                      PSTAT(K,I) = PSTAT(K,I)+COEFX(I-3,J)*CHEBV(K,J)
                  END DO
              END DO
          END DO
      ENDIF

      IF(LMOON) THEN
          DO I=1,3
              DO K=1,NM
                  PSTAT(K,I) = PSTAT(K,I)+PMOON(K,I)
              END DO
          END DO
          IF(LVEL.OR.LCNVEL) THEN
              DO I=4,6
                  DO K=1,NM
                      PSTAT(K,I) = PSTAT(K,I)+PMOON(K,I)
                  END DO
              END DO
          ENDIF
      ENDIF

      !  CORRECT THE PLANET EPHEMERIS / HERE CENTRAL BODY STATE VECTOR
      !  --------------------------------------------------------------
      IF (ECORR(1).NE.0.D0) THEN
          IF (LEPOCC) THEN
              MJDS=EMJDS0(1)
              CALL PLANPC(MJDS,EFSEC0(1),.TRUE.,.TRUE.,II,CB,TB)
              !WRITE(6,*) ' TEST PLANPC CALCULATIONS'
              !WRITE(6,*) ' BARYC: CBODY  ', CB
              !WRITE(6,*) ' BARYC: TBODY  ', TB
              DO I=1,6
                  PSTAE0(I)=CB(I)
                  !WRITE(6,*) ' BARYC : I , EPOCH   PSTAE0  ', PSTAE0(I)
              END DO
              !WRITE(6,*) ' BARYC : INIT. EPOCH  EMJDS0 ', EMJDS0(1),EFSE
          END IF
          IXCB=IPLNIN(ICBDGM)
          !WRITE(6,*) ' BARYC : IXCB  IS IT 2???  ', IXCB
          DO IE=1,NM
              DO IP=1,6
                  PSTATC(IP)=PSTAT(IE,IP)
              END DO
              DELT=DBLE(MJDSEC)+FSEC(IE)-EMJDS0(1)-EFSEC0(1)
              CALL EPHCOR(PSTAE0,PSTATC,DELT,BDSTAT(7,8), &
                          BDSTAT(7,IXCB),DXDEBC)
              !WRITE(6,*) ' BARYC : LEPOCH CNT BD   ', LEPOCC
              DO J=1,6
                  DO K=1,6
                      PSTAT(IE,J)=PSTAT(IE,J)+DXDEBC(J,K)*DEBC(K)
                      !WRITE(6,*) ' UPDATED PSTAT(IE,J)  ',IE, J, PSTAT(I
                      M=(J-1)*6+K
                      !WRITE(6,*) ' BARYC : M INDEX =  ',M
                      PARCBD(IE,M)=DXDEBC(J,K)
                  END DO
              END DO
          END DO
      END IF

      !  CONTINUE HERE WHEN NOT CORRECTING THE CENTRAL BODY POSITION ***
      !  ________________________________________________________________


      ! CALCULATE SUN COORDINATES
      IF(LSUNV) THEN
          DO I=1,NM
              CHEBS(I,1)  = ONE
              DO J=1,3
                  PSUN(I,J)  = ZERO
              END DO
          END DO
          FSECC1=FSECNP(ICHBOG(2,11))
          RNSEC=ONE/FNSEC(ICHBOG(2,11))
          DO I=1,NM
              TTT(I)=((MJDSEC-FSECC1)+FSEC(I))*RNSEC
              TTT(I)=TTT(I)*TWO-ONE
          END DO

          ! COMPUTE CHEBYSHEV POLYNOMIALS AT APPROPRIATE TIMES
          CALL CHEBPV(CHEBS,CHEBSV,TTT,IORSUN,NM,TWOT,.TRUE.)

          ! COMPUTE POSITION
          DO I=1,3
              DO J=1,IORSUN
                  DO K=1,NM
                      PSUN(K,I) = PSUN(K,I)+COEFS(I,J)*CHEBS(K,J)
                  END DO
              END DO
          END DO
      ENDIF

      ! ADD ASTEROID STATE TO SATELLITE STATE
      DO I = 1, NM
          DO J = 1, 3
              XPC(I,J) = XPC(I,J) + AA(KASTO+I-1+(J-1)*MINTIM)
          END DO
          IF (LVEL) THEN
              DO J = 1, 3
                  VPC(I,J) = VPC(I,J) + AA(KASTO+I-1+(J+3-1)*MINTIM)
              END DO
          END IF
      END DO
      ! ADD SUN POSITION TO SATELLITE POSITION
      XPC(1:NM,1:3) = XPC(1:NM,1:3) + PSUN(1:NM,1:3)
      ! ADD SUN VELOCITY TO SATELLITE VELOCITY
      IF (LVEL) THEN
          DO I = 1, NM
              DO J = 1, 3
                  DO K = 1, IORSUN
                      VPC(I,J) = VPC(I,J) + COEFS(J,K)*CHEBSV(I,K)*     &
     &                        TWO/FNSEC(1)
                  END DO
              END DO
          END DO
      END IF

      IF(LSUNV) THEN
          DO I=1,NM
              PSUN(I,1)=PSUN(I,1)-XPC(I,1)
              PSUN(I,2)=PSUN(I,2)-XPC(I,2)
              PSUN(I,3)=PSUN(I,3)-XPC(I,3)
          END DO
      END IF

      RETURN
      END
