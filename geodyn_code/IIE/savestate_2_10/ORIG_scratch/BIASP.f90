!$BIASP
      SUBROUTINE BIASP (AA    ,PMPA  ,NDIM1 ,NDIM2 ,OBS ,TPARTL,        &
     &    OBSTIM,DTIME ,PRMLBL,NUMOBS,LNPNM,LAVOID,OBSC)
!********1*********2*********3*********4*********5*********6*********7**
! BIASP            01/31/89            8805.06   PGMR - TVM
!
! FUNCTION:   Compute the measurement bias partials and load the
!             appropriate locations in the A-matrix
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA
!   PMPA               A-matrix = partial derivatives of measurements
!                      w.r.t adjusted paramters.
!   NDIM1              First dimension of PMPA array.
!   NDIM2              Second dimension of PMPA array.
!   OBS                Observation values.
!   TPARTL             Partial derivative of observations w.r.t time
!   OBSTIM             Observation time in elapsed seconds from MJDSBL
!                      in common CBLOKI
!   DTIME
!   PRMLBL             Parameter labels. For biases, PRMLBL(2,*)
!                      contains the UTC start time in MJDS form for the
!                      bias application interval.
!   NUMOBS             Number indicating which of types for this
!                      observation block For MTYPE .gt. 12 and .lt. 31
!                      this parameter can take on a value of 1 or 2.
!                      For all other measurement types =1.
!   LNPNM              Logical used to indicate if optimization is
!                      over the no. of parameters (true) or the numnber
!                      of measurements (false) True implies NDIM1 = the
!                      number of parameters. False implies NDIM1 = the
!                      number of measurements.
!
!  COMMENTS: BIAS PROCESSING ADDED FOR VLBI MEASUREMENTS AS FOLLOWS:
!   L.T.     - SIMPLE MEASUREMENT CONSTANT BIAS
!  11.06.91  - TROPOSPHERIC SCALE BIAS
!            - STATION CLOCK POLYNOMIALS
!            - ! TIMING BIAS OMMITED!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER( ZERO   = 0.0D0 )
      PARAMETER( HALF   = 0.5D0 )
      PARAMETER( ONE    = 1.0D0 )
      PARAMETER( TEN10  = 1.0D+10)
      PARAMETER( METRIX = 37 )
!
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
      COMMON/CBIASA/BIASE (4),BIASM (2),BSCALE(1)  ,TBIAS (1),          &
     &              BSTROP(3),BSIONO(3),CLKSTA(4,2),CLKSAT(4,2),        &
     &              CLKSTS(2,2),TROPZE(2,2),TROPGR(2,4),BSLBIA(1)
      COMMON/CBIASI/IEBIAS(4),IMBIAS(2),IBSCAL(1)  ,ITBIAS(1),          &
     &              IBTROP(3),IBIONO(3),KLKSTA(4,2),KLKSAT(4,2),        &
     &              KLKSTS(2,2),ITRPZE(2,2),ITRPGR(2,4),IBSBIA(1)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CYCSLP/LCYCTB
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
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
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IOBTYP/NDXST(2,35),NDXSAT(2,35),                           &
     &      NDRSTA(50),NDRSAT(50),NDRST2(50),NDRSA2(50),                &
     &      ITMBIT(999),IOETR(999),MTYPED(11),NTYPES,                   &
     &      NM0112(4),NM1314(4),NM1522(4),NM2330(4),                    &
     &      NM3138(4),NM3997(4),NM4098(4),NM9900(4),NMT199(4),NMT203(4),&
     &      NS0112(4),NS1314(4),NS1522(4),NS2330(4),                    &
     &      NS3138(4),NS3997(4),NS4098(4),NS9900(4),NST199(4),NST203(4),&
     &      ITYPD(12,2),ITYPDD(7,2),ILINKF(5),ISTAFN(5,3),              &
     &      ISATFN(5,3),ILINKT(5),ISTATN(5,3),ISATTN(5,3),              &
     &      MGOTO(12),MENTER(12),MEXIT(12),                             &
     &      NDST60(3,24),NDST82(6,12),NDST92(12,7),NTDRSS,              &
     &      NDSA60(3,24),NDSA82(6,12),NDSA92(12,7),                     &
     &      MTDRSS(7),KTDRSS(7),ITDRSS(4,7),JTDRSS(4,7),                &
     &      NLLC60(5,24),NLLC82(10,12),NLLC98(20,7),NNBIA,              &
     &      NXMTYP
      COMMON/IRAMPS/INDRAM,KKRAMP
      COMMON/LBTYPE/LCLOCK
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
      COMMON/RAMPS/TRINT(1000,2,2),FRINT(1000,2,2),TRAMP(1000,2),       &
     &             FRAMP(1000),FDRAMP(1000),RSCALX(1000),SCALX(1000),   &
     &             XINTS(1000),XINTB(1000),XINTS2(1000),RSCALY(1000)
!
      DIMENSION AA(1)
      DIMENSION OBSTIM(NM),DTIME(NM),PRMLBL(3,1)
      DIMENSION OBSC(NM)
      DIMENSION PMPA(NDIM1,NDIM2),OBS(NM),TPARTL(NM,2)
      DIMENSION LAVOID(1)
!
!
!********1*********2*********3*********4*********5*********6********
! START OF EXECUTABLE CODE *****************************************
!********1*********2*********3*********4*********5*********6********
!
      L1WIP=.FALSE.
      IF(MTYPE.EQ.42.AND.ICBDGM.NE.ITBDGM) L1WIP=.TRUE.
!     IF(L1WIP) GO TO 6000
      IF(L1WIP) GO TO 1500
      DO 10 I=1,8
      IF(KLKSAT(I,1).GT.0) LAVOID(KLKSAT(I,1))=.FALSE.
!     write(6,*)' dbg CHECK 1',KLKSAT(I,1),I,1,LAVOID(KLKSAT(I,1))
      IF(LSPL85.AND..NOT.LAVOID(KLKSAT(I,1))) THEN
! first link OK PMPA further down +1.D0
      DERIV=ZERO
       IF(I.EQ.2) DERIV=2.D0
       IF(I.EQ.4) DERIV=2.D0
       IF(I.EQ.7) DERIV=2.D0
        IF(LNPNM) GO TO 7
        DO  N=1,NM
        PMPA(N,KLKSAT(I,1))=DERIV
        ENDDO
    7   CONTINUE
        DO  N=1,NM
        PMPA(KLKSAT(I,1),N)=DERIV
!       write(6,*)' dbg 1 PMPA ',PMPA(KLKSAT(I,1),N),KLKSAT(I,1),N
        ENDDO
      ENDIF
      IF(KLKSTA(I,1).GT.0) LAVOID(KLKSTA(I,1))=.FALSE.
   10 END DO
1500  CONTINUE
      IF(LNPNM) GO TO 5000
! PARTIAL DERVIATIVE ARRAY DIMENSIONED (NM,NADJST)
!
! CONSTANT BIAS
      INDEXB=IMBIAS(NUMOBS)
      IF(INDEXB.LE.0) GO TO 300
      IRA=INDEXB-IPVAL0(IXBISA)+1
      IF(NAMBB.LE.0.OR.LCYCTB) IRA=INDEXB
      LAVOID(IRA)=.FALSE.
      IF(INDRAM.GT.0) THEN
         DO 100 N=1,NM
         PMPA  (N,IRA)=RSCALX(N)*2.D0
  100    CONTINUE
         GO TO 6000
      ELSE
         DO 200 N=1,NM
         PMPA  (N,IRA)=ONE

          if(mtype.eq.40) then
           IF (ABS(BIASM (NUMOBS)).LT.100.D0) THEN
           PMPA  (N,IRA)= ONE-(OBSC(N)/VLIGHT)
           ELSE
           PMPA  (N,IRA)= ONE
           ENDIF
          endif

  200    CONTINUE
      ENDIF
  300 CONTINUE
      IF(MTYPE.EQ.31.OR.MTYPE.EQ.32) GO TO 6000
      IF(MTYPE.LT.METRIX) GO TO 500
      INDEXB=IMBIAS(2)
      IF(INDEXB.LE.0) GO TO 500
      IF(MTYPE.GT.60.AND.MTYPE.LT.99) GO TO 400
! SCALE FACTOR (METRIC TYPES .LT. 60)
!      print *,'biasp: scale factor bias indexb: ',indexb
!      FACTOR=ONE/(ONE+BIASM (2))
      LAVOID(INDEXB)=.FALSE.
      DO 350 N=1,NM
!      PMPA  (N,INDEXB)=OBS(N)*FACTOR
      PMPA  (N,INDEXB)=OBS(N)
  350 END DO
      GO TO 500
! SECOND CONSTANT BIAS (DIFFERENCED MEASUREMENTS)
  400 CONTINUE
      IRA=INDEXB-IPVAL0(IXBISA)+1
      IF(NAMBB.LE.0) IRA=INDEXB
      LAVOID(IRA)=.FALSE.
      DO 450 N=1,NM
      PMPA  (N,IRA)=-ONE
  450 END DO
  500 CONTINUE
! TIMING BIAS AT  PRIMARY  RECEIVER
      INDEXB=ITBIAS(1)
      IF(INDEXB.LE.0) GO TO 700
!      print *,'biasp: timing bias at prim receiver indexb: ',indexb
      LAVOID(INDEXB)=.FALSE.
      DO 600 N=1,NM
      PMPA  (N,INDEXB)=TPARTL(N,1)
  600 END DO
  700 CONTINUE
! TIMING BIAS AT SECONDARY RECEIVER
      INDEXB=IBSCAL(1)
      IF(INDEXB.LE.0) GO TO 900
      LAVOID(INDEXB)=.FALSE.
!      print *,'biasp: timing bias at seco receiver indexb: ',indexb
      DO 800 N=1,NM
      PMPA  (N,INDEXB)=TPARTL(N,2)
  800 END DO
      IF(MTYPE.EQ.85) THEN
         DO 805 N=1,NM
         PMPA  (N,INDEXB)=0.D0
  805    CONTINUE
      ENDIF
  900 CONTINUE
      GO TO 6000
 5000 CONTINUE
! PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
!
! CONSTANT BIAS
      INDEXB=IMBIAS(NUMOBS)
      IF(INDEXB.LE.0) GO TO 5300
      IRA=INDEXB-IPVAL0(IXBISA)+1
      IF(NAMBB.LE.0.OR.LCYCTB) IRA=INDEXB
      LAVOID(IRA)=.FALSE.
!     write(6,*)' dbg CHECK ',LAVOID(IRA),IRA
      IF(INDRAM.GT.0) THEN
         DO 5100 N=1,NM
         PMPA(IRA,N)=RSCALX(N)*2.D0
 5100    CONTINUE
       GO TO 6000
      ELSE
!
         DO 5200 N=1,NM
         PMPA(IRA,N)=ONE

          if(mtype.eq.40) then
           IF (ABS(BIASM (NUMOBS)).LT.100.D0) THEN
           PMPA  (IRA,N)= ONE-(OBSC(N)/VLIGHT)
           ELSE
           PMPA  (IRA,N)= ONE
           ENDIF
          endif

         IF(LSPL85) PMPA(IRA,N)=2.D0
!        write(6,*)' dbg PMPA ',PMPA(IRA,N),IRA,N
 5200    CONTINUE
      ENDIF
 5300 CONTINUE

      IF(INDRAM.GT.0) RETURN
      IF(MTYPE.EQ.31.OR.MTYPE.EQ.32) GO TO 6000
      IF(MTYPE.LT.METRIX) GO TO 5500
      INDEXB=IMBIAS(2)
      IF (INDEXB.LE.0) GO TO 5500
      IF(MTYPE.GT.60.AND.MTYPE.LT.99) GO TO 5400
! SCALE FACTOR (METRIC TYPES .LT. 60)
!      FACTOR=ONE/(ONE+BIASM (2))
      LAVOID(INDEXB)=.FALSE.
      DO 5350 N=1,NM
!      PMPA(INDEXB,N)=OBS(N)*FACTOR
      PMPA(INDEXB,N)=OBS(N)
 5350 END DO
      GO TO 5500
 5400 CONTINUE
! SECOND CONSTANT BIAS (DIFFERENCED MEASUREMENTS)
      IRA=INDEXB-IPVAL0(IXBISA)+1
      IF(NAMBB.LE.0) IRA=INDEXB
      LAVOID(IRA)=.FALSE.
      DO 5450 N=1,NM
      PMPA  (IRA,N)=-ONE
 5450 END DO
 5500 CONTINUE
! TIMING BIAS AT  PRIMARY  RECEIVER
      INDEXB=ITBIAS(1)
      IF(INDEXB.LE.0) GO TO 5700
      LAVOID(INDEXB)=.FALSE.
      DO 5600 N=1,NM
      PMPA(INDEXB,N)=TPARTL(N,1)
 5600 END DO
 5700 CONTINUE
! TIMING BIAS AT SECONDARY RECEIVER
      INDEXB=IBSCAL(1)
      IF(INDEXB.LE.0) GO TO 5900
      LAVOID(INDEXB)=.FALSE.
      DO 5800 N=1,NM
      PMPA  (INDEXB,N)=TPARTL(N,2)
 5800 END DO
      IF(MTYPE.EQ.85) THEN
         DO 5805 N=1,NM
         PMPA  (INDEXB,N)=0.D0
 5805    CONTINUE
      ENDIF
 5900 CONTINUE
 6000 CONTINUE
      IF(MTYPE.LT.15) RETURN
!  *** ADD VLBI TROP. SCALE BIAS   *****  LT 11.06.91  ******
      IF(MTYPE.GT.22.AND.MTYPE.LT.31) RETURN
      IF(MTYPE.EQ.99.OR.MTYPE.EQ.100.OR.MTYPE.EQ.199) RETURN
! STATION REFRACTION SCALE BIASES
      ID=3
      IW=4
      II=6
      IF(MTYPE.GE.31) GO TO 7000
      ID=2+NUMOBS
      IW=4+NUMOBS
      II=6+NUMOBS
 7000 CONTINUE
!...TROPOSPHERE (DRY+WET)
      DO 9000 I=1,3
      INDEXB=IBTROP(I)
      IF(INDEXB.LE.0) GO TO 8000
      LAVOID(INDEXB)=.FALSE.
!      WRITE(6,*)  '  BIASP :  CALL PMPTRP  '
      CALL PMPTRP(PMPA,NDIM1,NDIM2,INDEXB,NM,                           &
     &   AA(KOBCOR(ID,I)),AA(KOBCOR(IW,I)),MTYPE,LNPNM)
 8000 CONTINUE
      IF(INDRAM.GT.0) GO TO 9000
      IF(L1WIP) GO TO 9000
!...IONOSPHERE
      INDEXB=IBIONO(I)
      IF(INDEXB.LE.0) GO TO 9000
      LAVOID(INDEXB)=.FALSE.
      CALL PMPION(PMPA,NDIM1,NDIM2,INDEXB,NM,                           &
     &   AA(KOBCOR(II,I)),                 LNPNM)
 9000 END DO
!  *** ADD VLBI STATION CLOCKS  *****  LT 11.06.91  ******
      IF(MTYPE.LT.31) RETURN
!
!   STATION CLOCK POLYNOMIALS
!     WRITE(6,*)  'BIASP:  *** CALL PMPCLK -STATION CLOCK '
!
!      print *,'biasp: calling station pmpclk'
      CALL PMPCLK(PMPA,NDIM1,NDIM2,NDXST,KLKSTA,OBSTIM,DTIME,           &
     &   PRMLBL,LNPNM)
      IF(INDRAM.GT.0) RETURN
      IF(L1WIP) RETURN
!
      IF(MTYPE.LT.METRIX) RETURN
!
! SATELLITE CLOCK POLYNOMIALS
!
      IF(.NOT.LCLOCK)THEN
!      print *,'biasp: calling pmptds'
         CALL PMPTDS(PMPA,NDIM1,NDIM2,NDXSAT,KLKSAT,OBSTIM,DTIME,       &
     &               PRMLBL,LNPNM,AA(KOBSTR))
       ELSE
!      print *,'biasp: calling satellite pmpclk'
          CALL PMPCLK(PMPA,NDIM1,NDIM2,NDXSAT,KLKSAT,OBSTIM,DTIME,      &
     &               PRMLBL,LNPNM)
      ENDIF
      RETURN
      END
