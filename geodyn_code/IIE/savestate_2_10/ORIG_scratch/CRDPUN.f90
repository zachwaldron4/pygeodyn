!$CRDPUN
      SUBROUTINE CRDPUN(AA    ,II    ,LL    ,IARC  ,ITRGLB,ITRARC,      &
     &                  PARMVC,PARMSG,LAFRC ,IPDFRC,IORFRC,ITACC ,      &
     &                  ISATID,NSATST,STDRG ,STSRD ,STACC ,ETUTC ,      &
     &                  IPTRUA,INDPI ,IPOLCD,PUTBIH,IELMTP,ISTANO,ICRD, &
     &                  PRMLBL,ITACCX,IMBSAT,IMBSTA,ICNL2,ICNH2,ICNV,   &
     &                  SAVSTA)
!********1*********2*********3*********4*********5*********6*********7**
! CRDPUN           85/04/23            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  CREATE NEW SETUP DECK BY READING ORIGINAL IIS SETUP
!            DECK AND UPDATING OPTION CARDS WITH ADJUSTED
!            PARAMETERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   IARC     I    S    CURRENT ARC NUMBER(IARC=0 FOR GLOBAL SET)
!   ITRGLB   I    S    GLOBAL ITERATION NUMBER
!   ITRARC   I    S    ARC (INNER) ITERATION NUMBER
!   PARMVC   I    A    CORRECTED PARAMETER VALUES ARRAY
!   PARMSG   I    A    PARAMETERS SIGMA ARRAY
!   LAFRC    I    A    LOGICAL INDICATORS FOR ADJUSTMENT OF DRAG,SOLAR
!                      RADIATION AND GENERAL ACCELERATION
!   IPDFRC   I    A    ORDER OF FORCE MODEL FOR DRAG SOL. RAD & GEN. ACC
!   IORFRC   I    A    NUMBER OF DISCRETE PERIODS USED BY THE HIGHEST
!                      ORDER EFFECT IN DRAG, DOL. RAD & GEN. ACC
!   ITACC    I    A    TYPE OF ACCELERATION (1=ALONG TRACK, 2=RADIAL,
!                      3=ACCROSSTRACK).
!   ISATID   I    A    SATELLITE ID
!   NSATST   I    A    NUMBER OF SATELLITES IN EACH SET
!   STDRG    I    A    STOP TIMES OF CORRECTION PERIODS FOR DRAG
!   STSRD    I    A    STOP TIMES OF CORRECTION PERIODS FOR SOLAR
!                      RADIATION
!   STACC    I    A    STOP TIMES OF CORRECTION PERIODS FOR GEN. ACC
!   ETUTC    I    A    ARRAY OF ET-UTC DIFFERENCES
!   IPTRUA   I    A    INDICES RELATING UNADJUSTED TO ADJUSTED
!                      PARAMETERS
!   INDPI    I    A    INDEX ARRAY FOR POLAR MOTION
!   IPOLCD   I    A    RELATES POLE INTERVALS TO ORIGINAL POLE CARDS
!   PUTBIH   I    A    BIRL VALUES FOR POLE/UT AT POLEUT TIMES
!   IELMTP   I    A    ARRAY TELLING WHICH COORDINATE SYSTEM THE INPUT
!                      ELEMENTS ARE
!   ISTANO   I    A    ARRAY CONTAINING STATION NUMBERS
!   ICRD     I    A    INDICES RELATED TO STATION ORDERING
!   PRMLBL   I    A    PARAMETER LABEL ARRAY
!   ITACCX
!   IMBSAT  I    A    MBIAS SAT ID
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER( IZERO = 0 )
      PARAMETER( HALF = 0.5D0 )
      PARAMETER( C0D0 = 0.0D0 )
      PARAMETER( C1D2 = 1.0D2 )
      PARAMETER( C1D6 = 1.0D6 )
      PARAMETER( C1DM5 = 1.0D-5 )
      PARAMETER( C1D10 = 1.0D10 )
      PARAMETER( C1D13 = 1.0D13 )
      PARAMETER( C6D1 = 6.0D1 )
      PARAMETER( NOPTS = 45 )
      PARAMETER( NOPTSS = 3 )
      PARAMETER( NASAVE = 500 )
!
      CHARACTER*80 ALINE, ASAVE(NASAVE)
      CHARACTER*80 CDNAMX
      CHARACTER*80 XXNAME
      CHARACTER*8  XDNAME
      CHARACTER*8  XTNAME
      CHARACTER  STR*128
!
      LOGICAL*4  FL_QUAPOS
!
      COMMON/ATTCNT/KATUD,KATBL,KATID,KATAD
      COMMON/CEBARC/NEBIAS,NBIASE,NXCEBA
      COMMON/CEDIT /EDITX ,EDTRMS,EDLEVL,CONVRG,GLBCNV,EBLEVL,EDITSW,   &
     &              ENPX  ,ENPRMS,ENPCNV,EDBOUN,FREEZI,FREEZG,FREEZA,   &
     &              XCEDIT
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CORA01/KFSEC0,KFSECB,KFSEC ,KFSECV,KH    ,KHV   ,KCTOL ,   &
     &              KRSQ  ,KVMATX,KCPP  ,KCPV  ,KCCP  ,KCCV  ,KCCPV ,   &
     &              KCCVV ,KXPPPP,KX    ,KPX   ,KSUMX ,KXDDOT,KSUMPX,   &
     &              KPXDDT,KAB   ,KPN   ,KAORN ,KSINLM,KCOSLM,KTANPS,   &
     &              KCRPAR,KVRARY,KXM   ,KXNP1 ,KXPRFL,KXM2  ,KXNNP1,   &
     &              KWRK  ,KFI   ,KGE   ,KB0DRG,KBDRAG,KAPGM ,KAPLM ,   &
     &              KCN   ,KSN   ,KSTID ,KTIDE ,KSTDRG,KSTSRD,KSTACC,   &
     &              KLGRAV,KGM   ,KAE   ,KFPL  ,KFEQ  ,KPLNPO,KPLNVL,   &
     &              KXEPOC,KCD   ,KCDDOT,KCR   ,KGENAC,KACN  ,KASN  ,   &
     &              KTHDRG,KCKEP ,KCKEPN,KXNRMZ,KXNRMC,KFSCEP,KFSCND,   &
     &              KAREA ,KXMASS,KRMSPO,KTCOEF,KTXQQ ,KTIEXP,KTXMM ,   &
     &              KTXLL1,KTXSN1,KTS2QQ,KT2M2H,KT2MHJ,KTXKK ,KTSCRH,   &
     &              KPXPK ,KAESHD,KCSAVE,KSSAVE,KCGRVT,KSGRVT,KXDTMC,   &
     &              KDNLT ,KTXSN2,KTNORM,KTWRK1,KTWRK2,KUNORM,KAERLG,   &
     &              KSINCO,KPARLG,KCONST,KBFNRM,KTDNRM,KCSTHT,KTPSTR,   &
     &              KTPSTP,KTPFYW,KPLMGM,KTPXAT,KEAQAT,KEAFSS,KEAINS,   &
     &              KACS  ,KECS  ,KSOLNA,KSOLNE,KSVECT,KSFLUX,KFACTX,   &
     &              KFACTY,KADIST,KGEOAN,KPALB ,KALBCO,KEMMCO,KCNAUX,   &
     &              KSNAUX,KPPER ,KACOSW,KBSINW,KACOFW,KBCOFW,KANGWT,   &
     &              KWT   ,KPLNDX,KPLANC,KTGACC,KTGDRG,KTGSLR,KWTACC,   &
     &              KWTDRG,KWTSLR,KTMACC,KTMDRG,KTMSLR,KATTUD,KDYACT,   &
     &              KACCBT,KACPER,KXDDNC,KXDDAO,KXNC  ,KXPPNC,KSMXNC,   &
     &              KXDDTH,KPDDTH,KXSSBS,KCPPNC,KEXACT,KXACIN,KXACOB,   &
     &              KPXHDT,KTPXTH,KPACCL,KTXSTA,KDELXS,KSMRNC,KPRX  ,   &
     &              KSMRNP,KDSROT,KXUGRD,KYUGRD,KZUGRD,KSUMRC,KXDDRC,   &
     &              KTMOS0,KTMOS, KTMOSP,KSMXOS,KSGTM1,KSGTM2,KSMPNS,   &
     &              KXGGRD,KYGGRD,KZGGRD,KXEGRD,KYEGRD,KZEGRD,KSSDST,   &
     &              KSDINS,KSDIND,KSSDSR,KSSDDG,KTATHM,KTAINS,KTAFSS,   &
     &              KSRAT ,KTRAT ,KHLDV ,KHLDA1,KHLDA4,KHLDA7,KQAST1,   &
     &              KQAST2,KQAST3,KQAST4,KQAST5,KQAST6,NXCA01
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
      COMMON/CORI01/KMJDS0,KMJDSB,KMJDSC,KMJDSV,KIBACK,KIBAKV,KIORDR,   &
     &              KIORDV,KNOSTP,KNOCOR,KNSAT ,KN3   ,KNEQN ,KNEQN3,   &
     &              KNH   ,KNHV  ,KNSTPS,KNSTPV,KICPP ,KICPV ,KICCP ,   &
     &              KICCV ,KICCPV,KICCVV,KISUMX,KIXDDT,KISMPX,KIPXDD,   &
     &              KNMAX ,KNTOLD,KNTOLO,KICNT ,KISNT ,KMM   ,KKK   ,   &
     &              KJJ   ,KHH   ,KIBDY ,KSIGN1,KSIGN2,KLL   ,KQQ   ,   &
     &              KIORFR,KIPDFR,KITACC,KJSAFR,KJSPFR,KMJDEP,KMJDND,   &
     &              KNVSTP,KNSTRN,KNDARK,KTIPPT,KTJBDY,                 &
     &              KICNTA,KISNTA,KISHDP,KIPTC ,KIPTS ,KIGTSR,KIXTMC,   &
     &              KTIPTT,KTNOSD,KTQNDX,KTLL1 ,KTJJBD,KTITDE,KTCNTR,   &
     &              KTNN  ,KITACX,KNMOVE,KPANEL,KPLPTR,KNADAR,KNADSP,   &
     &              KNADDF,KNADEM,KNADTA,KNADTC,KNADTD,KNADTF,KNADTX,   &
     &              KITPMD,KSCATT,KITPAT,KILTPX,KEASBJ,KEANMP,KEANAN,   &
     &              KEAPMP,KEAPAN,KEAMJS,KEAPPP,KEAAAA,KICNTT,KISNTT,   &
     &              KTPGRC,KTPGRS,KTPC  ,KTPS  ,KALCAP,KEMCAP,KNSEG ,   &
     &              KICNTP,KISNTP,KNRDGA,KNRDDR,KNRDSR,KIRDGA,KIRDRG,   &
     &              KIRSLR,KSTRTA,KSTRTD,KSTRTS,KDYNPE,KACCPE,KIBCKN,   &
     &              KNRAT ,KIXDDN,KISMXN,KDXDDN,KDSMXN,KICPPN,KACSID,   &
     &              KNEQNH,KHRFRC,KPTFBS,KPTFSB,KIPXDA,KIACCP,KXSTAT,   &
     &              KPXST ,KSALST,KMAPLG,KNMBUF,KSTEPS,KSGMNT,KSATIN,   &
     &              KMEMST,KNEQNI,KBUFIN,KWEMGA,KWEMDR,KTPATS,KTANMP,   &
     &              KTAPPP,KTAMJS,KTASID,KGPSID,KNSSVA,KPNALB,KBRAX1,   &
     &              KBRAX2,KBRAX3,NXCI01
      COMMON/CORI04/KNMP  ,KINDP ,KNDPAR,KNPVCT,KISATN,KISET ,KIANTO,   &
     &              KISATO,KIANTD,KISATD,KISTAD,KISATC,KMJDCG,KISTAT,   &
     &              KMJDEB,KMJDBN,KNDBIN,KNWTDB,KIXPAR,KNDBUF,KSSTNM,   &
     &              KSSTNA,KMBSAT,KMBSTA,KXKEY ,KXVKEY,KXFLAG,KIPNTF,   &
     &              KIPNTS,KNCON ,KKF   ,KIDATB,KNSTLV,KSLVID,KLLBIA,   &
     &              KTMRA1,KTMRA2,KIND1 ,KTARID,KATARD,KYAWID,KXOBLK,   &
     &              KDSCWV,KATRSQ,KATSAT,KKVLAS,KPARTP,KLTMSC,KLTASC,   &
     &              KCTBST,KSTBST,KANCUT,KANGPS,KANTYP,                 &
     &              KANTOF,                                             &
     &              KYSAT, KMBDEG,                                      &
     &              KMBNOD,KMBSST,KBSPLN,KSSPLN,KXIOBS,KIDLAS,KIAVP ,   &
     &              KXNAVP,KNEXCG,KIDEXC,KNREXC,KTELEO,KIKEY ,KIMKEY,   &
     &              KIMBLK,KIMPRT,KCN110,KCN111,KC110,KC111,KTDSAT,     &
     &              KTDANT,NXCI04
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/IDELTX/MXSTAT,NEVENTS,NDSTAT,IDSYST,NDELTX
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
      COMMON/PUNCH /LPNCH ,LPNALL,LPNADJ,LPNITR,NXPUNC
      COMMON/PLMONI/IMNNUM(5,2),NXPLMI
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/TITLEC/TITLEG(10,3),TITLEA(10,10),XTITLE
      COMMON/TITLER/TITLEN,XTITLR
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      COMMON/UCLJAL/LJBUS(50),LJPAN(50),LJTRR(50),LUGFO,LENVI,NUCLJA
      COMMON/UCLJAI/IUCLSAT(50),NUCLJAI
!
      CHARACTER(8)      :: ECHO
      CHARACTER(1)      :: IA3
      CHARACTER*80 CDS
      DIMENSION AA(1),BIAS(2),CDSG(5),ECHO(10),ETUTC(1)
      DIMENSION FIELD(4),FSEC(1),FSECOT(1),FSEC1(1)
      DIMENSION II(1),IELMTP(1),ISTANO(1),ICRD(1)
      DIMENSION IFIELD(8),IPTRUA(1),IPOLCD(1),INDPI(1)
      DIMENSION IFLD15(5),IFLD78(2),IA3(3)
      DIMENSION IPDFRC(3,1),IORFRC(3,1),ITACC(1),ISATID(1)
      DIMENSION JYMD(1),JHM(1)
      DIMENSION LL(1),LAFRC(4,3,1)
      DIMENSION NSATST(1)
      DIMENSION PARMVC(1),PARMSG(1),PUTBIH(3,1),PRMLBL(3,1)
      DIMENSION PBIAS(2),RF(6)
      DIMENSION STDRG(1),STSRD(1),STACC(1),STACDS(14)
      DIMENSION ITACCX(1),IMBSAT(3,1),IMBSTA(3,1)
      DIMENSION OPTION( NOPTS )
      DIMENSION OPTNSP( NOPTSS )
      DIMENSION ICNL2(NSTA),ICNH2(NSTA),ICNV(NSTA)
!
      DIMENSION ksave(NASAVE)
      DIMENSION SAVSTA(NSTA,3)
!
      EQUIVALENCE (IFIELD(1),IFLD15(1)),(IFIELD(7),IFLD78(1))
      EQUIVALENCE(CDNAME,XDNAME)
      EQUIVALENCE(XTNAME,XXNAME)
!
      CHARACTER(8)      :: CDNAME
      CHARACTER(8)      :: OPTION
      DATA OPTION/'DATA  ','ENDARC','ENDGLB','MBIAS ','MBIAS1',         &
     &            'MBIAS2','MBIAS3','PBIAS ','PBIAS1','PBIAS2',         &
     &            'PBIAS3','REFSYS','SATPAR','STAPOS','EARTH ',         &
     &            'TIDES ','ETIDES','OTIDES','GCOEF ','GCOEFC',         &
     &            'GCOEFS','H2LOVE','L2LOVE','POLEUT','ACCEL ',         &
     &            'DRAG  ','SOLRAD','PLANET','VLIGHT','EDIT  ',         &
     &            'EBIAS ','ETIDEN','OTIDEN','THRDRG','ACCEL9',         &
     &            'ACCELT','PLMOON','PANEL ','GCOFCT','GCOFST',         &
     &            'GTPEND','ATITUD','YAWPOL','LOCALG','DSTATE'/

!
      CHARACTER(8)      :: OPTNSP
      DATA OPTNSP/'ORBFIL','TOPATT','TOPBTM'/
!**********************************************************************
      CHARACTER(8)      :: ENDARC, ENDGLB
      DATA ENDARC/'ENDARC  '/,ENDGLB/'ENDGLB'/
! BIAS CARDS
      CHARACTER(8)      :: BIAS, PBIAS
      DATA BIAS /'BIAS2 ','BIAS3 '/
      DATA PBIAS/'PBIAS2 ','PBIAS3 '/
! REFSYS CARDS
      CHARACTER(8)      :: REFSYS,SATPAR,EPOCH,ELEMS1,ELEMS2
      DATA REFSYS,SATPAR,EPOCH,ELEMS1,ELEMS2/'REFSYS','SATPAR',         &
     &     'EPOCH ','ELEMS1','ELEMS2'/
      CHARACTER(8)      :: SLAVE
      DATA SLAVE/'SLAVE '/
! SUBGROUP "STAPOS" CARDS
!     DATA NSTACD/9/
      DATA NSTACD/12/
      CHARACTER(8)      :: ENDSTA
      DATA ENDSTA/'ENDSTA'/
      CHARACTER(8)      :: STACDS
      DATA STACDS/'ADJUSTED','CORREL  ','CONSTADJ','CONSTEND',          &
     &            'FIXED   ','GEODETIC','EXTRAGEO','ELCUTOFF',          &
     &            'INSTRMNT','STAVEL  ','TIMVEL  ','SIGVEL  ',          &
     &            'STATL2  ','STATH2  '/

! CONSTANTS
      CHARACTER(8)      :: GLOBAL, ARC
      DATA GLOBAL/ '/GLOB/ '/  ,ARC/ '/ARC/  '/

!      DATA CDSG/'DRAG  ','SOLRAD','ACCEL ','ACCEL9','ACCELT'/
      CHARACTER(8)      :: CDSG
      DATA CDSG/'DRAG  ','SOLRAD','ACCEL9','ACCEL ','ACCELT'/

      DATA ldebug/.false./
!c      DATA ldebug/.true./
      DATA  kentry/0/
      INTEGER  IOS
      LOGICAL*4  FL_OPVLBI
!
      DATA NUCL/1/
!
!
!**********************************************************************
! START OF EXECUTABLE CODE
!**********************************************************************
!
! INITIALIZE TIME PERIOD GRAVITY FLAG
!
      FL_QUAPOS = .FALSE.
      LTPG=.FALSE.
      NTPGC=0
      NTPGS=0
!
      FL_OPVLBI = .FALSE.
!
      if(ldebug)write(6,*) 'enter crdpun*****',IARC
      lacc99 = .true.
      lacct  = .false.
      lacc   = .false.
      kentry = 0
      do 5 i=1,nasave
      ASAVE(i) = ' '
    5 continue
!
   10 continue
      kentry = kentry +1
!     ....should never go through 10 loop more than 2 times
      if( kentry .gt. 2 ) then
         stop 16
      endif
      NARTTL=TITLEN
      LGLOBL=IARC.LE.0
      REWIND IUNT14
!
!      ldebug = .true.
      ldebug = .false.
!
      if(ldebug)write(6,*) 'crdpun: times in 10 loop ', kentry
      if(ldebug)write(6,*) 'crdpun: rewind unit 14'
!      if(ldebug)write(6,fmt='(1x,''entry ASAVE:'',A80)') ASAVE
!
! POSITION UNIT 14 AT PROPER LOCATION FOR GLOBAL OR ARC SET
      IF(LGLOBL) GO TO 200
! SKIP GLOBAL SET
  100 READ(IUNT14,81200,END=60900) CDNAME
      IF(CDNAME .NE. ENDGLB) GO TO 100
! SKIP TO THE CURRENT ARC NUMBER
      if(ldebug)write(6,*) 'crdpun: arc no. = ', iarc
      IF(IARC.EQ. 1) GO TO 180
      DO 150 JARC=2,IARC
  120 READ(IUNT14,81200,END=60900) CDNAME
      IF(CDNAME .NE. ENDARC) GO TO 120
  150 END DO
!
! PRINT IDENTIFIER FOR ARC SET AND INITIALIZE ARC VARIABLES
!
!----------------------------------------------------------------------
!
  180 continue
!
!     ....get the ACCEL9 99 line for this arc
!
      if( lacc99 ) then
              isave=1
              ASAVE(isave)=' '
              isknt=0
  181         continue
              read(IUNT14,fmt='(A80)') ALINE
              if(ldebug)write(6,fmt='(1x,''ALINE:'',A80)') ALINE
              if(ldebug)write(6,*) 'crdpun: isave, isknt ', isave, isknt
              jj1 = index(ALINE,'ACCEL9')
              jj2 = index(ALINE(11:12),'99')
!
!c              if( jj1 .ne. 0 ) isknt = isknt + 1
              if( jj1 .ne. 0  .and. jj2 .eq. 0 ) isknt = isknt + 1
!
              if(ldebug)write(6,*) 'crdpun: jj1,jj2 ', jj1, jj2
              if( jj1 .ne. 0 .and. jj2 .ne. 0 ) then
                 ASAVE(isave) = ALINE
                 if(ldebug)write(6,*) 'crdpun: ASAVE set = ALINE'
         if(ldebug)write(6,fmt='(1x, ''ASAVE:'',A80)') ASAVE(isave)
                 lacc99 = .false.
!
                 if( isave .eq. 1) isknt=0
                 if(ldebug)                                             &
     &              write(6,*) 'crdpun:  isknt, isave, ksave(isave) ',  &
     &                     isknt, isave, ksave(isave)
                 ksave(isave) = isknt
                 isknt=0
                 isave=isave+1
!
            endif
              if( index(ALINE(1:6), 'ACCEL ') .ne. 0 ) lacc  = .true.
              if( index(ALINE(1:6), 'ACCELT') .ne. 0 ) lacct = .true.
!
!             ....if you get an ENDARC or ENDALL, no ASAVE for this arc
              if( index(ALINE(1:4), 'ENDA' ) .eq. 0  ) then
                 go to 181
              else
                 lacc99 = .false.
                 go to 10
              endif
      endif
!----------------------------------------------------------------------
!
      WRITE(IOUT7,81000) ARC   ,IARC,ITRGLB,ITRARC
      if(ldebug)write(6,81000) ARC   ,IARC,ITRGLB,ITRARC
      LDSG  =.FALSE.
      LBIAS =.FALSE.
      LEBIAS=.FALSE.
      if(ldebug)write(6,*) 'crdpun: LDSG = ', LDSG
      GO TO 400
!
! PRINT IDENTIFIER FOR GLOBAL SET AND INITIALIZE GLOBAL VARIABLES
!
  200 WRITE(IOUT7,81000) GLOBAL,IARC,ITRGLB
      if(ldebug)write(6,81000) GLOBAL,IARC,ITRGLB
      LADJET=.FALSE.
      LADJOT=.FALSE.
      LGEOP =.FALSE.
      NPOLCD=0
  400 CONTINUE
      if(ldebug)write(6,*) 'crdpun: LDSG = ', LDSG
      IF(LGLOBL) GO TO 500
!
! READ ARC TITLE CARDS
!
      DO 410 I=1,NARTTL
      READ(IUNT14,81050,END=420) ECHO
      WRITE(IOUT7,81050) ECHO
      if(ldebug)write(6,81050) ECHO
  410 END DO
! ARC SET TITLE CARDS MUST BE FOLLOWED BY REFSYS CARD
      if(ldebug)write(6,*) 'crdpun: LDSG = ', LDSG
      GO TO 9000
! END OF FILE; IF GLOBAL SET-ERROR; IF ARC SET- NORMAL END OF JOB
  420 IF(LGLOBL) GO TO 60100
      GO TO 60000
!
! READ GLOBAL TITLE CARDS
!
  500 CONTINUE
      DO 550 I=1,3
      READ(IUNT14,81050,END=420) ECHO
      WRITE(IOUT7,81050) ECHO
      if(ldebug)write(6,81050) ECHO
  550 END DO
!
! READ OPTION CARDS
!
  600 CONTINUE
      READ ( IUNT14,81200,IOSTAT=IOS,END=60000)CDNAME,IFLD15,IA3, &
     &                                         IFLD78, FIELD
      IF ( CDNAME .EQ. 'OPVLBI' ) THEN
      IOS = 0
      FL_OPVLBI = .TRUE.
      END IF
      IF ( CDNAME .EQ. 'QUAPOS' ) THEN
           FL_QUAPOS = .TRUE.   ! set the flag of source position section
      ENDIF
      IF ( CDNAME .EQ. 'ENDQUA' ) THEN
      FL_QUAPOS = .FALSE.  ! set the flag of source position section
      END IF
      IF ( IOS.NE.0.AND..NOT.FL_OPVLBI.AND..NOT.FL_QUAPOS ) THEN
      WRITE ( 6, * )                                                   &
     & 'Error in reading 14-th logical unit: CDNAME=',CDNAME
      END IF
      IF ( CDNAME .EQ. 'ENDOPV' ) THEN
      IOS = 0
      FL_OPVLBI = .FALSE.
      END IF

      if(ldebug)write(6,*) 'crdpun: LDSG = ', LDSG
      if(ldebug)write(6,fmt='(1x,a8, 1x,4D15.5)') cdname,field
!     write(6,fmt='(1x,a8, 1x,4D15.5)') cdname,field
      CALL A3TOI6(IA3,I6,IFIELD(6))
      LPUN=LPNALL
      if(ldebug)write(6,*) 'crdpun: LPUN, LPNALL ',                     &
     &                              LPUN, LPNALL
!
! SET INTEGER FIELD COLUMNS
! IFIELD IS IC78,IC910,IC1112,IC1314,IC1517,IC1820,IC2122,IC2324
      IC78  =IFIELD( 1)
      IC7   =IFIELD( 1)/10
      IC8   =MOD(IFIELD( 1),10)
      IC910 =IFIELD( 2)
      IC9   =IFIELD( 2)/10
      IC10  =MOD(IFIELD( 2),10)
      IC1112=IFIELD( 3)
      IC11  =IFIELD(3)/10
      IC1011=IC10*10+IC11
      IC1314=IFIELD( 4)
      IC12  =MOD(IFIELD(3),10)
      IC1214=IC12*100+IC1314
      IC1114=IC1112*100+IC1314
      IC1517=IFIELD( 5)
      IC1820=IFIELD( 6)
      IC2124=IFIELD( 7)*100+IFIELD( 8)
      IC1824=IFIELD( 6)*10000 +IFIELD( 7)*100 +IFIELD( 8)
!
! TEST FOR OPTION CARDS
!
      ldebug = .false.
      if(ldebug)write(6,fmt='(1x,2a)') 'crdpun: above 700 cdname = ',   &
     &    cdname
!
      DO 700 I=1,NOPTS
      ICARD=I
      IF(CDNAME .EQ.OPTION(I)) GO TO                                    &
     &   (10500, 4000, 5000, 7000, 7000, 7000, 7000, 7000, 7000, 7000,  &
     &     7000, 9000, 9500,12000, 3500,13000, 5200, 7500, 5500, 5700,  &
     &     5800, 6600, 6800, 8700, 1300, 2000,10700, 8500,14000, 3700,  &
     &     8000, 5200, 7500,13500,1300,1300,8600,8200,8400,8410,8420,   &
     &     3000,15000,6900,1100),I
  700 END DO
!
! IF CARD NOT FOUND IN LIST THEN OUTPUT AS IS
!
      GO TO 50000
!
!*********************************************************************
! ** DSTATE  **                                            *
!*********************************************************************
 1100 CONTINUE
      IF(LDSTAT) GO TO 600
      LDSTAT=.TRUE.
      NDSOUT=0
      DO I=1,80
        CDS(I:I)=' '
      ENDDO
      CDS(1:8)=CDNAME
      DO 1150 ISAT=1,NSATA
      NXSTAT=II(KXSTAT-1+ISAT)
      IF(NXSTAT.EQ.0) GO TO 1150
      WRITE(CDS(18:24),89001) ISATID(ISAT)
      DO 1140 IXSTAT=1,NXSTAT
      TQ=AA(KTXSTA-1+(ISAT-1)*MXSTAT+IXSTAT)
      ITIME=TQ
      FSECOT(1)=TQ-DBLE(ITIME)
      CALL UTCET(.FALSE.,1,ITIME,FSECOT,FSEC1,ETUTC)
      CALL YMDHMS(ITIME,FSEC1,JYMD,JHM,FSEC,1)
      JYMD(1)=MOD(JYMD(1),1000000)
      TQ=DBLE(JYMD(1))*1000000.D0
      TQ=TQ+DBLE(JHM(1)*100)+FSEC(1)
      WRITE(CDS(45:59),89002) TQ
      WRITE(CDS(8:8),89003) IDSYST
      DO 1130 I=1,6
      NDSOUT=NDSOUT+1
      IF(NDSOUT.GT.NPVAL0(IXDXYZ)) GO TO 600
      WRITE(CDS(7:7),89003) I
      WRITE(CDS(25:44),89004) PARMVC(IPVAL0(IXDXYZ)-1+NDSOUT)
      WRITE(CDS(60:72),89005) PARMSG(IPVAL0(IXDXYZ)-1+NDSOUT)
      WRITE(IOUT7,89006) CDS
 1130 CONTINUE
 1140 CONTINUE
 1150 CONTINUE
      GO TO 600
!
!*********************************************************************
! ** ACCEL  **   SEE DRAG CARD                             * ACCEL  **
!*********************************************************************
 1300 CONTINUE
      IF(I.EQ.35) THEN
      CALL ACCP(20000,PARMVC,PRMLBL)
      GO TO 600
      ENDIF
!
!      ldebug = .true.
      ldebug = .false.
!
      if(ldebug)write(6,fmt='(1x,2a)') 'crdpun: below 1300 cdname = ',  &
     &    cdname
      if(ldebug)write(6,*) 'crdpun: LDSG = ', LDSG
!
      GO TO 2000
!*********************************************************************
! ** DRAG ACCEL AND SOLRAD CARDS                           * DRAG   **
!*********************************************************************
 2000 CONTINUE
      LPUN = .true.
!
!      ldebug = .true.
      ldebug = .false.
!
      if(ldebug)write(6,*) 'crdpun: below 2000 '
      if(ldebug)write(6,*) 'crdpun: ic1112 = ', ic1112
      if(ldebug)write(6,*) 'crdpun: ic7 = ', ic7
      if(ldebug)write(6,*) 'crdpun: field(2) ', field(2)
      if(ldebug)write(6,*) 'crdpun: field(3) ', field(3)
!
!
!     ....is parameter adjusted? --> 2050 and process
      IF(FIELD(3).NE.C0D0) GO TO 2050
!
!     ....no times on card? --> 50000 and write as is
      IF(FIELD(2).EQ.C0D0) GO TO 50000
!
!
! SPECIAL FORMAT NEEDED FOR CARDS WITH TIMES ON THEM
! OR FOR 9-PARAMETER GENERAL ACCELERATION TERMS
!
      IF(LPUN) THEN
         IF(IC1112.NE.99) THEN
!     ....old format ACCEL with 9 in col.7
            if(ldebug)write(6,*) 'crdpun: ic7 = ', ic7
!     if(CDNAME.NE.'ACCEL9')  WRITE(IOUT7,80421) CDNAME,IC78,IC910,
      if(CDNAME.NE.OPTION(35))  WRITE(IOUT7,80421) CDNAME,IC78,IC910,   &
     &        IC1824,FIELD(1),FIELD(2), FIELD(3)
!
            if(ldebug)write(6,80421) CDNAME,IC78,IC910,IC1824,          &
     &        FIELD(1), FIELD(2),FIELD(3)
!           READ(IUNT14,81200,END=60000) CDNAME,IFLD15,IA3,IFLD78,FIELD
         ENDIF
      ENDIF
      GO TO 600
!
! ON THE FIRST ADJUSTED DRAG,SOLRAD OR ACCEL CARD OUTPUT ALL ADJ. CARDS
!
 2050 continue
      if(ldebug)write(6,*) 'crdpun: below 2050 '
      if(ldebug)write(6,*) 'crdpun: LDSG ', LDSG
      IF(LDSG) GO TO 600
! SET POINTER TO START OF ADJUSTED DRAG,SOLRAD OR ACCEL INFORMATION
      IPTSTR=IPVAL0(IXDRAG )-1
      LDSG =.TRUE.
      if(ldebug)write(6,*) 'crdpun: LDSG ', LDSG
      if(ldebug)write(6,fmt='(1x,2a)') 'crdpun: below 2050 cdname = ',  &
     &   cdname
      NSATNO=0
      JCNT  =0
!
! OUTPUT ADJUSTED (DRAG,SOLRAD,ACCEL) INFORMATION FOR ALL SETS OF SATS
!
      if(ldebug)write(6,*) 'crdpun: before 2300 loop '
!
      isave = 1
      kskip = 0
      if(ldebug)write(6,*) 'crdpun: before 2300 loop ksave ', ksave
!
      DO 2300 ISET  =1,NSETA
      NS    =NSATST(ISET)
! LOOP ON ALL SATELLITES IN A SET
      DO 2260 ISATNO=1,NS
      NSATNO=NSATNO+1
! LOOP ON DRAG FOLLOWED BY SOLAR RADIATION FOLLOWED BY GENERAL ACC.
      if(ldebug)write(6,*) 'crdpun: ISET, ISATNO, NSATNO ',             &
     &                              ISET, ISATNO, NSATNO
      DO 2240 IDSG  =1,3
      if(ldebug)write(6,*) 'crdpun: 2240 IDSG ', IDSG
!
      ioffst = 0
      if ( IDSG .eq. 3 ) then
          if( lacc ) then
           ioffst = 1
          else if( lacct ) then
             ioffst = 2
          endif
      endif
! CALCULATE THE UCL FLAGS FOR SOLRAD CARD
      IF(IDSG.EQ.2) THEN
          IF(LJBUS(NUCL)) THEN
              IUCLFG=100
      write(6,*)'NUCL = ',NUCL
      write(6,*)'LJBUS(i) = ',LJBUS(NUCL)
          ELSEIF(LUGFO) THEN
              IUCLFG=200
          ELSEIF(LENVI) THEN
              IUCLFG=300
          ELSE
              IUCLFG=0
          ENDIF
          IF(LJPAN(NUCL)) IUCLFG=IUCLFG+10
          IF(LJTRR(NUCL)) IUCLFG=IUCLFG+1
      if(LJPAN(NUCL)) write(6,*)'LJPAN(i) = ',LJPAN(NUCL)
      if(LJTRR(NUCL)) write(6,*)'LJTRR(i) = ',LJTRR(NUCL)
          NUCL=NUCL+1
      ENDIF
!
!----------------------------------------------------------------------
!
      if(ldebug)write(6,*) 'crdpun: IDSG, kskip, isave, ksave(isave) ', &
     &                    IDSG, kskip, isave, ksave(isave)
      if(ldebug)write(6,*) 'crdpun: isave ', isave
      if(ldebug)write(6,*) 'crdpun: IDSG, lacc99 ', IDSG, lacc99
!
      if( kskip .eq. ksave(isave) ) then
         if( IDSG .eq. 3 .and. (.not.lacc99) ) then
             if( ASAVE(isave)(1:1) .ne. ' ') then
      if(ASAVE(isave)(1:6).NE.'ACCEL9') write(IOUT7,fmt='(A80)')        &
     &   ASAVE(isave)
                kskip = 0
             endif
!
             if(ldebug)write(6,*) 'crdpun: IDSG, lacc99 ', IDSG, lacc99
             if(ldebug)write(6,*) 'crdpun:aft isave ', isave
             if(ldebug)write(6,fmt='(1x,I5,1x,A,A80)')                  &
     &                 isave,'asave =', ASAVE(isave)
!
             isave = isave + 1
         endif
      endif
!
      if( idsg .eq. 3 ) kskip = kskip + 1
!
      if(ldebug)write(6,*) 'crdpun: aft IDSG, kskip, isave,',           &
     &   ' ksave(isave) ', IDSG, kskip, isave, ksave(isave)
      if(ldebug)write(6,*) 'crdpun:aft IDSG, lacc99 ', IDSG, lacc99
!----------------------------------------------------------------------
!
! LOOP ON ORDER (ITYP=4 REFERS TO TIME PERIODS AND NOT ORDER)
      DO 2200 ITYP=1,4
      TIME  =C0D0
      IOR=ITYP-1
!
      if(ldebug)write(6,*) 'crdpun: ityp = ', ityp
      if(ldebug)write(6,*) 'crdpun: LAFR = ', LAFRC(ITYP,IDSG,ISET)
!
      IF(LAFRC(ITYP,IDSG,ISET)) GO TO (2120,2140,2160,2180),ITYP
      GO TO 2200
! GENERATE (DRAG,SOLRAD OR ACCEL) CARDS
 2120 continue
!
      if(ldebug)write(6,*) 'crdpun: below 2120 '
!
      JCNT  =JCNT  +1
      VALUE =PARMVC(IPTSTR+JCNT)
      SIGMA =PARMSG(IPTSTR+JCNT)
!
      if(ldebug)write(6,*) 'crdpun: JCNT, IPTSTR ', JCNT, IPTSTR
      if(ldebug)write(6,*) 'crdpun: below 2120 VALUE = ', VALUE
      if(ldebug)write(6,*) 'crdpun: below 2120 SIGMA = ', SIGMA
!
      IF (IDSG .EQ. 3) THEN
!
        if(ldebug)write(6,*) 'crdpun: IDSG = ', IDSG
!
        if(CDSG(ioffst+IDSG).NE.OPTION(35)) WRITE(IOUT7,80401)          &
     &          CDSG(ioffst+IDSG),ITACC(NSATNO),IOR,                    &
     &          ISATID(NSATNO), VALUE,SIGMA
!
        if(ldebug)write(6,80401) CDSG(ioffst+IDSG) ,                    &
     &            ITACC(NSATNO),IOR, ISATID(NSATNO), VALUE,SIGMA
!
      ELSE
!
        if(ldebug)write(6,*) 'crdpun: IDSG = ', IDSG
!
        XTNAME=CDSG(IOFFST+IDSG)
        LNACC=XXNAME(1:4).NE.'ACCE'
!       IF(LNACC) WRITE(IOUT7,80400) CDSG(ioffst+IDSG),IOR,             &
!    &            ISATID(NSATNO),VALUE,SIGMA
        IF(LNACC) THEN
            IF(IDSG.NE.2) THEN
                WRITE(IOUT7,80400) CDSG(ioffst+IDSG),IOR,               &
     &            ISATID(NSATNO),VALUE,SIGMA
            ELSE
! FOR SOLRAD CRAD AND UCL MODEL
                WRITE(IOUT7,80405) CDSG(IOFFST+IDSG),IOR,IUCLFG,        &
     &            ISATID(NSATNO),VALUE,SIGMA
            ENDIF
        ENDIF
!
        if(ldebug)write(6,80400) CDSG(ioffst+IDSG)  ,IOR,               &
     &             ISATID(NSATNO), VALUE,SIGMA
!
      ENDIF
      GO TO 2200
! GENERATE (DRAG,SOLRAD OR ACCEL) DOT CARDS
 2140 continue
      if(ldebug)write(6,*) 'crdpun: below 2140 '
      JCNT  =JCNT  +1
      VALUE =PARMVC(IPTSTR+JCNT)
      SIGMA =PARMSG(IPTSTR +JCNT)
      IF (IDSG .EQ. 3) THEN
        if(ldebug)write(6,*) 'crdpun: IDSG = ', IDSG
        if(CDSG(ioffst+IDSG).NE.OPTION(35)) WRITE(IOUT7,80401)          &
     &     CDSG(ioffst+IDSG),ITACC(NSATNO),IOR,                         &
     &     ISATID(NSATNO),VALUE,SIGMA
!
        if(ldebug)write(6,80401) CDSG(ioffst+IDSG),ITACC(NSATNO),       &
     &            IOR,ISATID(NSATNO),VALUE,SIGMA
      ELSE
        if(ldebug)write(6,*) 'crdpun: IDSG = ', IDSG
        XTNAME=CDSG(IOFFST+IDSG)
        LNACC=XXNAME(1:4).NE.'ACCE'
!       IF(LNACC) WRITE(IOUT7,80400) CDSG(ioffst+IDSG),IOR,             &
!    &            ISATID(NSATNO),VALUE,SIGMA
        IF(LNACC) THEN
            IF(IDSG.NE.2) THEN
                WRITE(IOUT7,80400) CDSG(ioffst+IDSG),IOR,               &
     &            ISATID(NSATNO),VALUE,SIGMA
            ELSE
! FOR SOLRAD CRAD AND UCL MODEL
                WRITE(IOUT7,80405) CDSG(IOFFST+IDSG),IOR,IUCLFG,        &
     &            ISATID(NSATNO),VALUE,SIGMA
            ENDIF
        ENDIF
!
        if(ldebug)write(6,80400) CDSG(ioffst+IDSG)  ,IOR,               &
     &            ISATID(NSATNO), VALUE,SIGMA
      ENDIF
!
      GO TO 2200
! GENERATE (DRAG,SOLRAD OR ACCEL) DOUBLE DOT CARDS
 2160 continue
       if(ldebug)write(6,*) 'crdpun: below 2160 '
      JCNT  =JCNT+1
      VALUE =PARMVC(IPTSTR+JCNT)
      SIGMA =PARMSG(IPTSTR+JCNT)
      IF (IDSG .EQ. 3) THEN
        if(CDSG(ioffst+IDSG).NE.OPTION(35)) WRITE(IOUT7,80401)          &
     &         CDSG(ioffst+IDSG),ITACC(NSATNO),                         &
     &         IOR,ISATID(NSATNO), VALUE,SIGMA
!
        if(ldebug)write(6,80401) CDSG(ioffst+IDSG),ITACC(NSATNO),       &
     &         IOR, ISATID(NSATNO), VALUE,SIGMA
      ELSE
        XTNAME=CDSG(IOFFST+IDSG)
        LNACC=XXNAME(1:4).NE.'ACCE'
!       IF(LNACC) WRITE(IOUT7,80400) CDSG(ioffst+IDSG),IOR,             &
!    &            ISATID(NSATNO),VALUE,SIGMA
        IF(LNACC) THEN
            IF(IDSG.NE.2) THEN
                WRITE(IOUT7,80400) CDSG(ioffst+IDSG),IOR,               &
     &            ISATID(NSATNO),VALUE,SIGMA
            ELSE
! FOR SOLRAD CRAD AND UCL MODEL
                WRITE(IOUT7,80405) CDSG(IOFFST+IDSG),IOR,IUCLFG,        &
     &            ISATID(NSATNO),VALUE,SIGMA
            ENDIF
        ENDIF
!
        if(ldebug)write(6,80400) CDSG(ioffst+IDSG)  ,IOR,               &
     &            ISATID(NSATNO), VALUE,SIGMA
      ENDIF
      GO TO 2200
! GENERATE (DRAG,SOLRAD OR ACCEL) CARDS WITH TIMES
 2180 continue
!     write(6,*) 'crdpun: below 2180 '
      NPDS  =IPDFRC(IDSG,NSATNO)
      IC78  =IORFRC(IDSG,NSATNO)-1
!     NPDS  =IPDFRC(IDSG,ISET)
!     IC78  =IORFRC(IDSG,ISET)-1
!     WRITE(6,*)'**CRDPUN** IDSG,NPDS*',IDSG,NPDS,NSATNO,ISET
!     write(6,*) 'crdpun: ic78 ', ic78
!CCC      IF(IDSG.EQ.3)   IC78  =ITACC(NSATNO)*10+IC78
      IF( IDSG.EQ.3 .and. (lacc .or. lacct) ) IC78=ITACC(NSATNO)*10+IC78
      if(ldebug)write(6,*) 'crdpun: ic78 ', ic78
!
      INPDS0=0
      IF(NSATNO.GE.2)THEN
      ISETM1=NSATNO-1
      INPDS0=IPDFRC(IDSG,ISETM1)
      ENDIF
      DO 2190 IPDS  =1,NPDS
! CHECK FOR DRAG
      IF(IDSG.NE.1) GOTO 2182
!     TTIME=STDRG(IPDS)
      IT=INPDS0+IPDS
      TTIME=STDRG(IT)
      IF(.NOT.LL(KLAJDP+IPDS-1)) GOTO 2190
      GOTO 2186
! CHECK FOR SOLAR RADIATION
 2182 IF(IDSG.NE.2) GOTO 2184
!CC   TTIME=STSRD(IPDS)
      IT=INPDS0+IPDS
      TTIME=STSRD(IT)
      IF(.NOT.LL(KLAJSP+IPDS-1)) GOTO 2190
      GOTO 2186
! CHECK FOR GENERAL ACCELERATION
 2184 CONTINUE
!2184 TTIME=STACC(IPDS)
      IT=INPDS0+IPDS
      TTIME=STACC(IT)
      IF(.NOT.LL(KLAJGP+IPDS-1)) GOTO 2190
 2186 CONTINUE
!
      ITIME = TTIME
      FSECOT(1)= TTIME-ITIME
      JCNT  =JCNT+1
      VALUE =PARMVC(IPTSTR+JCNT)
      SIGMA =PARMSG(IPTSTR+JCNT)
! CONVERT TIME FROM MJDS IN UTC TO YYMMDD IN UTC.
      CALL UTCET(.FALSE.,1,ITIME,FSECOT,FSEC1,ETUTC)
      CALL YMDHMS(ITIME,FSEC1,JYMD,JHM,FSEC,1)
! ROUNDING IS BEING USED HERE FOR PURPOSE OF PRINTOUT
      FSEC(1) = FSEC(1)+C1DM5
!
!     ....CORRECT TIMES IF SECONDS >= 60
!
!     WRITE(6,*) 'CRDPUN:B JYMD, JHM, FSEC ', JYMD(1), JHM(1), FSEC(1)
!
      IF( FSEC(1) .GE. C6D1 ) THEN
         FSEC(1) = FSEC(1) - C6D1
         JHM(1) = JHM(1) + 1
      ENDIF
      IF( MOD( JHM(1), 100 ) .GE. 60 ) THEN
         JHM(1) = JHM(1) - 60
         JHM(1) = JHM(1) + 100
      ENDIF
      IF( INT( JHM(1) / 100 ) .GE. 24 ) THEN
         JHM(1) = JHM(1) - 2400
         CALL ADDYMD( JYMD(1), 1 )
      ENDIF
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
!
!     WRITE(6,*) 'CRDPUN:A JYMD, JHM, FSEC ', JYMD(1), JHM(1), FSEC(1)
!<<<<<
      XTIME=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)
!
! SPECIAL FORMAT NEEDED FOR ADJUSTED
! 9-PARAMETER GENERAL ACCELERATION TERMS
!
      if(ldebug)write(6,*) 'crdpun: ic78 = ', ic78
!
      IVAL=ITACCX(IPDS+(ISATNO-1)*9)
      IF(IVAL.GE.100) IVAL=IVAL-100
!
      if(ldebug)write(6,*) 'crdpun: ipds, isatno, ival ', ipds,         &
     &      isatno, ival
!
      if( IDSG .eq. 3 .and. ioffst .eq. 0 ) XTIME = C0D0
      if( IDSG .ne. 3 ) IVAL = 0
!
!        if(CDSG(ioffst+IDSG).NE.OPTION(35)) WRITE(IOUT7,80421)         &
!    &                   CDSG(ioffst+IDSG),IC78,IVAL,ISATID(NSATNO),    &
!    &                     VALUE,XTIME,SIGMA
      IF(IDSG.NE.2) THEN
         if(CDSG(ioffst+IDSG).NE.OPTION(35)) WRITE(IOUT7,80421)         &
     &                   CDSG(ioffst+IDSG),IC78,IVAL,ISATID(NSATNO),    &
     &                     VALUE,XTIME,SIGMA
      ELSE
! UCL MODEL FOR SOLRAD CARD
         if(CDSG(ioffst+IDSG).NE.OPTION(35)) WRITE(IOUT7,80425)         &
     &                   CDSG(ioffst+IDSG),IC78,IUCLFG,ISATID(NSATNO),  &
     &                     VALUE,XTIME,SIGMA
      ENDIF
!
      if(ldebug)write(6,80421) CDSG(ioffst+IDSG)  ,IC78,IVAL,           &
     &     ISATID(NSATNO), VALUE,XTIME,SIGMA
!
!
!----------------------------------------------------------------------
      if(ldebug)write(6,*) 'crdpun: 2186 IDSG, kskip, isave,',          &
     &  ' ksave(isave) ', IDSG, kskip, isave, ksave(isave)
      if(ldebug)write(6,*) 'crdpun: IDSG, lacc99 ', IDSG, lacc99
!
      if( kskip .eq. ksave(isave).and. ipds.ne.npds ) then
         if( IDSG .eq. 3 .and. (.not.lacc99) ) then
             if( ASAVE(isave)(1:1) .ne. ' ') then
       if(ASAVE(isave)(1:6).NE.'ACCEL9') write(IOUT7,fmt='(A80)')       &
     &              ASAVE(isave)
                kskip = 0
             endif
!
             if(ldebug)write(6,*) 'crdpun: IDSG, lacc99 ', IDSG, lacc99
             if(ldebug)write(6,fmt='(1x,I5,1x,A,A80)')                  &
     &                isave,'asave =', ASAVE(isave)
!
             isave = isave + 1
         endif
      endif
!
      if( idsg .eq. 3 .and. ipds.ne.npds) kskip = kskip + 1
!
      if(ldebug)write(6,*) 'crdpun: aft 2186 IDSG, kskip, isave,',      &
     &  ' ksave(isave) ', IDSG, kskip, isave, ksave(isave)
      if(ldebug)write(6,*) 'crdpun:aft IDSG, lacc99 ', IDSG, lacc99
!
!----------------------------------------------------------------------
!
 2190 END DO
       if(ldebug)write(6,*) 'crdpun: below 2190 '
 2200 END DO
 2240 END DO
 2260 END DO
 2300 END DO
      GOTO 600
!*********************************************************************
! ** ATITUD  **                                           * ATITUD  **
!*********************************************************************
 3000 CONTINUE
      IF(IC1011.EQ.99) THEN
      KATUD=0
      KATBL=KATBL+1
      IKAD=9*KATBL
      ENDIF
      IANG=IC7*10+IC8
      IF(IANG.EQ.0) GOTO 50000
      IF(FIELD(3).EQ.C0D0) GO TO 50000
      KATID=IC1214
      IF(KATUD.NE.KATID) THEN
      KATAD=KATAD+9
      KATUD=KATID
      ENDIF
      ISUB=IC7-2
      IORD=IC7+IC8+ISUB
      IATT  =IPVAL(IXATUD)
      IADJ  =IPTRUA(IATT  )
      FIELD(1)=PARMVC(IADJ+IKAD+IORD+KATAD)
      GOTO 50000
!*********************************************************************
! ** EARTH  **                                             * EARTH  **
!*********************************************************************
 3500 CONTINUE
      IF(IC9.GT.0) GO TO 50000
      IF(NPVAL0(IXGM  ).GT.0 .AND. FIELD(1).GT.C0D0)                    &
     &       FIELD(1)=PARMVC(IPVAL0(IXGM))
      IF(NPVAL0(IXSMA ).GT.0 .AND. FIELD(2).GT.C0D0)                    &
     &       FIELD(2)=PARMVC(IPVAL0(IXSMA))
      IF(NPVAL0(IXFLTP).GT.0 .AND. FIELD(3).GT.C0D0)                    &
     &       FIELD(3)=PARMVC(IPVAL0(IXFLTP))
      IF(NPVAL0(IXFLTE).GT.0 .AND. FIELD(4).GT.C0D0)                    &
     &       FIELD(4)=PARMVC(IPVAL0(IXFLTE))
      LPUN=LPNALL .OR. (LPNADJ .AND. NPVAL0(IXGM  )+NPVAL0(IXSMA )+     &
     &                 NPVAL0(IXFLTP)+NPVAL0(IXFLTE).GT.0 )
      GO TO 50000
!*********************************************************************
! ** EDIT   **                                             * EDIT   **
!*********************************************************************
 3700 CONTINUE
      LPUN=.TRUE.
! RESET EDIT RMS
      FIELD(2)=EDTRMS
      GO TO 50000
!*********************************************************************
! ** ENDARC **                                             * ENDARC **
!*********************************************************************
 4000 CONTINUE
!     IF(LPNALL) WRITE(IOUT7,81320) CDNAME,IFIELD(1),IFIELD(2)
      REWIND IUNT14
      RETURN
!*********************************************************************
!** ENDGLB **                                              * ENDGLB **
!*********************************************************************
 5000 CONTINUE
      IF(LPNALL) WRITE(IOUT7,81320) CDNAME,IFIELD(1),IFIELD(2)
      REWIND IUNT14
      RETURN
!*********************************************************************
! ** ETIDES ** ETIDEN **                            ETIDEN * ETIDES **
!*********************************************************************
 5200 CONTINUE
      IF(FIELD(3).EQ.C0D0 .AND. FIELD(4).EQ.C0D0) GO TO 50000
      IF(LADJET) GO TO 600
      LADJET=.TRUE.
! OUTPUT ALL ADJUSTED ETIDES AT ONE TIME
      IPTID =IPVAL0(IXETDE)
      IM    =-1
      DO 5240 I=1,NETADJ
      IM    =IM    +1
      ITD   =IPTID +IM
      MKH   =II(KMM  +IM)*100 + II(KKK  +IM)*10 + II(KHH  +IM)
      WRITE(IOUT7,81620) II(KSIGN1 +IM),MKH,II(KJJ  +IM),II(KIBDY+IM),  &
     & PARMVC(ITD   ),PARMVC(ITD+NETADJ),PARMSG(ITD),PARMSG(ITD+NETADJ)
 5240 END DO
      GO TO 600
!*********************************************************************
! ** GCOEF  **                                             * GCOEF  **
!*********************************************************************
 5500 CONTINUE
      IF(FIELD(3).EQ.C0D0 .AND. FIELD(4).EQ.C0D0) GO TO 50000
      LPUN=LPNADJ
      IFIELD(1)=10
! LOCATE POINTER FOR C OR S  COEFFICIENT
      NOFF =(IC1517-1)*IC1517/2 + 3*(IC1517-1) + IC1820
! C COEFFICIENTS
      IF(FIELD(3).LE.C0D0) GO TO 5600
      IGRV  =IPVAL(IXGPC )+NOFF
      IADJ  =IPTRUA(IGRV  )
      FIELD(1)=PARMVC(IADJ)
! S COEFFICIENTS
 5600 IF(FIELD(4).LE.C0D0) GO TO 50000
      IGRV  =IPVAL(IXGPS )+NOFF
      IADJ  =IPTRUA(IGRV  )
      FIELD(2)=PARMVC(IADJ)
      GO TO 50000
!*********************************************************************
! ** GCOFCT **                                             * GCOFCT **
!*********************************************************************
 8400 CONTINUE
      LPUN=.TRUE.
      LTPG=.TRUE.
      GO TO 50000
!*********************************************************************
! ** GCOFST **                                             * GCOFST **
!*********************************************************************
 8410 CONTINUE
      LPUN=.TRUE.
      LTPG=.TRUE.
      GO TO 50000
!*********************************************************************
! ** GTPEND **                                             * GTPEND **
!*********************************************************************
 8420 CONTINUE
      LPUN=.TRUE.
      LTPG=.FALSE.
      GO TO 50000
!*********************************************************************
! ** GCOEFC **                                             * GCOEFC **
!*********************************************************************
 5700 CONTINUE
      IF(FIELD(3).EQ.C0D0)  GO TO 50000
      IF(LTPG) GOTO 5710
      LPUN=LPNADJ
      IFIELD(1)=10
! LOCATE POINTER FOR C
      NOFF =(IC1517-1)*IC1517/2 + 3*(IC1517-1) + IC1820
! C COEFFICIENTS
      IGRV  =IPVAL(IXGPC )+NOFF
      IADJ  =IPTRUA(IGRV  )
      FIELD(1)=PARMVC(IADJ)
      GO TO 50000
 5710 CONTINUE
      LPUN=LPNADJ
      IADJ=IPTRUA(IPVAL(IXTGPC)+NTPGC)
      FIELD(1)=PARMVC(IADJ)
      NTPGC=NTPGC+1
      GO TO 50000
!*********************************************************************
! ** GCOEFS **                                             * GCOEFS **
!*********************************************************************
 5800 CONTINUE
      IF(FIELD(3).EQ.C0D0)  GO TO 50000
      IF(LTPG) GOTO 5810
      LPUN=LPNADJ
      IFIELD(1)=10
! LOCATE POINTER FOR S
      NOFF =(IC1517-1)*IC1517/2 + 3*(IC1517-1) + IC1820
! S COEFFICIENTS
      IGRV  =IPVAL(IXGPS )+NOFF
      IADJ  =IPTRUA(IGRV  )
      FIELD(1)=PARMVC(IADJ)
      GO TO 50000
 5810 CONTINUE
      LPUN=LPNADJ
      IADJ=IPTRUA(IPVAL(IXTGPS)+NTPGS)
      FIELD(1)=PARMVC(IADJ)
      NTPGS=NTPGS+1
      GO TO 50000
!*********************************************************************
! ** H2LOVE **                                             * H2LOVE **
!*********************************************************************
 6600 CONTINUE
      IF(FIELD(2).EQ.C0D0) GOTO 50000
      LPUN=LPNADJ
! REPLACE VALUE WITH NEW ADJUSTED VALUE
      FIELD(1)=PARMVC(IPVAL0(IXH2LV))
      GO TO 50000
!*********************************************************************
! ** L2LOVE **                                             * L2LOVE **
!*********************************************************************
 6800 CONTINUE
      IF(FIELD(2).EQ.C0D0) GOTO 50000
      LPUN=LPNADJ
! REPLACE VALUE WITH NEW ADJUSTED VALUE
      FIELD(1)=PARMVC(IPVAL0(IXL2LV))
      GOTO 50000
!**********************************************************************
!* ** LOCALG **                                             * LOCALG **
!**********************************************************************
 6900 CONTINUE
      READ(14,82400)
      GOTO 50000
!*********************************************************************
!** MBIAS & PBIAS **                               * PBIAS & MBIAS ***
!*********************************************************************
 7000 CONTINUE
!     IF(FIELD(4).EQ.C0D0) GO TO 50000
! ONLY GOTO 50000 WHEN CARD IS MBIAS OR MBIAS1. MBIAS2, MBIAS3, PBIAS
! PBIAS1, PBIAS2, AND PBIAS3 CARDS ARE PROCESSED IN THE FOLLOWING CODES
      IF(FIELD(2).EQ.C0D0.AND.(CDNAME.EQ.'MBIAS'.OR.CDNAME.EQ.'MBIAS1')&
     &      ) GO TO 50000
! ON FIRST PBIAS OR MBIAS CARD WITH ADJUSTMENT OUTPUT ALL ADJ BIASES
      IF(LBIAS) GO TO 600
      LBIAS=.TRUE.
! GET BIAS TYPE,STATION, AND TIME INFO FROM EMAT LABEL
      IPTR0 =IPVAL0(IXBISA)
      IPTR  =IPTR0
      NBIASA=NPVAL0(IXBISA)
      DO 7050 I=1,NBIASA
      IBTYPE= (MOD(PRMLBL(1,IPTR),C1D13))/C1D10+0.00005
      IBSTA = (MOD(PRMLBL(1,IPTR),C1D10))/C1D2+0.00005
      ITIME1=PRMLBL(2,IPTR) - INT(REPDIF)
      FSECOT(1)=C0D0
! CONVERT TIME FROM MJDS IN UTC TO YYMMDD IN UTC.
      CALL YMDHMS(ITIME1,FSECOT,JYMD,JHM,FSEC,1)
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
      STRTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)
      ITIME2=PRMLBL(3,IPTR)  - INT(REPDIF)
!***********************************************************************
!********* TEMPORARY PATCH *** 8606.0 ** TEMPORARY PATCH ***************
!***********************************************************************
!
! FORCE BIAS STOP TIME TO BE GREATER THAN OR EQUAL TO START TIME PLUS
!       FIFTY MINUTES. THIS IS ONLY AN APPROXIMATION AND MAY RESULT IN
!       ERRORS IF BIAS HAS DELIBERATE STEP CHANGE WITHIN A PASS OR IF
!       PASS LENGTH EXCEEDS 50 MINUTES. THIS IS DONE FOR ALL BIASES,
!       EVEN THOUGH PROBLEM BEING CIRCUMVENTED APPLIES ONLY TO PBIASES.
!***********************************************************************
!****THIS PATCH REMOVED IN 9106 BY JAM*******************************
!***********************************************************************
!
!     ITIME2=MAX0(ITIME2,ITIME1+3000)
!***********************************************************************
!************ END PATCH ****** 8606.0 ***** END PATCH ******************
!***********************************************************************
      CALL YMDHMS(ITIME2,FSECOT,JYMD,JHM,FSEC,1)
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
      STPTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)
      WRITE(IOUT7,81680) IBSTA,IBTYPE,IMBSAT(1,I),PARMVC(IPTR),         &
     &                   STRTIM,STPTIM,PARMSG(IPTR)
      IF(IBSTA.NE.IMBSTA(1,I)) then
         WRITE(6,*) ' ANDY CRDPUN STAT .NE.'
         write(6,'(A,3(1x,I10))')'crdpun: I, IBSTA, IMBSTA(1,I) ', &
                                          I, IBSTA, IMBSTA(1,I)
      ENDIF ! IBSTA.NE.IMBSTA(1,I)
      DO 7045 IJ=2,3
      IF(IMBSAT(IJ,I).EQ.0 .AND. IMBSTA(IJ,I).EQ.0)  GOTO 7045
!     WRITE(IOUT7,81685) IJ,IMBSTA(IJ,I),IBTYPE,IMBSAT(IJ,I)
      WRITE(IOUT7,81688) IJ,IMBSTA(IJ,I),IMBSAT(IJ,I)
 7045 END DO
      IPTR=IPTR+1
 7050 END DO
      GOTO 600
!*********************************************************************
!** OTIDES ** OTIDEN                            OTIDEN    * OTIDES ***
!*********************************************************************
 7500 CONTINUE
      IF(FIELD(3).EQ.C0D0 .AND. FIELD(4).EQ.C0D0) GO TO 50000
      IF(LADJOT) GO TO 600
      LADJOT=.TRUE.
! OUTPUT ALL ADJUSTED OTIDES AT ONE TIME
      IPTID =IPVAL0(IXETDE)+NETADJ*2-1
      IM    =NETADJ-1
      DO 7540 I=1,NOTADJ
      IM    =IM +1
      ITD   =IPTID +I
      MKH   =II(KMM  +IM)*100 + II(KKK  +IM)*10 +  II(KHH  +IM)
      WRITE(IOUT7,81600) II(KSIGN2 +IM),II(KLL  +IM),II(KQQ  +IM),      &
     &     II(KSIGN1 +IM),MKH,II(KJJ  +IM),II(KIBDY+IM),PARMVC(ITD   ), &
     &     PARMVC(ITD+NOTADJ),PARMSG(ITD),PARMSG(ITD+NOTADJ)
 7540 END DO
      GOTO 600
!*********************************************************************
! EBIAS  **                                                * EBIAS  **
!*********************************************************************
 8000 CONTINUE
      IF(LEBIAS) GO TO 50000
      LEBIAS=.TRUE.
! ON FIRST EBIAS CARD OUTPUT ALL ADJ EBIASES AS UNADJUSTED MBIASES
      DO 8100 I=1,NBIASE
      I1=I-1
!...INDEX OF EBIAS TYPE AND STATION NUMBER
      K0=KSTAMT+I1
!...EBIAS TYPE
      MTYPEB=INT(AA(K0)/1D8+HALF)
!...EBIAS STATION NUMBER
      ISTAEB=INT(AA(K0)-MTYPEB*1D8+HALF)
!...INDEX OF EBIAS VALUE
      K1=KEBVAL+I1
!...INDEX OF EBIAS MJDSEC
      K2=KMJDEB+I1
!...INDEX OF EBIAS START FSEC
      K3=KFSEB1+I1
!.....CONVERT TO CALENDAR FORM
      CALL YMDHMS(II(K2),AA(K3),JYMD,JHM,FSEC,1)
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
      STRTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)
!...INDEX OF EBIAS STOP FSEC
      K4=KFSEB2+I1
!.....CONVERT TO CALENDAR FORM
      CALL YMDHMS(II(K2),AA(K4),JYMD,JHM,FSEC,1)
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
      STPTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)
!...PUNCH ADJUSTED EBIAS VALUE AS UNADJUSTED MBIAS
      WRITE(IOUT7,81680) ISTAEB,MTYPEB,ISATID(NSATNO),AA(K1),           &
     &   STRTIM,STPTIM
 8100 END DO
      GOTO 50000
!*********************************************************************
! PANEL  **                                                * PANEL  **
!*********************************************************************
 8200 CONTINUE
      IPANEL=IC1112
      IPARM =IC1314
!CC      IF((IPARM.NE.1).OR.(FIELD(2).NE.C0D0)) THEN
      IF( (IPARM.NE.1) .AND. (FIELD(2).NE.C0D0) ) THEN
!      ....for adjusted parameter cards (not normal vector cards)
       ISATNM=IC1824
       ISETPN=0
! FIND WHAT SET THIS SAT. IS IN ** THIS CODE ASSUMES ONE SAT. PER SET
       DO 8202 ISBL=1,NSETA
       IF(ISATID(ISBL).EQ.ISATNM) ISETPN=ISBL
 8202  CONTINUE
       IF(ISETPN.EQ.0) GOTO 60350
! COMPUTE SET OFFSET
       ISTOFF=II(KPLPTR+ISETPN-1)
! COMPUTE ADJUSTED PARAMETER POINTER FROM UNADJUSTED PARAMETER POINTER
       GOTO (8210,8220,8230,8240,8250,8260,8270,8280,8290,8300),IPARM
 8210  GOTO 60300
 8220  IPNTU=IPVAL(IXAREA)+ISTOFF+IPANEL-2
       GOTO 8350
 8230  IPNTU=IPVAL(IXSPRF)+ISTOFF+IPANEL-2
       GOTO 8350
 8240  IPNTU=IPVAL(IXDFRF)+ISTOFF+IPANEL-2
       GOTO 8350
 8250  IPNTU=IPVAL(IXEMIS)+ISTOFF+IPANEL-2
       GOTO 8350
 8260  IPNTU=IPVAL(IXTMPA)+ISTOFF+IPANEL-2
       GOTO 8350
 8270  IPNTU=IPVAL(IXTMPC)+ISTOFF+IPANEL-2
       GOTO 8350
 8280  IPNTU=IPVAL(IXTIMD)+ISTOFF+IPANEL-2
       GOTO 8350
 8290  IPNTU=IPVAL(IXTIMF)+ISTOFF+IPANEL-2
       GOTO 8350
 8300  IPNTU=IPVAL(IXTHTX)+ISTOFF+IPANEL-2
 8350 CONTINUE
       IPNTA=IPTRUA(IPNTU)
       FIELD(1)=PARMVC(IPNTA)
       LPUN=LPNALL.OR.LPNADJ
       GOTO 50000
      ELSE
!      ....for normal vector cards and unadjusted parameters
       LPUN=LPNALL
       GOTO 50000
      ENDIF
!*********************************************************************
! PLANET **                                                * PLANET **
!*********************************************************************
 8500 CONTINUE
      IF(IC9.GT.0) GO TO 50000
      IF(NPVAL0(IXGM  ).GT.0 .AND. FIELD(1).GT.C0D0)                    &
     &       FIELD(1)=PARMVC(IPVAL0(IXGM))
      IF(NPVAL0(IXSMA ).GT.0 .AND. FIELD(2).GT.C0D0)                    &
     &       FIELD(2)=PARMVC(IPVAL0(IXSMA))
      IF(NPVAL0(IXFLTP).GT.0 .AND. FIELD(3).GT.C0D0)                    &
     &       FIELD(3)=PARMVC(IPVAL0(IXFLTP))
      IF(NPVAL0(IXFLTE).GT.0 .AND. FIELD(4).GT.C0D0)                    &
     &       FIELD(4)=PARMVC(IPVAL0(IXFLTE))
      LPUN=LPNALL .OR. (LPNADJ .AND. NPVAL0(IXGM  )+NPVAL0(IXSMA )+     &
     &                 NPVAL0(IXFLTP)+NPVAL0(IXFLTE).GT.0 )
      GO TO 50000
!*********************************************************************
! PLMOON **                                                * PLMOON **
!*********************************************************************
 8600 CONTINUE
      LOKADJ=.FALSE.
      IF(NPVAL0(IXPMGM).GT.0 .AND. FIELD(1).GT.C0D0) THEN
! FIND THE MOON THAT THIS CARD APPLIES TO
      INDX=0
      DO 8610 I=1,NPVAL0(IXPMGM)
!     PRINT *,' CRDPUN: IC1114 IS: ',IC1114
      IF(IMNNUM(I,2).EQ.IC1114) GOTO 8620
      INDX=INDX+1
 8610 END DO
! NO MATCH FOUND FOR THIS MOON BODY NUMBER
      GOTO 60200
 8620 CONTINUE
      FIELD(1)=PARMVC(IPVAL0(IXPMGM)+INDX)
      LOKADJ=.TRUE.
      ENDIF
      LPUN=LPNALL .OR. (LPNADJ .AND. LOKADJ)
      GO TO 50000
!*********************************************************************
!** POLEUT **                                             * POLEUT ***
!*********************************************************************
 8700 CONTINUE
      NPOLCD=NPOLCD+1
      IF(IC7.EQ.0) GO TO 8710
      LPUN=LPNADJ
! LOCATE WHICH POLE INTERVAL THIS POLE CARD IS ASSOCIATED WITH
      CALL FNDNUM(NPOLCD,IPOLCD,NPOLE,INTPOL)
! DETERMINE OFFSET IN ADJUSTED ARRAYS FOR THIS INTERVAL
      IOFF=INDPI(INTPOL)
      IPOLE=IPVAL0(IXPOLX)+(IOFF-1)*3
      FIELD(3)=(PARMVC(IPOLE  ))/SECRAD
      FIELD(4)=(PARMVC(IPOLE+1))/SECRAD
      FIELD(2)= PARMVC(IPOLE+2)
 8710 IF(LPUN)   WRITE(IOUT7,81321) CDNAME,IFLD15,IA3,IFLD78,FIELD
! READ SECOND CARD
      READ(IUNT14,81420) RF(1),RF(2),RF(3),RF(4)
      IF(LPUN)   WRITE(IOUT7,81430) RF(1),RF(2),RF(3),RF(4)
      IF(IC7.LT.2) GO TO 600
! READ THIRD CARD
      READ(IUNT14,81320) CDNAME,IFLD15,IA3,IFLD78,                      &
     &                    RF(1),RF(2),RF(3),RF(4)
      CALL A3TOI6(IA3,I6,IFIELD(6))
      IF(LPUN)   WRITE(IOUT7,81430) RF(1),RF(2),RF(3),RF(4)
      GOTO 600
!*********************************************************************
! REFSYS **                                                * REFSYS **
!*********************************************************************
 9000 CONTINUE
! REFSYS
      READ(IUNT14,81900,END=60100) CDNAME,IC78,IC9,IC10,IC1112,         &
     &     IC2126,IC2730,RF1,IC41
      WRITE(IOUT7,81910) CDNAME,IC78,IC9,IC10,IC1112,                   &
     &     IC2126,IC2730,RF1,IC41
      GOTO 600
!*********************************************************************
! SATPAR **                                                * SATPAR **
!*********************************************************************
 9500 CONTINUE
      WRITE(IOUT7,81320) CDNAME,IFLD15,IA3,IFLD78,FIELD
!     FIND CURRENT SATELLITE IN SATELLITE ID ARRAY
      CALL FNDNUM(IC1824,ISATID,NSATA,ISAT)
      IPSATP=IPVAL0(IXSATP)+(ISAT-1)*6
! EPOCH OR SLAVE
      READ(IUNT14,82200) CDNAME,IYMD,IHM,RF(1),IYMD1,IHM1,RF(2),IYMD2,  &
     & IHM2,RF(3)
      IF(CDNAME.EQ.SLAVE) GOTO 9525
! ** EPOCH **
      IYMDU=IYMD
      IHMU=IHM
      RFU=RF(1)
      IF(IYMD1.LE.0) GO TO 9520
      IYMDU=IYMD1
      IHMU=IHM1
      RFU=RF(2)
 9520 CONTINUE
      WRITE(IOUT7,82250) CDNAME,IYMDU,IHMU,RFU,IYMDU,IHMU,RFU,IYMD2,    &
     & IHM2,RF(3)
      GO TO 9550
! ** SLAVE **
 9525 CONTINUE
      WRITE(IOUT7,82260) CDNAME,IC1824
      GOTO 9550
!** ELEMS1 **
 9550 CONTINUE
      READ(IUNT14,82300) CDNAME,IC7,IC8,IC1114,RF(1),RF(2),RF(3)
      IC7  =IELMTP(ISAT)
      RF(1)=PARMVC(IPSATP  )
      RF(2)=PARMVC(IPSATP+1)
      RF(3)=PARMVC(IPSATP+2)
      WRITE(IOUT7,82300) CDNAME,IC7,IC8,IC1114,RF(1),RF(2),RF(3)
!** ELEMS2 **
      READ(IUNT14,82300) CDNAME,IC7,IC8,IC1114,RF(4),RF(5),RF(6)
      RF(4)=PARMVC(IPSATP+3)
      RF(5)=PARMVC(IPSATP+4)
      RF(6)=PARMVC(IPSATP+5)
      WRITE(IOUT7,82320) CDNAME,RF(4),RF(5),RF(6)
      GO TO 600
!*********************************************************************
! ** SELECT/DELETE OPTION *                                * SELECT **
!*********************************************************************
10500 CONTINUE
      IF(LPNALL) WRITE(IOUT7,81320) CDNAME,IFIELD(1),IFIELD(2)
10600 CONTINUE
      LPUN=LPNALL
      READ(IUNT14,81050,END=60100) ECHO
      IF(LPUN)  WRITE(IOUT7,81050) ECHO
      IF(ECHO(1).EQ.ENDARC) GO TO 4000
      GOTO 10600
!*********************************************************************
! SOLRAD **  SEE DRAG CARD                                 * SOLRAD **
! ********************************************************************
10700 GO TO 2000
!*********************************************************************
!  STAPOS **                                               * STAPOS **
!*********************************************************************
12000 CONTINUE
      LSTADJ=FIELD(1)+FIELD(2)+FIELD(3) .NE. C0D0
      IF(LPNALL) WRITE(IOUT7,81320) CDNAME,IFLD15,IA3,IFLD78,           &
     &   FIELD
12050 READ(IUNT14,81500,END=60100) CDNAME,IC9,IC10,IC1112,IC1316,IC1720,&
     &   RF(1),RF(2),RF(3),IPLATE,ECHO(1),ECHO(2)
      LPUN=LPNALL
      IF(CDNAME.EQ.ENDSTA)  THEN
         WRITE(IOUT7,'(a6)') CDNAME
         GO TO 600
      ENDIF
      IF(CDNAME.EQ.ENDGLB) GO TO 5000
      ISTA  =IC1316*10000+IC1720
! TEST FOR VARIOUS STATION SUBGROUP CARDS
      DO 12070 I=1,NSTACD
      IF(CDNAME.EQ.STACDS(I)) GOTO (12065,12200,12200,12200,12065,      &
     & 12200,12200,12200,12200,12230,12240,12240 ),I
      GO TO 12070
! SET LSTADJ TRUE WHEN ADJUSTED, OR FALSE WHEN FIXED CARD ENCOUNTERED
12065 LSTADJ=I.EQ.1
      GO TO 12200
12070 END DO
! IF NO MATCH WE HAVE A STATION COORDINATE CARD - WAS IT ADJUSTED
      IF(.NOT. LSTADJ) GO TO 12200
! STATION WAS ADJUSTED - FIND WHERE ITS COORDINATES ARE LOCATED
      CALL FNDNUM(ISTA,ISTANO,NSTA,ISTOFF)
      IF(ISTOFF.EQ.0) GO TO 12200
      LPUN=.TRUE.
      IC9=ICRD(ISTOFF)/10
!     IOFF=IPVAL0(IXSTAP)+3*(ISTOFF-1)
!     RF(1)=PARMVC(IOFF   )
!     RF(2)=PARMVC(IOFF+1 )
!     RF(3)=PARMVC(IOFF+2 )
      RF(1)=SAVSTA(ISTOFF,1)
      RF(2)=SAVSTA(ISTOFF,2)
      RF(3)=SAVSTA(ISTOFF,3)
!     WRITE(6,*) ' DBG STATION COORDINATES ',RF(1),RF(2),RF(3),ISTOFF
! LOAD COODINATES IN PROPER UNITS FOR PUNCH OUTPUT (WEI)
      GO TO (12100,12150,12110,12120),IC9
12100 CALL DEGOUT(RF(1),IDD1,IMM1,SEC1,1)
      RF(1)=DBLE(IDD1*10000+IMM1*100)+SEC1
      CALL DEGOUT(RF(2),IDD2,IMM2,SEC2,1)
      RF(2)=DBLE(IDD2*10000+IMM2*100)+SEC2
      GOTO 12150
12110 CALL DEGOUT(RF(2),IDD2,IMM2,SEC2,1)
      RF(2)=DBLE(IDD2*10000+IMM2*100)+SEC2
      GOTO 12150
12120 CALL DEGOUT(RF(1),IDD1,IMM1,SEC1,1)
      RF(1)=DBLE(IDD1*10000+IMM1*100)+SEC1
      CALL DEGOUT(RF(2),IDD2,IMM2,SEC2,1)
      RF(2)=DBLE(IDD2*10000+IMM2*100)+SEC2
12150 CONTINUE
12200 IF(LPUN)  WRITE(IOUT7,81520) CDNAME,IC9,IC10,IC1112,ISTA,RF(1),   &
     &                             RF(2),RF(3),IPLATE,ECHO(1),ECHO(2)
      GO TO 12050
12230 CONTINUE
!
      LVADJ=.FALSE.
      LBACK=.FALSE.
12235 READ(IUNT14,81530) CDNAME,IC1320,RF(1),RF(2),RF(3)
!
!     WRITE(6,81530) CDNAME,IC1320,RF(1),RF(2),RF(3)
! STATION VELOCITY IS ADJUSTED - FIND THE ADJUSTED VALUE LOCATION
!     IF(LVADJ.AND.LBACK)THEN
      IF(LBACK)THEN
      IF(LVADJ)THEN
      IF(NCVEL.EQ.0)THEN
       IF(LPNADJ)THEN
      ISTA  =IC1320
      CALL FNDNUM(ISTA,ISTANO,NSTA,ISTOFF)
!     IF(ISTOFF.EQ.0)THEN
!      IF(LPNALL)THEN
!      GO TO 12236
!      ELSE
!      GO TO 12050
!      ENDIF
!      ENDIF
       ISTAV=ICNV(ISTOFF)
      IOFF=IPVAL0(IXSTAV)+3*(ISTAV-1)
      RF(1)=PARMVC(IOFF   )
      RF(2)=PARMVC(IOFF+1 )
      RF(3)=PARMVC(IOFF+2 )
       ENDIF
      ENDIF
       ENDIF
12236 IF(LVADJ)THEN
      NCVEL=NCVEL+1
      ELSE
      NCVEL=NCVEL+2
       ENDIF
       ENDIF
!
      IF(LBACK)THEN
       IF(CDNAME.EQ.STACDS(11))THEN
       WRITE(IOUT7,81530) CDNAME,IC1320,RF(1)
       ELSE
       WRITE(IOUT7,81530) CDNAME,IC1320,RF(1),RF(2),RF(3)
       ENDIF
       IF(NCVEL.LT.3)THEN
        GO TO 12235
       ELSE
        GO TO 12050
       ENDIF
       ENDIF
!
!     IF(CDNAME.EQ.'TIMVEL')GO TO 12235
      IF(CDNAME.EQ.STACDS(11))GO TO 12235
      LBACK=.TRUE.
      NCVEL=0
      BACKSPACE IUNT14
      BACKSPACE IUNT14
      BACKSPACE IUNT14
!     IF(CDNAME.NE.'SIGVEL')THEN
      IF(CDNAME.NE.STACDS(12))THEN
      LVADJ=.FALSE.
      ELSE
      LVADJ=.TRUE.
      ENDIF
      GO TO 12235
!*********************************************************************
! STATION L2 AND H2                                   STA.L2 & H2
!*********************************************************************
12240 CONTINUE
      IF(RF(2).EQ.0) GO TO 12245
      ISTA  =IC1316*10000+IC1720
      CALL FNDNUM(ISTA,ISTANO,NSTA,ISTOFF)
      IF(ISTOFF.EQ.0) GO TO 12050
      IF(CDNAME.EQ.STACDS(13)) KL0=ICNL2(ISTOFF)
      IF(CDNAME.EQ.STACDS(14)) KH0=ICNH2(ISTOFF)
      IF(CDNAME.EQ.STACDS(13)) IOFF=IPVAL0(IXSTL2)+KL0-1
      IF(CDNAME.EQ.STACDS(14)) IOFF=IPVAL0(IXSTH2)+KH0-1
      RF(1)=PARMVC(IOFF   )
      WRITE(IOUT7,81530)CDNAME,ISTA,RF(1),RF(2)
      GO TO 12050
12245 IF(LPNALL)WRITE(IOUT7,81530)CDNAME,ISTA,RF(1),RF(2)
      GO TO 12050
!*********************************************************************
!  TIDES  **                                               * TIDES  **
!*********************************************************************
13000 CONTINUE
      LPUN=LPNALL
      IF(FIELD(2).EQ.C0D0) GO TO 50000
      LPUN=LPNADJ
      ITD   =1
      IF(IC78.EQ.21) ITD=3
      IF(IC78.EQ.30) ITD=2
      IOFF  =IPVAL0(IXTIDE)+KTIDA(ITD)-1
      FIELD(1)=PARMVC(IOFF)
      IF(IC78.EQ.21) FIELD(1)=FIELD(1)/DEGRAD
      GOTO 50000
!*********************************************************************
!  THRDRG **                                               * THRDRG **
!*********************************************************************
13500 CONTINUE
      LPUN=LPNALL
      GOTO 50000
!*********************************************************************
!  VLIGHT **                                               * VLIGHT **
!*********************************************************************
14000 CONTINUE
      LPUN=LPNALL
      IF(FIELD(3).EQ.C0D0) GOTO 50000
      LPUN=LPNADJ
      FIELD(1)=PARMVC(IPVAL0(IXVLIT))
      GOTO 50000
!*********************************************************************
!  YAWPOL **                                               * YAWPOL **
!*********************************************************************
15000 CONTINUE
        IF(LPNALL) WRITE(IOUT7,81200) CDNAME
  601   CONTINUE
        READ(IUNT14,81201,END=60000) CDNAMX
        IF(LPNALL) WRITE(IOUT7,81201) CDNAMX
        IF(CDNAMX.EQ.'YPLEND') GO TO 50000
        GO TO 601

!*********************************************************************
!  OUTPUT OPTION CARDS                                              **
!*********************************************************************
!*********************************************************************
50000 CONTINUE
!
!      ldebug = .true.
!
      if(ldebug)                                                        &
     & write(6,fmt='(1x,2a)') 'crdpun: below 50000 cdname = ', cdname
!
      if(ldebug)                                                        &
     & write(6,*) 'crdpun: below 50000 LPUN   = ', LPUN
!
      IF(.NOT.LPUN) then
!            ldebug = .false.
            GO TO 600
      endif
!
      IFMT=1
! TEST IF ALL INTEGER FIELDS ARE ZERO
      IF(IFIELD(1)+IFIELD(2)+IFIELD(3)+IFIELD(4)+                       &
     &   IFIELD(5)+IFIELD(6)+IFIELD(7)+IFIELD(8) .NE. 0) IFMT=2
! TEST IF ALL REAL FIELDS ARE ZERO
      IF(FIELD(1)+FIELD(2)+FIELD(3)+FIELD(4) .NE. C0D0) IFMT=IFMT+2
! IFMT=1 - PRINT CARD NAME ONLY; =2 PRINT INTEGER FIELDS;
!     =3 - PRINT REAL FIELDS   ; =4 PRINT INTEGER AND REAL FIELDS
      if(ldebug)write(6,*) 'crdpun: ifmt = ', ifmt
      GO TO (50020,50040,50060,50080), IFMT
50020 WRITE(IOUT7,81320) CDNAME
      if(ldebug)write(6,*) 'crdpun: below 50020 '
      if(ldebug)write(6,81320) CDNAME
!            ldebug = .false.
      GO TO 600
50040 WRITE(IOUT7,81320) CDNAME,IFLD15,IA3,IFLD78
      if(ldebug)write(6,*) 'crdpun: below 50040 '
      if(ldebug)write(6,81320) CDNAME,IFLD15,IA3,IFLD78
!            ldebug = .false.
      GO TO 600
50060 CONTINUE
!
! CHECK FOR SPECIAL NON-ADJUSTING OPTIONS
!
                                                     !ORBFIL
      IF(       CDNAME .eq. OPTNSP(1).or.                               &
     &          CDNAME .eq. OPTNSP(2).or.                               &
     &          CDNAME .eq. OPTNSP(3)       ) then
                                                     !TOPATT
                                                     !TOPBTM
         WRITE(IOUT7,81341) CDNAME,FIELD
      ELSE
         WRITE(IOUT7,81340) CDNAME,FIELD
      ENDIF
!
      if(ldebug)write(6,*) 'crdpun: below 50060 '
      if(ldebug)write(6,81340) CDNAME,FIELD
!            ldebug = .false.
      GO TO 600
50080 CONTINUE
      if(ldebug)write(6,*) 'crdpun: below 50080 '
      if(ldebug)write(6,*) FIELD
      if(ldebug)write(6,81320) CDNAME,IFLD15,IA3,IFLD78,FIELD
!
! CHECK FOR SPECIAL NON-ADJUSTING OPTIONS
!
                                                     ! MBIAS
      IF(       CDNAME .eq. OPTION(4).or.                               &
     &          CDNAME .eq. OPTION(5).or.                               &
     &          CDNAME .eq. OPTION(6).or.                               &
     &          CDNAME .eq. OPTION(7)       ) then
                                                     ! MBIAS1
                                                     ! MBIAS2
                                                     ! MBIAS3
         WRITE(IOUT7,81322) CDNAME,IFLD15,IA3,IFLD78,FIELD
                                                     !ORBFIL
      ELSEIF(   CDNAME .eq. OPTNSP(1).or.                               &
     &          CDNAME .eq. OPTNSP(2).or.                               &
     &          CDNAME .eq. 'ORBTVU'       ) then
                                                     !TOPATT
         WRITE(IOUT7,81323) CDNAME,IFLD15,IA3,IFLD78,FIELD
      ELSE
         WRITE(IOUT7,81320) CDNAME,IFLD15,IA3,IFLD78,FIELD
      ENDIF
!
      if(ldebug)write(6,81322) CDNAME,IFLD15,IA3,IFLD78,FIELD
!            ldebug = .false.
      GO TO 600
!*********************************************************************
! NORMAL END OF FILE - REWIND UNIT 14
!*********************************************************************
60000 REWIND IUNT14
      RETURN
! UNEXPECTED END OF FILE
60100 WRITE(IOUT6,80000) OPTION(ICARD)
      STOP
60200 WRITE(IOUT6,80200) OPTION(ICARD)
      STOP
60300 WRITE(IOUT6,80300)
      STOP
60350 WRITE(IOUT6,80350)
      STOP
60900 WRITE(IOUT6,89000) IARC,ITRGLB,ITRARC
      RETURN
!
! FORMATS
!
80000 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED WHILE SEARCHING',   &
     &   ' FOR ',A8/5X,' OR RELATED SUBGROUP CARDS')
80200 FORMAT(1X,'*** ABNORMAL TERMINATION IN ROUTINE CRDPUN ***',/,     &
     &1X,'*** NO MATCH FOUND FOR CURRENT PLMOON BODY NUMBER ***')
80300 FORMAT(1X,'*** ABNORMAL TERMINATION IN ROUTINE CRDPUN ***',/,     &
     &1X,'*** NOT EXPECTING NORMAL VECTOR FOR PANEL CARDS   ***')
80350 FORMAT(1X,'*** ABNORMAL TERMINATION IN ROUTINE CRDPUN ***',/,     &
     &1X,'*** NO SET FOUND FOR PANEL CARD SATID   ***')
80400 FORMAT(BZ,A6,I2,9X,I7,1PD20.13,15X,D13.6)
80401 FORMAT(BZ,A6,2I1,9X,I7,1PD20.13,15X,D13.6)
80405 FORMAT(BZ,A6,I2,I3,6X,I7,1PD20.13,15X,D13.6)
80420 FORMAT(BZ,A6,I2,9X,I7,D20.13,F15.2,D13.6)
!CCC80421 FORMAT(A6,I2,I2,7X,I7,D20.13,15X,D13.6)
80421 FORMAT(BZ,A6,I2,I2,7X,I7,D20.13,F15.2,D13.6)
80425 FORMAT(BZ,A6,I2,I3,6X,I7,D20.13,F15.2,D13.6)
81000 FORMAT(BZ,A6,I4,2I2)
81050 FORMAT(BZ,10A8)
                                                          ! read
81200 FORMAT(BZ,A6,4I2,I3,3A1,2I2,D20.8,D15.3,D13.1,D8.1)
81201 FORMAT(BZ,A80)
                                                             ! read
81320 FORMAT(BZ,A6,4I2,I3,3A1,2I2,1PD20.13,D15.8,D13.6,D8.1)
81321 FORMAT(BZ,A6,4I2,I3,3A1,2I2,1PD20.13,D15.8,D13.6,0PF8.5)
81322 FORMAT(BZ,A6,4I2,I3,3A1,2I2,1PD20.13,0PF15.1,F13.0,D8.1)
81323 FORMAT(BZ,A6,4I2,I3,3A1,2I2,F20.2,F15.1,D13.6,D8.1)
81340 FORMAT(BZ,A6,18X,        1PD20.13,D15.8,D13.6,D8.1)
81341 FORMAT(BZ,A6,18X,        F20.2,F15.1,D13.6,D8.1)
                                             ! read
81420 FORMAT(BZ,24X,D20.13,D15.8,D13.6,D8.1)
81430 FORMAT(BZ,24X,1PD20.13,D15.8,D13.6,D8.1)
                                               ! read
81500 FORMAT(BZ,A8,2I1,I2,2I4,3D15.6,I5,A8,A2)
81520 FORMAT(BZ,A8,2I1,I2, I8,1P,3D15.8,I5,A8,A2)
81530 FORMAT(BZ,A6,6X,I8,3D15.6)
81600 FORMAT('OTIDES',I2,2X,2I2,2I3,2I2,1PD20.13,D15.8,D13.6,D8.1)
81620 FORMAT('ETIDES',   4X, 4X,2I3,2I2,1PD20.13,D15.8,D13.6,D8.1)
81680 FORMAT('MBIAS ',I8,I3,I7,D20.13,F15.2,F13.0,1PD8.1)
81685 FORMAT('MBIAS ',I1,I8,I3,I7)
81688 FORMAT('MBIAS',I1,I8,3X,I7)
                                                ! read
81900 FORMAT(BZ,A6,I2,2I1,I2,8X,I6,I4,D10.3,I1)
81910 FORMAT(BZ,A6,I2,2I1,I2,8X,I6,I4,F10.7,I1)
                                        ! read
82200 FORMAT(BZ,A6,14X,3(I6,I4,D10.3) )
82250 FORMAT(BZ,A6,14X,3(I6,I4,F10.7) )
82260 FORMAT(BZ,A6,17X,I7 )
                                           ! read
82300 FORMAT(BZ,A6,I1,I1,2X,I4,6X,3D20.13)
82320 FORMAT(BZ,A6,14X,3D20.13)
82400 FORMAT(A6)
89000 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED WHILE SEARCHING',   &
     & ' ENDGLB OR ENDARC CARDS IN SUBROUTINE CRDPUN.'/                 &
     & 1X,'IARC,ITRGLB,ITRARC = ',3I5)
89001 FORMAT(I7)
89002 FORMAT(F15.2)
89003 FORMAT(I1)
89004 FORMAT(D20.8)
89005 FORMAT(D13.1)
89006 FORMAT(A80)
      END
