      SUBROUTINE FAST(MJDSEC,FSEC,AA,II,LL,NEQN,XP,VMAT,PXDDEX,         &
     &                XDDTOT,XDDSUN,XDDCMP,XDDSAT,XPS,XMNRT,XAXIS,      &
     &                OMEG,XAXISD,OMEGD,LONLYA,XDDTMC,LNOSAT,NCCALL,    &
     &                SFRAC,                                            &
!... Begin TJS...
     &                DORIED0,DORIED0T,DORIEDI,DORIEDIT,XDDP,IPS,XTGA,  &
     &                PXDDEXSC,NEQNS)
!... End TJS...
!
! INPUTS AND OUTPUTS IN TRUE OF REF
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      PARAMETER(MMAX=6)

! NEW RELATIVITY
      COMMON/BWLOGC/LVAREA,LTOPEX,LSPOT2,LGPSVA,LERS1,LMO,LMOC,LTDRSA,  &
     &              LMAGNA,LGFO,LTRMM,LEUVE,LVCL,LENVS,LCRYO,LGRAIL,    &
     &              LHY2A,LSARAL
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CBDACC/BD_ACCEL(10,999)
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/SUNJ2 /DXDDRJ2,PXDDJ2,SVXDDSN(3)
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
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
! NEW RELATIVITY

      COMMON/ASTSUN/BDAST(6),GMSAT,GEOPTS(3)
      COMMON/AXIS/LINTAX
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
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
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
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
      COMMON/CRMBI/RMBI(9)
      COMMON/CRMI/RMI(9)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/SETOPT/LSDRG,LSSRD,LSGA,LSORB,LTHDRG,LROCK4
      COMMON/SETPPT/JSPDRG(3),JSPSRD(3),JSPGA(3),JCSAT
      COMMON/TRQANG/DTQDAN(3,3,2)
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR

! Should be XMNRT, but having problems when dimensioning larger than XMNRT(1:3),
! so defining WMNRT in a COMMON BLOCK named INERTIA as a temporary patch
      COMMON/INERTIA/WMNRT(6,2)
!...End TJS DEBUG
! FOR MMAX=6, NMAXP1=7 AND NP=39
      DIMENSION ZMNRT(6)
      DIMENSION P(39),AORN(7),SINLM(7),COSLM(7),TANPSI(7)
      DIMENSION EXPRFL(39,6),D(39)
!
      DIMENSION AA(1),II(1),LL(1)
      DIMENSION XP(3,2),VMAT(3,6)

!... Begin TJS...
! d(d(orientation)/d(orientation_0))/dt =
!         d(d(orientation)/dt)/d(orientation) d(orientation)/d(orientation_0)
! DORIED0T = d(d(orientation)/d(orientation_0))/dt
! DORIED0  = d(orientation)/d(orientation_0)
!
! d(d(orientation)/d(inertia))/dt =
!         d(d(orientation)/dt)/d(orientation) d(orientation)/d(inertia)+
!         d(d(orientation)/dt)/d(inertia)
! DORIEDIT = d(d(orientation)/d(inertia))/dt
! DORIEDI  = d(orientation)/d(inertia)
!
! VOMAT    = d(d(orientation)/dt)/d(orientation)
      DIMENSION DORIED0T(12,12),DORIED0(12,12)
      DIMENSION DORIEDIT(12,6),DORIEDI(12,6)
      DIMENSION VOMAT(12,12)
      DIMENSION FSUNB(3),DFSBDU(3,3),DTIDR(3,9),DTIDX(3,3)
      DIMENSION DOTDI(3,6)
      DIMENSION DRDWDOR(3,12)
!... End TJS...

! NEW RELATIVITY
       DIMENSION PXDDEXSC(NEQNS,3)
       DIMENSION XPSUN(3,2)
       DIMENSION XAST(3)
       DIMENSION VAST(3)
       DIMENSION XAST_SUN_DIFF(3)
       DIMENSION XSAT_AST_DIFF(3)
       DIMENSION XSAT_DIFF_TOD(3)
       DIMENSION XAST_DIFF_TOD(3)
       DIMENSION XPS_AST_REF(3,2)
       DIMENSION XPTEMP(3,2)
       DIMENSION PXDDOT(NEQN,3)
       DIMENSION XTEMPS(6)
       DIMENSION XSATR(3)
       DIMENSION VSATR(3)
       DIMENSION DXDDR(3), DXDDRL(3), DXDDRL1(3), DXDDRL2(3), DXDDRLSC(3)
       DIMENSION DXDDRJ2(3),PXDDJ2(3),ACCEL_J2(3)
       DIMENSION REL_PM_ACCEL_SUM(3)
       DIMENSION REL_PM_ACCEL_SUM2(3)
       DIMENSION REL_PM_ACCEL_SUMA(3)
       DIMENSION REL_PM_ACCEL_SUMA2(3)
       DIMENSION DBETA_1(3),DBETA_2(3),DBETA_3(3),DBETA_4(3)
       DIMENSION DGAMMA_1(3),DGAMMA_2(3),DGAMMA_3(3),DGAMMA_4(3)
       DIMENSION DGM_1(3),DGM_2(3),DGM_3(3),DGM_4(3)

! NEW RELATIVITY

      DIMENSION XPN(3)
      DIMENSION PXDDEX(NEQN,3)
      DIMENSION XTEMP(6),HOLD(3,3),ISATID(1)
      DIMENSION XDDTOT(3),XDDSUN(3),XDDCMP(3),XDDSAT(3),XDDIF(3)
      DIMENSION XDDTMC(3)
      DIMENSION XDDSN2(3)
      DIMENSION HDDTOT(3),HDDCMP(3),HDDIF(3)
      DIMENSION SOLACC(3),PGRAV(3),XACC(3)
      DIMENSION IACCP(2)
! NEW REL XPS (3,2)
      DIMENSION XPS(3,2),XMNRT(3),XAXIS(3,3),OMEG(3),XAXISD(3,3),OMEGD(3)
      DIMENSION FSAT(3),FSUN(3),TGA(3),XTGA(3),OMEGA(3),TTGA(3)
      DIMENSION VRH(1),RMBIH(1),INDGH(1)
      DIMENSION VRHH(5),RMBIHH(9),INDGHH(10)
      DIMENSION AAA(25),III(1),LLL(1)
      DIMENSION ROTI(9),DUMQ(3)
      DIMENSION XDDP(3)
      DATA GCON/ 6.67300D-11/
      DATA FOUR/4.D0/
!... Begin TJS...
      DATA DDDR /57.2957795130823D0/
!... End TJS...
      EQUIVALENCE(VRH(1),SINPSI)
      EQUIVALENCE(RMBIH(1),RMBI(1))
      EQUIVALENCE(INDGH(1),NMAX)
!
      IF(IPS.EQ.2.AND..NOT.LINTAX) RETURN
      DO I=1,6
        ZMNRT(I)=WMNRT(I,IPS)
      ENDDO
!
! IF CALL IS FOR 2ND AST, PERFORM ONLY ORIENTATION COMPUTATIONS
      IF(LBINAST.AND.IPS.EQ.2) GO TO 25
      IF(.NOT.LBINAST) THEN
        DO I=1,3
          TTGA(I)=0.D0
        ENDDO
      ENDIF
!
      IF (NEQN.GT.0.AND..NOT.LBINAST) THEN
        DO I=1,NEQN
          PXDDEX(I,1)=0.D0
          PXDDEX(I,2)=0.D0
          PXDDEX(I,3)=0.D0
        END DO
      END IF
      IF (NEQN.GT.0) THEN
        DO I=1,NEQN
          PXDDOT(I,1)=0.D0
          PXDDOT(I,2)=0.D0
          PXDDOT(I,3)=0.D0
        END DO
      END IF

!
!!!
!!!  LALLS THAT WOULD HAVE BEEN MADE BY F
      IF(LNOSAT) THEN
         CALL PREFCR(NCCALL,SFRAC,.FALSE.,AA,COSTG,SINTG,ROTI)
         CALL REFCRS(.FALSE.,SINTG,COSTG,                               &
     &   AA(KDPSR),AA(KXPUT),AA(KYPUT),MJDSEC,FSEC,ROTI(1),             &
     &   ROTI(2),ROTI(3),ROTI(4),ROTI(5),ROTI(6),ROTI(7),               &
     &   ROTI(8),ROTI(9),AA(KA1UT),                                     &
     &   II(KINDPI),AA(KXDPOL),AA(KXDOTP),0)
         CALL PLANPO(MJDSEC,FSEC,.TRUE.,.FALSE.,AA,II)
         CALL SUNGRV(1,-999,DUMQ,0.0D0,.TRUE.,DUMQ,DUMQ)
      ENDIF
      INDVR=1
      INDCR=15
!
      DO I=1,5
        VRHH(I)=VRH(I)
      ENDDO
      DO I=1,9
        RMBIHH(I)=RMBIH(I)
      ENDDO
      DO I=1,10
        INDGHH(I)=INDGH(I)
      ENDDO
!
!
      ISATID(1)=II(KISATN)
      GMS=BDTRUE(4,8)
      GMSX=GMS+GM
!
!!!!!!!!! PATCH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      GMSAT=1.D0
      FACT=10.D0/250.D0
      GMSAT=GM*FACT*FACT*FACT
      GMSAT=0.D0
!!!!!!!!! PATCH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! INTIALIZET OUTPUT ARRAYS (ACCEL AND VMAT)
!
      R2=XP(1,1)*XP(1,1)+XP(2,1)*XP(2,1)+XP(3,1)*XP(3,1)
      R=SQRT(R2)
      R3=R*R2
      R5=R3*R2
      GS=GMSAT/GM
      DO 20 I=1,3
      XDDSUN(I)=-GMSX*XP(I,1)/R3
      IF(LNOSAT) THEN
        XDDTOT(I)=XDDSUN(I)
        XDDCMP(I)=0.0D0
        XDDIF(I)=0.0D0
      ELSE
        XDDTOT(I)=XDDSUN(I)-GS*GEOPTS(I)
        XDDCMP(I)=-GS*GEOPTS(I)
        XDDIF(I)=-GS*GEOPTS(I)
      ENDIF
! IF THE RUN IS FOR A BINARY ASTEROID SYSTEM, ADD IN THE ACCELERATION
! IMPARTED ON THE PRIMARY BY THE SECONDARY ASTEROID. THE DIFFERENTIAL
! EFFECT ON THE ARTIFICIAL SATELLITE FROM THE ACCELERATION IMPARTED ON
! THE PRIMARY ASTEROID  BY THE SECONDARY HAS ALREADT BEEB TAKEB INTO
! ACCOUNT IN SUBROUTINE F. SO THERE IS NO CONTRIBUTION TO XDDIF
!
      IF(LBINAST) THEN
        XDDTOT(I)=XDDTOT(I)+XDDP(I)
        XDDCMP(I)=XDDCMP(I)+XDDP(I)
      ENDIF
      HDDTOT(I)=0.0D0
      HDDCMP(I)=0.0D0
      HDDIF(I)=0.0D0
      DO 10 J=1,6
      VMAT(I,J)=0.0D0
  10  CONTINUE
  20  CONTINUE

!!!!!
!! SUN'S J2 ACCELERATION AND PARTIAL DERIVATIVE ON THE CENTRAL BODY
!!!!!

!            write(6,*)' dbg NEQN in FAST ',NEQN
!       IF(NPVAL(IXFGFM).GT.1) THEN
        IF(NPVAL(IXJ2SN).GT.1) THEN
!       ! C20S IS IN 2ND SLOT GOF GLOBAL PHANTOM FM
!       C20S=AA(KPRMV+IPVAL(IXFGFM))
        C20S=AA(KPRMV+IPVAL(IXJ2SN)-1)
        XSATR(1)=XPS(1,1)
        XSATR(2)=XPS(2,1)
        XSATR(3)=XPS(3,1)
        CALL SJ2(C20S,XSATR,DXDDR,NEQN,PXDDOT,MJDSEC,FSEC,AA,II)
        DO I=1,3

! WE ARE LOADING HERE.
          XDDTOT(I)=XDDTOT(I)+DXDDRJ2(I)                    ! J2 ACCEL
          XDDCMP(I)=XDDCMP(I)+DXDDRJ2(I)

!         PXDDEX(NEQN-2,I)=PXDDJ2(I)                        ! J2 PARTLS
          PXDDEX(NEQN-1,I)=PXDDJ2(I)                        ! J2 PARTLS


        ENDDO
        ENDIF

!!!

!      WRITE(6,87667) XDDSUN(1)/GMSX,XDDSUN(2)/GMSX,XDDSUN(3)/GMSX
!87667    FORMAT("Part. GM. Merc. OLD",3D25.11)

!
! ADD ACC OF INTGCB BODY TO BD_ACCEL ARRAY
! SUM XDDSUM (ACC OF INTGCB BODY WITH RSPCT TO SUN)
! AND ACC OF SUN

      BD_ACCEL(7,12)=BD_ACCEL(7,8)+XDDSUN(1)
      BD_ACCEL(8,12)=BD_ACCEL(8,8)+XDDSUN(2)
      BD_ACCEL(9,12)=BD_ACCEL(9,8)+XDDSUN(3)

!!!
!
! RELATIVISTIC PT MASS ACCEL
!
!! THIS PART HAS BEEN SUBSTITUTED BY REL_PM_ACC !!!
!
!!!      VLIT2=VLIGHT*VLIGHT
!!!      CFACT=GMSX/(R3*VLIT2)
!!!      VEL2=XP(1,2)*XP(1,2)+XP(2,2)*XP(2,2)+XP(3,2)*XP(3,2)
!!!      XFACT=(FOUR*GMSX/R-VEL2)*CFACT
!!!      VFACT=(XP(1,1)*XP(1,2)+XP(2,1)*XP(2,2)+XP(3,1)*XP(3,2))*FOUR*CFACT


!!!      DXDDRL(1)=XFACT*XP(1,1)+VFACT*XP(1,2)
!!!      DXDDRL(2)=XFACT*XP(2,1)+VFACT*XP(2,2)
!!!      DXDDRL(3)=XFACT*XP(3,1)+VFACT*XP(3,2)


!!!      DO I=1,3
!!!        XDDTOT(I)=XDDTOT(I)+DXDDRL(I)
!!!        XDDCMP(I)=XDDCMP(I)+DXDDRL(I)
!!!      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! COMPUTE PLANET GRAVIRY FIELD BOUNCED OFF THE SUN
      NMAX=4
      IF(NMAX.GT.MMAX) THEN
        WRITE(6,6000) NMAX,MMAX
        STOP
      ENDIF
      NMAXP1=NMAX+1
      NP=NMAX*(NMAX+1)/2+NMAX*3
      XPN(1)=-XP(1,1)
      XPN(2)=-XP(2,1)
      XPN(3)=-XP(3,1)
      CALL ECFSET(.TRUE.,XPN,INDCR,INDVR,R,R2,1,1,GMR3,AAA,III,LLL)
      C10H=AA(KCN)
      C11H=AA(KCN+1)
      S11H=AA(KSN+1)
!
      AA(KCN)=0.D0
      AA(KCN+1)=0.D0
      AA(KSN+1)=0.D0

!... Begin TJS...
!...  CALL EGRAV(AA(KXNRMZ),AA(KXNRMC),P,AA(KCN),AA(KSN),               &
!... &      AORN,SINLM,COSLM,                                           &
!... &      TANPSI,AA(KXM),AA(KXNP1),EXPRFL,                            &
!... &      XDDSN2,AAA(INDVR),D)
      CALL EGRAVG(AA(KXNRMZ),AA(KXNRMC),P,AA(KCN),AA(KSN),              &
     &      AORN,SINLM,COSLM,                                           &
     &      TANPSI,AA(KXM),AA(KXNP1),EXPRFL,                            &
     &      XDDSN2,AAA(INDVR),D,FSUNB,DFSBDU)
      FSUNB(1)=-FSUNB(1)*GMS/GCON
      FSUNB(2)=-FSUNB(2)*GMS/GCON
      FSUNB(3)=-FSUNB(3)*GMS/GCON
      DFSBDU(1,1)=-DFSBDU(1,1)*GMS/GCON
      DFSBDU(2,1)=-DFSBDU(2,1)*GMS/GCON
      DFSBDU(3,1)=-DFSBDU(3,1)*GMS/GCON
      DFSBDU(1,2)=-DFSBDU(1,2)*GMS/GCON
      DFSBDU(2,2)=-DFSBDU(2,2)*GMS/GCON
      DFSBDU(3,2)=-DFSBDU(3,2)*GMS/GCON
      DFSBDU(1,3)=-DFSBDU(1,3)*GMS/GCON
      DFSBDU(2,3)=-DFSBDU(2,3)*GMS/GCON
      DFSBDU(3,3)=-DFSBDU(3,3)*GMS/GCON
!... End TJS...
      AA(KCN)=C10H
      AA(KCN+1)=C11H
      AA(KSN+1)=S11H
!
!   OUT OF EGRAV XDDSN2 IS THE ACCELERATION IF THE SUN TOWARDS THE ASTEROID
      XDDSN2(1)=-XDDSN2(1)*GMS/GM
      XDDSN2(2)=-XDDSN2(2)*GMS/GM
      XDDSN2(3)=-XDDSN2(3)*GMS/GM
!
      DO I=1,3
        XDDTOT(I)=XDDTOT(I)+XDDSN2(I)
        XDDCMP(I)=XDDCMP(I)+XDDSN2(I)
        XDDIF(I)=XDDIF(I)+XDDSN2(I)
      ENDDO
!
      DO I=1,5
        VRH(I)=VRHH(I)
      ENDDO
      DO I=1,9
        RMBIH(I)=RMBIHH(I)
      ENDDO
      DO I=1,10
        INDGH(I)=INDGHH(I)
      ENDDO
!
!  END PLANET GRAVIRY FIELD BOUNCED OFF THE SUN
!
      IF(.NOT.LINTAX) GO TO 30
!
! AXES AS AN INTEGRATED QUANTITY
!
!    FIRST DEAL WITH ORBITED BODY; THEN DEAL WITH SUN
!
!    NEED TO TAKE OUT CB TERM  (PROBABLY NOT BECAUSE ONLY USED IN CROSS PRODUCT)
!
      IF(.NOT.LNOSAT) THEN
         XMSAT=GMSAT/GCON
         FSAT(1)=GEOPTS(1)*XMSAT
         FSAT(2)=GEOPTS(2)*XMSAT
         FSAT(3)=GEOPTS(3)*XMSAT
         TTGA(1)=-XPS(2,1)*FSAT(3)+XPS(3,1)*FSAT(2)
         TTGA(2)=-XPS(3,1)*FSAT(1)+XPS(1,1)*FSAT(3)
         TTGA(3)=-XPS(1,1)*FSAT(2)+XPS(2,1)*FSAT(1)
      ELSE
         TTGA(1)=0.0D0
         TTGA(2)=0.0D0
         TTGA(3)=0.0D0
      ENDIF
!

! BEGIN CODE (AND CONTINUING THROUGH 30) FOR ORIRNTAION
! AXES AS AN INTEGRATED QUANTITY
!
! IN THE CASE OF BINAY ASTEROID SKIP SUN TORQUE
! WORRY ONLY ABOUT MUTUAL TORQUES
!
      IF(LBINAST) GO TO 25
!
!
!     DEAL WITH SUN
!
      XMAST=GM/GCON
      FSUN(1)=-XDDSN2(1)*XMAST
      FSUN(2)=-XDDSN2(2)*XMAST
      FSUN(3)=-XDDSN2(3)*XMAST
      TTGA(1)=TTGA(1)-XPN(2)*FSUN(3)+XPN(3)*FSUN(2)
      TTGA(2)=TTGA(2)-XPN(3)*FSUN(1)+XPN(1)*FSUN(3)
      TTGA(3)=TTGA(3)-XPN(1)*FSUN(2)+XPN(2)*FSUN(1)
!
      DO I=1,3
        XTGA(I)=TTGA(1)*XAXIS(1,I)+TTGA(2)*XAXIS(2,I)+TTGA(3)*XAXIS(3,I)
      ENDDO
!
!  START HERE FOR SECONDARY ASTEROID
!
  25  CONTINUE
!... Begin TJS...
! Replace calculation of d{omega}/dt in principal axis system with general
! body-fixed system:  d{omega}/dt = I_c^{-1} (T-E_{omega} I_c omega)
!
! ZMNRT(1:6) contains the six independent elements of symmetric, positive-
! definite matrix I_c(1:3,1:3) such that:
!
!                         / ZMNRT(1) ZMNRT(4) ZMNRT(6) \
!                   I_c = | ZMNRT(4) ZMNRT(2) ZMNRT(5) |
!                         \ ZMNRT(6) ZMNRT(5) ZMNRT(3) /
!
!...  OMEGD(1)=(XTGA(1)-(XMNRT(3)-XMNRT(2))*OMEG(2)*OMEG(3))/XMNRT(1)
!...  OMEGD(2)=(XTGA(2)-(XMNRT(1)-XMNRT(3))*OMEG(1)*OMEG(3))/XMNRT(2)
!...  OMEGD(3)=(XTGA(3)-(XMNRT(2)-XMNRT(1))*OMEG(1)*OMEG(2))/XMNRT(3)

      VIOX=ZMNRT(1)*OMEG(1)+ZMNRT(4)*OMEG(2)+ZMNRT(6)*OMEG(3)
      VIOY=ZMNRT(4)*OMEG(1)+ZMNRT(2)*OMEG(2)+ZMNRT(5)*OMEG(3)
      VIOZ=ZMNRT(6)*OMEG(1)+ZMNRT(5)*OMEG(2)+ZMNRT(3)*OMEG(3)
      VOXIOX=             -OMEG(3)*VIOY+OMEG(2)*VIOZ
      VOXIOY= OMEG(3)*VIOX             -OMEG(1)*VIOZ
      VOXIOZ=-OMEG(2)*VIOX+OMEG(1)*VIOY
      OMEGD(1)=XTGA(1)-VOXIOX
      OMEGD(2)=XTGA(2)-VOXIOY
      OMEGD(3)=XTGA(3)-VOXIOZ
      CALL MULTIINV (3,1,ZMNRT,OMEGD)
!... End TJS...
!
      DO I=1,3
       OMEGA(I)=OMEG(1)*XAXIS(I,1)+OMEG(2)*XAXIS(I,2)+OMEG(3)*XAXIS(I,3)
      ENDDO
      DO I=1,3
        XAXISD(1,I)=OMEGA(2)*XAXIS(3,I)-OMEGA(3)*XAXIS(2,I)
        XAXISD(2,I)=OMEGA(3)*XAXIS(1,I)-OMEGA(1)*XAXIS(3,I)
        XAXISD(3,I)=OMEGA(1)*XAXIS(2,I)-OMEGA(2)*XAXIS(1,I)
      ENDDO

!... Begin TJS...
      DO I=1,12
         DO J=1,12
            VOMAT(I,J)=0.D0
         END DO
      END DO
! d(d(omega)/dt)/d(omega) = I_c^{-1} (E_{I_c omega}-E_{omega} I_c)
      VOMAT( 1, 1)=                  OMEG(3)*ZMNRT(4)-OMEG(2)*ZMNRT(6)
      VOMAT( 2, 1)=-OMEG(3)*ZMNRT(1)                 +OMEG(1)*ZMNRT(6)
      VOMAT( 3, 1)= OMEG(2)*ZMNRT(1)-OMEG(1)*ZMNRT(4)
      VOMAT( 1, 2)=                  OMEG(3)*ZMNRT(2)-OMEG(2)*ZMNRT(5)
      VOMAT( 2, 2)=-OMEG(3)*ZMNRT(4)                 +OMEG(1)*ZMNRT(5)
      VOMAT( 3, 2)= OMEG(2)*ZMNRT(4)-OMEG(1)*ZMNRT(2)
      VOMAT( 1, 3)=                  OMEG(3)*ZMNRT(5)-OMEG(2)*ZMNRT(3)
      VOMAT( 2, 3)=-OMEG(3)*ZMNRT(6)                 +OMEG(1)*ZMNRT(3)
      VOMAT( 3, 3)= OMEG(2)*ZMNRT(6)-OMEG(1)*ZMNRT(5)
      VOMAT( 1, 2)=VOMAT( 1, 2)-VIOZ
      VOMAT( 1, 3)=VOMAT( 1, 3)+VIOY
      VOMAT( 2, 1)=VOMAT( 2, 1)+VIOZ
      VOMAT( 2, 3)=VOMAT( 2, 3)-VIOX
      VOMAT( 3, 1)=VOMAT( 3, 1)-VIOY
      VOMAT( 3, 2)=VOMAT( 3, 2)+VIOX
!
!
!  TROUGH 27 CONTINUE IS CODE TO PUT THE EFFECT OF TORQUE INTO THE
!  VARIATIONAL EQUATIONS
!
      IF(LBINAST) GO TO 26
!
!   UP TO 26 CONTINUE IS THE CODE FOR THE TORQUE THAT A SINGLE ASTEROID
!   GETS FROM THE SUN
!
!
!
! d(d(omega)/dt)/d(axis) = I_c^{-1} kron(I,T_{inertial}^T)+
!                          I_c^{-1} R^T d(T_{inertial}/d(axis)
!
! d(T_{inertial}/d(axis) = kron(f_{sun}_B^T,E_{sun})+
!                          E_{sun}*R*d{f_{sun}_B}/d{axis}
!
!   d{f_{sun}_B}/d{axis} = d{f_{sun}_B}/d{u_{sin}_B}*kron(I,p_{sun}^T)
!
!              f_{sun}_B = sun force vector in body-fixed system
!                E_{sun} = sun position cross product matrix in inertial system
!              u_{sun}_B = unit sun position vector in body-fixed system
!                   kron = kronecker product
!
! first term...
! d(d(omega)/dt)/d(axis) = I_c^{-1} kron(I,T_{inertial}^T)
      VOMAT( 1, 4)=TTGA(1)
      VOMAT( 1, 5)=TTGA(2)
      VOMAT( 1, 6)=TTGA(3)
      VOMAT( 2, 7)=TTGA(1)
      VOMAT( 2, 8)=TTGA(2)
      VOMAT( 2, 9)=TTGA(3)
      VOMAT( 3,10)=TTGA(1)
      VOMAT( 3,11)=TTGA(2)
      VOMAT( 3,12)=TTGA(3)
! second term...
      DO I=1,3
         DO J=1,9
            DTIDR(I,J)=0.D0
         END DO
      END DO
! d(T_{inertial}/d(axis) = kron(f_{sun}_B^T,E_{sun})
      DTIDR(1,2)=-FSUNB(1)*XPN(3)
      DTIDR(1,3)= FSUNB(1)*XPN(2)
      DTIDR(2,1)= FSUNB(1)*XPN(3)
      DTIDR(2,3)=-FSUNB(1)*XPN(1)
      DTIDR(3,1)=-FSUNB(1)*XPN(2)
      DTIDR(3,2)= FSUNB(1)*XPN(1)
      DTIDR(1,5)=-FSUNB(2)*XPN(3)
      DTIDR(1,6)= FSUNB(2)*XPN(2)
      DTIDR(2,4)= FSUNB(2)*XPN(3)
      DTIDR(2,6)=-FSUNB(2)*XPN(1)
      DTIDR(3,4)=-FSUNB(2)*XPN(2)
      DTIDR(3,5)= FSUNB(2)*XPN(1)
      DTIDR(1,8)=-FSUNB(3)*XPN(3)
      DTIDR(1,9)= FSUNB(3)*XPN(2)
      DTIDR(2,7)= FSUNB(3)*XPN(3)
      DTIDR(2,9)=-FSUNB(3)*XPN(1)
      DTIDR(3,7)=-FSUNB(3)*XPN(2)
      DTIDR(3,8)= FSUNB(3)*XPN(1)
! R*d{f_{sun}_B}/d{u_{sin}_B}
      DO I=1,3
         DO J=1,3
            DTIDX(I,J)=XAXIS(I,1)*DFSBDU(1,J)+                         &
     &                 XAXIS(I,2)*DFSBDU(2,J)+                         &
     &                 XAXIS(I,3)*DFSBDU(3,J)
         END DO
      END DO
! E_{sun}*R*d{f_{sun}_B}/d{u_{sin}_B}
      DO J=1,3
         DTID1=                  -XPN(3)*DTIDX(2,J)+XPN(2)*DTIDX(3,J)
         DTID2= XPN(3)*DTIDX(1,J)                  -XPN(1)*DTIDX(3,J)
         DTID3=-XPN(2)*DTIDX(1,J)+XPN(1)*DTIDX(2,J)
         DTIDX(1,J)=DTID1
         DTIDX(2,J)=DTID2
         DTIDX(3,J)=DTID3
      END DO
! d(T_{inertial}/d(axis) = d(T_{inertial}/d(axis)+
!                          E_{sun}*R*d{f_{sun}_B}/d{u_{sin}_B}*
!                          kron(I,p_{sun}^T)
      DO J=1,3
         DO K=1,3
            DO I=1,3
               DTIDR(I,3*(J-1)+K)=DTIDR(I,3*(J-1)+K)+DTIDX(I,J)*XPN(K)
            END DO
         END DO
      END DO
! d(d(omega)/dt)/d(axis) = d(d(omega)/dt)/d(axis)+
!                          I_c^{-1} R^T d(T_{inertial})/d(axis)
      DO I=1,3
         DO J=1,9
            DTBDR=XAXIS(1,I)*DTIDR(1,J)+                                &
     &            XAXIS(2,I)*DTIDR(2,J)+                                &
     &            XAXIS(3,I)*DTIDR(3,J)
            VOMAT(I,3+J)=VOMAT(I,3+J)+DTBDR
         END DO
      END DO
      CALL MULTIINV (12,12,ZMNRT,VOMAT)
      GO TO 27
 26   CONTINUE
!
! CODE FOR ADDING THE TORQUE FROM THE OTHER ASTEROID
! IN A BINARY ASTEROID SYSTEM
!
! d(angle)/d(axis)
      CALL GETDADO (XAXIS,DRDWDOR)
! d(d(omega)/dt)/d(axis) = I_c^{-1} d(T_{body})/d(angle) d(angle)/d(axis)
      DO I=1,3
         DO J=4,12
            VOMAT(I,J)=DTQDAN(I,1,IPS)*DRDWDOR(1,J)/DDDR+               &
                       DTQDAN(I,2,IPS)*DRDWDOR(2,J)/DDDR+               &
                       DTQDAN(I,3,IPS)*DRDWDOR(3,J)/DDDR
         END DO
      END DO
      CALL MULTIINV (12,12,ZMNRT,VOMAT)
 27   CONTINUE
! d(d(axis)/dt)/d(omega) = kron(I,R) d(vec(E_{omega}))/d(omega),
!                    vec = column stacking operator
      VOMAT( 7, 1)= XAXIS(1,3)
      VOMAT( 8, 1)= XAXIS(2,3)
      VOMAT( 9, 1)= XAXIS(3,3)
      VOMAT(10, 1)=-XAXIS(1,2)
      VOMAT(11, 1)=-XAXIS(2,2)
      VOMAT(12, 1)=-XAXIS(3,2)
      VOMAT( 4, 2)=-XAXIS(1,3)
      VOMAT( 5, 2)=-XAXIS(2,3)
      VOMAT( 6, 2)=-XAXIS(3,3)
      VOMAT(10, 2)= XAXIS(1,1)
      VOMAT(11, 2)= XAXIS(2,1)
      VOMAT(12, 2)= XAXIS(3,1)
      VOMAT( 4, 3)= XAXIS(1,2)
      VOMAT( 5, 3)= XAXIS(2,2)
      VOMAT( 6, 3)= XAXIS(3,2)
      VOMAT( 7, 3)=-XAXIS(1,1)
      VOMAT( 8, 3)=-XAXIS(2,1)
      VOMAT( 9, 3)=-XAXIS(3,1)
! d(d(axis)/dt)/d(axis) = -kron(E_{omega},I)
      VOMAT( 4, 7)= OMEG(3)
      VOMAT( 5, 8)= OMEG(3)
      VOMAT( 6, 9)= OMEG(3)
      VOMAT( 4,10)=-OMEG(2)
      VOMAT( 5,11)=-OMEG(2)
      VOMAT( 6,12)=-OMEG(2)
      VOMAT( 7, 4)=-OMEG(3)
      VOMAT( 8, 5)=-OMEG(3)
      VOMAT( 9, 6)=-OMEG(3)
      VOMAT( 7,10)= OMEG(1)
      VOMAT( 8,11)= OMEG(1)
      VOMAT( 9,12)= OMEG(1)
      VOMAT(10, 4)= OMEG(2)
      VOMAT(11, 5)= OMEG(2)
      VOMAT(12, 6)= OMEG(2)
      VOMAT(10, 7)=-OMEG(1)
      VOMAT(11, 8)=-OMEG(1)
      VOMAT(12, 9)=-OMEG(1)
! d(d(orientation)/d(orientation_0))/dt =
!         d(d(orientation)/dt)/d(orientation) d(orientation)/d(orientation_0)
      DO I=1,12
         DO J=1,12
            DORIED0T(I,J)=VOMAT(I, 1)*DORIED0( 1,J)+                    &
     &                    VOMAT(I, 2)*DORIED0( 2,J)+                    &
     &                    VOMAT(I, 3)*DORIED0( 3,J)+                    &
     &                    VOMAT(I, 4)*DORIED0( 4,J)+                    &
     &                    VOMAT(I, 5)*DORIED0( 5,J)+                    &
     &                    VOMAT(I, 6)*DORIED0( 6,J)+                    &
     &                    VOMAT(I, 7)*DORIED0( 7,J)+                    &
     &                    VOMAT(I, 8)*DORIED0( 8,J)+                    &
     &                    VOMAT(I, 9)*DORIED0( 9,J)+                    &
     &                    VOMAT(I,10)*DORIED0(10,J)+                    &
     &                    VOMAT(I,11)*DORIED0(11,J)+                    &
     &                    VOMAT(I,12)*DORIED0(12,J)
         END DO
      END DO
! d(d(orientation)/d(inertia))/dt =
!         d(d(orientation)/dt)/d(orientation) d(orientation)/d(inertia)+
!         d(d(orientation)/dt)/d(inertia)
!
! first term...
! d(d(orientation)/d(inertia))/dt =
!         d(d(orientation)/dt)/d(orientation) d(orientation)/d(inertia)
      DO I=1,12
         DO J=1,6
            DORIEDIT(I,J)=VOMAT(I, 1)*DORIEDI( 1,J)+                    &
     &                    VOMAT(I, 2)*DORIEDI( 2,J)+                    &
     &                    VOMAT(I, 3)*DORIEDI( 3,J)+                    &
     &                    VOMAT(I, 4)*DORIEDI( 4,J)+                    &
     &                    VOMAT(I, 5)*DORIEDI( 5,J)+                    &
     &                    VOMAT(I, 6)*DORIEDI( 6,J)+                    &
     &                    VOMAT(I, 7)*DORIEDI( 7,J)+                    &
     &                    VOMAT(I, 8)*DORIEDI( 8,J)+                    &
     &                    VOMAT(I, 9)*DORIEDI( 9,J)+                    &
     &                    VOMAT(I,10)*DORIEDI(10,J)+                    &
     &                    VOMAT(I,11)*DORIEDI(11,J)+                    &
     &                    VOMAT(I,12)*DORIEDI(12,J)
         END DO
      END DO
! second term...explicit partial with respect to six independent elements
! of symmetric, postive definite moment of inertia matrix I_c conceptually
! stored in vector s(1:6) such that
!
!                         / s(1) s(4) s(6) \
!                   I_c = | s(4) s(2) s(5) |
!                         \ s(6) s(5) s(3) /
!
! d(d(omega)/dt)/d(inertia) = -I_c^{-1}*[d(I_c)/ds*d(omega)/dt+
!                                        E_{omega}*d(I_c)/ds*omega]
      DO I=1,3
         DO J=1,6
            DOTDI(I,J)=0.D0
         END DO
      END DO
! -d(I_c)/ds*omega
      DOTDI(1,1)=-OMEG(1)
      DOTDI(2,2)=-OMEG(2)
      DOTDI(3,3)=-OMEG(3)
      DOTDI(1,4)=-OMEG(2)
      DOTDI(2,4)=-OMEG(1)
      DOTDI(2,5)=-OMEG(3)
      DOTDI(3,5)=-OMEG(2)
      DOTDI(1,6)=-OMEG(3)
      DOTDI(3,6)=-OMEG(1)
! -E_{omega}*d(I_c)/ds*omega
      DO J=1,6
         VX=                   -OMEG(3)*DOTDI(2,J)+OMEG(2)*DOTDI(3,J)
         VY= OMEG(3)*DOTDI(1,J)                   -OMEG(1)*DOTDI(3,J)
         VZ=-OMEG(2)*DOTDI(1,J)+OMEG(1)*DOTDI(2,J)
         DOTDI(1,J)=VX
         DOTDI(2,J)=VY
         DOTDI(3,J)=VZ
      END DO
! -d(I_c)/ds*d(omega)/dt-E_{omega}*d(I_c)/ds*omega
      DOTDI(1,1)=DOTDI(1,1)-OMEGD(1)
      DOTDI(2,2)=DOTDI(2,2)-OMEGD(2)
      DOTDI(3,3)=DOTDI(3,3)-OMEGD(3)
      DOTDI(1,4)=DOTDI(1,4)-OMEGD(2)
      DOTDI(2,4)=DOTDI(2,4)-OMEGD(1)
      DOTDI(2,5)=DOTDI(2,5)-OMEGD(3)
      DOTDI(3,5)=DOTDI(3,5)-OMEGD(2)
      DOTDI(1,6)=DOTDI(1,6)-OMEGD(3)
      DOTDI(3,6)=DOTDI(3,6)-OMEGD(1)
! d(d(omega)/dt)/d(inertia) = -I_c^{-1}*[d(I_c)/ds*d(omega)/dt+
!                                        E_{omega}*d(I_c)/ds*omega]
      CALL MULTIINV (3,6,ZMNRT,DOTDI)
! second term...
! d(d(orientation)/d(inertia))/dt =
!         d(d(orientation)/dt)/d(orientation) d(orientation)/d(inertia)+
!         d(d(orientation)/dt)/d(inertia)
!
! note: d(d(axis)/dt)/d(inertia)=0
      DO I=1,3
         DO J=1,6
            DORIEDIT(I,J)=DORIEDIT(I,J)+DOTDI(I,J)
         END DO
      END DO
!... End TJS...

!
!
      IF(LONLYA.OR.IPS.EQ.2) GO TO 910
!
!
  30  CONTINUE
!
!!! SET UP POINTERS FOR ARC ROUTINRS
!
      IF(.NOT.LNOSAT) CALL SWTCPT(1,.FALSE.)
      ICBDH=ICBDGM
      ICBDGM=11

      IAPLM=JSPSRD(1)
      IAPGM=JSPSRD(2)
      ISTSRD=JSPSRD(3)
      ILAJSP=ISTSRD-KSTSRD+KLAJSP
      IGA=JSPGA(1)
      ISTGA=JSPGA(2)
      IDIFQ=ISTGA-KSTACC
! SUBROUTINES SOLRD, SUNGRV AND RESON USE TOD ; NEED SAT TRUE OF DATE
! COORDINATES AND THEN WILL NEED TO ROTATE ACCEl BACK TO TRUE OF REF
      XTEMP(1)=RMI(1)*XP(1,1)+RMI(4)*XP(2,1)+RMI(7)*XP(3,1)
      XTEMP(2)=RMI(2)*XP(1,1)+RMI(5)*XP(2,1)+RMI(8)*XP(3,1)
      XTEMP(3)=RMI(3)*XP(1,1)+RMI(6)*XP(2,1)+RMI(9)*XP(3,1)
      XTEMP(4)=RMI(1)*XP(1,2)+RMI(4)*XP(2,2)+RMI(7)*XP(3,2)
      XTEMP(5)=RMI(2)*XP(1,2)+RMI(5)*XP(2,2)+RMI(8)*XP(3,2)
      XTEMP(6)=RMI(3)*XP(1,2)+RMI(6)*XP(2,2)+RMI(9)*XP(3,2)

      XTEMPS(1)=RMI(1)*XPS(1,1)+RMI(4)*XPS(2,1)+RMI(7)*XPS(3,1)
      XTEMPS(2)=RMI(2)*XPS(1,1)+RMI(5)*XPS(2,1)+RMI(8)*XPS(3,1)
      XTEMPS(3)=RMI(3)*XPS(1,1)+RMI(6)*XPS(2,1)+RMI(9)*XPS(3,1)
      XTEMPS(4)=RMI(1)*XPS(1,2)+RMI(4)*XPS(2,2)+RMI(7)*XPS(3,2)
      XTEMPS(5)=RMI(2)*XPS(1,2)+RMI(5)*XPS(2,2)+RMI(8)*XPS(3,2)
      XTEMPS(6)=RMI(3)*XPS(1,2)+RMI(6)*XPS(2,2)+RMI(9)*XPS(3,2)
!!!!!FOR SOLRD AND RESON RESET SUN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!  THE CODE AND RESULTING ACCELERATIONS FOR SOLRD AND SUNGRA HAVE
!!!!!  BEEN RIGOROUSLY TESTED AGAINST: (1) A SINGLE SATELLITE IN SUN
!!!!!  ORBITING MODE AND (2) ALTERNATIVE CODE WITH EXPLICIT CALLS TO
!!!!!  PLANPO AND SUNGRV
      BDH1=BDTRUE(1,8)
      BDH2=BDTRUE(2,8)
      BDH3=BDTRUE(3,8)
      BDTRUE(1,8)=0.D0
      BDTRUE(2,8)=0.D0
      BDTRUE(3,8)=0.D0
      IF(.NOT.LSSRD) GO TO 100
      LVHOLD=LVAREA
      LVAREA=.FALSE.
      CALL SOLRD(MJDSEC,FSEC,XTEMP,XP(1,1),                             &
     &           DUM,DUM,DUM,                                           &
     &           AA(IAPLM),AA(IAPGM),LSORB,NEQN,SOLACC,VMAT,            &
     &           PXDDEX,AA(ISTSRD),LDUM,IDUM,                           &
     &           IDUM,LL(ILAJSP),AA,II,ISATID,ALAPGM,                   &
     &           RATIOP)
      LVAREA=LVHOLD
      DO I=1,3
        HDDTOT(I)=HDDTOT(I)+SOLACC(I)
        HDDCMP(I)=HDDCMP(I)+SOLACC(I)
        HDDIF(I)=HDDIF(I)+SOLACC(I)
      ENDDO
 100  CONTINUE
      IF(.NOT.LSGA) GO TO 200
          CALL RESON(AA(IGA+1),XTEMP(1),XTEMP(4),XACC,                  &
     &                PXDDEX,                                           &
     &                II(KITACX+IDIFQ),NEQN,AA(ISTGA),VMAT,IACCP)
      DO I=1,3
        HDDTOT(I)=HDDTOT(I)+XACC(I)
        HDDCMP(I)=HDDCMP(I)+XACC(I)
!        HDDIF(I)=HDDIF(I)+XACC(I)
      ENDDO
 200  CONTINUE
      BDTRUE(1,8)=BDH1
      BDTRUE(2,8)=BDH2
      BDTRUE(3,8)=BDH3

      CALL SUNGRA(1,XTEMP,LSORB,PGRAV,VMAT)
!      print*, "TBP SUNGRA",PGRAV(1),PGRAV(2),PGRAV(3)
!
!         TESTX=RMI(1)*PGRAV(1)+RMI(2)*PGRAV(2)+RMI(3)*PGRAV(3)
!         TESTY=RMI(4)*PGRAV(1)+RMI(5)*PGRAV(2)+RMI(6)*PGRAV(3)
!         TESTZ=RMI(7)*PGRAV(1)+RMI(8)*PGRAV(2)+RMI(9)*PGRAV(3)

!      WRITE(6,87648) TESTX,TESTY,TESTZ
!87648    FORMAT("SUNGRA",3D25.11)


!      !!IF(LASTSN)
      DO JJ=1,3
       PGRAV(JJ)=0.0D0
      END DO

!!!
!!!
!!!   CALCULATE RELATIVISTIC POINT MASS ACCELERATION
!!!   FOR BOTH ASTEROID AND SATELLITE
!!!
!!!   FOR THE SATELLITE, THE ACCELERATION IS THE DIFFERENCE
!!!   IN POINT MASS ACCELERATION OF THE SATELLITE AND THE
!!!   INTGCB BODY (ASTEROID)
!!!
!!!   FOR THE ASTEROID, THE ACCELERATION IS THE DIFFERENCE
!!!   IN POINT MASS ACCELERATION OF THE ASTEROID AND THE
!!!   SUN
!!!
!!!   POINT MASS ACCELERATION IS THE SUM OF ACCELERATION FROM
!!!   EACH PLANETARY BODY (INCLUDING BODIES IN EXTERNAL EPHERMERIS
!!!
!!!

!     Total number of bodies being considered
      IPPN_BODIES=12+ICBODY

!!!
!!!   FIRST COMPUTE ACC FOR THE SATELLITE, CALL REL_PM_ACC
!!!   FIRST FOR SATELLITE, THEN FOR ASTEROID
!!!   (SATELLITE ORBITING ASTEROID)
!!!
!!!   CONVERT XPS(SC POS) FROM AST CENTERED TO SSB CENTERED
!!!   ADD THE SPACECRAFT POSITION (ASTEROID CENTERED) TO THE AST
!!!   POSITION FOR SC POSITION
!!!

      DO JJ=1,3
        XPS_AST_REF(JJ,1)=XPS(JJ,1)+BD_ACCEL(JJ,12)
        XPS_AST_REF(JJ,2)=XPS(JJ,2)+BD_ACCEL(JJ+3,12)
      END DO

!     GO TO 99999
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!!!  THE FIRST PART SHOULD BE INCLUDED IN F IN ORDER TO HAVE THE NEW
!!!  RELATIVISTIC MODEL FOR ALL GEODYN RUNS
!!
!!   CALL REL_PM_ACC FOR PM ACCEL FOR SAT
!!
      CALL REL_PM_ACC(XPS_AST_REF,REL_PM_ACCEL_SUM,IPPN_BODIES,1,DBETA_1&
     &,DGAMMA_1,DGM_1)
!
      DO JJ=1,3
        XPTEMP(JJ,1)=BD_ACCEL(JJ,12)
        XPTEMP(JJ,2)=BD_ACCEL(JJ+3,12)
      END DO

!
!!!
!!!   CALL REL_PM_ACC FOR PM ACCEL FOR AST
!!!
      CALL REL_PM_ACC(XPTEMP,REL_PM_ACCEL_SUM2,IPPN_BODIES,2,DBETA_2    &
     &,DGAMMA_2,DGM_2)
!
!!!
!!!   CALCULATE DIFFERENCE IN SAT-AST ACC
!!!   XSAT_AST_DIFF IS THE ACCELERATION ON THE SPACECRAFT
!!!
      DO JJ=1,3
       XSAT_AST_DIFF(JJ)=REL_PM_ACCEL_SUM(JJ)-REL_PM_ACCEL_SUM2(JJ)
      END DO

!!!  END OF THE BLOCK TO BE MOVED IN F

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!99999 CONTINUE

!!
!!   NOW COMPUTE ACC FOR THE ASTEROID, CALL REL_PM_ACC
!!   FIRST FOR ASTEROID, THEN FOR THE SUN
!!   (ASTEROID ORBITING SUN)
!!
!!   CALL REL_PM_ACC FOR PM ACCEL FOR AST
!!
      CALL REL_PM_ACC(XPTEMP,REL_PM_ACCEL_SUMA,IPPN_BODIES,3,DBETA_3   &
     &,DGAMMA_3,DGM_3)
!!!!!!!!!!MODIFIED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      CALL REL_PM_ACC(XP,REL_PM_ACCEL_SUMA,IPPN_BODIES,3)
!
      DO JJ=1,3
       XPSUN(JJ,1)=BD_ACCEL(JJ,8)
       XPSUN(JJ,2)=BD_ACCEL(JJ+3,8)
      END DO
!
!!
!!!   CALL REL_PM_ACC FOR PM ACCEL FOR SUN
!!!
      CALL REL_PM_ACC(XPSUN,REL_PM_ACCEL_SUMA2,IPPN_BODIES,4,DBETA_4   &
     &,DGAMMA_4,DGM_4)

!!!!!!Partial Derivatives for the GM of the Sun !!!!!!!!!!!!!!!!!!!!
!!!!!!Partial Derivatives for Gamma and Beta !!!!!!!!!!!!!!!!!!!!!!!
!!Asteroid/Planet!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO JJ=1,3

!DEP the new order of parameters is Gamma,Beta.J2.GM SUN
!       PXDDEX(NEQN-3,JJ)=DGM_2(JJ)-DGM_4(JJ)                           ! GM Par

        PXDDEX(NEQN,JJ)=DGM_2(JJ)-DGM_4(JJ)

!       PXDDEX(NEQN-1,JJ)=DBETA_2(JJ)-DBETA_4(JJ)                       ! PPN-Pa
        PXDDEX(NEQN-2,JJ)=DBETA_2(JJ)-DBETA_4(JJ)

!       PXDDEX(NEQN,JJ)=DGAMMA_2(JJ)-DGAMMA_4(JJ)                       ! PPN-Pa
        PXDDEX(NEQN-3,JJ)=DGAMMA_2(JJ)-DGAMMA_4(JJ)

      END DO

!!Spacecraft!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO JJ=1,3
!       PXDDEXSC(NEQNS-3,JJ)=DGM_1(JJ)-DGM_2(JJ)                        ! GM Par
        PXDDEXSC(NEQNS,JJ)=DGM_1(JJ)-DGM_2(JJ)

!       PXDDEXSC(NEQNS-1,JJ)=DBETA_1(JJ)-DBETA_2(JJ)                    ! PPN-Pa
        PXDDEXSC(NEQNS-2,JJ)=DBETA_1(JJ)-DBETA_2(JJ)

!       PXDDEXSC(NEQNS,JJ)=DGAMMA_1(JJ)-DGAMMA_2(JJ)                    ! PPN-Pa
        PXDDEXSC(NEQNS-3,JJ)=DGAMMA_1(JJ)-DGAMMA_2(JJ)

      END DO

!
!!!
!!!   CALCULATE DIFFERENCE IN AST-SUN ACC
!!!   XAST_SUN_DIFF IS THE ACCELERATION ON THE ASTEROID
!!!
      DO JJ=1,3
      XAST_SUN_DIFF(JJ)=REL_PM_ACCEL_SUMA(JJ)-REL_PM_ACCEL_SUMA2(JJ)
      END DO
!
!!!
!!!   CONVERT ACCELERATIONS TO TOD

      XAST_DIFF_TOD(1)=RMI(1)*XAST_SUN_DIFF(1)+RMI(4)*XAST_SUN_DIFF(2)+&
     &         RMI(7)*XAST_SUN_DIFF(3)
      XAST_DIFF_TOD(2)=RMI(2)*XAST_SUN_DIFF(1)+RMI(5)*XAST_SUN_DIFF(2)+&
     &         RMI(8)*XAST_SUN_DIFF(3)
      XAST_DIFF_TOD(3)=RMI(3)*XAST_SUN_DIFF(1)+RMI(6)*XAST_SUN_DIFF(2)+&
     &         RMI(9)*XAST_SUN_DIFF(3)
!
      XSAT_DIFF_TOD(1)=RMI(1)*XSAT_AST_DIFF(1)+RMI(4)*XSAT_AST_DIFF(2)+&
     &         RMI(7)*XSAT_AST_DIFF(3)
      XSAT_DIFF_TOD(2)=RMI(2)*XSAT_AST_DIFF(1)+RMI(5)*XSAT_AST_DIFF(2)+&
     &         RMI(8)*XSAT_AST_DIFF(3)
      XSAT_DIFF_TOD(3)=RMI(3)*XSAT_AST_DIFF(1)+RMI(6)*XSAT_AST_DIFF(2)+&
     &         RMI(9)*XSAT_AST_DIFF(3)

!!! PGRAV HAS BE COMMENTED OUT SINCE THE THIRD BODY PERT. IS COMPUTED
!!! BY REL_PM_ACC

      DO JJ=1,3
       PGRAV(JJ)=XAST_DIFF_TOD(JJ)!+PGRAV(JJ)
      END DO
      DO I=1,3
        HDDTOT(I)=HDDTOT(I)+PGRAV(I)
        HDDCMP(I)=HDDCMP(I)+PGRAV(I)
      ENDDO
!!!!! RESET SUN TO ORIGINAL VALUE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!   TRANSFORM ACCELS INTO TRUE OF REF
!
         XDDTOT(1)=XDDTOT(1)+RMI(1)*HDDTOT(1)+RMI(2)*HDDTOT(2)          &
     &                      +RMI(3)*HDDTOT(3)
         XDDTOT(2)=XDDTOT(2)+RMI(4)*HDDTOT(1)+RMI(5)*HDDTOT(2)          &
     &                      +RMI(6)*HDDTOT(3)
         XDDTOT(3)=XDDTOT(3)+RMI(7)*HDDTOT(1)+RMI(8)*HDDTOT(2)          &
     &                      +RMI(9)*HDDTOT(3)
!
         XDDCMP(1)=XDDCMP(1)+RMI(1)*HDDCMP(1)+RMI(2)*HDDCMP(2)          &
     &                      +RMI(3)*HDDCMP(3)
         XDDCMP(2)=XDDCMP(2)+RMI(4)*HDDCMP(1)+RMI(5)*HDDCMP(2)          &
     &                      +RMI(6)*HDDCMP(3)
         XDDCMP(3)=XDDCMP(3)+RMI(7)*HDDCMP(1)+RMI(8)*HDDCMP(2)          &
     &                      +RMI(9)*HDDCMP(3)
!
         XDDIF(1)=XDDIF(1)+RMI(1)*HDDIF(1)+RMI(2)*HDDIF(2)              &
     &                    +RMI(3)*HDDIF(3)
         XDDIF(2)=XDDIF(2)+RMI(4)*HDDIF(1)+RMI(5)*HDDIF(2)              &
     &                    +RMI(6)*HDDIF(3)
         XDDIF(3)=XDDIF(3)+RMI(7)*HDDIF(1)+RMI(8)*HDDIF(2)              &
     &                    +RMI(9)*HDDIF(3)
!
!
      goto 88888
      IF(.NOT.LNOSAT) THEN
         XDDSAT(1)=XDDSAT(1)+XSAT_AST_DIFF(1)
         XDDSAT(2)=XDDSAT(2)+XSAT_AST_DIFF(2)
         XDDSAT(3)=XDDSAT(3)+XSAT_AST_DIFF(3)
         XDDTMC(1)=XDDTMC(1)+XSAT_AST_DIFF(1)
         XDDTMC(2)=XDDTMC(2)+XSAT_AST_DIFF(2)
         XDDTMC(3)=XDDTMC(3)+XSAT_AST_DIFF(3)
!DEP
      DO JJ=1,3
       XSAT_AST_DIFF(JJ)=0.D0
      END DO

      END IF
88888 continue


!!        WRITE(6,87654) XSAT_AST_DIFF(1:3)
!!87654    FORMAT(' TOR SUNGRAV FROM FAST: ',3D25.11)

!
!
!
!   TRANSFORM VMATRX INTO TRUE OF REF
!
      IF(LSORB) GO TO 500
      HOLD(1,1)=VMAT(1,1)*RMI(1)+VMAT(1,2)*RMI(2)+VMAT(1,3)*RMI(3)
      HOLD(2,1)=VMAT(2,1)*RMI(1)+VMAT(2,2)*RMI(2)+VMAT(2,3)*RMI(3)
      HOLD(3,1)=VMAT(3,1)*RMI(1)+VMAT(3,2)*RMI(2)+VMAT(3,3)*RMI(3)
      HOLD(1,2)=VMAT(1,1)*RMI(4)+VMAT(1,2)*RMI(5)+VMAT(1,3)*RMI(6)
      HOLD(2,2)=VMAT(2,1)*RMI(4)+VMAT(2,2)*RMI(5)+VMAT(2,3)*RMI(6)
      HOLD(3,2)=VMAT(3,1)*RMI(4)+VMAT(3,2)*RMI(5)+VMAT(3,3)*RMI(6)
      HOLD(1,3)=VMAT(1,1)*RMI(7)+VMAT(1,2)*RMI(8)+VMAT(1,3)*RMI(9)
      HOLD(2,3)=VMAT(2,1)*RMI(7)+VMAT(2,2)*RMI(8)+VMAT(2,3)*RMI(9)
      HOLD(3,3)=VMAT(3,1)*RMI(7)+VMAT(3,2)*RMI(8)+VMAT(3,3)*RMI(9)
      VMAT(1,1)=RMI(1)*HOLD(1,1)+RMI(2)*HOLD(2,1)+RMI(3)*HOLD(3,1)
      VMAT(2,1)=RMI(4)*HOLD(1,1)+RMI(5)*HOLD(2,1)+RMI(6)*HOLD(3,1)
      VMAT(3,1)=RMI(7)*HOLD(1,1)+RMI(8)*HOLD(2,1)+RMI(9)*HOLD(3,1)
      VMAT(1,2)=RMI(1)*HOLD(1,2)+RMI(2)*HOLD(2,2)+RMI(3)*HOLD(3,2)
      VMAT(2,2)=RMI(4)*HOLD(1,2)+RMI(5)*HOLD(2,2)+RMI(6)*HOLD(3,2)
      VMAT(3,2)=RMI(7)*HOLD(1,2)+RMI(8)*HOLD(2,2)+RMI(9)*HOLD(3,2)
      VMAT(1,3)=RMI(1)*HOLD(1,3)+RMI(2)*HOLD(2,3)+RMI(3)*HOLD(3,3)
      VMAT(2,3)=RMI(4)*HOLD(1,3)+RMI(5)*HOLD(2,3)+RMI(6)*HOLD(3,3)
      VMAT(3,3)=RMI(7)*HOLD(1,3)+RMI(8)*HOLD(2,3)+RMI(9)*HOLD(3,3)
!
      HOLD(1,1)=VMAT(1,4)*RMI(1)+VMAT(1,5)*RMI(2)+VMAT(1,6)*RMI(3)
      HOLD(2,1)=VMAT(2,4)*RMI(1)+VMAT(2,5)*RMI(2)+VMAT(2,6)*RMI(3)
      HOLD(3,1)=VMAT(3,4)*RMI(1)+VMAT(3,5)*RMI(2)+VMAT(3,6)*RMI(3)
      HOLD(1,2)=VMAT(1,4)*RMI(4)+VMAT(1,5)*RMI(5)+VMAT(1,6)*RMI(6)
      HOLD(2,2)=VMAT(2,4)*RMI(4)+VMAT(2,5)*RMI(5)+VMAT(2,6)*RMI(6)
      HOLD(3,2)=VMAT(3,4)*RMI(4)+VMAT(3,5)*RMI(5)+VMAT(3,6)*RMI(6)
      HOLD(1,3)=VMAT(1,4)*RMI(7)+VMAT(1,5)*RMI(8)+VMAT(1,6)*RMI(9)
      HOLD(2,3)=VMAT(2,4)*RMI(7)+VMAT(2,5)*RMI(8)+VMAT(2,6)*RMI(9)
      HOLD(3,3)=VMAT(3,4)*RMI(7)+VMAT(3,5)*RMI(8)+VMAT(3,6)*RMI(9)
      VMAT(1,4)=RMI(1)*HOLD(1,1)+RMI(2)*HOLD(2,1)+RMI(3)*HOLD(3,1)
      VMAT(2,4)=RMI(4)*HOLD(1,1)+RMI(5)*HOLD(2,1)+RMI(6)*HOLD(3,1)
      VMAT(3,4)=RMI(7)*HOLD(1,1)+RMI(8)*HOLD(2,1)+RMI(9)*HOLD(3,1)
      VMAT(1,5)=RMI(1)*HOLD(1,2)+RMI(2)*HOLD(2,2)+RMI(3)*HOLD(3,2)
      VMAT(2,5)=RMI(4)*HOLD(1,2)+RMI(5)*HOLD(2,2)+RMI(6)*HOLD(3,2)
      VMAT(3,5)=RMI(7)*HOLD(1,2)+RMI(8)*HOLD(2,2)+RMI(9)*HOLD(3,2)
      VMAT(1,6)=RMI(1)*HOLD(1,3)+RMI(2)*HOLD(2,3)+RMI(3)*HOLD(3,3)
      VMAT(2,6)=RMI(4)*HOLD(1,3)+RMI(5)*HOLD(2,3)+RMI(6)*HOLD(3,3)
      VMAT(3,6)=RMI(7)*HOLD(1,3)+RMI(8)*HOLD(2,3)+RMI(9)*HOLD(3,3)
  500 CONTINUE
!
! ADD TRUE OF REF SOLAR GRAVITY TERM INTO VMAT

      DO 520 I=1,3
      VMAT(I,I)=VMAT(I,I)-GMS/R3
      DO 510 J=1,3
      VMAT(I,J)=VMAT(I,J)+3.D0*GMS*XP(I,1)*XP(J,1)/R5
  510 CONTINUE
  520 CONTINUE
!
!!! RETURN POINTERS TO ORIGINAL VALUES

      ICBDGM=ICBDH
      IF(.NOT.LNOSAT) CALL SWTCPT(2,.FALSE.)
  910 CONTINUE
      RETURN
 6000 FORMAT(' EXECUTION TERMINATING IN FAST ; NMAX, MMAX ',2I10)
      END
