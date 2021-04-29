!$ALDST2
      SUBROUTINE ALDST2(AA,II,LL,FSECM,XBTOR,XTTOR,DT2B,TTKTOR,         &
     &                  RTKTOR,DB2R,HOLD15,XTRA,THETAG,COSTHG,          &
     &                  SINTHG,OTIDES,ETIDES,GEOIDS,OFF,FSECN,FSECK,    &
     &                  OCDRYT,OCWETT,XSN,VSN,ISAT,ISET,ATROT,LFULL,    &
     &                  UZ,UZSQ,TEMP,SSTS,PMPA,LEDITB,INDJ,POSPRT,      &
     &                  LOADSS,RLAND,ISATID,COF_COM_TOR,JOR,LPW,SCALE)
!********1*********2*********3*********4*********5*********6*********7**
! ALDST2           99/11/21            9910.0    PGMR - S.B. LUTHCKE
!
! FUNCTION: COMPUTE CONVERGED BOUNCE POINT; COMPUTE TRANSMIT RANGE;
!           ITERATE AND COMPUTE RECEIVE RANGE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC REAL ARRAY
!   II      I/O   A    DYNAMIC INTEGER ARRAY
!   LL      I/O   A    DYNAMIC LOGICAL ARRAY
!   FSECM    I    A    ELAPSED SECONDS FROM MJDSBL OF THE TIMES
!                      ASSOCIATED WITH THE BOUNCE POINT
!   XBTOR   I/O   A    TRUE OF REF. INERT. COORD. OF BOUNCE POINT
!   XTTOR   I/O   A    TRUE OF REF. INERT. COORD. OF TRANSMIT POINT
!   DT2B    I/O   A    TRANSMIT to BOUNCE RANGE
!   TTKTOR  I/O   A    TRANSMIT TRACKING POINT IN TOR
!   RTKTOR  I/O   A    RECEIVE TRACKING POINT IN TOR
!   DB2R    I/O   A    BOUNCE to TRANSMIT RANGE
!   HOLD15  I/O   A    HOLD VARIOUS COMPUTED QUANTITIES
!   XTRA    I/O   A    SCRATCH SPACE - ONLY USE STARTING AT (*,4)
!   THETAG   O    A    RIGHT ASCENSION OF GREENWICH
!   COSTHG   O    A    COS OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   O    A    SIN OF RIGHT ASCENSION OF GREENWICH
!   OTIDES   O    A    OCEAN TIDES INCLUDING LOAD TIDE
!   ETIDES   O    A    BODY and POLE TIDES
!   GEOIDS   O    A    MEAN SURFACE
!   OFF      I    A    TRUE OF REF. POINTING VECTOR
!   FSECN    I    A    RECEIVE TIME
!   FSECK    I    A    TRANSMIT TIME
!   OCDRYT   I    A    DRY TROP. CORRECTION
!   OCWETT   I    A    WET TROP. CORRECTION
!   XSN      O    A    TRUE OF REF. SAT. POS. at RECEIVE
!   VSN      O    A    TRUE OF REF. SAT. VEL. at RECEIVE
!   ISAT     I    S    SAT. NO.
!   ISET     I    S    SET. NO.
!   RLAND    A      I  LAND OCEAN FLAG (0=>DON'T KNOW ; 1=>OCEAN ;
!                      2=>LAND)
!   ISATID   I    S    SATELLITE ID
!   COF_COM_TOR  I  A  CENTER OF MASS TO CENTER OF FIGURE VECTOR IN TOR
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**

      USE MSSGFC_MOD

      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
      PARAMETER (TOL = 0.001D0)
!     PARAMETER (MAXIT=100)
      COMMON/ALTCB /NALTW,MAXIT,NPERT,NXALT
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBIASA/BIASE (4),BIASM (2),BSCALE(1)  ,TBIAS (1),          &
     &              BSTROP(3),BSIONO(3),CLKSTA(4,2),CLKSAT(4,2),        &
     &              CLKSTS(2,2),TROPZE(2,2),TROPGR(2,4),BSLBIA(1)
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLSSOT/LSSTMO,LOTMOD,LPRDES,NXLSST
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
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
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
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
      COMMON/CORI02/KNTIME,KMJSTB,KMJSTC,KMJSTE,KISECT,                 &
     &              KINDH ,KNMH  ,KIVSAT,KIPXPF,KNTRTM,KMSTRC,          &
     &              KMSTRE,KISCTR,KISATR,KITRUN,KTRTMB,KTRTMC,          &
     &              KMJDVS,KNTMVM,NXCI02
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
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CORL02/KLNDTT,KLWSTR,KLNDTR,KLEVMF,KLTPMS,NXCL02
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CPARTL/LPART ,LPARTI,NXPARL
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CRM5OE/RM5OE(9)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/EDTALT/LEDRQ(10000)
      COMMON/EXATFL/LEXATO
      COMMON/EXATGL/LEXATT,NXEAGL
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/MREFDT/DMESRF,RMESRF,XMREF
      COMMON/NEQINT/NINTOT,MEMTOT,NEQNIM,NXNEQS
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
      COMMON/QQTIDE/IQMAX,IQMIN,IQDIF,NUMFRQ,NBAND
      COMMON/SSTCOM/NCBAR, NSBAR ,NCDOT ,NSDOT ,NCAPRD,NCBPRD,NSAPRD,   &
     &              NSBPRD,NCBARA,NSBARA,NCDOTA,NSDOTA,NCAPDA,NCBPDA,   &
     &              NSAPDA,NSBPDA,ISSTEP,NXSST
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      COMMON/XOVERR/XBTIME
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
!
      DIMENSION AA(1),II(1),LL(1)
      DIMENSION FSECM(NM),FSECN(NM),FSECK(NM)
      DIMENSION XBTOR(MINTIM,3),XTTOR(MINTIM,3)
      DIMENSION DT2B(MINTIM),DB2R(MINTIM)
      DIMENSION TTKTOR(MINTIM,3),RTKTOR(MINTIM,3)
      DIMENSION HOLD15(MINTIM,16)
      DIMENSION THETAG(NM),COSTHG(NM),SINTHG(NM)
      DIMENSION OTIDES(NM),ETIDES(NM),GEOIDS(NM)
      DIMENSION OFF(MINTIM,3),XTRA(MINTIM,33)
      DIMENSION XSN(MINTIM,3),VSN(MINTIM,3)
      DIMENSION OCDRYT(NM),OCWETT(NM)
      DIMENSION ATROT(3,3,MINTIM)
      DIMENSION UZ(NM),UZSQ(NM),TEMP(NM)
      DIMENSION SSTS(NM),PMPA(NDIM1,NDIM2)
      DIMENSION RLAND(NM)
      DIMENSION COF_COM_TOR(NM,3)

      DATA ONE/1.D0/

      dimension tstlat(5),tstlon(5)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      LEDITB=.FALSE.
      MAXIT=11
!
!      print *,'aldst2: degrad: ',degrad
      ITER=0

   10 CONTINUE
      ITER=ITER+1
!      print *,'aldst2: iter: ',iter

      IF(ITER.GT.MAXIT) THEN
       IF(LPW) THEN
       WRITE(6,*) ''
       WRITE(6,*) 'PROBLEM IN ALDST2 FOR THIS BLOCK OF OBSERVATIONS'
       WRITE(6,*) 'TOO MANY BOUNCE ITERATIONS: ',ITER
       WRITE(6,*) ''
!      STOP 69
       ENDIF
      LEDITB=.TRUE.
      ENDIF
      IF(ITER.GT.MAXIT) GO TO 60
!
! GET MAGNITUDE AND SIGN OF THE DIFFERENCE OF THE
! ECF BOUNCE POINT VECTOR and TIME VARYING SURFACE VECTOR
!
! NOTE: SOME OF THE VALUES BELOW ARE COMPUTED ONLY ON A SPECIFIC
!       BOUNCE ITERATION AND THEN EXPECTED TO REMAIN FOR SUBSEQUENT
!       BOUNCE ITERATIONS - ITER
!
! HOLD15(*,1)     :  XPOLE
! HOLD15(*,2)     :  YPOLE
! HOLD15(*,3)     :  GEODETIC LATITUDE OF SURFACE (RAD.)
! HOLD15(*,4)     :  EAST LONGITUDE OF SURFACE (RAD.)
! HOLD15(*,5)     :  ELLIPSOID HT. OF TIME VARYING SURFACE
! HOLD15(*,6-8)   :  EARTH TIDE VECTOR CORRECTION
! HOLD15(*,9)     :  MAGNITUDE OF THE DIFFERENCE BETWEEN THE SURFACE
!                    ECF VECTOR AND THE BOUNCE GUESS ECF VECTOR. THE
!                    SIGN CONVENTION IS INCLUDED
! HOLD15(*,10)    :  USED TO HOLD ITERATIVE VALUE OF DB2R
!

      CALL ALDBSF(AA,II,LL,FSECM,XBTOR,XTRA,HOLD15(1,1),HOLD15(1,2),    &
     &     THETAG,COSTHG,SINTHG,HOLD15(1,3),HOLD15(1,4),HOLD15(1,5),    &
     &     OTIDES,GEOIDS,ETIDES,HOLD15(1,6),HOLD15(1,9),ITER,LFULL,     &
     &     UZ,UZSQ,TEMP,SSTS,PMPA,LOADSS,RLAND,JOR,LPW,SCALE)


!
! COMPUTE SURFACE ELLIP. HT. - CURRENT BOUNCE POINT ELLIP. HT.
!
      LCNVER=.TRUE.
      DO N=1,NM
       IF(INT(OTIDES(N)).NE.-999999) THEN
        IF(ABS(HOLD15(N,9)).GT.TOL) LCNVER=.FALSE.
!       print *,'aldst2: n,lcnver: ',n,lcnver
!       print *,'aldst2: hold15(n,9): ',hold15(n,9)
       ENDIF
      END DO

!
! COMPUTE NEW BOUNCE POINT AND ITERATE IF NOT CONVERGED
!

      IF(.NOT.LCNVER) THEN

!        print *,'aldst2: xbtor before:'
!        print *,xbtor(1,1),xbtor(1,2),xbtor(1,3)
!      xmag=dsqrt(xbtor(1,1)**2+xbtor(1,2)**2+xbtor(1,3)**2)
!      print *,'aldst2: xmag2 before: ',xmag
       DO N=1,NM
       XBTOR(N,1)=XBTOR(N,1)-HOLD15(N,9)*OFF(N,1)
       XBTOR(N,2)=XBTOR(N,2)-HOLD15(N,9)*OFF(N,2)
       XBTOR(N,3)=XBTOR(N,3)-HOLD15(N,9)*OFF(N,3)
       END DO
!        print *,'aldst2: xbtor after:'
!        print *,xbtor(1,1),xbtor(1,2),xbtor(1,3)
!      xmag=dsqrt(xbtor(1,1)**2+xbtor(1,2)**2+xbtor(1,3)**2)
!      print *,'aldst2: xmag2 after: ',xmag

       GOTO 10

      ENDIF
   60 CONTINUE


!
! CONVERGED AND NOW COMPUTE TRANSMIT LEG RANGE
!
!     write(6,*)' dbg FINAL SSTS(1) ',SSTS(1)
      DO N=1,NM
       DT2B(N)=SQRT((XTTOR(N,1)-XBTOR(N,1))**2 +                        &
     &              (XTTOR(N,2)-XBTOR(N,2))**2 +                        &
     &              (XTTOR(N,3)-XBTOR(N,3))**2 )
!...FIRST GUESS RECEIVE TIME
       FSECN(N)=FSECK(N)+2.0D0*(DT2B(N)+ABS(OCDRYT(N))+ABS(OCWETT(N)))&
     &                         /VLIGHT
       IF(ABS(FSECN(N)-FSECK(N)).GT.1.0D0) THEN
           LEDRQ(N)=.TRUE.
           ! jjm 20120531 M in line below is undefined -- probably should
           !original  FSECN(N)=FSECK(M)+0.001D0
           FSECN(N)=FSECK(N)+0.001D0  ! jjm 20120531
       ENDIF
       HOLD15(N,10)=0.0D0
      END DO
!      print *,'aldst2: dt2b, fseckn: ',dt2b(1),fseck(1),fsecn(1)


!
! ITERATE AND COMPUTE RECEIVE LEG RANGE
!

      ITER=0
   20 CONTINUE
      ITER=ITER+1
!      print *,'aldst2: receive iter: ',iter

      IF(ITER.GT.MAXIT) THEN
       WRITE(6,*) ''
       WRITE(6,*) 'PROBLEM IN ALDST2 FOR THIS BLOCK OF OBSERVATIONS'
       WRITE(6,*) 'TOO MANY RECEIVE ITERATIONS: ',ITER
       WRITE(6,*) ''
!      STOP 69
      LEDITB=.TRUE.
      ENDIF
       IF(ITER.GT.MAXIT) GOTO 70

!...GET TRUE OF REF POSITION OF SATELLITE AT ALTIM RECEIVE
!      print *,'aldst2: calling orbit'


!    GET INTERGRATION INFORMATIOM IF NECESSARY
      IF(NINTOT.GT.0) THEN
      CALL FNDNUM(ISATID,II(KSATIN),NINTOT,IRET)
      IF(IRET.GT.0) THEN
      KSUMXOS=MAPSAT(INTOSP,1)
      KSUMPOS=MAPSAT(INTOSP,2)
      KXDDTOS=MAPSAT(INTOSP,3)
      KPDDTOS=MAPSAT(INTOSP,4)
      KNSTEPS=II(KSTEPS-1+IRET)
      NEQNI=II(KNEQNI-1+IRET)
      ENDIF
      ENDIF
!    END GET INTERGRATION INFORMATIOM IF NECESSARY

      CALL ORBIT(MJDSBL,FSECN,AA(KS),NM,II(KINDH),II(KNMH),II(KMJDSC),  &
     &   AA(KFSEC),II(KMJDSV),AA(KFSECV),LL(KLSETS),II(KIPXPF),         &
     &   II(KIVSAT),1,II(KNMAX),II(KNTOLD),XSN,AA(KPXPFM),              &
     &   LNADJ,.FALSE.,II(KPLPTR),II(KPANEL),II(KNMOVE),                &
     &   AA(KTPMES+4*MINTIM),LL(KLTPMS+6*MINTIM),LL(KSETDN),AA(KTMOS0), &
     &   AA(KTMOS),AA(KTMOSP),AA(KSMXOS-1+KSUMXOS),AA(KSMXOS-1+KSUMPOS),&
     &   AA(KSMXOS-1+KXDDTOS),AA(KSMXOS-1+KPDDTOS),KNSTEPS,NEQNI,       &
     &   II(KSGMNT-1+INTOSP),AA(KSGTM1-1+INTOSP),AA(KSGTM2-1+INTOSP),   &
     &   AA,II,LL)

      ! MODIFY SPACECRAFT POSITION FROM CENTER OF MASS
      ! TO CENTER OF FIGURE ORIGIN
      XSN(1:NM,1:3) = XSN(1:NM,1:3) + COF_COM_TOR(1:NM,1:3)

!      xdftmp=sqrt((xsn(1,1)-xsk(1,1))**2 +
!     &            (xsn(1,2)-xsk(1,2))**2 +
!     &            (xsn(1,3)-xsk(1,3))**2 )
!      print *,'aldst2: 1 xsn: ',xsn(1,1),xsn(1,2),xsn(1,3)
!      print *,'aldst2: 1 xsk: ',xsk(1,1),xsk(1,2),xsk(1,3)
!      print *,'aldst2: 1 xdftmp: ',xdftmp

!...GET TOR RECEIVE OFFSET AT FIRST GUESS RECEIVE TIME ONLY
      IF(ITER.EQ.1) THEN
!      print *,'aldst2: call before altpt iter: ',iter
      CALL ALTPT(AA,II,LL,NM,MJDSBL,FSECN,ISAT,ISET,XTRA(1,1),          &
     &           RTKTOR(1,1),XTRA(1,4),XSN,VSN,0,ATROT,                 &
     &           XTRA(1,15),1,FRQLNK)
!      print *,'aldst2:rtktor: ',rtktor(1,1),rtktor(1,2),rtktor(1,3)
!      print *,'aldst2:ttktor: ',ttktor(1,1),ttktor(1,2),ttktor(1,3)
      ENDIF


!...COMPUTE RECEIVE LEG RANGE
      DO N=1,NM
       IF(INDJ.GT.0.AND.INDJ.LE.3) XSN(N,INDJ)=XSN(N,INDJ)+POSPRT
       DB2R(N)=SQRT( ( (XSN(N,1)+RTKTOR(N,1)) -XBTOR(N,1) )**2 +        &
     &               ( (XSN(N,2)+RTKTOR(N,2)) -XBTOR(N,2) )**2 +        &
     &               ( (XSN(N,3)+RTKTOR(N,3)) -XBTOR(N,3) )**2 )
!...COMPUTE NEW RECEIVE TIME
      FSECN(N)=FSECK(N)+(DT2B(N)+DB2R(N)+2.0D0*                         &
     &                  (ABS(OCDRYT(N))+ABS(OCWETT(N))))/VLIGHT
      IF(ABS(FSECN(N)-FSECK(N)).GT.1.0D0) THEN
           LEDRQ(N)=.TRUE.
           FSECN(N)=FSECK(N)+0.001D0
       ENDIF
      END DO
!      print *,'aldst2: db2r1 : ',db2r(1)
!      print *,'aldst2: dt2b1 : ',dt2b(1)
!      print *,'aldst2: db2rnm: ',db2r(nm)
!      print *,'aldst2: dt2bnm: ',dt2b(nm)

!      print *,'aldst2: fsecn fseck 1: ',fsecn(1),fseck(1)
!      print *,'aldst2: fsecn fseck nm: ',fsecn(nm),fseck(nm)

      LCNVER=.TRUE.
      DO N=1,NM
       RLDIFF=DB2R(N)-HOLD15(N,10)
        IF(RLDIFF.GT.TOL.AND..NOT.LEDRQ(N)) THEN
         LCNVER=.FALSE.
         EXIT
        ENDIF
      END DO
!       print *,'aldst2: lcnver: ',lcnver
!       print *,'aldst2: 1 rldiff: ',rldiff

      IF(.NOT.LCNVER) THEN
       DO N=1,NM
        HOLD15(N,10)=DB2R(N)
       END DO
       GOTO 20
      ENDIF
!      print *,'aldst2: tmpscr14,db2r 1: ',tmpscr(1,14),db2r(1)
!      print *,'aldst2: tmpscr14,db2r nm: ',tmpscr(nm,14),db2r(nm)

   70  CONTINUE




! testing code

!      dlat=-42.0d0
!      dlon=330.0d0
!      time=49100.0D0
!       CALL PERTH2(dlat,dlon,time,tide,lsdata,.false.)
!       CALL LPEQMT(time*86400.0d0,dlat,tidel)
!       TIDE = TIDE + TIDEL
!       write(6,*) ''
!       write(6,*) 'ALDST2: GOT99 TEST:'
!       write(6,*) 'TEST: time,dlat,dlon,tide,lsdata:'
!       write(6,*) time,dlat,dlon,tide,lsdata
!       write(6,*) ''
!
!
!      tstlat(1)=-0.026626
!      tstlat(2)=-0.075664
!      tstlat(3)=-0.124702
!      tstlat(4)=-0.664117
!      tstlat(5)=-1.007375
!
!      tstlon(1)=265.758918
!      tstlon(2)=265.776401
!      tstlon(3)=265.793883
!      tstlon(4)=265.986199
!      tstlon(5)=266.108606
!
!      DO I=1,5
!      CALL GINTRP(4,5.D0,MSSGRD,XLATS,XLONW,DPHI,DLAM,NNL,
!     &            NEW,NNL,NEW,TSTLAT(i),TSTLON(i),HTMSS)
!      print *,'tstlat,tstlon,htmss: ',tstlat(i),tstlon(i),htmss
!      END DO
!
!      stop 69

      RETURN
      END
