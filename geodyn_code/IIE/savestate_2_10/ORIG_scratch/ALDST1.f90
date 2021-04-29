!$ALDST1
      SUBROUTINE ALDST1(AA,II,LL,FSECK,FSECKA,XSK,VSK,ISAT,ISET,OFF,    &
     &                  XTRA,WORK,BAXIS,THETAG,OCDRYT,                  &
     &                  OCWETT,OBSMET,FSECM,XBTOR,XTTOR,DT2B,           &
     &                  TTKTOR,ITERP,ATROT,JOR,LEDIT)
!********1*********2*********3*********4*********5*********6*********7**
! ALDST1           99/11/21            9910.0    PGMR - S.B. LUTHCKE
!
! FUNCTION: COMPUTE FIRST GUESS BOUNCE POINT AND TRANSMIT RANGE
!           FROM INTERSECTION WITH ELLIPSOID
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC REAL ARRAY
!   II      I/O   A    DYNAMIC INTEGER ARRAY
!   LL      I/O   A    DYNAMIC LOGICAL ARRAY
!   FSECK    I    A    ELAPSED SECONDS FROM MJDSBL OF THE TIMES
!                      ASSOCIATED WITH SATELLITE TRANSMIT TIMES.
!   FSECKA    I   A    ELAPSED SECONDS FROM MJDSBL OF THE TIMES
!                      ASSOCIATED WITH SATELLITE TRANSMIT TIMES
!                      FOR ATTITUDE EVALUATION (IN CASE OF ATTITUDE
!                      TIMING BIAS)
!   XSK      I    A    TRUE OF REF. INERT. COORD. OF SATELLITE AT TRANSM
!   VSK      I    A    TRUE OF REF. INERT. VELOCITY OF SATELLITE AT TRAN
!   ISAT     I    S    SAT. NO.
!   ISET     I    S    SET. NO.
!   OFF      O    A    TRANSMIT INST. POINTING UNIT VECTOR
!   XTRA    I/O   A    SCRATCH SPACE - ONLY USE STARTING AT (*,4)
!   WORK    I/O   A    SCRATCH SPACE - CONTAINS CG CORR. ON OUTPUT
!   BAXIS    I    S    SEMIMINOR AXIS OF EARTH
!   THETAG  I/O   A    SIMPLY INITIALIZED TO ZERO
!   OCDRYT   O    A    DRY TROP. 1-WAY RANGE CORRECTION
!   OCWETT   O    A    WET TROP. 1-WAY RANGE CORRECTION
!   OBSMET   O    A
!   FSECM    O    A    BOUNCE POINT TIME
!   XBTOR    O    A    APPROXIMATE BOUNCE POINT TOR COORDS (ELLIP. INTER
!   XTTOR    O    A    TRANSMIT S/C TRACKING POINT TOR COORDS
!   DT2B     O    A    DISTANCE - TRANSMIT TO BOUNCE
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
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
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
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
      COMMON/EXATFL/LEXATO
      COMMON/EXATGL/LEXATT,NXEAGL
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/MREFDT/DMESRF,RMESRF,XMREF
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
!
      DIMENSION  AA(1),II(1),LL(1)
      DIMENSION FSECK(NM),FSECM(NM),XSK(MINTIM,3),VSK(MINTIM,3)
      DIMENSION FSECKA(NM)
      DIMENSION OFF(MINTIM,3),XTRA(MINTIM,33)
      DIMENSION WORK(NM),THETAG(NM)
      DIMENSION OCDRYT(NM),OCWETT(NM),OBSMET(NM)
      DIMENSION XBTOR(MINTIM,3),XTTOR(MINTIM,3),DT2B(MINTIM)
      DIMENSION TTKTOR(MINTIM,3)
      DIMENSION ATROT(3,3,MINTIM)
      DIMENSION LEDIT(NM)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! CALL ALTPT TO GET TOTAL ROTATION SBF TO TOR AT TRANSMIT
! ALSO GET OFFSET AT TRANSMIT
! TTKTOR(*,1-3) CONTAINS TOR OPTICAL CENTER OFFSET
! ...ITERP PASSED TO ALTPT AS PARTIAL EVALUATION INDICATOR
!
!      print *,'aldst1: call to altpt 1 ITERP: ',ITERP
      IF(MTYPE.NE.33) THEN
      CALL ALTPT(AA,II,LL,NM,MJDSBL,FSECKA,ISAT,ISET,OFF,TTKTOR(1,1),   &
     &           XTRA(1,1),XSK,VSK,ITERP,ATROT,XTRA(1,12),2,FRQLNK)
!      omag=dsqrt(off(1,1)**2+off(1,2)**2+off(1,3)**2)
!      print *,'aldst1: omag: ',omag
!
! SAVE THE CG CORRECTION INTO WORK
!
!  AND
!
!  WITH THE ICESat POINTING VECTOR BEING DETERMINED FROM A
!  COMBINATION OF A VECTOR FROM THE DATA RECORD AND QUATERNIONS FROM
!  THE EXAT01 FILE, THERE IS THE POSSIBILITY THAT OFF (THE POINTING
!  VECTOR) WILL BE UNDEFINED FOR A FEW EPOCHS. FOR THESE CASES
!  ALTPT WILL HAVE SET OFF TO (999,999,999). FOR THESE CASES
!  RESET OFF TO GEOCENTRIC POINTING AND XTRA(*,33) WILL BE
!  SET TO 999. THE GEOCENTRIC POINTING ALLOWS NORMAL PROCESING
!  TO CONTINUE. FOR THE POINTS WITH MISSING POINTING, DIRALT WILL
!  RESET RESID TO 9999999999 USING XTRA(*,33) AS A GUIDE.
      DO N=1,NM
       WORK(N)=XTRA(N,1)
       XTRA(N,33)=0.D0
       DOTP=OFF(N,1)*XSK(N,1)+OFF(N,2)*XSK(N,2)+OFF(N,3)*XSK(N,3)
       RQ=SQRT(XSK(N,1)*XSK(N,1)+XSK(N,2)*XSK(N,2)+XSK(N,3)*XSK(N,3))
       DOTP=DOTP/RQ
!!!!!!!!!!!!!!
        IF (DOTP.GT.0.0D0) THEN
          OFF(N,1)=-OFF(N,1)
          OFF(N,2)=-OFF(N,2)
          OFF(N,3)=-OFF(N,3)
        ENDIF
!!!!!!!!!!!!!!
       IF(OFF(N,1).GT.998.D0) THEN
          OF1=-XSK(N,1)
          OF2=-XSK(N,2)
          OF3=-XSK(N,3)
          OF=SQRT(OF1*OF1+OF2*OF2+OF3*OF3)
          XTRA(N,33)=999.D0
          OFF(N,1)=OF1/OF
          OFF(N,2)=OF2/OF
          OFF(N,3)=OF3/OF
       ENDIF
      ENDDO

      ENDIF

! If EEQ J2000 is the reference, the ellipsoidal
! computations will not work
! Rotate the quantities to TOD (better in all cases anyhow),
! then at end rotate back to TOR
      CALL SATUPD(MJDSBL,FSECKA,OFF,OFF,AA,NM,MINTIM,                   &
     &            .TRUE.,.FALSE.,II,JOR,1)
      CALL SATUPD(MJDSBL,FSECKA,XSK,XSK,AA,NM,MINTIM,                   &
     &            .TRUE.,.FALSE.,II,JOR,1)
      CALL SATUPD(MJDSBL,FSECKA,TTKTOR,TTKTOR,AA,NM,MINTIM,             &
     &            .TRUE.,.FALSE.,II,JOR,1)
!
!**** BEGIN LOOP 1000 TO COMPUTE THE DISTANCE BETWEEN S/C AND ELLIPSOID
!
!      print *,'aldist: at 1000 do iterp:',iterp
      DO 1000 N=1,NM
      ITC=0
 100  CONTINUE
      ITC=ITC+1
!
! OFF(1,3)(2,3)(3,3) are the direction cosines of the observation
! line  wrt the TOR inertial  Cartesian coordinate system.
!
      COSA=-OFF(N,1)
      COSB=-OFF(N,2)
      COSC=-OFF(N,3)
!
! Define quantities needed for the solution of the equation that
! will provide the point of intersection of a line with the earth's
! ellipsoid.
!
      COS2A=COSA**2.D0
      COS2B=COSB**2.D0
      COS2C=COSC**2.D0
!     Normalize by semiminor axis of Earth
      BP=AE/BAXIS
      BP2=BP**2.D0
      QL=(COS2A+COS2B)
      QM=BP2*COS2C
      QR=COSC*COSB
      QS=COSC*COSA
      QN=COS2C
!
  500 CONTINUE
!
!     Normalize x,y,z  coordinates of the satellite tracking point
!
      XTTOR(N,1)=(XSK(N,1)+TTKTOR(N,1))/BAXIS
      XTTOR(N,2)=(XSK(N,2)+TTKTOR(N,2))/BAXIS
      XTTOR(N,3)=(XSK(N,3)+TTKTOR(N,3))/BAXIS
      QP=XTTOR(N,1)**2.D0+XTTOR(N,2)**2.D0
      QV=XTTOR(N,1)*QS+XTTOR(N,2)*QR
      QU=QL+QM
!
!     Compute quantities needed for solving equation
!
      ZN=XTTOR(N,3)
      ZN2=ZN**2.D0
      QW=QN*QP+ZN2*QL-2.D0*ZN*QV-QM
      QR1=(QV-ZN*QL)**2.D0-(QU*QW)
      IF(QR1.LE.0.D0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  THIS OBSERVATION WILL BE EDITED
        LEDIT(N)=.TRUE.
!  GIVE IT A REASONABLE LINE OS SITE VECTOR
!  SO IT WILL MAKE IT THROUGH THE REST OF YHE COMPUTATIONS
        IF(ITC.EQ.1) THEN
          OF1=-XSK(N,1)
          OF2=-XSK(N,2)
          OF3=-XSK(N,3)
          OF=SQRT(OF1*OF1+OF2*OF2+OF3*OF3)
          XTRA(N,33)=999.D0
          OFF(N,1)=OF1/OF
          OFF(N,2)=OF2/OF
          OFF(N,3)=OF3/OF
          GO TO 100
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      write(6,*)' MISSED ONE'
      GOTO 1000
      ENDIF
      QR1=SQRT(QR1)
!
!     Compute solutions to 2nd order equations:
!
      ZPRIM1=(  (ZN*QL-QV)+QR1 )/QU
      ZPRIM2=(  (ZN*QL-QV)-QR1 )/QU
!
      DZ1=XTTOR(N,3)-ZPRIM1
      DZ2=XTTOR(N,3)-ZPRIM2
      IF(ABS(DZ1).LT.ABS(DZ2))THEN
       DZ=DZ1
       XBTOR(N,3)=ZPRIM1
      ELSE
       DZ=DZ2
       XBTOR(N,3)=ZPRIM2
      ENDIF
!
!     Compute normalized x and y prime
!
      XBTOR(N,1)=XTTOR(N,1)-DZ*COSA/COSC
      XBTOR(N,2)=XTTOR(N,2)-DZ*COSB/COSC
!
! XBTOR are the normalized cooordinates of P prime
! where the altimetric observation intersects the ellipsoid. We need to
! calculate the geocentric longitude and latitude of this point
!
!     Compute normalized distance between transmit tracking point and el
!
      DX=(XTTOR(N,1)-XBTOR(N,1))**2.D0
      DY=(XTTOR(N,2)-XBTOR(N,2))**2.D0
      DZ=DZ**2.D0
!
!     Compute unnormalized distance between satellite and ellipsoid
!
      DT2B(N)=SQRT(DX+DY+DZ)*BAXIS
!      write(6,*)' dbg 1 dt2b ',dt2b(N)

!
! GET UNORMALIZED SATELLITE AND INTERSECTION COORDINATES IN THE TOR SYST
!  PLUS CORRECT TOR S/C COORDINATES FOR CM OFFSET
!
! ...Intersection
      XBTOR(N,1)=XBTOR(N,1)*BAXIS
      XBTOR(N,2)=XBTOR(N,2)*BAXIS
      XBTOR(N,3)=XBTOR(N,3)*BAXIS

! ...S/C transmit tracking point
      XTTOR(N,1)=XTTOR(N,1)*BAXIS
      XTTOR(N,2)=XTTOR(N,2)*BAXIS
      XTTOR(N,3)=XTTOR(N,3)*BAXIS

 1000 END DO
!
!**** END LOOP 1000 TO COMPUTE THE DISTANCE BETWEEN S/C AND ELLIPSOID **
!  AT This point:
!                 hold(*,7-9) : intersection TOR coords
!                 hold(*,4-6) : S/C TOR coords for transmit tracking poi
!                 hold(*,10)  : distance between s/c and intersection
!
! **********************************************************************

!
! COMPUTE ATMOSPHERIC REFRACTION AT THE FIRST GUESS BOUNCE POINT
! ON THE FIRST ITERATION
! ... THETAG SHOULD BE ZERO HERE - LON NOT USED
!
      IF((.NOT.LPRE9(3).OR..NOT.LPRE9(4)).AND.ICBDGM.EQ.ITBDGM) THEN
!      print *,'aldst1: call to altrfc'

      CALL ALTRFC(XTTOR,XBTOR,OFF,DT2B,THETAG,XTRA(1,7),                &
     &       XTRA(1,15),OCDRYT,OCWETT,OBSMET,NM,MTYPE,.TRUE.,MJDSBL,    &
     &       AA(KFSEC),AA)

      ENDIF

! GET TOD QUANTITIES BACK IN TOR
      CALL SATUPD(MJDSBL,FSECKA,OFF,OFF,AA,NM,MINTIM,                   &
     &            .TRUE.,.TRUE.,II,JOR,1)
      CALL SATUPD(MJDSBL,FSECKA,XSK,XSK,AA,NM,MINTIM,                   &
     &            .TRUE.,.TRUE.,II,JOR,1)
      CALL SATUPD(MJDSBL,FSECKA,TTKTOR,TTKTOR,AA,NM,MINTIM,             &
     &            .TRUE.,.TRUE.,II,JOR,1)
      CALL SATUPD(MJDSBL,FSECKA,XTTOR,XTTOR,AA,NM,MINTIM,               &
     &            .TRUE.,.TRUE.,II,JOR,1)
      CALL SATUPD(MJDSBL,FSECKA,XBTOR,XBTOR,AA,NM,MINTIM,               &
     &            .TRUE.,.TRUE.,II,JOR,1)

!
! COMPUTE APPROXIMATE BOUNCE POINT TIME
!
!      write(6,*)' dbg 2 dt2b ',dt2b(1)
      DO N=1,NM
       FSECM(N) = FSECK(N) + (DT2B(N)+ABS(OCDRYT(N))+ABS(OCWETT(N)))  &
     &                       /VLIGHT
      END DO

      RETURN
      END
