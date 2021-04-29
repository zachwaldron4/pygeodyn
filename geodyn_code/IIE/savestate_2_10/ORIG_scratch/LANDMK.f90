!$LANDMK
      SUBROUTINE LANDMK(FSECN, N_EQN, ITAR, OBSPIX, SIGPIX, INDSAT, &
              ISAT, ISET, OFF, XTRA, HOLD, ATROT, ATPER, NDIMX1, &
              NDIMX2, NDIMX3, PXPF, PMPA, IPTFMG, ILNFMG, NFMG, PIXEL, &
              EDTSIG, ISTATS_L, OBSTIM, NDIMA1, NDIMA2, NDIMA3, PXPF2, &
              NEQNB,AA, II, LL)
!*******************************************************************
!  ROUTINE NAME:  LANDMK     2013/08/12        PGMR - JOSEPH NICHOLAS
!
! FUNCTION: COMPUTE THE PIXEL COORDINATES OF A LANDMARK.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   -------------------------------------------
!   MJDSBL   I    S    MODIFIED JULIAN DAY SECONDS
!   FSECN    I    A    SECONDS SINCE MJDSBL FOR EACH MEASUREMENT
!   NM       I    S    NUMBER OF MEASUREMENTS
!   N_EQN    I    A    NUMBER OF FORCE MODEL PARAMETERS
!   ITAR     I    S    TARGET ID FOR LANDMARK
!   POSLMK   I    A    POSITION OF LANDMARK IN BODY-FIXED COORDS
!   OBSPIX   I    A    OBSERVATIONS (X AND Y PIXEL COORDINATES)
!   SIGPIX   I    A    OBSERVATION SIGMAS
!   INDSAT   I    A    SATELLITE ID
!   ISAT     I    S    INTERNAL GEODYN SATELLITE NUMBER
!   ISET     I    S    INTERNAL GEODYN SET NUMBER (=ISAT)
!   OFF      O    S    EMPTY ARRAY FOR ALTPTI
!   XTRA     O    S    SCRATCH ARRAY
!   HOLD     O    S    SCRATCH ARRAY
!   ATROT    ?    A
!   ATPER    ?    A
!   NDIMX1   I    S    FIRST DIMENSION OF PXPF
!   NDIMX2   I    S    SECOND DIMENSION OF PXPF
!   NDIMX3   I    S    THIRD DIMENSION OF PXPF
!   PXPF     O    A    ARRAY FOR PARTIALS
!   PMPA     O    A    ARRAY FOR PARTIALS
!   IPTFMG   I    A    WHERE FORCE MODEL GROUP STARTS IN PMPA
!   ILNFMG   I    A    LENGTH OF EACH FORCE MODEL GROUP
!   NFMG     I    A    NUMBER OF FORCE MODEL GROUPS
!   PIXEL    O    A    PIXEL COORDINATES
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    REAL DYNAMIC ARRAY
!   LL      I/O   A    REAL DYNAMIC ARRAY
!*******************************************************************
      IMPLICIT NONE

      INCLUDE 'COMMON_DECL.inc'
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/ATTCBD/BATPER,ATTPRT
      COMMON/AXIS/LINTAX
      COMMON/CAMCCD/CKXMAT(30,6),CAMCTR(30,2),XKXCCD
      COMMON/CAMSAT/NCAM,NSCID(30),NXSCID
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CKOUNT/KNTBLK,KSEGMN,KNTOBS,KZROBS
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CSTATS/MSTATS,MTYPES,NXCSTT
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
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
      COMMON/CORA05/KOBTYP,KDSCRP,KGMEAN,KGLRMS,KWGMEA,KWGRMS,KTYMEA,   &
     &       KTYRMS,KWTMTY,KWTYRM,KTMEAN,KTRMS ,KWMEAN,KWTRMS,KWTRND,   &
     &       KPRVRT,KEBSTT,KVLOPT,NXCA05
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
      COMMON/CORI05/KNOBGL,KNOBWG,KNOBTY,KNOBWY,KNOBST,KNOBWT,KNEBOB,   &
     &              KJSTAT,NXCI05
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
      COMMON/CORL05/KLGPRV,NXCL05
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
      COMMON/DPLNOR/DXGEO1,DXGEO2,DXFRC1,DXFRC2,XPLNOR
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
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
      COMMON/PORINF/PLOR(228,2),DO0DA0(12,6,2)
      COMMON/RIMAGE/VIMAGE(6),QUCAM(4),QURSYS,TLBOD2
      COMMON/TARGET/NLXYZ(200000),NPLTAR,NTARG,NATARG,NXTARG
      COMMON/TOPSCL/SCDIRA,SCLNDM,PSCDRA,PSCLND
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      COMMON/XOVER /KXKNT,KXBST,KXOBL,KXBUP,KXBPA,IAPLSC,IAPLL
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA

      ! SUBROUTINE ARGUMENTS
      DOUBLE PRECISION XSMR(3),XSMT(3),XSME(3)
      DOUBLE PRECISION RQ1(3,3),RQ2(3,3),RQ3(3,3),QUAT(4),QUATO(4)
      INTEGER :: N_EQN, NEQNB, ISAT, ISET, NDIMX1, NDIMX2, NDIMX3
      INTEGER :: NDIMA1, NDIMA2, NDIMA3
      INTEGER :: INDSAT(3), IPTFMG(MAXFMG,MSATA)
      INTEGER :: ILNFMG(MAXFMG,MSATA), NFMG(MSATA), ITAR
      INTEGER :: ISTATS_L
      INTEGER :: IAST,IXAST
      INTEGER :: II(*)
      LOGICAL :: LL(*)
      DOUBLE PRECISION :: TQ,OBQ1,OBQ2
      DOUBLE PRECISION :: FSECN(NM)
      DOUBLE PRECISION :: OBSPIX(NM,2), SIGPIX(NM,2)
      DOUBLE PRECISION :: PIXEL(NM,2), PXPF(NDIMX1,NDIMX2,NDIMX3)
      DOUBLE PRECISION :: PXPF2(NDIMA1,NDIMA2,NDIMA3)
      DOUBLE PRECISION :: PMPA(NDIM1,NDIM2)
      DOUBLE PRECISION :: OFF(MINTIM,3)
      DOUBLE PRECISION :: XTRA(MINTIM,33), HOLD(MINTIM,30)
      DOUBLE PRECISION :: ATROT(3,3,MINTIM), ATPER(1)
      DOUBLE PRECISION :: EDTSIG(NM)
      DOUBLE PRECISION :: AA(*)
      DOUBLE PRECISION :: ELEVSC(1)
      DOUBLE PRECISION :: TMPRESID(2), TMPRATIO(2), TMPSIGMA(2)
      DOUBLE PRECISION :: OBSTIM(NM)

      INTEGER :: I, J, K, N
      DOUBLE PRECISION :: XSM(MINTIM,21)
      DOUBLE PRECISION :: XSN(MINTIM,21)
      DOUBLE PRECISION :: FSEC_EMIT(NM), SATLMKLT(NM)
      DOUBLE PRECISION :: COSW(NM), SINW(NM)
      DOUBLE PRECISION :: FLMK_IAU(MINTIM,3), FLMK_TOR(MINTIM,3), PX(2)
      DOUBLE PRECISION :: SAT2LMK_BF(MINTIM,3), SAT2LMK_TOR(MINTIM,3)
      DOUBLE PRECISION :: SBFTOR(3,3), FRQLNK, TRKTOR(3), W_ANGLE
      DOUBLE PRECISION :: POSLMK(3)
      DOUBLE PRECISION :: FKMAT(6), DISTORT(4), FOCLEN, CTR_PX(2)
      DOUBLE PRECISION :: COF_COM_BF(3), COF_COM_TOR(NM,3)

      ! FOR PARTIALS
      INTEGER :: IPERT, IPERTPOS, IPERTPLAN, IPERTATT, IPERTLMK
      INTEGER :: IPERTCAM, IPERTCOF
      INTEGER :: IP, IPMIN, IPMAX, IRADECPERT
      INTEGER :: NDMXU1, NDMXU2, NDMXU3
      INTEGER :: JPMPAT
      DOUBLE PRECISION :: FPERTPOS, FPERTFOCLEN, FPERTDISTORT, &
     & FPERT_COFCOM
      DOUBLE PRECISION :: POSLMK_ORIG(3), XSM_ORIG(MINTIM,3)
      DOUBLE PRECISION :: FOCLEN_ORIG, DISTORT_ORIG(4)
      DOUBLE PRECISION :: COF_COM_BF_ORIG(3)
      DOUBLE PRECISION :: PARTL_POS(NM,3,2), PARTL_PLAN(3,NM,2)
      DOUBLE PRECISION :: PARTL_ATT(3,NM,2,2), PARTL_LMK(NM,3,2)
      DOUBLE PRECISION :: PARTL_CAM(NM,5,2), PARTL_COFCOM(NM,3,2)
      DOUBLE PRECISION :: FULLTIME(NM), RESID(NM), WEIGHT(NM)
      DOUBLE PRECISION :: DADA0(3,6), DADIC(3,6), XR(NDPOR)

      INTEGER :: ISATID, IRET1, ITERP
      INTEGER :: KSUMXOS, KSUMPOS, KXDDTOS, KPDDTOS, KNSTEPS
      INTEGER :: NEQNI
      INTEGER :: ITARPT, JTARID, JTARPT, KTARPT, NAJ, N1
      LOGICAL :: LTARJ, LEDIT(NM)
      DOUBLE PRECISION :: SIGMA(NM), RATIO(NM)

      INTEGER :: IDCAM, IYMD, IHM, INDSYS, IPTD, IRET2
      INTEGER :: NMWTD, NTYPE, JJ, JQ, KQ
      LOGICAL :: LSIM
      DOUBLE PRECISION :: BB, CAMID, DTAR, RESIDU, SATIMG, SEC, ZERO
      INTEGER :: IPSCL
      DOUBLE PRECISION :: TNORM(3),TARGR

      CHARACTER(LEN = 8) DTYPE(1)
      CHARACTER(LEN = 8) BLKNEW, BLKOLD
      CHARACTER(LEN = 1) TIMSYS(3)
      DATA TIMSYS/'R', 'X', 'T'/
      DATA DTYPE/'LANDMK   '/
      DATA ZERO/0.D0/
      DATA BLKNEW/'(*NEW*)'/, BLKOLD/'(CONT.)'/
      DATA LSIM/.FALSE./
!     DATA LSIM/.TRUE./

      !*****************************************************************
      !******************* START OF EXECUTABLE CODE ********************
      !*****************************************************************

      FPERTPOS = 10.0D0 ! METERS TO PERTURB POSITION
      FPERTFOCLEN = 1.0D0
      FPERTDISTORT = 1.0D-8
      FPERT_COFCOM = 1.0D0 ! METERS TO PERTURB COF-COM OFFSET

      IPERTPOS = 1
      IPERTPLAN = 2
      IPERTATT = 3
      IPERTLMK = 4
      IPERTCAM = 5
      IPERTCOF = 6 ! PERTURB CENTER-OF-FIGURE TO CENTER-OF-MASS VECTOR

      IAST=1
      IF(LBINAST.AND.TLBOD2.GT.1.5D0) IAST=2
      ! FIND INDICES FOR LANDMARK (D. ROWLANDS)
      IF (NTARG <= 0) THEN
          WRITE(6,'(A)') &
                  ' EXECUTION TERMINATING PROCESSING LANDMARK DATA'
          WRITE(6,'(A)') ' NO INPUT TARGETS SUPPLIED'
          STOP
      END IF

      DO N = 1, NTARG
          ITARPT = N
          JTARID = II(KTARID-1+N)
          IF (ITAR == JTARID) EXIT
      END DO

      IF (ITAR /= JTARID) THEN
          WRITE(6,'(A)') ' EXECUTION TERMINATING PROCESSING LANDMARK &
                  &DATA'
          WRITE(6,'("TARGET#",I0,"NOTAMONGTHE",I0,&
                  &"INPUTTARGETS")') ITAR, NTARG
          STOP
      END IF

      N1 = (ITARPT-1) * 3
      JTARPT = KPRMV + IPVAL(IXTARG) - 2 + N1
      KTARPT = KPTRUA + IPVAL(IXTARG) - 2 + N1
      NAJ = 0

      DO I = 1, 3
          IF (II(KTARPT+I) > 0) THEN
              NAJ = NAJ + 1
          END IF
          IF (I == 1) THEN
              JPMPAT = II(KTARPT + I)
          END IF
      END DO

      LTARJ = .FALSE.
      IF (LSTINR) THEN
          IF (NAJ == 3) THEN
              LTARJ = .TRUE.
          END IF
          IF (NAJ > 0 .AND. NAJ /= 3) THEN
              WRITE(6,'(2A)') ' EXECUTION TERMINATING ', &
                      'PROCESSING LANDMARK DATA'
              WRITE(6,'(" TARGETID ", I10, " HAS ", I2, &
                      & " COMPONENTS ADJUSTING")') JTARID, NAJ
              WRITE(6,'(2A)') ' THERE SHOULD BE EITHER 0 or 3 ', &
                      'COMPONENTS ADJUSTING'
              STOP
          END IF
          IF(LTARJ.AND.PSCLND.GT.0.001D0) THEN
             WRITE(6,6000)
             WRITE(6,6001)
             WRITE(6,6002)
             STOP
          ENDIF
      END IF

      ISATID = II(KISATN + INDSAT(1) - 1)
      IRET1 = 1
      IF (NINTOT > 0) THEN
          CALL FNDNUM(ISATID, II(KSATIN), NINTOT, IRET1)
          IF (IRET1 > 0) THEN
              KSUMXOS = MAPSAT(INTOSP, 1)
              KSUMPOS = MAPSAT(INTOSP, 2)
              KXDDTOS = MAPSAT(INTOSP, 3)
              KPDDTOS = MAPSAT(INTOSP, 4)
              KNSTEPS = II(KSTEPS-1 + IRET1)
              NEQNI = II(KNEQNI-1 + IRET1)
          END IF
      END IF

      ! GET LANDMARK COORDINATES FROM INPUT CARDS
      DO I = 1, 3
          POSLMK(I) = AA(KPRMV-1 + IPVAL(IXTARG) + N1 + I - 1)
          IF (NPVAL0(IXTARG) > 0) THEN
              POSLMK(I) = AA(KPRMVC-1 + IPVAL0(IXTARG) + N1 + I - 1)
          END IF
          POSLMK(I)=POSLMK(I)*SCLNDM
      END DO

      IF (NPVAL0(IXCAME) /= 0 .AND. MOD(NPVAL0(IXCAME),5) /= 0) THEN
          WRITE(6,'(2A)') ' EXECUTION TERMINATING ', &
                  'PROCESSING LANDMARK DATA'
          WRITE(6,'(2A)') ' THERE SHOULD BE EITHER 0 OR MULTIPLE OF 5', &
                  ' CAMERA PARAMETERS ADJUSTING.'
          STOP
      END IF

      ! GET CAMERA ID
      CAMID = VIMAGE(3)
      IDCAM = INT(CAMID)
      CALL FNDNUM(IDCAM, NSCID, 30, IRET2)

      ! GET CAMERA PARAMETERS FROM INPUT CARDS
      FOCLEN = AA(KPRMV + IPVAL(IXCAME)-1 + (IRET2-1)*5)
      IF (NPVAL0(IXCAME) >= 5) THEN
          FOCLEN = AA(KPRMVC + IPVAL0(IXCAME)-1 + (IRET2-1)*5)
      END IF

      DO I = 1, 4
          DISTORT(I) = AA(KPRMV + IPVAL(IXCAME)-1 + (IRET2-1)*5 + I)
          IF (NPVAL0(IXCAME) >= 5) THEN
              DISTORT(I) = AA(KPRMVC + IPVAL0(IXCAME)-1 &
                      + (IRET2-1)*5 + I)
          END IF
      END DO

      ! LOAD CCD INFORMATION (IF GIVEN ON CAMERA CARD)
      IF (IRET2 /= 0) THEN
          DO J = 1, 6
              FKMAT(J) = CKXMAT(IRET2, J)
          END DO
          DO J = 1, 2
              CTR_PX(J) = CAMCTR(IRET2, J)
          END DO
      END IF

      ! INITIALIZE BODY FIXED COM-COF OFFSET
      COF_COM_BF(1:3) = 0.0D0
      IF (NPVAL(IXCOFF) > 0) THEN
          DO I = 1, 3
              COF_COM_BF(I) = AA(KPRMV-1 + IPVAL(IXCOFF)-1 + I)
          END DO
      END IF
      CALL BF_TO_TOR(NM, COF_COM_BF, COF_COM_TOR, MJDSBL, FSECN, AA, II)

      ! OBTAIN SATELLITE POSITIONS AT CORRECT SATELLITE TIMES
      CALL ORBSET(II(KNSAT), II(KIVSAT), LL(KLSETS), INDSAT(1), &
              KNSTEPS, NEQNI, II(KSGMNT-1+IRET1), AA(KSGTM1-1+IRET1), &
              AA(KSGTM2-1+IRET1), IRET1, MJDSBL, FSECN, FSECN, 1, AA, &
              II)

      CALL ORBIT(MJDSBL, FSECN, AA(KS), 1, II(KINDH), II(KNMH), &
              II(KMJDSC), AA(KFSEC), II(KMJDSV), AA(KFSECV), &
              LL(KLSETS), II(KIPXPF), II(KIVSAT), 1, II(KNMAX), &
              II(KNTOLD), XSM, PXPF, .FALSE., .FALSE., II(KPLPTR), &
              II(KPANEL), II(KNMOVE), AA(KTPMES+4*MINTIM), &
              LL(KLTPMS+6*MINTIM), LL(KSETDN), AA(KTMOS0-1+INTOSP), &
              AA(KTMOS-1+INTOSP), AA(KTMOSP-1+INTOSP), &
              AA(KSMXOS-1+KSUMXOS), AA(KSMXOS-1+KSUMPOS), &
              AA(KSMXOS-1+KXDDTOS), AA(KSMXOS-1+KPDDTOS), KNSTEPS, &
              NEQNI, II(KSGMNT-1+IRET1), AA(KSGTM1-1+IRET1), &
              AA(KSGTM2-1+IRET1), AA, II, LL)
      IF(LBINAST.AND.IAST.EQ.2) THEN
! TARGET IS SECONDARY  BODY
              XSM(1,1)=XSM(1,1)-AA(KASTO)
              XSM(1,2)=XSM(1,2)-AA(KASTO+MINTIM)
              XSM(1,3)=XSM(1,3)-AA(KASTO+2*MINTIM)
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! OUTPUT FOR ALTSIM; GET ASTEROID FIXED COORDINATES OF SATELLIRE
              XSMR(1)=XSM(1,1)
              XSMR(2)=XSM(1,2)
              XSMR(3)=XSM(1,3)
              CALL SATUPD(MJDSBL, FSECN,XSMR, XSMT, AA,                 &
     &                1, 1, .TRUE., .FALSE., II, 0,IAST)
              IF (LINTAX) THEN
                  CALL GETWCS(1, AA(KDPOR), COSW, SINW,IAST)
              ELSE
                  CALL ROTXTR(MJDSBL, FSECN, .FALSE., AA(KEQN), &
                          AA(KSRTCH), AA(KTHETG), COSW, SINW, NM, AA, &
                          AA(KACOSW), AA(KBSINW), AA(KANGWT), &
                          AA(KWT), AA(KACOFW), AA(KBCOFW), II,IAST)
              END IF

              XSME(1)= COSW(1)*XSMT(1)+SINW(1)*XSMT(2)
              XSME(2)=-SINW(1)*XSMT(1)+COSW(1)*XSMT(2)
              XSME(3)=XSMT(3)
                  CALL ALTPTI(AA, II, LL, 1, MJDSBL, FSECN, ISAT, &
                          ISET, OFF, XTRA(I,4), XTRA(I,7), XSM(I,1), &
                          XSM(I,19), 0, ATROT(1,1,I), HOLD, 2, &
                          FRQLNK, 33, SBFTOR,IAST)
!
! RQ1 IS THE MATRIX FRO PF TO TOD
              RQ1(1,1)=COSW(1)
              RQ1(1,2)=-SINW(1)
              RQ1(1,3)=0.D0
              RQ1(2,1)=SINW(1)
              RQ1(2,2)=COSW(1)
              RQ1(2,3)=0.D0
              RQ1(3,1)=0.D0
              RQ1(3,2)=0.D0
              RQ1(3,3)=1.D0
! RQ2 IS THE MATRX FROM PF TO J2000
              RQ2(1,1)=AA(KROTMT       )*RQ1(1,1)                       &
     &                +AA(KROTMT+NDCRD2)*RQ1(2,1)                       &
     &                +AA(KROTMT+NDCRD3)*RQ1(3,1)
!
              RQ2(2,1)=AA(KROTMT+NDCRD4)*RQ1(1,1)                       &
     &                +AA(KROTMT+NDCRD5)*RQ1(2,1)                       &
     &                +AA(KROTMT+NDCRD6)*RQ1(3,1)
!
              RQ2(3,1)=AA(KROTMT+NDCRD7)*RQ1(1,1)                       &
     &                +AA(KROTMT+NDCRD8)*RQ1(2,1)                       &
     &                +AA(KROTMT+NDCRD9)*RQ1(3,1)
!
              RQ2(1,2)=AA(KROTMT       )*RQ1(1,2)                       &
     &                +AA(KROTMT+NDCRD2)*RQ1(2,2)                       &
     &                +AA(KROTMT+NDCRD3)*RQ1(3,2)
!
              RQ2(2,2)=AA(KROTMT+NDCRD4)*RQ1(1,2)                       &
     &                +AA(KROTMT+NDCRD5)*RQ1(2,2)                       &
     &                +AA(KROTMT+NDCRD6)*RQ1(3,2)
!
              RQ2(3,2)=AA(KROTMT+NDCRD7)*RQ1(1,2)                       &
     &                +AA(KROTMT+NDCRD8)*RQ1(2,2)                       &
     &                +AA(KROTMT+NDCRD9)*RQ1(3,2)
!
              RQ2(1,3)=AA(KROTMT       )*RQ1(1,3)                       &
     &                 +AA(KROTMT+NDCRD2)*RQ1(2,3)                      &
     &                 +AA(KROTMT+NDCRD3)*RQ1(3,3)
!
              RQ2(2,3)=AA(KROTMT+NDCRD4)*RQ1(1,3)                       &
     &                +AA(KROTMT+NDCRD5)*RQ1(2,3)                       &
     &                +AA(KROTMT+NDCRD6)*RQ1(3,3)
!
              RQ2(3,3)=AA(KROTMT+NDCRD7)*RQ1(1,3)                       &
     &                +AA(KROTMT+NDCRD8)*RQ1(2,3)                       &
     &                +AA(KROTMT+NDCRD9)*RQ1(3,3)
! RQ3 IS THE MATRIX FROM PF TO SBF
              RQ3(1,1)=SBFTOR(1,1)*RQ2(1,1)+SBFTOR(2,1)*RQ2(2,1)        &
     &                +SBFTOR(3,1)*RQ2(3,1)
!
              RQ3(2,1)=SBFTOR(1,2)*RQ2(1,1)+SBFTOR(2,2)*RQ2(2,1)        &
     &                +SBFTOR(3,2)*RQ2(3,1)
!
              RQ3(3,1)=SBFTOR(1,3)*RQ2(1,1)+SBFTOR(2,3)*RQ2(2,1)        &
     &                +SBFTOR(3,3)*RQ2(3,1)
!
              RQ3(1,2)=SBFTOR(1,1)*RQ2(1,2)+SBFTOR(2,1)*RQ2(2,2)        &
     &                +SBFTOR(3,1)*RQ2(3,2)
!
              RQ3(2,2)=SBFTOR(1,2)*RQ2(1,2)+SBFTOR(2,2)*RQ2(2,2)        &
     &                +SBFTOR(3,2)*RQ2(3,2)
!
              RQ3(3,2)=SBFTOR(1,3)*RQ2(1,2)+SBFTOR(2,3)*RQ2(2,2)        &
     &                +SBFTOR(3,3)*RQ2(3,2)
!
              RQ3(1,3)=SBFTOR(1,1)*RQ2(1,3)+SBFTOR(2,1)*RQ2(2,3)        &
     &                +SBFTOR(3,1)*RQ2(3,3)
!
              RQ3(2,3)=SBFTOR(1,2)*RQ2(1,3)+SBFTOR(2,2)*RQ2(2,3)        &
     &                +SBFTOR(3,2)*RQ2(3,3)
!
              RQ3(3,3)=SBFTOR(1,3)*RQ2(1,3)+SBFTOR(2,3)*RQ2(2,3)        &
     &                +SBFTOR(3,3)*RQ2(3,3)
!
              CALL ROTQAT(SBFTOR,QUATO)
              CALL ROTQAT(RQ3,QUAT)
!
              TQ=DBLE(MJDSBL)+FSECN(1)
! TIME. PF STATE, SBFTOR, PFSBF
              WRITE(39,77665) TQ,XSME,QUATO,QUAT
77665         FORMAT(F20.6,3F20.3,8F20.10)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! SAVE VARIABLES SO THEY CAN BE RESTORED AFTER PERTURBATION
      XSM_ORIG(1:NM, 1:3) = XSM(1:NM, 1:3)
      POSLMK_ORIG(1:3) = POSLMK(1:3)
      FOCLEN_ORIG = FOCLEN
      DISTORT_ORIG(1:4) = DISTORT(1:4)
      COF_COM_BF_ORIG(1:3) = COF_COM_BF(1:3)

      BB = SQRT(XSM(1,1)**2 + XSM(1,2)**2 + XSM(1,3)**2)
      write(6,*) '****** BEGIN LANDMK PROCESSING ******'
      write(6,*) ' RADIUS SAT-PLANET ', BB
      CC = SQRT(POSLMK(1)**2 + POSLMK(2)**2 + POSLMK(3)**2)
      write(6,*) ' RADIUS IMAGE-PLANET ', CC

      DO IPERT = 0, 6
          IF (IPERT == 0) THEN
              IPMIN = 0
              IPMAX = 0
          ELSE IF (IPERT == IPERTCAM) THEN
              IPMIN = 1
              IPMAX = 5
          ELSE IF (IPERT == IPERTATT) THEN
              IPMIN = 1
              IPMAX = 3
              IF(IAPLL.GT.0) IPMAX=6
          ELSE
              IPMIN = 1
              IPMAX = 3
          END IF

          DO IP = IPMIN, IPMAX
              ! PERTURB PARAMETERS
              IF (IPERT == IPERTPOS) THEN ! SATELLITE POSITION
                  XSM(1:NM, IP) = XSM(1:NM, IP) + FPERTPOS
              END IF
              IF (IPERT == IPERTLMK) THEN ! LANDMARK POSITION
                  POSLMK(IP) = POSLMK(IP) + FPERTPOS
              END IF
              IF (IPERT == IPERTCOF) THEN ! COF-COM OFFSET
                  COF_COM_BF(IP) = COF_COM_BF(IP) + FPERT_COFCOM
              END IF

              CALL BF_TO_TOR(NM, COF_COM_BF, COF_COM_TOR, &
                      MJDSBL, FSECN, AA, II)

              ! CORRECT SPACECRAFT POSITION FOR CENTER OF FIGURE OFFSET
              XSM(1:NM,1:3) = XSM(1:NM,1:3) + COF_COM_TOR(1:NM,1:3)

              DO I = 1, NM
                  ! APPROXIMATE LIGHT-TIME FROM LANDMARK TO SATELLITE
                  SATLMKLT(I) = ABS(SQRT(SUM(POSLMK(1:3)**2)) &
                          - SQRT(SUM(XSM(I,1:3)**2))) / VLIGHT
                  SATIMG = ABS(SQRT(SUM(POSLMK(1:3)**2)) &
                          - SQRT(SUM(XSM(I,1:3)**2)))
              END DO
              ! CALCULATE TIME LIGHT WAS EMITTED BY LANDMARK
              FSEC_EMIT(1:NM) = FSECN(1:NM) - SATLMKLT(1:NM)

              ! ROTATE LANDMARK POSITION FROM PLANET BODY FIXED TO IAU
              IF (LINTAX) THEN
                  CALL GETWCS(NM, AA(KDPOR), COSW, SINW,IAST)
              ELSE
                  CALL ROTXTR(MJDSBL, FSEC_EMIT, .FALSE., AA(KEQN), &
                          AA(KSRTCH), AA(KTHETG), COSW, SINW, NM, AA, &
                          AA(KACOSW), AA(KBSINW), AA(KANGWT), &
                          AA(KWT), AA(KACOFW), AA(KBCOFW), II,IAST)
              END IF

              ! PERTURB W ANGLE
              IF (IPERT == IPERTPLAN .AND. IP == 3) THEN
                  DO I = 1, NM
                      W_ANGLE = ATAN2(SINW(I), COSW(I)) + DXGEO1*DEGRAD
                      COSW(I) = COS(W_ANGLE)
                      SINW(I) = SIN(W_ANGLE)
                  END DO
              END IF

              FLMK_IAU(1:NM,1) = COSW(1:NM)*POSLMK(1) &
                      - SINW(1:NM)*POSLMK(2)
              FLMK_IAU(1:NM,2) = SINW(1:NM)*POSLMK(1) &
                      + COSW(1:NM)*POSLMK(2)
              FLMK_IAU(1:NM,3) = POSLMK(3)

              ! ROTATE LANDMARK POSITION FROM IAU TO TRUE OF REFERENCE
              IF (IPERT == IPERTPLAN .AND. IP < 3) THEN
                  ! FLAG TO PERTURB RA (IP=1) OR DEC (IP=2)
                  IRADECPERT = IP
              ELSE
                  IRADECPERT = 0
              END IF
              CALL SATUPD(MJDSBL, FSEC_EMIT, FLMK_IAU, FLMK_TOR, AA, &
                      NM, MINTIM, .TRUE., .TRUE., II, IRADECPERT,IAST)

              ! COMPUTE SATELLITE-TO-LANDMARK VECTOR IN TRUE OF REFERENCE
              DO I = 1, NM
                  SAT2LMK_TOR(I,1:3) = FLMK_TOR(I,1:3) - XSM(I,1:3)
              END DO

              ! ROTATE SATELLITE-TO-LANDMARK VECTOR INTO SAT BODY FIXED
              IF (IPERT == IPERTATT) THEN
                  ! PERTERB ROLL (IP=1), PITCH (IP=2), YAW (IP=3)
                  ITERP = 10 + IP
              ELSE
                  ITERP = 0
              END IF
              DO I = 1, NM
                  CALL ALTPTI(AA, II, LL, 1, MJDSBL, FSECN, ISAT, &
                          ISET, OFF, XTRA(I,4), XTRA(I,7), XSM(I,1), &
                          XSM(I,19), ITERP, ATROT(1,1,I), HOLD, 2, &
                          FRQLNK, 33, SBFTOR,IAST)
                  TRKTOR(1:3) = XTRA(1,4:6)
                  SAT2LMK_TOR(I,1:3) = SAT2LMK_TOR(I,1:3) - TRKTOR(1:3)
                  SAT2LMK_BF(I,1:3) = MATMUL(TRANSPOSE(SBFTOR), &
                          SAT2LMK_TOR(I,1:3))
                  SAT2LMK_BF(I,1:3) = SAT2LMK_BF(I,1:3) &
                          / SQRT(SUM(SAT2LMK_BF(I,1:3)**2))
              END DO

              ! PERTURB CAMERA PARAMETERS
              IF (IPERT == IPERTCAM) THEN
                  IF (IP == 1) THEN
                      FOCLEN = FOCLEN + FPERTFOCLEN
                  ELSE IF (IP >= 2 .AND. IP <= 5) THEN
                      DISTORT(IP-1) = DISTORT(IP-1) + FPERTDISTORT
                  END IF
              END IF

              ! GET PIXEL COORDINATES AND CALCULATE PARTIALS
              DO I = 1, NM
                  IF (IPERT == 0) THEN
                      write(6,*)' CAMERA PARAMETERS'
                      write(6,*)' FKMAT ', FKMAT
                      write(6,*)' DISTORT ', DISTORT
                      write(6,*)' FOCLEN ', FOCLEN
                      write(6,*)' CTR_PX ', CTR_PX
                      write(6,*)' SATELLITE TO LANDMARK, &
                             &BODY FIXED UNIT VECTOR'
                      write(6,*) SAT2LMK_BF(1,1), SAT2LMK_BF(1,2), &
                              SAT2LMK_BF(1,3)
                  END IF

                  CALL GETPX(PX, FKMAT, DISTORT, FOCLEN, CTR_PX, &
                          SAT2LMK_BF(I,1:3))
                  DO J = 1, 2
                      IF (IPERT == 0) THEN
                          PIXEL(I,J) = PX(J)
                      ELSE IF (IPERT == IPERTPOS) THEN
                          PARTL_POS(I,IP,J) &
                                  = -(PIXEL(I,J) - PX(J)) / FPERTPOS
                      ELSE IF (IPERT == IPERTPLAN) THEN
                          PARTL_PLAN(IP,I,J) &
                                  = -(PIXEL(I,J) - PX(J)) / DXGEO1
                      ELSE IF (IPERT == IPERTATT) THEN
                          JQ=IP
                          KQ=1
                          IF(IP.GT.3) THEN
                            JQ=IP-3
                            KQ=2
                          ENDIF
                          PARTL_ATT(IP,I,KQ,J) &
                                  = -(PIXEL(I,J) - PX(J)) / ATTPRT
                      ELSE IF (IPERT == IPERTLMK) THEN
                          PARTL_LMK(I,IP,J) &
                                  = -(PIXEL(I,J) - PX(J)) / FPERTPOS
                      ELSE IF (IPERT == IPERTCAM) THEN
                          IF (IP == 1) THEN
                              PARTL_CAM(I,IP,J) &
                                      = -(PIXEL(I, J) - PX(J)) &
                                      / FPERTFOCLEN
                          ELSE IF (IP >= 2 .AND. IP <= 5) THEN
                              PARTL_CAM(I,IP,J) &
                                      = -(PIXEL(I, J) - PX(J)) &
                                      / FPERTDISTORT
                          END IF
                      ELSE IF (IPERT == IPERTCOF) THEN
                          PARTL_COFCOM(I,IP,J) &
                                  = -(PIXEL(I,J) - PX(J)) / FPERT_COFCOM
                      END IF
                  END DO
              END DO

              ! RESTORE VARIABLES FROM UNPERTURBED VALUES
              XSM(1:NM, 1:3) = XSM_ORIG(1:NM, 1:3)
              POSLMK(1:3) = POSLMK_ORIG(1:3)
              FOCLEN = FOCLEN_ORIG
              DISTORT(1:4) = DISTORT_ORIG(1:4)
              COF_COM_BF(1:3) = COF_COM_BF_ORIG(1:3)
          END DO
      END DO

      ! CHAIN PARTIALS
      IF (LNPNM) THEN
          NDMXU1 = N_EQN
          NDMXU2 = 3
          NDMXU3 = NM
      ELSE
          NDMXU1 = NM
          NDMXU2 = N_EQN
          NDMXU3 = 3
      END IF
      DO I = 1, NM
          FULLTIME(I) = MJDSBL + FSECN(I)
      END DO

      DO J = 1, 2
          CALL CLEARA(PMPA, MAPARM*NM)

          ! CHAIN FORCE MODEL PARTIALS
          CALL CHAIN(PARTL_POS(1,1,J), NM, 3, 1, PXPF, NDMXU1, &
                  NDMXU2, NDMXU3, N_EQN, 3, IPTFMG(1,ISAT), &
                  ILNFMG(1,ISAT), NFMG(ISAT), PMPA, NDIM1, NDIM2, &
                  LNPNM, .FALSE., .FALSE., 1.0D0, LL(KLAVOI))
          ! CHAIN FORCE MODEL PARTIALS OF SECONDARY ASTEROID
       IF(LBINAST.AND.TLBOD2.GT.1.5D0) THEN
          CALL CHAIN(PARTL_POS(1,1,J), NM, 3, 1, AA(KASTP), NDIMA1, &
                  NDIMA2, NDIMA3, NEQNB, 3, IPTFMG(1,2), &
                  ILNFMG(1,2), NFMG(2), PMPA, NDIM1, NDIM2, &
                  LNPNM, .TRUE., .FALSE., 1.0D0, LL(KLAVOI))
       ENDIF

          ! CHAIN PLANOR PARTIALS
          IF (LSTINR .AND. NPVAL0(IXXTRO) > 0) THEN
              IF (LINTAX) THEN
                  IXAST=(IAST-1)*MINTIM*NDPOR
                  DO JQ = 1, NDPOR
                    XR(JQ) = AA(KDPOR +IXAST+ (JQ-1)*MINTIM) !AATJS(1,J)
                  END DO
                  CALL GETDADA0(XR(4), XR(13), DO0DA0(1,1,IAST), DADA0)
                  CALL GETDADIC(XR(4), XR(157), DADIC)

              END IF
              CALL CHPOR(PARTL_PLAN(1,1,J), PMPA, NM, NDIM1, NDIM2, &
                      LNPNM, FULLTIME, LL(KLAVOI), AA(KANGWT), &
                      AA(KWT), AA(KPPER), DADA0, DADIC, IAST)
          END IF

          ! CHAIN ATTITUDE PARTIALS
          IF (IAPLSC > 0) THEN
              CALL CHNAT2(PARTL_ATT(1,1,1,J), PMPA, NM, NDIM1, NDIM2, &
                      LNPNM, FULLTIME, ATPER(ISAT), LL(KLAVOI), IAPLSC)
          END IF
          IF (IAPLL > 0) THEN
              CALL CHNAT2(PARTL_ATT(1,1,2,J), PMPA, NM, NDIM1, NDIM2, &
                      LNPNM, FULLTIME, ATPER(ISAT), LL(KLAVOI), IAPLL)
          END IF

          ! 'CHAIN' LANDMARK PARTIALS
          ! JPMPAT IS THE PMPA INDEX OF THE X COORD OF THE LANDMARK
          IF (LTARJ) THEN
              DO I = 1, NM
                  DO K = 1, 3
                      IF (LNPNM) THEN
                          PMPA(JPMPAT-1 + K, I) = PARTL_LMK(I, K, J)
                      ELSE
                          PMPA(I, JPMPAT-1 + K) = PARTL_LMK(I, K, J)
                      END IF
                      LL(KLAVOI + JPMPAT-2 + K) = .FALSE.
                  END DO
              END DO
          END IF

          ! 'CHAIN' LANDMARK SCALE PARTIALS
          IF(PSCLND.GT.0.001D0.AND.LSTINR) THEN
           IPSCL=(PSCLND+0.001D0)
           LL(KLAVOI + IPSCL-1) = .FALSE.
!!!        TARGR=POSLMK(1)*POSLMK(1)+POSLMK(2)*POSLMK(2)                &
!!!  &          +POSLMK(3)*POSLMK(3)
!!!        TARGR=DSQRT(TARGR)
           TARGR=1.D0
           TNORM(1)=POSLMK(1)/TARGR
           TNORM(2)=POSLMK(2)/TARGR
           TNORM(3)=POSLMK(3)/TARGR
           DO I=1,NM
           DO K = 1, 3
             IF (LNPNM) THEN
                PMPA(IPSCL,I)=PMPA(IPSCL,I)+PARTL_LMK(I,K,J)*TNORM(K)
             ELSE
                PMPA(I,IPSCL)=PMPA(I,IPSCL)+PARTL_LMK(I,K,J)*TNORM(K)
             END IF
           END DO
           END DO
          END IF

          ! 'CHAIN' CAMERA PARTIALS
          IDCAM = INT(CAMID)
          CALL FNDNUM(IDCAM, NSCID, 30, IRET2)

          IF (NPVAL0(IXCAME) >= 5) THEN
              IPTD = IPVAL0(IXCAME) - 1 + (IRET2 - 1)*5
              DO I = 1, NM
                  DO K = 1, 5
                      IF (LNPNM) THEN
                          PMPA(K+IPTD, I) = PARTL_CAM(I, K, J)
                      ELSE
                          PMPA(I, K+IPTD) = PARTL_CAM(I, K, J)
                      END IF
                      LL(KLAVOI + IPTD + K - 1) = .FALSE.
                  END DO
              END DO
          END IF

          ! 'CHAIN' COF-COM PARTIALS
          IF (NPVAL0(IXCOFF) > 0) THEN
              DO I = 1, NM
                  DO K = 1, 3
                      IF (LNPNM) THEN
                          PMPA(IPVAL0(IXCOFF)-1+K, I) &
                                  = PARTL_COFCOM(I, K, J)
                      ELSE
                          PMPA(I, IPVAL0(IXCOFF)-1+K) &
                                  = PARTL_COFCOM(I, K, J)
                      END IF
                      LL(KLAVOI-1 + IPVAL0(IXCOFF)-1 + K) = .FALSE.
                  END DO
              END DO
          END IF

          ! SET UP RESIDUAL AND WEIGHT ARRAYS
          DO I = 1, NM
              LEDIT(I) = (SIGPIX(I,J) <= 0.0D0)
          END DO

          DO I = 1, NM
              RESID(I) = OBSPIX(I,J) - PIXEL(I,J)
              WEIGHT(I) = 1.0D0 / SIGPIX(I,J)**2
          END DO

          IF (J == 1) THEN
              write(6,*) ' PIXEL COORDINATE ONE RESIDUAL(IN PIXELS)', &
                      RESID(1)
          END IF
          IF (J == 2) THEN
              write(6,*) ' PIXEL COORDINATE TWO RESIDUAL(IN PIXELS)', &
                      RESID(1)
          END IF

          TMPRESID(J) = RESID(1)
          TMPRATIO(J) = RESID(1) / EDTSIG(1)
          TMPSIGMA(J) = RESID(1) / SIGPIX(1, J)

!!!!      IF(J.EQ.1) OBQ1=-RESID(1)
          IF(J.EQ.1) OBQ1=PIXEL(1,J)
!!!!      IF(J.EQ.1) OBQ1=OBSPIX(1,J)
!!!!      IF(J.EQ.2) OBQ2=-RESID(1)
          IF(J.EQ.2) OBQ2=PIXEL(1,J)
!!!!      IF(J.EQ.2) OBQ2=OBSPIX(1,J)
          ! SUM INTO NORMAL MATRIX
          CALL SUMNPF(PMPA, RESID, WEIGHT, NADJST, MAPARM, NM, &
                  AA(KSUM1), AA(KSUM2), AA(KPDLTA), LL(KLAVOI))
      END DO
      RATIO(1) = RESID(1) / EDTSIG(1)

      ! PRINT RESIDUAL HEADERS
      INDSYS = MOD(ITSYS, 10000) / 100
      INDSYS = MIN(MAX(INDSYS+1,1), 3)

      SIGMA(1) = RESID(1) / SIGPIX(1, 1)
      KNTOBS = KNTOBS + 1
      KNTBLK = KNTBLK + 1
      CALL YMDHMS(MJDSBL, OBSTIM, IYMD, IHM, SEC, NM)
      IF (IYMD > 1000000) THEN
          IYMD = IYMD - 1000000
      END IF

      IPAGE6 = IPAGE6 + 1
      WRITE(6,10001) NARC, NINNER, NGLOBL, IPAGE6, DTYPE(1), BLKNEW, &
              ITAR, 0, 0, TIMSYS(INDSYS)

10001 FORMAT('1', 15X, 'OBSERVATION RESIDUALS FOR ARC', I3, &
              ' FOR INNER ITERATION', I3, ' OF GLOBAL ITERATION', &
              I2, 17X, 'UNIT  6 PAGE NO.', I6/          2X, &
              'STATION-SATELLITE CONFIGURATION ', 2X, A8, 12X, &
              A7/23X, 'TARGET  NUMBERS -', I8, 12X, I8, 12X, I8, &
              /2X, 'DATE  GREENWICH TIME', 3X, 'PIXEL', 3X, &
              'OBSERVATION', 4X, 'RESIDUAL', 2X, &
              'RATIO TO SIGMA', 4X, ' SIGMA ', 4X, 'OBS NO.', 2X, &
              'BLOCK'/1X, 'YYMMDD HHMM SEC-UTC-', A1)

      DO JJ = 1, 2
          WRITE(6,10400) IYMD, IHM, SEC, JJ, OBSPIX(1,JJ), &
                  TMPRESID(JJ), TMPRATIO(JJ), TMPSIGMA(JJ), KNTOBS, &
                  KNTBLK
      END DO

10400 FORMAT(1X, I6, I5, F10.6, 3X, I2, 2X, F15.6, F15.6, &
              F12.4, 1X, F12.4, I9, I8)

      NM = 1
      NTYPE = MTYPE
      NMWTD = 1
      SIGMA(1) = RESID(1) / SQRT(SIGPIX(1,1))

      CALL SMSTAT(AA(KTMEAN), AA(KTRMS), AA(KWMEAN), AA(KWTRMS), &
              AA(KWTRND), AA(KPRVRT), AA(KTYMEA), AA(KTYRMS), &
              AA(KWTMTY), AA(KWTYRM), II(KNOBST), II(KNOBWT), &
              II(KNOBTY), II(KNOBWY), LL(KLGPRV), RESID     , &
              NM        , SIGMA     , RATIO     , NMWTD     , &
              NTYPE     , ISTATS_L)

      ELEVSC(1) = TMPRESID(2)

      DTAR = DBLE(ITAR)
      IF (LBINRI) THEN
          CALL BINLEN33(AA, II, DTAR)
      END IF

      IF (LLOCRI) THEN
          CALL CONHD
          WRITE(IUNT19) ZERO, ZERO, ZERO, POSLMK(1), POSLMK(2), &
                  POSLMK(3), ZERO, ZERO, ZERO
          WRITE(IUNT19) OBSTIM, XSM(1,1), XSM(1,2), XSM(1,3), &
                  XSM(1,19), XSM(1,20), XSM(1,21)
      END IF
      IF (LBINRI) THEN
          CALL BINRDR(AA, II, MJDSBL, OBSTIM, EDTSIG, TMPRESID(1), &
                  AA(KTPRTL), AA(KTHG), ELEVSC, NELEVS, LEDIT, &
                  RESIDU, NM, AA(KXUTDT))
      END IF

      IF (LODRI) THEN
          CALL BOD(AA, II, NM)
      END IF
      IF(LSIM) THEN
        TQ=MJDSBL+FSECN(1)-68.184D0
        WRITE(29,7000) ITAR,TQ,OBQ1,OBQ2
7000    FORMAT(I10,F20.3,2D25.16)
      ENDIF
6000  FORMAT(' EXECUTION TERMINATING IN SUBROUTINE LANDMK')
6001  FORMAT(' ATTEMPT TO ADJUST TARGET COORDINATES AND')
6002  FORMAT(' TARGET SCALE SIMULTANEOUSLY')
      END SUBROUTINE
