!$CONGEN
      SUBROUTINE CONGEN(AA,II,LL,INDSAT,INDSET,XTRA,PXEPA,EDTSIG,MAXNM,&
     &    MXLASR,NDIMA,NDIMB,NDIMC,LIN,PV,PMPA,PXPF,RMVPRT,OBSSIG,OBS, &
     &    C2PART,BOUNCE,BPPART)
!********1*********2*********3*********4*********5*********6*********7**
! CONGEN           00/00/00            0000.0    PGMR - ?
! FUNCTION CONTROL CONSTRAINTS OF CROSSOVER BOUNCING POINTS PROCESSING
!
! I/O PARAMETERS:
!
!   INDSAT   I    A    FOR A GIVEN INTERNAL SAT NO (1,2,3) INDSAT
!                      GIVES THE ACTUAL LOCATION IN THE SATELLITE
!                      RELATED ARRAYS FOR THIS SATELLITE.
!                      (EG.   SAT ID=ISATNO(INDSAT(1,2,OR 3))
!   INDSET   I    A    FOR A GIVEN INTERNAL SATELLITE NUMBER INDSET TELL
!                      WHICH SAT SET THAT THE GIVEN SAT BELONGS TO.
!   XTRA    I/O   A    SCRATCH SPACE
!   PXEPA   I/O   A    SCARTCH SPACE FOR PARTIALS OF ECF BOUNCE POINT
!                      WRT VARIOUS PARAMETERS
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/EXATFR/FSSCEA,RINTEA
      COMMON/SVOBS/CONOBS
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CINTI/IBACK ,IBACKV,IORDER,IORDRV,NOSTEP,NOCORR,           &
     &      NSAT  ,N3    ,NEQN  ,NEQN3 ,NH    ,NHV   ,NSTEPS,           &
     &      NSTEPV,ICPP  ,ICPV  ,ICCP  ,ICCV  ,ICCPV ,ICCVV ,           &
     &      ISUMX ,IXDDOT,ISUMPX,IPXDDT
      COMMON/CKOUNT/KNTBLK,KSEGMN,KNTOBS,KZROBS
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CLPRNR/LPR9NR(24),LPRNRM(24,3)
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/CON2E/KCNBST0,KCNBST1,KBUP110,KBUP111,KBPA110,KBPA111,KDIST&
     &            ,KDSIG
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
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
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
      COMMON/COFSTL/LOFEXT(2)
      COMMON/CONTRL/LSIMDT,LOBS  ,LORB  ,LNOADJ,LNADJL,LORFST,LORLST,   &
     &              LORALL,LORBOB,LOBFST,LOBLST,LOBALL,LPREPO,LORBVX,   &
     &              LORBVK,LACC3D,LSDATA,LCUTOT,LACCEL,LDYNAC,LFRCAT,   &
     &              LNIAU, NXCONT
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
      COMMON/CORI05/KNOBGL,KNOBWG,KNOBTY,KNOBWY,KNOBST,KNOBWT,KNEBOB,   &
     &              KJSTAT,NXCI05
      COMMON/CORA05/KOBTYP,KDSCRP,KGMEAN,KGLRMS,KWGMEA,KWGRMS,KTYMEA,   &
     &       KTYRMS,KWTMTY,KWTYRM,KTMEAN,KTRMS ,KWMEAN,KWTRMS,KWTRND,   &
     &       KPRVRT,KEBSTT,KVLOPT,NXCA05
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
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CORL05/KLGPRV,NXCL05
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DELOFF/NOFFST,NSATDL,NSTADL,NCGMAS,NXDELO
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
      COMMON/NEQINT/NINTOT,MEMTOT,NEQNIM,NXNEQS
      COMMON/LSNAPX/LSNAP
      COMMON/OCCLTL/LOCCLT(200),LMSHD(200)
      COMMON/NSEGSC/NSEGM(80)
      COMMON/XOVER /KXKNT,KXBST,KXOBL,KXBUP,KXBPA,IAPLSC,IAPLL
      COMMON/XOVERR/XBTIME
      COMMON/XOVERS/NRXTOT,NXBLK,NUSIDE,NDEGX2,NDEGXX,NUCON,ITERNU,     &
     &              IPEDIT,IDEDIT,NXXOVR
      COMMON/CITER /NINNER,NARC,NGLOBL
!
      DIMENSION AA(1),II(1),LL(1)
      DIMENSION EDTSIG(1)
      DIMENSION INDSAT(3),INDSET(3)
      DIMENSION XTRA(MINTIM,33)
      DIMENSION PXEPA(NDIM1,NDIM2,3,MXLASR)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION PXPF(NDIMA,NDIMB,NDIMC,2)
      DIMENSION PV(NADJST,2,3)
      DIMENSION RMVPRT(NDIMA,NDIMB,NDIMC,2,2)
!
      DIMENSION RINDEX(1)
      DIMENSION XBNC(1,3,2)
      DIMENSION OBS(1),RESID(1),OBSSIG(1)
      DIMENSION SIGMA(1),RATIO(1)
      DIMENSION LAVOSV(1000)
      DIMENSION NUM(3)
      DIMENSION BLKSEG(2),TIMSYS(3)
      DIMENSION DTYPE(2)
      DIMENSION C2PART(NEQNG,3,2,2,1)
      DIMENSION BOUNCE(NM,3,2,1)
      DIMENSION BPPART(NDIM1,NDIM2,3,2,1)
      DATA NUM/3*0/


      CHARACTER*1      BLANK, EDTFLG
      CHARACTER*8      DTYPE
      DATA DTYPE/'DISTANCE','RAD SEP '/
      CHARACTER*8      BLKNEW, BLKOLD
      DATA BLANK/' ' /,EDTFLG/'*'/,BLKNEW/'(*NEW*)'/,BLKOLD/'(CONT.)'/
      CHARACTER*1      BLKSEG, TIMSYS, SEGMNT
      DATA BLKSEG/'A','B'/
      DATA TIMSYS/'R','X','T'/
      CHARACTER*8      HTOR, HTOD, HRADAL
      DATA HTOR/' T.O.R. '/,HTOD/' T.O.D. '/
      DATA HRADAL/' RADIAL '/

!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
       write(6,*)'                                         '
       write(6,50505) MTYPE
50505  FORMAT(' ***********  ENTER  CONGEN MTYPE=',I4,' *********** ')
       write(6,*)'                                         '

! INITIALIZE INDICES
       IRET0=0
       IRET1=0
! READ LASER ID, CONSTRAINED OBSERVATION AND ITS SIGMA
       ILAS=INT(BLKDAT(4,1))
       CONOBS=DBLE(KDIST)/1000.D0
!      OBSSIG(1)=DFLOAT(KDSIG)/1000.D0

! INITIALIZE POINTERS
!
! EVENT TIMES
      JFSECN=KFSECS(1,1)
      JFSECM=KFSECS(2,1)
      JFSECK=KFSECS(3,1)
      JFSECO=KFSECS(4,1)
! OBSERVATION CORRECTIONS RECORD
      JOBMET=KOBCOR(1,1)
      JOCTPC=KOBCOR(2,1)
      JOCDRY=KOBCOR(3,1)
      JOCWET=KOBCOR(4,1)
      IF(LPRNRM(5,1)) JOCWET=KOBCOR(5,1)
      JOCION=KOBCOR(6,1)
! OCEAN DYNAMICS RECORD
      JETIDE=KOBCOR(2,4)
      JOTIDE=KOBCOR(3,4)
      IF(LPRNRM(13,1)) JOTIDE=KOBCOR(4,4)
      JSST=KOBCOR(5,4)
! LOCATION DATA RECORD
      JUNQIN=KOBCOR(7,7)
      JRLAND=KOBCOR(8,7)
! LOGICAL FLAGS FOR OUTPUT
      LDIOUT=.FALSE.
      LXGOUT=.FALSE.
!
!   IMAGES ARE TAGGED RECEIVE  TIME FSECN
!  GET AN APPROXIMATE TRANSMIT TIME
      AA(JFSECK)=(AA(JFSECN)-ABS(AA(KOBS)/VLIGHT))
!  CONVERT CURRENT DATE TO YMD HMS
      OBSTIM=AA(JFSECN)
      CALL YMDHMS(MJDSBL,OBSTIM,IYMD,IHM,SEC,NM)
      IF(IYMD.GT.1000000)IYMD=IYMD-1000000
!     WRITE(6,*)' OBS TIME IN ET ', IYMD,IHM,SEC
      CALL UTCET(.FALSE.,1,MJDSBL,AA(JFSECN),FSECOT3,AA(KA1UT))
      CALL YMDHMS(MJDSBL,FSECOT3,IYMD,IHM,SEC,NM)
      IF(IYMD.GT.1000000)IYMD=IYMD-1000000
      WRITE(6,1004)IYMD,IHM,SEC,CONOBS,OBSSIG(1)
1004  FORMAT('  OBS TIME IN UTC DIST DSIG',I6,I6,F10.5,F10.3,D12.5)

!
! IN CASE OF SIMULATION OF INTERPLANETRY,INITIALIZE LOCCLT TO FALSE
      NELVS=0
      LSIMX=LSIMDT
      IF(ICBDGM.EQ.ITBDGM) LSIMX=.FALSE.
      IF(LSIMX) THEN
        DO I=1,NM
          LOCCLT(I)=.FALSE.
          LMSHD( I)=.FALSE.
        ENDDO
      ENDIF
!
! CHECK FOR APPLICABLE ALTIMETER ANTENNA OFFSETS
      JXYZOF(1)=KFRQOF
      JXYOF2(1)=KFRQOF
      JXYOF3(1)=KFRQOF
      JSADLY(1)=KFRQSA
      JSTDLY(1)=KFRQST
      JXYZCG(1,1)=KFRQOF
      JXYZCG(2,1)=KFRQOF
      MJDFCG(1,1)=0
      MJDFCG(2,1)=0
      LOFEXT(1)=.FALSE.
      IF(NOFFST.GT.0) THEN
        CALL IDANT(ISATID,0,.FALSE.,IANT)
        CALL OFFDEL(ISATID,0,BLKDAT(3,1),                               &
     &      II(KISATO),II(KISATD),II(KISATC),II(KISTAD),                &
     &      AA(KFRQOF),AA(KFRQSA),AA(KFRQST),II(KMJDCG),JICOOR,         &
     &      JXYZOF(1),JSADLY(1),JSTDLY(1),JXYZCG(1,1),MJDFCG(1,1),      &
     &      IANT,JXYOF2(1),LL(KNSCID),II(KIANTO),LL(KEXTOF),            &
     &      LOFEXT(1),.TRUE.,II,FRQLNK,1,AA,JXYOF3(1))
      ENDIF

! ******  DONE WITH INFORMATION WE NEED FOR THIS BLOCK *****

!********** INITIALIZE INDICES FOR THE APPROPRIATE MTYPE ***********

      IF(MTYPE.EQ.110) THEN
!     write(6,*)' dbg INITIALIZING 110 '
!
! KBUP110= LOCATION OF BLOCK BEGIN IN THE ENTIRE STREAM OF DATA
! KBPA110= SEQUENTIAL NUMBER OF BLOCK IN THE ENTIRE STREAM OF BLOCKS
      ISAVE0=KBUP110
      ISAVE20=KBPA110
! POINTER TO  KEY  ARRAY FOR THIS  BLOCK
      JDIMN0=KCNBST0+NM-1
! INITIALIZE LFNDX=.TRUE. MEANS SECOND BLOCK FOUND
      LFNDX0=.FALSE.
! INITIALIZE LODD=.TRUE. MEANS THE PRESENT BLOCK KEY IS ODD
      LODD0=.FALSE.

      ELSEIF(MTYPE.EQ.111) THEN
!
! KBUP111= LOCATION OF BLOCK BEGIN IN THE ENTIRE STREAM OF DATA
! KBPA111= SEQUENTIAL NUMBER OF BLOCK IN THE ENTIRE STREAM OF BLOCKS
      ISAVE1=KBUP111
      ISAVE21=KBPA111
! POINTER TO  KEY  ARRAY FOR THIS  BLOCK
      JDIMN1=KCNBST1+NM-1
! INITIALIZE LFNDX=.TRUE. MEANS SECOND BLOCK FOUND
      LFNDX1=.FALSE.
! INITIALIZE LODD=.TRUE. MEANS THE PRESENT BLOCK KEY IS ODD
      LODD1=.FALSE.

      ENDIF

!********** END INITIALIZE INDICES FOR THE APPROPRIATE MTYPE ***********


         IF(MTYPE.EQ.110) THEN
!**********************************************************************
! IDENTIFY THE BLOCK 110 KEY
!**********************************************************************
!
! LOCATE THE KEY IN THE II(KCN110) ARRAY LOADED IN OBSRD
! THIS ARRAY IS 2* # OF BLOCKS AND IS FILLED AS NEW BLOCKS
! ARE READ.
!
! KCNBST0 IS THE LOCATION OF THE CURRENT BLOCK IN THE TOTAL STREAM
! OF BLOCKS
!
      KINDX=KCN110+KCNBST0-1
      KEY0=II(KINDX)
      RKEY0=DBLE(KEY0)
!     write(6,*)' dbg KEY IS ',KEY0,KINDX

! CHECK IF THE KEY IS EVEN OR ODD
      CALL ODD(KEY0,LODD0)
! THE CODE BELOW FOR >>>> EVEN <<<< KEYS
! LOOP 1 PROCESS EVEN OR ODD
!
!**********************************************************************
! THE BLOCK KEY HAS BEEN IDENTIFIED
!**********************************************************************
!
        IF(LODD0) THEN
! FIND THE EVEN KEY IN THE PAIR
        MKEY0=KEY0+1
!     write(6,*)' dbg so the other  KEY will be  ',MKEY0
        ELSE
! FIND THE ODD KEY IN THE PAIR
        MKEY0=KEY0-1
!     write(6,*)' dbg so the other  KEY will be  ',MKEY0
        ENDIF
!
!**********************************************************************
! THE FIRST KEY HAS BEEN IDENTIFIED
! HAS THE PAIRING KEY BEEN FOUND ALREADY?
!**********************************************************************
!
        CALL FNDNUM(MKEY0,II(KC110),JDIMN0,IRET0)
!
! IRET0=0 NO IT HAS NOT BEEN FOUND
! IRET0.NE.0 IT HAS BEEN FOUND IN LOCATION IRET0 IN THE FULL KEY ARRAY
      IF(IRET0.EQ.0) THEN
      write(6,*)' CURRENT KEY ',KEY0
      write(6,*)' WE DO NOT HAVE A PAIR YET WITH PAIRING KEY ',MKEY0
      KPAIR=1
      ELSE
      write(6,*)' WE DO HAVE A PAIR NOW WITH KEYS ',MKEY0-1,MKEY0
      KPAIR=2
      ENDIF

!**********************************************************************

          ELSEIF(MTYPE.EQ.111) THEN

!**********************************************************************
! IDENTIFY THE BLOCK 111 KEY
!**********************************************************************
!
! LOCATE THE KEY IN THE II(KCN110) ARRAY LOADED IN OBSRD
! THIS ARRAY IS 2* # OF BLOCKS AND IS FILLED AS NEW BLOCKS
! ARE READ.
!
! KCNBST1 IS THE LOCATION OF THE CURRENT BLOCK IN THE TOTAL STREAM
! OF BLOCKS
!
      KINDX=KCN111+KCNBST1-1
      KEY1=II(KINDX)
      RKEY1=DBLE(KEY1)

! CHECK IF THE KEY IS EVEN OR ODD
      CALL ODD(KEY1,LODD1)
! THE CODE BELOW FOR >>>> EVEN <<<< KEYS
! LOOP 1 PROCESS EVEN OR ODD
!
!**********************************************************************
! THE BLOCK KEY HAS BEEN IDENTIFIED
!**********************************************************************
!
!
        IF(LODD1) THEN
! FIND THE EVEN KEY IN THE PAIR
        MKEY1=KEY1+1
        ELSE
! FIND THE ODD KEY IN THE PAIR
        MKEY1=KEY1-1
        ENDIF
!
!**********************************************************************
! THE FIRST KEY HAS BEEN IDENTIFIED
! HAS THE PAIRING KEY BEEN FOUND ALREADY?
!**********************************************************************
!
        CALL FNDNUM(MKEY1,II(KC111),JDIMN1,IRET1)
!
! IRET1=0 NO IT HAS NOT BEEN FOUND
! IRET1.NE.0 IT HAS BEEN FOUND IN LOCATION IRET1 IN THE FULL KEY ARRAY
      IF(IRET1.EQ.0) THEN
      write(6,*)' CURRENT KEY ',KEY1
      write(6,*)' WE DO NOT HAVE A PAIR YET WITH CURRENT KEY ',MKEY1
      KPAIR=1
      ELSE
      write(6,*)' WE DO HAVE A PAIR NOW WITH KEYS ',MKEY1-1,MKEY1
      KPAIR=2
      ENDIF

!**********************************************************************

          ENDIF

!**********************************************************************
! CALL ORBIT
!**********************************************************************

! FIND THE SATELLITE ID
      ISATID=II(KISATN+INDSAT(1)-1)
      ISAT=INDSAT(1)
!
! CALL ORBIT TO SAVE PXPF AT RECEIVE TIME
!
      IF(NINTOT.GT.0) THEN
      IF(INTOSP.EQ.0) INTOSP=1
      CALL FNDNUM(ISATID,II(KSATIN),NINTOT,IRET)
      IF(IRET.GT.0) THEN
      KNSTEPS=II(KSTEPS-1+IRET)
      NEQNI=II(KNEQNI-1+IRET)
      ENDIF
      ENDIF

      CALL ORBSET(II(KNSAT),II(KIVSAT),LL(KLSETS),ISAT,                 &
     &   KNSTEPS,NEQNI,II(KSGMNT-1+IRET),                               &
     &   AA(KSGTM1-1+IRET),AA(KSGTM2-1+IRET),IRET,MJDSBL,AA(JFSECN),    &
     &   AA(JFSECN-1+NM),NM,AA,II)

      IF(IRET.GT.0) THEN
      KSUMXOS=MAPSAT(INTOSP,1)
      KSUMPOS=MAPSAT(INTOSP,2)
      KXDDTOS=MAPSAT(INTOSP,3)
      KPDDTOS=MAPSAT(INTOSP,4)
      ENDIF

!     write(6,*)' dbg call ORBIT ',MJDSBL,AA(JFSECN),CONOBS
      CALL ORBIT(MJDSBL,AA(JFSECN),AA(KS),1,II(KINDH),II(KNMH),         &
     &   II(KMJDSC),  &
     &   AA(KFSEC),II(KMJDSV),AA(KFSECV),LL(KLSETS),II(KIPXPF),         &
     &   II(KIVSAT),1,II(KNMAX),II(KNTOLD),AA(KXTK),PXPF,               &
     &   .FALSE.,.FALSE.,II(KPLPTR),II(KPANEL),II(KNMOVE),              &
     &   AA(KTPMES+4*MINTIM),LL(KLTPMS+6*MINTIM),LL(KSETDN),            &
     &   AA(KTMOS0-1+INTOSP),AA(KTMOS-1+INTOSP),AA(KTMOSP-1+INTOSP),    &
     &   AA(KSMXOS-1+KSUMXOS),AA(KSMXOS-1+KSUMPOS),AA(KSMXOS-1+KXDDTOS),&
     &   AA(KSMXOS-1+KPDDTOS),KNSTEPS,NEQNI,II(KSGMNT-1+IRET),          &
     &   AA(KSGTM1-1+IRET),AA(KSGTM2-1+IRET),AA,II,LL)


!*********************************************************************
! SAVE INFO FROM THE FIRST OCCURANCE HERE
!*********************************************************************

      IF(MTYPE.EQ.110) THEN

      IF(IRET0.EQ.0) THEN
! PUT THE KEY IN THE II(KC110) ARRAY  FOR FIRST ENCOUNTER
!     II(KC110+KCNBST0-1)=MKEY0
      II(KC110+KCNBST0-1)=KEY0
      WRITE(6,*)' THIS IS THE FIRST OCCURANCE IN THE 110 PAIR '
      WRITE(6,*)' JUST SAVING INFORMATION FOR OBS w/KEY=',KEY0
      ENDIF

      ELSEIF(MTYPE.EQ.111) THEN

      IF(IRET1.EQ.0) THEN
! PUT THE KEY IN THE II(KC111) ARRAY  FOR FIRST ENCOUNTER
!     II(KC111+KCNBST1-1)=MKEY1
      II(KC111+KCNBST1-1)=KEY1
!     WRITE(6,*)' THIS IS THE FIRST OCCURANCE IN THE 111 PAIR '
!     WRITE(6,*)' JUST SAVING INFORMATION FOR OBS w/KEY=',KEY1
      ENDIF

      ENDIF

!**********************************************************************
!  SAVE FM PARTIALS
!**********************************************************************
      IF(MTYPE.EQ.110) THEN

      DO I=1,NM
! SAVE PARTIALS OF XYZ WRT FM PARAMETERS (PXPF) VARIATIONAL PARTIALS
      IF(LNPNM) GOTO 30315
      DO 30314 KK=1,NDIMB
      DO 30313 JJ=1,3
      C2PART(KK,JJ,1,1,KBPA110)=PXPF(I,KK,JJ,1)
      C2PART(KK,JJ,2,1,KBPA110)=PXPF(I,KK,JJ,2)
! DEBUG ****************************************************************
!     write(6,*)' PARTIALS NM,NP ',C2PART(KK,JJ,1,1,KBPA110),KK,JJ,    &
!    &  KBPA110
! DEBUG ****************************************************************
30313 END DO
30314 END DO
      GOTO 30318
30315 CONTINUE
      DO 30317 KK=1,NDIMA
      DO 30316 JJ=1,3
      C2PART(KK,JJ,1,1,KBPA110)=PXPF(KK,JJ,I,1)
      C2PART(KK,JJ,2,1,KBPA110)=PXPF(KK,JJ,I,2)
! DEBUG ****************************************************************
!     write(6,*)' PARTIALS NP,NM ',C2PART(KK,JJ,1,1,KBPA110),KK,JJ,    &
!    &  KBPA110
! DEBUG ****************************************************************
30316 END DO
30317 END DO
30318 CONTINUE
      KBPA110=KBPA110+1
      ENDDO

      ELSEIF(MTYPE.EQ.111) THEN

      DO I=1,NM
! SAVE PARTIALS OF XYZ WRT FM PARAMETERS (PXPF) VARIATIONAL PARTIALS
      IF(LNPNM) GOTO 40315
      DO 40314 KK=1,NDIMB
      DO 40313 JJ=1,3
      C2PART(KK,JJ,1,2,KBPA111)=PXPF(I,KK,JJ,1)
      C2PART(KK,JJ,2,2,KBPA111)=PXPF(I,KK,JJ,2)
! DEBUG ****************************************************************
!     write(6,*)' PARTIALS NM,NP ',C2PART(KK,JJ,2,KBPA111),KK,JJ,KBPA111
! DEBUG ****************************************************************
40313 END DO
40314 END DO
      GOTO 40318
40315 CONTINUE
      DO 40317 KK=1,NDIMA
      DO 40316 JJ=1,3
      C2PART(KK,JJ,1,2,KBPA111)=PXPF(KK,JJ,I,1)
      C2PART(KK,JJ,2,2,KBPA111)=PXPF(KK,JJ,I,2)
! DEBUG ****************************************************************
!     write(6,*)' PARTIALS NP,NM ',C2PART(KK,JJ,2,KBPA111),KK,JJ,KBPA111
! DEBUG ****************************************************************
40316 END DO
40317 END DO
40318 CONTINUE
      KBPA111=KBPA111+1
      ENDDO

      ENDIF

! WE NEED THEM IN RMVPRT ALSO FOR USE OF PAIR PARTIALS IN EXDYN1
      DO  I=1,NM
      IF(LNPNM) THEN
      DO JJ=1,NDIMA
      DO IJ=1,NDIMB
      RMVPRT(JJ,IJ,I,1,KPAIR)=PXPF(JJ,IJ,I,1)
      RMVPRT(JJ,IJ,I,2,KPAIR)=PXPF(JJ,IJ,I,2)
      ENDDO
      ENDDO
      GOTO 300

      ELSE
      DO JJ=1,NDIMA
      DO IJ=1,NDIMB
      RMVPRT(I,JJ,IJ,1,KPAIR)=PXPF(I,JJ,IJ,1)
      RMVPRT(I,JJ,IJ,2,KPAIR)=PXPF(I,JJ,IJ,2)
      ENDDO
      ENDDO
      ENDIF

 300  CONTINUE
      ENDDO
!**********************************************************************
!  SAVED FM PARTIALS
!**********************************************************************

!**********************************************************************
!  INITIALIZATIONS BEFORE CALLING LTALTO
!**********************************************************************
! LXBEAM NEEDS TO BE TRUE IN LTALTO. NORMALLY THE CONSTRAINTS DATA
! WILL BE PROCESSED TOGETHER WITH MULTIBEAM CROSSOVER, SO LXBEAM SHOULD
! BE TRUE. HERE WE LET IT BE TRUE ANYWAY.
      LXBEAM=.TRUE.
! SET LSNAP TO TRUE, SINCE OUR CONSTRAINT DATA WILL BE OUT OF TIME ORDER.
! LSNAP NEEDS TO BE TRUE IN ORGRATE AND PGRATE FOR ORBIT INTERPOLATION.
      LSNAP=.TRUE.
! SAVE LX2ALT
      LSAVE=LX2ALT
      LX2ALT=.TRUE.
! INTIALIZE NEQN, WILL BE USED IN ORBIT AS DIMENSION
      NEQN=II(KNEQN-1+INDSET(1))
! SET UP ITERNU, THIS IS NECESSARY FOR CALCULATING THE PXIPFM
      ITERNU=0
! SET UP NUCON TO NM. THIS IS TO MAKE SURE THAT THE PMATT
! HAS THE SAME DIMENSION IN LTALTO AND EXDYN1 AND CONFORM
      NUSAV=NUCON
      NUCON=NM
!**********************************************************************
!  END INITIALIZATIONS BEFORE CALLING LTALTO
!**********************************************************************

!**********************************************************************
! CALCULATE THE BOUNCING POINT AND PARTIALS BY CALLING LTALTO
!**********************************************************************
!     write(6,*)'CALL LTALTO = ',KPAIR,ILAS,MJDSBL,AA(JFSECN)
!     write(6,*)'                              '
      CALL LTALTO(AA,II,LL,LNADJ,MJDSBL,AA(JFSECN),AA(JFSECM),        &
     &            AA(JFSECK),NM,MTYPE,INDSAT(1),INDSET(1),        &
     &            AA(KOBS),AA(KXTN),AA(KXSM),AA(KXTK),AA(KVTN),       &
     &            AA(KVTK),AA(KXSJ),AA(KTHETG),AA(KCOSTH),AA(KSINTH), &
     &            AA(JOBMET),AA(JOCDRY),AA(JOCWET),AA(JOCION),        &
     &            AA(JOTIDE),AA(KX2SCR),LNPNM,LDIOUT,LXGOUT,          &
     &            AA(KPMATT),AA(KXHOLD),AA(JSST),RINDEX,AA(KSATLT),   &
     &            AA(KSATLN),AA(KSATH),AA(KXY2),AA(KZT),AA(KRT),      &
     &            AA(KRA),XTRA,AA(KATROT),AA(KXUTDT),AA(JUNQIN),      &
     &            AA(JETIDE),ISATID,AA(JRLAND))

! DEBUG
!        print*,'mjds:',kpair,mjdsbl+aa(jfsecn),ISATID
!        print*,(aa(kpxpfm+k),k=0,8)
!        print*,(aa(kpxpfm+k),k=9,17)
!        print*,(aa(kpxpfm+k),k=18,26)
!        print*, '---------------------'
!        do i=0,19
!          print*,i+1,aa(kpmatt+i*3),aa(kpmatt+3*i+1),aa(kpmatt+3*i+2)
!        enddo
!        print*, '---------------------'
!**********************************************************************
! END CALCULATE THE BOUNCING POINT AND PARTIALS BY CALLING LTALTO
!**********************************************************************
! DEBUG
! WE NEED TO SET LSNAP BACK TO FALSE IN ORDER NOT TO BE CONFUSED
! WITH THE MULTIBEAM CROSSOVER. IF WE DO NOT SET IT BACK TO FALSE THE
! NEXT BLOCK WILL BE TREATED BY SNAPGN.
      LSNAP=.FALSE.
      LX2ALT=LSAVE
!
!**********************************************************************
! LOAD BOUNCE POINT IN BOUNCE FOR LATER USE AND XBNC FOR USE IN EXDYN1
!**********************************************************************

      IF(MTYPE.EQ.110) THEN

      DO I=1,NM
! BOUNCE POINT COORDINATES FOR 110 1st OCCURANCE
      DO JJ=1,3
      BOUNCE(I,JJ,1,KBUP110)=AA(KX2SCR-1+I*JJ)
      END DO
      END DO

      ELSEIF(MTYPE.EQ.111)  THEN

      DO I=1,NM
      DO JJ=1,3
      BOUNCE(I,JJ,2,KBUP111)=AA(KX2SCR-1+I*JJ)
      END DO
      END DO

      ENDIF

! LOAD THE SAME INFO IN XBNC TO BE USED IN EXDYN1
! SAVE THE ECF BOUNCING POINT
! NOTE KPAIR CAN ONLY BE 1 OR 2
!     DO I=1,NM
      DO I=1,1
        XBNC(I,1,KPAIR)=AA(KX2SCR-1+I)
        XBNC(I,2,KPAIR)=AA(KX2SCR-1+I*2)
        XBNC(I,3,KPAIR)=AA(KX2SCR-1+I*3)
      ENDDO
      F=XBNC(1,1,KPAIR)*XBNC(1,1,KPAIR) +                     &
     &  XBNC(1,2,KPAIR)*XBNC(1,2,KPAIR) +                     &
     &  XBNC(1,3,KPAIR)*XBNC(1,3,KPAIR)
      YYY=SQRT(F)
      write(6,*)' BNCCOO',XBNC(1,1,KPAIR),XBNC(1,2,KPAIR),XBNC(1,3,KPAIR)
      write(6,*)' RADIUS TO PLANET ',YYY
!
!*************************************************************************
! END LOAD BOUNCE POINT IN BOUNCE FOR LATER USE AND XBNC FOR USE IN EXDYN1
!*************************************************************************

!*************************************************************************
! WRITE OUT BINARY LENGTHS RECORD
!*************************************************************************
      IF(LBINRI) CALL BINLEN(AA,II)
!
!*************************************************************************
! CALCULATE AND SAVE PARTIALS FOR THIS PASS
!*************************************************************************
!       print*,'call exdyn1 IAPLSC,IAPLL,ILAS ',IAPLSC,IAPLL,KPAIR, &
!    &     ILAS,LNPNM

      CALL EXDYN1(IDUM,IDUM,INDSAT(1),INDSET(1),AA,II,LL,           &
     &            AA(KXTN),AA(KXSM),AA(KXTK),AA(KPXEXI),            &
     &            II(KNEQN),II(KN3),II(KIPTFM),II(KILNFM),          &
     &            II(KNFMG),PXEPA(1,1,1,ILAS),DUM,DUM,DUM,          &
     &            MJDSBL,AA(JFSECN),XBNC(1,1,KPAIR),AA(KXVTM1),     &
     &            NDIM1,NDIM2,XBTIME,IAPLSC,IAPLL,                  &
     &            RMVPRT(1,1,1,1,KPAIR),                            &
     &            NDIMA,NDIMB,NDIMC,AA(KPMATT),AA(KATPER),NM,       &
     &            MAXNM,.TRUE.,KPAIR)


!*************************************************************************
! SAVE PARTIALS OF BOUNCE POINT WRT TO VARIOUS PARAMETERS
!*************************************************************************
      IF(MTYPE.EQ.110) THEN

      IF(LNPNM) THEN
      DO J=1,NDIM1
      DO I=1,NDIM2
      DO KK=1,3
! MTYPE 110 1st OCCURANCE
      BPPART(J,I,KK,1,KBUP110)=PXEPA(J,1,KK,ILAS)
      ENDDO
      ENDDO
      ENDDO
       GOTO 60001
      ELSE
      DO J=1,NDIM1
      DO I=1,NDIM2
      DO KK=1,3
! MTYPE 110 1st OCCURANCE
      BPPART(I,J,KK,1,KBUP110)=PXEPA(J,1,KK,ILAS)
      ENDDO
      ENDDO
      ENDDO
      ENDIF
60001 CONTINUE
      KBUP110 = KBUP110 +1

      ELSEIF(MTYPE.EQ.111) THEN

      IF(LNPNM) THEN
      DO J=1,NDIM1
      DO I=1,NDIM2
      DO KK=1,3
! MTYPE 111 1st OCCURANCE
      BPPART(J,I,KK,2,KBUP111)=PXEPA(J,1,KK,ILAS)
      ENDDO
      ENDDO
      ENDDO
       GOTO 60002
      ELSE
      DO J=1,NDIM1
      DO I=1,NDIM2
      DO KK=1,3
! MTYPE 111 1st OCCURANCE
      BPPART(I,J,KK,2,KBUP111)=PXEPA(J,1,KK,ILAS)
      ENDDO
      ENDDO
      ENDDO
      ENDIF
60002 CONTINUE

      KBUP111 = KBUP111 +1

      ENDIF

!*************************************************************************
! SAVED PARTIALS OF BOUNCE POINT WRT TO VARIOUS PARAMETERS
!*************************************************************************
!
!*************************************************************************
! UP TO THIS POINT THE COMPUTATIONS WERE DONE FOR EACH OBSERVATION
! IF THE FIRST OBS IN A PAIR WAS ENCOUNTERED WE SAVED INFORMATION
! IF WE HAD A PAIR WE PROCESSED THE SECOND OBS IN THE PAIR
! NOW IN CASE WE HAVE PROCESSED THE SECOND OBSERVATION, WE NEED TO GET
! INFORMATION FROM THE FIRST BEFORE CALLING THE MODELING SUBROUTINE
! CONFORM
!*************************************************************************

!       AT THIS POINT IF IRET0 IRET1 are >0 LOAD BOTH PAIRS
!       LOAD PV FROM BPPART
!       LOAD XBNC FROM BOUNCE
!       LOAD RMVPRT FROM C2PART
!*************************************************************************
!   PROCESS 110 DISTANCE
!*************************************************************************
        IF(IRET0.GT.0.AND.MTYPE.EQ.110) THEN
          IF(KPAIR.NE.2) THEN
          WRITE(6,*)' STOP IN CONGEN , KPAIR SHOULD BE 2 and NOT ',KPAIR
          STOP
          ENDIF
!   CLEAR UP COMPLETELY THE RMVPRT ARRAY ONLY IF A PAIR HAS BEEN PROCESSED
      NXDM=NDIMA*NDIMB*NDIMC*2*2
      CALL CLEARA(RMVPRT,NXDM)
      NDXM=NADJST*6
      CALL CLEARA(PV,NXDM)
        CALL FNDNUM(MKEY0,II(KC110),1000,IRET2)
        XBNC(1,1,1)=BOUNCE(1,1,1,IRET2)
        XBNC(1,2,1)=BOUNCE(1,2,1,IRET2)
        XBNC(1,3,1)=BOUNCE(1,3,1,IRET2)
        write(6,*)'BOUNCE 1',XBNC(1,1,1),XBNC(1,2,1),XBNC(1,3,1),IRET2, &
     & MKEY0
!       write(6,*)'BOUNCE 2',XBNC(1,1,2),XBNC(1,2,2),XBNC(1,3,2)

      DO  I=1,NM
      IF(LNPNM) THEN
      DO JJ=1,NDIMA
      DO IJ=1,NDIMB
      RMVPRT(JJ,IJ,I,1,1)=C2PART(JJ,IJ,1,1,IRET2)
      RMVPRT(JJ,IJ,I,2,1)=C2PART(JJ,IJ,2,1,IRET2)
      RMVPRT(JJ,IJ,I,1,2)=PXPF(JJ,IJ,I,1)
      RMVPRT(JJ,IJ,I,2,2)=PXPF(JJ,IJ,I,2)
      ENDDO
      ENDDO
      GOTO 310

      ELSE
      DO JJ=1,NDIMA
      DO IJ=1,NDIMB
      RMVPRT(I,JJ,IJ,1,1)=C2PART(IJ,JJ,1,1,IRET2)
      RMVPRT(I,JJ,IJ,2,1)=C2PART(IJ,JJ,2,1,IRET2)
      RMVPRT(I,JJ,IJ,1,2)=PXPF(I,JJ,IJ,1)
      RMVPRT(I,JJ,IJ,2,2)=PXPF(I,JJ,IJ,2)
      ENDDO
      ENDDO
      ENDIF

 310  CONTINUE
      ENDDO

            IF(LNPNM) THEN
         DO JJ=1,NADJST
         DO KK=1,3
          PV(JJ,1,KK)=BPPART(JJ,1,KK,1,IRET2)
          PV(JJ,2,KK)= PXEPA(JJ,1,KK,ILAS)
        ENDDO
        ENDDO
            ELSE
         DO JJ=1,NADJST
         DO KK=1,3
          PV(JJ,1,KK)=BPPART(1,JJ,KK,1,IRET2)
          PV(JJ,2,KK)= PXEPA(1,JJ,KK,ILAS)
        ENDDO
        ENDDO
            ENDIF

        ENDIF

!*************************************************************************
!   PROCESS 111 RADIAL SEPARATION
!*************************************************************************
        IF(IRET1.GT.0.AND.MTYPE.EQ.111) THEN
          IF(KPAIR.NE.2) THEN
          WRITE(6,*)' STOP IN CONGEN , KPAIR SHOULD BE 2 and NOT ',KPAIR
          STOP
          ENDIF
!   CLEAR UP COMPLETELY THE RMVPRT ARRAY ONLY IF A PAIR HAS BEEN PROCESSED
      NXDM=NDIMA*NDIMB*NDIMC*2*2
      CALL CLEARA(RMVPRT,NXDM)
      NDXM=NADJST*6
      CALL CLEARA(PV,NXDM)
        CALL FNDNUM(MKEY1,II(KC111),1000,IRET3)
        XBNC(1,1,1)=BOUNCE(1,1,2,IRET3)
        XBNC(1,2,1)=BOUNCE(1,2,2,IRET3)
        XBNC(1,3,1)=BOUNCE(1,3,2,IRET3)
!       write(6,*)'BOUNCE 1',XBNC(1,1,1),XBNC(1,2,1),XBNC(1,3,1)
!       write(6,*)'BOUNCE 2',XBNC(1,1,2),XBNC(1,2,2),XBNC(1,3,2)

      DO  I=1,NM
      IF(LNPNM) THEN
      DO JJ=1,NDIMA
      DO IJ=1,NDIMB
      RMVPRT(JJ,IJ,I,1,1)=C2PART(JJ,IJ,1,2,IRET3)
      RMVPRT(JJ,IJ,I,2,1)=C2PART(JJ,IJ,2,2,IRET3)
      RMVPRT(JJ,IJ,I,1,2)=PXPF(JJ,IJ,I,1)
      RMVPRT(JJ,IJ,I,2,2)=PXPF(JJ,IJ,I,2)
      ENDDO
      ENDDO
      GOTO 320

      ELSE
      DO JJ=1,NDIMA
      DO IJ=1,NDIMB
      RMVPRT(I,JJ,IJ,1,1)=C2PART(IJ,JJ,1,2,IRET3)
      RMVPRT(I,JJ,IJ,2,1)=C2PART(IJ,JJ,2,2,IRET3)
      RMVPRT(I,JJ,IJ,1,2)=PXPF(I,JJ,IJ,1)
      RMVPRT(I,JJ,IJ,2,2)=PXPF(I,JJ,IJ,2)
      ENDDO
      ENDDO
      ENDIF

 320  CONTINUE
      ENDDO

            IF(LNPNM) THEN
         DO JJ=1,NADJST
         DO KK=1,3
          PV(JJ,1,KK)=BPPART(JJ,1,KK,2,IRET3)
          PV(JJ,2,KK)= PXEPA(JJ,1,KK,ILAS)
        ENDDO
        ENDDO
            ELSE
         DO JJ=1,NADJST
         DO KK=1,3
          PV(JJ,1,KK)=BPPART(1,JJ,KK,2,IRET3)
          PV(JJ,2,KK)=PXEPA(1,JJ,KK,ILAS)
        ENDDO
        ENDDO
            ENDIF

        ENDIF

!*************************************************************************
!   DONE LOADING
!*************************************************************************

! SAVE LAVOID
      IF(KPAIR.EQ.1) THEN
      DO K=1,NADJST
        LAVOSV(K)=LL(KLAVOI+K-1)
      ENDDO
      ENDIF
      DO K=1,NADJST
      ENDDO

      IF(KPAIR.EQ.2) THEN
!      print*,'call conform ',MTYPE,NM,NADJST,CONOBS
!     write(6,*)'dbgCO',XBNC(1,1,1),XBNC(1,2,1),XBNC(1,3,1)
!     write(6,*)'dbgCO',XBNC(1,1,2),XBNC(1,2,2),XBNC(1,3,2)

        CALL CONFORM(MTYPE,NM,NADJST,NDIM1,NDIM2,XBNC,NADJST,2,LNPNM, &
     &              PV(1,1,1),                                     &
     &              CONOBS,OBSSIG,AA(KPMPA),RESID,AA,LL,LAVOSV,MXLASR,&
     &              ISAT,ILAS)

!       write(6,*) ninner,narc,mjdsbl,aa(jfsecn),obs(1),resid(1)
!       print*, 'dbg OBS RESID: obs res',obs(1),resid(1)
! CALL SMSTAT FOR SUMMARY OF STATISTICS
          EDTSIG(1)=OBSSIG(1)
        DO I=1,NM
          RATIO(I)=RESID(I)/EDTSIG(I)
          SIGMA(I)=RESID(I)/OBSSIG(I)
!       write(6,*)' dbg RATIO ',RATIO(I),EDTSIG(I)
!       write(6,*)' dbg SIGMA ',SIGMA(I),OBSSIG(I)
        ENDDO

!
! PRINT RESIDUAL HEADERS
      INDSYS=MOD(ITSYS,10000)/100
      INDSYS=MIN(MAX(INDSYS+1,1),3)
!
! OUTPUT UTC CALENDAR TIMES
!
!     OBSTIM=AA(JFSECN)
      KNTOBS=KNTOBS+1
      KNTBLK=KNTBLK+1
!     CALL YMDHMS(MJDSBL,OBSTIM,IYMD,IHM,SEC,NM)
!     CALL YMDHMS(MJDSBL,FSECOT3,IYMD,IHM,SEC,NM)
!     IF(IYMD.GT.1000000)IYMD=IYMD-1000000
!     write(6,*)' dbg time yymmdd hhmm sec ',iymd,ihm,sec

!
      IPAGE6=IPAGE6+1
          IF(ICTYPE.EQ.110) THEN
          WRITE(6,10001,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,DTYPE(1),&
     &    BLKNEW,(NUM(I),I=1,3),                                        &
     &    TIMSYS(INDSYS)
          ELSE
          WRITE(6,10001,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,DTYPE(2),&
     &    BLKNEW,(NUM(I),I=1,3),                                        &
     &    TIMSYS(INDSYS)
          ENDIF

10001 FORMAT('1',15X,'OBSERVATION RESIDUALS FOR ARC',                   &
     &   I3,' FOR INNER ITERATION',I3,' OF GLOBAL ITERATION',I2,17X,    &
     &   'UNIT  6 PAGE NO.',I6/                                         &
     &   2X,'STATION-SATELLITE CONFIGURATION ',2X,A8,12X,               &
     &   A7/23X,'STATION NUMBERS -',I8,12X,I8,12X,I8,                   &
     &   /2X,'DATE  GREENWICH TIME',6X,'OBSERVATION',9X,                &
     &   'RESIDUAL',2X,'RATIO TO SIGMA',4X, ' SIGMA ', 4X,              &
     &   'OBS NO.',2X,'BLOCK'/1X,'YYMMDD HHMM SEC-UTC-',A1)

          WRITE(6,10400,IOSTAT=IERR)                                    &
     &                IYMD,IHM,SEC,CONOBS,RESID(1),                     &
     &                RATIO(1),SIGMA(1), KNTOBS,KNTBLK
10400 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,1X,F12.4, 6X,             &
     &       I9,I8)




        NMWTD=NM
        CALL SMSTAT  (AA(KTMEAN),AA(KTRMS ),AA(KWMEAN),AA(KWTRMS),      &
     &   AA(KWTRND),AA(KPRVRT),AA(KTYMEA),AA(KTYRMS),AA(KWTMTY),        &
     &   AA(KWTYRM),II(KNOBST),II(KNOBWT),II(KNOBTY),II(KNOBWY),        &
     &   LL(KLGPRV),RESID    ,NM        ,SIGMA    ,RATIO     ,        &
     &   NMWTD     ,MTYPE     ,JSTATS    )

! RESET THE COUNTER
        KPAIR=0
      ENDIF
! RECOVER NUCON
      NUCON=NUSAV

      IF(MTYPE.EQ.110) THEN
      KCNBST0=KCNBST0+1
!     write(6,*)' bdg increase KCNBST0 ',KCNBST0
      ELSE
      KCNBST1=KCNBST1+1
      ENDIF
       write(6,*)'                                         '
       write(6,*)' ***********  EXIT   CONGEN ************** '
       IF(KPAIR.EQ.0) THEN
       ENDIF
       write(6,*)'                                         '
      RETURN
      END
