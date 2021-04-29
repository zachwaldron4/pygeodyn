

!$TRAGEN
      SUBROUTINE TRAGEN(AA,II,LL,NSAT,NTMPB,NTMCR,NTIMES,MJDSTC,MJDSTE, &
     &    ISECT,FSECTC,FSECTE,DSECT,FSECTI,BFTRAJ,IUNTRJ,LENDTT,LWSET,  &
     &    LTRJND,                                                       &
     &    INDP,RPOLE,PXPOLE,S,NMP,NM,XDPOLE,UTDT)
!
!********1*********2*********3*********4*********5*********6*********7**
! TRAGEN           00/00/00            0000.0    PGMR - BILL EDDY
!                                      9203.02          J. McCarthy
!
!
! FUNCTION          WRITE OUT THE TRAJECTORY FILE TO
!                   THE SPECIFIED UNIT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA       I    A    ARRAY FOR REAL NUMBERS
!   II       I    A    ARRAY FOR INTEGERS
!   LL       I    A    ARRAY FOR LOGICALS
!   NSAT     I    S    NUMBER OF SATELLITES
!   NTMPB    I    S    MAXIMUM BUFFER SIZE
!   NTMCR
!   NTIMES   I    A    NUMBER OF POINTS IN EACH BUFFER (?)
!   MJDSTC   I    S    TRAJECTORY START DATE & TIME IN MJD SEC
!   MJDSTE   I    S    TRAJECTORY STOP DATE & TIME IN MJD SEC
!   ISECT
!   FSECTE   I    S    FRACTIONAL SECONDS OF STOP TIME
!   DSECT
!   FSECTI   O    A
!   BFTRAJ   O    A    ARRAY CONTAINING TRAJECTORY INFORMATION
!   IUNTRJ   I    S    UNIT CONTAINING TRAJECTORY INFO
!   LENDTT
!   LWSET
!   LTRJND
!   INDP
!   RPOLE    O    A    POLAR MOTION ROTATION MATRIX
!   PXPOLE   O    A    POLAR MOTION PARTIALS
!   XDPOLE   O    A    POLAR MOTION RATE PARTIALS
!   S        O    A    INTERPOLATION FRACTIONS
!   NMP
!   NM       I    S    NUMBER OF MEASUREMENTS IN BLOCK
!
! COMMENTS:
!
!
! *******1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER(  narrl = 30000      )
!C      PARAMETER(  NWD = 31 )
      PARAMETER(  NWD = 24 )
      PARAMETER(  ZERO = 0.0D0       )
      PARAMETER(  ONE = 1.D0         )
      PARAMETER(  THOUS  = 1000.D0   )
      PARAMETER(  X1PE6 = 1000000.D0 )
      PARAMETER(  X9PD9 = 9.D9       )
!
      COMMON/TRIND/LFTRAJ
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/COFSTL/LOFEXT(2)
!>>
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
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
      COMMON/CORL02/KLNDTT,KLWSTR,KLNDTR,KLEVMF,KLTPMS,NXCL02
      COMMON/CREFMT/REFMT(9)
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
      COMMON/CTHDOT/THDOT
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      COMMON/G2SINF/G2SVER,G2SDAT,G2SRTM,G2SCOM,TDFVER,TDFRTM,SETCDS,   &
     &              GPCKSM,GPDEG ,GPORD ,XG2SIN
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LOLOAD/LOLMD,LOLAJ,L2FOLT,LAPLOD
      COMMON/OLDADJ/NPV0OL(3,3),JPV0OL(3,3),NYZ0OL(3,3),JYZ0OL(3,3),    &
     &              NEO0OL(3,3),JEO0OL(3,3)
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/OLOADA/LXYZCO,LEOPTO
      COMMON/NEQINT/NINTOT,MEMTOT,NEQNIM,NXNEQS
      COMMON/POLESW/LDNPOL,NXPOLS
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TRAJA/FSECTR
      COMMON/TRAJI/MJDSTR,NTRAJC,MTRAJC,NXTRAJ
      COMMON/TRJREF/NORBTV,NORBFL,NCOFM,NXTREF
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
      COMMON/TIDEOP/EOPTID(2,3)
!
      DIMENSION RROT(3,3)
      DIMENSION AA(1),II(1),LL(1),NSAT(NTRAJC),NTMPB(NTRAJC),           &
     &   NTMCR(NTRAJC),NTIMES(NTRAJC),MJDSTC(NTRAJC),MJDSTE(NTRAJC),    &
     &   ISECT(NTRAJC),FSECTC(NTRAJC),FSECTE(NTRAJC),DSECT(NTRAJC),     &
     &   FSECTI(NINTIM),LENDTT(NTRAJC),LWSET(NTRAJC),FSECOT(1),         &
     &   IYMD(1),IHM(1),SEC(1),BFTRAJ(2048,NTRAJC),IUNTRJ(NTRAJC),      &
     &   JPRD(NWD),                                                     &
     &   XECF(2*narrl),XDECF(narrl)                                     &
     &   ,INDP(MINTPL,4),RPOLE(3,3,MINTPL),PXPOLE(3,2,MINTPL)           &
     &   ,NINTPL(4),NMP(mintpl,4),S(NM),XDPOLE(3,2,MINTPL)
      DIMENSION UTDT(NM,4)
      !orig -wrong DIMENSION RQUAT(1000,4)
      DOUBLE PRECISION, dimension(4, 1000 ) :: rquat
      DIMENSION MXTLOC(1)
      DIMENSION X1(3),V1(3)
      DIMENSION X2(3),V2(3)
      DIMENSION XYZCGI(3),XSATOD(3)
      DIMENSION DUM(3),DUM1(3,2,1)
      DIMENSION XENV(3,3),XYZDUM(1),XMN1(3)
      DIMENSION XECFTP(3)
      DIMENSION SUNPOS_J2000(3)
      DIMENSION SATPOS_J2000(3)
      DIMENSION SATVEL_J2000(3)
      DIMENSION SATPOS_TOR(3)
      DIMENSION SATVEL_TOR(3)
      DOUBLE PRECISION, dimension(3) ::  xecf_temp
      DOUBLE PRECISION, dimension(3) ::  vecf_temp
!
      EQUIVALENCE (XECF(narrl+1),XDECF(1))
!
      DATA KENTRY/0/
      DATA J1/1/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO I=1,6
        EOPTID(I,1)=0.D0
      ENDDO

      DO I=1,1000
      !orig  -wrong RQUAT(I,1)=0.D0
      !orig  -wrong RQUAT(I,2)=0.D0
      !orig  -wrong RQUAT(I,3)=0.D0
      !orig  -wrong RQUAT(I,4)=0.D0
      RQUAT(1,I)=0.D0
      RQUAT(2,I)=0.D0
      RQUAT(3,I)=0.D0
      RQUAT(4,I)=0.D0
      ENDDO
!
      KENTRY = KENTRY + 1
      IF( KENTRY .EQ. 1 ) THEN
         NWOIDS = 2048*NTRAJC
         DO 20 I=1,NWOIDS
            BFTRAJ(I,1) = ZERO
   20    CONTINUE
      ENDIF
!
!      write(6,*) 'TRAGEN: KENTRY ', KENTRY
!      IF( KENTRY .GT. 22 ) STOP
!      write(6,*)'TRAGEN: NTRAJC ', NTRAJC
!
      DO 4000 ICRD=1,NTRAJC
!
!      write(6,*)'TRAGEN: ICRD, LENDTT(ICRD) ', ICRD, LENDTT(ICRD)
!
      IF(LENDTT(ICRD)) GO TO 4000
      DIFSEC=DBLE(MJDSTC(ICRD)-MJDSTR)+(FSECTC(ICRD)-FSECTR)
!
!      write(6,*)'IN TRAGEN:DIFSEC, MJDSTC(ICRD), MJDSTR  ',
!     1                     DIFSEC, MJDSTC(ICRD), MJDSTR
!      write(6,*)'IN TRAGEN: FSECTC(ICRD), FSECTR  ',
!     1                      FSECTC(ICRD), FSECTR
!      write(6,*)'IN TRAGEN: TEST DIFSEC GT ZERO  '
!
      IF(DIFSEC.GT.ZERO) GO TO 4000
      NPTS=NTIMES(ICRD)
!
!      write(6,*)'IN TRAGEN: NPTS, ICRD, NTIMES(ICRD) ',
!     1                      NPTS, ICRD, NTIMES(ICRD)
!
      FSECTN=FSECTC(ICRD)+NPTS*DSECT(ICRD)
      ISECTN=INT(FSECTN)
      FSECTN=FSECTN-DBLE(ISECTN)
      MJDSTN=MJDSTC(ICRD)+NPTS*ISECT(ICRD)+ISECTN
      DIFSEC=DBLE(MJDSTE(ICRD)-MJDSTN)+(FSECTE(ICRD)-FSECTN)
      DSECS=DBLE(ISECT(ICRD))+DSECT(ICRD)
!
!      write(6,*)'IN TRAGEN: DIFSEC, DSECS,MJDSTN  ',
!     1                      DIFSEC, DSECS, MJDSTN
!      write(6,*)'IN TRAGEN: TEST DIFSEC GE ZERO  '
!
      IF(DIFSEC.GE.ZERO) GO TO 1000
      DIFSCS=DBLE(MJDSTE(ICRD)-MJDSTC(ICRD))                          &
     &            +(FSECTE(ICRD)-FSECTC(ICRD))
      NPTS=INT(DIFSCS/DSECS)
!
!      write(IOUT6,*)'IN TRAGEN: NPTS, DIFSCS, DSECS ',
!     1                          NPTS, DIFSCS, DSECS
!
      NPTS=NPTS+1
      LENDTT(ICRD)=.TRUE.
 1000 CONTINUE
!   write(6,*)'IN TRAGEN: BELOW 1000 '
      DO 2000 N=1,NPTS
      N1=N-1
      FSECTI(N)=FSECTC(ICRD)+DBLE(N1)*DSECS
 2000 END DO
!
!   INITIALIZE POINTERS AND COUNTERS FOR READING FROM ARRAYS AND WRITING
!   INTO BUFFERS
      IPRD=1
      NTMLTP=NPTS
      IPWRT=MOD(NTMCR(ICRD),NTMPB(ICRD))+1
      NTMLTB=NTMPB(ICRD)-IPWRT+1
!
!   write(IOUT6,*)'IN TRAGEN: NTMLTB,NTMPB,IPWRT ',NTMLTB,NTMPB,IPWRT
!
      NTMLBP=MIN(NTMLTP,NTMLTB)
!
!   write(IOUT6,*)' TRAGEN: NTMLBP,NTMLTP,NTMLTB ',
!  1                        NTMLBP,NTMLTP,NTMLTB
!
      IF(LWSET(ICRD)) GO TO 2010
!   ONE SATELLITE;PASS CAN BE ACCOMMODATED BY ONE CALL TO ORBIT
      ITO=0
      NNPTS=NPTS
      NTRY=1
      NNNSAT=1

      ISATID=II(KISATN+NSAT(ICRD)-1)

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
      CALL ORBSET(II(KNSAT),II(KIVSAT),LL(KLSETS),NSAT(ICRD) ,          &
     &   KNSTEPS,NEQNI,II(KSGMNT-1+IRET),                               &
     &   AA(KSGTM1-1+IRET),AA(KSGTM2-1+IRET),IRET,MJDSTC(ICRD),         &
     &   FSECTI(IPRD),FSECTI(IPRD-1+NNPTS),NNPTS,AA,II)
      LNRCOT=.FALSE.
      GO TO 2090
 2010 CONTINUE
!    WHOLE SATELLITE SET IN TRAGEN FILE FIRST TIME THRU
!    ORBIT CAN HANDLE ONLY 5 SATS AT A TIME;THERE MAY HAVE TO BE
!    MULTIPLE CALLS TO ORBIT;ADDITIONALY THE PASS MAY HAVE TO BE
!    BROKEN UP IF THERE IS NOT ENOUGH SPACE IN THE CURRENT BUFFER
      ITO=0
      NNPTS=0
!    DETERMINE STARTING AND FINISHING SATELLITES
      NSET=NSAT(ICRD)
      JSATE=1
      DO  2012 ISET=1,NSETA
      LL(KLSETS-1+ISET)=.FALSE.
      ISATID=II(KISATN+ISET-1)
      IF(ISET.GT.NSET) GO TO 2012
      JSATB=JSATE
      JSATE=JSATB+II(KNSAT+ISET-1)-1
 2012 END DO
      LL(KLSETS+NSET-1)=.TRUE.
!    CAN ONLY DO 5 SATELLITES AT A TIME
      NTRIES=(JSATE-JSATB)/5+1
      GO TO 2016
!    INITIALIZE POINTERS &COUNTERS SO CAN WRITE INTO A NEW BUFFER
 2015 CONTINUE
      ITO=0
      NNPTS=0
      IPWRT=1
      NTMLTB=NTMPB(ICRD)
      NTMLBP=MIN(NTMLTP,NTMLTB)
 2016 CONTINUE
! ADVANCE THE POINTERS WITHIN THE PASS
      ITO=ITO+NNPTS
      NNPTS=NTMLBP
! GO BACK TO THE FIRST 5 SATELLITES IN SET
      NTRY=0
      ISATB=JSATB-5
      ISATE=MIN(JSATB,JSATE-5)
!    RETURN TO SAME PASS SEGMENT WITH NEW SATELLITES
 2020 CONTINUE
      ISATB=ISATB+5
      ISATE=ISATE+5
      ISATE=MIN(ISATE,JSATE)
      NNNSAT=ISATE-ISATB+1
      NTRY=NTRY+1
      LNRCOT=.FALSE.
      IF(NTRY.GT.NTRIES) GO TO 2850
      IF(NTRY.GT.1) LNRCOT=.TRUE.
      IK=0
      DO 2030 ISAT=ISATB,ISATE
      II(KIVSAT+IK)=ISAT
      IK=IK+1
 2030 END DO
 2090 CONTINUE
!
!     write(6,*) 'TRAGEN: CALL ORBIT',NNPTS
!     write(6,*) 'TRAGEN: IPRD, ITO ', IPRD, ITO
!     write(6,*) 'TRAGEN: FSECTI ', FSECTI
!     write(6,*) 'TRAGEN: MJDSTC(ICRD), FSECTI(IPRD+ITO) ',
!    1                    MJDSTC(ICRD), FSECTI(IPRD+ITO),FSECTI(IPRD+
!    2     ITO+1)

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


!
      CALL ORBIT(MJDSTC(ICRD),FSECTI(IPRD+ITO),AA(KS),NNPTS,II(KINDH),  &
     &   II(KNMH),II(KMJDSC),AA(KFSEC),II(KMJDSV),AA(KFSECV),LL(KLSETS),&
     &   II(KIPXPF),II(KIVSAT),1,II(KNMAX),II(KNTOLD),AA(KXTN),         &
     &   AA(KXTN),.TRUE.,.FALSE.,                                       &
     &   II(KPLPTR),II(KPANEL),II(KNMOVE),                              &
     &   AA(KTPMES),LL(KLTPMS),LL(KSETDN),                              &
     &   AA(KTMOS0-1+INTOSP),AA(KTMOS-1+INTOSP),AA(KTMOSP-1+INTOSP),    &
     &   AA(KSMXOS-1+KSUMXOS),AA(KSMXOS-1+KSUMPOS),AA(KSMXOS-1+KXDDTOS),&
     &   AA(KSMXOS-1+KPDDTOS),KNSTEPS,NEQNI,II(KSGMNT-1+IRET),          &
     &   AA(KSGTM1-1+IRET),AA(KSGTM2-1+IRET),AA,II,LL)
      CALL SATUPD(MJDSTC(ICRD),FSECTI(IPRD+ITO),AA(KXTN),AA(KXTN),AA,   &
     &            NNPTS,MINTIM,.TRUE.,.FALSE.,II,0,1)
      CALL SATUPD(MJDSTC(ICRD),FSECTI(IPRD+ITO),AA(KVTN),AA(KVTN),AA,   &
     &            NNPTS,MINTIM,.FALSE.,.FALSE.,II,0,1)
      IF(LNRCOT) GO TO 2600
!
      IF(ICBDGM.EQ.3) THEN
!
!     ....changed fsecti --> fsecti(IPRD+ITO) 3/18//92 jjm
!
!!!!!!!!!!!!!!
      IF(LOLMD) THEN
        CALL OLDSET(1,0,NNPTS,AA(KSTAIN),II(KIP0OL),II(KNDOLA),         &
     &              II(KKIOLA),LOLAJ,LOLMDS,                            &
     &              JABCOF,JPXSPA,JOLPAE,JOLPAN,JOLPAV,JXLOCV,          &
     &              JABCCM,JPXSP1,JOLPXC,JOLPYC,JOLPZC,                 &
     &              JABCEO,JPXSP2,JOLPXP,JOLPYP,JOLPUP,LALOAD,ISITA,    &
     &              XENV)
      ENDIF
      IF(LEOPTO.OR.LOLMDS.OR.LXYZCO) THEN
         CALL OLTIME(MJDSTC(ICRD),FSECTI(IPRD+ITO),NNPTS,AA(KANGFC),    &
     &               AA(KSPEED),                                        &
     &               AA(KSCROL),AA(KSCROL+4*NTOLFR),II(KPTOLS),         &
     &               AA(KANGT),AA(KCONAM),AA(KTWRK1),AA(KGRDAN),        &
     &               AA(KUANG),AA(KFAMP),II(KJDN))
      ENDIF
      IF(LEOPTO) THEN
         CALL OLDEOP(MJDSTC(ICRD),FSECTI(IPRD+ITO),NNPTS,AA(JABCOF),    &
     &              AA(KANGFC),AA(KSPEED),                              &
     &              AA(JXLOCV),XYZDUM,AA(KSCROL),AA(KROTMT),.FALSE.,    &
     &              AA(JPXSPA),NPV0OL(1,J1),NPV0OL(2,J1),NPV0OL(3,J1),  &
     &              II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),        &
     &              AA(JPXSP1),NYZ0OL(1,J1),NYZ0OL(2,J1),NYZ0OL(3,J1),  &
     &              II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),        &
     &              AA(JPXSP2),NEO0OL(1,J1),NEO0OL(2,J1),NEO0OL(3,J1),  &
     &              II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,     &
     &              LEOPTO,XMN1)
      ENDIF
!!!!!!!!!!!!!!
      CALL GRHRAP(MJDSTC(ICRD),FSECTI(IPRD+ITO),.TRUE.,.TRUE.,          &
     &      AA(KEQN),AA(KSRTCH),                                        &
     &      AA(KTHETG),AA(KCOSTH),AA(KSINTH),NNPTS,AA,II,UTDT(1,1),     &
     &      .FALSE.,DUM,DUM1)
      ELSE
         CALL ROTXTR(MJDSTC(ICRD),FSECTI(IPRD+ITO),                     &
     &      .TRUE.,AA(KEQN),AA(KSRTCH),                                 &
     &      AA(KTHETG),AA(KCOSTH),AA(KSINTH),NNPTS,AA,                  &
     &      AA(KACOSW),AA(KBSINW),AA(KANGWT),AA(KWT),AA(KACOFW),        &
     &      AA(KBCOFW),II,1)
      ENDIF
       LFTRAJ=.FALSE.
!
!      write(6,*) 'TRAGEN: IPRD, ITO ', IPRD, ITO
!      write(6,*) 'TRAGEN: FSECTI ', FSECTI
!      write(6,*) 'TRAGEN: MJDSTC(ICRD), FSECTI(IPRD+ITO) ',
!     1                    MJDSTC(ICRD), FSECTI(IPRD+ITO)
!      write(6,*) 'TRAGEN: aa(kthetg) ', (aa(kthetg+iii-1),iii=1,npts)
!      write(6,*) 'TRAGEN: aa(kcosth) ', (aa(kcosth+iii-1),iii=1,npts)
!      write(6,*) 'TRAGEN: aa(ksinth) ', (aa(ksinth+iii-1),iii=1,npts)
!      write(6,*) 'TRAGEN: IPWRT ', IPWRT
!
      IF(IPWRT.GT.1) GO TO 2500
 2100 CONTINUE
!
!   write(6,*) 'TRAGEN: AFTER 2100'
!
!   START A NEW DATA BUFFER
      IPWRT=1
      NTMLTB=NTMPB(ICRD)
      NTMLBP=MIN(NTMLTP,NTMLTB)
!   WRITE OUT DATA BUFFER  COUNT
      NDBUF=NTMCR(ICRD)/NTMPB(ICRD)+1
      BFTRAJ(1,ICRD)=DBLE(NDBUF)
!   WRITE OUT BUFFER START TIME YYMMDDHHMMSS UTC FOLLOWED BY
!   FRACTIONAL REMAINING SECONDS
      CALL UTCET(.FALSE.,1,MJDSTC(ICRD),FSECTI(IPRD),FSECOT,AA(KA1UT))
      SUTC1=MJDSTC(ICRD)+FSECOT(1)
      CALL YMDHMS(MJDSTC(ICRD),FSECOT,IYMD,IHM,SEC,1)
      ISEC=SEC(1)
      SEC(1)=SEC(1)-ISEC
      BFTRAJ(2,ICRD)=DBLE(ISEC+100*IHM(1))+X1PE6*DBLE(IYMD(1))
      BFTRAJ(3,ICRD)=SEC(1)
!   WRITE OUT MJDS
      BFTRAJ(4,ICRD)=DBLE(MJDSTC(ICRD))
!   LEAVE SPACE FOR 1 WORD:NUMBER OF TRAJECT TIMES IN THIS BUFFER
 2500 CONTINUE
!
!cc   write(6,*) 'TRAGEN: AFTER 2500'
!
!   WRITE  NTMLBP ELAPSED ET TIMES (START WITH FSECTI(IPRD)
!   WRITE  NTMLBP THETAG'S (START WITH FSECTI(IPRD)
!   NTMLBP IS THE ACTUAL NUMBER OF ELAPSED ET TIMES AND THETAG'S
!   WRITTEN, BUT SINCE THE ACTUAL NUMBER OF POINTS BEING WRITTEN
!   INTO THE BUFFER IS NOT KNOWN, THE POINTER TO WRITE THE THETAG'S
!   IS OFFSET BY THE MAXIMUM SIZE OF THE BUFFER WHICH IS KNOWN.
!   HENCE, THE FIRST DATA POINT IS WRITTEN AT 5+2*NTMPB+I, WHERE
!   NTMPB IS THE MAXIMUM BUFFER SIZE AND I IS THE COUNT OF THE
!   DATA POINTS.
!
      XDNNP=DBLE(MJDSTC(ICRD))-BFTRAJ(4,ICRD)
!
!   write(6,*) 'TRAGEN: BEFORE 2510 NTMLBP ', NTMLBP
!   write(6,*) 'TRAGEN: IPWRT, ICRD, NTMPB(ICRD) ',
!  1                    IPWRT, ICRD, NTMPB(ICRD)
!
      DO 2510 I=1,NTMLBP
      IPT1=IPWRT+I+4
      IPT2=IPT1+NTMPB(ICRD)
      BFTRAJ(IPT1,ICRD)=FSECTI(IPRD+I-1)+XDNNP
      BFTRAJ(IPT2,ICRD)=AA(KTHETG+I-2+IPRD)
!
!      if( kentry .lt. 10 ) then
!      write(6,*) 'TRAGEN: I, IPT1, IPT2 , ICRD ', I, IPT1, IPT2, ICRD
!      write(6,*) 'TRAGEN: AA(KTHETG+I-2+IPRD) ', AA(KTHETG+I-2+IPRD)
!      write(6,*) 'TRAGEN: xdnnp, fsecti(iprd+i-1) ',
!     1  xdnnp, fsecti(iprd+i-1)
!      write(6,*) 'TRAGEN: BFTRAJ(IPT1,ICRD)  ', BFTRAJ(IPT1,ICRD)
!      write(6,*) 'TRAGEN: BFTRAJ(IPT2,ICRD)  ', BFTRAJ(IPT2,ICRD)
!      endif
!
 2510 END DO
 2600 CONTINUE
!
!   WRITE  NTMLBP DATA POINTS (START READING FROM FSECTI(IPRD+ITO))
!
      DO 2800 I=1,NNNSAT
      JXTN=(I-1)*MINTIM+KXTN
      JVTN=(I-1)*MINTIM+KVTN
      JPRD(1)=JXTN-2+IPRD
      CALL SUBTRK(AA(JPRD(1)+1),AA(KTHETG-1+IPRD+ITO),AA(KXY2),AA(KZT), &
     &   AA(KRT),AA(KSATLT),AA(KSATLN),AA(KSATH),NTMLBP)
!
!      write(6,*) 'TRAGEN: after subtrk AA(KTHETG-1+IPRD+ITO) ',  &
!                 (  AA(KTHETG-2+IPRD+ITO+ijjj), ijjj=1,ntmlbp)
!      write(6,*) 'TRAGEN: after subtrk AA(KSATLT) ',  &
!                 ( AA(KSATLT-1+ijjj), ijjj=1,ntmlbp)
!      write(6,*) 'TRAGEN: after subtrk AA(KSATLN) ',  &
!                 ( AA(KSATLN-1+ijjj), ijjj=1,ntmlbp)
!
      JPWRT = 5 + (IPWRT-1)* NWD + 2*NTMPB(ICRD)+                       &
     &           ((NTRY-1)*5+I-1)* NWD *NTMPB(ICRD)
!
!
!   write(6,*) 'TRAGEN: IPWRT, NWD, ICRD, NTMPB(ICRD), NTRY ',  &
!                       IPWRT, NWD, ICRD, NTMPB(ICRD), NTRY
!   write(6,*) 'TRAGEN: JPWRT ', JPWRT
!
!
      JPRD(4)=JVTN-2+IPRD
      DO 2660 J=1,2
      JPRD(J+1)=JPRD(J)+MINTIM
      JPRD(J+4)=JPRD(J+3)+MINTIM
 2660 END DO
      JPRD(7)=KSATLT-1
      JPRD(8)=KSATLN-1
      JPRD(9)=KSATH-1
!
!
! CHECK TO SEE IF NTMLBP IS EXCEEDING LIMITS
!
      IF (NTMLBP .GT. 1000) THEN
        write(IOUT6,*)'TERMINATE IN TRAGEN:NO.ELAPSED ET TIMES EXCEEDED'
        STOP
      ENDIF
!
! TO OBTAIN EARTH CENTERED   FIXED POSITION AND VELOCITY VECTOR
!
!
!
!     ....changed to fix ecf output 3/18/92 jjm
!
!      CALL ECFIXP(AA(JXTN),AA(KCOSTH),AA(KSINTH),XECF,
!     1            MINTIM,NTMLBP,NTMLBP)
!
      CALL ECFIXP(AA(JPRD(1)+1),AA(KCOSTH-1+IPRD+ITO),                  &
     &     AA(KSINTH-1+IPRD+ITO),XECF,                                  &
     &            MINTIM,NTMLBP,NTMLBP)

!!!!!!!!!!!!!!
      IF(LOLMD) THEN
        CALL OLDSET(1,0,NTMLBP,AA(KSTAIN),II(KIP0OL),II(KNDOLA),        &
     &              II(KKIOLA),LOLAJ,LOLMDS,                            &
     &              JABCOF,JPXSPA,JOLPAE,JOLPAN,JOLPAV,JXLOCV,          &
     &              JABCCM,JPXSP1,JOLPXC,JOLPYC,JOLPZC,                 &
     &              JABCEO,JPXSP2,JOLPXP,JOLPYP,JOLPUP,LALOAD,ISITA,    &
     &              XENV)
      ENDIF
      IF(LEOPTO.OR.LOLMDS.OR.LXYZCO) THEN
         CALL OLTIME(MJDSTC(ICRD),FSECTI(IPRD+ITO),NTMLBP,AA(KANGFC),   &
     &               AA(KSPEED),                                        &
     &               AA(KSCROL),AA(KSCROL+4*NTOLFR),II(KPTOLS),         &
     &               AA(KANGT),AA(KCONAM),AA(KTWRK1),AA(KGRDAN),        &
     &               AA(KUANG),AA(KFAMP),II(KJDN))
      ENDIF
      IF(LEOPTO) THEN
         CALL OLDEOP(MJDSTC(ICRD),FSECTI(IPRD+ITO),NTMLBP,AA(JABCOF),   &
     &              AA(KANGFC),AA(KSPEED),                              &
     &              AA(JXLOCV),XYZDUM,AA(KSCROL),AA(KROTMT),.FALSE.,    &
     &              AA(JPXSPA),NPV0OL(1,J1),NPV0OL(2,J1),NPV0OL(3,J1),  &
     &              II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),        &
     &              AA(JPXSP1),NYZ0OL(1,J1),NYZ0OL(2,J1),NYZ0OL(3,J1),  &
     &              II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),        &
     &              AA(JPXSP2),NEO0OL(1,J1),NEO0OL(2,J1),NEO0OL(3,J1),  &
     &              II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,     &
     &              LEOPTO,XMN1)
      ENDIF

      CALL ICRSITRF(AA(KROTMT),AA(KDPSR),AA(KXPUT),AA(KYPUT),           &
     &              AA(KA1UT),II(KINDPI),MJDSTC(ICRD),FSECTI(IPRD+ITO), &
     &              AA(KCOSTH),AA(KSINTH),NTMLBP,MINTIM,RQUAT,NORBFL,   &
     &              AA(KXDPOL),AA(KXDOTP),RROT,.TRUE.)

!orig -wrong ! DO N=1,NTMLBP
!orig -wrong ! WRITE(6,*)' RQUAT',RQUAT(N,1),RQUAT(N,2),RQUAT(N,3),RQUAT(N,4),N
!orig -wrong ! ENDDO

      !WRITE(6,'(A,1x,I9)')'tragen: NTMLBP ', NTMLBP

      !do  N=1,NTMLBP
      !    WRITE(6,'(A,1x,I9,4(1x,E15.7))')&
      !      'tragen: N, RQUAT(1:4) ', &
      !        N, RQUAT(1,N),RQUAT(2,N),RQUAT(3,N),RQUAT(4,N)
      !enddo

!
!
!     write(6,6677) ((AA(KXTN+KJJ-1+KXX),KJJ=1,3*MINTIM,MINTIM),
!    1                 KXX=1,NPTS)
!6677  FORMAT(' AA(KXTN): '/(1X,3D18.8))
!         write(6,6680) (AA(KCOSTH+KJJJ-1),KJJJ=1,NPTS)
!6680  FORMAT(' AA(KCOSTH): '/(1X,3D18.8))
!         write(6,6679) (AA(KSINTH+KJJJJ-1),KJJJJ=1,NPTS)
!6679  FORMAT(' AA(KSINTH): '/(1X,3D18.8))
!      write(6,*) 'AFTER ECFIXP:XECF ',XECF
!      write(6,*) 'tragen: xecf ', (xecf(jjecf),jjecf=1,mintim)
!
!
!     ....changed to fix ecf output 3/18/92 jjm
!
!      CALL ECFIXV(AA(JVTN),AA(KCOSTH),AA(KSINTH),XECF,XDECF,
!     1            MINTIM,NTMLBP,NTMLBP)
!
      CALL ECFIXV(AA(JPRD(4)+1),AA(KCOSTH-1+IPRD+ITO),                  &
     &      AA(KSINTH-1+IPRD+ITO),XECF,XDECF,                           &
     &            MINTIM,NTMLBP,NTMLBP)
!
!
!      write(6,*) 'tragen: xdecf ', (xdecf(jjdecf),jjdecf=1,mintim)
!
!
! SET START OF XECF AND XDECF
!
      JPRD(10)=0
      JPRD(13)=narrl
      DO 2705 J=1,2
      JPRD(J+10)=JPRD(10+(J-1))+NTMLBP
      JPRD(J+13)=JPRD((J-1)+13)+NTMLBP
 2705 END DO
!
! WRITE XECF AND XDECF TO THE TRAJECTORY BUFFER
!
!      write(6,*) 'JPRD: ',JPRD
!      write(6,*) 'NTMLBP: ',NTMLBP
!


      DO 2710 JJ=1,NTMLBP

          ! FIRST SAVE THE SATELLITE TOD COORDINATES

          XSATOD(1)=AA(JPRD(1)+JJ)
          XSATOD(2)=AA(JPRD(2)+JJ)
          XSATOD(3)=AA(JPRD(3)+JJ)

          ! IF OUTPUT IN CENTER OF FIGURE SYSTEM

          IF(NCOFM.EQ.1) THEN

              ! CONVERT CGMASS IN SBF TO TOR FRAME
              ISET1=NSAT(ICRD)-1
              CALL XCGB2I(LL(KEALQT+ISET1),AA(KXYZCG),XYZCGI,  &
                          MJDSTC(ICRD), FSECTI(IPRD+JJ-1),     &
                          II(KEASBJ+ISET1),AA(KEAQAT),         &
                          II(KEAMJS+ISET1),AA(KEAFSS+ISET1),   &
                          AA(KEAINS+ISET1),                    &
                          II(KISATN+ISET1),II(KISATC),II(KMJDCG),II,AA )

              ! CONVERT CGMASS VECTOR FROM TOR TO TOD

              CALL SATUPD(MJDSTC(ICRD),FSECTI(IPRD+JJ-1),XYZCGI,XYZCGI, &
                          AA,1,1,.TRUE.,.FALSE.,II,0,1)

              ! THE CGMASS COORDINATES SHOULD BE SUBTRACTED FROM
              ! THE SATELLITE COORDINATES

              XSATOD(1)=XSATOD(1)-XYZCGI(1)
              XSATOD(2)=XSATOD(2)-XYZCGI(2)
              XSATOD(3)=XSATOD(3)-XYZCGI(3)
          ENDIF !  NCOFM.EQ.1


          ! OUTPUT REFERENCE SYSTEM  NOT  TOD

          IF(NORBFL.NE.0) THEN

              ! TOR

              DO IJ=1,3
                  !X1(IJ)=AA(JPRD(IJ)+JJ)
                  X1(IJ)=XSATOD(IJ)
                  V1(IJ)=AA(JPRD(3+IJ)+JJ)
              ENDDO  ! IJ

              !CALL TDORTR(MJDSTC(ICRD),FSECTI(IPRD+JJ-1),X1,X1,AA,1,1,.T
              !    &           .TRUE.,II)
              !CALL TDORTR(MJDSTC(ICRD),FSECTI(IPRD+JJ-1),V1,V1,AA,1,1,.F
              !    &           .TRUE.,II)

              ! CALL SATUPD INSTEAD OF TDORTR TO DO THE TOD TO TOR TRANSF

              CALL SATUPD(MJDSTC(ICRD),FSECTI(IPRD+JJ-1),&
                          X1,X1,AA,1,1,.TRUE., .TRUE.,II,0,1)
              CALL SATUPD(MJDSTC(ICRD),FSECTI(IPRD+JJ-1),&
                          V1,V1,AA,1,1,.FALSE., .TRUE.,II,0,1)

              !----------------------------------------------------------
              ! MEAN OF 2000

              IF(NORBFL.EQ.2) THEN
                  DO IJ=1,3
                      X2(IJ)=X1(IJ)
                      V2(IJ)=V1(IJ)
                  ENDDO
                  X1(1)=X2(1)*REFMT(1)+X2(2)*REFMT(4)+X2(3)*REFMT(7)
                  X1(2)=X2(1)*REFMT(2)+X2(2)*REFMT(5)+X2(3)*REFMT(8)
                  X1(3)=X2(1)*REFMT(3)+X2(2)*REFMT(6)+X2(3)*REFMT(9)
                  V1(1)=V2(1)*REFMT(1)+V2(2)*REFMT(4)+V2(3)*REFMT(7)
                  V1(2)=V2(1)*REFMT(2)+V2(2)*REFMT(5)+V2(3)*REFMT(8)
                  V1(3)=V2(1)*REFMT(3)+V2(2)*REFMT(6)+V2(3)*REFMT(9)
              ENDIF !  NORBFL.EQ.2

              BFTRAJ(JPWRT+1,ICRD)=X1(1)
              BFTRAJ(JPWRT+2,ICRD)=X1(2)
              BFTRAJ(JPWRT+3,ICRD)=X1(3)
              BFTRAJ(JPWRT+4,ICRD)=V1(1)
              BFTRAJ(JPWRT+5,ICRD)=V1(2)
              BFTRAJ(JPWRT+6,ICRD)=V1(3)

              !----------------------------------------------------------
       ELSE

              ! TOD

!     BFTRAJ(JPWRT+1,ICRD)=AA(JPRD(1)+JJ)
!     BFTRAJ(JPWRT+2,ICRD)=AA(JPRD(2)+JJ)
!     BFTRAJ(JPWRT+3,ICRD)=AA(JPRD(3)+JJ)
      BFTRAJ(JPWRT+1,ICRD)=XSATOD(1)
      BFTRAJ(JPWRT+2,ICRD)=XSATOD(2)
      BFTRAJ(JPWRT+3,ICRD)=XSATOD(3)
      BFTRAJ(JPWRT+4,ICRD)=AA(JPRD(4)+JJ)
      BFTRAJ(JPWRT+5,ICRD)=AA(JPRD(5)+JJ)
      BFTRAJ(JPWRT+6,ICRD)=AA(JPRD(6)+JJ)
       ENDIF !  NORBFL.NE.0

       ! ECF
!-------------------------------------------------------------------------------
!
      BFTRAJ(JPWRT+10,ICRD)=XECF(JPRD(10)+JJ)
      BFTRAJ(JPWRT+11,ICRD)=XECF(JPRD(11)+JJ)
      BFTRAJ(JPWRT+12,ICRD)=XECF(JPRD(12)+JJ)
      BFTRAJ(JPWRT+13,ICRD)=XECF(JPRD(13)+JJ)
      BFTRAJ(JPWRT+14,ICRD)=XECF(JPRD(14)+JJ)
      BFTRAJ(JPWRT+15,ICRD)=XECF(JPRD(15)+JJ)
!
      BFTRAJ(JPWRT+21,ICRD)=RQUAT(JJ,1)
      BFTRAJ(JPWRT+22,ICRD)=RQUAT(JJ,2)
      BFTRAJ(JPWRT+23,ICRD)=RQUAT(JJ,3)
      BFTRAJ(JPWRT+24,ICRD)=RQUAT(JJ,4)
!
!>>>> debug
!cc      ZZLON = ATAN2( BFTRAj(JPWRT+2,ICRD), BFTRAj(JPWRT+1,ICRD) )
!cc      ZZLON = ZZLON / DEGRAD
!cc      ZZRAD = SQRT( BFTRAj(JPWRT+2,ICRD)**2 + BFTRAj(JPWRT+1,ICRD)**2
!cc      ZZLAT = ATAN2( BFTRAj(JPWRT+3,ICRD), ZZRAD )
!cc      ZZLAT = ZZLAT / DEGRAD
!
!cc      ZLON = ATAN2( BFTRAj(JPWRT+11,ICRD), BFTRAj(JPWRT+10,ICRD) )
!cc      ZLON = ZLON / DEGRAD
!cc      ZRAD = SQRT( BFTRAj(JPWRT+10,ICRD)**2 + BFTRAj(JPWRT+11,ICRD)**
!cc      ZLAT = ATAN2( BFTRAj(JPWRT+12,ICRD), ZRAD )
!cc      ZLAT = ZLAT / DEGRAD
!
!cc      ZLON = ATAN2( XECF(JPRD(11)+JJ), XECF(JPRD(10)+JJ) )
!cc      ZLON = ZLON / DEGRAD
!cc      ZRAD = SQRT( XECF(JPRD(11)+JJ)**2 + XECF(JPRD(10)+JJ)**2 )
!cc      ZLAT = ATAN2( XECF(JPRD(12)+JJ), ZRAD )
!cc      ZLAT = ZLAT / DEGRAD
!
!cc      write(6,*) 'TRAGEN: ZLAT,xlat ', ZLAT, bftraj(jpwrt+7,icrd)
!cc      write(6,*) 'TRAGEN: ZZLAT ', ZZLAT
!cc      write(6,*) 'TRAGEN: ZLON, xlon ', ZLON, bftraj(jpwrt+8,icrd)
!cc      write(6,*) 'TRAGEN: ZZLON ', ZZLON
!cc      write(6,*) 'TRAGEN: ZLAT, ZLON ', ZLAT, ZLON
!cc      write(6,fmt='(1x,a/1x,3d24.16/1x,3d24.16)')
!cc     1   'TRAGEN:BFTRAJ(JPWRT+10-15,ICRD) ',
!cc     1    ( BFTRAJ(JPWRT+jjxxx,ICRD), jjxxx=10,15)
!cc      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+10,ICRD) ',BFTRAJ(JPWRT+10,ICRD
!cc      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+11,ICRD) ',BFTRAJ(JPWRT+11,ICRD
!cc      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+12,ICRD) ',BFTRAJ(JPWRT+12,ICRD
!cc      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+13,ICRD) ',BFTRAJ(JPWRT+13,ICRD
!cc      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+14,ICRD) ',BFTRAJ(JPWRT+14,ICRD
!cc      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+15,ICRD) ',BFTRAJ(JPWRT+15,ICRD
!<<<<< debug
!
!
!     ....call inpole to get polar motion
!
!
!      write(6,*) 'tragen: before bufear icrd ,ntmlbp ', icrd ,ntmlbp
!      write(6,*) 'tragen: before bufear MJDStc(icrd),fsecti ', &
!                          MJDStc(icrd),(FSECti(jjj),jjj=icrd, &
!                                 icrd+ntmlbp-1)
!
       J1 = 1
       call bufear(mjdstc(icrd), fsecti(iprd),                          &
                   mjdstc(icrd), fsecti(iprd+ntmlbp-1),                 &
                   1, mjdr, fsecr, lrecal, aa,ii)

!>>>>> debug
!      write(6,*) 'tragen: after bufear MJDr ,FSECr,lrecal ', MJDr ,FSECr,lrecal
!      write(6,*) 'tragen: AA(KA1UT),AA(KDPSR),AA(KXPUT),AA(KYPUT) ',&
!                          AA(KA1UT),AA(KDPSR),AA(KXPUT),AA(KYPUT)
!      write(6,*) 'tragen: ldnpol ', ldnpol
!      write(6,*) 'tragen: MJDSN,FSECN,NM,XMN ', &
!                          MJDSN,FSECN,NM,XMN
!      write(6,*) 'tragen: II(KINDPI),NINTPL(J1),NMP(1,J1) ', &
!                          II(KINDPI),NINTPL(J1),NMP(1,J1)
!      write(6,*) 'tragen: INDP(1,J1),PXPOLE,RPOLE,S ', &
!                          INDP(1,J1),PXPOLE,RPOLE,S
!<<<<< debug

!!!!!!!!!!!!!!
      IF(LOLMD) THEN
        CALL OLDSET(1,0,1,AA(KSTAIN),II(KIP0OL),II(KNDOLA),             &
     &              II(KKIOLA),LOLAJ,LOLMDS,                            &
     &              JABCOF,JPXSPA,JOLPAE,JOLPAN,JOLPAV,JXLOCV,          &
     &              JABCCM,JPXSP1,JOLPXC,JOLPYC,JOLPZC,                 &
     &              JABCEO,JPXSP2,JOLPXP,JOLPYP,JOLPUP,LALOAD,ISITA,    &
     &              XENV)
      ENDIF
      IF(LEOPTO.OR.LOLMDS.OR.LXYZCO) THEN
         CALL OLTIME(MJDSTC(ICRD),FSECTI(IPRD+ITO+jj-1),1,AA(KANGFC),   &
     &               AA(KSPEED),                                        &
     &               AA(KSCROL),AA(KSCROL+4*NTOLFR),II(KPTOLS),         &
     &               AA(KANGT),AA(KCONAM),AA(KTWRK1),AA(KGRDAN),        &
     &               AA(KUANG),AA(KFAMP),II(KJDN))
      ENDIF
      IF(LEOPTO) THEN
         CALL OLDEOP(MJDSTC(ICRD),FSECTI(IPRD+ITO+jj-1),1,AA(JABCOF),   &
     &              AA(KANGFC),AA(KSPEED),                              &
     &              AA(JXLOCV),XYZDUM,AA(KSCROL),AA(KROTMT),.FALSE.,    &
     &              AA(JPXSPA),NPV0OL(1,J1),NPV0OL(2,J1),NPV0OL(3,J1),  &
     &              II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),        &
     &              AA(JPXSP1),NYZ0OL(1,J1),NYZ0OL(2,J1),NYZ0OL(3,J1),  &
     &              II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),        &
     &              AA(JPXSP2),NEO0OL(1,J1),NEO0OL(2,J1),NEO0OL(3,J1),  &
     &              II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,     &
     &              LEOPTO,XMN1)
      ENDIF
!!!!!!!!!!!!!!
      CALL INPOLP(AA(KA1UT),AA(KDPSR),AA(KXPUT),AA(KYPUT),              &
     &   .false.,LDNPOL,.false.,                                        &
     &   mjdstc(icrd), fsecti(iprd+ito+jj-1),                           &
     &    1,XMN,II(KINDPI),NINTPL(J1),NMP(1,J1),                        &
     &   INDP(1,J1),PXPOLE,RPOLE,S,XDPOLE,AA(KXDOTP))
!
!      write(6,*) 'tragen aft inp: II(KINDPI),NINTPL(J1),NMP(1,J1) ', &
!                                  II(KINDPI),NINTPL(J1),NMP(1,J1)
!      write(6,*) 'tragen aft inp: INDP(1,J1),PXPOLE,RPOLE,S ', &
!                                  INDP(1,J1),PXPOLE,RPOLE,S
!
!     ....rpole(3,1,jd) = x  rpole(2,3,jd) = y
!
!   ADDITIONAL GEODYN OUTPUT WITH EVERY TRAJECTORY RECORD FOR
!   THE TOPEX POE
!
!     ....16 is polar motion x,   17 is polar motion y
!     ....   rpole(3,1,jd) = x       rpole(2,3,jd) = y
!
      XPOLE = rpole(3,1,1)
      YPOLE = rpole(2,3,1)
      XECFTP(1)=XECF(JPRD(10)+JJ)+XPOLE*XECF(JPRD(12)+JJ)
      XECFTP(2)=XECF(JPRD(11)+JJ)-YPOLE*XECF(JPRD(12)+JJ)
      XECFTP(3)=XECF(JPRD(12)+JJ)-XPOLE*XECF(JPRD(10)+JJ)               &
     &                           +YPOLE*XECF(JPRD(11)+JJ)
      CALL SUBTK2(XECFTP,AA(KXY2),AA(KZT),                              &
     &   AA(KRT),AA(KSATLT),AA(KSATLN),AA(KSATH),1)
      BFTRAJ(JPWRT+7,ICRD)=AA(KSATLT)
      BFTRAJ(JPWRT+8,ICRD)=AA(KSATLN)
      BFTRAJ(JPWRT+9,ICRD)=AA(KSATH)

      xpole = rpole(3,1,1) / secrad  * thous
      ypole = rpole(2,3,1) / secrad  * thous
!
      BFTRAJ(JPWRT+16,ICRD)= xpole
      BFTRAJ(JPWRT+17,ICRD)= ypole
!
!      if( kentry .lt. 10 ) then
!         write(6,*) 'tragen: x= rpole(3,1,1) = ', rpole(3,1,1)
!         write(6,*) 'tragen: y= rpole(2,3,1) = ', rpole(2,3,1)
!         write(6,*) 'tragen: xpole = ', xpole
!         write(6,*) 'tragen: ypole = ', ypole
!      endif



!-------------------------------------------------------------------------------

!      this section computes ECF with polar motion

!!       xecf_temp(1) = xecf(JPRD(10)+jj)
!!       xecf_temp(2) = xecf(JPRD(11)+jj)
!!       xecf_temp(3) = xecf(JPRD(12)+jj)
!!       vecf_temp(1) = xecf(JPRD(13)+jj)
!!       vecf_temp(2) = xecf(JPRD(14)+jj)
!!       vecf_temp(3) = xecf(JPRD(15)+jj)

!     RPOLE(1,1,JD)=ONE
!     RPOLE(2,1,JD)=X*Y
!     RPOLE(3,1,JD)=X
!     RPOLE(1,2,JD)=ZERO
!     RPOLE(2,2,JD)=ONE
!     RPOLE(3,2,JD)=-Y
!     RPOLE(1,3,JD)=-X
!     RPOLE(2,3,JD)=Y
!     RPOLE(3,3,JD)=ONE

!!      write(6,'(/A,3(1x,E24.16))') 'tragen: rpole(1:3,1,1) ', &
!!                                            rpole(1:3,1,1)
!!      write(6,'(A,3(1x,E24.16))') 'tragen:  rpole(1:3,2,1) ', &
!!                                            rpole(1:3,2,1)
!!      write(6,'(A,3(1x,E24.16))') 'tragen:  rpole(1:3,3,1) ', &
!!                                            rpole(1:3,3,1)


!!      write(6,'(/A,3(1x,E24.16))') &
!!       'tragen: xecf (no polar motion) ', &
!!       xecf(JPRD(10)+jj), xecf(JPRD(11)+jj), xecf(JPRD(12)+jj)
!!      write(6,'(A,3(1x,E24.16))')&
!!       'tragen: vecf (no polar motion) ', &
!!       xecf(JPRD(13)+jj), xecf(JPRD(14)+jj), xecf(JPRD(15)+jj)
!!
!!      xecf_temp(1) = rpole(1,1,1) * XECF(JPRD(10)+JJ) +  &
!!                     rpole(2,1,1) * XECF(JPRD(11)+JJ) +  &
!!                     rpole(3,1,1) * XECF(JPRD(12)+JJ)
!!
!!      xecf_temp(2) = rpole(1,2,1) * XECF(JPRD(10)+JJ) +  &
!!                     rpole(2,2,1) * XECF(JPRD(11)+JJ) +  &
!!                     rpole(3,2,1) * XECF(JPRD(12)+JJ)
!!
!!      xecf_temp(3) = rpole(1,3,1) * XECF(JPRD(10)+JJ) +  &
!!                     rpole(2,3,1) * XECF(JPRD(11)+JJ) +  &
!!                     rpole(3,3,1) * XECF(JPRD(12)+JJ)
!!
!!
!!      vecf_temp(1) = rpole(1,1,1) * XECF(JPRD(13)+JJ) +  &
!!                     rpole(2,1,1) * XECF(JPRD(14)+JJ) +  &
!!                     rpole(3,1,1) * XECF(JPRD(15)+JJ)
!!
!!      vecf_temp(2) = rpole(1,2,1) * XECF(JPRD(13)+JJ) +  &
!!                     rpole(2,2,1) * XECF(JPRD(14)+JJ) +  &
!!                     rpole(3,2,1) * XECF(JPRD(15)+JJ)
!!
!!      vecf_temp(3) = rpole(1,3,1) * XECF(JPRD(13)+JJ) +  &
!!                     rpole(2,3,1) * XECF(JPRD(14)+JJ) +  &
!!                     rpole(3,3,1) * XECF(JPRD(15)+JJ)
!!
!!      write(6,'(/A,3(1x,E24.16))')  &
!!            'tragen: xecf_temp(1:3) (with polar motion) ', &
!!                     xecf_temp(1:3)
!!      write(6,'( A,3(1x,E24.16)/)') &
!!            'tragen: vecf_temp(1:3) (with polar motion) ', &
!!                     vecf_temp(1:3)
!!
!!!      ECF with polar motion
!!
!!       BFTRAJ(JPWRT+10,ICRD)= xecf_temp(1)
!!       BFTRAJ(JPWRT+11,ICRD)= xecf_temp(2)
!!       BFTRAJ(JPWRT+12,ICRD)= xecf_temp(3)
!!       BFTRAJ(JPWRT+13,ICRD)= vecf_temp(1)
!!       BFTRAJ(JPWRT+14,ICRD)= vecf_temp(2)
!!       BFTRAJ(JPWRT+15,ICRD)= vecf_temp(3)


!-------------------------------------------------------------------------------

       ! ECF without polar motion

       BFTRAJ(JPWRT+10,ICRD)=XECF(JPRD(10)+JJ)
       BFTRAJ(JPWRT+11,ICRD)=XECF(JPRD(11)+JJ)
       BFTRAJ(JPWRT+12,ICRD)=XECF(JPRD(12)+JJ)
       BFTRAJ(JPWRT+13,ICRD)=XECF(JPRD(13)+JJ)
       BFTRAJ(JPWRT+14,ICRD)=XECF(JPRD(14)+JJ)
       BFTRAJ(JPWRT+15,ICRD)=XECF(JPRD(15)+JJ)

!-------------------------------------------------------------------------------

       !orig BFTRAJ(JPWRT+21,ICRD)=RQUAT(JJ,1)
       !orig BFTRAJ(JPWRT+22,ICRD)=RQUAT(JJ,2)
       !orig BFTRAJ(JPWRT+23,ICRD)=RQUAT(JJ,3)
       !orig BFTRAJ(JPWRT+24,ICRD)=RQUAT(JJ,4)

       BFTRAJ(JPWRT+21,ICRD)=RQUAT(1,JJ)
       BFTRAJ(JPWRT+22,ICRD)=RQUAT(2,JJ)
       BFTRAJ(JPWRT+23,ICRD)=RQUAT(3,JJ)
       BFTRAJ(JPWRT+24,ICRD)=RQUAT(4,JJ)


!
!>>>> debug
!      ZZLON = ATAN2( BFTRAj(JPWRT+2,ICRD), BFTRAj(JPWRT+1,ICRD) )
!      ZZLON = ZZLON / DEGRAD
!      ZZRAD = SQRT( BFTRAj(JPWRT+2,ICRD)**2 + BFTRAj(JPWRT+1,ICRD)**2
!      ZZLAT = ATAN2( BFTRAj(JPWRT+3,ICRD), ZZRAD )
!      ZZLAT = ZZLAT / DEGRAD
!
!      ZLON = ATAN2( BFTRAj(JPWRT+11,ICRD), BFTRAj(JPWRT+10,ICRD) )
!      ZLON = ZLON / DEGRAD
!      ZRAD = SQRT( BFTRAj(JPWRT+10,ICRD)**2 + BFTRAj(JPWRT+11,ICRD)**
!      ZLAT = ATAN2( BFTRAj(JPWRT+12,ICRD), ZRAD )
!      ZLAT = ZLAT / DEGRAD
!
!      ZLON = ATAN2( XECF(JPRD(11)+JJ), XECF(JPRD(10)+JJ) )
!      ZLON = ZLON / DEGRAD
!      ZRAD = SQRT( XECF(JPRD(11)+JJ)**2 + XECF(JPRD(10)+JJ)**2 )
!      ZLAT = ATAN2( XECF(JPRD(12)+JJ), ZRAD )
!      ZLAT = ZLAT / DEGRAD
!
!      write(6,*) 'TRAGEN: ZZLAT, ZLAT, xlat ', &
!                          ZZLAT, ZLAT, bftraj(jpwrt+7,icrd)
!      write(6,*) 'TRAGEN: ZZLON, ZLON, xlon ', &
!                          ZZLON, ZLON, bftraj(jpwrt+8,icrd)
!      write(6,*) 'TRAGEN: ZLAT, ZLON ', ZLAT, ZLON
!      write(6,fmt='(1x,a/1x,3d24.16/1x,3d24.16)') &
!                 'TRAGEN:BFTRAJ(JPWRT+10-15,ICRD) ', &
!                       ( BFTRAJ(JPWRT+jjxxx,ICRD), jjxxx=10,15)
!      write(6,*) 'TRAGEN:BFTRAJ(JPWRT+10:JPWRT+15,ICRD) ', &
!                         BFTRAJ(JPWRT+10:JPWRT+15,ICRD
!<<<<< debug
!
!-------------------------------------------------------------------------------
!
      ! COMPUTE ORBIT ANGLES
      CALL PLANPO(MJDSTC(ICRD), FSECTI(IPRD+JJ-1), .FALSE., .FALSE.)
      SUNPOS_J2000(1:3) = BDSTAT(1:3,8)
      SATPOS_TOR(1) = AA(JXTN)
      SATPOS_TOR(2) = AA(JXTN+MINTIM)
      SATPOS_TOR(3) = AA(JXTN+MINTIM*2)
      SATVEL_TOR(1) = AA(JVTN)
      SATVEL_TOR(2) = AA(JVTN+MINTIM)
      SATVEL_TOR(3) = AA(JVTN+MINTIM*2)
      SATPOS_J2000(1) = REFMT(1)*SATPOS_TOR(1) + REFMT(4)*SATPOS_TOR(2) &
                      + REFMT(7)*SATPOS_TOR(3)
      SATPOS_J2000(2) = REFMT(2)*SATPOS_TOR(1) + REFMT(5)*SATPOS_TOR(2) &
                      + REFMT(8)*SATPOS_TOR(3)
      SATPOS_J2000(3) = REFMT(3)*SATPOS_TOR(1) + REFMT(6)*SATPOS_TOR(2) &
                      + REFMT(9)*SATPOS_TOR(3)
      SATVEL_J2000(1) = REFMT(1)*SATVEL_TOR(1) + REFMT(4)*SATVEL_TOR(2) &
                      + REFMT(7)*SATVEL_TOR(3)
      SATVEL_J2000(2) = REFMT(2)*SATVEL_TOR(1) + REFMT(5)*SATVEL_TOR(2) &
                      + REFMT(8)*SATVEL_TOR(3)
      SATVEL_J2000(3) = REFMT(3)*SATVEL_TOR(1) + REFMT(6)*SATVEL_TOR(2) &
                      + REFMT(9)*SATVEL_TOR(3)
      call ORBANG(SATPOS_J2000, SATVEL_J2000, SUNPOS_J2000, &
                    GPS_BETAP, GPS_YAWANG, GPS_SOMEGA)

      BFTRAJ(JPWRT+18,ICRD)= GPS_BETAP ! beta prime angle
      BFTRAJ(JPWRT+19,ICRD)= GPS_YAWANG ! yaw angle
      BFTRAJ(JPWRT+20,ICRD)= GPS_SOMEGA ! orbit angle
!
! THE FOLLOWING STILL NEED TO BE FILLED-THEY ARE PLACE HOLDERS
!
      !!!!   BFTRAJ(JPWRT+21,ICRD)=12.0  ! quat(1)
      !!!!   BFTRAJ(JPWRT+22,ICRD)=13.0  ! quat(2)
      !!!!   BFTRAJ(JPWRT+23,ICRD)=14.0  ! quat(3)
      !!!!   BFTRAJ(JPWRT+24,ICRD)=15.0  ! quat(4)

!!!!orig -wrong !      BFTRAJ(JPWRT+21,ICRD)=12.0
!!!!orig -wrong !      BFTRAJ(JPWRT+22,ICRD)=13.0
!!!!orig -wrong !      BFTRAJ(JPWRT+23,ICRD)=14.0
!!!!orig -wrong !      BFTRAJ(JPWRT+24,ICRD)=15.0

      BFTRAJ(JPWRT+25,ICRD)=16.0
      BFTRAJ(JPWRT+26,ICRD)=17.0
      BFTRAJ(JPWRT+27,ICRD)=18.0
      BFTRAJ(JPWRT+28,ICRD)=19.0
      BFTRAJ(JPWRT+29,ICRD)=20.0
      BFTRAJ(JPWRT+30,ICRD)=21.0
      BFTRAJ(JPWRT+31,ICRD)=22.0
!>>>
      JPWRT=JPWRT+NWD
!<<<



 2710 END DO



!
!   ....END NEW STUFF
!
 2800 END DO
!
!   write(6,*) '2800 WHAT IS LWSET(ICRD)?  ',LWSET
!
      IF(LWSET(ICRD)) GO TO 2020
 2850 CONTINUE
!   write(6,*) 'AFTER 2850  '
!
!     ADVANCE COUNTERS AND POINTERS
!
      NTMCR(ICRD)=NTMCR(ICRD)+NTMLBP
      IPRD=IPRD+NTMLBP
      NTMLTP=NTMLTP-NTMLBP
      IPWRT=IPWRT+NTMLBP
      NTMLTB=NTMLTB-NTMLBP
!
!   write(6,*) 'IN TRAGEN: 2850  NTMCR(ICRD), NTMLBP ',
!  1                               NTMCR(ICRD), NTMLBP
!   write(6,*) 'IN TRAGEN: 2850  IPRD ', IPRD
!   write(6,*) 'IN TRAGEN: 2850  NTMLTP ', NTMLTP
!   write(6,*) 'IN TRAGEN: 2850  IPWRT ', IPWRT
!   write(6,*) 'IN TRAGEN: 2850  NTMLTB ', NTMLTB
!
!
!   AT THIS PT EITHER AT END OF BUFFER,END OF PASS OR BOTH
!   DECIDE WHETHER TO CLOSE BUFFER
!
      IF(NTMLTB.GT.0.AND..NOT.LENDTT(ICRD)) GO TO 2890
!
!   CLOSE THE BUFFER
!   WRITE NUMBER OF TIMES IN THIS BUFFER
!
      NTMTB=MOD(NTMCR(ICRD)-1,NTMPB(ICRD))+1
      BFTRAJ(5,ICRD)=DBLE(NTMTB)
!
!  COMPUTE THE UTC TIME AT THE END OF THE TIME SPAN OF THE BUFFER
!
      MJDSEC=BFTRAJ(4,ICRD)+BFTRAJ(IPT1,ICRD)
      RSECIN=BFTRAJ(4,ICRD)+BFTRAJ(IPT1,ICRD)-MJDSEC
      CALL UTCET(.FALSE.,1,MJDSEC,RSECIN,RSECOT,AA(KA1UT))
      SUTC2=MJDSEC+RSECOT
!
!   CHECK IF LEAP SECOND HAS OCCURRED IN THIS DATA BUFFER
!
      TEST=SUTC2-SUTC1-DSECS*(BFTRAJ(5,ICRD)-1.D0)
      IF (ABS(TEST).GT.0.998D0) THEN
        BFTRAJ(1,ICRD)=BFTRAJ(1,ICRD)+0.5D0
      ENDIF
!
!   WRITE THE BUFFER
!
      IUNIT=IUNTRJ(ICRD)
!
!>>> debug
!      write(6,*) 'BFTRAJ: ', (BFTRAJ(IJJJ,ICRD),IJJJ=1,NTMTB)
!      write(6,*) 'TRAGEN:  data buffer'
!      write(6,6678) (IJJJ, (BFTRAJ(IJJJ+IKKK,ICRD),IKKK=0,3),
!     1                        IJJJ=1,2048,4)
 6678 FORMAT(' BFTRAJ: '/(1X,I4,1X,4D18.8))
!      write(6,*) 'TRAGEN:  interpreted buffer'
!      mtimbf = ntmpb(icrd)
!      mtmlbp = bftraj(5,icrd) + 0.1d0
!      write(6,*) 'TRAGEN:  NTIMBF ', NTIMBF
!      write(6,*) 'TRAGEN:  NTMLBP ', NTMLBP
!      write(6,*) 'TRAGEN:  mtimbf ', mtimbf
!      write(6,*) 'TRAGEN:  mtmlbp ', mtmlbp
!      write(6,*) 'TRAGEN:  ntmpb(icrd) ', ntmpb(icrd)
!      write(6,*) 'TRAGEN:  NSAT ', NSAT
!      write(6,*) 'TRAGEN:  NWD ', NWD
!      write(6,*) 'TRAGEN:  first 5 words of buffer'
!      write(6,6879) (IJJJ, BFTRAJ(IJJJ,ICRD),IJJJ=1,5)
 6879 FORMAT(1X,I4,1X,D18.8)
!         write(6,*) 'TRAGEN:  trajectory times of buffer'
!         write(6,6879) (IJJJ, BFTRAJ(IJJJ,ICRD),IJJJ=6,mtmlbp+5)
!         write(6,*) 'TRAGEN:  RA of Greenwich '
!         ijk1 = mtimbf+6
!         ijk2 = mtmlbp+ mtimbf+5
!         write(6,6879) (IJJJ, BFTRAJ(IJJJ,ICRD),IJJJ=ijk1,ijk2)
!
!         do 6555 ijjj = 1, NSAT(ICRD)
!         write(6,*) 'TRAGEN:  data packet for satellite ', ijjj
!         ijk1 = (ijjj+1)*mtimbf+6 + (ijjj-1)* NWD
!         ijk2 = (ijjj+1)*mtimbf+5 +  ijjj   * NWD
!         write(6,6879) (IJxJ, BFTRAJ(IJxJ,ICRD),IJxJ=ijk1,ijk2)
 6555    continue
!<<< debug
!

!          mjds from default reference time
           BFTRAJ(4,ICRD)= BFTRAJ(4,ICRD) + REPDIF

         WRITE(IUNIT) BFTRAJ(1:2048,ICRD)

 2890 CONTINUE
!
!   DECIDE WHETHER TO WRITE INTO ANOTHER BUFFER THIS PASS
!   IF DATA REMAINS IN PASS IT WILL GO INTO NEW BUFFER
!
      IF(NTMLTP.GT.0.AND.LWSET(ICRD)) GO TO 2015
      IF(NTMLTP.GT.0) GO TO 2100
      MJDSTC(ICRD)=MJDSTN
      FSECTC(ICRD)=FSECTN
      IF(.NOT.LENDTT(ICRD)) GO TO 4000
!
! WRITE SENTINEL RECORD
!
      BFTRAJ(1,ICRD)=X9PD9
      BFTRAJ(2,ICRD)=NDBUF
      BFTRAJ(3,ICRD)=G2SRTM
      BFTRAJ(4,ICRD)=G2SVER
      BFTRAJ(5,ICRD)=G2EVER
      DO 3750 K = 6,2048
         BFTRAJ(K,ICRD) = ZERO
 3750 END DO

          WRITE(IUNIT) BFTRAJ(1:2048,ICRD)

 4000 END DO
!
!   write(6,*) 'TRAGEN: BELOW 4000'
!
      MXTLOC = MAXLOC(MJDSTC)
      LTRAJ=.NOT.LENDTT(MXTLOC(1))
      MJDSTR=MJDSTC(MXTLOC(1))
      FSECTR=FSECTC(MXTLOC(1))

      IF(NTRAJC.LT.2) GO TO 6000

      DO 5000 ICRD=1,NTRAJC
      IF(MXTLOC(1).EQ.ICRD) GOTO 5000
      IF(LENDTT(ICRD)) GOTO 5000
      LTRAJ=LTRAJ.OR..NOT.LENDTT(ICRD)
      DIFSEC=DBLE(MJDSTC(ICRD)-MJDSTR)+(FSECTC(ICRD)-FSECTR)
      IF(DIFSEC.GE.ZERO) GO TO 5000
      MJDSTR=MJDSTC(ICRD)
      FSECTR=FSECTC(ICRD)
 5000 END DO
 6000 CONTINUE
!
!   write(6,*) 'TRAGEN: BELOW 6000 RETURN'
!
      LTRJND=.NOT.LTRAJ
      RETURN
      END
