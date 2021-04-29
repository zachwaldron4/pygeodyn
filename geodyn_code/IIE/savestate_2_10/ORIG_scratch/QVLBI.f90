!QVLBI
      SUBROUTINE QVLBI(AA,II,LL,INDSTA,INPSTA,LELEVS,PMPA,STAINF,    &
     &                   QUAINF, VLB_STR,SIGSP,NBSPLN,NSSPLN,VLBTOT )
! ************************************************************************
! *                                                                      *
! *   Function:        Controls the processing of vlbi observations      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES.                *
! *  II      I/O   A    DYNAMIC ARRAY FOR INTEGER VARIABLES              *
! *  INDSTA   I    A    FOR A GIVEN INTERNAL STA NO (1,2,3) INDSTA       *
! *                     GIVES THE ACTUAL LOCATION IN THE STATION         *
! *                     RELATED ARRAYS FOR THIS STATION.                 *
! *                     (EG.   STA NO=ISTANO(INDSTA(1,2,OR 3))           *
! *  INPSTA   I    A    POINTER ARRAY THAT RELATES THE INTERNAL STATION  *
! *                     NUMBER (1,2,3) TO THE STATIONS DEFINED IN THE    *
! *                     DATA HEADER RECORDS (N=1,2,3). (EG.              *
! *                     ISTAP=INPSTA(1) WOULD TELL WHICH OF THE THREE    *
! *                     POSSIBLE STATIONS DEFINED IN THE DATA HEADER     *
! *                     RECORDS PERTAINED TO INTERNAL STATION NO. 1)     *
! *  LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL VARIABLES              *
! *                                                                      *
! * STAINF   I    A    STATION INFORMATION ARRAY:                        *
! *                     1  LATITUDE    2  COS(LATITUDE)   3  SIN(LAT.)   *
! *                     4  LONGITUDE   5  COS(LONGITUDE)  6  SIN(LONG.)  *
! *                     7  SPHEROID HT 8  ANTENNA MT TYPE 9  ANTENNA DISP*
! *                     10 WAVLN REC   11 WAVLN TRM       12 ELEV CUTOFF *
! *                     13 PLATE NO.   14 OCN LOAD SITE NO               *
! *                                                                      *
! *  ### 23-MAR-1990    QVLBI     v2.0 (c) L. Tsaoussi  14-OCT-2004 ###  *
! *  ###                               (c) L. Petrov                ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ONE=1.D0, TWO=2.D0 )
      INCLUDE 'COMMON_DECL.inc'
      COMMON/LFVLBI/L1STV
      COMMON/SAV_VLB/VARC(10000),VGLB(10000),EST_VEC(10000)
      COMMON/CTHDOT/THDOT
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CBLOKV/QUANO(3),EHAT(3),DEDA(3),DEDD(3),EFLG
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COBREL/KDELCT(6,8)
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
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/CSIML/LSIMTI
! /CSTA  / STATION GEODETIC INFORMATION USED IN MEASUREMENT CORRECTIONS
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/EMAT  /EMTNUM,EMTPRT,EMTCNT,VMATRT,VMATS,VMATFR,FSCVMA,    &
     &              XEMAT
      COMMON/IORCHB/ IORM,IORJ,IORSA,IORSUN,IOREM,IORMO,IORCB,IORTB,    &
     &               IGRP,ICHBOG(2,14),IORSCB(988),NXORCH
      COMMON/NETFLG/LNNT_CNS,LNNR_CNS,LNNR_SRC, &
     &  LNNT_CNS_USE(100),LNNR_CNS_USE(100),LNNR_SRC_USE(100)
      COMMON/NETSIG/DNNT_STA_SIGMA,DNNR_STA_SIGMA,DNNR_SRC_SIGMA, &
     &  DNNT_STA_RH(3),DNNR_STA_RH(3),DNNR_SRC_RH(3) ,            &
     &  DRCOO_TRS(3,100)
      COMMON/NETCON/TRANET(3,2),ROTNET(3,2),ROTQUA(3,2),XNETC
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
      COMMON/NUTHLD /NUTLST
      COMMON/NUTINF/NNUT,NNUTAA,NXNUTA
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
!     COMMON/RAMPS/
      COMMON/SETDI /NSETDP,NFRSED,NFRSDI,NFRSLN,NXSETI
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/SPBIAS/NCTSTA,NXSPB
      COMMON/VLBI/NQUA,NADJQ,NADJQV,NQUAB,IYMDV,NVOPT,NXVLBI
      COMMON/VLBTM/VLMJD1,VLSEC1,VLMJD2,VLSEC2
      COMMON/CBIASI/IEBIAS(4),IMBIAS(2),IBSCAL(1)  ,ITBIAS(1),          &
     &              IBTROP(3),IBIONO(3),KLKSTA(4,2),KLKSAT(4,2),        &
     &              KLKSTS(2,2),ITRPZE(2,2),ITRPGR(2,4),IBSBIA(1)
      COMMON/ SPLINC/ INDSP,INDTSP,INDTNO,INDTEA

!
      DIMENSION AA(*),II(*),LL(*),INDSTA(3),INPSTA(3),JXM(3), &
     &          QUAINF(12,NQUAB),JDELCT(3),STAINF(NSTAIN,1), &
     &          STANFO(25),QUANFO(12),NINTPL(3),LELEVS(3,4)
      DIMENSION TIMVEL(2)
      DIMENSION DUM(3),DUM2(1,3),RROT(3,3)
      DIMENSION VLB_STR(NVOPT)
      CHARACTER VLB_STR * 64
      CHARACTER STRING*15
      DIMENSION UEN_G1(3,3),REN_G1(3,3)
      DIMENSION UEN_G2(3,3),REN_G2(3,3)
      DIMENSION PLANET(1,3,11),PLANV(1,3,11)
      DIMENSION S_CRS(3,NQUAB)
      DIMENSION LCLKOF(2),LCLKRA(2),LCLKAC(2)
      DIMENSION  MJD_START(2),TAI_START(2)
      DIMENSION MOUNT_TYP(2),ANT_DISP(2)
      DIMENSION NODE_SEC(100),MJD_EPC(100),SEC_EPC(100)
      DIMENSION NODE_SEC_TRP(100),MJD_EPC_TRP(100),SEC_EPC_TRP(100)
      DIMENSION NODE_SEC_GRN(100),MJD_EPC_GRN(100),SEC_EPC_GRN(100)
      DIMENSION NODE_SEC_GRE(100),MJD_EPC_GRE(100),SEC_EPC_GRE(100)
      DIMENSION NBSPLN(5,NCTSTA),NSSPLN(2,NCTSTA),SIGSP(6,NCTSTA)
      DIMENSION LTHERD(2)
      CHARACTER STA_NAM(2)*8
!
      EQUIVALENCE (JXM1,JXM(1)),(JXM2,JXM(2))
      EQUIVALENCE (STANFO(1),RLAT)
      INTEGER  L_STA, L_SOU, M_STA, M_SOU, MJD_BEG,MJD_END, &
     &           NOBS, STA_IND(2), STA_NUM(2), IND_STA, IND_SOU, &
     &           NUM_STA, IND_STA1, IND_STA2, MJD_EPC, MJD_OBS
      INTEGER  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
      INTEGER  I,J
      INTEGER  CNS_CODE
! THIS ARRAY ABOVE MUST BE NSTA LONG
      CHARACTER  STR*64
      PARAMETER  ( M_STA =  32 )
      PARAMETER  ( M_SOU = 512)
      PARAMETER  ( VTD__NDER = 17)
      CHARACTER  C_STA(M_STA)*8, C_SOU(M_SOU)*8, SOU_NAM*8
      CHARACTER*8         TMP_CHAR
      EQUIVALENCE ( TMP_CHAR ,  TMP_DP  )
      DOUBLE PRECISION TAU_GR,  RATE_PH, ACCL_PH, DER_DEL(19),  &
     &           DER_RAT(17)
      DOUBLE PRECISION ATM_PRES(2), ATM_TEMP(2)
      DOUBLE PRECISION COO_TRS(3,2),COO_CRS(3,2),&
     &           VEL_CRS(3,2),VEL_TRS(3,2), &
     &           UEN_TO_TRS(3,3,2)
      DOUBLE PRECISION REN_TO_TRS(3,3,2),ELEV(2),LAT_GCN(2),LAT_GDT(2)
      DOUBLE PRECISION HEI_ELL(2),LONG(2),TRS_TO_CRS(3,3),PLAN(3,2,11)
      !!!INTEGER, EXTERNAL ::  FIND_CLOCK_EPOCH
      INTEGER  CLK_PIN(0:2,32),TRP_PIN(32),EOP_PIN(3),STC_PAR(3),   &
     &           STC_PIN(32,3),STC_IND_STA(32,3)
      INTEGER  SOU_INDEX
      LOGICAL*4  FL_EOP_ESTIM
      LOGICAL, DIMENSION(:), ALLOCATABLE :: LEDIT_EXTRA
      DATA KENTRY/0/


      !!!!SAVE       NOBS, SUM1

!******************************************************************************
!  BEGIN EXECUTABLE CODE
!******************************************************************************
!
! FLAG FOR FIRST OBSERVATION ONLY
!     DATA L1ST/.TRUE./
!
!
!************************************************************************
       KENTRY=KENTRY+1
! DEBUG
!     WRITE(6,*)' DBG VLB_STR IN QVLBI ',nvopt
!     DO JJ=1,NVOPT
!      WRITE(6,30303) VLB_STR(JJ)
!      IF ( VLB_STR(JJ)(2:14) == 'VLBI_CONTROL:' ) THEN
!      WRITE(6,*)' dbg VLBI CONTROL DEFINITION '
!      ENDIF
!      IF ( VLB_STR(JJ)(2:14) == 'VLBI_EXPNAME:' ) THEN
!      WRITE(6,*)' dbg VLBI EXP_NAME DEFINITION '
!      ENDIF
!      IF ( VLB_STR(JJ)(2:17) == 'VTD_DEBUG_LEVEL:' ) THEN
!      WRITE(6,*)' dbg VTD_DEBUG_LEVEL DEFINITION '
!      ENDIF
!      IF ( VLB_STR(JJ)(2:18) == 'VLBI_DEBUG_LEVEL:' ) THEN
!      WRITE(6,*)' dbg VLBI_DEBUG_LEVEL DEFINITION '
!      ENDIF
!30303 FORMAT(A64)
!     ENDDO

      DO I=1,2
      LTHERD(I)=.FALSE.
      ENDDO

!
! --- Clear time derivative array
! --- Part of the original code --- No time derivatives have been computed
!
      IF(MTYPE.NE.36) CALL CLEARA ( AA(KTPRTL), 2*NM )

      IF(KENTRY.EQ.1) THEN
      DO I=1,3
      DO J=1,100
      DRCOO_TRS(I,J)=0.D0
      ENDDO
      ENDDO
      ENDIF

! --- Clear partial derivative array
!
      IF(MTYPE.EQ.36) GOTO 415
      IF ( .NOT. LNADJ ) THEN
           NCLEAR=NM*NADJST
           CALL CLEARA ( AA(KPMPA), NCLEAR )
           DO 410 J1=1,NADJST
              LL(KLAVOI+J1-1) = .TRUE.
 410       CONTINUE
      END IF
 415       CONTINUE
!
!
! --- Set dynamic array pointers for times, positions, and velocities
! --- And chebychev polynomials and coefficients
!
      JFSEC=KFSECS(1,1)
      DO 420 J2=1,2
         JDELCT(J2)=KDELCT(J2,1)
 420  CONTINUE
!
! --- Set dynamic array pointers for mean pole station coordinates
!
      DO 430 J3=1,2
         JXM(J3)=KXMN(J3,1)
 430  CONTINUE
      IF(MTYPE.EQ.36) THEN
      DO 431 J3=1,2
         JXM(J3)=KXMN(1,J3)
 431  CONTINUE
      ENDIF
!
      JCHEMR = KCHEMR+(IORTB*NM)
      JCHMOR = KCHMOR+(IORMO*NM)
      JEPOSR = KEPOSR+(    3*NM)
      JCHEMT = KCHEMT+(IORTB*NM)
      JCHMOT = KCHMOT+(IORMO*NM)
      JEPOST = KEPOST+(    3*NM)
      JCHCBS = KCHCBS+(IORCB*NM)
      JCPOSS = KCPOSS+(    3*NM)
!
!
      DO 440 J4=1,NQUAB
         IF ( QUANO(1) .EQ. QUAINF(2,J4) ) THEN
              CALL COPY_R8 ( 12, QUAINF(1,J4), QUANFO )
         END IF
 440  CONTINUE

! GET QUASAR COORDINATES
      DO 445 J4=1,NQUAB
       S_CRS(1,J4) =QUAINF(6,J4)*QUAINF(8,J4)
       S_CRS(2,J4) =QUAINF(5,J4)*QUAINF(8,J4)
       S_CRS(3,J4) = QUAINF(7,J4)
 445  CONTINUE
!
!
!
! L1STV is true during the first observation procesing of each iteration
!     write(6,*)' dbg qvlbi L1STV ',L1STV
      IF ( L1STV ) THEN
! -------- This section is for executed before processing the first
! -------- observation
!
! -------- Initiliaze the counter
!
         NOBS_SES = 0
           NOBS = 0
!
!
!
! NUMBER OF STATIONS
           L_STA = NSTA
!
! -------- Extract station name list
!
       IF(KENTRY.EQ.1) THEN
       WRITE(6,*)'                                                    '
       WRITE(6,*)' **************  VLBI REPORT *********************  '
       WRITE(6,*)'                                                    '
       ENDIF

           DO 450 J5=1,NSTA

      TMP_DP=AA(KSTNAM-1+J5)
      C_STA(J5)=TMP_CHAR

       IF(KENTRY.EQ.1) THEN

                   WRITE ( 6, * ) 'QVLBI Sta: ',J5,' C_STA=',C_STA(J5)
       ENDIF

! This code below for station net rotation

      CNS_CODE= NINT ( STAINF(19,J5) )
      LNNT_CNS_USE(J5)=.FALSE.
      LNNT_CNS=.FALSE.
      IF(CNS_CODE.EQ.1.OR.CNS_CODE.EQ.3) THEN
      LNNT_CNS_USE(J5)=.TRUE.
      LNNT_CNS=.TRUE.
      ENDIF
      LNNR_CNS_USE(J5)=.FALSE.
      LNNR_CNS=.FALSE.
      IF(CNS_CODE.EQ.2.OR.CNS_CODE.EQ.3) THEN
      LNNR_CNS_USE(J5)=.TRUE.
      LNNR_CNS=.TRUE.
      ENDIF
 450       CONTINUE
!
! NUMBER OF QUASARS
           L_SOU = NQUAB
           DO 460 J6=1,NQUAB
      TMP_DP=QUAINF(1,J6)
      C_SOU(J6)=TMP_CHAR

       IF(KENTRY.EQ.1) THEN
       WRITE ( 6, * ) 'QVLBI Sou: ',J6,' C_SOU=',C_SOU(J6)
       ENDIF

      CNS_CODE= NINT ( STAINF(19,J5) )
      LNNR_SRC_USE(J5)=.FALSE.
      LNNR_SRC=.FALSE.
      IF(CNS_CODE.EQ.2) LNNR_SRC_USE(J5)=.TRUE.
 460       CONTINUE


! This code below for quasar network rotation

       DNNT_STA_RH(1) = TRANET(1,1)
       DNNT_STA_RH(2) = TRANET(2,1)
       DNNT_STA_RH(3) = TRANET(3,1)
       DNNT_STA_SIGMA = TRANET(1,2)
!
       DNNR_STA_RH(1) = ROTNET(1,1)
       DNNR_STA_RH(2) = ROTNET(2,1)
       DNNR_STA_RH(3) = ROTNET(3,1)
       DNNR_STA_SIGMA = ROTNET(1,2)

!
       DNNR_SRC_RH(1) = ROTQUA(1,1)
       DNNR_SRC_RH(2) = ROTQUA(2,1)
       DNNR_SRC_RH(3) = ROTQUA(3,1)
       DNNR_SRC_SIGMA = ROTQUA(1,2)

!
!
! -------- Store nominal start and nonimal stop of the session
!
           MJD_BEG = VLMJD1
           MJD_END = VLMJD2
           TAI_BEG = VLSEC1
           TAI_END = VLSEC2
!          write(6,*)' dbg MJDs ',MJD_BEG,MJD_END,TAI_BEG,TAI_END
!
      END IF
!
      IF ( NOBS_SES == 0  .AND.  .NOT. LITER1 ) THEN

       NOBS_SES = NOBS
       NOBS     = 0
       CALL COPY_R8 ( NPVAL0(IXARC),VARC,EST_VEC)
       CALL COPY_R8 ( NPVAL0(IXGLBL),VGLB,EST_VEC(NPVAL0(IXARC)+1))
       KRES=0

      ENDIF

!
! --- Further section is for processing each observation
!
!
! --- Extract MJD of the observation
!
      ISECD=MOD(MJDSBL,86400)
      MJD_OBS = 30000+(MJDSBL-ISECD)/86400
!
! --- Extract the source name
!
      TMP_DP=QUANFO(1)
      SOU_NAM=TMP_CHAR
      IND_SOU = SOU_INDEX(SOU_NAM,C_SOU,L_SOU)
!
!
! --- Extract station names
!

      DO 480 J8=1,2
      TMP_DP=AA(KSTNAM-1+INDSTA(J8))
      STA_NAM(J8)=TMP_CHAR
      STA_NUM(J8) = II(KISTNO + INDSTA(J8)-1)
      CALL FNDNUM(STA_NUM(J8),II(KISTNO),L_STA,STA_IND(J8))
 480  CONTINUE
       DO JJ=1,L_STA
       ENDDO
!
! Extract Meteorological data

      DO 490 J9=1,NM
! sum seconds since midnight of MJDSBL plus fractional seconds in ET
         FSCVLE=DBLE(ISECD)+AA(JFSEC-1+J9)
         TDFQ=AA(JFSEC-1+J9)-AA(KOBTIM-1+J9)
         FSCVLU=FSCVLE-TDFQ
         OBSVLB=AA(KOBS-1+J9)
         ISTAP=INPSTA(1)
         JMET1  =KOBCOR(1,ISTAP)-1+J9
         CALL DMETV(AA(JMET1),PVLB1,TVLB1)
         ISTAP=INPSTA(2)
         JMET2  =KOBCOR(1,ISTAP)-1+J9
         CALL DMETV(AA(JMET2),PVLB2,TVLB2)
         ATM_PRES(1) = PVLB1*100.0D0
         ATM_PRES(2) = PVLB2*100.0D0
         ATM_TEMP(1) = TVLB1 + 273.15D0
         ATM_TEMP(2) = TVLB2 + 273.15D0
         JSMC  = KSMCOR - 1 + J9
         JSMC2 = KSMCR2 - 1 + J9
!
!        IF(MTYPE.EQ.31) THEN
!************************************************************************
!
              WRITE ( 6, 314 ) NOBS, TAI_OBS, STA_NAM(1), ATM_PRES(1), &
     &                         ATM_TEMP(1)
              WRITE ( 6, 314 ) NOBS, TAI_OBS, STA_NAM(2), ATM_PRES(2), &
     &                         ATM_TEMP(2)
 314          FORMAT ( 'METEO:  Nobs: ',I5,' Tai: ',F7.1,' sta: ', &
     &                  A, ' pres: ', F10.3, ' Temp=', F7.3 )
!
!************************************************************************
!        ENDIF
!
! ------ CALL VTSTST FOR BOTH STATIONS

      LVLBI=.TRUE.
      LSPOSO=.FALSE.
      L1SKIP=.FALSE.
      IEXIT=1
      LYARA=.FALSE.

!     STATION 1
!
      IFACT=0
      JCHEMR=KCHEMR+(IORTB*NM)*IFACT
      JCHMOR=KCHMOR+(IORMO*NM)*IFACT
      JEPOSR=KEPOSR+(    3*NM)*IFACT
      JCHEMT=KCHEMT+(IORTB*NM)*IFACT
      JCHMOT=KCHMOT+(IORMO*NM)*IFACT
      JEPOST=KEPOST+(    3*NM)*IFACT
      JCHCBS=KCHCBS+(IORCB*NM)*IFACT
      JCPOSS=KCPOSS+(    3*NM)*IFACT
      JXSTT =KXSTT +(3*MINTIM)*IFACT
      JXSTR =KXSTR +(3*MINTIM)*IFACT
      JXSS  =KXSS  +(3*MINTIM)*IFACT

      JJ1=1
      CALL  VTSTST (MJDSBL     ,AA(JFSEC),AA(JFSEC),AA(JFSEC),     &
     &   AA(JFSEC ),AA(JFSEC ),AA(KXTN  ),AA(KXSM  ),AA(KXTK  ),   &
     &   AA(KXSJ  ),AA(KXTI  ),AA(KVTN  ),AA(KVSM  ),AA(KVTK  ),   &
     &   AA(KVSJ  ),AA(KVTI  ),AA(JXM(1)),AA(JXM(1)),AA(JXM(1)),   &
     &   AA(KRNM  ),AA(KRKM  ),AA(KRKJ  ),AA(KRIJ  ),AA(KRRNM ),   &
     &   AA(KRRKM ),AA(KRRKJ ),AA(KRRIJ ),AA(KURNM ),AA(KURKM ),   &
     &   AA(KURKJ ),AA(KURIJ ),AA(KPRRNM),AA(KPRRKM),AA(KPRRKJ),   &
     &   AA(KPRRIJ),II(KNMP  ),INDSTA    ,II(KINDP ),AA(KRPOLE),   &
     &   AA(KPXPOL),NINTPL    ,MINTPL    ,AA(KS0   ),AA(KS1   ),   &
     &   AA(KXTP  ),AA(KRELV ),AA(KPXSXP),AA(KPXSLV),AA(KCOSTH),   &
     &   AA(KSINTH),INDSAT    ,SNDUM     ,NM        ,LPOLAD    ,   &
     &   LIGHT     ,LSPOSO    ,L1SKIP    ,IEXIT     ,.FALSE.   ,   &
     &   AA(KGRLT1),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),AA(JDELCT(1)),&
     &   AA(JDLCTK),AA(JDLCTI),AA(KPSTAT),AA(KRSUNS),AA(JCHEMR),   &
     &   AA(JCHMOR),AA(JEPOSR),AA(JCHEMT),AA(JCHMOT),AA(JEPOST),   &
     &   AA(JCHCBS),AA(JCPOSS),AA(KSCRTM),AA(JXSTT) ,AA(JXSTR) ,   &
     &   AA(JXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,  &
     &   lyara     ,isnumb ,LADD  ,AA(KXDPOL),AA(KPXDXP),AA(KXUTDT),&
     &   LVLBI,JJ1,RROT,UEN_G1,REN_G1,PSI,TIMVEL(1),LNUTAD)
       MOUNT_TYP(1)=TMOUNT
       ANT_DISP(1)=DISP
       COSPSI=COS(PSI)
       SINPSI=SIN(PSI)
!
! debug
!     write(6,*)' dbg TRS TO CRS ',RROT(1,1),RROT(2,1),RROT(3,1)
!     write(6,*)' dbg TRS TO CRS ',RROT(1,2),RROT(2,2),RROT(3,2)
!     write(6,*)' dbg TRS TO CRS ',RROT(1,3),RROT(2,3),RROT(3,3)
!     write(6,*)' dbg vel  stat 1 ' &
!    & ,AA(KVTN),AA(KVTN+MINTIM),AA(KVTN+MINTIM*2)
! debug
!
! ------ Load coordinates of the first station
!        Load station inertial CRS velocities too
!
      ISTAR = 0
      DO 510 J1=1,NSTA
         IF ( STA_NAM(1) .EQ. C_STA(J1) ) THEN
         IF (STAINF(20,J1).EQ.1.D0) LTHERD(1)=.TRUE.
              ISTAR = J1
              GOTO 910
         END IF
 510  CONTINUE
 910  CONTINUE
         J1=1
!  Load  terrestrial coordinates from VTSTST
         COO_TRS(1,J1)=AA(KXTP)
         COO_TRS(2,J1)=AA(KXTP+1)
         COO_TRS(3,J1)=AA(KXTP+2)
         DRCOO_TRS(1,ISTAR)=COO_TRS(1,J1)
         DRCOO_TRS(2,ISTAR)=COO_TRS(2,J1)
         DRCOO_TRS(3,ISTAR)=COO_TRS(3,J1)
!  Load  celestial coordinates from VTSTST
         COO_CRS(1,J1)=AA(KXTN)
         COO_CRS(2,J1)=AA(KXTN+MINTIM)
         COO_CRS(3,J1)=AA(KXTN+2*MINTIM)
!       write(6,*)' dbg QVLBI COO_TRS ',COO_TRS(1,J1),COO_TRS(2,J1), &
!    &   COO_TRS(3,J1)
!       write(6,*)' dbg QVLBI COO_CRS ',COO_CRS(1,J1),COO_CRS(2,J1), &
!    &   COO_CRS(3,J1)
         VEL_CRS(1,J1)=AA(KVTN)
         VEL_CRS(2,J1)=AA(KVTN+MINTIM)
         VEL_CRS(3,J1)=AA(KVTN+MINTIM*2)


!     write(6,*)' dbg UEN_G ',UEN_G1(1,1),UEN_G1(2,1),UEN_G1(3,1)
!     write(6,*)' dbg UEN_G ',UEN_G1(1,2),UEN_G1(2,2),UEN_G1(3,2)
!     write(6,*)' dbg UEN_G ',UEN_G1(1,3),UEN_G1(2,3),UEN_G1(3,3)
!     write(6,*)' dbg REN_G ',REN_G1(1,1),REN_G1(2,1),REN_G1(3,1)
!     write(6,*)' dbg REN_G ',REN_G1(1,2),REN_G1(2,2),REN_G1(3,2)
!     write(6,*)' dbg REN_G ',REN_G1(1,3),REN_G1(2,3),REN_G1(3,3)
!
! ------ Load Up East Nort (UEN) to TRS matrix for station 1
!
      UEN_TO_TRS(1,1,J1) = UEN_G1(1,1)
      UEN_TO_TRS(2,1,J1) = UEN_G1(2,1)
      UEN_TO_TRS(3,1,J1) = UEN_G1(3,1)
      UEN_TO_TRS(1,2,J1) = UEN_G1(1,2)
      UEN_TO_TRS(2,2,J1) = UEN_G1(2,2)
      UEN_TO_TRS(3,2,J1) = UEN_G1(3,2)
      UEN_TO_TRS(1,3,J1) = UEN_G1(1,3)
      UEN_TO_TRS(2,3,J1) = UEN_G1(2,3)
      UEN_TO_TRS(3,3,J1) = UEN_G1(3,3)

! ------ Load Radial East Nort (REN) to TRS matrix station 1
!
      REN_TO_TRS(1,1,J1) = REN_G1(1,1)
      REN_TO_TRS(2,1,J1) = REN_G1(2,1)
      REN_TO_TRS(3,1,J1) = REN_G1(3,1)
      REN_TO_TRS(1,2,J1) = REN_G1(1,2)
      REN_TO_TRS(2,2,J1) = REN_G1(2,2)
      REN_TO_TRS(3,2,J1) = REN_G1(3,2)
      REN_TO_TRS(1,3,J1) = REN_G1(1,3)
      REN_TO_TRS(2,3,J1) = REN_G1(2,3)
      REN_TO_TRS(3,3,J1) = REN_G1(3,3)

!    Also load in, geocentric,geodetic latitudes and height above ellipsoid
!
      LAT_GCN(J1)=PSI
      LAT_GDT(J1)=STANFO(1)
      HEI_ELL(J1)=STANFO(7)
      LONG(J1)=STANFO(4)

!************************************************************************
!
!     STATION 2
!
      IFACT=1
      JCHEMR=KCHEMR+(IORTB*NM)*IFACT
      JCHMOR=KCHMOR+(IORMO*NM)*IFACT
      JEPOSR=KEPOSR+(    3*NM)*IFACT
      JCHEMT=KCHEMT+(IORTB*NM)*IFACT
      JCHMOT=KCHMOT+(IORMO*NM)*IFACT
      JEPOST=KEPOST+(    3*NM)*IFACT
      JCHCBS=KCHCBS+(IORCB*NM)*IFACT
      JCPOSS=KCPOSS+(    3*NM)*IFACT
      JXSTT =KXSTT +(3*MINTIM)*IFACT
      JXSTR =KXSTR +(3*MINTIM)*IFACT
      JXSS  =KXSS  +(3*MINTIM)*IFACT

      JJ1=2
      CALL  VTSTST (MJDSBL     ,AA(JFSEC),AA(JFSEC),AA(JFSEC),     &
     &   AA(JFSEC ),AA(JFSEC ),AA(KXTK  ),AA(KXSM  ),AA(KXTK  ),   &
     &   AA(KXSJ  ),AA(KXTI  ),AA(KVTN  ),AA(KVSM  ),AA(KVTK  ),   &
     &   AA(KVSJ  ),AA(KVTI  ),AA(JXM(2)),AA(JXM(2)),AA(JXM(2)),   &
     &   AA(KRNM  ),AA(KRKM  ),AA(KRKJ  ),AA(KRIJ  ),AA(KRRNM ),   &
     &   AA(KRRKM ),AA(KRRKJ ),AA(KRRIJ ),AA(KURNM ),AA(KURKM ),   &
     &   AA(KURKJ ),AA(KURIJ ),AA(KPRRNM),AA(KPRRKM),AA(KPRRKJ),   &
     &   AA(KPRRIJ),II(KNMP  ),INDSTA(1) ,II(KINDP ),AA(KRPOLE),   &
     &   AA(KPXPOL),NINTPL    ,MINTPL    ,AA(KS0   ),AA(KS1   ),   &
     &   AA(KXTP  ),AA(KRELV ),AA(KPXSXP),AA(KPXSLV),AA(KCOSTH),   &
     &   AA(KSINTH),INDSAT    ,SNDUM     ,NM        ,LPOLAD    ,   &
     &   LIGHT     ,LSPOSO    ,L1SKIP    ,IEXIT     ,.FALSE.   ,   &
     &   AA(KGRLT2),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),AA(JDELCT(2)),&
     &   AA(JDLCTK),AA(JDLCTI),AA(KPSTAT),AA(KRSUNS),AA(JCHEMR),   &
     &   AA(JCHMOR),AA(JEPOSR),AA(JCHEMT),AA(JCHMOT),AA(JEPOST),   &
     &   AA(JCHCBS),AA(JCPOSS),AA(KSCRTM),AA(JXSTT) ,AA(JXSTR) ,   &
     &   AA(JXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,  &
     &   lyara     ,isnumb ,LADD  ,AA(KXDPOL),AA(KPXDXP),AA(KXUTDT),&
     &   LVLBI,JJ1,RROT,UEN_G2,REN_G2,PSI,TIMVEL(2),LNUTAD)
       MOUNT_TYP(2)=TMOUNT
       ANT_DISP(2)=DISP
       COSPSI=COS(PSI)
       SINPSI=SIN(PSI)
!
!     write(6,*)' dbg TRS TO CRS ',RROT(1,1),RROT(2,1),RROT(3,1)
!     write(6,*)' dbg TRS TO CRS ',RROT(1,2),RROT(2,2),RROT(3,2)
!     write(6,*)' dbg TRS TO CRS ',RROT(1,3),RROT(2,3),RROT(3,3)
!     write(6,*)' dbg terr coo stat 2 ',AA(KXTP),AA(KXTP+1),AA(KXTP+2)
!     write(6,*)' dbg vel stat 2 ' &
!    & ,AA(KVTN),AA(KVTN+MINTIM),AA(KVTN+MINTIM*2)
!
! ------ Load coordinates of the second station
!
      ISTAR = 0
      DO 511 J1=1,NSTA
         IF ( STA_NAM(2) .EQ. C_STA(J1) ) THEN
         IF (STAINF(20,J1).EQ.1.D0) LTHERD(2)=.TRUE.
              ISTAR = J1
              GOTO 911
         END IF
 511  CONTINUE
 911  CONTINUE
         J1=2
!  Load  terrestrial coordinates from VTSTST
         COO_TRS(1,J1)=AA(KXTP)
         COO_TRS(2,J1)=AA(KXTP+1)
         COO_TRS(3,J1)=AA(KXTP+2)
         DRCOO_TRS(1,ISTAR)=COO_TRS(1,J1)
         DRCOO_TRS(2,ISTAR)=COO_TRS(2,J1)
         DRCOO_TRS(3,ISTAR)=COO_TRS(3,J1)
!  Load  celestial coordinates from VTSTST
         COO_CRS(1,J1)=AA(KXTK)
         COO_CRS(2,J1)=AA(KXTK+MINTIM)
         COO_CRS(3,J1)=AA(KXTK+2*MINTIM)
!      write(6,*)' dbg QVLBI COO_TRS ',COO_TRS(1,J1),COO_TRS(2,J1), &
!    &   COO_TRS(3,J1)
!      write(6,*)' dbg QVLBI COO_CRS ',COO_CRS(1,J1),COO_CRS(2,J1), &
!    &   COO_CRS(3,J1)
         VEL_CRS(1,J1)=AA(KVTN)
         VEL_CRS(2,J1)=AA(KVTN+MINTIM)
         VEL_CRS(3,J1)=AA(KVTN+MINTIM*2)

!     write(6,*)' dbg UEN_G ',UEN_G2(1,1),UEN_G2(2,1),UEN_G2(3,1)
!     write(6,*)' dbg UEN_G ',UEN_G2(1,2),UEN_G2(2,2),UEN_G2(3,2)
!     write(6,*)' dbg UEN_G ',UEN_G2(1,3),UEN_G2(2,3),UEN_G2(3,3)
!     write(6,*)' dbg REN_G ',REN_G2(1,1),REN_G2(2,1),REN_G2(3,1)
!     write(6,*)' dbg REN_G ',REN_G2(1,2),REN_G2(2,2),REN_G2(3,2)
!     write(6,*)' dbg REN_G ',REN_G2(1,3),REN_G2(2,3),REN_G2(3,3)

! ------ Load Up East Nort (UEN) to TRS matrix for station 2

      UEN_TO_TRS(1,1,J1) = UEN_G2(1,1)
      UEN_TO_TRS(2,1,J1) = UEN_G2(2,1)
      UEN_TO_TRS(3,1,J1) = UEN_G2(3,1)
      UEN_TO_TRS(1,2,J1) = UEN_G2(1,2)
      UEN_TO_TRS(2,2,J1) = UEN_G2(2,2)
      UEN_TO_TRS(3,2,J1) = UEN_G2(3,2)
      UEN_TO_TRS(1,3,J1) = UEN_G2(1,3)
      UEN_TO_TRS(2,3,J1) = UEN_G2(2,3)
      UEN_TO_TRS(3,3,J1) = UEN_G2(3,3)

! ------ Load Radial East Nort (REN) to TRS matrix  for station 2
!
      REN_TO_TRS(1,1,J1) = REN_G2(1,1)
      REN_TO_TRS(2,1,J1) = REN_G2(2,1)
      REN_TO_TRS(3,1,J1) = REN_G2(3,1)
      REN_TO_TRS(1,2,J1) = REN_G2(1,2)
      REN_TO_TRS(2,2,J1) = REN_G2(2,2)
      REN_TO_TRS(3,2,J1) = REN_G2(3,2)
      REN_TO_TRS(1,3,J1) = REN_G2(1,3)
      REN_TO_TRS(2,3,J1) = REN_G2(2,3)
      REN_TO_TRS(3,3,J1) = REN_G2(3,3)

      LAT_GCN(J1)=PSI
      LAT_GDT(J1)=STANFO(1)
      HEI_ELL(J1)=STANFO(7)
      LONG(J1)=STANFO(4)

!
! ------ Load TRS_TO_CRS matrix i
!
      TRS_TO_CRS(1,1)=RROT(1,1)
      TRS_TO_CRS(2,1)=RROT(2,1)
      TRS_TO_CRS(3,1)=RROT(3,1)
      TRS_TO_CRS(1,2)=RROT(1,2)
      TRS_TO_CRS(2,2)=RROT(2,2)
      TRS_TO_CRS(3,2)=RROT(3,2)
      TRS_TO_CRS(1,3)=RROT(1,3)
      TRS_TO_CRS(2,3)=RROT(2,3)
      TRS_TO_CRS(3,3)=RROT(3,3)

      CALL BCCORD(MJDSBL,AA(JFSEC),NM,PLANET,PLANV,AA,II)
!
! ------ Load planet coordinates and velocities BC
!
         CALL PLAN_IN (PLAN, PLANET, PLANV)
!
! ------ Compute VLBI theoretical time delay and its partial derivatives
!
         NOBS    = NOBS + 1

!  TAI=(TAI-UTC) 33 + ET (FSCVLE) - (ET-UTC) 65.184

      CALL UTCET(.FALSE.,NM,MJDSBL,FSCVLE,UTSS,AA(KA1UT))
      CALL UTC_TO_TAI(MJD_OBS,FSCVLE,TAI)
      TAI_OBS = FSCVLE - (FSCVLE-UTSS) + (TAI- FSCVLE)
         UTC = TAI_OBS
         CALL UTC_TO_TAI(MJD_OBS,UTC,TAI)
         TDB_M_UTC = TAI - UTC + 32.184D0
       EPOCH= (MJD_OBS - MJD_BEG)*86400.D0 + (TAI_OBS-SEC_BEG)
!       write(6,*)' dbg setting epoch ',EPOCH,MJD_OBS,MJD_BEG,   &
!    &  (MJD_OBS - MJD_BEG)*86400.D0,TAI_OBS,SEC_BEG,(TAI_OBS-SEC_BEG)
         EPOCH=EPOCH-32

       CALL VLBI_DELAY( SOU_NAM, STA_NAM(1),STA_NAM(2),MJD_OBS,      &
     &               TAI_OBS,TAU_GR,  RATE_PH, ACCL_PH,              &
     &               DER_DEL, DER_RAT, S_CRS, NQUAB, C_STA,NSTA,     &
     &               C_SOU,ATM_PRES,COO_TRS,VEL_CRS,UEN_TO_TRS,      &
     &               REN_TO_TRS, ELEV,LAT_GCN,LAT_GDT,HEI_ELL,LONG,  &
     &               TRS_TO_CRS,COO_CRS,PLAN,VEL_TRS,MOUNT_TYP,      &
     &               ANT_DISP,QUANFO,ATM_TEMP,LTHERD)

!             WRITE ( 6, 230 ) NOBS, TAU_GR +    &
!    &        AA ( KOBCOR(8,INPSTA(2))+J9-1 )
!230          FORMAT ( 'QVLBI Nobs: ',I5,' Tau_gr_corr: ', F25.15 )


         UTC_VAL = TAI_BEG
         CALL UTC_TO_TAI(MJD_BEG,UTC_VAL,TAI_VAL)
         TDB_M_UTC = TAI_VAL - UTC_VAL + 32.184D0
 155     FORMAT ( A, 1X, F12.7 )
!
!
! ------ Initialization of the equation of conditions vector
!
         IF(MTYPE.NE.36) CALL NOUT_R8 ( NADJST, AA(KPMPA) )
!
!
! ------ Now insert appropriate partial derivative in the slot of
! ------ equation of condition vector
!
!
!                     BIAS PARAMETERS
!
! -------------- Clock offset
!
!
       IF (NPVAL0(IXBISA).GT.0) THEN

         IF(KLKSTA(1,1).GT.0) THEN
!        WRITE(6,*)' CLOCK BIAS FOR THE FIRST STATION ',KLKSTA(1,1)
         LL(KLAVOI+KLKSTA(1,1)-1) = .FALSE.
         AA(KPMPA+KLKSTA(1,1)-1)  = -1.0D0

!        write(6,*)'dbg CLOCK OFFSET PARAMETER FIRST STATION', &
!    &   AA(KPMPA+KLKSTA(1,1)-1),KPMPA+KLKSTA(1,1)-1
       ENDIF

!
! -------------- Clock rate
!

       IF(KLKSTA(2,1).GT.0) THEN
!        WRITE(6,*)' CLOCK DRIFT FOR THE FIRST STATION ',KLKSTA(2,1)
         LL(KLAVOI+KLKSTA(2,1)-1) = .FALSE.
         CALL GDAT_TO_MJD_SEC( AA(KPRML0+(KLKSTA(2,1)-1)*3+1), &
     &   MJD_START(1), TAI_START(1) )
         TAI_START(1)=TAI_START(1)+32.D0
         AA(KPMPA+KLKSTA(2,1)-1)  = &
     &   -(TAI_OBS - TAI_START(1) + TDB_M_UTC )/86400.0D0  &
     &   -(MJD_OBS - MJD_START(1))
!        write(6,*)'dbg CLOCK RATE PARAMETER FIRST STATION', &
!    &   AA(KPMPA+KLKSTA(2,1)-1),KPMPA+KLKSTA(2,1)-1,KLKSTA(2,1)
       ENDIF
!
! -------------- Clock second derivative
!

       IF(KLKSTA(3,1).GT.0) THEN
!        WRITE(6,*)' CLOCK ACCELERATION FOR THE FIRST STATION ', &
!    &   KLKSTA(3,1)
         LL(KLAVOI+KLKSTA(3,1)-1) = .FALSE.
         CALL GDAT_TO_MJD_SEC( AA(KPRML0+(KLKSTA(2,1)-1)*3+1), &
     &   MJD_START(1), TAI_START(1) )
         TAI_START(1)=TAI_START(1)+32.D0
         AA(KPMPA+KLKSTA(3,1)-1)  = &
     &   -( (TAI_OBS - TAI_START(1) + TDB_M_UTC )/86400.0D0  &
     &   +(MJD_OBS - MJD_START(1) )) ** 2
!     write(6,*)'dbg CLOCK ACCEL PARAMETER',  &
!    & AA(KPMPA+KLKSTA(3,1)-1), KPMPA+KLKSTA(3,1)-1,KLKSTA(3,1)
       ENDIF

!        IF(KLKSTA(4,1).GT.0) THEN
!        WRITE(6,*)' CLOCK ACCELERATION RATE FOR THE FIRST STATION '
!        ENDIF

!
! -------------- Clock offset
!

       IF(KLKSTA(1,2).GT.0) THEN
         WRITE(6,*)' CLOCK BIAS FOR THE SECOND STATION ',KLKSTA(1,2)
         LL(KLAVOI+KLKSTA(1,2)-1) = .FALSE.
         AA(KPMPA+KLKSTA(1,2)-1)  =  1.0D0
!     write(6,*)'dbg CLOCK OFFSET PARAMETER SECOND STATION' , &
!    &  AA(KPMPA+KLKSTA(1,2)-1),KPMPA+KLKSTA(1,2)-1

       ENDIF

         IF(KLKSTA(2,2).GT.0) THEN
!        WRITE(6,*)' CLOCK DRIFT FOR THE SECOND STATION ', &
!    &   KLKSTA(2,2)
         LL(KLAVOI+KLKSTA(2,2)-1) = .FALSE.
         CALL GDAT_TO_MJD_SEC( AA(KPRML0+(KLKSTA(2,2)-1)*3+1), &
     &   MJD_START(2), TAI_START(2) )
         TAI_START(2)=TAI_START(2)+32.D0
         AA(KPMPA+KLKSTA(2,2)-1)  = &
     &   (TAI_OBS - TAI_START(2))/86400.0D0  &
     &   + (MJD_OBS - MJD_START(2)) &
     &   + (TDB_M_UTC + TAU_GR + OBS_CAL )/86400.D0
!     write(6,*)'dbg CLOCK RATE PARAMETER SECOND STATION' , &
!    & AA(KPMPA+KLKSTA(2,2)-1),KPMPA+KLKSTA(2,2)-1,KLKSTA(2,2)
       ENDIF
!
! -------------- Clock rate
!

         IF(KLKSTA(3,2).GT.0) THEN
!        WRITE(6,*)' CLOCK ACCELERATION FOR THE SECOND STATION ', &
!    &   KLKSTA(3,2)
         LL(KLAVOI+KLKSTA(3,2)-1) = .FALSE.
         AA(KPMPA+KLKSTA(3,2)-1)  = &
     &   ( (TAI_OBS - TAI_START(2))/86400.0D0  &
     &   +(MJD_OBS - MJD_START(2))    &
     &   + (TDB_M_UTC + TAU_GR + OBS_CAL )/86400.0D0 )**2
!     write(6,*)'dbg CLOCK ACCEL PARAMETER SECOND STATION' , &
!    &  AA(KPMPA+KLKSTA(3,2)-1),KPMPA+KLKSTA(3,2)-1
       ENDIF

!        IF(KLKSTA(4,2).GT.0) THEN
!        WRITE(6,*)' CLOCK ACCELERATION RATE FOR THE SECOND STATION '
!        ENDIF
!
! ------ Clock function (spline version)
!
!     write(6,*)' dbg pointers for Clock function (spline version)'
!     write(6,*)'klksts',KLKSTS(1,1),KLKSTS(1,2),KLKSTS(2,1),KLKSTS(2,2)
!     write(6,*)' dbg from label ',AA(KPRML0+(KLKSTS(1,1)-1)*3)
!     write(6,*)' dbg from label ',AA(KPRML0+(KLKSTS(1,1)-1)*3+1)
!     write(6,*)' dbg from label ',AA(KPRML0+(KLKSTS(1,1)-1)*3+2)

      IF(KLKSTS(1,1).GT.0.OR.KLKSTS(1,2).GT.0) THEN

      IF(KLKSTS(1,1).GT.0) THEN
      SEG=MOD(AA(KPRML0+(KLKSTS(1,1)-1)*3+2),1000.0D0)
      ELSEIF(KLKSTS(1,2).GT.0) THEN
      SEG=MOD(AA(KPRML0+(KLKSTS(1,2)-1)*3+2),1000.0D0)
      ENDIF
      NSEG=INT(SEG)

      ENDIF

      IF(KLKSTS(1,1).GT.0.OR.KLKSTS(1,2).GT.0) THEN

! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(1) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.304) THEN
!     write(6,*)'constr ', II(KISTNO + INDSTA(1) -1 ),NSSPLN(1,JJ)
      NBSPLN(2,JJ)=KLKSTS(1,1)
      NBSPLN(3,JJ)=NSEG
      IND1=JJ
      ENDIF
      ENDDO
! constraints
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(2) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.304) THEN
!     write(6,*)'constr ', II(KISTNO + INDSTA(2) -1 ),NSSPLN(1,JJ)
      NBSPLN(2,JJ)=KLKSTS(1,2)
      NBSPLN(3,JJ)=NSEG
      IND=JJ
      ENDIF
      ENDDO
! constraints

      DO I=1,NSEG
      IF(KLKSTS(1,1).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(KLKSTS(1,1)+(I-1)-1)*3+1), &
     &   MJD_EPC(I), SEC_EPC(I))
      ELSEIF(KLKSTS(1,2).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(KLKSTS(1,2)+(I-1)-1)*3+1), &
     &   MJD_EPC(I), SEC_EPC(I))
      ENDIF

      NODE_SEC(I)=(MJD_EPC(I)-MJD_BEG)*86400.0D0 + (SEC_EPC(I)-SEC_BEG)
!     write(6,*)' dbg NODE_SEC(I) ',NODE_SEC(I),I
      ENDDO
      SPAN =NODE_SEC(2)-NODE_SEC(1)
      SIGSP(5,IND)=SPAN
      SIGSP(5,IND1)=SPAN

! FIND OUT IN WHICH SEGMENT ARE WE NOW
       DO JJ=1,NSEG
       IF ( EPOCH.GT.NODE_SEC(JJ).AND.EPOCH.LT.NODE_SEC(JJ+1))  &
     &    INDSP=JJ
       ENDDO

          IF(INDSP.GT.NSEG) THEN
          WRITE(6,*)' EXECUTION TERMINATING IN QVLBI '
          WRITE(6,*)' TOO BIG INDEX FOR CLOCK SPLINE PARAMETER '
          ENDIF

! COMPUTE FORWARD_FRACTION
      FORWARD_FRACTION  = (EPOCH -NODE_SEC(INDSP)) / SPAN
!     write(6,*)' dbg COMPUTING FORWARD FRA ',FORWARD_FRACTION,EPOCH, &
!   & NODE_SEC(INDSP),SPAN
! COMPUTE BACKWARD_FRACTION
      BACKWARD_FRACTION = 1.D0 - FORWARD_FRACTION
      ENDIF

!
! ----------- First station ...
!

      IF(KLKSTS(1,1).GT.0) THEN
         LL(KLAVOI+KLKSTS(2,1)-1) = .FALSE.
         AA(KPMPA+KLKSTS(2,1)-1)  = -BACKWARD_FRACTION
!     write(6,*)'dbg CLOCK SPLINE1',AA(KPMPA+KLKSTS(2,1)-1), &
!    &   KPMPA+KLKSTS(2,1)-1,KLKSTS(2,1)
      ENDIF

      IF(KLKSTS(2,1).GT.0) THEN
         LL(KLAVOI+KLKSTS(2,1)+1-1) = .FALSE.
         AA(KPMPA+KLKSTS(2,1)+1-1)  = -FORWARD_FRACTION
!     write(6,*)'dbg CLOCK SPLINE2',AA(KPMPA+KLKSTS(2,1)+1-1), &
!    &   KPMPA+KLKSTS(2,1)+1-1,KLKSTS(2,1)+1

      ENDIF

!
! ----------- ... and the second station
!

      IF(KLKSTS(1,2).GT.0) THEN
         LL(KLAVOI+KLKSTS(2,2)-1) = .FALSE.
         AA(KPMPA+KLKSTS(2,2)-1)  = BACKWARD_FRACTION
!     write(6,*)'dbg CLOCK SPLINE3',AA(KPMPA+KLKSTS(2,2)-1), &
!    &   KPMPA+KLKSTS(2,2)-1,KLKSTS(2,2)
      ENDIF

      IF(KLKSTS(2,2).GT.0) THEN
         LL(KLAVOI+KLKSTS(2,2)+1-1) = .FALSE.
         AA(KPMPA+KLKSTS(2,2)+1-1)  = FORWARD_FRACTION
!     write(6,*)'dbg CLOCK SPLINE4',AA(KPMPA+KLKSTS(2,2)+1-1), &
!    &   KPMPA+KLKSTS(2,2)+1-1,KLKSTS(2,2)+1
      ENDIF

!
! ------ Tropospheric zenith path delay (non-spline version)
!

       IF(IBTROP(1).GT.0) THEN
       WRITE(6,*)' TROPOSPHERIC BIAS FOR THE FIRST STATION '
         LL(KLAVOI+IBTROP(1)-1) = .FALSE.
         AA(KPMPA+IBTROP(1)-1)  = DER_DEL(12)
!     write(6,*)' dbg qvlbi TROP DELAY non-spline ',  &
!    & AA(KPMPA+IBTROP(1)-1),KPMPA+IBTROP(1)-1
       ENDIF

       IF(IBTROP(2).GT.0) THEN
       WRITE(6,*)' TROPOSPHERIC BIAS FOR THE SECOND STATION '
         LL(KLAVOI+IBTROP(2)-1) = .FALSE.
         AA(KPMPA+IBTROP(2)-1)  = DER_DEL(13)
!     write(6,*)' dbg qvlbi TROP DELAY non-spline2',  &
!    & AA(KPMPA+IBTROP(2)-1),KPMPA+IBTROP(2)-1
       ENDIF

        ENDIF
      DO I1=1,NCTSTA
!     write(6,*)'dbg NBS ',NBSPLN(1,I1),NBSPLN(2,I1),NBSPLN(3,I1),  &
!    &               NBSPLN(4,I1),NBSPLN(5,I1),NSSPLN(1,I1),  &
!    &               NSSPLN(2,I1),SIGSP(1,I1),SIGSP(2,I1),SIGSP(3,I1), &
!    &               SIGSP(4,I1), SIGSP(5,I1), SIGSP(6,I1)
      ENDDO

!
! ------ Tropospheric zenith path delay (spline version)
!
!     write(6,*)' dbg pointers for trop delay (spline version)'
!     write(6,*)'dbg itrpze ',ITRPZE(1,1),ITRPZE(1,2),ITRPZE(2,1),    &
!    &ITRPZE(2,2)
!   FORTLEZA
!     if(nobs.eq.95) itrpze(2,1)=21
!     if(nobs.eq.513) itrpze(2,1)=44
!     if(nobs.eq.790) itrpze(2,1)=58
!     if(nobs.eq.791) itrpze(2,1)=58
!     if(nobs.eq.837) itrpze(2,1)=61
!     if(nobs.eq.1117) itrpze(2,1)=78
!     if(nobs.eq.1118) itrpze(2,1)=78
!     if(nobs.eq.1120) itrpze(2,1)=78
!   HARTRAO
!     if(nobs.eq.7) itrpze(2,1)=8
!     if(nobs.eq.95) itrpze(2,2)=22
!     if(nobs.eq.513) itrpze(2,2)=44
!     if(nobs.eq.790) itrpze(2,2)=58
!     if(nobs.eq.792) itrpze(2,1)=58
!     if(nobs.eq.1117) itrpze(2,2)=78
!     if(nobs.eq.1119) itrpze(2,1)=78
!     if(nobs.eq.1121) itrpze(2,1)=78
!     IF(ITRPZE(1,1).GT.0.AND.ITRPZE(2,1).GT.0) THEN
!     write(6,*)' dbg ITRPZE ',ITRPZE(2,1),ITRPZE(2,1)+1
!     ENDIF
!     IF(ITRPZE(1,2).GT.0.AND.ITRPZE(2,2).GT.0) THEN
!     write(6,*)' dbg ITRPZE ',ITRPZE(2,2),ITRPZE(2,2)+1
!     ENDIF
!     write(6,*)ITRPZE(1,1),ITRPZE(1,2),ITRPZE(2,1),ITRPZE(2,2)
      IF(ITRPZE(1,1).GT.0.OR.ITRPZE(1,2).GT.0) THEN
!     write(6,*)' TROP SPLINE PARAMETER PARTIALS ,INDTSP ',INDTSP
      IF(ITRPZE(1,1).GT.0) THEN
      SEG=MOD(AA(KPRML0+(ITRPZE(1,1)-1)*3+2),1000.0D0)
      ELSEIF(ITRPZE(1,2).GT.0) THEN
      SEG=MOD(AA(KPRML0+(ITRPZE(1,2)-1)*3+2),1000.0D0)
      ENDIF
      NSEG=INT(SEG)

      IND=0
      IND1=0
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(1) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.501) THEN
!     write(6,*)'constr ', II(KISTNO + INDSTA(1) -1 ),NSSPLN(1,JJ)
      NBSPLN(2,JJ)=ITRPZE(1,1)
      NBSPLN(3,JJ)=NSEG
      IND1=JJ
      ENDIF
      ENDDO
! constraints
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(2) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.501) THEN
!     write(6,*)'constr ', II(KISTNO + INDSTA(2) -1 ),NSSPLN(1,JJ)
      NBSPLN(2,JJ)=ITRPZE(1,2)
      NBSPLN(3,JJ)=NSEG
      IND=JJ
      ENDIF
      ENDDO
! constraints

!     write(6,*)' TROP SPLINE PARAMETER PARTIALS nseg' ,nseg
      DO I=1,NSEG
      IF(ITRPZE(1,1).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(ITRPZE(1,1)+(I-1)-1)*3+1), &
     &   MJD_EPC_TRP(I), SEC_EPC_TRP(I))
      ELSEIF(ITRPZE(1,2).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(ITRPZE(1,2)+(I-1)-1)*3+1), &
     &   MJD_EPC_TRP(I), SEC_EPC_TRP(I))
      ENDIF
      NODE_SEC_TRP(I)=(MJD_EPC_TRP(I)-MJD_BEG)*86400.0D0 +     &
     &                (SEC_EPC_TRP(I)-SEC_BEG)
!     write(6,*)' dbg NODE_SEC ',NODE_SEC_TRP(I)
      ENDDO
      SPAN =NODE_SEC_TRP(2)-NODE_SEC_TRP(1)
!     write(6,*)' dbg SPAN ',SPAN
      SIGSP(5,IND)=SPAN
      SIGSP(5,IND1)=SPAN

! FIND OUT IN WHICH SEGMENT ARE WE NOW
       DO JJ=1,NSEG
       IF ( EPOCH.GT.NODE_SEC_TRP(JJ).AND.EPOCH.LT.NODE_SEC_TRP(JJ+1))  &
     &    INDTSP=JJ
       ENDDO

           IF(INDTSP.GT.NSEG) THEN
           WRITE(6,*)' EXECUTION TERMINATING IN QVLBI '
           WRITE(6,*)' TOO BIG INDEX FOR EASR TROP SPLINE PARAMETER '
           ENDIF

! COMPUTE FORWARD_FRACTION
        FORWARD_FRACTION  = (EPOCH -NODE_SEC_TRP(INDTSP)) / SPAN
!       write(6,*)' dbg  FORWARD_FRACTION ', FORWARD_FRACTION,   &
!    & EPOCH,NODE_SEC_TRP(INDTSP),SPAN,INDTSP
! COMPUTE BACKWARD_FRACTION
        BACKWARD_FRACTION = 1.D0 - FORWARD_FRACTION
!       write(6,*)' dbg  BACKWARD_FRACTION ', BACKWARD_FRACTION

      ENDIF

!
! ----------- First station ...
!

      IF(ITRPZE(1,1).GT.0) THEN
      LL(KLAVOI+ITRPZE(2,1)-1) = .FALSE.
      AA(KPMPA+ITRPZE(2,1)-1)  =  DER_DEL(12)* BACKWARD_FRACTION
!     write(6,*)'dbg TROP SPLINE1',AA(KPMPA+ITRPZE(2,1)-1), &
!    &   KPMPA+ITRPZE(2,1)-1, ITRPZE(2,1)
      ENDIF

      IF(ITRPZE(2,1).GT.0) THEN
      LL(KLAVOI+ITRPZE(2,1)+1-1) = .FALSE.
      AA(KPMPA+ITRPZE(2,1)+1-1)  =  DER_DEL(12)*FORWARD_FRACTION
!     write(6,*)'dbg TROP SPLINE2',AA(KPMPA+ITRPZE(2,1)+1-1), &
!    &   KPMPA+ITRPZE(2,1)+1-1, ITRPZE(2,1)+1
      ENDIF
!
! ----------- ... and the second station
!

      IF(ITRPZE(1,2).GT.0) THEN
!     LL(KLAVOI+ITRPZE(1,2)-1) = .FALSE.
      LL(KLAVOI+ITRPZE(2,2)-1) = .FALSE.
!     AA(KPMPA+ITRPZE(1,2)-1)  =  DER_DEL(13)*BACKWARD_FRACTION
      AA(KPMPA+ITRPZE(2,2)-1)  =  DER_DEL(13)*BACKWARD_FRACTION

!     write(6,*)'dbg TROP SPLINE3',AA(KPMPA+ITRPZE(2,2)-1), &
!    &   KPMPA+ITRPZE(2,2)-1, ITRPZE(2,2)
      ENDIF

      IF(ITRPZE(2,2).GT.0) THEN
      LL(KLAVOI+ITRPZE(2,2)+1-1) = .FALSE.
      AA(KPMPA+ITRPZE(2,2)+1-1)  =  DER_DEL(13)*FORWARD_FRACTION
!     write(6,*)'dbg TROP SPLINE4',AA(KPMPA+ITRPZE(2,2)+1-1), &
!    &   KPMPA+ITRPZE(2,2)+1-1 , ITRPZE(2,2)+1
      ENDIF
!
! ------ Tropospheric north gradient (spline)
!
!     write(6,*)' dbg pointers for trop delay (north gradient)'
!     write(6,*)ITRPGR(1,1),ITRPGR(1,2),ITRPGR(2,1),ITRPGR(2,2)
      IF(ITRPGR(1,1).GT.0.OR.ITRPGR(1,2).GT.0) THEN
!     write(6,*)' TROP SPLINE NORTH GRAD PARAMETERS ,INDTNO ',INDNOT
      IF(ITRPGR(1,1).GT.0) THEN
      SEG=MOD(AA(KPRML0+(ITRPGR(1,1)-1)*3+2),1000.0D0)
      ELSEIF(ITRPGR(1,2).GT.0) THEN
      SEG=MOD(AA(KPRML0+(ITRPGR(1,2)-1)*3+2),1000.0D0)
      ENDIF
      NSEG=INT(SEG)
      IND=0
      IND1=0
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(1) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.502) THEN
      NBSPLN(2,JJ)=ITRPGR(1,1)
      NBSPLN(3,JJ)=NSEG
      IND1=JJ
      ENDIF
      ENDDO
! constraints
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(2) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.502) THEN
      NBSPLN(2,JJ)=ITRPGR(1,2)
      NBSPLN(3,JJ)=NSEG
      IND=JJ
      ENDIF
      ENDDO
! constraints

!     write(6,*)' TROP SPLINE PARAMETER PARTIALS NORTH nseg' ,nseg
      DO I=1,NSEG
      IF(ITRPGR(1,1).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(ITRPGR(1,1)+(I-1)-1)*3+1), &
     &   MJD_EPC_GRN(I), SEC_EPC_GRN(I))
      ELSEIF(ITRPGR(1,2).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(ITRPGR(1,2)+(I-1)-1)*3+1), &
     &   MJD_EPC_GRN(I), SEC_EPC_GRN(I))
      ENDIF
      NODE_SEC_GRN(I)=(MJD_EPC_GRN(I)-MJD_BEG)*86400.0D0 +     &
     &                (SEC_EPC_GRN(I)-SEC_BEG)
      ENDDO
      SPAN =NODE_SEC_GRN(2)-NODE_SEC_GRN(1)
      SIGSP(5,IND)=SPAN
      SIGSP(5,IND1)=SPAN

! FIND OUT IN WHICH SEGMENT ARE WE NOW
       DO JJ=1,NSEG
       IF ( EPOCH.GT.NODE_SEC_GRN(JJ).AND.EPOCH.LT.NODE_SEC_GRN(JJ+1))  &
     &    INDTNO=JJ
       ENDDO

           IF(INDTNO.GT.NSEG) THEN
           WRITE(6,*)' EXECUTION TERMINATING IN QVLBI '
           WRITE(6,*)' TOO BIG INDEX FOR EASR NORTH SPLINE PARAMETER '
           ENDIF

! COMPUTE FORWARD_FRACTION
        FORWARD_FRACTION  = (EPOCH -NODE_SEC_GRN(INDTNO)) / SPAN
! COMPUTE BACKWARD_FRACTION
        BACKWARD_FRACTION = 1.D0 - FORWARD_FRACTION

      ENDIF
!
! ----------- First station ...
!

      IF(ITRPGR(1,1).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,1)-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,1)-1)  = DER_DEL(14)*BACKWARD_FRACTION
!     write(6,*)' dbg TROP N1',KPMPA,AA(KPMPA+ITRPGR(2,1)-1), &
!    &   KPMPA+ITRPGR(2,1)-1,ITRPGR(2,1)
      ENDIF

      IF(ITRPGR(2,1).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,1)+1-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,1)+1-1)  = DER_DEL(14)*FORWARD_FRACTION
!     write(6,*)' dbg TROP N2',KPMPA,AA(KPMPA+ITRPGR(2,1)+1-1), &
!    &   KPMPA+ITRPGR(2,1)+1-1,ITRPGR(2,1)+1
      ENDIF
!
! ----------- ... and the second station
!

      IF(ITRPGR(1,2).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,2)-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,2)-1)  = DER_DEL(16)*BACKWARD_FRACTION
!     write(6,*)' dbg TROP N3',KPMPA,AA(KPMPA+ITRPGR(2,2)-1), &
!    &   KPMPA+ITRPGR(2,2)-1,ITRPGR(2,2)
!     write(6,*)' dbg ' , DER_DEL(16),BACKWARD_FRACTION
      ENDIF

      IF(ITRPGR(2,2).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,2)+1-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,2)+1-1)  =  DER_DEL(16)*FORWARD_FRACTION
!     write(6,*)' dbg TROP N4',KPMPA,AA(KPMPA+ITRPGR(2,2)+1-1), &
!    &   KPMPA+ITRPGR(2,2)+1-1,ITRPGR(2,2)+1
      ENDIF


!
! ------ Tropospheric east gradient (spline)
!
!     write(6,*)' dbg pointers for trop delay (east gradient)'
!     write(6,*)ITRPGR(1,3),ITRPGR(1,4),ITRPGR(2,3),ITRPGR(2,4)
      IF(ITRPGR(1,3).GT.0.OR.ITRPGR(1,4).GT.0) THEN
!     write(6,*)' TROP SPLINE EAST GRAD PARAMETERS ,INDTEA ',INDTEA
      IF(ITRPGR(1,3).GT.0) THEN
      SEG=MOD(AA(KPRML0+(ITRPGR(2,1)-1)*3+2),1000.0D0)
      ELSEIF(ITRPGR(1,4).GT.0) THEN
      SEG=MOD(AA(KPRML0+(ITRPGR(1,4)-1)*3+2),1000.0D0)
      ENDIF
      NSEG=INT(SEG)
!     write(6,*)' TROP SPLINE PARAMETER PARTIALS EAST nseg' ,nseg
      IND1=0
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(1) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.503) THEN
      NBSPLN(2,JJ)=ITRPGR(1,3)
      NBSPLN(3,JJ)=NSEG
      IND1=JJ
      ENDIF
      ENDDO
! constraints
! constraints
      DO JJ=1,NCTSTA
      IF(II(KISTNO + INDSTA(2) -1 ).EQ.NSSPLN(1,JJ).AND.&
     &NBSPLN(1,JJ).EQ.503) THEN
      NBSPLN(2,JJ)=ITRPGR(1,4)
      NBSPLN(3,JJ)=NSEG
      IND=JJ
      ENDIF
      ENDDO
! constraints
      DO I=1,NSEG
      IF(ITRPGR(1,3).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(ITRPGR(1,3)+(I-1)-1)*3+1), &
     &   MJD_EPC_GRE(I), SEC_EPC_GRE(I))
      ELSEIF(ITRPGR(1,4).GT.0) THEN
      CALL GDAT_TO_MJD_SEC( AA(KPRML0+(ITRPGR(1,4)+(I-1)-1)*3+1), &
     &   MJD_EPC_GRE(I), SEC_EPC_GRE(I))
      ENDIF
      NODE_SEC_GRE(I)=(MJD_EPC_GRE(I)-MJD_BEG)*86400.0D0 +     &
     &                (SEC_EPC_GRE(I)-SEC_BEG)
      ENDDO
      SPAN =NODE_SEC_GRE(2)-NODE_SEC_GRE(1)
      SIGSP(5,IND)=SPAN
      SIGSP(5,IND1)=SPAN

! FIND OUT IN WHICH SEGMENT ARE WE NOW
       DO JJ=1,NSEG
       IF ( EPOCH.GT.NODE_SEC_GRE(JJ).AND.EPOCH.LT.NODE_SEC_GRE(JJ+1))  &
     &    INDTEA=JJ
       ENDDO

           IF(INDTEA.GT.NSEG) THEN
           WRITE(6,*)' EXECUTION TERMINATING IN QVLBI '
           WRITE(6,*)' TOO BIG INDEX FOR EAST GRAD SPLINE PARAMETER '
           ENDIF

! COMPUTE FORWARD_FRACTION
        FORWARD_FRACTION  = (EPOCH -NODE_SEC_GRE(INDTEA)) / SPAN
! COMPUTE BACKWARD_FRACTION
        BACKWARD_FRACTION = 1.D0 - FORWARD_FRACTION

      ENDIF
!
! ----------- First station ...
!

      IF(ITRPGR(1,3).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,3)-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,3)-1)  =  DER_DEL(15)*BACKWARD_FRACTION
!     write(6,*)' dbg TROP E1',KPMPA,AA(KPMPA+ITRPGR(2,3)-1), &
!    &   KPMPA+ITRPGR(2,3)-1,ITRPGR(2,3)
      ENDIF

      IF(ITRPGR(2,3).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,3)+1-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,3)+1-1)  = DER_DEL(15)*FORWARD_FRACTION
!     write(6,*)' dbg TROP E2',KPMPA,AA(KPMPA+ITRPGR(2,3)+1-1), &
!    &   KPMPA+ITRPGR(2,3)+1-1,ITRPGR(2,3)+1
      ENDIF
!
! ----------- ... and the second station
!

      IF(ITRPGR(1,4).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,4)-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,4)-1)  = DER_DEL(17)*BACKWARD_FRACTION
!     write(6,*)' dbg TROP E3',KPMPA,AA(KPMPA+ITRPGR(2,4)-1), &
!    &   KPMPA+ITRPGR(1,4)-1,ITRPGR(2,4)
      ENDIF

      IF(ITRPGR(2,4).GT.0) THEN
      LL(KLAVOI+ITRPGR(2,4)+1-1) = .FALSE.
      AA(KPMPA+ITRPGR(2,4)+1-1)  = DER_DEL(17)*FORWARD_FRACTION
!     write(6,*)' dbg TROP E4',KPMPA,AA(KPMPA+ITRPGR(2,4)+1-1), &
!    &   KPMPA+ITRPGR(2,4)+1-1,ITRPGR(2,4)+1
      ENDIF




!                     POLE PARAMETERS

      NPARP=II(KINDP+MINTPL*(NINTPL(1)-1))
      ISX=IPVAL0(IXPOLX)+3*(NPARP-1)
      ISY=ISX+1
      ISA=ISY+1
!     write(6,*)' dbg IND ',ISX,ISY,ISA
!     write(6,*)' dbg POLE  PARAMETERS',II(KINDP),NPARP,MINTPL,NINTPL(1)
      IF(NPVAL0(IXPOLX).GT.0) THEN
!
!
! ----------- X pole coordinate
!
      LL(KLAVOI+ISX-1) = .FALSE.
      AA(KPMPA+ISX-1) =  DER_DEL(2)
!     write(6,*)'dbg DER_DEL(2) ',DER_DEL(2)
!     write(6,*)'dbg X pole ',AA( KPMPA+ISX-1), &
!    &   KPMPA+ISX-1
       ENDIF

      IF(NPVAL0(IXPOLY).GT.0) THEN
!
! ----------- Y pole coordinate
!
      LL(KLAVOI+ISY-1) = .FALSE.
      AA( KPMPA+ISY-1)  =  DER_DEL(1)
!     write(6,*)'dbg DER_DEL(1) ',DER_DEL(1)
!     write(6,*)'dbg Y pole ',AA( KPMPA+ISY-1), &
!    &   KPMPA+ISY-1
      ENDIF

      IF(NPVAL0(IXUT1).GT.0) THEN
!
! ----------- UT1 angle
!
      LL(KLAVOI+ISA-1) = .FALSE.
      AA(KPMPA+ISA-1) = -DER_DEL(3)*7.2921158D-5
!     write(6,*)'dbg UT1 ',AA( KPMPA+ISA-1), &
!    &   KPMPA+ISA-1
      ENDIF
!
!                     POLE RATE PARAMETERS
!      write(6,*)' dbg POLE RATE PARAMETERS '
      NPARP=II(KINDP+MINTPL*(NINTPL(1)-1))
      ISXD=IPVAL0(IXPXDT)+3*(NPARP-1)
      ISYD=ISXD+1
      ISAD=ISYD+1

      IF(NPVAL0(IXPXDT).GT.0) THEN
      MJD_POLEUT = (AA(KDPSR+1)+AA(KDPSR+2))/(2.0D0*86400.0D0) + 30000
      TAI_POLEUT = (AA(KDPSR+1)+AA(KDPSR+2))/2.0D0 - &
     &             (MJD_POLEUT-30000)*86400.0D0 - 32.184D0
!     WRITE ( 6, * ) 'QVLBI_POLEUT: MJD_POLEUT = ',MJD_POLEUT, &
!    &                         ' TAI_POLEUT = ',TAI_POLEUT
!
! ----------- X pole coordinate
!
      LL(KLAVOI+ISXD-1) = .FALSE.
      AA( KPMPA+ISXD-1) =  DER_DEL(2) *                     &
     &        ( ( MJD_OBS - MJD_POLEUT) *86400.0D0 + &
     &        ( TAI_OBS - TAI_POLEUT)  ) !/206264.806D0
!     write(6,*)'dbg Xpolrate',AA( KPMPA+ISXD-1)  ,           &
!    & KPMPA+ISXD-1

      ENDIF

      IF(NPVAL0(IXPYDT).GT.0) THEN
      MJD_POLEUT = (AA(KDPSR+1)+AA(KDPSR+2))/(2.0D0*86400.0D0) + 30000
      TAI_POLEUT = (AA(KDPSR+1)+AA(KDPSR+2))/2.0D0 - &
     &    (MJD_POLEUT-30000)*86400.0D0 - 32.184D0
!
! ----------- Y pole coordinate
!
      LL(KLAVOI+ISYD-1) = .FALSE.
      AA( KPMPA+ISYD-1)  =  DER_DEL(1) *                    &
     &        ( ( MJD_OBS - MJD_POLEUT) *86400.0D0 + &
     &        ( TAI_OBS - TAI_POLEUT)  )  !  /206264.806D0
!      write(6,*)'dbg Ypolrate',AA( KPMPA+ISYD-1),             &
!    & KPMPA+ISYD-1

      ENDIF

      IF(NPVAL0(IXUTDT).GT.0) THEN
      MJD_POLEUT = (AA(KDPSR+1)+AA(KDPSR+2))/(2.0D0*86400.0D0) + 30000
      TAI_POLEUT = (AA(KDPSR+1)+AA(KDPSR+2))/2.0D0 - &
     &             (MJD_POLEUT-30000)*86400.0D0 - 32.184D0

!
! ----------- UT1 angle
!
      LL(KLAVOI+ISAD-1) = .FALSE.
      AA(KPMPA+ISAD-1)  = -DER_DEL(3) *      &
     &              ( (MJD_OBS - MJD_POLEUT)*86400.0D0 + &
     &              ( TAI_OBS - TAI_POLEUT)   ) *7.2921158D-5
!     write(6,*)'dbg UT1rate',AA( KPMPA+ISAD-1),&
!    & KPMPA+ISAD-1
      ENDIF


!                     NUTATION PARAMETERS
!     write(6,*)' dbg QVLBI' , NPVAL0(IXDPSI),NPVAL0(IXEPST)
      IF(NPVAL0(IXDPSI).GT.0) THEN
      INDEX1=IPVAL0(IXDPSI)
      INDPSI=INDEX1+(NUTLST-2)
      LL(KLAVOI+INDPSI-1)=.FALSE.
      AA(KPMPA+INDPSI-1)=DER_DEL(18)
      write(6,*)' dbg PMPA dpsi ',DER_DEL(18)
      ENDIF

      IF(NPVAL0(IXEPST).GT.0) THEN
      INDEX1=IPVAL0(IXEPST)
      INDEPS=INDEX1+(NUTLST-2)
      LL(KLAVOI+INDEPS-1)=.FALSE.
      AA(KPMPA+INDEPS-1)=DER_DEL(19)
      write(6,*)' dbg PMPA epst ',DER_DEL(19)
      ENDIF

!
!                     STATION POSITION PARAMETERS
!
       NP1=INDSTA(1)*3
       NP2=INDSTA(2)*3

       IF(NP1.GT.0.AND.NP1.LE.NPVAL0(IXSTAP)) THEN
       INDX1=IPVAL0(IXSTAP)+(INDSTA(1)-1)*3
        DO JJ=1,3
          LL(KLAVOI+INDX1+JJ-2) = .FALSE.
          AA(KPMPA+INDX1+JJ-2)  = &
     &    DER_DEL(JJ+3)
!     write(6,*)' dbg  DER_DEL(JJ+3)',DER_DEL(JJ+3),JJ+3
!     write(6,*)' dbg  STATION COO',AA(KPMPA+INDX1+JJ-2),  &
!    & KPMPA+INDX1+JJ-2,INDX1+JJ-1
        ENDDO
       ENDIF

       IF(NP2.GT.0.AND.NP2.LE.NPVAL0(IXSTAP)) THEN
       INDX2=IPVAL0(IXSTAP)+(INDSTA(2)-1)*3
        DO JJ=1,3
          LL(KLAVOI+INDX2+JJ-2) = .FALSE.
          AA(KPMPA+INDX2+JJ-2)  = &
     &    DER_DEL(JJ+6)
!     write(6,*)' dbg  DER_DEL(JJ+6)',DER_DEL(JJ+6),JJ+6
!     write(6,*)' dbg  STATION COO 2 ',AA(KPMPA+INDX2+JJ-2),  &
!    & KPMPA+INDX2+JJ-2,INDX2+JJ-1
        ENDDO
       ENDIF
!
!                     STATION VELOCITY PARAMETERS
!
!   the first station

       NP1=INDSTA(1)*3
       NP2=INDSTA(2)*3

       IF(NP1.GT.0.AND.NP1.LE.NPVAL0(IXSTAV)) THEN
       INDX1=IPVAL0(IXSTAV)+(INDSTA(1)-1)*3
        DO JJ=1,3
          LL(KLAVOI+INDX1+JJ-2) = .FALSE.
          AA(KPMPA+INDX1+JJ-2)  = &
     &    DER_DEL(JJ+3)*(MJDSBL-TIMVEL(1)+AA(JFSEC))
!     write(6,*)'dbg DER_DEL(JJ+3)',DER_DEL(JJ+3),JJ+3
!     write(6,*)' dbg  STATION VEL',AA(KPMPA+INDX1+JJ-2),  &
!    & KPMPA+INDX1+JJ-2,INDX1+JJ-1
        ENDDO
       ENDIF


       IF(NP2.GT.0.AND.NP2.LE.NPVAL0(IXSTAV)) THEN
       INDX2=IPVAL0(IXSTAV)+(INDSTA(2)-1)*3
        DO JJ=1,3
          LL(KLAVOI+INDX2+JJ-2) = .FALSE.
          AA(KPMPA+INDX2+JJ-2)  = &
     &    DER_DEL(JJ+6)*(MJDSBL-TIMVEL(2)+AA(JFSEC))
!     write(6,*)' dbg  DER_DEL(JJ+6)',DER_DEL(JJ+6),JJ+6
!     write(6,*)' dbg  STATION VEL 2 ',AA(KPMPA+INDX2+JJ-2),  &
!    & KPMPA+INDX2+JJ-2,INDX2+JJ-1
        ENDDO
       ENDIF


!
!                     QUASAR PARAMETERS

      INDX1=0
      INDX2=0
       IF(NPVAL0(IXVLBI).GT.0) THEN

          do jjk=1,NPVAL0(IXVLBI),2
        DDD1 =ABS(AA(KPRMV0-2+IPVAL0(IXVLBI)+JJK)-QUAINF(3,IND_SOU))
        DDD2 =ABS(AA(KPRMV0-2+IPVAL0(IXVLBI)+JJK+1)-QUAINF(4,IND_SOU))
        IF(DDD1.LE.000000.1.AND.DDD2.LE.0.0000001) THEN
        INDX1=IPVAL0(IXVLBI)-1+JJK
        INDX2=IPVAL0(IXVLBI)-1+JJK+1
          ENDIF

          enddo

       END IF

        IF(INDX1.EQ.0.AND.INDX2.EQ.0) GO TO 3322
       LL(KLAVOI+INDX1-1) = .FALSE.
       LL(KLAVOI+INDX2-1) = .FALSE.
       AA(KPMPA+INDX1-1)  = &
     &    DER_DEL(10)
       write(6,*)' dbg RA ',AA(KPMPA+INDX1-1),KPMPA+INDX1-1,INDX1
       AA(KPMPA+INDX2-1)  = &
     &    DER_DEL(11)
       write(6,*)' dbg DECL ',AA(KPMPA+INDX2-1),KPMPA+INDX2-1,INDX2

 3322  CONTINUE
!      END IF

!
! ----------- Baseline-dependent clock function
!
       IF(IBSBIA(1).GT.0) THEN
!      WRITE(6,*)'dbg BSLN PARAMETER LABEL',AA(KPRML0+(IBSBIA(1)-1)*3)
!      write(6,*)' dbg IBSBIA(1) ',IBSBIA(1)
       WRITE (UNIT=STRING(1:15),FMT='(F15.0)')AA(KPRML0+(IBSBIA(1)-1)*3)
       IF(STRING(1:4).EQ.'4800') WRITE(6,*)' BASELINE CLOCK FUNCTION '
       READ ( UNIT=STRING(5:9), FMT='(F5.0)' ) STA1
       READ ( UNIT=STRING(10:14), FMT='(F5.0)' ) STA2
       ISTA1=INT(STA1)
       ISTA2=INT(STA2)
!      write(6,*)' dbg block stations ',II(KISTNO + INDSTA(1) -1)
!      write(6,*)' dbg block stations ',II(KISTNO + INDSTA(2) -1)

            IF(II(KISTNO + INDSTA(1) -1 ).EQ.ISTA1.AND.&
     &      II(KISTNO + INDSTA(2) -1 ).EQ.ISTA2) THEN
            LL(KLAVOI+(IBSBIA(1)-1))=.FALSE.
          AA(KPMPA+(IBSBIA(1)-1))=1.D0
          WRITE(6,*)' dbg BOTH STATIONS IN THE BASELINE CONFIGURATION '
!     write(6,*)' dbg B2 ',AA(KPMPA+(IBSBIA(1)-1)),IBSBIA(1)
          ENDIF
            IF(II(KISTNO + INDSTA(1) -1 ).EQ.ISTA2.AND.&
     &      II(KISTNO + INDSTA(2) -1 ).EQ.ISTA1) THEN
            LL(KLAVOI+(IBSBIA(1)-1))=.FALSE.
          AA(KPMPA+(IBSBIA(1)-1))=-1.D0
          WRITE(6,*)' dbg BOTH STATIONS IN THE BASELINE CONFIGURATION '
!     write(6,*)' dbg B2 ',AA(KPMPA+(IBSBIA(1)-1)),IBSBIA(1)
          ENDIF
       ENDIF


       IF(MTYPE.EQ.31) AA(KSMCOR + J9-1) = 0.0D0
       VLBTOT=TAU_GR+AA(KOBCOR(7,INPSTA(2)))
!******   DEP DELTA DOR MEASUREMENTS  *************
!
      IF(MTYPE.EQ.36) RETURN

!
! ------ Put right hand side of the equation
!
! TEMPORARILLY REMOVE CORRECTION IT INTERF. WITH RELATIVISTIC CORR OF METRIC
         AA(KOBSC-1+J9)  = TAU_GR +  AA ( KOBCOR(8,INPSTA(1))+J9-1 )
!        AA(KOBSC-1+J9)  = TAU_GR +  AA ( KOBCOR(8,INPSTA(2))+J9-1 )
         IF(MTYPE.EQ.36) AA(KOBSC-1+J9)  = TAU_GR
         write(6,*)' dbg AA(KOBSC-1+J9) ',AA(KOBSC-1+J9)
 490  CONTINUE
!
!************************************************************************

      IF ( .NOT. LPSBL ) THEN

! -------- Edit, print, sum statistics and sum into normal equations
!
        ALLOCATE(LEDIT_EXTRA(NM))
        LEDIT_EXTRA(:) = .FALSE.
        CALL PROCES(AA, II, LL, AA(KOBSC), AA(KRESID), AA(KSIGMA), &
     &              AA(KRATIO), AA(KPMPA), AA(KOBS), AA(KSMCOR),   &
     &              AA(KOBTIM), AA(KOBSIG), AA(KIAUNO), AA(KEDSIG), &
     &              II(KINDH ), II(KNMH), AA(KS), AA(KS0), AA(KS1), &
     &              LL(KLEDIT), AA(KELEVS), AA(KDSCRP), JSTATS, &
     &              MTYPE, INDSTA, INDSAT, LELEVS, II(KISTNO), &
     &              AA(KSTNAM), AA(KXPMPA), LL(KLSDAT), 1, 1, &
     &              LL(KLSDAT), LL(KLFEDT),LEDIT_EXTRA )
        DEALLOCATE(LEDIT_EXTRA)
!          END IF
!
      END IF
      L1STV=.FALSE.
      RETURN
      END  SUBROUTINE  QVLBI
