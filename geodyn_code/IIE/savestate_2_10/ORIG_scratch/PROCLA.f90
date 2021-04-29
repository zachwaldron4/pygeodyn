!$PROCLA
      SUBROUTINE PROCLA(AA    ,II    ,LL    ,OBSC  ,RESID ,SIGMA ,      &
     &    RATIO ,PMPA  ,OBS   ,SUMCOR,OBSTIM,OBSSIG,XIAUNO,EDTSIG,      &
     &    IYMD  ,IHM   ,SEC   ,OBSU  ,RESIDU,LEDIT ,ELEVSC,DSCRPT,      &
     &    ISTATS,NTYPE ,INDSTA,INDSAT,LELEVS,ISTANO,STNAME,XPMPA,       &
     &    LSDATE,N1,N2,LRDEDT,LGEDIT,LEDIT_EXTRA)
!********1*********2*********3*********4*********5*********6*********7**
! PROCLA           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: LASER ALTIMETER (LATMOD=.TRUE.) RESIDUAL PROCESSING
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A
!   II      I/O   A
!   LL      I/O   A
!   OBSC
!   RESID
!   SIGMA
!   RATIO
!   PMPA
!   OBS
!   SUMCOR
!   OBSTIM
!   OBSSIG
!   XIAUNO
!   EDTSIG
!   IYMD
!   IHM
!   SEC
!   OBSU
!   RESIDU
!   LEDIT
!   ELEVSC
!   DSCRPT
!   ISTATS
!   NTYPE
!   INDSTA
!   INDSAT
!   LELEVS
!   ISTANO
!   STNAME
!   XPMPA   I/O A  CROSSOVER PARTIAL BOOKKEEPING ARRAY
!   LSDATE   I  A  LOGICAL FLAGS FOR OBSERVATION EDITING IN THE
!                  NEW SEA SURFACE TOPOGRAPHY MODEL
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CBLOKV/QUANO(3),EHAT(3),DEDA(3),DEDD(3),EFLG
      COMMON/CEDIT /EDITX ,EDTRMS,EDLEVL,CONVRG,GLBCNV,EBLEVL,EDITSW,   &
     &              ENPX  ,ENPRMS,ENPCNV,EDBOUN,FREEZI,FREEZG,FREEZA,   &
     &              XCEDIT
      COMMON/CEDITL/LNEDIT
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CKOUNT/KNTBLK,KSEGMN,KNTOBS,KZROBS
      COMMON/CLPRNR/LPR9NR(24),LPRNRM(24,3)
      COMMON/CNELVS/KNELEV(999)
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CNORML/LNORM ,LNORMP
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/COBSI /MODRES,NXCOBS
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
      COMMON/CORA05/KOBTYP,KDSCRP,KGMEAN,KGLRMS,KWGMEA,KWGRMS,KTYMEA,   &
     &       KTYRMS,KWTMTY,KWTYRM,KTMEAN,KTRMS ,KWMEAN,KWTRMS,KWTRND,   &
     &       KPRVRT,KEBSTT,KVLOPT,NXCA05
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
      COMMON/CORA07/KELEVT,KSRFEL,KTINT,KFSCSD,KXTIM,KFSDM,             &
     & KSGM1,KSGM2,KNOISE,KEPHMS,NXCA07
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
      COMMON/CORI05/KNOBGL,KNOBWG,KNOBTY,KNOBWY,KNOBST,KNOBWT,KNEBOB,   &
     &              KJSTAT,NXCI05
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CORL05/KLGPRV,NXCL05
      COMMON/CPARFL/LPARFI,LPFEOF,LARCNU,LRORR
      COMMON/CPARTL/LPART ,LPARTI,NXPARL
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/CSIML/LSIMTI
      COMMON/CUNIT9/L9SET ,L9RES ,L9RSST,L9RSTY,L9EBS ,L9APAR,L9SUMP,   &
     &              L9GPAR,L9UPDT,L9LAST,L9OUT ,NXUNT9
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)
      COMMON/GEDIT/NGOBS
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
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
      COMMON/ION2EL/LIONC,LFRQOP,LFRQOF,NXIONL
      COMMON/IRAMPS/INDRAM,KKRAMP
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
      COMMON/NUDITY/INUDP,INUDT,INUDF
      COMMON/PARPRC/NPROCS,NPROCI(5,10)
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
      COMMON/SHIRTS/LNSNS,NXNSNS
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/XOVER /KXKNT,KXBST,KXOBL,KXBUP,KXBPA,IAPLSC,IAPLL
      COMMON/XOVERS/NRXTOT,NXBLK,NUSIDE,NDEGX2,NDEGXX,NUCON,ITERNU,     &
     &              IPEDIT,IDEDIT,NXXOVR
!

!      CHARACTER*8     SIGMA
!     ....the coding in this subroutine uses the SIGMA array as both
!     ....a DOUBLEPRECISION array, and as a CHARACTER array
!     ....I have tried to make this work using the equivalenced variable
!     ....TMP_CHAR and TMP_DP  to transfer character data to SIGMA
!     ....Someday, another array should be defined and the two functions
!     ....the SIGMA array should be separated -- jjm 9/98

!      CHARACTER*8      DSCRPT
      CHARACTER*8      XNAME


      DIMENSION AA(*),II(*),LL(*),INDSAT(3,4),INDSTA(3,4),LELEVS(3,4)
      DIMENSION OBSC  (NM),RESID (NM),SIGMA (NM),RATIO (NM),            &
     &     PMPA(NM,NADJST),OBS   (NM),SUMCOR(NM),OBSTIM(NM),            &
     &          OBSSIG(NM),XIAUNO(NM),EDTSIG(NM),LEDIT (NM),            &
     &     ELEVSC(NM,  12),DSCRPT(7,1)
      DIMENSION QQ(4)
      DIMENSION SEC(NM),IHM(NM),IYMD(NM),OBSU(NM),RESIDU(NM)
!      CHARACTER*8      STNAME
      DIMENSION BLKSEG(2),TIMSYS(3),ISTANO(*),STNAME(*),XNAME(3)
      DIMENSION NUM(3)
      DIMENSION IRETIH(1500)
!     DIMENSION XPMPA(NRXTOT,*)
      DIMENSION XPMPA(N1,N2)
      DIMENSION LSDATE(NM)
      DIMENSION IELPR(2,6)
      DIMENSION LRDEDT(NM)
      DIMENSION LGEDIT(1)
      LOGICAL, INTENT(IN) :: LEDIT_EXTRA(NM)
!
      DATA METRIX/37/
      DATA ZERO/0.0D0/,ONE/1.0D0/,TENT/1.0D4/
      CHARACTER*1      BLANK, EDTFLG
      CHARACTER*8      BLKNEW, BLKOLD
      DATA BLANK/' ' /,EDTFLG/'*'/,BLKNEW/'(*NEW*)'/,BLKOLD/'(CONT.)'/
      CHARACTER*1      BLKSEG, TIMSYS, SEGMNT
      DATA BLKSEG/'A','B'/
      DATA TIMSYS/'R','X','T'/
      CHARACTER*8      HTOR, HTOD, HRADAL
      DATA HTOR/' T.O.R. '/,HTOD/' T.O.D. '/
      DATA HRADAL/' RADIAL '/

      CHARACTER*8      TMP_CHAR
      equivalence ( TMP_CHAR, TMP_DP )
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      do i=1,2
        do j=1,6
          IELPR(i,j) = 0
        enddo
      enddo
!      write(6,*) 'procla: nelvs,KNELEV(MTYPE) ',nelvs,KNELEV(MTYPE)
      NELVS=KNELEV(MTYPE)
      NELEVS=NELVS
      NELVP=NELEVS
!      write(6,*) 'procla: nelvs, nelevs, nelvp ', nelvs, nelevs, nelvp
!
! Set a conversion factor for simulated data replacement model
!
      IF(LSIMDT) THEN
         FACT = ONE/(ONE-VLITEC)
         IF(MTYPE.LT.METRIX) FACT=ONE
      ELSE
         FACT=ONE
      ENDIF
!      print *,'procla: lx1alt: ',lx1alt
!
      LNELEV=.TRUE.
      LNORML=LNORM.AND.NTYPE.GE.METRIX
!      print *,'procla: lnelev,lnorml: ',lnelev,lnorml
!
! LOAD OBSERVATIONS INTO OUTPUT ARRAY SCALING TO OUTPUT UNITS
!
      DO 200 N=1,NM
      OBSU  (N)=OBS   (N)*OBSCAL(NTYPE)
  200 END DO
!
! CORRECT OBSERVATIONS FOR SPEED OF LIGHT CHANGE
!
!       print *,'procla: before vlight scaling vlitec:',vlitec
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)
      DO 400 N=1,NM
!     RESID (N)=RESID (N)-OBS   (N)*VLITEC
  400 END DO
!      print *,'procla: after vlight scaling '
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)
!
! SET EDIT LOGICAL SWITCHES FOR CULLED OR ZERO SIGMA DATA
!
      DO 600 N=1,NM
      LEDIT (N)=OBSSIG(N).LE.ZERO.OR..NOT.LSDATE(N)
  600 END DO
      DO NN = 1, NM
          IF (LEDIT_EXTRA(NN)) THEN
              LEDIT(NN) = .TRUE.
          END IF
      END DO

!99300 FORMAT(' ** PROCES **  RESID BEFORE SUMCOR SUBTRACTED ='/
!    1   (1X,6D12.5))
! APPLY SUM OF OBSERVATION CORRECTIONS
!      print *,'procla: before sumcor 650 '
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)
      DO 650 N=1,NM
      RESID (N)=RESID (N)-SUMCOR(N)
  650 END DO
!      print *,'procla: after sumcor 650 '
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)

      IF(LACC) THEN
!      print *,'procla: before lacc 700 '
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)
      DO 700 N=1,NM
      RESID (N)=RESID (N)-AA(KNOISE-1+N)
  700 END DO
!      print *,'procla: after lacc 700 '
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)
      ENDIF
!      PRINT 99325,RESID
!99325 FORMAT(' ** PROCES **  RESID AFTER SUMCOR SUBTRACTED ='/
!     1   (1X,6D12.5))
!
! FILL EDITING INDICATOR WITH BLANKS
!
      DO 1100 N=1,NM
!      SIGMA(N)=BLANK           ! char jjm
                                ! char jjm
      TMP_CHAR=BLANK
                        ! char jjm
      SIGMA(N)=TMP_DP
 1100 END DO

!
! LOAD RESIDUALS INTO OUTPUT ARRAY SCALING TO OUTPUT UNITS
!
!      print *,'procla: loading for output array 1300 '
!      print *,'procla: ntype,rescal ',ntype,rescal(ntype)
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)
      DO 1300 N=1,NM
      RESIDU(N)=RESID (N)*RESCAL(NTYPE)
 1300 END DO
!      print *,'procla: after loading for output array 1300 '
!      print *,'procla: RESID(1): ',RESID(1)
!      print *,'procla: RESID(2): ',RESID(2)
!      print *,'procla: RESID(NM): ',RESID(NM)

      KSEGMN=KSEGMN+1
      SEGMNT=BLANK
!      print *,'procla: lnorml: ',lnorml
      IF(.NOT.LNORML) GO TO 1375
!
! FORM NORMAL POINT FITTED RESIDUALS HERE (METRIC OBSERVATIONS ONLY)!
!
      ISAT=INDSAT(1,1)
      ISATL1=ISAT-1
      IPXPK=KPXPK+36*ISATL1
      IDIAGN=KDIAGN+6*ISATL1
      IXPARM=KIXPAR+6*ISATL1
!cc      write(6,*) 'procla: before normed sumcor ',
!cc     &          (aa(ksmcor-1+iij),iij=1,nm)
      CALL NORMED(AA(IPXPK ),AA(IDIAGN),AA(KPMPA ),RESID     ,          &
     &            EDTSIG    ,OBSSIG    ,AA(KTPRTL),SIGMA     ,          &
     &            II(IXPARM),LEDIT     ,LL(KLEDT1),LL(KLEDT2),          &
     &            NM        ,ISAT      ,KNTBLK    ,SEGMNT    ,          &
     &            LL(KLAVOI))
 1375 CONTINUE
! CLEAR RATIO TO EDITING SIGMA ARRAY AND THEN
! COMPUTE RATIO OF RESIDUALS TO EDITING SIGMAS
!
      DO 1400 N=1,NM
      RATIO (N)=ZERO
!CC      WRITE(6,*) ' PROCES : EDTSIG,RESID  ',EDTSIG(N),RESID(N)
      IF(EDTSIG(N).LE.ZERO) GO TO 1400
      RATIO (N)=RESID (N)/EDTSIG(N)
 1400 END DO
!
! COMPUTE ABSOLUTE VALUE OF RATIO TO SIGMA
!
      DO 1600 N=1,NM
      SEC(N)=ABS(RATIO(N))
 1600 END DO
!
! EDIT LARGE RATIOS
!
      DO 1800 N=1,NM

      IF(SEC(N).LE.EDLEVL) GO TO 1700
      LEDIT(N)=.TRUE.
 1700 CONTINUE

      IF(LEDIT(N)) THEN
!         SIGMA(N)=EDTFLG       ! char
                                ! char
         TMP_CHAR=EDTFLG
                                ! char
         SIGMA(N)=TMP_DP
      ELSE
!         SIGMA(N) = BLANK      ! char
                                ! char
         TMP_CHAR = BLANK
                                ! char
         SIGMA(N) = TMP_DP
      ENDIF

 1800 END DO

      INDSYS=MOD(ITSYS,10000)/100
      INDSYS=MIN(MAX(INDSYS+1,1),3)
!
! OUTPUT UTC CALENDAR TIMES
!
      CALL YMDHMS(MJDSBL,OBSTIM,IYMD,IHM,SEC,NM)
      DO 2010 N=1,NM
      IF(IYMD(N).GT.1000000)IYMD(N)=IYMD(N)-1000000
 2010 END DO

!      print *,'procla: lresid: ',lresid
      IF(.NOT.LRESID) GO TO 7100
      L9RESD=L9RES.AND.L9OUT
!
! DETERMINE HOW MANY STATIONS THERE ARE
!
!      XNAME(1)=DSCRPT(2,ISTATS)
!      XNAME(2)=DSCRPT(4,ISTATS)
!      XNAME(3)=DSCRPT(6,ISTATS)

      TMP_DP=DSCRPT(2,ISTATS)
      XNAME(1)=TMP_CHAR
      TMP_DP=DSCRPT(4,ISTATS)
      XNAME(2)=TMP_CHAR
      TMP_DP=DSCRPT(6,ISTATS)
      XNAME(3)=TMP_CHAR

!
      DO 2110 I=1,3
         DO 2111 J=1,NSTA
!         IF(XNAME(I).EQ.STNAME(J)) THEN
         TMP_DP =  STNAME(J)
         IF(XNAME(I).EQ.TMP_CHAR) THEN
            NUM(I)=ISTANO(J)
         ENDIF
 2111    CONTINUE
 2110 END DO
!
! PRINT RESIDUAL HEADERS
!
      NLINE6=ILINE6+10
      IF(NLINE6.LE.MLINE6) GO TO 2200
      IPAGE6=IPAGE6+1
      IF(.NOT.LSIMDT) THEN
        IF (NELEVS.LE.3) THEN
          WRITE(IOUT6,10000,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,     &
     &    (DSCRPT(I,ISTATS),I=1,7),BLKNEW,(NUM(I),I=1,3),               &
     &    TIMSYS(INDSYS)
        ELSE
          WRITE(IOUT6,10001,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,     &
     &    (DSCRPT(I,ISTATS),I=1,7),BLKNEW,(NUM(I),I=1,3),               &
     &    TIMSYS(INDSYS)
        ENDIF
      ELSE
        IF (NELEVS.LE.3) THEN
          WRITE(IOUT6,10002,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,     &
     &    (DSCRPT(I,ISTATS),I=1,7),BLKNEW,(NUM(I),I=1,3),               &
     &    TIMSYS(INDSYS)
        ELSE
          WRITE(IOUT6,10003,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,     &
     &    (DSCRPT(I,ISTATS),I=1,7),BLKNEW,(NUM(I),I=1,3),               &
     &    TIMSYS(INDSYS)
        ENDIF
      ENDIF
      IF(NELVS.GT.0) THEN
         WRITE(IOUT6,10540,IOSTAT=IERR)                                 &
     &                    (IELPR(1,IQP),IELPR(2,IQP),IQP=1,NELVS)
      ENDIF
      ILINE6=4
      IF(NELVS.GT.0) ILINE6=ILINE6+1
      GO TO 2400
!
 2200 CONTINUE
      WRITE(IOUT6,10100,IOSTAT=IERR)                                    &
     &                   TIMSYS(INDSYS),(DSCRPT(I,ISTATS),I=1,7),       &
     &                   KNTBLK,SEGMNT
      WRITE(IOUT6,10101,IOSTAT=IERR) (NUM(I),I=1,3)
      IF(NELVS.GT.0) THEN
         WRITE(IOUT6,10540,IOSTAT=IERR)                                 &
     &                   (IELPR(1,IQP),IELPR(2,IQP),IQP=1,NELVS)
      ENDIF
      ILINE6=ILINE6+2
      IF(NELVS.GT.0) ILINE6=ILINE6+1
!
 2400 CONTINUE
      IF(.NOT.L9RESD) GO TO 2800
      NLINE9=ILINE9+9
      IF(NLINE9.LE.MLINE9) GO TO 2600
      IPAGE9=IPAGE9+1
      WRITE(IOUT9,10009,IOSTAT=IERR) NARC,IPAGE9,NINNER,NGLOBL,         &
     &   (DSCRPT(I,ISTATS),I=1,7)
      ILINE9=4
      GO TO 2800
!
 2600 CONTINUE
      WRITE(IOUT9,10109,IOSTAT=IERR) (DSCRPT(I,ISTATS),I=1,7)
      ILINE9=ILINE9+2
!
 2800 CONTINUE
!
! LOOP THRU AND PRINT BLOCK MEASUREMENTS IN GROUPS OF SIX
      MODULO=MIN(MODRES*6,MAX(NM-1,1))
      DO 7000 K=1,NM,MODULO
      NLINE6=ILINE6+8
      IF(NLINE6.LE.MLINE6) GO TO 3000
      IPAGE6=IPAGE6+1
      IF(.NOT.LSIMDT) THEN
        IF (NELEVS.LE.3) THEN
           WRITE(IOUT6,10000,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,    &
     &      (DSCRPT(I,ISTATS),I=1,7),BLKOLD,(NUM(I),I=1,3),             &
     &      TIMSYS(INDSYS)
        ELSE
           WRITE(IOUT6,10001,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,    &
     &      (DSCRPT(I,ISTATS),I=1,7),BLKOLD,(NUM(I),I=1,3),             &
     &      TIMSYS(INDSYS)
        ENDIF
      ELSE
        IF (NELEVS.LE.3) THEN
           WRITE(IOUT6,10002,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,    &
     &      (DSCRPT(I,ISTATS),I=1,7),BLKOLD,(NUM(I),I=1,3),             &
     &      TIMSYS(INDSYS)
        ELSE
           WRITE(IOUT6,10003,IOSTAT=IERR) NARC,NINNER,NGLOBL,IPAGE6,    &
     &      (DSCRPT(I,ISTATS),I=1,7),BLKOLD,(NUM(I),I=1,3),             &
     &      TIMSYS(INDSYS)
        ENDIF
      ENDIF
      IF(NELVS.GT.0) THEN
         WRITE(IOUT6,10540,IOSTAT=IERR)                                 &
     &              (IELPR(1,IQP),IELPR(2,IQP),IQP=1,NELVS)
      ENDIF
      ILINE6=4
      IF(NELVS.GT.0) ILINE6=ILINE6+1
!
 3000 CONTINUE
      ILINE6=ILINE6+1
      WRITE(IOUT6,10300)
      IF(.NOT.L9RESD) GO TO 5000
      NLINE9=ILINE9+7
      IF(NLINE9.LE.MLINE9) GO TO 4000
      IPAGE9=IPAGE9+1
      WRITE(IOUT9,10009,IOSTAT=IERR) NARC,IPAGE6,NINNER,NGLOBL,         &
     &   (DSCRPT(I,ISTATS),I=1,7)
      ILINE9=4
 4000 CONTINUE
!
      ILINE9=ILINE9+1
      WRITE(IOUT9,10300,IOSTAT=IERR)
 5000 CONTINUE
      KP5=K+MODULO-1
      KP5=MIN(KP5,NM)
      DO 6000 I=K,KP5,MODRES
      ILINE6=ILINE6+1
      IF(L9RESD) ILINE9=ILINE9+1
      KOUNTO=KNTOBS+I
      IF(LPREPW) CALL PREWRT(IYMD(I),IHM(I),SEC(I),KOUNTO,KNTBLK,       &
     &                       SEGMNT,NTYPE,SUMCOR(I),AA,I,RESCAL(NTYPE))
      IF(LNELEV) THEN
        IF(.NOT.LSIMDT) THEN
          WRITE(IOUT6,10400,IOSTAT=IERR)                                &
     &                IYMD(I),IHM(I),SEC(I),OBSU(I),RESIDU(I),          &
     &                RATIO(I),SIGMA(I), KOUNTO,KNTBLK,SEGMNT
        ELSE
          RSID= -1.0*RESIDU(I)*FACT
          WRITE(IOUT6,10510,IOSTAT=IERR) IYMD(I),IHM(I),SEC(I),RSID,    &
     &                       KOUNTO,KNTBLK,SEGMNT
        ENDIF
      ENDIF

      ILINE6=ILINE6+1
      JGX=KOBCOR(4,7)-1+I
      JOX=KOBCOR(3,4)-1+I
      IF(LPRE(13,1)) JOX=KOBCOR(4,4)-1+I
      JEX=KOBCOR(2,4)-1+I
      JSSX=KOBCOR(5,4)-1+I
      WRITE(IOUT6,10500,IOSTAT=IERR) AA(JGX),AA(JOX),AA(JEX),AA(JSSX)

      IF(LNELEV.AND.L9RESD)                                             &
     &   WRITE(IOUT9,10409,IOSTAT=IERR)                                 &
     &                     IYMD(I),IHM(I),SEC(I),OBSU(I),RESIDU(I),     &
     &                     RATIO(I),SIGMA(I), KOUNTO
      IF(LNELEV) GO TO 6000
        IF(.NOT.LSIMDT) THEN
          IF(NELEVS.LE.3)    THEN

                                                        ! jjm 9/98
             SELECT CASE( NELEVS )
                 CASE( 1 )
                    WRITE(IOUT6,10431) IYMD(I),IHM(I),SEC(I),OBSU(I),   &
     &                  RESIDU(I),RATIO(I),SIGMA(I),                    &
     &                  (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT
                 CASE( 2 )
                    WRITE(IOUT6,10432) IYMD(I),IHM(I),SEC(I),OBSU(I),   &
     &                  RESIDU(I),RATIO(I),SIGMA(I),                    &
     &                  (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT
                 CASE( 3 )
                    WRITE(IOUT6,10433) IYMD(I),IHM(I),SEC(I),OBSU(I),   &
     &                  RESIDU(I),RATIO(I),SIGMA(I),                    &
     &                  (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT
             END SELECT


!               ....ASSIGN is obsolete                  ! jjm 9/98
!!            IF (NELEVS.EQ.1 ) ASSIGN 10431 TO IFMT
!!            IF (NELEVS.EQ.2 ) ASSIGN 10432 TO IFMT
!!            IF (NELEVS.EQ.3 ) ASSIGN 10433 TO IFMT
!!            WRITE(IOUT6,IFMT) IYMD(I),IHM(I),SEC(I),OBSU(I),
!!     &       RESIDU(I),RATIO(I),SIGMA(I),(ELEVSC(I,J1),J1=1,NELEVS),
!!     &                     KOUNTO,KNTBLK,SEGMNT

          ELSE
            WRITE(IOUT6,10430,IOSTAT=IERR)                              &
     &       IYMD(I),IHM(I),SEC(I),OBSU(I),                             &
     &       RESIDU(I),RATIO(I),SIGMA(I),(ELEVSC(I,J1),J1=1,4),         &
     &       KOUNTO,KNTBLK,SEGMNT
          ENDIF
        ELSE
          RSID=-1.0*RESIDU(I)*FACT
          IF(NELEVS.LE.3)    THEN


                                                        ! jjm 9/98
             SELECT CASE( NELEVS )
                 CASE( 1 )
                    WRITE(IOUT6,10531) IYMD(I),IHM(I),SEC(I),RSID,      &
     &               (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT
                 CASE( 2 )
                    WRITE(IOUT6,10532) IYMD(I),IHM(I),SEC(I),RSID,      &
     &               (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT
                 CASE( 3 )
                    WRITE(IOUT6,10533) IYMD(I),IHM(I),SEC(I),RSID,      &
     &               (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT
             END SELECT



!               ....ASSIGN is obsolete                  ! jjm 9/98
!!            IF (NELEVS.EQ.1 ) ASSIGN 10531 TO IFMT
!!            IF (NELEVS.EQ.2 ) ASSIGN 10532 TO IFMT
!!            IF (NELEVS.EQ.3 ) ASSIGN 10533 TO IFMT
!!            WRITE(IOUT6,IFMT) IYMD(I),IHM(I),SEC(I),RSID,
!!     &       (ELEVSC(I,J1),J1=1,NELEVS),KOUNTO,KNTBLK,SEGMNT


          ELSE
            WRITE(IOUT6,10530,IOSTAT=IERR) IYMD(I),IHM(I),SEC(I),RSID,  &
     &       (ELEVSC(I,J1),J1=1,4),KOUNTO,KNTBLK,SEGMNT
          ENDIF
        ENDIF
      IF (NELEVS.GT.4)                                                  &
     &         WRITE(IOUT6,10420,IOSTAT=IERR) (ELEVSC(I,J2),J2=5,8)
      IF (NELEVS.GT.8)                                                  &
     &         WRITE(IOUT6,10420,IOSTAT=IERR) (ELEVSC(I,J3),J3=9,12)
      IF(L9RESD)                                                        &
     &   WRITE(IOUT9,10419,IOSTAT=IERR)                                 &
     &      IYMD(I),IHM(I),SEC(I),OBSU(I),RESIDU(I),                    &
     &      RATIO(I),SIGMA(I),ELEVSC(I,1),KOUNTO
 6000 END DO
 7000 END DO
      KNTOBS=KNTOBS+NM
 7100 CONTINUE
!
! COMPUTE RATIO OF RESIDUALS TO WEIGHTING SIGMAS AND COMPRESS OUT ZEROS
!    FROM RATIO OF RESIDUALS TO  EDITING  SIGMAS
!
      NMWTD=0
      DO 7400 N=1,NM
      NGOBS=NGOBS+1
      IF(LSAVEF) LGEDIT(NGOBS)=LEDIT(N)
      IF(LFREEZ) LEDIT(N)=LGEDIT(NGOBS)

! ADD COMMENT FOR CROSSOVERS
      IF(LEDIT(N)) GO TO 7400
      NMWTD=NMWTD+1
                                            ! DP
      SIGMA (NMWTD)=RESID (N)/OBSSIG(N)
      RATIO (NMWTD)=RATIO (N)
 7400 END DO

! SUM IN STATISTICS
!      print *,'procla: call to smstat'
      CALL SMSTAT  (AA(KTMEAN),AA(KTRMS ),AA(KWMEAN),AA(KWTRMS),        &
     &   AA(KWTRND),AA(KPRVRT),AA(KTYMEA),AA(KTYRMS),AA(KWTMTY),        &
     &   AA(KWTYRM),II(KNOBST),II(KNOBWT),II(KNOBTY),II(KNOBWY),        &
     &   LL(KLGPRV),RESIDU    ,NM        ,SIGMA     ,RATIO     ,        &
     &   NMWTD     ,NTYPE     ,ISTATS    )
                                                                    ! DP
!
! WRITE OUT INFORMATION TO BINARY RESIDUAL FILE IF REQUESTED
!
!CC      write(6,*) 'procla: before binrdr sumcor ',
!CC     &          (aa(ksmcor-1+iij),iij=1,nm)
!      print *,'procla: call to binrdr,lbinri: ',lbinri
      IF(LBINRI) CALL BINRDR(AA,II,MJDSBL,OBSTIM,EDTSIG,RESID,          &
     &           AA(KTPRTL),AA(KTHG),ELEVSC,NELEVS,LEDIT,RESIDU,NM,     &
     &           AA(KXUTDT))
!
! WRITE OUT OBSERVATIONS &  OBS DIRECTORY TO BINARY RESIDUAL FILE
!       IF REQUESTED
!     ....new location for BOD call  (old location was at end of OBSGEN)
!
!      print *,'procla: call to bod,lodri: ',lodri
      IF(LODRI) THEN
!CC         write(6,*) 'procla: CALL BOD '
         CALL BOD(AA,II,NM)
      ENDIF
!
!
!     ......new location for normal points
!
! COMPUTE AND OUTPUT NORMAL POINTS
!
!CC      write(6,*) 'procla: before normbn sumcor ',
!CC     &          (aa(ksmcor-1+iij),iij=1,nm)
!      print *,'procla: call to nrombn,lnorml: ',lnorml
      IF(LNORML) CALL NORMBN(AA,II,LL,MJDSBL    ,OBSTIM    ,IYMD      , &
     &          IHM       ,SEC       ,AA(KTPRTL),OBS       ,LEDIT     , &
     &          NM        ,II(KMJDBN),II(KNDBIN),II(KNWTDB),AA(KTMPCR), &
     &        AA(KRA   ),OBSCAL(NTYPE)        ,RESCAL(NTYPE)        ,   &
     &    DSCRPT(1,ISTATS),TIMSYS(INDSYS)       ,KNTBLK    ,SEGMNT    , &
     &          NTYPE     ,SUMCOR    )
!
!CC      write(6,*) 'procla: after  normbn sumcor ',
!CC     &          (aa(ksmcor-1+iij),iij=1,nm)
!
! EXIT IF NO OBSERVATIONS WEIGHTED
      IF(LACC)GOTO 9500
      IF(LNADJ) GO TO 9500
      IF(NMWTD.LE.0) GO TO 9500
!
! CLEAR OBSERVATION WEIGHT ARRAY
!
      DO 8100 N=1,NM
                                 ! DP
      SIGMA (N)=ZERO
 8100 END DO
!
! DEFINE OBSERVATIONS WEIGHTS
!
      NCLEAN=0
      DO 8200 N=1,NM
      IF(LEDIT(N)) GO TO 8200
      NCLEAN=NCLEAN+1
                                    ! DP
      SIGMA (N)=ONE/OBSSIG(N)**2
 8200 END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(LSTINR.AND.NCLEAN.GT.0) THEN
        JQ1=KOBCOR(6,4)
        JQ2=KOBCOR(7,4)
        JQ3=KOBCOR(8,4)
        JQ4=KOBCOR(9,4)
        WRITE(41,70000) NCLEAN,MJDSBL,BLKDAT(4,1)
        NCLEAN=0
        DO N=1,NM
          IF(.NOT.LEDIT(N)) THEN
            NCLEAN=NCLEAN+1
            QQ(1)=AA(JQ1-1+N)
            QQ(2)=AA(JQ2-1+N)
            QQ(3)=AA(JQ3-1+N)
            QQ(4)=AA(JQ4-1+N)
            WRITE(41,70001) NCLEAN,OBSTIM(N),OBS(N),QQ
          ENDIF
        ENDDO
      ENDIF
70000 FORMAT(2I12,F10.2)
70001 FORMAT(I6,6D25.16)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! CALL ESTIMATION SUMMATION ROUTINE
!
      IF(LNPNM) GO TO 9000
!
! PARTIALS ORDERED (NM,NP).
!
                                                                 ! DP
      CALL SUMNM (AA(KPMPA ),RESID ,SIGMA ,NADJST,MAPARM,NM,            &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KSRTCH), LL(KLAVOI))
      GO TO 9500
!
! PARTIALS ORDERED (NP,NM).
!
 9000 CONTINUE
      IF(LPARTI) GO TO 9200
      IF(NPROCS.GT.1.AND.LSTINR) GO TO 9150
      IF(NADJST-NAMBB.GT.3000) GO TO 9100
      IF(.NOT.LGPS) GOTO 9100
      CALL SUMNP (AA(KPMPA ),RESID ,SIGMA ,NADJST    , MAPARM    ,NM,   &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA),LL(KLAVOI))
      GO TO 9500
 9100 CONTINUE
      CALL SUMNPF(AA(KPMPA ),RESID ,SIGMA ,NADJST,MAPARM,NM,            &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA),LL(KLAVOI))
      GO TO 9500
 9150 CONTINUE
      CALL SUMNPG(AA(KPMPA ),RESID ,SIGMA ,NADJST,MAPARM,NM,            &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA),LL(KLAVOI))
      GO TO 9500
!
! PARTITIONED SUMMATION OF NORMAL MATRIX. PARTIALS ORDERED (NP,NM).
!
 9200 CONTINUE
                                                              ! DP
      CALL SUMNPP(AA(KPMPA ),RESID ,SIGMA ,MAPARM,NM,                   &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA))
 9500 CONTINUE
!      print *,'procla: after 9500'
!      print *,'procla:lnadj,lparfi: ',lnadj,lparfi
      IF(LSIMTI) CALL SHDDEL(NM,LEDIT,IYMD,IHM,SEC)
      IF(LSIMTI) CALL SIMDAT(AA,II,LL,OBS,RESID,SUMCOR,NTYPE,NM,NMWTD,  &
     & LEDIT)
      NELEVS=NELVP
      IF(LNADJ) RETURN
      IF(.NOT.LPARFI) RETURN
      JFSECM=KFSECS(1,1)
!      print *,'procla: call to pdfile'
      CALL PDFILE(IYMD,IHM,SEC,OBSTIM,OBS,AA(KTMCOR),SUMCOR,            &
     &   AA(KSMCR2),AA(KOBCOR(6,7)),AA(KDTIME),RESID,SIGMA,             &
     &   AA(JFSECM),AA(KXSM),AA(KVSM),AA(KSATLT),AA(KSATLN),AA(KSATH),  &
     &   AA(KXTN),AA(KVTN),AA(KXTI),AA(KVTI),INDSAT,INDSYS,             &
     &   AA(KPDLTA),AA(KPMPA),AA,II,LL,AA(KXUTDT))
                                                              ! DP
 9600 RETURN
!
10000 FORMAT('1',15X,'OBSERVATION RESIDUALS FOR ARC',                   &
     &   I3,' FOR INNER ITERATION',I3,' OF GLOBAL ITERATION',I2,17X,    &
     &   'UNIT  6 PAGE NO.',I6/                                         &
     &   2X,'STATION-SATELLITE CONFIGURATION ',7(1X,A8),22X,            &
     &   A7/23X,'STATION NUMBERS -',I8,12X,I8,12X,I8,                   &
     &   /2X,'DATE  GREENWICH TIME',6X,'OBSERVATION',9X,                &
     &   'RESIDUAL',2X,'RATIO TO SIGMA',2X,3('ELEVATION',3X),1X,        &
     &   'OBS NO.',2X,'BLOCK'/1X,'YYMMDD HHMM SEC-UTC-',A1)
!    6   'RESIDUAL',2X,'RATION TO SIGMA',2X,3('ELEVATION',3X),1X,
10001 FORMAT('1',15X,'OBSERVATION RESIDUALS FOR ARC',                   &
     &   I3,' FOR INNER ITERATION',I3,' OF GLOBAL ITERATION',I2,17X,    &
     &   'UNIT  6 PAGE NO.',I6/                                         &
     &   2X,'STATION-SATELLITE CONFIGURATION ',7(1X,A8),22X,            &
     &   A7/23X,'STATION NUMBERS -',I8,12X,I8,12X,I8,                   &
     &   /2X,'DATE  GREENWICH TIME',6X,'OBSERVATION',9X,                &
     &   'RESIDUAL',2X,'RATIO TO SIGMA',4X,4('ELEVAT',4X),1X,           &
     &   'OBS NO.',2X,'BLOCK'/1X,'YYMMDD HHMM SEC-UTC-',A1)
10002 FORMAT('1',15X,'OBSERVATION RESIDUALS FOR ARC',                   &
     &   I3,' FOR INNER ITERATION',I3,' OF GLOBAL ITERATION',I2,17X,    &
     &   'UNIT  6 PAGE NO.',I6/                                         &
     &   2X,'STATION-SATELLITE CONFIGURATION ',7(1X,A8),22X,            &
     &   A7/23X,'STATION NUMBERS -',I8,12X,I8,12X,I8,                   &
     &   /2X,'DATE  GREENWICH TIME',6X,'SIM.OBSERVATION',9X,            &
     &   2X,3('ELEVATION',3X),1X,                                       &
     &   'OBS NO.',2X,'BLOCK'/1X,'YYMMDD HHMM SEC-UTC-',A1)
10003 FORMAT('1',15X,'OBSERVATION RESIDUALS FOR ARC',                   &
     &   I3,' FOR INNER ITERATION',I3,' OF GLOBAL ITERATION',I2,17X,    &
     &   'UNIT  6 PAGE NO.',I6/                                         &
     &   2X,'STATION-SATELLITE CONFIGURATION ',7(1X,A8),22X,            &
     &   A7/23X,'STATION NUMBERS -',I8,12X,I8,12X,I8,                   &
     &   /2X,'DATE  GREENWICH TIME',6X,'SIM.OBSERVATION',9X,            &
     &   4('ELEVAT',4X),1X,                                             &
     &   'OBS NO.',2X,'BLOCK'/1X,'YYMMDD HHMM SEC-UTC-',A1)
10100 FORMAT('0YYMMDD HHMM SEC-UTC-',A1,                                &
     &   ' ** STATION-SAT CONFIG.',7(1X,A8),' ** FOR NEW BLOCK',I6,A1)
20100 FORMAT('  ALT OBS 1  ALT OBS 2 ',                                 &
     &   '        RESIDUAL          SIGMA      RATIO TO SIGMA ',        &
     &   '**  FOR BLOCK',I6,A1,13X,'XOVER COUNT  BLOCK')
10101 FORMAT(22X,' ** STATION NUMBERS -         ',I8,10X,I8,10X,I8)
10300 FORMAT(1X)
10400 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,A1,1X,       36X,         &
     &       I9,I8,A1)
20400 FORMAT(1X,2F10.1,F20.9,F15.6,F12.4,37X,                           &
     &       I9,I8,A1)
10410 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,A1,1X, F12.6,24X,         &
     &       I9,I8,A1)
10420 FORMAT(72X,4F10.3,12X)
10431 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,A1,1X,F12.6,24X,          &
     &       I9,I8,A1)
10432 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,A1,1X,2F12.6,12X,         &
     &       I9,I8,A1)
10433 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,A1,1X,3F12.6,             &
     &       I9,I8,A1)
10430 FORMAT(1X,I6,I5,F10.6,F20.9,F15.6,F12.4,A1,2X,4F10.3,             &
     &       I9,I8,A1)
10510 FORMAT(1X,I6,I5,5X,F10.6,F16.6,43X,I9,I8,A1)
10530 FORMAT(1X,I6,I5,5X,F10.6,F16.6,9X,4F10.3,1X,I9,I8,A1)
10531 FORMAT(1X,I6,I5,5X,F10.6,F16.6,8X,F12.6,24X,I9,I8,A1)
10532 FORMAT(1X,I6,I5,5X,F10.6,F16.6,8X,2F12.6,12X,I9,I8,A1)
10533 FORMAT(1X,I6,I5,5X,F10.6,F16.6,8X,3F12.6,1X,I9,I8,A1)
10009 FORMAT('1OBSERVATION RESIDUALS FOR ARC',I3,                       &
     &        25X,'UNIT  9 PAGE NO.',I4/                                &
     &   ' FOR INNER ITERATION',I3,' OF GLOBAL ITERATION',I2/           &
     &   ' STA-SAT CONFIG',7(1X,A8)/                                    &
     &   '  DATE  GREENWICH TIME',6X,'OBSERVATION',4X,                  &
     &   'RESIDUAL',2X,'RES/SIG',4X,'ELEV.',2X,'OBS NO.')
10109 FORMAT('0STA-SAT CONFIG',7(1X,A8))
10409 FORMAT(1X,I6,I5,F10.6,F17.6,F12.4,F 9.3,A1,1X, 8X ,1X,I7)
10419 FORMAT(1X,I6,I5,F10.6,F17.6,F12.4,F 9.3,A1,1X,F8.4,1X,I7)
10500 FORMAT('         GEOID',F11.3,'    OTIDE',F9.3,                   &
     &       '   ETIDE',F9.3,'   SST',F9.3)
10540 FORMAT(' EL ANGLES ',6(4X,'(',I9,',', I8,')'))
      END
