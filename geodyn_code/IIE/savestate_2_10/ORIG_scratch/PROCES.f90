!$PROCES
       SUBROUTINE PROCES(AA    ,II    ,LL    ,OBSC  ,RESID ,SIGMA ,  &
      &           RATIO ,PMPA  ,OBS   ,SUMCOR,OBSTIM,                &
      &           OBSSIG,XIAUNO,EDTSIG, IYMD, IHM, SEC,              &
      &           OBSU  ,RESIDU,LEDIT ,ELEVSC,DSCRPT,ISTATS,NTYPE ,  &
      &           INDSTA,INDSAT,LELEVS,ISTANO,STNAME,XPMPA,          &
      &           LSDATE,N1,N2,LRDEDT,LGEDIT,LEDIT_EXTRA)


!********1*********2*********3*********4*********5*********6*********7**
! PROCES           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
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
!   LEDIT_EXTRA I A  IF LEDIT_EXTRA(N) IS TRUE, EDIT OUT NTH MEASUREMENT
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      CHARACTER*80 CARD1,CARD2,CARD3
      SAVE
!
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CMB85/XN85(2),RMS85(2)
      COMMON/DCORR/XNDDOR
      COMMON/ACCELO/IDYNDL(3),IDYNSN(3),IGEOSN(3),NXACCO
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
      COMMON/C12PRM/IDSATS(10,2),IP1,IP2,NXC12P
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
      COMMON/CEBARC/NEBIAS,NBIASE,NXCEBA
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
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
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
      COMMON/DWNWGT/LDWNWT,LDNWMT(999),NXDWNW
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      COMMON/HUBERC/CHUBER(200),XHUBER
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
      COMMON/PARPRO/MPIPRO
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      COMMON/RAMPS/TRINT(1000,2,2),FRINT(1000,2,2),TRAMP(1000,2),       &
     &             FRAMP(1000),FDRAMP(1000),RSCALX(1000),SCALX(1000),   &
     &             XINTS(1000),XINTB(1000),XINTS2(1000),RSCALY(1000)
      COMMON/RESBIG/AMBIGR(1000)
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
      COMMON/SHIRTS/LNSNS,NXNSNS
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/TPARM/MTPARM,MTPRM1,IPS51,IPS52,MTOBS,IT2PRM(2,28000)
      COMMON/XOVER /KXKNT,KXBST,KXOBL,KXBUP,KXBPA,IAPLSC,IAPLL
      COMMON/XOVERS/NRXTOT,NXBLK,NUSIDE,NDEGX2,NDEGXX,NUCON,ITERNU,     &
     &              IPEDIT,IDEDIT,NXXOVR
      COMMON/XPARMB/MPARMB,NPARMB,MPOBS,JPAMB,ISKIPE,                   &
     &             ITPARM(5,28000),ISTABP(3,28000),                     &
     &             ISATBP(3,28000),IPRMBF(28000)
      COMMON / XRESID_TMP / SUM_SQ_WEIGHTS, SUM_SQ_OBSERVABLES, &
     &                      SUM_SQ_CONTRIB
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
      DIMENSION DCORQ(10000)
      DIMENSION OBSTMQ(2)
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

!...Begin TJS DEBUG
      DATA L1ST /.TRUE./
!...End TJS DEBUG

      CHARACTER*8      TMP_CHAR
      equivalence ( TMP_CHAR, TMP_DP )
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
70001 FORMAT(I7)    
      IF(LSTINR.AND.MTYPE.EQ.65) THEN
        DO I=1,80
         CARD1(I:I)=' '
         CARD2(I:I)=' '
         CARD3(I:I)=' '
        ENDDO
        CARD1(1:6)='DELETE'
        CARD1(15:17)='165'
        WRITE(CARD1(18:24),70001) JSATNO(1)
        CARD2(1:6)='DELETE'
        CARD2(15:17)='265'
        WRITE(CARD2(18:24),70001) JSATNO(2)
        CARD3(1:6)='DELETE'
        CARD3(15:17)='365'
        WRITE(CARD3(18:24),70001) JSATNO(3)
      ENDIF
!...Begin TJS DEBUG
      IF (L1ST) TQ0=DBLE(MJDSBL)+OBSTIM(1)
      L1ST=.FALSE.
!...End TJS DEBUG

      IF(NTYPE.EQ.36) THEN
         DO I=1,NADJST
           AA(KPMPA-1+I)=1.D9*AA(KPMPA-1+I)/VLIGHT
         ENDDO
      ENDIF

!!    write(6,'(A,2(1x,I4))') 'proces: MTYPE, NTYPE ', MTYPE, NTYPE

      IF(IP1.GT.0.AND.IP2.GT.0) THEN
      IF(LNPNM) THEN
      KK=NADJST
      MM=NM
      ELSE
      KK=NM
      MM=NADJST
      ENDIF
      CALL SWITCH(AA(KPMPA),KK,MM,LNPNM)
      ENDIF
       IF(XGPSTB.GT.0.D0) THEN
         IPX=PYUSQ(5)
         IQP=(IPX-1)*4+KPYSQH-1
         AA(IQP+1)=PYUSQ(1)
         AA(IQP+2)=PYUSQ(2)
         AA(IQP+3)=PYUSQ(3)
         AA(IQP+4)=PYUSQ(4)
       ENDIF
      RESHLD=RESCAL(NTYPE)
      OBSHLD=OBSCAL(NTYPE)
      !write(6,*)'proces: ntype, RESHLD, OBSHLD ', ntype, RESHLD, OBSHLD
      IF(INDRAM.GT.0) RESCAL(NTYPE)=1.D0
      IF(INDRAM.GT.0) OBSCAL(NTYPE)=1.D0
      IF(LITER1.AND.INDRAM.GT.0.AND.(NTYPE.EQ.54.OR.NTYPE.EQ.42)) THEN
         DO 2 I=1,NM
         EDTSIG(I)=EDTSIG(I)*100.D0
         OBSSIG(I)=OBSSIG(I)*100.D0
    2    CONTINUE
         !WRITE(6,*) 'proces:2  EDTSIG ', EDTSIG(1:NM)
         !WRITE(6,*) 'proces:2  OBSSIG ', OBSSIG(1:NM)
      ENDIF
      do i=1,2
        do j=1,6
          IELPR(i,j) = 0
        enddo
      enddo
      !write(6,'(A,2(1x,E15.7))') 'proces: RESHLD, OBSHLD ', RESHLD, OBSH

!      write(6,*) 'proces: nelvs,KNELEV(MTYPE) ',nelvs,KNELEV(MTYPE)
      NELVS=KNELEV(MTYPE)
      NELEVS=NELVS
      NELVP=NELEVS
!      write(6,*) 'proces: nelvs, nelevs, nelvp ', nelvs, nelevs, nelvp
      IF(NELVS.GT.0) THEN
         NELEVS=NELVS
         DO 10 I=1,NELVS
         IF(IELSTA(I).LE.0) GO TO 5
         IELPR(1,I)=ISTANO(IELSTA(I))
         IELPR(2,I)=II(KISATN-1+IELSAT(I))
!         write(6,*) 'proces:  i, IELPR(,I) ', I,IELPR(1,I),IELPR(2,I)
         GO TO 10
    5    CONTINUE
         IELPR(1,I)=II(KISATN-1+IELSAT(I))
         IELPR(2,I)=II(KISATN-1+IELSA2(I))
!         write(6,*) 'proces:  i, IELPR(,I) ', I,IELPR(1,I),IELPR(2,I)
   10    CONTINUE
      ENDIF
!
! Set a conversion factor for simulated data replacement model
!
      IF(LSIMDT) THEN
         FACT = ONE/(ONE-VLITEC)
         IF(MTYPE.LT.METRIX) FACT=ONE
      ELSE
         FACT=ONE
      ENDIF
       IF(LX1ALT) THEN
       DO 20 I=1,NM
       AA(KXOBSV+I-1)=0.D0
       AA(KXOBSW+I-1)=0.D0
       AA(KXEDSW+I-1)=0.D0
       AA(KXSIGW+I-1)=0.D0
   20  CONTINUE
       ENDIF
!
! PRINTS OUT T.O.D IF PCE DATA IS TRUE OF DATE OR RADIAL IF PCE
! DATA IS RADIAL OR T.O.R. IF PCE DATA IS TRUE OF REFERANCE
      IF(MTYPE.LE.12) THEN
      IF(.NOT.LPRNRM(23,1)) THEN
!      DSCRPT(2,ISTATS)=HRADAL
      TMP_CHAR=HRADAL
      DSCRPT(2,ISTATS)=TMP_DP
      GOTO 30
      ENDIF
      IF(LPRNRM(22,1)) THEN
!      DSCRPT(2,ISTATS)=HTOR
      TMP_CHAR=HTOR
      DSCRPT(2,ISTATS)=TMP_DP
      GOTO 30
      ENDIF
!      DSCRPT(2,ISTATS)=HTOD
      TMP_CHAR=HTOD
      DSCRPT(2,ISTATS)=TMP_DP
      ENDIF
   30 CONTINUE
!
      LNELEV=NTYPE.LT.13.OR.NTYPE.GT.98.OR.NELEVS.LE.0
      LNORML=LNORM.AND.NTYPE.GE.METRIX
!CCCC
!CCCC DEBUG
!CCCC
!     WRITE(6,*) 'IN PROCES: ISTANO ', (ISTANO(I),I=1,4)
!     WRITE(6,100) (STNAME(I),I=1,15)
!100   FORMAT ('IN PROCES: STNAME ',15A8)
!
! LOAD OBSERVATIONS INTO OUTPUT ARRAY SCALING TO OUTPUT UNITS
!
      !WRITE(6,'(A,1x,I4,1x,E15.7)') 'proces: NTYPE, OBSCAL ', &
      !                                       NTYPE, OBSCAL(NTYPE)
      DO 200 N=1,NM
      OBSU  (N)=OBS   (N)*OBSCAL(NTYPE)
  200 END DO
      IF(MTYPE.LT.31) GO TO 500
      IF(MTYPE.GT.98.AND.MTYPE.LT.200) GO TO 350
      IF(MTYPE.GT.203) GO TO 350
!
! COMPUTE RESIDUALS FOR METRIC AND VLBI OBSERVATIONS
!
      IF(MTYPE.LT.200) GO TO 290

      DO 220 N=1,NM
      RESID (N)=OBSC  (N)
      OBSC(N)=OBS(N)-RESID(N)
  220 END DO
      GO TO 350

  290 CONTINUE

      !WRITE(6,'(A,2(1x,I3))') 'proces: aft 290 MTYPE, NTYPE = ', &
      !                                         MTYPE, NTYPE

      DO 300 N=1,NM
      RESID (N)=OBS   (N)-OBSC  (N)
      !if( n == 1 )then
      !WRITE(6,'(A,1x,I3,3(1x,E24.16))') &
      !      ' PROCES:300 n, obs, obsc, resid ', &
      !                   n, obs(n), obsc(n), resid(n)
      !endif
  300 END DO
  350 CONTINUE

! CORRECT METRIC OBSERVATIONS FOR SPEED OF LIGHT CHANGE
! AND SKIP FOR VLBI MEASUREMENTS  ****

      IF (MTYPE.EQ.31.OR.MTYPE.EQ.36) GO TO 500
      DO 400 N=1,NM
      RESID (N)=RESID (N)-OBS   (N)*VLITEC
  400 END DO
  500 CONTINUE

      !WRITE(6,'(A,1x,I3,3(1x,E24.16))') &
      !      ' PROCES:aft500 1, obs, obsc, resid ', &
      !                      1, obs(1), obsc(1), resid(1)

! IF SIMDAT CARDS ARE INCLUDED AND THE IONOSPHERIC OPTION IS SELECTED
! SKIP CODE WHICH MAY CAUSE OBSERVATIONS TO BE OUTPUT ON UNIT 20
! TO BE EDITED OR SKIPPED.
      IF(LIONC.AND.LSIMDT) GOTO 599
!
! SET EDIT LOGICAL SWITCHES FOR CULLED OR ZERO SIGMA DATA
!

      DO 600 N=1,NM
      LEDIT (N)=OBSSIG(N).LE.ZERO.OR..NOT.LSDATE(N)
  600 END DO
  599 CONTINUE

      IF (MTYPE == 31) THEN

! IF SIMDAT CARDS ARE INCLUDED AND THE IONOSPHERIC OPTION IS SELECTED
! SKIP CODE WHICH MAY CAUSE OBSERVATIONS TO BE OUTPUT ON UNIT 20
! TO BE EDITED OR SKIPPED. ** IONOSPHERIC OPTION NOT YET AVAILABLE FOR
! VLBI DATA **
      IF(LIONC.AND.LSIMDT) GOTO 602
!  SET EDIT LOGICALS FOR VLBI DELAY DATA USING INPUT EDITING FLAG
      DO 601 N=1,NM
      LEDIT (N)=EFLG.GT.TENT
!     WRITE(6,*)  ' PROCES  N , EFLG ,LEDIT ',N,EFLG,LEDIT(N)
  601 END DO
  602 CONTINUE

      ENDIF ! MTYPE == 31

      IF(INUDF.LT.0.AND.LNSNS) THEN
        INUDP=INUDP+1
        INUDT=INUDT+NM
        DO 603 N=1,NM
        LEDIT (N)=.TRUE.
  603   CONTINUE
      ENDIF
!
! IF THIS BLOCK IS ASSOCIATED WITH A BAD BIAS, DELETE THE OBSERVATIONS
      IF(ISKIPE.LT.0) THEN
        DO N=1,NM
          LEDIT(N)=.TRUE.
        ENDDO
      ENDIF
! IF SIMDAT CARDS ARE INCLUDED AND THE IONOSPHERIC OPTION IS SELECTED
! SKIP CODE WHICH MAY CAUSE OBSERVATIONS TO BE OUTPUT ON UNIT 20
! TO BE EDITED OR SKIPPED.
      IF(LIONC.AND.LSIMDT) GOTO 610
!
! EDIT OBSERVATIONS BASED ON ELEVATION CUTOFF
!
      IF(.NOT.LNELEV.AND.NELVS.LE.0) CALL ELVCUT(LEDIT ,ELEVSC,NM,      &
     &                               AA(KSTAIN),ISTAEL,NELEVS,          &
     &                               LL(KLANTC))
      IF(INDRAM.EQ.0) THEN
      IF(.NOT.LNELEV.AND.NELVS.GT.0) CALL ELVCUT(LEDIT ,ELEVSC,NM,      &
     &                             AA(KSTAIN),IELSTA,NELVS,LL(KLANTC))
      ELSE
      IF(.NOT.LNELEV.AND.NELVS.GT.0) CALL ELVCUT(LEDIT ,ELEVSC,NM,      &
     &                             AA(KSTAIN),ISTAEL,NELVS,LL(KLANTC))
      ENDIF
  610 CONTINUE
      JNDRAM=INDRAM
      INDRAH=INDRAM
      INDRAM=0
! IF SIMDAT CARDS ARE INCLUDED AND THE IONOSPHERIC OPTION IS SELECTED
! SKIP CODE WHICH MAY CAUSE OBSERVATIONS TO BE OUTPUT ON UNIT 20
! TO BE EDITED OR SKIPPED.
      IF(LIONC.AND.LSIMDT) GOTO 620
!
! EDIT SATELLITE TO SATELLITE MEASUREMENTS BASED ON IONOSPHERE CUTOFF
!
      IF(LGPS) GOTO 620
      IF(NTYPE.EQ.57 .OR. NTYPE.EQ.58 .OR. NTYPE.EQ.56)                 &
     & CALL IONEDT(LEDIT,NM,AA(KXSM),AA(KXTK))
!     PRINT 99300,RESID
  620 CONTINUE

!99300 FORMAT(' ** PROCES **  RESID BEFORE SUMCOR SUBTRACTED ='/
!    1   (1X,6D12.5))

!-----------------------------------------------------------------------------

! APPLY SUM OF OBSERVATION CORRECTIONS

      IF(JNDRAM.EQ.0.OR.NTYPE.NE.54) THEN
       IF(MTYPE.EQ.36) GOTO 649

      DO 650 N=1,NM

      !if( N == 1 )then
      !    write(6,'(A,1x,I3,2(1x,E24.16))')&
      !          'proc:650bef N, RESID(N), SUMCOR(N) ', &
      !                       N, RESID(N), SUMCOR(N)
      !endif

      RESID(N)=RESID(N) - SUMCOR(N)

      !if( N == 1 )then
      !    write(6,'(A,1x,I3,2(1x,E20.10))')&
      !          'proc: aft N, RESID(N)           ', &
      !                     N, RESID(N)
      !endif

  650 END DO
  649 CONTINUE
       IF(MTYPE.EQ.36) THEN
      DO 641 N=1,NM
      RESID (N)=RESID (N)-((SUMCOR(N)/VLIGHT)*1.D09)
  641 END DO
      dd=(resid(1)/1.D+09)*VLIGHT
      write(6,*)'                           '
      write(6,*)' RESIDUAL IN METERS',dd
      write(6,*)'                           '
       ENDIF


      ENDIF !  JNDRAM.EQ.0.OR.NTYPE.NE.54

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          IF(MTYPE.EQ.36.AND.NPVAL0(IXFAGM).EQ.1) THEN
          IPT=IPVAL0(IXFAGM)-1
          LL(KLAVOI+IPT)=.FALSE.
          IF(LNPNM) THEN
             DO I=1,NM
               JPT=(I-1)*NADJST+IPT
               AA(KPMPA+JPT)=1.D0
               RESID(I)=RESID(I)                                     &
     &                 -AA(KPMPA+JPT)*AA(KPRMVC+IPT)
             ENDDO
          ELSE
             DO I=1,NM
               JPT=(IPT)*NM+I-1
               AA(KPMPA+JPT)=1.D0
               RESID(I)=RESID(I)                                     &
     &                 -AA(KPMPA+JPT)*AA(KPRMVC+IPT)
             ENDDO
          ENDIF
       ENDIF
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!------------------------------------------------------------------------------

      IF(NPVAL0(IXOFFS).GT.0) THEN

         CALL OFFCOR(LNPNM,NADJST,NM,RESID,AA(KPRMV0),AA(KPRMVC),    &
     &               AA(KPMPA),AA(KPMPA))

         !do  n = 1, nm
         !    if( N == 1 )then
         !        write(6,'(A,1x,I3,1x,E24.16)')&
         !              'proc: aft650 offcor    N, RESID(N) ', N, RESID(N
         !    endif
         !enddo

      ENDIF

!...Begin TJS DEBUG
!     IF (JSTANO(1).EQ.JSTANO(2)) THEN
!        IF (LNPNM) THEN
!           CALL DUMPCOL (JSTANO(1),TQ0,MJDSBL,OBSTIM,RESID,AA(KPMPA),  &
!    &                    NM,NADJST,IPVAL0(IXXTRO),IPVAL0(IXFGFM))
!        ELSE
!           CALL DUMPROW (JSTANO(1),TQ0,MJDSBL,OBSTIM,RESID,AA(KPMPA),  &
!    &                    NM,NADJST,IPVAL0(IXXTRO),IPVAL0(IXFGFM))
!        END IF
!     END IF
!...End TJS DEBUG


!-------------------------------------------------------------------------------

      !write(6,'(A,3x,L1)') 'proc: LACC ', LACC

      IF(LACC) THEN
      DO 700 N=1,NM
      RESID (N)=RESID (N)-AA(KNOISE-1+N)
  700 END DO
      !do  n = 1, 1 ! nm
      !    write(6,'(A,1x,I4,1x,E24.16)')&
      !          'proc: aft 700 N, RESID(N) ', N, RESID(N)
      !enddo
      ENDIF  ! LACC


      !write(6,99325) RESID
99325 FORMAT(' ** PROCES **  RESID AFTER SUMCOR SUBTRACTED ='/&
             (1X,5D24.16))
!
! FILL EDITING INDICATOR WITH BLANKS
!
      DO 1100 N=1,NM
!      SIGMA(N)=BLANK   ! char jjm
      TMP_CHAR=BLANK    ! char jjm
      SIGMA(N)=TMP_DP   ! char jjm
 1100 END DO
!
! REMOVE ADJUSTED BIASES FROM RESIDUALS AND SOLVE FOR ELECTRONIC BIASES
!
! RAMP ******
      IF(INDRAM.GT.0.AND.LITER1.AND.NGLOBL.EQ.1) THEN
      RESCAL(NTYPE)=1.D0
      DO 1200 N=1,NM
      EDTSIG(N)=EDTSIG(N)*100.D0
      OBSSIG(N)=OBSSIG(N)*100.D0
 1200 END DO
      ENDIF
! RAMP ******
      LBEDIT=.FALSE.
      LSCALE=.FALSE.
! IF SIMDAT CARDS ARE INCLUDED AND THE IONOSPHERIC OPTION IS SELECTED
! SKIP CODE WHICH MAY CAUSE OBSERVATIONS TO BE OUTPUT ON UNIT 20
! TO BE EDITED OR SKIPPED.
      IF(LIONC.AND.LSIMDT) GOTO 1210
      !WRITE(6,*) 'PROCES: CALL BIASES'
      INDRAM=INDRAH

      !write(6,'(A,1x,I4,1x,E24.16)')&
      !          'proc: bef call biases 1, RESID(1) ', 1, RESID(1)

      CALL BIASES(AA(KPMPA),RESID ,RESIDU,EDTSIG,RATIO ,OBSSIG,SIGMA ,  &
     &LEDIT ,NDIM1 ,NDIM2 ,NM    ,NTYPE ,ISTATS,LBEDIT,LNEDIT,          &
     &RESCAL(NTYPE),LSCALE,AA    ,II    ,LL    )

      !write(6,'(A,1x,I4,1x,E24.16)')&
      !          'proc: aft call biases 1, RESID(1) ', 1, RESID(1)
      INDRAM=0
 1210 CONTINUE

!!!!  IF(ICBDGM.NE.3) THEN
       DO IQP=1,NM
        IF(AMBIGR(IQP).GT.0.D0) THEN
          AMBIGH=AMBIGR(IQP)*.5D0
          RESID(IQP)=MOD(RESID(IQP),AMBIGR(IQP))
          IF(RESID(IQP).GT. AMBIGH) RESID(IQP)=RESID(IQP)-AMBIGR(IQP)
          IF(RESID(IQP).LT.-AMBIGH) RESID(IQP)=RESID(IQP)+AMBIGR(IQP)
        ENDIF
       ENDDO
!!!!  ENDIF

!------------------------------------------------------------------------------

      ! FANTOM  CGMASS OFFSET

!     IF( NPVAL(IXFAGM).GT.0) THEN

          !do  n = 1, nm
          !    if( N == 1 )then
          !    write(6,'(A,1x,I4,1x,E20.10)')&
          !          'proc: bef cgcor  N, RESID(N) ', N, RESID(N)
          !    endif
          !enddo

          !write(6,'(A)')'proc: call CGCOR '

!       CALL CGCOR( LNPNM,NADJST,NM,RESID,AA(KPRMV0),AA(KPRMVC),&
!    &              AA(KPMPA),AA(KPMPA))
          !do  n = 1, nm
          !    if( N == 1 )then
          !        write(6,'(A,1x,I4,1x,E20.10)')&
          !              'proc: aft cgcor     N, RESID(N) ', N, RESID(N)
          !    endif
          !enddo

!     ENDIF

!------------------------------------------------------------------------------

      !write(6,'(A,3x,L1)') 'proc: LSCALE ', LSCALE

      IF(LSCALE) GO TO 1350

! LOAD RESIDUALS INTO OUTPUT ARRAY SCALING TO OUTPUT UNITS

      DO 1300 N=1,NM
      RESIDU(N)=RESID (N)*RESCAL(NTYPE)
      !if( n == 1 )then
      !    write(6,'(A,1x,I4,3(1x,E24.16))')&
      !          'proc:1300 N, RESID(N), RESCAL(NTYPE), RESIDU(N) ', &
      !                     N, RESID(N), RESCAL(NTYPE), RESIDU(N)
      !endif
 1300 END DO
 1350 CONTINUE
      KSEGMN=KSEGMN+1
      SEGMNT=BLANK
      IF(NTYPE.GT.12.AND.NTYPE.LT.31) SEGMNT=BLKSEG(KSEGMN)
      IF(.NOT.LNORML) GO TO 1375

! FORM NORMAL POINT FITTED RESIDUALS HERE (METRIC OBSERVATIONS ONLY)!

      ISAT=INDSAT(1,1)
      ISATL1=ISAT-1
      IPXPK=KPXPK+36*ISATL1
      IDIAGN=KDIAGN+6*ISATL1
      IXPARM=KIXPAR+6*ISATL1

!cc      write(6,*) 'proces: before normed sumcor ',
!cc     &          (aa(ksmcor-1+iij),iij=1,nm)

      CALL NORMED(AA(IPXPK ),AA(IDIAGN),AA(KPMPA ),RESID     ,          &
     &            EDTSIG    ,OBSSIG    ,AA(KTPRTL),SIGMA     ,          &
     &            II(IXPARM),LEDIT     ,LL(KLEDT1),LL(KLEDT2),          &
     &            NM        ,ISAT      ,KNTBLK    ,SEGMNT    ,          &
     &            LL(KLAVOI))
 1375 CONTINUE

! CLEAR RATIO TO EDITING SIGMA ARRAY AND THEN
! COMPUTE RATIO OF RESIDUALS TO EDITING SIGMAS

      DO 1400 N=1,NM
      RATIO (N)=ZERO
      !WRITE(6,*) ' PROCES : EDTSIG,RESID  ',EDTSIG(N),RESID(N),NTYPE
      IF(EDTSIG(N).LE.ZERO) GO TO 1400
      RATIO (N)=RESID (N)/EDTSIG(N)
      !if( n == 1 )then
      !WRITE(6,'(A,2(1x,I4),3(1x,E24.16))') &
      !      'proces: N,NTYPE,RESID(N),EDTSIG(N),RATIO(N) ', &
      !               N,NTYPE,RESID(N),EDTSIG(N),RATIO(N)
      !endif
 1400 END DO
      IF(LBEDIT) GO TO 2000
      IF(LGPS) THEN
      IF(LITER1.AND.MTYPE.EQ.40) GO TO 2000
      IF(LITER1.AND.MTYPE.EQ.51) GO TO 2000
      ENDIF

! COMPUTE ABSOLUTE VALUE OF RATIO TO SIGMA

      DO 1600 N=1,NM
      SEC(N)=ABS(RATIO(N))
 1600 END DO

! EDIT LARGE RATIOS

      DO 1800 N=1,NM
      IF(SEC(N).LE.EDLEVL) GO TO 1700
      LEDIT(N)=.TRUE.
 1700 CONTINUE
      IF ( MTYPE .EQ. 31 ) THEN
      DO 410 J1=1,NM                    ! %%%%%%%%%%
         LEDIT(J1) = .FALSE.            ! %%%%%%%%%%
 410  CONTINUE                          ! %%%%%%%%%%
      END IF
! IF SIMDAT CARDS ARE INCLUDED AND THE IONOSPHERIC OPTION IS SELECTED
! SET SIGMA SO THAT OBSERVATIONS TO BE OUTPUT ON UNIT 20 WILL NOT
! TO BE EDITED OR SKIPPED.
      IF(LIONC.AND.LSIMDT)LEDIT(N)=.FALSE.
      IF(LEDIT(N)) THEN
!         SIGMA(N)=EDTFLG       ! char
         TMP_CHAR=EDTFLG        ! char
         SIGMA(N)=TMP_DP        ! char
      ELSE
!         SIGMA(N) = BLANK      ! char
         TMP_CHAR = BLANK       ! char
         SIGMA(N) = TMP_DP      ! char
      ENDIF
      IF(INDRAM.GT.0) THEN
      IF(LRDEDT(N)) THEN
!         SIGMA(N)=EDTFLG       ! char
         TMP_CHAR=EDTFLG        ! char
         SIGMA(N)=TMP_DP        ! char
         LEDIT(N)=LRDEDT(N)
      ELSE
!         SIGMA(N) = BLANK      ! char
         TMP_CHAR = BLANK       ! char
         SIGMA(N) = TMP_DP      ! char

      ENDIF
      ENDIF
 1800 END DO
 2000 CONTINUE
      INDSYS=MOD(ITSYS,10000)/100
      INDSYS=MIN(MAX(INDSYS+1,1),3)
!
! OUTPUT UTC CALENDAR TIMES
!
      CALL YMDHMS(MJDSBL,OBSTIM,IYMD,IHM,SEC,NM)
      DO 2010 N=1,NM
      IF(IYMD(N).GT.1000000)IYMD(N)=IYMD(N)-1000000

      !write(6,'(A,1x,I3,1x,F12.1, 3(1x,E15.7))')  &
      !'proc: n, mjdsbl + obstim(n), resid(n) ', &
      !       n, mjdsbl + obstim(n), resid(n)

 2010 END DO
!
!CC COMPUTE AND OUTPUT NORMAL POINTS
!C      IF(LNORML) CALL NORMBN(AA,II,LL,MJDSBL    ,OBSTIM    ,IYMD
!C     1          IHM       ,SEC       ,AA(KTPRTL),OBS       ,LEDIT
!C     2          NM        ,II(KMJDBN),II(KNDBIN),II(KNWTDB),AA(KTMPCR)
!C     3          AA(KRA   ),OBSCAL(NTYPE)        ,RESCAL(NTYPE)
!C     4    DSCRPT(1,ISTATS),TIMSYS(INDSYS)       ,KNTBLK    ,SEGMNT
!C     5          NTYPE     ,SUMCOR    )
!
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
      RESX=RESCAL(NTYPE)
      IF(NTYPE.EQ.36) WRITE(6,78914) RESX
78914 FORMAT(' PROCES; DDOR RESX ',D25.16)
      IF(JNDRAM.GT.0.AND.NTYPE.EQ.54) THEN
        RESX=-RSCALX(I)/AA(KDTIME-1+I)
      ENDIF
      IF(LPREPW) CALL PREWRT(IYMD(I),IHM(I),SEC(I),KOUNTO,KNTBLK,       &
     &                       SEGMNT,NTYPE,SUMCOR(I),AA,I,RESX)
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
      IF(.NOT.LNELEV.OR.MTYPE.LE.98) GO TO 5100
      IF(MTYPE.GE.200.AND.MTYPE.LE.203) GO TO 5100
      ILINE6=ILINE6+1
      JGX=KOBCOR(4,7)-1+I
      JOX=KOBCOR(3,4)-1+I
      IF(LPRE(13,1)) JOX=KOBCOR(4,4)-1+I
      JEX=KOBCOR(2,4)-1+I
      JSSX=KOBCOR(5,4)-1+I
      WRITE(IOUT6,10500,IOSTAT=IERR) AA(JGX),AA(JOX),AA(JEX),AA(JSSX)
 5100 CONTINUE
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
! JBN REMOVE  1802
!     IF(LGPS) THEN
!     IF(LITER1.AND.MTYPE.EQ.51) LEDIT(N)=.FALSE.
!     IF(LITER1.AND.MTYPE.EQ.40) LEDIT(N)=.FALSE.
!     ENDIF
! ADD COMMENT FOR CROSSOVERS
      IF(LEDIT(N)) GO TO 7400
      NMWTD=NMWTD+1
                                            ! DP
      SIGMA (NMWTD)=RESID (N)/OBSSIG(N)
      RATIO (NMWTD)=RATIO (N)
 7400 END DO
!
! CROSSOVERS
! INITIALIZE  THE IRETIH ARRAY
      IF(MTYPE.EQ.100) THEN
      DO 7401 N=1,1500
      IRETIH(N)=0
 7401 END DO
      ENDIF
! FOR CROSSOVER ALTIMETRY MEASUREMENTS CHECK THE KEY
      IF(MTYPE.EQ.100) THEN
      JDIMN=KXBST+NM-1
! INITIALIZE FLAG LOCATION
      KXFLIN=KXFLAG+KXBUP-1
! INITIALIZE RESIDUAL POINTER
      IXL=KXL-1
! INITIALIZE SIGMA POINTER
      IXS=KXSIG-1
! INITIALIZE EDTSIG POINTER
      IXE=KXEDTS-1
! INITIALIZE THE RAW OBSERVATION ARRAYS
      IXA1=KXALO1-1
      IXA2=KXALO2-1
! INITIALIZE IRETIH
      IRH=0
! LOOP BEGINS FOR EACH BLOCK
      DO 90201 I=1,NM
! CHECK THE ALTIMETRY EDITING
! LFNDX SECOND OBSERVATION FOUND
      LFNDX=.FALSE.
! INITIALIZE LODD AND JSIG
! LODD (ODD KEY)
! JSIG (INTEGER +1 OR -1 TO MULTIPLY PARTIALS AND RESIDUALS
! FOR ODD AND EVEN KEY
      LODD=.FALSE.
      JSIG=1
! LOCATE THE KEY IN THE II(KXVKEY) ARRAY LOADED IN OBSRD
! THIS ARRAY IS 2*# OF CROSSOVERS AND IS FILLED AS NEW BLOCKS
! ARE READ.
! KXBST IS THE LOCATION OF THE CURRENT OBSERVATION IN THE TOTAL STREAM
! OF OBSERVATIONS
      KINDX=KXVKEY+KXBST-1
      KEY=II(KINDX)
! CHECK IF THE KEY IS EVEN OR ODD
      CALL ODD(KEY,LODD)
! THE CODE BELOW FOR >>>> EVEN <<<< KEYS
        IF(.NOT.LODD) THEN
! SET SIGN TO -1 ie THIS OBSERVATIONS PARTIAL WILL BE SUBTRACTED
! FROM THE ODD IN THE PAIR
        JSIG=-1
! FIND THE ODD KEY IN THE PAIR
        MKEY=KEY+1
! HAS THE ODD KEY BEEN FOUND ALREADY?
        CALL FNDNUM(MKEY,II(KXVKEY),JDIMN,IRET)
! IRET=0 NO IT HAS NOT BEEN FOUND
! IRET.NE.0 IT HAS BEEN FOUND IN LOCATION IRET IN THE FULL KEY ARRAY
! IRET=0 AND KEY EVEN --> SET FLAG TO 1 AT THE CURRENT LOCATION
          IF(IRET.EQ.0) THEN
! PUT THE KEY IN THE II(KXKEY) ARRAY SO THAT WHEN THE PAIR IS FOUND THE
! LOCATION OF THE PARTIAL WILL BE EASILY IDENTIFIED.
          II(KXKEY+KXBUP-1)=MKEY
! CHECK IF ALTIMETRIC OBSERVATION IS EDITED IF SO SET FLAG TO 3
            IF(LEDIT(I)) THEN
              II(KXFLIN)=3
              ELSE
              II(KXFLIN)=1
            ENDIF
          ELSE
! SECOND OBSERVATION WAS FOUND. WHERE IN II(KXKEY) IS THE PAIR?
      CALL FNDNUM(KEY,II(KXKEY),NRXTOT,IRET2)
      IF(IRET2.NE.0) THEN
! IN LOCATION IRET2
! IRET=I AND KEY EVEN --> SET FLAG TO 2
! CHECK IF ALTIMETRIC OBSERVATION IS EDITED IF SO SET FLAG TO 3
           IF(LEDIT(I)) THEN
             II(KXFLAG+IRET2-1)=3
             ELSE
             II(KXFLAG+IRET2-1)=2
           ENDIF
! SET FLAG TO TRUE
           LFNDX=.TRUE.
           IRH=IRH+1
           IRETIH(IRH)=IRET2
      ELSE
! THE PAIR BELONGS TO THE SAME LOGICAL BLOCK
! MKEY IS AT THE IRET LOCATION OF II(KXVKEY)
          II(KXKEY+KXBUP-1)=MKEY
          ENDIF
          ENDIF
        ELSE
! SET SIGN TO 1 ie THIS OBSERVATIONS PARTIAL WILL BE ADDED
! TO THE EVEN IN THE PAIR
        JSIG=1
        MKEY=KEY-1
        CALL FNDNUM(MKEY,II(KXVKEY),JDIMN,IRET)
! IRET=0 AND KEY ODD --> SET FLAG TO 1 AT THE CURRENT LOCATION
         IF(IRET.EQ.0) THEN
         II(KXKEY+KXBUP-1)=MKEY
           IF(LEDIT(I)) THEN
           II(KXFLIN)=3
           ELSE
           II(KXFLIN)=1
           ENDIF
         ELSE
      CALL FNDNUM(KEY,II(KXKEY),NRXTOT,IRET2)
      IF(IRET2.NE.0) THEN
! IRET=I AND KEY ODD --> SET FLAG TO 2
           IF(LEDIT(I)) THEN
           II(KXFLAG+IRET2-1)=3
           ELSE
           II(KXFLAG+IRET2-1)=2
           ENDIF
           LFNDX=.TRUE.
           IRH=IRH+1
           IRETIH(IRH)=IRET2
         ELSE
! THE PAIR BELONGS TO THE SAME LOGICAL BLOCK
! MKEY IS AT THE IRET LOCATION OF II(KXVKEY)
          II(KXKEY+KXBUP-1)=MKEY
         ENDIF
         ENDIF
       ENDIF
! ADVANCE IN THE SHORT ARRAY ONLY IF THE SECOND OBSERVATION HAS NOT
! BEEN FOUND
      IF(.NOT.LFNDX)KXFLIN=KXFLIN+1
! FOR CROSSOVER ALTIMETRY MEASUREMENTS FILL UP THE PMPA ARRAY
! KPA IS THE SAME BUT COMPUTED DIFFERENTLY IN THE CASE OF FIRST
! OR SECOND OBSERVATION
        IF(LFNDX) THEN
        KPA=IRET2
        ELSE
        KPA=KXBUP
        ENDIF
! FILL UP PARTIAL , RESIDUAL , SIGMA AND EDTSIG ARRAYS
      IXLKPA=IXL+KPA
      IXSIG =IXS+KPA
      IXEDS =IXE+KPA
      IXALO1= IXA1 +KPA
      IXALO2= IXA2 +KPA
      AA(IXLKPA)=AA(IXLKPA)+RESID(I)*JSIG
      AA(IXSIG)=AA(IXSIG)+OBSSIG(I)**2.D0
      AA(IXEDS)=AA(IXEDS)+EDTSIG(I)**2.D0
! FILL UP THE RAW ALTIMETRIC OBSERVATION ARRAYS
        IF(LFNDX) THEN
        AA(IXALO2)=OBSU(I)
        ELSE
        AA(IXALO1)=OBSU(I)
        ENDIF
      IF(LNPNM) GOTO 90250
      DO 90200 J=1,NADJST
      XPMPA(KPA,J)=XPMPA(KPA,J)+PMPA(I,J)*JSIG
90200 END DO
      GOTO 90270
90250 CONTINUE
      DO 90260 J=1,NADJST
      XPMPA(J,KPA)=XPMPA(J,KPA)+PMPA(J,I)*JSIG
90260 END DO
90270 CONTINUE
! ADVANCE THE BLOCK START POINTER AND ONLY FOR FIRST ENCOUNTER
! THE SHORT ARRAY POINTER
      KXBST=KXBST+1
      IF(.NOT.LFNDX) KXBUP=KXBUP+1
90201 END DO
      ENDIF
!
      IF(MTYPE.EQ.100) THEN
! FIRST CHECK IF IRETIH(1) IS ZERO THIS MEANS NO PAIRS FORMED
! IN THIS BLOCK
      IF(IRETIH(1).EQ.0) GO TO 9600
! ZERO OUT PMPA AND RESID ARRAYS
! ZERO OUT RATIO , SIGMA AND EDTSIG ARRAYS
      NTPMPA=NM*NADJST
      KNTPMP=KPMPA-1
      DO 49998 JP=1,NTPMPA
      AA(KNTPMP+JP)=0.D0
49998 END DO
      DO 49997 JP=1,NM
      RESID(JP)=0.D0
      RATIO(JP)=0.D0
                             ! DP
      SIGMA(JP)=0.D0
!     EDTSIG(JP)=0.D0
49997 END DO
! IDENTIFY THE LOCATIONS IN AA(KXPMPA) AND AA(KXL) WITH PAIRS
! INITIALIZE READY TO OUTPUT OBSERVATION COUNT
      IOUTNM=0
      DO 50000 JN=1,1500
      IRJN=IRETIH(JN)
      IF(IRJN.EQ.0) GO TO 50000
      IOUTNM=IOUTNM+1
      RESID(IOUTNM)=AA(KXL+IRJN-1)
      RESIDU(IOUTNM)=RESID(IOUTNM)*RESCAL(100)
! BELOW IS THE RATIO OF RESIDUAL TO SIGMA SQUARED
!     SIGMA(IOUTNM)=RESIDU(IOUTNM)/AA(KXSIG+IRJN-1)      ! DP
! BELOW IS THE OBSERVATION SIGMA AND THE RESIDUAL RATIO TO SIGMA
      AA(KXSIGW+IOUTNM-1)=SQRT(AA(KXSIG+IRJN-1))
                                                            ! DP
      SIGMA(IOUTNM)=RESIDU(IOUTNM)/SQRT(AA(KXSIG+IRJN-1))
      AA(KXOBSV+IOUTNM-1)=AA(KXALO1+IRJN-1)
      AA(KXOBSW+IOUTNM-1)=AA(KXALO2+IRJN-1)
      AA(KXEDSW+IOUTNM-1)=SQRT(AA(KXEDTS+IRJN-1))
      RATIO(IOUTNM)=RESIDU(IOUTNM)/AA(KXEDSW+IOUTNM-1)
! BELOW IS THE RATIO OF RESIDUAL TO EDITING SIGMA
!     RATIO(IOUTNM)=RESID(IOUTNM)/EDTSIG(IOUTNM)
      IF(LNPNM) GOTO 49985
      DO 49980 JG=1,NADJST
      PMPA(IOUTNM,JG)=XPMPA(IRJN,JG)
49980 END DO
      XPMPA(IRJN,JG)=0.D0
      GOTO 49995
49985 CONTINUE
      DO 49990 JG=1,NADJST
      PMPA(JG,IOUTNM)=XPMPA(JG,IRJN)
49990 END DO
      XPMPA(JG,IRJN)=0.D0
49995 CONTINUE
      AA(KXL+IRJN-1)=0.D0
      AA(KXALO1+IRJN-1)=0.D0
      AA(KXALO2+IRJN-1)=0.D0
      AA(KXSIG+IRJN-1)=0.D0
50000 END DO
! SAVE IOUTNM
      NM=IOUTNM
! ONLY TEMPORARILLY
      NMWTD=NM
      ENDIF
      IF(MTYPE.EQ.100) THEN
! XOV PRINT
      IF(.NOT.LRESID) GO TO 18000
      WRITE(IOUT6,20150,IOSTAT=IERR)
20150 FORMAT(///,10X,' ***** CROSSOVER OUTPUT INFORMATION ',            &
     &'*****'/)
      WRITE(IOUT6,20100,IOSTAT=IERR)KNTBLK,SEGMNT
! LOOP THRU AND PRINT BLOCK MEASUREMENTS IN GROUPS OF SIX
      MODULO=MIN(MODRES*6,MAX(NM-1,1))
      DO 17000 K=1,NM,MODULO
      KP5=K+MODULO-1
      KP5=MIN(KP5,NM)
      DO 16000 I=K,KP5,MODRES
      ILINE6=ILINE6+1
      IF(L9RESD) ILINE9=ILINE9+1
      KXKNT=KXKNT+1
      IF(LNELEV)                                                        &
     &   WRITE(IOUT6,20400,IOSTAT=IERR)                                 &
     &    AA(KXOBSV+I-1),AA(KXOBSW+I-1),RESIDU(I),                      &
     &    AA(KXSIGW+I-1),RATIO(I), KXKNT ,KNTBLK,SEGMNT
16000 END DO
17000 END DO
      WRITE(IOUT6,20160,IOSTAT=IERR)
20160 FORMAT(///)
      ENDIF
18000 CONTINUE
! XOV PRINT
! SUM IN STATISTICS

      CALL SMSTAT  (AA(KTMEAN),AA(KTRMS ),AA(KWMEAN),AA(KWTRMS),        &
     &   AA(KWTRND),AA(KPRVRT),AA(KTYMEA),AA(KTYRMS),AA(KWTMTY),        &
     &   AA(KWTYRM),II(KNOBST),II(KNOBWT),II(KNOBTY),II(KNOBWY),        &
     &   LL(KLGPRV),RESIDU    ,NM        ,SIGMA     ,RATIO     ,        &
     &   NMWTD     ,NTYPE     ,ISTATS    )
                                                                    ! DP
!
! WRITE OUT INFORMATION TO BINARY RESIDUAL FILE IF REQUESTED
!
!CC      write(6,*) 'proces: before binrdr sumcor ',
!CC     &          (aa(ksmcor-1+iij),iij=1,nm)
      IF(LBINRI) CALL BINRDR(AA,II,MJDSBL,OBSTIM,EDTSIG,RESID,          &
     &           AA(KTPRTL),AA(KTHG),ELEVSC,NELEVS,LEDIT,RESIDU,NM,     &
     &           AA(KXUTDT))
!
! WRITE OUT OBSERVATIONS &  OBS DIRECTORY TO BINARY RESIDUAL FILE
!       IF REQUESTED
!     ....new location for BOD call  (old location was at end of OBSGEN)
!
      IF(LODRI) THEN
!CC         write(6,*) 'proces: CALL BOD '
         CALL BOD(AA,II,NM)
      ENDIF
!
!
!     ......new location for normal points
!
! COMPUTE AND OUTPUT NORMAL POINTS
!
!CC      write(6,*) 'proces: before normbn sumcor ',
!CC     &          (aa(ksmcor-1+iij),iij=1,nm)
      IF(LNORML) CALL NORMBN(AA,II,LL,MJDSBL    ,OBSTIM    ,IYMD      , &
     &          IHM       ,SEC       ,AA(KTPRTL),OBS       ,LEDIT     , &
     &          NM        ,II(KMJDBN),II(KNDBIN),II(KNWTDB),AA(KTMPCR), &
     &        AA(KRA   ),OBSCAL(NTYPE)        ,RESCAL(NTYPE)        ,   &
     &    DSCRPT(1,ISTATS),TIMSYS(INDSYS)       ,KNTBLK    ,SEGMNT    , &
     &          NTYPE     ,SUMCOR    )
!
!CC      write(6,*) 'proces: after  normbn sumcor ',
!CC     &          (aa(ksmcor-1+iij),iij=1,nm)
!
! EXIT IF NO OBSERVATIONS WEIGHTED
      IF(LACC)GOTO 9500
      IF(LNADJ) GO TO 9500
70002 FORMAT(I6) 
70003 FORMAT(I4) 
70004 FORMAT(F4.1) 
70005 FORMAT(A80) 
      IF(NMWTD.LE.0) THEN
        IF(MTYPE.EQ.65.AND.LSTINR) THEN
          OBSTMQ(1)=OBSTIM(1)-0.1D0
          OBSTMQ(2)=OBSTIM(NM)+0.1D0
          CALL YMDHMS(MJDSBL,OBSTMQ,IYMD,IHM,SEC,2)
          IF(IYMD(1).GT.1000000)IYMD(1)=IYMD(1)-1000000
          IF(IYMD(2).GT.1000000)IYMD(2)=IYMD(2)-1000000
          WRITE(CARD1(41:46),70002) IYMD(1)
          WRITE(CARD1(47:50),70003) IHM(1)
          WRITE(CARD1(51:54),70004) SEC(1)
          DO IQ=41,54
           IF(CARD1(IQ:IQ).EQ.' ') CARD1(IQ:IQ)='0'
          ENDDO
          WRITE(CARD1(61:66),70002) IYMD(2)
          WRITE(CARD1(67:70),70003) IHM(2)
          WRITE(CARD1(71:74),70004) SEC(2)
          DO IQ=61,74
           IF(CARD1(IQ:IQ).EQ.' ') CARD1(IQ:IQ)='0'
          ENDDO
          CARD2(41:74)=CARD1(41:74)
          CARD3(41:74)=CARD1(41:74)
          WRITE(47,70005) CARD1
          WRITE(47,70005) CARD2
          WRITE(47,70005) CARD3
        ENDIF
        GO TO 9500
      ENDIF
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

      DO 8200 N=1,NM
      IF(LEDIT(N)) GO TO 8200
                                    ! DP
      SIGMA (N)=ONE/OBSSIG(N)**2

 8200 END DO
!
! JTW ADD ELEVATION DEPENDENT DOWNWEIGHTING
!
!      write(6,*)'write pre-downweight weights,and elevations'
!      DO N=1,NM
!      write(6,'(I4,2X,2(F12.4,2X))')N,SIGMA(N),ELEVSC(N,1)
!      END DO

!      write(6,*)'calc downweight scale factors'
!      write(6,*)'N,MTYPE,LEDIT,ELEV,SF,SIGMA',SIGMA(1)

      IF (LDWNWT) THEN
       IF (LDNWMT(MTYPE)) THEN
        DO N=1,NM
        ELEVTMP=ELEVSC(N,1)
        IF (NELVS.GT.1) THEN
         DO JJ=2,NELVS
          IF(ELEVSC(N,JJ).LT.ELEVTMP) ELEVTMP=ELEVSC(N,JJ)
         END DO
        END IF
        IELEV1=INT(ELEVTMP)
        IELEV2=IELEV1+1
! find values of scale factor for IELEV1 and IELEV2
        DWSF1=AA(KDWNWT+((MTYPE-1)*90)+(90-IELEV1))
        DWSF2=AA(KDWNWT+((MTYPE-1)*90)+(90-IELEV2))
        DWSF =DWSF1+(DWSF2-DWSF1)*&
     &        ((ELEVTMP-DBLE(IELEV1))/(DBLE(IELEV2-IELEV1)))
!!line below is if ELEV > 89, there is no 90 in downweight sf arrays
!!so just use sf for 89.
        IF (IELEV1.EQ.89) DWSF=AA(KDWNWT+((MTYPE-1)*90)+(90-IELEV1))
        SIGMA(N)=SIGMA(N)*DWSF
!        write(6,'(2I4,L,2X,F12.4,2X,2(F12.4,2X))')N,MTYPE,&
!     &                LEDIT(N),ELEVTMP,DWSF,SIGMA(N)
        END DO
       END IF
      END IF
!
!
      IF(LSTINR.AND.MTYPE.GE.200.AND.MTYPE.LE.203) THEN
         TP1=MJDSBL+OBSTIM(1)
         WRITE(91) MTYPE,NM,TP1
         DO N=1,NM
           WRITE(91) OBS(N),OBSC(N),RESID(N),OBSSIG(N),LEDIT(N)
         ENDDO
      ENDIF
!
! SUM THE NUMBER OF WEIGHTED OBSERVATIONS INTO THE OBS
! COUNT FOR THE ANBIGUITY AND TROP BIASES
      IF(JPAMB.GT.0) ITPARM(4,JPAMB)=ITPARM(4,JPAMB)+NMWTD
      IF(IPS51.GT.0) IT2PRM(1,IPS51)=IT2PRM(1,IPS51)+NMWTD
      IF(IPS52.GT.0) IT2PRM(1,IPS52)=IT2PRM(1,IPS52)+NMWTD
!
      IF(MPIPRO.GT.0.AND.LSTINR) THEN
      IF(LNPNM) THEN
      CALL U63(NADJST,NM,NM,AA(KPMPA),RESID,SIGMA,LNPNM)
      ELSE
      CALL U63(NM,NADJST,NM,AA(KPMPA),RESID,SIGMA,LNPNM)
      ENDIF
      ENDIF
      IF(MPIPRO.GT.0) GO TO 9500

      ! EDIT ADDITIONAL DATA BASED ON LEDIT_EXTRA
      DO N = 1, NM
          IF (LEDIT_EXTRA(N)) THEN
              LEDIT(N) = .TRUE.
          END IF
      END DO
      IF(MTYPE.EQ.85.OR.MTYPE.EQ.65) THEN
       IPQ=-1
       IF(JSATNO(1).EQ.1807001) IPQ=1
       IF(JSATNO(1).EQ.9806701) IPQ=2
       IF(IPQ.GT.0) THEN
         DO N=1,NM
          IF(.NOT.LEDIT(N)) THEN
            XN85(IPQ)=XN85(IPQ)+1.D0
            RMS85(IPQ)=RMS85(IPQ)+RESID(N)*RESID(N)
          ENDIF
         ENDDO
       ENDIF
      ENDIF
!!!      IF(NINNER.GT.3) THEN
!!!      IF(CHUBER(MTYPE).LT.1.D20) CALL HUBER(NM,CHUBER(MTYPE),OBSSIG,    &
!!!     &                                      SIGMA,RESID)
!!!      ENDIF
!
! CALL ESTIMATION SUMMATION ROUTINE
!
      IF(LSTINR.AND.MTYPE.EQ.65.AND.JPAMB.GT.0) THEN
      DO N=1,NM
      IF(.NOT.LEDIT(N)) THEN
        CALL YMDHMS(MJDSBL,OBSTIM(N),IYMD,IHM,SEC,1)
        IF(IYMD(1).GT.1000000)IYMD(1)=IYMD(1)-1000000
        WRITE(37,70000) JPAMB,IYMD(1),IHM(1),SEC(1),OBS(N),RESID(N)
70000 FORMAT(I12,2X,I6,2X,I4,2X,F10.1,2F20.3)
      ELSE
          OBSTMQ(1)=OBSTIM(N)-0.1D0
          OBSTMQ(2)=OBSTIM(N)+0.1D0
          CALL YMDHMS(MJDSBL,OBSTMQ,IYMD,IHM,SEC,2)
          IF(IYMD(1).GT.1000000)IYMD(1)=IYMD(1)-1000000
          IF(IYMD(2).GT.1000000)IYMD(2)=IYMD(2)-1000000
          WRITE(CARD1(41:46),70002) IYMD(1)
          WRITE(CARD1(47:50),70003) IHM(1)
          WRITE(CARD1(51:54),70004) SEC(1)
          DO IQ=41,54
           IF(CARD1(IQ:IQ).EQ.' ') CARD1(IQ:IQ)='0'
          ENDDO
          WRITE(CARD1(61:66),70002) IYMD(2)
          WRITE(CARD1(67:70),70003) IHM(2)
          WRITE(CARD1(71:74),70004) SEC(2)
          DO IQ=61,74
           IF(CARD1(IQ:IQ).EQ.' ') CARD1(IQ:IQ)='0'
          ENDDO
          CARD2(41:74)=CARD1(41:74)
          CARD3(41:74)=CARD1(41:74)
          WRITE(47,70005) CARD1
          WRITE(47,70005) CARD2
          WRITE(47,70005) CARD3
      ENDIF 
      END DO
      ENDIF

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
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@        sigma = 1.d11 ! %%%%%%%%%%%%%%%%%%%%%%%%%%
!@        write ( 6, * ) ' process before sumnpf nadjst=',nadjst,' nm=',nm, &
!@     &                 ' maparm=',maparam ! %%%%%%
!@        write ( 6, * ) ' sumnpf  resid=',resid,' sigma=',sigma ! %%%%%%%%%%%
!@        do 510 j1=1,nadjst
!@           write ( 6, * ) ' process j1=',j1,' der=',aa(kpmpa+j1-1), &
!@     &                    ' lav=',ll(klavoi+j1-1)
!@ 510    continue
!@         write ( 6, * ) 'RESID_1 = ', RESID, ' SIGMA=', 1.D0/DSQRT(SIGMA)
!@  write ( 46, * ) 'DERIV = ', aa(kpmpa+6), aa(kpmpa+7), aa(kpmpa+8), aa
!(kpmpa+11) ! %%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Keep in mind SIGMA is a square of weight
!

      CALL SUMNPF(AA(KPMPA ),RESID ,SIGMA ,NADJST,MAPARM,NM,            &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA),LL(KLAVOI))
      IF ( MTYPE .EQ. 31 ) THEN
!
! ------- In the case of VLBI we update accumulators of the sum of squares of
! ------- weighted observables and for sum of square of weights
!
      DO 420 J2=1,NM
      SUM_SQ_OBSERVABLES = SUM_SQ_OBSERVABLES + RESID(J2)**2*SIGMA(J2)
      SUM_SQ_WEIGHTS = SUM_SQ_WEIGHTS + SIGMA(J2)
420       CONTINUE
      END IF
!@         call matview_3 ( nadjst, aa(ksum1) )
!@         call matview_1 ( nadjst, 1, aa(ksum2) )

      GO TO 9500
 9150 CONTINUE
      CALL SUMNPG(AA(KPMPA ),RESID ,SIGMA ,NADJST,MAPARM,NM,            &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA),LL(KLAVOI))
      GO TO 9500
!
! PARTITIONED SUMMATION OF NORMAL MATRIX. PARTIALS ORDERED (NP,NM).
!
 9200 CONTINUE
      IF(NBIASE.GT.0.OR.NPROCS.LE.1) THEN
                                                              ! DP
      CALL SUMNPP(AA(KPMPA ),RESID ,SIGMA ,MAPARM,NM,                   &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA))
      ELSE
      CALL SUMNPG(AA(KPMPA ),RESID ,SIGMA ,NADJST,MAPARM,NM,            &
     &            AA(KSUM1 ),AA(KSUM2 )   ,AA(KPDLTA),LL(KLAVOI))
      ENDIF
 9500 CONTINUE
      IF(LSIMTI) CALL SHDDEL(NM,LEDIT,IYMD,IHM,SEC)
      IF(LSIMTI) CALL SIMDAT(AA,II,LL,OBS,RESID,SUMCOR,NTYPE,NM,NMWTD,  &
     & LEDIT)
      NELEVS=NELVP
      INDRAM=INDRAH
      RESCAL(NTYPE)=RESHLD
      OBSCAL(NTYPE)=OBSHLD

      IF(LNADJ) RETURN
      IF(.NOT.LPARFI) RETURN
      JFSECM=KFSECS(2,1)
      IF(MTYPE.LT.13.OR.MTYPE.GT.98) JFSECM=KFSECS(1,1)
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
10531 FORMAT(1X,I6,I5,5X,F10.6,D16.6,8X,F12.6,24X,I9,I8,A1)
10532 FORMAT(1X,I6,I5,5X,F10.6,D16.6,8X,2F12.6,12X,I9,I8,A1)
10533 FORMAT(1X,I6,I5,5X,F10.6,D16.6,8X,3F12.6,1X,I9,I8,A1)
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
