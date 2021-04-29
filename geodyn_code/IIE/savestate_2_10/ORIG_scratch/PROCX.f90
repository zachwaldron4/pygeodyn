!$PROCX
      SUBROUTINE PROCX(AA    ,II    ,LL    ,RESID ,SIGMA ,              &
     &    RATIO ,PMPA  ,OBS   ,OBSSIG, OBSU ,RESIDU,ISTATS,             &
     &    NTYPE ,X2TIME, X2OBS,RINDEX,                                  &
     &    XSCR  ,NOBSBL,   XSN, VCXBM,X2PART, NEQN ,PXPF  ,             &
     &    NDIMX1,NDIMX2,NDIMX3,    N3, ILATP, ISAT,  ISET ,             &
     &    V1    ,X2AUX ,    NU,X2VPRT, X2VPA, NDMX1, NDMX2,             &
     &    NDMX3 ,PMATT , X2PAT,NREDUC)
!********1*********2*********3*********4*********5*********6*********7**
! PROCX            00/00/00            0000.0    PGMR -DESPINA PAVLIS
!
!
! FUNCTION: PROCESS DYNAMIC CROSSOVERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A
!   II      I/O   A
!   LL      I/O   A
!   RESID
!   SIGMA
!   RATIO
!   PMPA
!   OBS
!   OBSSIG
!   OBSU
!   RESIDU
!   ISTATS
!   NTYPE
!   X2TIME  I/O A  CROSSOVER 2 TIME ARRAY
!   X2OBS   I/O A  CROSSOVER 2 OBSERVATION ARRAY
!   RINDEX  I/O A  AUXILIARY INDEX ARRAY FOR PAIRED BLOCKS IDENTIFICATIO
!   XSCR    I/O A  SCRATCH ARRAY FOR BLOCL INFORMATION MANIPULATION
!   NOBSBL  I/O A  ARRAY KEEPING THE NUMBER OF OBSERVATIONS PER BLOCK
!   XSN     I   A  TOR COORDINATES OF THE S/C
!   VCXSN   I/O A  AUXILIARY ARRAY FOR LOCATING PAIRED BLOCK COORDINATES
!   NEQN    I   A  NUMBER OF FORCE MODEL PARAMETERS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
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
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CORL05/KLGPRV,NXCL05
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
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
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/XOVER /KXKNT,KXBST,KXOBL,KXBUP,KXBPA,IAPLSC,IAPLL
      COMMON/XOVERR/XBTIME
      COMMON/XOVERS/NRXTOT,NXBLK,NUSIDE,NDEGX2,NDEGXX,NUCON,ITERNU,     &
     &              IPEDIT,IDEDIT,NXXOVR
!
      DIMENSION AA(*),II(*),LL(*)
      DIMENSION RESID (NM),SIGMA (NM),RATIO (NM),                       &
     &     PMPA(NM,NADJST),OBS   (NM),                                  &
     &          OBSSIG(NM)
      DIMENSION OBSU(NM),RESIDU(NM)
      DIMENSION IELPR(2,6)
      DIMENSION X2TIME(1),RINDEX(NXBLK,3),XSCR(20,MINTIM),              &
     &NOBSBL(NXBLK,5) ,X2OBS(1)
      DIMENSION XSN(MINTIM,3)
      DIMENSION VCXBM(3,1)
      DIMENSION V1(NM)
      DIMENSION X2AUX(NU,10)
      DIMENSION PMATT(NUCON,3,20)
      DIMENSION X2PAT(3,20,NREDUC)
      DIMENSION X2VPA(NUCON,3,20,2)
      DIMENSION PXPF(NDIMX1,NDIMX2,NDIMX3,2)
!     DIMENSION X2PART(NEQN,N3,NREDUC),X2VPRT(NDMX1,NDMX2,NDMX3,2)
      DIMENSION X2PART(NEQNG,N3,NREDUC),X2VPRT(NDMX1,NDMX2,NDMX3,2)
      DIMENSION ILATP(NXBLK,2)
      DIMENSION NEQN(MSETA)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!**********************************************************************
! INITIALIZATIONS
!**********************************************************************
!
      KXBSTS=KXBST
      IRET2=0
!
! KXBUP= LOCATION OF BLOCK BEGIN IN THE ENTIRE STREAM OF DATA
! KXBPA= SEQUENTIAL NUMBER OF BLOCK IN THE ENTIRE STREAM OF BLOCKS
! NU =   TOTAL NUMBER OF POINTS USED IN A STREAM FOR POLYNOMIAL FITTING
! NDEGX2=DEGREE OF POLYNOMIAL FOR FITTING THE DATA
      ISAVE=KXBUP
      ISAVE2=KXBPA
      NU1=NU+1
      ND1=NDEGX2+1
! POINTER TO CROSSOVER KEY  ARRAY FOR THIS  BLOCK
      JDIMN=KXBST+NM-1
! INITIALIZE LFNDX=.TRUE. MEANS SECOND BLOCK FOUND
      LFNDX=.FALSE.
! INITIALIZE LODD=.TRUE. MEANS THE PRESENT BLOCK KEY IS ODD
      LODD=.FALSE.
!
!**********************************************************************
! IDENTIFY THE BLOCK CROSSOVER KEY
!**********************************************************************
!
! LOCATE THE KEY IN THE II(KXVKEY) ARRAY LOADED IN OBSRD
! THIS ARRAY IS 2* # OF BLOCKS AND IS FILLED AS NEW BLOCKS
! ARE READ.
!
! KXBST IS THE LOCATION OF THE CURRENT BLOCK IN THE TOTAL STREAM
! OF BLOCKS
!
      KINDX=KXVKEY+KXBST-1
      KEY=II(KINDX)
! CHECK IF THE KEY IS EVEN OR ODD
      CALL ODD(KEY,LODD)
! THE CODE BELOW FOR >>>> EVEN <<<< KEYS
! LOOP 1 PROCESS EVEN OR ODD
!
!**********************************************************************
! THE BLOCK KEY HAS BEEN IDENTIFIED
!**********************************************************************
!
        IF(LODD) THEN
! FIND THE EVEN KEY IN THE PAIR
        MKEY=KEY-1
        ELSE
! FIND THE ODD KEY IN THE PAIR
        MKEY=KEY+1
        ENDIF
!
!**********************************************************************
! THE PAIRING KEY HAS BEEN IDENTIFIED
! HAS THE PAIRING KEY BEEN FOUND ALREADY?
!**********************************************************************
!
        CALL FNDNUM(MKEY,II(KXVKEY),JDIMN,IRET)
!
! IRET=0 NO IT HAS NOT BEEN FOUND
! IRET.NE.0 IT HAS BEEN FOUND IN LOCATION IRET IN THE FULL KEY ARRAY
!
!    A IF BEGIN
!
          IF(IRET.EQ.0) THEN
!**********************************************************************
! FIRST ENCOUNTER OF PAIR - SAVE INFORMATION FOR LATER
!**********************************************************************
!
! PUT THE KEY IN THE II(KXKEY) ARRAY
          II(KXKEY+KXBST-1)=MKEY
!         write(6,*)' key ',ii(kxkey+kxbst-1),kxbst
! SAVE NUMBER OF OBS/ LOCATION OF BLOCK / SEQUENTIAL # OF BLOCK
!  DO EDITING HERE AND LOAD THE NUMBER OF WEIGHTED OBSERVATIONS
          NOBSBL(KXBST,1)=NM
          NOBSBL(KXBST,2)=ISAVE
          NOBSBL(KXBST,5)=ISAVE2
! SAVE ISAT
          NOBSBL(KXBST,3)=ISAT
! SAVE ISET
          NOBSBL(KXBST,4)=ISET
! SAVE THE BLOCK MJDSEC
          RINDEX(KXBST,1)=DBLE(MJDSBL)
! SAVE THE BASE TIME FOR POYLOMIAL EVALUATION NEED TO SUBTRACT BP TIME L
          RINDEX(KXBST,3)= XBTIME
! IDENTIFY AND SAVE THE BEST CROSSOVER TIME ESTIMATE ONLY DURING THE
! FIRST INNER ITERATION AND SAVE IT
          IF(LITER1) THEN
          IMID=(NM/2)+1
          RINDEX(KXBST,2)= AA(KFSECS(1,1)+IMID-1)
          ENDIF
! SAVE BEGINING LOCATION OF ATTITUDE PARTIALS FOR THE SPACECRAFT AND THE
          ILATP(KXBST,1)=IAPLSC
          ILATP(KXBST,2)=IAPLL
!
!  B IF BEGIN
!
      IF(NM.GE.3) THEN
      DO 90201 I=1,NM
!      THE TIMES SAVED ARE FSECN (RECEIVED TIME)
!     write(6,*)' dbg SAVING FRACTIONAL TIMES AND COORDINATES '
! SAVE TIMES FOR FIRST OCCURANCE
      TIMES=AA(KFSECS(1,1)+I-1)
      X2TIME(KXBUP)=TIMES
! SAVE OBSERVATIONS FOR FIRST OCCURANCE
      X2OBS(KXBUP)=OBS(I)
! SAVE BOUNCE POINT COORDINATES FOR FIRST OCCURANCE
      DO 30312 J=1,3
      VCXBM(J,KXBUP)=XSCR(18-1+J,I)
30312 END DO
! INCREASE THE NUMBER OF OBSERVATIONS BY ONE
      KXBUP=KXBUP+1
90201 END DO
!
!**********************************************************************
!  SAVE PARTIALS BELOW ONLY IF REACHED ITERATION SPECIFIED
!**********************************************************************
         IF(NINNER.GT.ITERNU) THEN
! FIND POINTER FOR PXPF
      TBEST=DBLE(MJDSBL)+RINDEX(KXBST,2)
      DO I=1,NM
      OBSU(I)=DBLE(MJDSBL)+AA(KFSECS(1,1)+I-1)
      ENDDO
      CALL FNDRN(TBEST,OBSU,NM,IRETFM)
      IF(IRETFM.GT.0) THEN
      IP=IRETFM-(NUCON/2)
      ENDIF
! SAVE PARTIALS OF BOUNCE POINT WRT VARIOUS  PARAMETERS
      IP0=0
      DO I=1,NUCON
      CALL SAVATP(PMATT,X2PAT,NUCON,NREDUC,KXBPA,I)
! SAVE PARTIALS OF OBS WRT FM PARAMETERS
      IF(LNPNM) GOTO 30315
      DO 30314 KK=1,NEQN(ISET)
      DO 30313 JJ=1,3
      X2PART(KK,JJ,KXBPA)=PXPF(IP+IP0,KK,JJ,1)
! DEBUG ****************************************************************
!     write(6,*)' PARTIALS NM,NP  ',X2PART(KK,JJ,KXBPA),KK,JJ,KXBPA
! DEBUG ****************************************************************
30313 END DO
30314 END DO
      GOTO 30318
30315 CONTINUE
      DO 30317 KK=1,NEQN(ISET)
      DO 30316 JJ=1,3
      X2PART(KK,JJ,KXBPA)=PXPF(KK,JJ,IP+IP0,1)
! DEBUG ****************************************************************
!     write(6,*)' PARTIALS NP,NM  ',X2PART(KK,JJ,KXBPA),KK,JJ,KXBPA
! DEBUG ****************************************************************
30316 END DO
30317 END DO
30318 CONTINUE
      KXBPA=KXBPA+1
      IP0=IP0+1
      ENDDO
         ENDIF
!**********************************************************************
!  DONE SAVING PARTIALS
!**********************************************************************
!
      ENDIF
!
!  B IF END
!
!  A ELSE
          ELSE
!  A ELSE
!
!**********************************************************************
! SECOND BLOCK WAS FOUND. WHERE IN II(KXKEY) IS THE PAIR?
!**********************************************************************
!
      CALL FNDNUM(KEY,II(KXKEY),NRXTOT,IRET2)
      KEYP=KEY
!
!  C IF BEGIN
!
      IF(IRET2.NE.0) THEN
!      write(6,*)'                                               '
!      write(6,*)'  *********  CROSSOVER INFORMATION  *********  '
!      write(6,*)'                                               '
!      write(6,*)' FOUND THE OTHER KEY AT PREVIOUS BLOCK ',IRET2
!
!  GET TIMES FOR BOTH ENCOUNTERS
      MJDSC1=INT(RINDEX(IRET2,1))
      MJDSC2=MJDSBL
!
!  GET BEST TIME ESTIMATES FOR BOTH ENCOUNTERS
      T1EST=RINDEX(IRET2,2)
      IF(LITER1) THEN
      IMID=(NM/2)+1
      T2EST=AA(KFSECS(1,1)+IMID-1)
      ELSE
      T2EST=RINDEX(KXBST,2)
      ENDIF
! DEBUG ****************************************************************
!      write(6,*)' BEST CROSSOVER TIME ESTIMATE ',T1EST,T2EST
! DEBUG ****************************************************************
!  GET BASE TIME FOR POLYNOMIAL FOR BOTH ENCOUNTERS
      TAT1=RINDEX(IRET2,3)
      TAT2=XBTIME
! DEBUG ****************************************************************
!      write(6,*)' BASE TIME FOR POLYNOMIAL ',TAT1,TAT2
! DEBUG ****************************************************************
!  GET NUMBER OF OBS PER BLOCK FOR PREVIOUS ENCOUNTER/ PRESENT=NM
      NMP=NOBSBL(IRET2,1)
!
!      write(6,*)' NUMBER OF OBS IN THE TWO STREAMS ',NMP,NM
!      write(6,*)'                                               '
!      write(6,*)'  *********  CROSSOVER INFORMATION  *********  '
!      write(6,*)'                                               '
!
!  GET ISAT AND ISET FOR PREVIOUS ENCOUNTER
      ISAT1=NOBSBL(IRET2,3)
      ISET1=NOBSBL(IRET2,4)
! DEBUG ****************************************************************
!     write(6,*)' ISAT  ',NOBSBL(IRET2,3),ISAT
!     write(6,*)' ISET  ',NOBSBL(IRET2,4),ISAT
! DEBUG ****************************************************************
! GET LOCATION OF PMPA FOR THE ATTITUDE PARTIALS FOR S/C AND LASER
      IPAT11=ILATP(IRET2,1)
      IPAT12=ILATP(IRET2,2)
      IPAT21=IAPLSC
      IPAT22=IAPLL
! DEBUG ****************************************************************
!     write(6,*)' POINT AT PMPA FOR ATT PARTIALS S/C ',IPAT11,IPAT21
!     write(6,*)' POINT AT PMPA FOR ATT PARTIALS LASER ',IPAT12,IPAT22
! DEBUG ****************************************************************
! GET NUMBER OF OF FIRST OBSERVATION OF PREVIOUS STREAM IN THE THE BIG A
      IRET3=NOBSBL(IRET2,2)
! GET NUMBER OF OF FIRST PARTIAL OF PREVIOUS STREAM IN THE THE BIG ARRAY
      IRET9=NOBSBL(IRET2,5)
! AT THIS POINT WE FOUND 2 BLOCKS FORMING A PAIR. EACH ONE OF THEM HAS
! AT LEAST 3 OBSERVATIONS.
      IF(NOBSBL(IRET2,1).NE.NM) THEN
      write(6,*)' BLOCKS DO NOT HAVE EQUAL NUMBER OF POINTS ',          &
     &NOBSBL(IRET2,1),NMWTD
      JJ=0
      ENDIF
      IF(IRET3.EQ.0) THEN
      WRITE(6,*)' TIMES FOR CROSSOVERS WERE NOT FOUND ERROR!!! '
      STOP 16
      ENDIF
!
!***********************************************************************
! SAVE INFORMATION FOR BLOCKS IN THE SCRATCH ARRAY
!***********************************************************************
      JJ=0
      DO 30301 I=1,NM
      XSCR(1,I)=X2TIME(IRET3+JJ)
      XSCR(2,I)=AA(KFSECS(1,1)+I-1)
      XSCR(3,I)=X2OBS(IRET3+JJ)
      XSCR(4,I)=OBS(I)
! 5-8 MAY BE USED  FOR SCRATCH
      XSCR(9,I)=VCXBM(1,IRET3+JJ)
      XSCR(10,I)=VCXBM(2,IRET3+JJ)
      XSCR(11,I)=VCXBM(3,IRET3+JJ)
      XSCR(12,I)=XSCR(18,I)
      XSCR(13,I)=XSCR(19,I)
      XSCR(14,I)=XSCR(20,I)
! 15-17 MAY BE USED  FOR SCRATCH
      XSCR(9,I)=VCXBM(1,IRET3+JJ)
!     XSCR(15,I)=
!     XSCR(16,I)=
!     XSCR(17,I)=
! DEBUG ****************************************************************
!     write(6,*)' FRAC TIMES FOR BOTH BLOCKS ',XSCR(1,I),XSCR(2,I)
!    .,T1EST,T2EST
!     write(6,*)' TOR S/C FOR BOTH BLOCKS ',XSCR(3,I),XSCR(6,I)
!     write(6,*)' TOR S/C FOR BOTH BLOCKS ',XSCR(4,I),XSCR(7,I)
!     write(6,*)' TOT S/C FOR BOTH BLOCKS ',XSCR(5,I),XSCR(8,I)
!     write(6,*)' EF BOUNCE FOR BOTH BLOCKS ',XSCR(9,I),XSCR(12,I)
!     write(6,*)' EF BOUNCE FOR BOTH BLOCKS ',XSCR(10,I),XSCR(13,I)
!     write(6,*)' EF BOUNCE FOR BOTH BLOCKS ',XSCR(11,I),XSCR(14,I)
! DEBUG ****************************************************************
      JJ=JJ+1
30301 continue
!
!***********************************************************************
! DONE SAVING INFORMATION
!***********************************************************************
!
!***********************************************************************
! OBTAIN PARTIALS OF BOUNCE POINT WRT. VARIOUS PARAMETERS
!***********************************************************************
!
         IF(NINNER.GT.ITERNU) THEN
! FIND POINTER FOR PXPF
      TBEST=DBLE(MJDSBL)+RINDEX(KXBST,2)
      DO I=1,NM
      OBSU(I)=DBLE(MJDSBL)+AA(KFSECS(1,1)+I-1)
      ENDDO
      CALL FNDRN(TBEST,OBSU,NM,IRETFM)
!
      IF(IRETFM.GT.0) THEN
      IP=IRETFM-(NUCON/2)
      ENDIF

!   CLEAR UP COMPLETELY THE X2VPRT ARRAY
      NXDM=NDMX1*NDMX2*NDMX3*2
      CALL CLEARA(X2VPRT,NXDM)

      JJ=0
      IP0=0
      DO I=1,NUCON
      CALL GETATP(PMATT,X2PAT,X2VPA,NREDUC,NUCON,IRET9+JJ,I)
! OBTAIN THE PARTIALS OF OBS WRT FM PARAMETERS BUT DO NOT SAVE THEM YET
      IF(LNPNM) GOTO 30304
      DO 30303 KK=1,NEQN(ISET)
      DO 30302 K2=1,3
      X2VPRT(I,KK,K2,1)=X2PART(KK,K2,IRET9+JJ)
      X2VPRT(I,KK,K2,2)=PXPF(IP+IP0,KK,K2,1)
! DEBUG ****************************************************************
!     write(6,*)' dbg  1 part ',x2part(kk,k2,iret9+jj),kk,k2,iret9+jj,jj
!     write(6,*)' dbg  2 part',pxpf(IP+IP0,KK,K2,1),i,kk,k2
! DEBUG ****************************************************************
30302 END DO
30303 END DO
      GOTO 30307
30304 CONTINUE
      DO 30306 KK=1,NEQN(ISET)
      DO 30305 K2=1,3
      X2VPRT(KK,K2,I,1)=X2PART(KK,K2,IRET9+JJ)
      X2VPRT(KK,K2,I,2)=PXPF(KK,K2,IP+IP0,1)
! DEBUG ****************************************************************
!     write(6,*)' dbg partials for first part ',x2part(kk,k2,iret9+jj)
!     write(6,*)' dbg partials for second part',pxpf(KK,K2,IP+IP0,1)
! DEBUG ****************************************************************
30305 END DO
30306 END DO
30307 CONTINUE
      IP0=IP0+1
      JJ=JJ+1
      ENDDO
         ENDIF
!
!***********************************************************************
! DONE WITH PARTIALS
!***********************************************************************
!
      LFNDX=.TRUE.
      ENDIF
!
!  C IF END
!
!
      IF(NMP.LT.NU) THEN
      KXBST=KXBST+1
      write(6,*)' dbg RETURN 2 ',NMP,NU
      GOTO 9600
      ENDIF
!
!***********************************************************************
!
!
!
!
!
!
!
!
!***********************************************************************
!***********************************************************************
! REDUCE STREAMS FROM NM TO NU OR NUCON AFTER ITERNU - PROCESS STREAM 1
!***********************************************************************
!
! AS OF  01/04/2000 THE FOLLOWING THREE LINES OF CODE ARE UNTESTED.
! THESE THREE LINES ARE INTENDED TO CURE A PROBLEM THAT OCCURED WHEN
! CROSSOVERS HAVE POOR INITIAL TIME ESTIMATES ON THE 1ST ITERTATION.
! ON THE LATER ITERATIONS CROSSOVERS WERE BEING REJECTED BECAUSE
! NOT ENOUGH POINTS COULD BE RETAINED AROUND THE CROSSOVER. THE
! TEST FOR # OF POINTS WAS BEING BASED ON THE NUMBER REQUIRED FOR
! THE 1ST ITERATION (NOT THE LATER ITERATIONS).
!
      IF(NINNER.GT.ITERNU) THEN
      NUSIDE=NUCON/2
      ENDIF
!***********************************************************************
!
! FIND A POINTER THAT SHOWS THE BEGINING OF NU IN A STREAM OF NM
      DO I=1,NMP
      V1(I)=XSCR(1,I)
      ENDDO
      CALL FNDRN(T1EST,V1,NMP,IRET4)
! DEBUG ****************************************************************
!     write(6,*)'STREAM 1 BEST TIME ESTIM AT OBS ',IRET4,xscr(3,iret4),
!    . T1EST
! DEBUG ****************************************************************
!
! CHECK IF THE IRET4 OBSERVATION IS NEGATIVE AND IF NOT LOAD IT
! IF OBS IS NEGATIVE MOVE THE BEST ESTIMATE
!
      JJ=0
51511 CONTINUE
      IF(XSCR(3,IRET4).LE.0.D0) THEN
! DELETE OBSERVATION
         IF(JJ.EQ.0) THEN
         IRET4=IRET4+1
         JJ=JJ+1
         ELSE
         IRET4=IRET4-1
         JJ=JJ+1
            IF (JJ.GT.NUSIDE) THEN
       WRITE(6,*)' PROCX 1 TOO MANY BAD OBSERVATIONS AROUND THE BEST',  &
     &' ESTIMATE'
       WRITE(6,*)' THIS STREAM WILL NOT BE USED IN THE CROSSOVER ',&
     &'COMPUTATIONS'
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
            KXBST=KXBST+1
            GOTO 9600
            ENDIF
         ENDIF
      GOTO 51511
! DEBUG ****************************************************************
!     write(6,*)' FOR  THIS STREAM THE BEST ESTIMATE MOVED TO ',iret4
! DEBUG ****************************************************************
      ENDIF
!
! I NOW IS THE MIDDLE LOCATION OF THE NU STREAM
! LOAD THE BEST ESTIMATE  OBSERVATION INFORMATION
      I=1+NUSIDE
! DEBUG ****************************************************************
!     write(6,*)' fill up  ',I,' with obs ',iret4,xscr(3,iret4)
! DEBUG ****************************************************************
      X2AUX(I,1)=XSCR(1,IRET4)
      X2AUX(I,3)=XSCR(9,IRET4)
      X2AUX(I,4)=XSCR(10,IRET4)
      X2AUX(I,5)=XSCR(11,IRET4)
!
! NOW PICK OBSERVATIONS  ABOVE THE BEST OCCURANCE
      IRETB=IRET4
      DO IK=1,NUSIDE
      IRETB=IRETB-1
      IF(IRETB.LE.0) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=0
51512       CONTINUE
            IF(XSCR(3,IRETB).LE.0.D0) THEN
            IRETB=IRETB-1
      IF(IRETB.LE.0) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=JJ+1
               IF (JJ.GT.NUSIDE) THEN
       WRITE(6,*)' PROCX 2 TOO MANY BAD OBSERVATIONS'
       WRITE(6,*)' THIS STREAM WILL NOT BE USED IN THE CROSSOVER',&
     &'COMPUTATIONS'
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
            KXBST=KXBST+1
               GOTO 9600
               ENDIF
            GOTO 51512
            ENDIF
      I=I-1
! DEBUG ****************************************************************
!     write(6,*)' fill up  ',I,' with obs ',IRETB,xscr(3,IRETB)
! DEBUG ****************************************************************
      X2AUX(I,1)=XSCR(1,IRETB)
      X2AUX(I,3)=XSCR(9,IRETB)
      X2AUX(I,4)=XSCR(10,IRETB)
      X2AUX(I,5)=XSCR(11,IRETB)
!
      ENDDO
!
      I=I+NUSIDE+1
!
! NOW PICK OBSERVATIONS  BELOW  THE BEST OCCURANCE
      IRETB=IRET4
      DO IK=1,NUSIDE
      IRETB=IRETB+1
      IF(IRETB.GT.NMP) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=0
71512       CONTINUE
            IF(XSCR(3,IRETB).LE.0.D0) THEN
! DELETE OBSERVATION
            IRETB=IRETB+1
      IF(IRETB.GT.NMP) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=JJ+1
               IF (JJ.GT.NUSIDE) THEN
       WRITE(6,*)' PROCX 3 TOO MANY BAD OBSERVATIONS'
       WRITE(6,*)' THIS STREAM WILL NOT BE USED IN THE CROSSOVER',&
     &'COMPUTATIONS'
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
            KXBST=KXBST+1
            GOTO 9600
               ENDIF
            GOTO 71512
            ENDIF
!
! DEBUG ****************************************************************
!     write(6,*)' fill up  ',I,' with obs ',IRETB,xscr(3,IRETB)
! DEBUG ****************************************************************
      X2AUX(I,1)=XSCR(1,IRETB)
      X2AUX(I,3)=XSCR(9,IRETB)
      X2AUX(I,4)=XSCR(10,IRETB)
      X2AUX(I,5)=XSCR(11,IRETB)
!
      I=I+1
      ENDDO
!***********************************************************************
!***********************************************************************
!  DONE WITH STREAM 1
!***********************************************************************
!***********************************************************************
!
!
!
!
!
!
!
!
!
!***********************************************************************
!***********************************************************************
! REDUCE STREAMS FROM NM TO NU - PROCESS STREAM 2
!***********************************************************************
!***********************************************************************
!
      DO I=1,NM
      V1(I)=XSCR(2,I)
      ENDDO
      CALL FNDRN(T2EST,V1,NM ,IRET5)
! DEBUG ****************************************************************
!     write(6,*)'STREAM 2 BEST TIME ESTIM AT OBS ',IRET5,xscr(4,iret5),
!    .  T2EST
! DEBUG ****************************************************************
!
! CHECK IF THE IRET5 OBSERVATION IS NEGATIVE AND IF NOT LOAD IT
! IF OBS IS NEGATIVE MOVE THE BEST ESTIMATE
!
      JJ=0
81511 CONTINUE
      IF(XSCR(4,IRET5).LE.0.D0) THEN
         IF(JJ.EQ.0) THEN
         IRET5=IRET5+1
         JJ=JJ+1
         ELSE
         IRET5=IRET5-1
         JJ=JJ+1
            IF (JJ.GT.NUSIDE) THEN
      WRITE(6,*)' PROCX 4 TOO MANY BAD OBSERVATIONS AROUND THE BEST',   &
     &'ESTIMATE'
       WRITE(6,*)' THIS STREAM WILL NOT BE USED IN THE CROSSOVER',&
     &'COMPUTATIONS'
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
            KXBST=KXBST+1
            GOTO 9600
            ENDIF
         ENDIF
      GOTO 81511
      ENDIF
!
! I NOW IS THE MIDDLE LOCATION OF THE NU STREAM
! LOAD THE BEST ESTIMATE  OBSERVATION INFORMATION
      I=1+NUSIDE
      X2AUX(I,2)=XSCR(2,IRET5)
      X2AUX(I,6)=XSCR(12,IRET5)
      X2AUX(I,7)=XSCR(13,IRET5)
      X2AUX(I,8)=XSCR(14,IRET5)
!
! NOW PICK OBSERVATIONS  ABOVE THE BEST OCCURANCE
      IRETB=IRET5
      DO IK=1,NUSIDE
      IRETB=IRETB-1
      IF(IRETB.LE.0) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=0
81512       CONTINUE
            IF(XSCR(4,IRETB).LE.0.D0) THEN
! DELETE OBSERVATION
            IRETB=IRETB-1
      IF(IRETB.LE.0) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=JJ+1
               IF (JJ.GT.NUSIDE) THEN
       WRITE(6,*)' PROCX 5 TOO MANY BAD OBSERVATIONS'
       WRITE(6,*)' THIS STREAM WILL NOT BE USED IN THE CROSSOVER ',&
     &'COMPUTATIONS'
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
            KXBST=KXBST+1
               GOTO 9600
               ENDIF
            GOTO 81512
            ENDIF
      I=I-1
      X2AUX(I,2)=XSCR(2,IRETB)
      X2AUX(I,6)=XSCR(12,IRETB)
      X2AUX(I,7)=XSCR(13,IRETB)
      X2AUX(I,8)=XSCR(14,IRETB)
!
      ENDDO
!
      I=I+NUSIDE+1
!
! NOW PICK OBSERVATIONS  BELOW  THE BEST OCCURANCE
      IRETB=IRET5
      DO IK=1,NUSIDE
      IRETB=IRETB+1
      IF(IRETB.GT.NM) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=0
91512       CONTINUE
            IF(XSCR(4,IRETB).LE.0.D0) THEN
! DELETE OBSERVATION
            IRETB=IRETB+1
      IF(IRETB.GT.NM) THEN
         IF(NINNER.GT.ITERNU) THEN
      WRITE(6,*)' CANNOT RETAIN  ',NUCON,' OBSERVATIONS FOR THIS BLOCK'
         ELSE
      WRITE(6,*)' CANNOT RETAIN  ',NU,' OBSERVATIONS FOR THIS BLOCK '
         ENDIF
      GOTO 9600
      ENDIF
            JJ=JJ+1
               IF (JJ.GT.NUSIDE) THEN
       WRITE(6,*)' PROCX 6 TOO MANY BAD OBSERVATIONS'
       WRITE(6,*)' THIS STREAM WILL NOT BE USED IN THE CROSSOVER ',&
     &'COMPUTATIONS'
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
           KXBST=KXBST+1
               GOTO 9600
               ENDIF
            GOTO 91512
            ENDIF
!
      X2AUX(I,2)=XSCR(2,IRETB)
      X2AUX(I,6)=XSCR(12,IRETB)
      X2AUX(I,7)=XSCR(13,IRETB)
      X2AUX(I,8)=XSCR(14,IRETB)
!
      I=I+1
      ENDDO
!
!***********************************************************************
!***********************************************************************
! DONE WITH STREAM 2
!***********************************************************************
!***********************************************************************
!
!
!
!
!***********************************************************************
! SET POINTERS FOR EXDYNX
!***********************************************************************
      IF(LNPNM) THEN
      NDM1=NADJST
      NDM2=NUCON
      ELSE
      NDM1=NUCON
      NDM2=NADJST
      ENDIF
!***********************************************************************
! SET POINTERS FOR EXDYNX AFTER ITERNU ITERATIONS
!***********************************************************************
      IF(NINNER.LE.ITERNU) THEN
      NMX=NU
      ND1X=ND1
      ELSE
      NMX=NUCON
      ND1X=NDEGXX+1
      ENDIF
      NMX1=NMX+1
      T11=XSCR(1,1)
      T12=XSCR(1,NM)
      T21=XSCR(2,1)
      T22=XSCR(2,NM)
      PPARM=0.D0
      IF(NPVAL0(IXRSEP).GE.1) THEN
         IP=IPVAL(IXRSEP)
         PPARM=AA(KPRMV-1+IP)
      ENDIF
!***********************************************************************
!  CALL EXDYNX
!***********************************************************************
      CALL EXDYNX(NU,NU1,ND1X,ISAT1,ISET1,ISAT,ISET,AA,II,LL,           &
     &          AA(KXTN),AA(KXSM),AA(KXTK),PMPA,AA(KPXEXI),NEQN,        &
     &          N3,II(KIPTFM),II(KILNFM),II(KNFMG),AA(KPXEPA),          &
     &          AA(KX2COF),MJDSC1,MJDSC2,T1EST,T2EST,X2AUX(1,1),        &
     &          X2AUX(1,2),X2AUX(1,3),X2AUX(1,9),X2AUX(1,10),           &
     &          AA(KDDDA),NDM1,NDM2,TAT1,IPAT11,IPAT12,TAT2,IPAT21,     &
     &          IPAT22,AA(KSUM1),AA(KSUM2),OBSSIG,X2VPRT,NDMX1,NDMX2,   &
     &          NDMX3,X2VPA,RESID,AA(KATPER),NMX,NMX1,T11,T12,T21,T22,  &
     &          LRESID,LED,KEYP,LSTITR,PPARM,AA(KPDLTA))
!***********************************************************************
      DIST=-RESID(1)
      RESIDU(1)=RESID(1)
      RATIO(1)=RESID(1)/OBSSIG(1)
      SIGMA(1)=RESID(1)/OBSSIG(1)
      IF(LED) THEN
        RATIO(1)=0.D0
        SIGMA(1)=0.D0
      ENDIF
!
!***********************************************************************
! SAVE THE NEW TIME ESTIMATES NOW
!***********************************************************************
      RINDEX(IRET2,2)=T1EST
      RINDEX(KXBST,2)=T2EST
! DEBUG ****************************************************************
!      write(6,*)' dbg NEW TIMES ',T1EST,IRET2,T2EST,KXBST
! DEBUG ****************************************************************
      ENDIF
!
!    A IF END
!
      IF(IRET2.EQ.0) GOTO 9600
      NM=1
      NMWTD=1
      IF(LED) NMWTD=0
!***********************************************************************
      CALL SMSTAT  (AA(KTMEAN),AA(KTRMS ),AA(KWMEAN),AA(KWTRMS),        &
     &   AA(KWTRND),AA(KPRVRT),AA(KTYMEA),AA(KTYRMS),AA(KWTMTY),        &
     &   AA(KWTYRM),II(KNOBST),II(KNOBWT),II(KNOBTY),II(KNOBWY),        &
     &   LL(KLGPRV),RESIDU    ,NM        ,SIGMA     ,RATIO     ,        &
     &   NMWTD     ,NTYPE     ,ISTATS    )
!***********************************************************************
 9600 CONTINUE
      KXBST=KXBSTS+1
      RETURN
!
      END
