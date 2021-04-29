      SUBROUTINE ALTPTI(AA,II,LL,NM,MJDSBL,FSEC,ISAT,ISET1,OFF,TRK,SCR, &
     &                 XSM,VSM,ITERP,ATROT,HOLD,IDA,FRQION,MTYPE,SBFTOR,&
     &                 IAST)
!***********************************************************************
!
!   GET TRUE OF REF UNIT POINTING VECTOR FOR ALTIMETER
!   TIME AND THE TRUE OF REF OFFSET VECTOR FROM THE CENTER OF
!   MASS TO THE ALTIMETER TRACKING POINT. THE FOLLOWING
!   APPROXIMATIONS AND ASSUMPTIONS ARE MADE:
!
!   1) ALTIMETER ALIGNED TO Z AXIS OF SPACECRAFT
!
!
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/LDA/LDORANT
      COMMON/ALTCB /NALTW,MAXIT,NPERT,NXALT
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/ALTIDQ/NNAQM
      COMMON/ANTID/ISATAN(3),ISTAN1(3),ISATN1(3),ISTAN2(3),ISATN2(3),   &
     &             IANNB1(3),IANNB2(3)
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
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
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
      COMMON/COFSTL/LOFEXT(2)
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
                            ! jjm 9/98
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
                            ! jjm 9/98
      COMMON/CORL02/KLNDTT,KLWSTR,KLNDTR,KLEVMF,KLTPMS,NXCL02
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CORL07/KLPTS,NXCL07
      COMMON/EXATGI/MEASAT,MEAFIL,MEAMEM,MEAPLA,MEAANT,NXEAGI
      COMMON/LALTPT/LNQT
      COMMON/LANTID/LANTBD,LASALT
      COMMON/OFFASL/LOFFA1,LOFFA2
      COMMON/RIMAGE/VIMAGE(6),QUCAM(4),QURSYS,TLBOD2
!
      DIMENSION SBFTOR(3,3)
      DIMENSION AA(1),II(1),LL(1),FSEC(NM)
      DIMENSION OFF(MINTIM,3),TRK(MINTIM,3),SCR(MINTIM,11)
      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3)
      DIMENSION OFFSET(3,2),XYOF21(3),XYOF31(3)
      DIMENSION ATROT(3,3,MINTIM)
      DIMENSION HOLD(MINTIM,14)
      DIMENSION DUMA(1)
      DIMENSION RQ1(1000),RQ2(1000),RQ3(1000),RQ4(1000)
      DIMENSION ROTQ(3,3),QUAT(4)
      DATA D6/1.D+06/
      DATA LSIMND/.FALSE./
!
        XNORM=QUCAM(1)*QUCAM(1)+QUCAM(2)*QUCAM(2)+QUCAM(3)*QUCAM(3)     &
     &       +QUCAM(4)*QUCAM(4)
        XTEST=ABS(XNORM-1.D0)
!!!!  FOR SIMULATION
        IF(LSIMND) XTEST=100.D0
!!!!
      IANT=0
      IF(LANTBD) IANT=IANNB1(1)
      IF(LASALT) THEN
      IANT=BLKDAT(4,1)
      INTR=IANT/10
      INTT=IANT-(INTR*10)
      IF(IDA.EQ.1) IANT=INTR
      IF(IDA.EQ.2) IANT=INTT
      ENDIF

      LDORANT=.FALSE.
!
! CHECK FOR APPLICABLE TRANSPONDER OFFSETS OR DELAYS
!
      JXYZOF(1)=KFRQOF
      JXYZOF(2)=KFRQOF
      JXYOF2(1)=KFRQOF
      JXYOF2(2)=KFRQOF
      JXYOF3(1)=KFRQOF
      JXYOF3(2)=KFRQOF
      JSADLY(1)=KFRQSA
      JSADLY(2)=KFRQSA
      JSTDLY(1)=KFRQST
      JSTDLY(2)=KFRQST
      JXYZCG(1,1)=KFRQOF
      JXYZCG(2,1)=KFRQOF
      JXYZCG(1,2)=KFRQOF
      JXYZCG(2,2)=KFRQOF
      MJDFCG(1,1)=0
      MJDFCG(2,1)=0
      MJDFCG(1,2)=0
      MJDFCG(2,2)=0
      LOFEXT(1)=.FALSE.
      LOFEXT(2)=.FALSE.
!
      ISATID=II(KISATN-1+ISAT)
      ISTAID=-99
!!!!!!IF(MTYPE.EQ.33.OR.MTYPE.EQ.34) GO TO 105
!
! Compute OFFSET and CENTER OF MASS pointers for the satellite
!
!!!!      IF(NALTW.GT.0) THEN
!!!!      CALL FNDNUM(ISATID,II(KDSCWV),NALTW,IRET)
!!!!      ENDIF
!!!!      IF(IRET.EQ.0.AND.BLKDAT(3,1).EQ.0.D0) THEN
!!!!      WRITE(6,*)'LASER ALTIMETER WAVELENGTH IS NOT PROVIDED BY THE DATA'
!!!!      WRITE(6,*)'OR PROVIDED ON A ALTWVL OPTION CARD  '
!!!!      WRITE(6,*)'EXECUTION TERMINATED IN ALTPTI  '
!!!!      STOP 16
!!!!      ENDIF
!!!!      IF(IRET.GT.0) BLKDAT(3,1)=VLIGHT*D6/AA(KALTWV-1+IRET)
!!!!      FRQION=BLKDAT(3,1)

!      write(6,*)' dbg REPLACING WVL ',blkdat(3,1)
!      write(6,*)'altpti: call offdel'

      CALL OFFDEL(ISATID,ISTAID,BLKDAT(3,1),                            &
     &   II(KISATO),II(KISATD),II(KISATC),II(KISTAD),                   &
     &   AA(KFRQOF),AA(KFRQSA),AA(KFRQST),II(KMJDCG),JCOOR1,            &
     &   JXYZOF(1),JSADLY(1),JSTDLY(1),JXYZCG(1,1),MJDFCG(1,1),         &
     &   IANT,ii(KANTOF), JXYOF2(1),LL(KNSCID),II(KIANTO),LL(KEXTOF),   &
     &   LOFEXT(1),.TRUE.,II,FRQLNK,1,AA,JXYOF3(1))
!
!
      OFFSET(1,1)=0.D0
      OFFSET(2,1)=0.D0
      OFFSET(3,1)=0.D0
      OFFSET(1,2)=0.D0
      OFFSET(2,2)=0.D0
      OFFSET(3,2)=0.D0
      XYOF21(1)=0.D0
      XYOF21(2)=0.D0
      XYOF21(3)=0.D0
      XYOF31(1)=0.D0
      XYOF31(2)=0.D0
      XYOF31(3)=0.D0
      IF(JXYZOF(1).GT.KFRQOF) THEN
         OSCALE=0.D0
         IF(JXYZCG(1,1).GT.KFRQOF) OSCALE=1.D0
         DO 100 I=1,3
         OFFSET(I,1)=AA(JXYZOF(1)-1+I)-AA(JXYZCG(1,1)-1+I)*OSCALE
         OFFSET(I,2)=AA(JXYZOF(1)-1+I)-AA(JXYZCG(2,1)-1+I)*OSCALE
!        print *,'altpti: i,jxyzof: ',i,aa(JXYZOF(1)-1+I)
!         print *,'altpti: i,jxyzcg: ',i,aa(JXYZCG(1,1)-1+I)*oscale
!         print *,'altpti: i,jxyzcg 2: ',i,aa(JXYZCG(2,1)-1+I)*oscale
  100    CONTINUE
      ENDIF
  105    CONTINUE
!
!
! IF SUBROUTINE OFFDEL WAS ABLE TO DEFINE JCOOR1, JCOOR1 WILL BE
! GREATER THAN 0; OTHERWISE IT WILL BE -9999
      IF(JCOOR1.LT.0) THEN
        ICRSYS=0
      ELSE
        ICOOR1=II(JCOOR1)
        ICRSYS=ICOOR1/100
      ENDIF
!
        DO 110 N=1,NM
        DO 110 I=1,3
        DO 110 J=1,3
        ATROT(I,J,N)=0.D0
  110   CONTINUE
        DO 120 N=1,NM
        DO 120 J=1,3
        ATROT(J,J,N)=1.D0
  120   CONTINUE
        IF(NASAT.GT.0) THEN
!       print *,'altpti: call to getrpy'
!!!!        IF(LXBEAM) THEN
!!!!        CALL XETRPY(AA,II,MJDSBL,FSEC,II(KATSAT),II(KKVLAS),AA(KATIME), &
!!!!     &              II(KATRSQ),ISATID,ITERP,ATROT,AA(KATPER),ISET1,ROLL,&
!!!!     &              PITCH,YAW,TDIF,SINWT,COSWT,.TRUE.,.FALSE.,XSM,VSM)
!!!!        ELSE
        CALL GETRPY(AA,II,MJDSBL,FSEC,II(KATSAT),II(KKVLAS),AA(KATIME), &
     &              II(KATRSQ),ISATID,ITERP,ATROT,AA(KATPER),ISET1,ROLL,&
     &              PITCH,YAW,TDIF,SINWT,COSWT,.TRUE.,.FALSE.,XSM,VSM)
!!!!        ENDIF
!      write(6,*)'dbg aft getrpy',ATROT(1,1,1),ATROT(1,2,1),ATROT(1,3,1)
!      write(6,*)'dbg aft getrpy',ATROT(2,1,1),ATROT(2,2,1),ATROT(2,3,1)
!      write(6,*)'dbg aft getrpy',ATROT(3,1,1),ATROT(3,2,1),ATROT(3,3,1)
       ENDIF
!
! COMPUTE OFFSET CORRECTION USING EXTERNAL ATTITUDE ORIENTATION
! IF REQUESTED
!
      IF(LOFEXT(1).OR.MTYPE.EQ.33.AND..NOT.LSIMND) THEN

        IF(.NOT.LL(KEALQT+ISET1-1).AND.XTEST.GT.0.001D0) THEN
           WRITE(6,6000)
           WRITE(6,6001)
           WRITE(6,6002)
           WRITE(6,6003)
           WRITE(6,6004)
           STOP
        ENDIF

        ISBJ20 = II(KEASBJ+ISET1-1)
        NUMAEA = II(KEANAN+ISET1-1)
        IPANEA = II(KEAPAN+ISET1-1)
        MJDSEA = II(KEAMJS+ISET1-1)
        FSSCEA = AA(KEAFSS+ISET1-1)
        RINTEA = AA(KEAINS+ISET1-1)
        LINATO = .FALSE.

      ENDIF
            JQ1=KOBCOR(6,4)
            JQ2=KOBCOR(7,4)
            JQ3=KOBCOR(8,4)
            JQ4=KOBCOR(9,4)

! FOR SIMULATION
        LXAH=LXATTD
        IF(MTYPE.EQ.33.OR.MTYPE.EQ.34) LXATTD=.FALSE.
        IF(LSIMND) LXATTD=.TRUE.
!
        IF(LXATTD) THEN
        DO I=1,NM
         IF(LSIMND) THEN
           ZQ1=-XSM(I,1)
           ZQ2=-XSM(I,2)
           ZQ3=-XSM(I,3)
           RQ=SQRT(ZQ1*ZQ1+ZQ2*ZQ2+ZQ3*ZQ3)
           ZQ1=ZQ1/RQ
           ZQ2=ZQ2/RQ
           ZQ3=ZQ3/RQ
           YQ1=ZQ2*VSM(I,3)-ZQ3*VSM(I,2)
           YQ2=ZQ3*VSM(I,1)-ZQ1*VSM(I,3)
           YQ3=ZQ1*VSM(I,2)-ZQ2*VSM(I,1)
           RQ=SQRT(YQ1*YQ1+YQ2*YQ2+YQ3*YQ3)
           YQ1=YQ1/RQ
           YQ2=YQ2/RQ
           YQ3=YQ3/RQ
           XQ1=YQ2*ZQ3-YQ3*ZQ2
           XQ2=YQ3*ZQ1-YQ1*ZQ3
           XQ3=YQ1*ZQ2-YQ2*ZQ1
           ROTQ(1,1)=XQ1
           ROTQ(2,1)=XQ2
           ROTQ(3,1)=XQ3
           ROTQ(1,2)=YQ1
           ROTQ(2,2)=YQ2
           ROTQ(3,2)=YQ3
           ROTQ(1,3)=ZQ1
           ROTQ(2,3)=ZQ2
           ROTQ(3,3)=ZQ3
           CALL ROTQAT(ROTQ,QUAT)
           RQ1(I)=QUAT(1)
           RQ2(I)=QUAT(2)
           RQ3(I)=QUAT(3)
           RQ4(I)=QUAT(4)
         ELSE
           RQ1(I)=AA(JQ1-1+I)
           RQ2(I)=AA(JQ2-1+I)
           RQ3(I)=AA(JQ3-1+I)
           RQ4(I)=AA(JQ4-1+I)
!        ENDIF FOR LSIMND
         ENDIF
        ENDDO
        ELSE
        DO I=1,NM
        RQ1(I)=0.D0
        RQ2(I)=0.D0
        RQ3(I)=0.D0
        RQ4(I)=0.D0
        ENDDO
!       ENDIF FOR LXATTD
        ENDIF
!
        LXH=LXAH
!
! IN SOME CASES SBF TO PLANET FIXED QUATERNIONS WILL BE AVAILABLE
! AT MEASUREMENT EPOCHS. THESE SHOULD ONLY BE USED ASALAST RESORT,
! WHEN THEY ARE USED, USE THE PLANWTARY ORIENTATION MODEL TO GET
! THE SBF TO J2000 QUATERNIONS
!
        IF((MTYPE.EQ.33.OR.MTYPE.EQ.34).AND.XTEST.LT.0.1D0) THEN
          LXATTD=.TRUE.
           IF(QURSYS.LT.0.05D0) THEN
            CALL QUATBK(MJDSBL,FSEC,NM,AA,II,SCR,AA(KCOSTG),AA(KSINTG), &
     &                RQ1,RQ2,RQ3,RQ4,IAST)
           ELSE
             RQ1(1)=QUCAM(1)
             RQ1(2)=QUCAM(2)
             RQ1(3)=QUCAM(3)
             RQ1(4)=QUCAM(4)
           ENDIF
        ENDIF
!
        IF(LOFEXT(1) .OR. LXATTD.OR.MTYPE.EQ.33.OR.MTYPE.EQ.34) THEN
        CALL TRKXTI(AA(KEAQAT),II(KEAAAA),MEAMEM,MEAANT,                &
     &              OFF,OFFSET,                                         &
     &              XYOF21,FSEC,TRK,SCR,IANT,ISBJ20,NUMAEA,             &
     &              IPANEA,MJDSEA,FSSCEA,RINTEA,LINATO,AA,II,           &
     &              ICRSYS,.TRUE.,MINTIM,MJDSBL,ATROT,AA(KOFDRV),       &
     &              LOFFA1,RQ1,RQ2,RQ3,RQ4,.TRUE.,LL(KLANTC),           &
     &              II(KANCUT),.FALSE.,LWND,DUMA,1,1,AA(KANTBL),        &
     &              AA(KPHC),XYOF31,ISATID,SBFTOR)
        LXATTD=LXH
!      write(6,*)'dbg aft tr',ATROT(1,1,1),ATROT(1,2,1),ATROT(1,3,1)
!      write(6,*)'dbg aft tr',ATROT(2,1,1),ATROT(2,2,1),ATROT(2,3,1)
!      write(6,*)'dbg aft tr',ATROT(3,1,1),ATROT(3,2,1),ATROT(3,3,1)
        IF(.NOT.LINATO) GO TO 500
      ENDIF
      IF(LX2ALT) LNQT=.TRUE.
!
! ANALYTICAL ATTITUDE MODELS
      IF(ICRSYS.LT.0.OR.ICRSYS.GT.14) THEN
          WRITE(6,6000)
          WRITE(6,6001)
          WRITE(6,6005) ICRSYS
          WRITE(6,6006)
          STOP
      ENDIF
      IF(ICRSYS.EQ.1.OR.ICRSYS.EQ.4) THEN
         CALL TRKPC2(XSM,OFF,SCR,OFFSET,FSEC,SCR(1,10),AA,              &
     &               .TRUE.,LOFFA1,II,ISET,.TRUE.,MINTIM,ATROT,         &
     &                AA(KOFDRV),HOLD(1,1),HOLD(10,1),LL(KLANTC),       &
     &                II(KANCUT),.FALSE.,LWND,DUMA,1,1)
      ENDIF
      IF(ICRSYS.EQ.2) THEN
         CALL TRKTOP(XSM,VSM,OFF,OFFSET,SCR,FSEC,                       &
     &               TRK,SCR,LL(KLPTS),AA,II,II,                        &
     &               AA(KTPMES+4*MINTIM),LL(KLTPMS+6*MINTIM),           &
     &               .FALSE.,1,ISATID,.TRUE.,MINTIM,ATROT,DUMA,.FALSE., &
     &               .FALSE.,.FALSE.,DUMA,LL(KLANTC),                   &
     &                II(KANCUT),.FALSE.,LWND,DUMA,1,1,AA(KANTBL),      &
     &                LDORANT)
!!!      ....added CORL02 common block because KLTPMS was undefined ! jj
!!!      ....added CORA02 common block because KTPMES was undefined ! jj
      ENDIF
      IF(ICRSYS.EQ.3) THEN
          CALL TRKSPT(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,ISATID,     &
     &                .TRUE.,MINTIM,ATROT,LL(KLANTC),II(KANCUT),        &
     &                AA(KSTAIN),ISTAID,DUMA,AA(KANTBL),LDORANT)
      ENDIF
      IF(ICRSYS.EQ.5) THEN
          CALL TRKERS(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,ISATID,     &
     &                .TRUE.,MINTIM,ATROT,LL(KLANTC),II(KANCUT),        &
     &                .FALSE.,.FALSE.,DUMA,AA(KSTAIN),ISTAID,DUMA,      &
     &                AA(KANTBL),LDORANT)
      ENDIF
      IF(ICRSYS.EQ.6) THEN

! Note that this model assumes that the HGA points at the Earth and
! gimbal angles are fixed.  Therefore, the position of the tracking
! point in the body-fixed system is constant.   Also, note that since
! the rotation about the s/c Y-axis is ignored, the location of the
! HGA in inertial space is necessarily correct. However, with Doppler
! measurements this approximation is suitable.
          CALL TRKMO(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,                   &
     &               XYOF21,AA,II,.TRUE.,MINTIM,ATROT,LL(KLANTC),       &
     &               II(KANCUT),ISATID,AA(KOFDRV),LOFFA1)
      ENDIF
      IF(ICRSYS.EQ.7.OR.ICRSYS.EQ.8 ) THEN
! The following TDRSS routine only works with single link offset
          CALL TRKTDS(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,            &
     &                .TRUE.,MINTIM,ATROT,LL(KLANTC),II(KANCUT),ISATID, &
     &                AA(KOFDRV),LOFFA1)
      ENDIF
      IF(ICRSYS.EQ.9) THEN
          CALL TRKMAG(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,ISATID,     &
     &                .TRUE.,MINTIM,ATROT,LL(KLANTC),II(KANCUT),        &
     &                AA(KOFDRV),LOFFA1)
      ENDIF
      IF(ICRSYS.EQ.10) THEN
          CALL TRKGFO(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,ISATID,     &
     &                .TRUE.,LOFFA1,AA(KOFDRV),.TRUE.,MINTIM,ATROT,     &
     &                LL(KLANTC),II(KANCUT))
      ENDIF
      IF(ICRSYS.EQ.11) THEN
          CALL TRKTRM(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,ISATID,     &
     &                LL(KLANTC),II(KANCUT),MINTIM,AA(KOFDRV),LOFFA1)
      ENDIF
      IF(ICRSYS.EQ.12) THEN
          CALL TRKEUV(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,ISATID,     &
     &               LL(KLANTC),II(KANCUT),MINTIM,AA(KOFDRV),LOFFA1)
      ENDIF

      IF(ICRSYS.EQ.13) THEN

! pd %%%%%% unit vector x is in the direction of forward velocity

!jjm          CALL TRKVCL(XSM,VSM,OFF,OFFSET,FSEC,TRK,SCR,AA,II,            &
!jjm     &                .TRUE.,MINTIM,ATROT,LL(KLANTC),II(KANCUT),ISATID)

           CALL TRKICE(XSM,VSM,off,OFFSET,FSEC,trk,scr,AA,II,           &
                      .TRUE.,MINTIM,ATROT,LL(KLANTC),II(KANCUT),ISATID, &
                      .TRUE., duma, .FALSE., LWND,duma, 1, 1,           &
                      AA(KANTBL) ,.FALSE. ,duma )
!altpti CALL TRKTOP(XSM,VSM,OFF,OFFSET,SCR,FSEC,                       &
!     &               TRK,SCR,LL(KLPTS),AA,II,II,                        &
!     &               AA(KTPMES+4*MINTIM),LL(KLTPMS+6*MINTIM),           &
!     &               .FALSE.,1,ISATID,.TRUE.,MINTIM,ATROT,DUMA,.FALSE., &
!     &               .FALSE.,.FALSE.,DUMA,LL(KLANTC),                   &
!     &                II(KANCUT),.FALSE.,LWND,DUMA,1,1,AA(KANTBL))
!robsum    CALL TRKTOP(XSM,VSM,URNM2,OFFSET,ELEVSC,FSECSA,         &
!                       RA,WORK,LPTS,AA,II,INDSTA,                       &
!                       TPMES1,LTPMS1,.TRUE.,2,ISATID,.FALSE.,NM,ATROT,  &
!                       AA(KPHC),LGPSTP,.TRUE.,LOFFA1,AA(KOFDRV),        &
!                       LL(KLANTC),II(KANCUT),LAPWND,L2PWND,W2PU,ISEQ,   &
!                       IDNW,ANTTAB)



      ENDIF
  500 CONTINUE
      IF(LOFEXT(1).AND.LINATO) NNAQM=NNAQM+NM
      RETURN
 6000 FORMAT(' ')
 6001 FORMAT(' EXECUTION TERMINATING IN SUBROUTINE ALTPTI.')
 6002 FORMAT(' EXTERNAL ATTITUDE REQUESTED FOR ALTIMETER')
 6003 FORMAT(' POINTING FOR SATELLITE ',I9,' BUT NO EXTERNAL')
 6004 FORMAT(' ATTITUDE FILE WAS PROVIDED.')
 6005 FORMAT(' ATTITUDE MODEL # ',I3,' REQUESTED BUT IS')
 6006 FORMAT(' NOT AVAILABLE.')
      END
