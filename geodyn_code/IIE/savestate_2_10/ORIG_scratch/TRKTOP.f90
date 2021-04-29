!$TRKTOP
      SUBROUTINE TRKTOP(XSM,VSM,UI,OFFSET,ELEV,FSECRA,                  &
     &                  RA,DR,LPTS,AA,II,INDSTA,                        &
     &                  TPMEAS,LTPMES,LAPLRA,IMODE,ISATID,              &
     &                  LALT,NDIMRA,ATROT,PHC,LGPSTP,L1ST,LOFFAJ,       &
     &                  DWRKDO,LANTCT,IANTSC,LWNDI,LWNDO,WPU,ISEQ,IUPDN,&
     &                  STAINF,ANTTAB)
!********1*********2*********3*********4*********5*********6*********7**
! TRKTOP           91/12/16            9110.3    PGMR - ANDREW MARSHALL
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!            TRACKING POINT AND S/C C.G. NOT AT BODY-FIXED
!            ORIGIN FOR TOPEX
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I         EARTH CENTERED INERTIAL POSITION VECTORS
!   VSM      I         EARTH CENTERED INERTIAL VELOCITY VECTORS
!   UI       I         UNIT INERTIAL TOPOCENTRIC S/C POSITION
!   OFFSET   I         BODY CENTERED FIXED TRACKING POINT LOCATION
!   ELEV     I    A    S/C ELEVATION WRT STATION
!   FSECRA   I    A    ANTENNA CORR. TIME SINCE BLOCK START
!   LPTS     I    A    LOGICAL FLAGS FOR ACCEPTED SIMULATED OBSERVATION
!                      IN A PSEUDO BLOCK
!   RA            A    OFFSET LOCATION MAPPED INTO INERTIAL COORD.
!   DR       O         CORRECTIONS TO RANGES
!   TPMES1   I    A    TOPEX ATT. BACK VALUES (REAL)
!   LTPMS1   I    A    TOPEX ATT. BACK VALUES (LOG.)
!   LAPLRA   I    S    IF TRUE THEN MTYPE IS 51 AND APPLY LRA CORRECTION
!   IMODE    I    S    =2 COMPUTE LRA CORRECTION ONLY
!   LANTCT   I    A    LOGICAL ANTENNA CUTOFF
!   IANTSC   I    A    ANTENNA CUT SATELLITE IDS
!
!
! COMMENTS:
!
! REFERENCE:
!   MINOTT, P.O., "READER'S GUIDE TO THE "RETRO" PROGRAM OUTPUT,"
!    NASA GSFC X-722-76-267, SEPTEMBER, 1976.
!
!********1*********2*********3*********4*********5*********6*********7**
      use antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

! JASON GPS ANTENNA NUMBER 2 NORMAL IN SBF

      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/CANTEN/NCUT,NXANTE
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
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
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      COMMON/NLRA/NLRARC,NLRAIN,NLRAD1,NLRAD2,NLRAPT,NLRAST,NSARAL, &
     &            NXNLRA
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/CTHDOT/THDOT
      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),OFFSET(3,2),UI(NDIMRA,3),   &
     &   RA(NDIMRA,3),DR(NM),FSECRA(NM),ELEV(NM),LPTS(NM)
      DIMENSION BDFOFF(3),UNTOFF(3),XL(3),UNTPOS(3),UNTVEL(3)
      DIMENSION XPOS(3),VPOS(3)
      DIMENSION AA(1),II(1),DUM(9)
      DIMENSION INDSTA(3),TPMEAS(MINTIM,2),LTPMES(MINTIM,3)
      DIMENSION TPMES(2),LTPMS(3),CROSS(3),ROT(9),ROT2(3,3)
      DIMENSION ATROT(3,3,MINTIM)
      DIMENSION PHC(NM)
      DIMENSION DWRKDO(NM,3)
      DIMENSION LANTCT(1)
      DIMENSION IANTSC(1)
      DIMENSION WPU(NM,3,2),VWND(3,2)
      DIMENSION ANTTAB(1),DUMQ(1)
      DIMENSION STAINF(NSTAIN,*)

      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
      EQUIVALENCE(ROT,ROT2)

      LOGICAL :: L_sat_phc
      LOGICAL :: L_ant_phc
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
! debug extatt
!      print *,'********** TRKTOP **********'
!      print *,'trktop: imode is: ',imode
!      write(6,*)'enter trktop:imode',imode
! debug extatt
!       write(6,*)'INDSTA'
!       write(6,*)INDSTA

      IANT=-9999
      LPHC=.FALSE.

      L_sat_phc  = .FALSE.
      L_ant_phc  = .FALSE.

      PHC(1:NM) = 0.0D0  ! jjm 20120601

      !IF( MTYPE .EQ. 85 ) LPHC=.TRUE.


      !write(6,'(A,1x,I4,1x,F6.1)') &
      !      'trktop:1 MTYPE, XGPSTB      ', MTYPE, XGPSTB
      !write(6,'(A,1x,I4,1x,F6.1)') &
      !      'trktop:1 ISEQ, XANTSL(ISEQ) ', &
      !                ISEQ, XANTSL(ISEQ)

      if( XGPSTB .GT. 0.D0  .or.                     &
          MTYPE  .EQ. 51    .OR.                     &
          MTYPE  .EQ. 39         ) then

          !write(6,'(A,1x,I4,1x,F6.1)') &
          !      'trktop:2 MTYPE, XGPSTB            ', MTYPE, XGPSTB

          IANT=(XANTSL(ISEQ)+.001D0)

          !write(6,'(A,1x,I4,1x,F6.1, 1x, I6)') &
          !      'trktop:2 ISEQ, XANTSL(ISEQ), IANT ', &
          !                ISEQ, XANTSL(ISEQ), IANT

      endif ! xgpstb...



      ! IF(IANT.GT.0.AND.IANT.LE.99) LPHC=.TRUE.
      !if( MTYPE .EQ. 85  .and.  &
      !    iant > 0                 ) LPHC = .TRUE.   ! jjm 20121203

!      write(6,'(A,1x,I4,1x,I6, 4x, L1)') 'trktop: ISEQ, IANT, LPHC ', &
!                                                  ISEQ, IANT, LPHC
!      write(6,'(A,1x,I10)')  'trktop: ISATID ', ISATID


      if( nant_sat > 0 )then
          call get_index2( isatid, iant, II(KANTYP),  NANT_sat, &
                           index_sat, L_sat_phc )
!          write(6,'(A,1x,I10, 2(1x,I6), 4x, L1)') &
!                 'trktop: ISATID, iant, index_sat, L_sat_phc', &
!                          ISATID, iant, index_sat, L_sat_phc
      endif


      if( nantpt > 0 )then
          call get_index( iant,  &
                          II(KANTYP+ 2*(NANT_sat + NANT_sta) ),  &
                          NANTPT, index_ant, L_ant_phc )
!          write(6,'(A,1x,I4,1x,I6, 4x, L1)') &
!                 'trktop: IANT, index_ant, L_ant_phc', &
!                          IANT, index_ant, L_ant_phc
      endif  ! nantpt > 0



      !if( .not. L_sat_phc )then
      !    write(501,'(A,1x,I10)') &
      !          'trktop: no PCV info for sat ', ISATID
      !endif ! .not. L_sat_phc




      !IF( MTYPE .EQ. 85                .and.                      &
      !IF( XGPSTB > 0.0D0  .and.  iant > 0  .and.  &
      IF( XGPSTB > 0.0D0  .and.                &
          ( L_sat_phc .or. L_ant_phc )              ) LPHC=.TRUE.

      !IF( MTYPE .EQ. 85  .and.   L_sat_phc  ) LPHC=.TRUE.
      !IF(  L_sat_phc .or. L_ant_phc ) LPHC=.TRUE.

      !write(6,'(A,3x,L1)') 'trktop: LPHC ', LPHC

      IF( LPHC ) THEN

        if( NANTPT > 0 .and. L_ant_phc )then

            IPOFF=KANTPS(index_ant) + isum_sat + isum_sta
            NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
            NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

            !write(6,'(A,4(1x,I6))') &
            !      'trktop:a IANT, IPOFF, NDIMA, NDIMZ ', &
            !                IANT, IPOFF, NDIMA, NDIMZ

        endif ! L_ant_phc


        if( L_sat_phc )then

            IPOFF = KANTPS_sat(index_sat)

            NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
            NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

            !write(6,'(A,4(1x,I6))') &
            !      'trktop: sa index_sat, IPOFF, NDIMA, NDIMZ ', &
            !                  index_sat, IPOFF, NDIMA, NDIMZ

            !write(6,'(A,4(1x,I10))') &
            !      'trktop: satid ... &
            !      &index_sat, ii( KANTYP + (index_sat -1) * 2   ) ', &
            !       index_sat, ii( KANTYP + (index_sat -1) * 2   )
            !write(6,'(A,4(1x,I10))') &
            !      'trktop: ant   ... &
            !      &index_sat, ii( KANTYP + (index_sat -1) * 2 +1) ', &
            !       index_sat, ii( KANTYP + (index_sat -1) * 2 +1)


        endif ! L_sat_phc


      ENDIF ! LPHC


!      write(6,'(A,3(1x,I4))') 'trktop: MTYPE, ISEQ ', &
!                                       MTYPE, ISEQ
!      write(6,'(A,3x,L1)') 'trktop: LPHC ', LPHC


! INITIALIZE THE POINTER AND DIMENSIONS OF THIS TABLE


      LWNDO=LWNDI
      IF(IMODE.EQ.2) GOTO 110
       IF(LWNDI) THEN
          IF(IUPDN.EQ.1) THEN
            VWND(1,1)=VHIGH(1,1,ISEQ)
            VWND(2,1)=VHIGH(2,1,ISEQ)
            VWND(3,1)=VHIGH(3,1,ISEQ)
            VWND(1,2)=VHIGH(1,2,ISEQ)
            VWND(2,2)=VHIGH(2,2,ISEQ)
            VWND(3,2)=VHIGH(3,2,ISEQ)
          ELSE
            VWND(1,1)=VLOW(1,1,ISEQ)
            VWND(2,1)=VLOW(2,1,ISEQ)
            VWND(3,1)=VLOW(3,1,ISEQ)
            VWND(1,2)=VLOW(1,2,ISEQ)
            VWND(2,2)=VLOW(2,2,ISEQ)
            VWND(3,2)=VLOW(3,2,ISEQ)
          ENDIF
       ENDIF
!
! COMPUTE BODY FIXED OFFSET UNIT VECTOR
      VMAG = SQRT(OFFSET(1,2)**2+OFFSET(2,2)**2+OFFSET(3,2)**2)
!      print *,'offset2: ',offset(1,2),offset(2,2),offset(3,2)
!      print *,'vmag: ',vmag
      BDFOFF(1)=OFFSET(1,2)/VMAG
      BDFOFF(2)=OFFSET(2,2)/VMAG
      BDFOFF(3)=OFFSET(3,2)/VMAG
!
! COMPUTE INERTIAL OFFSET UNIT VECTOR
      DO 100 I=1,NM
      IF(.NOT.LPTS(I))  THEN
         RA(I,1) = ZERO
         RA(I,2) = ZERO
         RA(I,3) = ZERO
         IF(.NOT.LALT) GOTO 100
      ENDIF
      XPOS(1) = XSM(I,1)
      XPOS(2) = XSM(I,2)
      XPOS(3) = XSM(I,3)
      VPOS(1) = VSM(I,1)
      VPOS(2) = VSM(I,2)
      VPOS(3) = VSM(I,3)
! COMPUTE TRUE OF REFERENCE SUN POSITION AT REQUESTED MEAS. TIME
        CALL PLANPO(MJDSBL,FSECRA(I),.FALSE.,.FALSE.,AA,II)
        TPMES(1)=TPMEAS(I,1)
        TPMES(2)=TPMEAS(I,2)
        LTPMS(1)=LTPMES(I,1)
        LTPMS(2)=LTPMES(I,2)
        LTPMS(3)=LTPMES(I,3)
!        print *,'trktop: before call to topatt'
        CALL TOPATT(MJDSBL,FSECRA(I),XPOS,VPOS,BDFOFF(1),BDFOFF(2),     &
     &          BDFOFF(3),UNTOFF(1),UNTOFF(2),UNTOFF(3),                &
     &          DUM,1,0,AA(KTPSTR),AA(KTPSTP),II(KITPMD),               &
     &         AA(KTPFYW),.FALSE.,DUM,DUM,TPMES,LTPMS,BDSTAT(1,8),      &
     &          ISATID,II(KIDATB),                                      &
     &          AA(KSABIA),AA(KSBTM1),AA(KSBTM2),AA(KYAWBS),            &
     &          II(KISATN),AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV), &
     &          AA,IDUM,ROT,II(KYAWID),ATROT(1,1,I),II(KTPATS))
       IF(LWNDI) THEN
          DO JJ=1,2
          DO III=1,3
            WPU(I,III,JJ)=ROT2(III,1)*VWND(1,JJ)                        &
     &                   +ROT2(III,2)*VWND(2,JJ)                        &
     &                   +ROT2(III,3)*VWND(3,JJ)
          ENDDO
          ENDDO

          !if( i == 1 ) then
          !    write(6,'(A,3(1x,E15.7))') &
          !          'trktop:  WPU(1,1,1:3) ', WPU(1,1,1:3)
          !    write(6,'(A,3(1x,E15.7))') &
          !          'trktop:  WPU(1,2,1:3) ', WPU(1,2,1:3)
          !    write(6,'(A,3(1x,E15.7))') &
          !          'trktop:  WPU(1,3, 1:3) ', WPU(1,3,1:3)
          !endif


       ENDIF ! LWNDI

      IF(LOFFAJ) THEN
       IF(L1ST) THEN
         DWRKDO(I,1)=UI(I,1)*ROT(1)                                     &
     &              +UI(I,2)*ROT(2)                                     &
     &              +UI(I,3)*ROT(3)
         DWRKDO(I,2)=UI(I,1)*ROT(4)                                     &
     &              +UI(I,2)*ROT(5)                                     &
     &              +UI(I,3)*ROT(6)
         DWRKDO(I,3)=UI(I,1)*ROT(7)                                     &
     &              +UI(I,2)*ROT(8)                                     &
     &              +UI(I,3)*ROT(9)
       ELSE
         DWRKDO(I,1)=-UI(I,1)*ROT(1)                                    &
     &               -UI(I,2)*ROT(2)                                    &
     &               -UI(I,3)*ROT(3)
         DWRKDO(I,2)=-UI(I,1)*ROT(4)                                    &
     &               -UI(I,2)*ROT(5)                                    &
     &               -UI(I,3)*ROT(6)
         DWRKDO(I,3)=-UI(I,1)*ROT(7)                                    &
     &               -UI(I,2)*ROT(8)                                    &
     &               -UI(I,3)*ROT(9)
       ENDIF !  L1ST

      ENDIF !  LOFFAJ



! scott
!       if( i == 1 )then
!           write(6,'(A,1x,I8,3(1x,E15.7))')  &
!                 'trktop: i,ui ',i,ui(i,1),ui(i,2),ui(i,3)
!           write(6,'(A,1x,E15.7)')  'trktop: XGPSTB ', XGPSTB
!       endif ! i == 1

!  ANTENNA CUTOFF FOR GPS SATELLITES

      if( XGPSTB .GT. 0.D0 ) then

          !write(6,'(A)')'trktop: call antang'

          CALL ANTANG(ISEQ,ROT,UI(I,1),UI(I,2),UI(I,3), LANTCT(I))

      endif !  XGPSTB .GT. 0.D0


! debug scott >>>>


!      untoff1 = rot(1)*bdfoff(1)+rot(4)*bdfoff(2)+rot(7)*bdfoff(3)
!      untoff2 = rot(2)*bdfoff(1)+rot(5)*bdfoff(2)+rot(8)*bdfoff(3)
!      untoff3 = rot(3)*bdfoff(1)+rot(6)*bdfoff(2)+rot(9)*bdfoff(3)


!      untoff1 = rot(1)*bdfoff(1)+rot(4)*bdfoff(2)+rot(7)*bdfoff(3)
!      untoff2 = rot(2)*bdfoff(1)+rot(5)*bdfoff(2)+rot(8)*bdfoff(3)
!      untoff3 = rot(3)*bdfoff(1)+rot(6)*bdfoff(2)+rot(9)*bdfoff(3)

!      print *,'trktop: untoff1 comp: ',untoff1, untoff(1)
!      print *,'trktop: untoff2 comp: ',untoff2, untoff(2)
!      print *,'trktop: untoff3 comp: ',untoff3, untoff(3)



! COMPUTE UNIT POSITION AND VELOCITY VECTORS
!      POSMAG = SQRT(XSM(I,1)**2+XSM(I,2)**2+XSM(I,3)**2)
!      UNTPOS(1) = XSM(I,1)/POSMAG
!      UNTPOS(2) = XSM(I,2)/POSMAG
!      UNTPOS(3) = XSM(I,3)/POSMAG

!      uimag = SQRT(ui(I,1)**2+ui(I,2)**2+ui(I,3)**2)

!      tmp = untpos(1)*dum(1)+untpos(2)*dum(2)+untpos(3)*dum(3)
!      tmp2 = acos(tmp)*57.29577951308D0

!      if(tmp2.le.29.5D0.or.tmp2.ge.30.5D0) then
!      print *,'trktop: ang with pos: ',tmp2
!      endif


!      if(abs(tmp2).ge.0.0D0.and.abs(tmp2).lt.40.0D0) then
!      print *,'trktop: tmp2 uimag: ',tmp2,uimag
!      endif

!      if(abs(tmp2).le.180.0D0.and.abs(tmp2).gt.160.0D0) then
!      print *,'trktop: tmp2: ',tmp2
!      endif

! debug scott <<<<


      XV=UI(I,1)
      YV=UI(I,2)
      ZV=UI(I,3)
      IF(L1ST) THEN
          XV=-XV
          YV=-YV
          ZV=-ZV
      ENDIF

      IF( LPHC ) THEN

          if( i == 1 )then

              !write(6,'(A,3(1x,E15.7))') &
              !      'trktop: XV, YV, ZV ', XV, YV, ZV
              !write(6,'(A,2(1x,I10))')  'trktop: I, IMQP ', i, imqp

              if( NANTPT > 0 .and. L_ant_phc )then
                  !write(6,'(A,3(1x,I10))') &
                  !      'trktop: iant, kantps(iant), ipoff ', &
                  !               iant, kantps(iant), ipoff
              endif ! L_ant_phc


              if( L_sat_phc )then

                  IPOFF = KANTPS_sat(index_sat)

                  NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
                  NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

                  !write(6,'(A,4(1x,I10))') &
                  !      'trktop: sa index_sat, IPOFF, NDIMA, NDIMZ ', &
                  !                  index_sat, IPOFF, NDIMA, NDIMZ

                  !write(6,'(A,1x,E15.7,1x,I10)') &
                  !      'trktop: sa ANTTAB(IPOFF+3) , NDIMA   ', &
                  !                  ANTTAB(IPOFF+3) , NDIMA
                  !write(6,'(A,1x,E15.7,1x,I10)') &
                  !      'trktop: sa ANTTAB(IPOFF  ) , NDIMZ   ', &
                  !                  ANTTAB(IPOFF  ) , NDIMZ

                  !write(6,'(A,4(1x,I10))') &
                  !  'trktop: satid &
                  !  &...index_sat, ii( KANTYP + (index_sat-1) * 2 )',&
                  !      index_sat, ii( KANTYP + (index_sat-1) * 2 )
                  !write(6,'(A,4(1x,I10))') &
                  !  'trktop: ant ...  &
                  !  &index_sat, ii( KANTYP + (index_sat-1) * 2 +1)',&
                  !   index_sat, ii( KANTYP + (index_sat-1) * 2 +1)


              endif ! L_sat_phc



          endif ! i == 1


!          write(6,'(A)') 'trktop: call antsac '

          CALL ANTSAC(ROT,XV,YV,ZV,ANTTAB(IPOFF+6),NDIMA,NDIMZ,   &
     &                ANTTAB(IPOFF),ISEQ,PHC(I),2,.FALSE.)


          !!!!!DO NOT CALL ANTSJ2
          !      ELSE
          !        CALL ANTSJ2(ROT,XV,YV,ZV,DUMQ(1),ISEQ,PHC(I))
          !!!!!DO NOT CALL ANTSJ2

      ENDIF  ! LPHC


!     print *,'trktop: bdfoff,untoff: '
!      print *,bdfoff
!      print *,untoff
!      stop 69
!
!!!ADD UNIT 22 STATION PHASE CORRECTION, jtw 20130416
         CALL FNDNUM(MTYPE,NANTMT,99,IDUM)
         IF (IDUM.GT.0) THEN
            IDORTAB=0
            IF(MTYPE.EQ.NANTMT(IDUM)) THEN
            IDORTAB=INT(STAINF(16,INDSTA(2)))
            ITAB=KANTPS(IDORTAB)
            IF(NM==1) WRITE(6,*)'(IDORTAB.GT.0)',(IDORTAB.GT.0)
                 IF(IDORTAB.GT.0) THEN
                     CALL DORANT(ANTTAB(ITAB+6),ANTTAB(ITAB),    &
     &                    PHC(I),AA(KELEVS+I-1),                 &
     &                    INT(ANTTAB(ITAB+3)),INT(ANTTAB(ITAB)))
                 END IF
            END IF
         END IF

      RA(I,1) = UNTOFF(1)*VMAG
      RA(I,2) = UNTOFF(2)*VMAG
      RA(I,3) = UNTOFF(3)*VMAG
      IF(LALT) THEN
        UI(I,1)=+ROT(7)
        UI(I,2)=+ROT(8)
        UI(I,3)=+ROT(9)
      ENDIF ! LALT

! debug extatt
!      ramag=ra(i,1)**2+ra(i,2)**2+ra(i,3)**2
!      ramag=sqrt(ramag)
!      print *,'trktop: i,ra: ',i,ra(i,1),ra(i,2),ra(i,3)
!      print *,'trktop: i,ramag: ',i,ramag
! debug extatt
  100 END DO
      IF(LALT) RETURN
!
!...RANGE CORRECTION IS DOT PRODUCT OF INERTIAL OFFSET AND
!...UNIT INERTIAL TOPOCENTRIC VECTORS
      CALL DOTPRD(RA,UI,DR,NM,NM,NM,3)
      IF(LPHC) THEN
        IF(L1ST) THEN
           DO I=1,NM
              DR(I)=DR(I)+PHC(I)
           ENDDO
        ELSE
           DO I=1,NM
              DR(I)=DR(I)-PHC(I)
           ENDDO
        ENDIF ! L1ST
!        write(6,'(A,1x,E20.10)')'trktop: phc(1) ', phc(1)

      ENDIF ! LPHC


      IF (IDORTAB.GT.0) THEN
           DO I=1,NM
              DR(I)=DR(I)+PHC(I)
           ENDDO
      END IF
!       WRITE(6,*)'NM,ELEV,PHC'
!       DO JJ=1,NM
!       WRITE(6,*)JJ,AA(KELEVS+JJ-1),PHC(JJ)
!       ENDDO
!
  110 CONTINUE
!
      IF((NLRARC.EQ. ZERO).or..not.LAPLRA) RETURN
! debug extatt
!      print *,'trktop: computing lra correction'
! debug extatt
! COMPUTE LRA CORRECTION TO OFFSET POINT, TAKING VELOCITY ABBERATION
! EFFECT INTO ACCOUNT (SEE MINOTT PAPER FOR VARIABLE DEFINITIONS)
!
! COMPUTE UNIT POSITION AND VELOCITY VECTORS
      DO 200 I = 1,NM
      IF(.NOT.LPTS(I)) GOTO 200
      POSMAG = SQRT(XSM(I,1)**2+XSM(I,2)**2+XSM(I,3)**2)
      UNTPOS(1) = XSM(I,1)/POSMAG
      UNTPOS(2) = XSM(I,2)/POSMAG
      UNTPOS(3) = XSM(I,3)/POSMAG
! THE FOLLOWING COMPUTATION OF THE RELATIVE VELOCITY BETWEEN THE S/C AND
! STATION IS BASED ON:  1) THE INERTIAL VELOCITY OF EARTH FIXED STATION
! VSTA = W X R .  2) R THE INERTIAL STATION POSITION IS THE XTN STATION
! FOR MEASUREMENT TYPE 51 COMPUTED IN MTSTST.  3) FOR EACH LINK OF MEAS.
! 51 THE SAME STATION POSITION IS USED NAMELY XTN.  4) THIS WILL ONLY
! WORK FOR MEASUREMENT TYPE 51 WHICH IS CONSISTENT WITH THE REST OF THE
! LRA CORRECTION.
      RLVEL1 = VSM(I,1) + AA(KXTN+MINTIM+I-1)*THDOT
      RLVEL2 = VSM(I,2) - AA(KXTN+I-1)*THDOT
      RLVEL3 = VSM(I,3)
      VELMAG = SQRT(RLVEL1**2+RLVEL2**2+RLVEL3**2)
      UNTVEL(1) = RLVEL1/VELMAG
      UNTVEL(2) = RLVEL2/VELMAG
      UNTVEL(3) = RLVEL3/VELMAG
!
! DEFINE X-AXIS AS DESCRIBED IN MINOTT REPORT (UI x UNTPOS)
      XL(1) = UI(I,2)*UNTPOS(3) - UI(I,3)*UNTPOS(2)
      XL(2) = UI(I,3)*UNTPOS(1) - UI(I,1)*UNTPOS(3)
      XL(3) = UI(I,1)*UNTPOS(2) - UI(I,2)*UNTPOS(1)
      XLMAG = SQRT(XL(1)**2 + XL(2)**2 + XL(3)**2)
      XL(1) = XL(1)/XLMAG
      XL(2) = XL(2)/XLMAG
      XL(3) = XL(3)/XLMAG
!
! COMPUTE w ANGLE (COS=-XL dot UNTVEL,SIN=-XL cross UNTVEL)
      COSW = (-XL(1)*UNTVEL(1)  -XL(2)*UNTVEL(2) -XL(3)*UNTVEL(3))
      CROSS(1) = UNTVEL(2)*XL(3) - UNTVEL(3)*XL(2)
      CROSS(2) = UNTVEL(3)*XL(1) - UNTVEL(1)*XL(3)
      CROSS(3) = UNTVEL(1)*XL(2) - UNTVEL(2)*XL(1)
      SINW =  SQRT(CROSS(1)**2+CROSS(2)**2+CROSS(3)**2)
! EVALUATE TRIPLE PRODUCT TO INSURE CORRECT QUADRANT
!(-XL cross UNTVEL dot R: if >1, sinw=sinw, if<1 sinw=-sinw)
      TRIPLE = UNTPOS(1)*CROSS(1)+UNTPOS(2)*CROSS(2)+UNTPOS(3)*CROSS(3)
      IF(TRIPLE.LT.ZERO) SINW=-SINW
!
! COMPUTE v
      V = SQRT(ONE - (AE/POSMAG)**2 * COS(DEGRAD*ELEV(I))**2)
!
! COMPUTE PSI
      TEMP = SQRT(COSW**2+V**2 * SINW**2)
      PSI = TWO/VLIGHT*VELMAG*TEMP
!
! COMPUTE ETA
      ETA = ATAN2(SINW*V,COSW)
!
! FIND GRIDPOINT IN LRA RANGE CORRECTION MATRIX
! (51x51,2micro-radian per pixel, center at (26.0,26.0))
      X = PSI*1.0D6*COS(ETA)/2.0D0+26.0
      Y = PSI*1.0D6*SIN(ETA)/2.0D0+26.0
!
! COMPUTE INCIDENCE ANGLE (ui dot r)
      ANGINC=ACOS(UI(I,1)*UNTPOS(1)+UI(I,2)*UNTPOS(2)+UI(I,3)*UNTPOS(3))
      ANGINC=ANGINC/DEGRAD
! FIND INCIDENCE ANGLE POINTER IN RANGE CORR ARRAY
! THIS ASSUMES 2.5DEG RNGCOR INTERVALS FROM INCIDENCE ANG 0-20 AND
!              5.0DEG RNGCOR INTERVALS FROM INCIDENCE ANG 20-60

      IF( ANGINC .LT. 20.0D0 ) THEN
          IPT = INT(ANGINC/2.5D0)+1
      ELSE
          IPT = INT(ANGINC/5.0D0)+5
      ENDIF

! IF INCIDENCE ANGLE>60.0 DEG, USE 60.0 VALUE
      IF(IPT.GE.NLRAIN) IPT=NLRAIN
! INTERPOLATE FOR RANGE CORRECTION BASED ON INCIDENCE ANGLE (ui dot r)
      CALL ITPLRA(AA(KLRARC),AA(KLRARC+NLRAPT),AA(KLRARC+2*NLRAPT),     &
     &            AA(KLRARC+3*NLRAPT),IPT,RACOR,NLRAIN,NLRAD1,NLRAD2,   &
     &            NLRAST,X,Y,ANGINC,INDSTA,AA(KSTAIN))
! CORRECT RANGE
      DR(I) = DR(I) + RACOR
  200 END DO
      RETURN
      END
