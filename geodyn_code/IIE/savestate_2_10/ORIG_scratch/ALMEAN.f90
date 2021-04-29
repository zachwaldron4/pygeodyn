!$ALMEAN
      SUBROUTINE ALMEAN(AA,II,LL,MJDSBL,FSECN,NM,XTD,XTP,XMP,SATLAT,    &
     &                  SATLON,SATH,THETAG,COSTHG,SINTHG,OTIDES,LSKY,   &
     &                  LAST,LSKIPP,VARR,HOLD,LDIOUT,LXGOUT,ETIDES,     &
     &                  RLAND,JOR)
!********1*********2*********3*********4*********5*********6*********7**
!  ALMEAN          00/00/00         0000.0      PGMR - ?
!
! FUNCTION:  COMPUTES PLANET FIXED COORDINATES GIVEN TOD
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA       I    A
!   II
!   LL
!   MJDSBL   I    S    TIME OF BLOCK START IN INTEGER SECONDS PAST
!                      GEODYN INTERNAL REFERENCE TIME
!   FSECN    I    A    ELAPSED SECONDS FROM MJDSBL OF THE TIMES ASSOCIATED
!                      WITH OBSERVATION TRANSMIT
!   NM       I    A    NUMBER OF MEASUREMENTS
!   XTD      I    A    POSITION VECTOR IN TOD
!   XTP      O    A    POSITION VECTOR IN MEAN POLE
!   XMP      O    A    STATION VECTOR IN MEAN POLE
!   SATLAT   I    A    SATELLITE GEODETIC LATITUDES
!   SATLON   I    A    SATELLITE EAST LONGITUDES
!   SATH     I    A    SATELLITE HEIGHT ABOVE REF. ELLIPSOID
!   THETAG   I    A    VECTOR OF VALUES OF RIGHT ASCENSION OF GREENWICH
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES  OF RIGHT ASCENSION OF GREENWICH
!   OTIDES   I    A    OCEAN TIDE HEIGHTS
!   LSKY     I    S    TRUE IF POINT ON GROUND
!   LAST     I    S    TRUE IF CURRENTLY LAST ITERATION
!   LSKIPP   I    S
!   VARR     I    A    AUXILIARY FOR TIME CONVERSIONS IN THE RAY MODEL
!   HOLD     I    A    Correction due to solid earth tides.
!   LDIOUT   I    S
!   LXGOUT   I    S
!   ETIDES   I    A    SOLID EARTH TIDE HEIGHTS
!   RLAND    I    A    FLAG FOR WHETHER ALTIMETRY IS OVER LAND OR OCEAN
!                       (0=>DON'T KNOW ;1=>LAND; 2=>OCEAN)
!   JOR      I    S    INDICATOR OF COORDINATE SYSTEM PERTURBATIONS FOR
!                      NUMERICAL PARTIALS (0 NO PERT; 1 PERT RA; 2 PERT
!                      DEC; 3 PERT W ANG
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/AXIS/LINTAX
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
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
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/DPLNOR/DXGEO1,DXGEO2,DXFRC1,DXFRC2,XPLNOR
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
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
      COMMON/POLESW/LDNPOL,NXPOLS
      COMMON/TIDEOP/EOPTID(2,3)
      DIMENSION AA(1),II(1),LL(1),FSECN(NM)
      DIMENSION COSTHG(NM),SINTHG(NM),THETAG(NM),OTIDES(NM)
      DIMENSION XTD(MINTIM,3),XTP(MINTIM,3),XMP(MINTIM,3)
      DIMENSION SATLAT(NM),SATLON(NM),SATH(NM)
      DIMENSION XHOLD(1)
      DIMENSION VARR(NM)
      DIMENSION HOLD(MINTIM,6),ETIDES(NM),RLAND(NM)
      DIMENSION XENV(3,3)
      DIMENSION DUM(3),DUM1(3,2,1)
!
!      print *,'almean: at top'

      DO I=1,6
        EOPTID(I,1)=0.D0
      ENDDO

      DO JJ=1,NM
      VARR(JJ)=0.D0
      ENDDO
      LGLOBA=.FALSE.
      LMEAN=.TRUE.
!
!
!
      LGRND=.NOT.LSKY
! all 4 = false
! *** set back to v0302
      LSET=LMEAN.AND.LGRND
      LCET=LSET.AND.LETID2
      LSOT=LMEAN.AND.LGRND
      LCOT=LSOT.AND.LOTRM2
!     LSET=.FALSE.
!     LCET=.FALSE.
!     LSOT=.FALSE.
!     LCOT=.FALSE.
      LOL=LMEAN.AND.LGRND
      LPOLA=LPOLAD.AND.LGLOBA
      LOLA=LOLAJ.AND.LGLOBA
      LETAOF=.NOT.LGLOBA


!
! UNTIL FURTHER CHECKOUT:
!
!      print *,'almean: lgrnd,lcet,lsot:',lgrnd,lcet,lsot
!      print *,'almean: lcot,lol,lpola:',lcot,lol,lpola
!      print *,'almean: letaof,lola:',letaof,lola
!

!
! GET PLANET FIXED POSITION OF SATELLITE AT ALTIM RECEIVE
      IF(ICBDGM.EQ.3) THEN
!
      IF(LOLMD) THEN
!        print *,'almean: call to oldset'
        CALL OLDSET(1,-1,NM,AA(KSTAIN),II(KIP0OL),II(KNDOLA),           &
     &              II(KKIOLA),LOLA,LOLMDS,                             &
     &              JABCOF,JPXSPA,JOLPAE,JOLPAN,JOLPAV,JXLOCV,          &
     &              JABCCM,JPXSP1,JOLPXC,JOLPYC,JOLPZC,                 &
     &              JABCEO,JPXSP2,JOLPXP,JOLPYP,JOLPUP,LALOAD,ISITA,    &
     &              XENV)
      ENDIF
      IF(LEOPTO.OR.LOLMDS.OR.LXYZCO) THEN
         CALL OLTIME(MJDSBL,FSECN,NM,AA(KANGFC),AA(KSPEED),AA(KSCROL),  &
     &               AA(KSCROL+4*NTOLFR),II(KPTOLS),AA(KANGT),          &
     &               AA(KCONAM),AA(KTWRK1),AA(KGRDAN),AA(KUANG),        &
     &               AA(KFAMP),II(KJDN))
      ENDIF
      IF(LEOPTO) THEN
!        print *,'almean: call to oldeop'
         CALL OLDEOP(MJDSBL,FSECN,NM,AA(JABCOF),AA(KANGFC),AA(KSPEED),  &
     &              AA(JXLOCV),XHOLD,AA(KSCROL),AA(KROTMT),LOLA,        &
     &              AA(JPXSPA),NPV0OL(1,1),NPV0OL(2,1),NPV0OL(3,1),     &
     &              II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),        &
     &              AA(JPXSP1),NYZ0OL(1,1),NYZ0OL(2,1),NYZ0OL(3,1),     &
     &              II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),        &
     &              AA(JPXSP2),NEO0OL(1,1),NEO0OL(2,1),NEO0OL(3,1),     &
     &              II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,     &
     &              LEOPTO,XHOLD)
      ENDIF
!        print *,'almean: call to grhrap'
      CALL GRHRAP(MJDSBL,FSECN,.TRUE.,.TRUE. ,AA(KEQN),AA(KSRTCH),      &
     &            THETAG,COSTHG,SINTHG,NM,AA,II,AA(KXUTDT),.TRUE.,DUM,  &
     &            DUM1)
!     do i=1,nm
!     write(6,*)' dbg XTD ',xtd(i,1),xtd(i,2),xtd(i,3),i
!     enddo
      DO 610 I=1,NM
      XTP(I,1)= XTD(I,1)*COSTHG(I)+XTD(I,2)*SINTHG(I)
      XTP(I,2)=-XTD(I,1)*SINTHG(I)+XTD(I,2)*COSTHG(I)
      XTP(I,3)= XTD(I,3)
  610 END DO
! OBTAIN POLAR MOTION ROTATION MATRICES AND PARTIAL DERIVATIVE MATRICES
!        FROM WHICH TO INTERPOLATE ACROSS THE ENTIRE BLOCK OF DATA
!        print *,'almean: call to inpolp'
      CALL INPOLP(AA(KA1UT),AA(KDPSR),AA(KXPUT),AA(KYPUT),.TRUE.,LDNPOL,&
     &   LPOLA,MJDSBL,FSECN,NM,XHOLD,II(KINDPI),NINTPL,II(KNMP),        &
     &   II(KINDP),AA(KPXPOL),AA(KRPOLE),AA(KS0),AA(KXDPOL),AA(KXDOTP))
! COMPUTE TRUE POLE STATION COORDINATES AND INTERPOLATE PARTIAL DERIV.
!         MATRICES FOR ANTIRE BLOCK OF DATA
      NINTP2=NINTPL*2
!        print *,'almean: call to altmpv'
      CALL ALTMPV(AA(KS0),AA(KS1),II(KNMP),II(KINDP),AA(KRPOLE),        &
     &            AA(KPXPOL),LPOLA,XMP,XTP,                             &
     &   AA(KPXSXP),AA(KPXSLV),NM,NINTPL,NINTP2,MJDSBL,                 &
     &   FSECN,COSTHG,SINTHG,LCET,AA(KROTMT),AA(KSTAIN),                &
     &   AA(KPLTDV),AA(KPLTVL),0,AA(KSTVEL),AA(KTIMVL),                 &
     &   AA(KPMPXE),AA(KPMPXI),II(KICNL2),II(KICNH2),.FALSE.,AA,        &
     &   LETAOF,HOLD,II,LSET,ETIDES)
      IF(.NOT.LOL) GO TO 620
      IF(.NOT.LOLMDS.AND..NOT.LXYZCO) GO TO 620
!        print *,'almean: call to oload'
      CALL OLOAD(MJDSBL,FSECN,NM,AA(JABCOF),AA(KANGFC),AA(KSPEED),      &
     &           AA(JXLOCV),XMP,AA(KSCROL),AA(KROTMT),LOLA,             &
     &           AA(JPXSPA),NPV0OL(1,1),NPV0OL(2,1),NPV0OL(3,1),        &
     &           II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),           &
     &           AA(JPXSP1),NYZ0OL(1,1),NYZ0OL(2,1),NYZ0OL(3,1),        &
     &           II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),           &
     &           AA(JPXSP2),NEO0OL(1,1),NEO0OL(2,1),NEO0OL(3,1),        &
     &           II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,LEOPTO, &
     &           0,.TRUE.,MINTIM,LALOAD,AA(KALCOF),II(KSITE),ISITA,     &
     &           XENV)
  620 CONTINUE
      IF(.NOT.LAST) GOTO 626
!     IF(LX2ALT.AND.NOT.LAST) GOTO 626
      IF(LCOT) THEN
!        print *,'almean: ray tide eval'
!      FUTURE SITE OF RICHARD DON'T CALL ME DICK RAY TIDE ROUTINE
!*************************************************
!  ATTENTION FSECN IS NOT A GOOD CHOICE TO STORE THE UTC SECONDS. IN THE
!  FURURE WE SHOULD USE A SCRATCH ARRAY
      KSC1=KSRTCH
      KSC2=KSC1+MINTIM
      KSC3=KSC2+MINTIM
      KSC4=KSC3+MINTIM
      DO 622 I=1,NM
      THETAG(I)=0.D0
  622 END DO
      CALL SUBTRK(XMP,THETAG,AA(KSC2),AA(KSC3),AA(KSC4),XTP(1,1),       &
     &            XTP(1,2),XTP(1,3),NM)
      CALL UTCET(.FALSE.,NM,MJDSBL,FSECN,VARR,AA(KA1UT))

        DO 625 N=1,NM
        IF(RLAND(N).GT..99D0.AND.RLAND(N).LT.1.01D0) THEN
        TIME=DBLE(MJDSBL)+VARR(N)
        RMJD =(TIME/86400.D0)+TMGDN2
        CALL PERTH2(XTP(N,1),XTP(N,2),RMJD,TIDE,LSDATA,.FALSE.)
        ELSE
        LSDATA=.FALSE.
        ENDIF
        IF(LSDATA) THEN
         CALL LPEQMT(RMJD*86400.0D0,XTP(N,1),TIDEL)
         OTIDES(N)=(TIDE+TIDEL)/100.0D0
        ELSE
         OTIDES(N)=-999999.9D0
         OTIDES(N)=0.D0
        ENDIF
  625   CONTINUE
!*********************************
      ENDIF

  626   CONTINUE

      IF(LSOT) THEN
!        print *,'almean: ray tide eval2'
        KSC1=KSRTCH-1
        DO 630 I=1,NM
! LET -9999999 TIDES STAY. THEY WILL CAUSE EDITS
!       IF(OTIDES(I).LT.-999998.D0) GO TO 630
        AA(KSC1+I)=1.D0-OTIDES(I)/SQRT(XMP(I,1)*XMP(I,1)       &
     &                                       +XMP(I,2)*XMP(I,2)       &
     &                                       +XMP(I,3)*XMP(I,3))
        XMP(I,1)=XMP(I,1)*AA(KSC1+I)
        XMP(I,2)=XMP(I,2)*AA(KSC1+I)
        XMP(I,3)=XMP(I,3)*AA(KSC1+I)
  630   CONTINUE

      ENDIF

  635 CONTINUE

      ELSE

!        print *,'almean: call to rotxtr'
      IF (LINTAX) THEN
         CALL GETWCS(NM, AA(KDPOR), COSTHG, SINTHG,1)
         THETAG(1:NM) = ATAN2(SINTHG(1:NM), COSTHG(1:NM))
      ELSE
      CALL ROTXTR(MJDSBL,FSECN,.TRUE.,AA(KEQN),AA(KSRTCH),              &
     &   THETAG,COSTHG,SINTHG,NM,AA,AA(KACOSW),AA(KBSINW),AA(KANGWT),   &
     &   AA(KWT),AA(KACOFW),AA(KBCOFW),II,1)
      END IF
      IF (JOR == 3) THEN
         THETAG(1:NM) = THETAG(1:NM) + DEGRAD*DXGEO2
         COSTHG(1:NM) = COS(THETAG(1:NM))
         SINTHG(1:NM) = SIN(THETAG(1:NM))
      END IF
      DO 640 I=1,NM
      XMP(I,1)= COSTHG(I)*XTD(I,1)+SINTHG(I)*XTD(I,2)
      XMP(I,2)=-SINTHG(I)*XTD(I,1)+COSTHG(I)*XTD(I,2)
      XMP(I,3)=XTD(I,3)
  640 END DO

      ENDIF

!
      IF(LSKIPP) RETURN
!
      KSC1=KSRTCH
      KSC2=KSC1+MINTIM
      KSC3=KSC2+MINTIM
      KSC4=KSC3+MINTIM
      DO 650 I=1,NM
      AA(KSC4+I-1)=0.D0
  650 END DO
!        print *,'almean: call to subtrk'
      CALL SUBTRK(XMP,AA(KSC4),AA(KSC1),AA(KSC2),AA(KSC3),SATLAT,       &
     &            SATLON,SATH,NM)
      RETURN
      END
