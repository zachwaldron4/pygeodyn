!$ALDBSF
      SUBROUTINE ALDBSF(AA,II,LL,FSECM,XBTOR,XTRA,XPOLE,YPOLE,          &
     &                  THETAG,COSTHG,SINTHG,SURLAT,SURLON,SURH,        &
     &                  OTIDES,GEOIDS,ETIDES,XEPTID,SBMDIF,ITER,        &
     &                  LFULL,UZ,UZSQ,TEMP,SSTS,                        &
     &                  PMPA,LOAD,RLAND,JOR,LPW,SCALE)
!********1*********2*********3*********4*********5*********6*********7**
! ALDBSF           00/03/19            9912.0    PGMR - S.B. LUTHCKE
!
! FUNCTION: CONVERT TOR BOUNCE POINT GUESS TO ECF MEAN POLE,
!           COMPUTE ECF MEAN POLE VECTOR TO SURFACE AT BOUNCE POINT
!           GUESS, MODIFY ECF SURFACE VECTOR TO INCLUDE TIME VARYING
!           EFFECTS (e.g. OTIDES, ETIDES, POLTIDE ....).
!           RETURN THE MAGNITUDE AND PROPER SIGN OF THE DIFFERENCE
!           BETWEEN THE SURFACE AND THE BOUNCE POINT GUESS.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   FSECM    I    A    BOUNCE TIME
!   XBTOR    I    A    BOUNCE TOR VECTOR
!   XTRA     I    A    SCRATCH SPACE
!   XPOLE   I/O   A    X OF POLE
!   YPOLE   I/O   A    Y OF POLE
!   THETAG  I/O   A    RIGHT ACSENSION OF GREENWICH
!   COSTHG  I/O   A    COSINE OF THETAG
!   SINTHG  I/O   A    SINE OF THETAG
!   SURLAT   O    A    GEODETIC LATITUDE OF SURFACE (RAD.)
!   SURLON   O    A    EAST LONGITUDE OF SURFACE (RAD.)
!   SURH     O    A    ELLIPSOID HT. OF SURFACE
!   OTIDES   O    A    OCEAN TIDE HT.
!   GEOIDS   O    A    SURFACE HT.
!   ETIDES   O    A    EARTH TIDE HT.
!   XEPTID   O    A    EARTH TIDE VECTOR CORRECTION
!   SBMDIF   O    A    MAGNITUDE OF THE DIFFERENCE BETWEEN THE SURFACE
!                      ECF VECTOR AND THE BOUNCE GUESS ECF VECTOR.  THE
!                      SIGN CONVENTION IS INCLUDED
!   ITER     I    S    ITERATION NUMBER FOR BOUNCE POINT FINDING ITERATI
!   LFULL    I    S    IF = TRUE THEN COMPUTE ALL CORRECTIONS
!                      IF = FALSE ASSUMED TO HAVE BEEN CALLED BEFORE AND
!                      MUCH OF THE DATA IS ALREADY LOADED SO DO NOT
!                      RECOMPUTE AND RELOAD.  THIS TAKES CARE OF REDUNDA
!                      COMPUTATIONS FROM 3 CALLS TO ALDST2 IN DIRALT.
!   RLAND    A      I  LAND OCEAN FLAG (0=>DON'T KNOW ; 1=>OCEAN ;
!                      2=>LAND)
!
! COMMENTS:
!
! FOR THE TIME BEING, ALTIMETRY WILL NOT BE USED TO SOLVE FOR POLAR
! MOTION
!
!
! THE USE OF XTRA:
!
! XTRA(*,1-3)              : TOD BOUNCE VECTOR
! 1st USE OF XTRA(*,4-6)   : ECF TRUE POLE BOUNCE VECTOR
! XTRA(*,7-9)              : ECF MEAN POLE BOUNCE VECTOR
! 2nd USE OF XTRA(*,4-6)   : ECF MEAN POLE CORREC FOR BODY + POLE TIDE
! XTRA(*,10-26)            : SCRTCH SPACE FOR SOLIDT
! 3rd USE OF XTRA(*,4-6)   : ECF MEAN POLE VECTOR TO SURFACE
!
!
!********1*********2*********3*********4*********5*********6*********7**

      USE MSSGFC_MOD
      USE DEMinterpolate, only : interpolate_DEMgrids

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      PARAMETER( LOLA = .FALSE.)
      PARAMETER( ZERO = 0.0D0)
      parameter( ms = 3 )

      COMMON/AXIS/LINTAX
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
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
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CORL02/KLNDTT,KLWSTR,KLNDTR,KLEVMF,KLTPMS,NXCL02
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/DPLNOR/DXGEO1,DXGEO2,DXFRC1,DXFRC2,XPLNOR
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LNDTRP/BSLAT(10000),BSLON(10000),BSHT(10000),BSXYX(10000,3)
      COMMON/LOLOAD/LOLMD,LOLAJ,L2FOLT,LAPLOD
      COMMON/MULTDM/MLTDEM,MEMDEM,NXMDEM
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
      COMMON/OLOADA/LXYZCO,LEOPTO
      COMMON/OLDADJ/NPV0OL(3,3),JPV0OL(3,3),NYZ0OL(3,3),JYZ0OL(3,3),    &
     &              NEO0OL(3,3),JEO0OL(3,3)
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/POLESW/LDNPOL,NXPOLS
      COMMON/SSTCOM/NCBAR, NSBAR ,NCDOT ,NSDOT ,NCAPRD,NCBPRD,NSAPRD,   &
     &              NSBPRD,NCBARA,NSBARA,NCDOTA,NSDOTA,NCAPDA,NCBPDA,   &
     &              NSAPDA,NSBPDA,ISSTEP,NXSST
      COMMON/TIDEOP/EOPTID(2,3)

      DIMENSION AA(1),II(1),LL(1),FSECM(NM)
      DIMENSION XBTOR(MINTIM,3),XTRA(MINTIM,33)
      DIMENSION XPOLE(MINTIM),YPOLE(MINTIM)
      DIMENSION COSTHG(NM),SINTHG(NM),THETAG(NM)
      DIMENSION SURLAT(NM),SURLON(NM),SURH(NM)
      DIMENSION OTIDES(NM),GEOIDS(NM),ETIDES(NM)
      DIMENSION XEPTID(MINTIM,3),SBMDIF(NM)
      DIMENSION UZ(NM),UZSQ(NM),TEMP(NM)
      DIMENSION SSTS(NM),PMPA(NDIM1,NDIM2)
      DIMENSION RLAND(NM)
      DIMENSION XENV(3,3)
      DIMENSION DUM(3),DUM1(3,2,1)
      DATA LSIM/.FALSE./

!**********************************************************************
! START OF EXECUTABLE CODE
!**********************************************************************
      DO I=1,NM
        RLAND(I)=1.D0
      ENDDO
      DO I=1,6
        EOPTID(I,1)=0.D0
      ENDDO

!       letid2=.FALSE.
!       lxyzco=.FALSE.
!       leopto=.FALSE.
!      print *,''
!      print *,'aldbsf: ms no. dbg: ',ms
!      print *,''
!      print *,'aldbsf: start of routine, iter,lfull:',iter,lfull
!      print *,'aldbsf: leopto,lxyzco',leopto,lxyzco
!      print *,'aldbsf: lmsswg,lotrm2,letid2',lmsswg,lotrm2,letid2
!
! SET QUANTITIES and POINTERS
!
!!!      SCALE1=0.D0
!!!      IF(NPVAL0(IXFGGM).GE.7) THEN
!!!       SCALE=AA(KPRMVC-1+IPVAL0(IXFGGM)+6)
!!!       SCALE1=SCALE-1.D0
!!!      ENDIF
      KSC1=KSRTCH
      KSC2=KSC1+MINTIM
      KSC3=KSC2+MINTIM
      KSC4=KSC3+MINTIM
      KSC5=KSC4+MINTIM
      KSC6=KSC5+MINTIM
      KSC7=KSC6+MINTIM
      KSC8=KSC7+MINTIM
      KSC9=KSC8+MINTIM
!     KSCA=KSC9+2*MINTIM (KSC9 IS 2*MINTIM)

      LMLTDEM=.FALSE.
      IF(MLTDEM.EQ.1) LMLTDEM=.TRUE.
!
! COMPUTE TOD BOUNCE COORDS AND STORE
!
      IF (JOR == 3) THEN
          CALL SATUPD(MJDSBL,FSECM,XBTOR,XTRA(1,1),AA,NM,MINTIM,        &
     &                .TRUE.,.FALSE.,II,0,1)
      ELSE
          CALL SATUPD(MJDSBL,FSECM,XBTOR,XTRA(1,1),AA,NM,MINTIM,        &
     &                .TRUE.,.FALSE.,II,JOR,1)
      ENDIF
!
! COMPUTE MEAN POLE ECF COORDINATES FOR THE BOUNCE POINT
! ...INCLUDE TIME VARYING SURFACE EFFECTS
!

!      earth or planet 1:
       IF(ICBDGM.EQ.3) THEN
!
! ...COMPUTE PARAMETERS FOR CENTER OF MASS AND TIDAL EOP PHENOMENA
! .... OCEAN LOADING USED AS AN ALIAS
! .... OCEAN LOADING NOT COMPUTED AS A SITE FOR THE DIR.ALT. BOUNCE PTS.
!
!     iter 1:
      IF(ITER.EQ.1.AND.LFULL) THEN

      IF(LEOPTO.OR.LXYZCO) THEN
!      print *,'aldbsf: call to oldset'
        CALL OLDSET(1,-1,NM,AA(KSTAIN),II(KIP0OL),II(KNDOLA),           &
     &              II(KKIOLA),LOLA,LOLMDS,                             &
     &              JABCOF,JPXSPA,JOLPAE,JOLPAN,JOLPAV,JXLOCV,          &
     &              JABCCM,JPXSP1,JOLPXC,JOLPYC,JOLPZC,                 &
     &              JABCEO,JPXSP2,JOLPXP,JOLPYP,JOLPUP,LALOAD,ISITA,    &
     &              XENV)
      ENDIF
      IF(LEOPTO.OR.LOLMDS.OR.LXYZCO) THEN
         CALL OLTIME(MJDSBL,FSECM,NM,AA(KANGFC),AA(KSPEED),AA(KSCROL),  &
     &               AA(KSCROL+4*NTOLFR),II(KPTOLS),AA(KANGT),          &
     &               AA(KCONAM),AA(KTWRK1),AA(KGRDAN),AA(KUANG),        &
     &               AA(KFAMP),II(KJDN))

      ENDIF

      IF(LEOPTO) THEN
!      print *,'aldbsf: call to oldeop'
         CALL OLDEOP(MJDSBL,FSECM,NM,AA(JABCOF),AA(KANGFC),AA(KSPEED),  &
     &              AA(JXLOCV),AA(KSC1),AA(KSCROL),AA(KROTMT),LOLA,     &
     &              AA(JPXSPA),NPV0OL(1,1),NPV0OL(2,1),NPV0OL(3,1),     &
     &              II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),        &
     &              AA(JPXSP1),NYZ0OL(1,1),NYZ0OL(2,1),NYZ0OL(3,1),     &
     &              II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),        &
     &              AA(JPXSP2),NEO0OL(1,1),NEO0OL(2,1),NEO0OL(3,1),     &
     &              II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,     &
     &              LEOPTO,AA(KSC1))
      ENDIF

!
! ... OBTAIN POLAR MOTION ROTATION MATRICES FROM WHICH TO
! ... INTERPOLATE ACROSS THE ENTIRE BLOCK OF DATA
!
!      print *,'aldbsf: call to inpolp'
      CALL INPOLP(AA(KA1UT),AA(KDPSR),AA(KXPUT),AA(KYPUT),.TRUE.,LDNPOL,&
     &   LOLA,MJDSBL,FSECM,NM,AA(KSC1),II(KINDPI),NINTPL,II(KNMP),      &
     &   II(KINDP),AA(KPXPOL),AA(KRPOLE),AA(KS0),AA(KXDPOL),AA(KXDOTP))

!
! ... INTERPOLATE FOR POLAR MOTION
!
      NINTP2=NINTPL*2
!      print *,'aldbsf: call to getpol'
      CALL GETPOL(AA(KS0),AA(KS1),II(KNMP),AA(KRPOLE),NM,               &
     &            NINTPL,NINTP2,XPOLE,YPOLE)

!
! ...COMPUTE RIGHT ASCENSION OF GREENWICH PARAMETERS
!
!      print *,'aldbsf: call to grhrap'
      CALL GRHRAP(MJDSBL,FSECM,.TRUE.,.TRUE. ,AA(KEQN),AA(KSRTCH),      &
     &            THETAG,COSTHG,SINTHG,NM,AA,II,AA(KXUTDT),.FALSE.,DUM, &
     &            DUM1)

      ENDIF  ! iter 1

!
! ...COMPUTE ECF True Pole COORDINATES OF BOUNCE POINT GUESS
!
      DO I=1,NM
       XTRA(I,4)= XTRA(I,1)*COSTHG(I)+XTRA(I,2)*SINTHG(I)
       XTRA(I,5)=-XTRA(I,1)*SINTHG(I)+XTRA(I,2)*COSTHG(I)
       XTRA(I,6)= XTRA(I,3)
! .....SET THETAG BACK TO ZERO FOR SUBTRK and ALTRFC
       THETAG(I)=ZERO
      ENDDO

!
! ...COMPUTE ECF Mean Pole COORDINATES OF BOUNCE POINT GUESS
!
      DO I=1,NM
       XTRA(I,7)=XTRA(I,4)+XTRA(I,6)*XPOLE(I)
       XTRA(I,8)=XTRA(I,5)-XTRA(I,6)*YPOLE(I)
       XTRA(I,9)=XTRA(I,6)-XTRA(I,4)*XPOLE(I)+XTRA(I,5)*YPOLE(I)
      ENDDO

!
! ...APPLY CENTER OF MASS PHENOMENA
! .... OCEAN LOADING USED AS AN ALIAS
! .... OCEAN LOADING NOT COMPUTED AS A SITE FOR THE DIR.ALT. BOUNCE PTS.
!
      IF(LXYZCO) THEN
!      print *,'aldbsf: call to oload '
      CALL OLOAD(MJDSBL,FSECM,NM,AA(JABCOF),AA(KANGFC),AA(KSPEED),      &
     &           AA(JXLOCV),XTRA(1,7),AA(KSCROL),AA(KROTMT),LOLA,       &
     &           AA(JPXSPA),NPV0OL(1,1),NPV0OL(2,1),NPV0OL(3,1),        &
     &           II(JOLPAE),II(JOLPAN),II(JOLPAV),AA(JABCCM),           &
     &           AA(JPXSP1),NYZ0OL(1,1),NYZ0OL(2,1),NYZ0OL(3,1),        &
     &           II(JOLPXC),II(JOLPYC),II(JOLPZC),AA(JABCEO),           &
     &           AA(JPXSP2),NEO0OL(1,1),NEO0OL(2,1),NEO0OL(3,1),        &
     &           II(JOLPXP),II(JOLPYP),II(JOLPUP),LOLMDS,LXYZCO,LEOPTO, &
     &           0,.TRUE.,MINTIM,LALOAD,AA(KALCOF),II(KSITE),ISITA,     &
     &           XENV)
      ENDIF

!
! ...COMPUTE GEODETIC LAT, LON, HT. FOR BOUNCE POINT GUESS
!
      CALL SUBTRK(XTRA(1,7),THETAG,AA(KSC1),AA(KSC2),AA(KSC3),          &
     &            SURLAT,SURLON,SURH,NM)
!  CONVERT SOME QUANTITIES TO GEOCENTRIC (USE TEMP AS SCRATCH ARRAY )
      CALL GDSGCE(AA(KSC2),AA(KSC1),UZ,UZSQ,TEMP,NM)

!      print *,'aldbsf ms: surlat, surlon: ',surlat(ms),surlon(ms)
!      print *,'aldbsf ms: surh: ',surh(ms)

      ELSE     !  earth or planet 1
!
! ...COMPUTE Planet CF COORDINATES OF BOUNCE POINT GUESS
!

      IF (ITER.EQ.1.AND.LFULL) THEN
!      print *,'aldbsf: call to rotxtr'
       IF (LINTAX) THEN
         CALL GETWCS(NM, AA(KDPOR), COSTHG, SINTHG,1)
         THETAG(1:NM) = ATAN2(SINTHG(1:NM), COSTHG(1:NM))
       ELSE
         CALL ROTXTR(MJDSBL,FSECM,.TRUE.,AA(KEQN),AA(KSRTCH),           &
     &     THETAG,COSTHG,SINTHG,NM,AA,AA(KACOSW),AA(KBSINW),            &
     &     AA(KANGWT),AA(KWT),AA(KACOFW),AA(KBCOFW),II,1)
       END IF
       IF (JOR == 3) THEN
         THETAG(1:NM) = THETAG(1:NM) + DEGRAD*DXGEO2
         COSTHG(1:NM) = COS(THETAG(1:NM))
         SINTHG(1:NM) = SIN(THETAG(1:NM))
       END IF
      ENDIF

      DO I=1,NM
       XTRA(I,7)= COSTHG(I)*XTRA(I,1)+SINTHG(I)*XTRA(I,2)
       XTRA(I,8)=-SINTHG(I)*XTRA(I,1)+COSTHG(I)*XTRA(I,2)
       XTRA(I,9)= XTRA(I,3)
! .....SET THETAG BACK TO ZERO FOR SUBTRK and ALTRFC
       THETAG(I)=ZERO
      ENDDO
!
! ...COMPUTE GEODETIC LAT, LON, HT. FOR BOUNCE POINT GUESS
!
      CALL SUBTRK(XTRA(1,7),THETAG,AA(KSC1),AA(KSC2),AA(KSC3),          &
     &            SURLAT,SURLON,SURH,NM)
!  CONVERT SOME QUANTITIES TO GEOCENTRIC (USE TEMP AS SCRATCH ARRAY )
      CALL GDSGCE(AA(KSC2),AA(KSC1),UZ,UZSQ,TEMP,NM)


      ENDIF  ! earth or planet 1

!
! ...COMPUTE GEOCENTRIC LAT, LON, FOR BOUNCE POINT GUESS
!

       DO I=1,NM
       X2=XTRA(I,7)*XTRA(I,7)
       Y2=XTRA(I,8)*XTRA(I,8)
       Z2=XTRA(I,9)*XTRA(I,9)
       RADIUS=SQRT(X2+Y2+Z2)
       XY=SQRT(X2+Y2)
! COSINE OF GEOCENTRIC LATITUDE
       UZ(I)=XY/RADIUS
! INE OF GEOCENTRIC LATITUDE
       UZSQ(I)=XTRA(I,9)/RADIUS
       RLAM=ATAN2(XTRA(I,8),XTRA(I,7))
! COSINE OF EST LONGITUDE
       AA(KSC3+I-1)=COS(RLAM)
! SINE OF EST LONGITUDE
       AA(KSC4+I-1)=SIN(RLAM)
       ENDDO

!      print *,'aldbsf ms: surlat, surlon: ',surlat(ms),surlon(ms)
!      print *,'aldbsf ms: surh: ',surh(ms)

!
! COMPUTE SURFACE HT. AT LAT. and LON.
!
!     earth or planet 2:
      IF(ICBDGM.EQ.3) THEN

!...COMPUTE SURFACE HT. FOR OCEAN DATA
!...SURFACE HT. WILL INCLUDE BOTH MSS and OTIDES
!
!...COMPUTE GOT99 OCEAN TIDES
!
      IF(LOTRM2.AND.ITER.EQ.2.AND.LFULL) THEN
!      print *,'aldbsf: evaluate ocean tides'

       CALL UTCET(.FALSE.,NM,MJDSBL,FSECM,XTRA(1,30),AA(KA1UT))

       DO N=1,NM
       IF(RLAND(N).GT..99D0.AND.RLAND(N).LT.1.99D0) THEN
       

        TIME=DBLE(MJDSBL)+XTRA(N,30)

        RMJD=(TIME/86400.0D0)+TMGDN2

!       if(n.eq.ms) then
!         print *,'aldbsf: ms,rmjd: ',ms,rmjd
!       endif
        CALL PERTH2(SURLAT(N),SURLON(N),                                &
     &              RMJD,TIDE,LSDATA,.FALSE.)
      ELSE
        LSDATA=.FALSE.
      ENDIF

        IF(LSDATA) THEN
         CALL LPEQMT(RMJD*86400.0D0,SURLAT(N),TIDEL)
         OTIDES(N)=(TIDE+TIDEL)/100.0D0
        ELSE
         OTIDES(N)=-999999.9D0
        ENDIF
       END DO

      ENDIF
!       write(6,*)' dbg GEOIDS ',GEOIDS(1)

 1000 CONTINUE

!
!...COMPUTE MSS
!
      IF(LMSS) THEN
      JJ=1

!       print *,'aldbsf: evaluate surface'

       DO N=1,NM

       IF(INT(OTIDES(N)).NE.-999999.AND.RLAND(N).GT..99D0               &
     &    .AND.RLAND(N).LT.1.01D0) THEN
       CALL GINTRP(4,5.D0,GRID1,XLATS(JJ),XLONW(JJ),DPHI(JJ),DPHI(JJ),  &
     &      NNL(JJ),NEW(JJ),NNL(JJ),NEW(JJ),SURLAT(N),SURLON(N),        &
     &      GEOIDS(N))
       ENDIF

       END DO

      ENDIF

!
!...COMPUTE DEM

      IF(LMLTDEM) THEN

       DO N=1,NM
         GEOIDS(N)=interpolate_DEMgrids(XTRA(N,14),XTRA(N,13))
       ENDDO

      ELSE IF(LDEM) THEN
       JJ=2
!
       DO N=1,NM
       IF(RLAND(N).GT..99D0.AND.RLAND(N).LT.1.01D0) THEN
       CALL GINTRP(4,5.D0,GRID2,XLATS(JJ),XLONW(JJ),DPHI(JJ),DPHI(JJ),  &
     &      NNL(JJ),NEW(JJ),NNL(JJ),NEW(JJ),SURLAT(N),SURLON(N),        &
     &      GEOIDS(N))
       ENDIF
       END DO

      ENDIF
!
!...PUT LAT AND LON BACK TO RADIANS FOR BELOW COMPUTATIONS
!
      DO N=1,NM
       SURLAT(N)=SURLAT(N)*DEGRAD
       SURLON(N)=SURLON(N)*DEGRAD
      ENDDO

!
! ....COMPUTE EARTH TIDE and POLE TIDE CONTRIBUTION TO SURFACE VECTOR
! ....OTHERWISE ETIDES COME FROM DATA
!
!     earth tides:
      IF(LETID2.AND.ITER.EQ.2.AND.LFULL) THEN

!      print *,'aldbsf: evaluate etides'

       DO N=1,NM
!... IN SOLIDT THE FIRST TWO ELEMENTS OF SCRTCH ARE EXPECTED
!....TO BE THE X and Y OF THE POLE FOR POLTID COMPUTATION
        XTRA(N,10) = XPOLE(N)
        XTRA(N,11) = YPOLE(N)
        AA(KSC9+N-1)=XPOLE(N)
        AA(KSC9+NM+N-1)=YPOLE(N)
        AA(KSC1+N-1)=COS(SURLAT(N))
        AA(KSC2+N-1)=SIN(SURLAT(N))
        AA(KSC3+N-1)=COS(SURLON(N))
        AA(KSC4+N-1)=SIN(SURLON(N))
       ENDDO

!... THE BODY TIDE RADIAL AND TRANSVERSE COMPONENTS ARE APPLIED

       CALL SOLIDT(XTRA(1,7),MJDSBL,FSECM,COSTHG,SINTHG,NM,AA(KSC9),    &
     &             XTRA(1,4),AA(KPXSLV),AA(KSC1),AA(KSC2),AA(KSC3),     &
     &             AA(KSC4),MINTIM,.FALSE.,LOLA,.FALSE.,XEPTID,         &
     &             AA(KDXTID),AA(KANFSD),AA(KSPDSD),AA(KPRMFS),         &
     &             AA(KSCRFR),AA(KCOSAS),AA(KSINAS),AA,II)
!.....LOAD ETIDES WITH THE RADIAL PART OF THE CORRECTION.  THE
!.....HORIZONTAL PART OF SMALL ENOUGH THAT WE CAN IGNORE IT HERE.
       DO N=1,NM
        XEPTID(N,1)=XTRA(N,4)-XTRA(N,7)
        XEPTID(N,2)=XTRA(N,5)-XTRA(N,8)
        XEPTID(N,3)=XTRA(N,6)-XTRA(N,9)
        BMAG=SQRT(XTRA(N,7)**2+XTRA(N,8)**2+XTRA(N,9)**2)
        TMAG=SQRT(XTRA(N,4)**2+XTRA(N,5)**2+XTRA(N,6)**2)
        DIFMAG=TMAG-BMAG
        ETIDES(N)=DIFMAG
       ENDDO

!       dmgms=sqrt(xeptid(ms,1)**2+xeptid(ms,2)**2+xeptid(ms,3)**2)
!       print *,'aldbsf: dmgms: ',dmgms
!       print *,'aldbsf: etides: ',etides(ms)

       ENDIF !  earth tides

!... TOTAL SURFACE HEIGHT
       DO N=1,NM
        SURH(N)= GEOIDS(N) + OTIDES(N) +  ETIDES(N)
       ENDDO

!      print *,'aldbsf ms: surlat, surlon: ',surlat(ms),surlon(ms)
!      print *,'aldbsf ms: surh: ',surh(ms)
!      print *,'aldbsf ms: otides: ',otides(ms)
!      print *,'aldbsf ms: etides: ',etides(ms)
!      print *,'aldbsf ms: geoids: ',geoids(ms)

      CALL XYZGEO(SURLAT,SURLON,SURH,NM,AA(KSC1),                       &
     &            XTRA(1,4),XTRA(1,5),XTRA(1,6))

      ELSE !  earth or planet 2

! future site of planetary topography
!
! COMPUTE PLANETARY TOPOGRAPHY USING THE SSCOEF OPTION
!
! DEP CALL SST ALWAYS
!     IF(.NOT.LSSTMD.AND..NOT.LSSTAJ) GO TO 2000
!     IF(LFULL.AND.ITER.EQ.1) THEN
!     IF(LFULL) THEN
!

          JCCOS=KSSTWT
          JCSIN=JCCOS+NCAPRD
          JSCOS=JCSIN+NCBPRD
          JSSIN=JSCOS+NSAPRD
!      write(6,*)' dbg calling STT '
      CALL SST(NM,UZ,UZSQ,AA(KSC3),AA(KSC4),                            &
     &    AA(KSSTCC),AA(KSSTSS),AA(KPN),AA(KCOSLM),                     &
     &    AA(KSINLM),AA(KXPRFL),SSTS,PMPA,LNPNM,NDIM1,NDIM2,            &
     &    AA(KWRK),II(KPTRUA),II(KPTRAU),AA,II,MJDSBL,FSECM,II(KSSTNA), &
     &    AA(JCCOS),AA(JCSIN),AA(JSCOS),AA(JSSIN),XTRA(1,31),LOAD,      &
     &    LL(KLAVOI),.FALSE.)
!
!         write(6,*)' dbg SSTS 1   ',SSTS(1)
!
!     ENDIF
!
 2000 CONTINUE

      DO N=1,NM
      XTRA(N,4)= SSTS(N)*UZ(N)*AA(KSC3+N-1)
      XTRA(N,5)= SSTS(N)*UZ(N)*AA(KSC4+N-1)
      XTRA(N,6)= SSTS(N)*UZSQ(N)
      ENDDO
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
2001  CONTINUE
!
!...COMPUTE DEM
!
      IF(LMLTDEM) THEN

        DO N=1,NM
          GEOIDS(N)=interpolate_DEMgrids(SURLON(N),SURLAT(N))
          SCALE1=SCALE-1.D0
          GEOIDS(N)=GEOIDS(N)+SCALE1*GEOIDS(N)
          GEOIDS(N)=GEOIDS(N)-AE
        ENDDO

      ELSE IF(LDEM) THEN
       JJ=2

       DO N=1,NM
!      IF(RLAND(N).GT..99D0.AND.RLAND(N).LT.1.01D0) THEN
       CALL GINTRP(4,5.D0,GRID2,XLATS(JJ),XLONW(JJ),DPHI(JJ),DPHI(JJ),  &
     &      NNL(JJ),NEW(JJ),NNL(JJ),NEW(JJ),SURLAT(N),SURLON(N),        &
     &      GEOIDS(N))
       SCALE1=SCALE-1.D0
       GEOIDS(N)=GEOIDS(N)+SCALE1*GEOIDS(N)
! FOR THE PARTICULAR CASE OF RQ36
!!!!       GEOIDS(N)=GEOIDS(N)+SCALE1*400.D0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           GEOIDS(N)=GEOIDS(N)-AE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      ENDIF
       END DO

      ENDIF
!
!...PUT LAT AND LON BACK TO RADIANS FOR BELOW COMPUTATIONS
!
!     WRITE(6,55886) SURLAT(1),SURLON(1),GEOIDS(1)
55886 FORMAT(' AF GRD EVAL LT,LN HT ',3F20.4)
      DO N=1,NM
       IF(LSIM) THEN
         BSLAT(N)=SURLAT(N)
         BSLON(N)=SURLON(N)
         BSHT(N)=GEOIDS(N)
       ENDIF
       SURLAT(N)=SURLAT(N)*DEGRAD
       SURLON(N)=SURLON(N)*DEGRAD
      ENDDO
       DO N=1,NM
        SURH(N)= GEOIDS(N)
       ENDDO
      CALL XYZGEO(SURLAT,SURLON,SURH,NM,AA(KSC1),                       &
     &            XTRA(1,4),XTRA(1,5),XTRA(1,6))
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
      ENDIF  ! earth or planet 2

!
! COMPUTE E or Planet CF VECTOR AT LAT., LON. and HT.=SURFACE HT.
!
!
! COMPUTE MAGNITUDE and SIGN OF DIFF. BETWEEN SURFACE VECTOR and
! BOUNCE POINT GUESS - STORE IN SBMDIF
!
!     write(6,*)' Surface Vector (1) ',XTRA(1,4),XTRA(1,5),XTRA(1,6)
!     write(6,*)' Bounce Point (1)  ',XTRA(1,7),XTRA(1,8),XTRA(1,9)
      DO N=1,NM
       IF(LSIM) THEN
         BSXYX(N,1)=XTRA(N,4)
         BSXYX(N,2)=XTRA(N,5)
         BSXYX(N,3)=XTRA(N,6)
         BSXYX(N,1)=XTRA(N,7)
         BSXYX(N,2)=XTRA(N,8)
         BSXYX(N,3)=XTRA(N,9)
       ENDIF
       BMAG=SQRT(XTRA(N,7)**2+XTRA(N,8)**2+XTRA(N,9)**2)
       SMAG=SQRT(XTRA(N,4)**2+XTRA(N,5)**2+XTRA(N,6)**2)
       DIFMAG=SMAG-BMAG
       TMPDIF=SQRT( (XTRA(N,4)-XTRA(N,7))**2 +                          &
     &              (XTRA(N,5)-XTRA(N,8))**2 +                          &
     &              (XTRA(N,6)-XTRA(N,9))**2 )
       SBMDIF(N)=SIGN(TMPDIF,DIFMAG)

!       if(n.eq.ms) then
!         print *,'aldbsf: ms difmag, tmpdif: ',ms,difmag,tmpdif
!         print *,'aldbsf: ms sbmdif: ',sbmdif(ms)
!       endif

      ENDDO

!      print *,'aldbsf: sbmdif ms: ',sbmdif(ms)

      RETURN
      END
