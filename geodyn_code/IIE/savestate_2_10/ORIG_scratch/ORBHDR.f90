!$ORBHDR
      SUBROUTINE ORBHDR(NAME,SXYZ,XLOCV,STAINF,AA,II,LL,UTDT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      REAL  EBUF
!
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/ARCPR /FSECRF,XARCPR
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
      COMMON/CELEMX/TRUE,ECC
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
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
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
!>>>>
      COMMON/CORBIN/MJDSNF,NENTRY
      COMMON/CORBNA/FNFSTR,FNFSTP,FNFRT,XCORN
      COMMON/CORBNF/MAXINF,NORBNF,INFSTA(15),INFNDX(15),INFSTR,INFSTP,  &
     &              INFRT ,NXCORB
      COMMON/CORBUF/ORBBUF(50,7)
!<<<<
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
! /CORI04/ DYNAMIC ARRAY POINTERS FOR MEASUREMENT PROCESSING ARRAYS
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
!
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
!
!>>>>>>>>
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!<<<<<<<<<
!
      DIMENSION BIHMAT(9), FSECTM(1)
      DIMENSION NAME(2,1),SXYZ(3,1),XLOCV(3,3,1),STAINF(16,1)
      DIMENSION IBUF(50,14),EBUF(50,14)
      DIMENSION FSECX(4),MJDSX(4),FSECUT(4),IYMDX(4),IHMX(4),SECX(4)
      DIMENSION IDAY(4),AEI(6),AA(1),II(1),LL(1)
      DIMENSION UTDT(1,1)
      DIMENSION DUM(3),DUM2(3,2,1)
      dimension pkpx(6,6)
      EQUIVALENCE (ORBBUF(1,1),IBUF(1,1),EBUF(1,1))
      DATA C20/-1.0826270D-3/,C30/ 2.5364140D-6/,                       &
     &     C40/ 1.6233497D-6/,C50/ 2.2608567D-7/
      DATA J1/1/,J2/2/,J3/3/
!C      DATA IUNT30/31/    ! original
!     ....set unit to 29 to avoid conflict with ORBFIL which uses
!     ....units 30 and up  -- jjm 12/5/95
      DATA IUNT30/29/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!     ....this subroutine works only for one satellite
!
!     OLD VERSION: NEXT 2 LINES
!     OPEN(UNIT=IUNT30,FILE='ORBINF.DAT',STATUS='NEW',
!    1   FORM='UNFORMATTED')
!
!
!     NEW VERSION:
!     AUTHOR: W.WERNER, 17.7.89
!     ZEUGEN: O.MONTANBRUCK, E.GILL
!
!
!ccc  OPEN(UNIT=IUNT30,STATUS='NEW',
!ccc 1   FORM='UNFORMATTED')
!
!     END OF CHANGES 17.7.89
!
!
!     write(6,*) 'orbhdr:  stainf(1,1) ', stainf(1,1)
      NENTRY=0
      MJDSNF=INFSTR
      CALL CLEARA(ORBBUF,350)
      ORBBUF(1,1)=76796291.0D0
      ORBBUF(2,1)=II(KISATN)
      FSECX(1)=FNFSTR
      FSECX(2)=FNFSTP
      FSECX(3)=FSECRF
      FSECX(4)=AA(KFSEC0)
      MJDSX(1)=INFSTR
      MJDSX(2)=INFSTP
      MJDSX(3)=MJDSRF
      MJDSX(4)=II(KMJDS0)
!     write(6,*) 'orbhdr:  mjdsx ',
!    1                     mjdsx
!     write(6,*) 'orbhdr:  fsecx ',
!    1                     fsecx
!     write(6,*) 'orbhdr: ka1ut, aa(ka1ut)     ',
!    1                    ka1ut, aa(ka1ut)
      DO 100 I=1,4
!     write(6,*) 'orbhdr: i, mjdsx(i), fsecx(i) ',
!    1                    i, mjdsx(i), fsecx(i)
      CALL UTCET(.FALSE.,1,MJDSX(I),FSECX(I),FSECUT(I),AA(KA1UT))
      CALL YMDHMS(MJDSX(I),FSECUT(I),IYMDX(I),IHMX(I),SECX(I),1)
      IY=IYMDX(I)/10000
!     MJDSY=(IY-41)*36525*864 !! CHANGED BY TVM 22-FEB-85
!     MJDSY=((IY-41)*36525/100)*86400
!     MJDSY=(IY-41)*365+(IY-41)/4      ! CHANGED BY WW,GSOC 16-APR-87
      MJDSY=(IY-41)*365+(IY-41)/4
!     IDAY(I)=MJDSX(I)/86400-MJDSY+6   ! CHANGED BY WW,GSOC 16-APR-87
      IDAY(I)=MJDSX(I)/86400-MJDSY+6
!     MJDSY=((IY-41)*365+(IY-41)/4)*86400
!     IDAY(I)=(MJDSX(I)-MJDSY)/86400+6 !! CHANGED BY DG 26-NOV-84
!     IDAY(I)=(MJDSX(I)-MJDSY)/86400+7 || CHANGED BACK BY TVM 22-FEB-85
!     IDAY(I)=(MJDSX(I)-MJDSY)/86400+5 ! MODIFIED FEB 87 BY ACB
      ISEC=MOD(MJDSX(I),86400)
!     write(iout6,*) 'orbhdr: i, iymdx(i) ', i, iymdx(i)
!     write(iout6,*) 'orbhdr: i, ihmx(i) ', i, ihmx(i)
!     write(iout6,*) 'orbhdr: i, secx(i) ', i, secx(i)
      SECX(I)=SECX(I)+DBLE(IHMX(I)/100*3600+MOD(IHMX(I),100)*60)
!     write(iout6,*) 'orbhdr: i, secx(i) ', i, secx(i)
  100 END DO
      ORBBUF(4,1)=IYMDX(1)
      ORBBUF(5,1)=IDAY(1)
      ORBBUF(6,1)=INT(SECX(1))
      ORBBUF(7,1)=IYMDX(2)
      ORBBUF(8,1)=IDAY(2)
      ORBBUF(9,1)=INT(SECX(2))
      ORBBUF(10,1)=FNFRT
!
!     write(iout6,*) ' orbbuf(4-10,1) ', (orbbuf(jjj,1),jjj=4,10)
!
      ORBBUF(27,1)=IYMDX(3)
      ORBBUF(28,1)=IDAY(3)
!ccc  CALL GRHRAN(MJDSRF,FSECX(J3),.FALSE.,AA(KEQN),AA(KSRTCH),
      CALL GRHRAN(MJDSRF,FSECX(J3),.TRUE.,.TRUE.,                       &
     &   AA(KEQN),AA(KSRTCH),                                           &
     &   AA(KTHETG),AA(KCOSTH),AA(KSINTH),1,AA,II,UTDT,.FALSE.,DUM,     &
     &   DUM2)
      ORBBUF(29,1)=AA(KTHETG)
!     write(iout6,*) ' aa(kthetg)  ', aa(kthetg)
      CUL=AE
      CUL2=CUL*CUL
      CUL3=CUL*CUL2
      ORBBUF(37,1)=-1.5D0*C20*CUL2
      ORBBUF(38,1)=-2.5D0*C30*CUL3
      ORBBUF(39,1)=(3.75D0*C40*CUL2)*CUL2
      ORBBUF(40,1)=(C50*CUL3)*CUL2
      CUT=SQRT(AE**3/GM)
!     ORBBUF(80,1)=CD
!     ORBBUF(81,1)=ASAT*1.0D4
!     ORBBUF(82,1)=MSAT*1.0D3
! MODIFIED FEB 87 TO INSERT CORRECT CD,ASAT,MSAT
!      ORBBUF(80,1)=AA(KCD)
      ORBBUF(30,2)=AA(KCD)
!
!     ....AREA FOR DRAG
!      ORBBUF(81,1)=AA(KAREA+MSATA)*1.0D4
!      ORBBUF(81,1)=AA(KAREA      )*1.0D4   ! fix 12/6/95 jjm
                                           ! fix 12/6/95 jjm
      ORBBUF(31,2)=AA(KAREA      )*1.0D4
!
!     ....satellite mass in grams
!      ORBBUF(82,1)=AA(KXMASS)*1.0D3
      ORBBUF(32,2)=AA(KXMASS)*1.0D3
!
!     ....solar radiation coefficient (Cr)
!>>>  ORBBUF(83,1)=AA(KCR)
!     ....only good for one satellite
!      ORBBUF(83,1)=AA(KAPGM) / AA(KAPLM)
      ORBBUF(33,2)=AA(KAPGM) / AA(KAPLM)
!<<<
!
!     ....AREA FOR SOLRAD
!      ORBBUF(84,1)=AA(KAREA)*1.0D4
      ORBBUF(34,2)=AA(KAREA)*1.0D4
!
!      ORBBUF(91,1)=1.0D0
      ORBBUF(41,2)=1.0D0
!      ORBBUF(92,1)=1.0D0
      ORBBUF(42,2)=1.0D0
!      ORBBUF(101,1)=((II(KMJDS0)-MJDSRF)+(AA(KFSEC0)-FSECRF))/CUT
      ORBBUF(1,3)=((II(KMJDS0)-MJDSRF)+(AA(KFSEC0)-FSECRF))/CUT
      JX=KPRMV
      IF(LOBS) JX=KPRMVC
      JY=JX+1
      JZ=JX+2
      JXD=JX+3
      JYD=JX+4
      JZD=JX+5
!     ....convert Cartesian elements to Keplerian elements in rad (idrad
      CALL ELEM(AA(JX),AA(JY),AA(JZ),AA(JXD),AA(JYD),AA(JZD),AEI,       &
     &   1,pkpx,GM)
!     write(iout6,*) ' orbhdr: aei ', aei
!     ....satellite semi-major axis in canonical units of length (CUL)
!     ....at t0
!      ORBBUF(102,1)=AEI(1)/CUL
      ORBBUF(2,3)=AEI(1)/CUL
!     ....satellite orbit eccentricity at t0
!      ORBBUF(103,1)=AEI(2)
      ORBBUF(3,3)=AEI(2)
!     ....satellite true anomaly at t0
!      ORBBUF(104,1)=TRUE
      ORBBUF(4,3)=TRUE
!      INDX=105
      INDX=5
!      INDV=108
      INDV=8
      DO 200 I=1,3
!      ORBBUF(INDX,1)=AA(JX)/CUL
      ORBBUF(INDX,3)=AA(JX)/CUL
      INDX=INDX+1
      JX=JX+1
!      ORBBUF(INDV,1)=AA(JXD)*CUT/CUL
      ORBBUF(INDV,3)=AA(JXD)*CUT/CUL
      INDV=INDV+1
      JXD=JXD+1
  200 END DO
!      ORBBUF(111,1)=sqrt(ORBBUF(105,1)**2+ORBBUF(106,1)**2
!     1                   +ORBBUF(107,1)**2)
      ORBBUF(11,3)=SQRT(ORBBUF(5,3)**2+ORBBUF(6,3)**2                   &
     &                   +ORBBUF(7,3)**2)
!      ORBBUF(112,1)=sqrt(ORBBUF(108,1)**2+ORBBUF(109,1)**2
!     1                   +ORBBUF(110,1)**2)
      ORBBUF(12,3)=SQRT(ORBBUF(8,3)**2+ORBBUF(9,3)**2                   &
     &                   +ORBBUF(10,3)**2)
!     ....satellite mean anomaly in degrees
!      ORBBUF(114,1)=AEI(6) / degrad
      ORBBUF(14,3)=AEI(6) / degrad
!     ....satellite argument of perigee in degrees
!      ORBBUF(116,1)=AEI(5) / degrad
      ORBBUF(16,3)=AEI(5) / degrad
!     ....satellite inclination in degrees
!      ORBBUF(117,1)=AEI(3) / degrad
      ORBBUF(17,3)=AEI(3) / degrad
!     ....satellite right ascension of ascending node in degrees
!      ORBBUF(118,1)=AEI(4) / degrad
      ORBBUF(18,3)=AEI(4) / degrad
      DMNMOT=SQRT(GM/AEI(1)**3)
!     ....satellite mean motion at epoch in rad/sec
!      ORBBUF(120,1)=DMNMOT
      ORBBUF(20,3)=DMNMOT
!     ....satellite eccentric anomaly at epoch in degrees
!      ORBBUF(121,1)=ECC
      ORBBUF(21,3)=ECC
!
      SPSISQ = SIN( AEI(3) ) * SIN( AEI(5) )
      SPSISQ = SPSISQ**2
      EARTH = AE + FSQ32 * SPSISQ**2 - FFSQ32 * SPSISQ
      AESAT = AEI(1) * AEI(2)
      PERHT = AEI(1) - AESAT - EARTH
      if( aei(2) .lt. 1.0D0 ) then
         APOHT = AEI(1) + AESAT - EARTH
      else
         APOHT =  -1.0D6
      endif
      AE35 = ( AE / ABS( AEI(1) ) )**3.5 /                              &
     &       ( 1.0D0 - AEI(2)**2 )**2
      COSI = COS( AEI(3) )
      RANDOT = -9.97D0 * AE35 * COSI
      PERDOT = 4.98D0 * AE35 * ( 5.0D0 * COSI**2 - 1.0D0 )
!     ....satellite arg. perigee dot at epoch in rad/cut
!      ORBBUF(122,1)=PERDOT*DEGRAD*CUT/SECDAY
      ORBBUF(22,3)=PERDOT*DEGRAD*CUT/SECDAY
!     ....satellite node rt. asc. dot at epoch in rad/cut
!      ORBBUF(123,1)=RANDOT*DEGRAD*CUT/SECDAY
      ORBBUF(23,3)=RANDOT*DEGRAD*CUT/SECDAY
!     ....satellite period at epoch in cut
!      ORBBUF(124,1)=TWOPI/DMNMOT/CUT
      ORBBUF(24,3)=TWOPI/DMNMOT/CUT
!     ....satellite perigee height at epoch in cul
!      ORBBUF(125,1)=PERHT/CUL
      ORBBUF(25,3)=PERHT/CUL
!     ....satellite apogee height at epoch in cul
!      ORBBUF(126,1)=APOHT/CUL
      ORBBUF(26,3)=APOHT/CUL
!     write(iout6,*) ' orbhdr: perht, apoht ', perht, apoht
!
      IYE=IYMDX(4)/10000
      IDE=IYMDX(4)-IYE*10000
      IME=IDE/100
      IDE=IDE-IME*100
      IH=IHMX(4)/100
      IM=IHMX(4)-IH*100
!     ....epoch time broken into year, month, day, etc.
!      ORBBUF(191,1)=IYE
      ORBBUF(41,4)=IYE
!      ORBBUF(192,1)=IME
      ORBBUF(42,4)=IME
!      ORBBUF(193,1)=IDE
      ORBBUF(43,4)=IDE
!      ORBBUF(194,1)=IH
      ORBBUF(44,4)=IH
!      ORBBUF(195,1)=IM
      ORBBUF(45,4)=IM
!      ORBBUF(196,1)=INT(MOD(SECX(4),60.0D0)*1.0D3+0.5D0)
      ORBBUF(46,4)=INT(MOD(SECX(4),60.0D0)*1.0D3+0.5D0)
!
!     write(iout6,*) ' orbbuf(41-46,3)=orbbuf(191-196,1) ',
!    1                (orbbuf(jjj,3),jjj=41,46   )
!
!     ....orbit theory indicator
!      ORBBUF(200,1)=2.0D0
      ORBBUF(50,4)=2.0D0
      WRITE(IUNT30) ORBBUF
!
!     ....end of title record------------------------------------
!
      CALL CLEARA(ORBBUF,350)
!     write(iout6,*) ' orbhdr: norbnf ', norbnf
!
!     ....create station header records
!
      IF(NORBNF.LE.0) GO TO 3000
!     write(iout6,*) ' orbhdr: infsta ', infsta
      DO 1000 N=1,NORBNF
      I=INFNDX(N)
!     write(iout6,*) ' orbhdr: 1000 n,i ', n, i
!     write(iout6,*) ' orbhdr: infsta(n) ', infsta(n)
      if( i .le. 0 ) go to 1000
      IBUF(N,1)=NAME(1,I)
      IBUF(N,2)=INFSTA(N)
 1000 END DO
!     write(iout6,*) ' orbhdr: after 1000 '
!     write(iout6,*) ' orbhdr: infndx ', infndx
      DO 2001 N=1,NORBNF
      M=INFNDX(N)
!     write(iout6,*) ' orbhdr: 2000 n,m ', n, m
      if( m .le. 0 ) go to 2001
      DO 2000 I=1,3
      J=I+1
      K=I+4
!     write(iout6,*) ' orbhdr: i,j,k,m ', i,j,k,m
!     write(iout6,*) ' orbhdr: sxyz(i,m) ', sxyz(i,m)
!     write(iout6,*) ' orbhdr: xlocv(i,1,m) ', xlocv(i,1,m)
      ORBBUF(N,J)=SXYZ(I,M)
      ORBBUF(N,K)=XLOCV(I,1,M)
!     write(iout6,*) ' orbhdr: n,j, orbbuf(n,j) ',
!    1                         n,j, orbbuf(n,j)
!     write(iout6,*) ' orbhdr: n,k, orbbuf(n,k) ',
!    1                         n,k, orbbuf(n,k)
 2000 END DO
 2001 END DO
!     write(iout6,*) ' orbhdr: after 2000 '
 3000 CONTINUE
      WRITE(IUNT30) ORBBUF
!
!----------------------------------------------------------------
!
      CALL CLEARA(ORBBUF,350)
      IF(NORBNF.LE.0) GO TO 5000
      DO 4000 N=1,NORBNF
         M=INFNDX(N)
!        write(iout6,*) ' orbhdr: 4000 n,m ', n, m
         if( m .le. 0 ) go to 4000
         DO 4001 I=1,3
            K=I+3
            ORBBUF(N,I)=XLOCV(I,2,M)
            ORBBUF(N,K)=XLOCV(I,3,M)
 4001    CONTINUE
 4000 END DO
 5000 CONTINUE
!     write(iout6,*) ' orbhdr: after 5000 '
      WRITE(IUNT30) ORBBUF
!
!----------------------------------------------------------------
!
      CALL CLEARA(ORBBUF,350)
!     ESQ=1.0D0-(1.0D0-FG)**2
!     ESQ=(2.0D0-FG)*FG                 ! WW GSOC, 16-APR-87
      ESQ=(2.0D0-FG)*FG
      IF(NORBNF.LE.0) GO TO 7000
      DO 6000 N=1,NORBNF
      M=INFNDX(N)
!     write(iout6,*) ' orbhdr: 6000 n,m ', n, m
      if( m .le. 0 ) go to 6000
!
!     ....station N geodetic latitude (rad)
      ORBBUF(N,1)=STAINF(1,M)
!     ....station N longitude (rad)
      ORBBUF(N,2)=STAINF(4,M)
!
!     write(iout6,*) 'orbhdr: m,stainf ',m,(stainf(kkk5,m),kkk5=1,7)
!     write(iout6,*) 'orbhdr: n,orbhdr ',n,(orbbuf(n,kkk5),kkk5=1,2)
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  12/89 JJM
!CCCC      SLTGSQ=STAINF(2,M)**2
!     ....stainf(3,M) is the sine of the latitude of station M
      SLTGSQ = STAINF(3,M)**2
!CC      CLATG=sqrt(1.0D0-SLTGSQ)
!     ....stainf(2,M) is the cosine of the latitude of station M
      CLATG = STAINF(2,M)
      R=AEG/SQRT(1.0D0-ESQ*SLTGSQ)
      RCL=CLATG*(STAINF(7,M)+R)
      THETAP=STAINF(1,M)-ATAN(SXYZ(3,M)/RCL)
!     ....sine of (geodetic latitude - geocentric latitude)
      ORBBUF(N,3)=SIN(THETAP)
!     ....cosine of (geodetic latitude - geocentric latitude)
      ORBBUF(N,4)=COS(THETAP)
!     ....height of station above ellipsoid (m)
      EBUF(N,9)=STAINF(7,M)
!
!     WRITE(IOUT6,*) 'orbhdr: m, sltgsq, clatg, r ',
!    1                        m, sltgsq, clatg,   r
!     write(iout6,*) 'orbhdr: n, thetap, orbbuf(n,3),orbbuf(n,4) ', n,
!    1             thetap, orbbuf(n,3),orbbuf(n,4)
!     write(iout6,*) 'orbhdr: n, ebuf(n,9) ', n, ebuf(n,9)
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 12/89 jjm
 6000 END DO
 7000 CONTINUE
!     write(iout6,*) ' orbhdr: after 7000 '
!     ....earth ellipsoid semi-major axis (m)
      ORBBUF(26,5)=AEG
!     ....earth ellipsoid flattening
      EBUF(3,10)=FG
!     EBUF(4,10)=NORBNF !! CHANGED BY DG 26-NOV-84
!     ....number of stations
      IBUF(4,10)=NORBNF
!
!
      NDAY = (MJDSX(1) + FSECUT(1) + 1 ) / SECDAY
!     ....Julian Date of first true pole coordinate
!      ORBBUF(228,1) = 2430000.5D0 + NDAY
      ORBBUF(28,5) = 2430000.5D0 + NDAY
!     ....Julian Date of fifth true pole coordinate
!      ORBBUF(229,1) = ORBBUF(228,1) + 40.0D0
      ORBBUF(29,5) = ORBBUF(28,5) + 40.0D0
!     ....conversion factor for pole (seconds to radians)
!      ORBBUF(230,1) = SECRAD
      ORBBUF(30,5) = SECRAD
!
      MJD1 = MJDSX(1)
      FSC1 = FSECUT(1)
      DO 7100 J=1,5
      MJD2 = MJD1
      FSC2 = FSC1
!
      CALL BUFEAR( MJD1, FSC1, MJD2, FSC2, 1, MJDR, FSECR, LR, AA,II)
      FSECTM(1) = FSC1
!
      CALL INPOLE( AA(KA1UT), AA(KDPSR), AA(KXPUT), AA(KYPUT),          &
     &  .FALSE., .FALSE., .FALSE., MJD1, FSECTM, 1, DUM1, IDUM1,        &
     &  IDUM2, IDUM3, IDUM4, DUM2, BIHMAT, DUM3, AA(KXDPOL),AA(KXDOTP))
!
!     ....x component of true pole
      EBUF(10+J, 10) = BIHMAT(3)
!     ....y component of true pole
      EBUF(15+J, 10) = BIHMAT(8)
! INCREMENT TIME BY 10 DAYS
      MJD1 = MJD1 + 864000
 7100 END DO
!     write(iout6,*) ' orbhdr: after 7100 '
!
!
!     ....time system indicator (1=UT1, 2=UT2, 3=UTC, 4=A1)
!      IBUF(471,1)=3
      IBUF(21,10)=3
! ABOVE ADDED 3/8/87 ACB
      WRITE(IUNT30) ORBBUF
      CALL CLEARA(ORBBUF,350)
!     write(iout6,*) ' orbhdr: return from orbhdr '
      RETURN
      END
