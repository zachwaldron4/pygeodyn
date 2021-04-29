!$ORBINF
      SUBROUTINE ORBINF(AA,II,LL,SXYZ,XLOCV,LENDNF,UTDT)
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
      COMMON/CORL02/KLNDTT,KLWSTR,KLNDTR,KLEVMF,KLTPMS,NXCL02
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
      COMMON/CORI02/KNTIME,KMJSTB,KMJSTC,KMJSTE,KISECT,                 &
     &              KINDH ,KNMH  ,KIVSAT,KIPXPF,KNTRTM,KMSTRC,          &
     &              KMSTRE,KISCTR,KISATR,KITRUN,KTRTMB,KTRTMC,          &
     &              KMJDVS,KNTMVM,NXCI02
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
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
      COMMON/CTHDOT/THDOT
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
      COMMON/NEQINT/NINTOT,MEMTOT,NEQNIM,NXNEQS
      COMMON/IOS/ISTOS1,ISTOS2,ISTOS3,ISTLST,INTSUB(3),JNTSUB(3),      &
     &        ILSTSG(80),INTOSP,MAPSAT(3,4)
!
      DIMENSION FSTEMP(1), FSECYR(1), IYMDRF(1), IHMRF(1), SECRF(1)
      DIMENSION SXYZ(3,1),XLOCV(3,3,1),UHAT(3),RENV(3)
      DIMENSION AA(1),II(1),LL(1),EBUF(100,7),IBUF(100,7),      &
     &   FSECX(7),XECF(7,3),VECF(7,3),XSUN(7,3),USUN(7,3),XYZSUN(3),  &
     &   XLUN(7,3),ULUN(7,3),XYZLUN(3),RS(7),RM(7),                   &
     &   RNG(15,7),AZ(15,7),EL(15,7),FSECUT(7)
      DIMENSION IND1(2),IND2(2),IYMD(7),IHM(7),SEC(7)
      DIMENSION prot(9)
      DIMENSION UTDT(1,4)
      DIMENSION DUM(3),DUM1(3,2,1)

      EQUIVALENCE (ORBBUF(1,1),EBUF(1,1),IBUF(1,1))
      DATA ZERO/0.0D0/,ONE/1.0D0/
!C      DATA IUNT30/31/    ! original
!     ....set unit to 29 to avoid conflict with ORBFIL which uses
!     ....units 30 and up  -- jjm 12/5/95
      DATA IUNT30/29/
      DATA J1/1/,J2/2/
      DATA LFIRST/.TRUE./
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!     write(6,*) 'orbinf: enter orbinf'
!
!     NPTS=(INFSTP-MJDSNF)/INFRT+1 || CHANGED BY TVM 22-FEB-85
!     NPTS=(INFSTP-MJDSNF)/INFRT
!  CHANGED BACK BY ANITA ON 22-MAY-86
!
!CC   NPTS=(INFSTP-MJDSNF)/INFRT+1
!     write(6,*) 'orbinf: infstp, mjdsnf, fnfrt ',
!    1                    infstp, mjdsnf, fnfrt
      NPTS=(INFSTP-MJDSNF)/FNFRT+1.D0
!
      IF( LFIRST ) THEN
         FRACFQ = ZERO
         LFIRST = .FALSE.
         CALL CLEARA(ORBBUF,350)
      ENDIF
!
      LENDNF=NPTS.LE.0
      IF(LENDNF) RETURN
      NPTS=MIN(NPTS,1)
      DO 1000 I=1,NPTS
!CC   FSECX(I)=(I-1)*INFRT+FNFSTR
      FSECX(I)=(I-1)*FNFRT+FNFSTR + FRACFQ
 1000 END DO


      ISATID=II(KISATN)
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


!     write(6,*) 'orbinf: after 1000  '
!
      CALL ORBSET(II(KNSAT),II(KIVSAT),LL(KLSETS),1 ,                   &
     &   KNSTEPS,NEQNI,II(KSGMNT-1+IRET),          &
     &   AA(KSGTM1-1+IRET),AA(KSGTM2-1+IRET),IRET,MJDSNF,FSECX(1),      &
     &   FSECX(NPTS),NPTS,AA,II)
      CALL ORBIT(MJDSNF,FSECX,AA(KS),NPTS,II(KINDH),II(KNMH),           &
     &   II(KMJDSC),AA(KFSEC),II(KMJDSV),AA(KFSECV),LL(KLSETS),         &
     &   II(KIPXPF),II(KIVSAT),1,II(KNMAX),II(KNTOLD),AA(KXTN),         &
     &   AA(KXTN),.TRUE.,.FALSE.,II(KPLPTR),II(KPANEL),II(KNMOVE),      &
     &   AA(KTPMES+4*MINTIM),LL(KLTPMS+6*MINTIM),LL(KSETDN),            &
     &   AA(KTMOS0-1+INTOSP),AA(KTMOS-1+INTOSP),AA(KTMOSP-1+INTOSP),    &
     &   AA(KSMXOS-1+KSUMXOS),AA(KSMXOS-1+KSUMPOS),AA(KSMXOS-1+KXDDTOS),&
     &   AA(KSMXOS-1+KPDDTOS),KNSTEPS,NEQNI,II(KSGMNT-1+IRET),          &
     &   AA(KSGTM1-1+IRET),AA(KSGTM2-1+IRET),AA,II,LL)

      CALL GRHRAN(MJDSNF,FSECX,.FALSE.,.TRUE.,                          &
     &   AA(KEQN),AA(KSRTCH),                                           &
     &   AA(KTHETG),AA(KCOSTH),AA(KSINTH),NPTS,AA,II,UTDT,.FALSE.,DUM,  &
     &   DUM1)
      CALL SUBTRK(AA(KXTN),AA(KTHETG),AA(KXY2),AA(KZT),AA(KRT),         &
     &   AA(KSATLT),AA(KSATLN),AA(KSATH),NPTS)
      CALL ECFIXP(AA(KXTN),AA(KCOSTH),AA(KSINTH),XECF,MINTIM,7,NPTS)
      CALL ECFIXV(AA(KVTN),AA(KCOSTH),AA(KSINTH),XECF,VECF,MINTIM,      &
     &   7,NPTS)
!
      THDT2S=360.985645D0*DEGRAD/SECDAY
      THDT2S = THDOT
!
      DO 2000 I=1,NPTS
      CALL SUNPO(MJDSNF,FSECX(I),XYZSUN,XYZLUN,TOBLIQ,USUN,ULUN,        &
     &   prot, .FALSE.,.TRUE.,.TRUE.,.TRUE.,.FALSE.,AA,II)
      RS(I)=XYZSUN(1)**2+XYZSUN(2)**2+XYZSUN(3)**2
      RS(I)=SQRT(RS(I))
      RM(I)=XYZLUN(1)**2+XYZLUN(2)**2+XYZLUN(3)**2
      RM(I)=SQRT(RM(I))
      DO 2000 J=1,3
      XSUN(I,J)=XYZSUN(J)/RS(I)
      XLUN(I,J)=XYZLUN(J)/RM(I)
 2000 CONTINUE
!     write(6,*) 'orbinf: after 2000  '
      CALL ECFIXP(XSUN,AA(KCOSTH),AA(KSINTH),USUN,7,7,NPTS)
      CALL ECFIXP(XLUN,AA(KCOSTH),AA(KSINTH),ULUN,7,7,NPTS)
      IF(NORBNF.LE.0) GO TO 7000

      IF( M .LE. 0 ) GO TO 6001
      DO 6000 N=1,NORBNF
      M=INFNDX(N)
      DO 6000 K=1,NPTS
      R=ZERO
      DO 3000 I=1,3
      UHAT(I)=XECF(K,I)-SXYZ(I,M)
      R=R+UHAT(I)**2
 3000 END DO
      R=SQRT(R)
      RNG(N,K)=R
      DO 4000 I=1,3
      UHAT(I)=UHAT(I)/R
 4000 END DO
      DO 5000 I=1,3
      RENV(I)=ZERO
      DO 5000 J=1,3
      RENV(I)=RENV(I)+UHAT(J)*XLOCV(J,I,M)
 5000 CONTINUE
      CALL DARCTN(RENV(J1),RENV(J2),AZ(N,K),1)
      AZ(N,K)=AZ(N,K)/DEGRAD
      COSEL=SQRT(ONE-RENV(3)**2)
      EL(N,K)=ATAN(RENV(3)/COSEL)/DEGRAD
 6000 CONTINUE
 6001 CONTINUE
 7000 CONTINUE
!     write(6,*) 'orbinf: after 7000  '
!
      FSTEMP(1) = FSECRF
!
! CONVERT REFERENCE DATE/TIME FROM ET TO UTC
      CALL UTCET(.FALSE., 1, MJDSRF, FSTEMP, FSECUT, AA(KA1UT) )
      FSECUT(1) = FSECUT(1) + 1
      CALL YMDHMS( MJDSRF, FSECUT, IYMDRF, IHMRF, SECRF, 1)
!
!
!  SET IYREF TO BE JAN 0 OF THE REFERENCE YEAR
      IYREF = IYMDRF(1) / 10000 * 10000 + 0100
!
!
! CONVERT JAN 0 TO INTERNAL MJDS FORMAT AND CHANGE BACK TO ET
      CALL YMDTIS(IYREF, 0, MJDSYR)
      FSTEMP(1) = zero
!
!...........................................................
!
!     ....remove call to utcet since mjdsyr should be jan 0 of ref
!     ....year in et time already    12/89 jjm
!
!CC CONVERT TIME BACK TO ET TIME
!CC      CALL UTCET(.TRUE., 1, MJDSYR, FSTEMP, FSECYR, AA(KA1UT) )
!
      fsecyr(1) = zero
!
!cc      write(iout6, 19115) MJDSYR, FSECYR
!cc19115 FORMAT(' ORBINF:19115 MJDSYR, FSECYR ',I12, G24.16 )
!
!...........................................................
!
!
!...................
!c      WRITE(iout6,*) ' et times '
!c      WRITE(iout6,*) ' mjdsnf, fsecx(1) ',mjdsnf, fsecx(1)
!c      CALL YMDHMS(MJDSNF,FSECX,IYMD,IHM,SEC,NPTS)
!c      DO 7778 I=1,NPTS
!c      WRITE(IOUT6,*) 'i, iymd, ihm, sec ', i, iymd(i), ihm(i), sec(i)
!c7778  continue
      CALL UTCET(.FALSE.,NPTS,MJDSNF,FSECX,FSECUT,AA(KA1UT))
!
!     ....test new version of ymdhms
!     ....this should fix problems with sec < 0   12/89 jjm
!>>>>>>>>>
      CALL ymdhms(MJDSNF,FSECUT,IYMD,IHM,SEC,NPTS)
!<<<<<<<<<<
!
!
!c      WRITE(iout6,*) ' utc times '
!c      WRITE(iout6,*) ' mjdsnf, fsecut(1) ',mjdsnf, fsecut(1)
!c      DO 7779 I=1,NPTS
!c      WRITE(IOUT6,*) 'i, iymd, ihm, sec ', i, iymd(i), ihm(i), sec(i)
!c7779  continue
!
!...........................................................
!
      NLOOP=1
      KENTRY=MOD(NENTRY,7)
      MENTRY=KENTRY+NPTS
      MXNTRY=MIN(MENTRY,7)
      IF(MENTRY.GT.MXNTRY) NLOOP=2
      KENTRY=KENTRY+1
      IND1(1)=KENTRY
      IND1(2)=1
      IND2(1)=MXNTRY
      MENTRY=MOD(MENTRY,7)
      IND2(2)=MENTRY
      J=0
      DO 9000 ILOOP=1,NLOOP
      NTRY1=IND1(ILOOP)
      NTRY2=IND2(ILOOP)
      DO 8000 IENTRY=NTRY1,NTRY2
      J=J+1
!
!CC      ORBBUF(1,IENTRY)=(DFLOAT(MJDSNF-MJDSRF)+(FSECX(J)-FSECRF))/SECD
      ORBBUF(1,IENTRY)=( DBLE( MJDSNF - MJDSYR )                      &
     &                 + ( FSECX(J) - FSECYR(1) ) )  / SECDAY
!     write(iout6, 19116) MJDSYR, FSECYR(1), MJDSNF, FSECX(J),
!    1                ORBBUF(1, IENTRY)
19116 FORMAT(' ORBINF:19116 MJDSYR, FSECYR(1), MJDSNF, FSECX(J)',       &
     &    2(2X,I12, G24.16 )/' ORBBUF(1,IENTRY) ', G24.16)
!
      IBUF(3,IENTRY)=IYMD(J)
      IBUF(4,IENTRY)=IHM(J)
      EBUF(5,IENTRY)=SEC(J)
!
!>>>>>>>
!     write(iout6,*)  ' ibuf(3,),ibuf(4,),ebuf(5,) ',ibuf(3,ientry),
!    1                  ibuf(4,ientry), ebuf(5,ientry)
!<<<<<<<
!
!     ....sun position unit vector (ecf)
      EBUF(11,IENTRY)=USUN(J,1)
      EBUF(12,IENTRY)=USUN(J,2)
      EBUF(13,IENTRY)=USUN(J,3)
!     ....moon position unit vector (ecf)
      EBUF(14,IENTRY)=ULUN(J,1)
      EBUF(15,IENTRY)=ULUN(J,2)
      EBUF(16,IENTRY)=ULUN(J,3)
!     ....earth-sun distance(m)
      ORBBUF(9,IENTRY)=RS(J)
!     ....earth-moon distance (m)
      ORBBUF(10,IENTRY)=RM(J)
      DO 7200 I=1,3
      IX=I+10
      IV=I+13
!     ....satellite ecf position (m)
      ORBBUF(IX,IENTRY)=XECF(J,I)
!     ....satellite ecf velocity (m/s)
      ORBBUF(IV,IENTRY)=VECF(J,I)
 7200 END DO
!     write(6,*) 'orbinf: after 7200  '
!     ....SAT LONGITUDE  (DEG)
      EBUF(33,IENTRY)=AA(KSATLN+J-1)
!     ....SAT GEOD. LATITUDE   (DEG)
      EBUF(34,IENTRY)=AA(KSATLT+J-1)
!     ....SAT HEIGHT (M)
      ORBBUF(18,IENTRY)=AA(KSATH+J-1)
!     ....R.A. GREENWICH (RAD)
      ORBBUF(19,IENTRY)=AA(KTHETG+J-1)
!     ....THETA G DOT  (RAD/S)
      ORBBUF(20,IENTRY)=THDT2S
      IF(NORBNF.LE.0) GO TO 8000
      DO 7500 N=1,NORBNF
!     ....satellite azimuth at station N
      EBUF(40+N,IENTRY)=AZ(N,J)
!     ....satellite elevation at station N
      EBUF(55+N,IENTRY)=EL(N,J)
!     ....satellite range at station N
      ORBBUF(35+N,IENTRY)=RNG(N,J)
 7500 END DO
 8000 END DO
!     write(6,*) 'orbinf: after 8000  '
      IF(NTRY2.LT.7) GO TO 9000
      WRITE(IUNT30) ORBBUF
      CALL CLEARA(ORBBUF,350)
 9000 END DO
      NENTRY=MENTRY
!
!CC   MJDSNF=MJDSNF+NPTS*INFRT
      MJDSNF=MJDSNF+NPTS*FNFRT
      FRACFQ = FRACFQ + ( FNFRT - INT(FNFRT) )
!
!     write(6,*) 'orbinf: return from orbinf '
      RETURN
      END
