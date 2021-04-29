!$MNGRAV
      SUBROUTINE MNGRAV(MJDSEC,FSEC,XTEMP,XDDTMP,VMAT,EXPPAR,           &
     &                  NEQN,IDSATS,AA,II,LL)
!
      IMPLICIT DOUBLE PRECISION   (A-H,O-Z),LOGICAL (L)
      SAVE
!
!********1*********2*********3*********4*********5*********6*********7**
! MNGRAV           04/29/90            9004.0    PGMR - J. McCarthy
!                  10/01/91            9110.0         - S.B. LUTHCKE
!
! FUNCTION         CALCULATE ACCELERATIONS ON SATELLITE AND
!                  VARIATIONAL PARTIALS.  ALSO, CALCULATE EXPLICIT
!                  PARTIALS IF SOLVING FOR MOON GM.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    MEASUREMENT BLOCK START TIME IN MJD SECONDS
!   FSEC     I    A    FRACTIONAL SECONDS FOR EACH POINT IN BLOCK
!   XTEMP    I    A    SATELLITE STATE VECTOR
!   XDDTMP   O    A    TRUE OF REFERENCE ACCELERATIONS DUE TO MOONS
!   VMAT    I/O   A    VARIATIONAL PARTIALS MATRIX TO SUM INTO
!   EXPPAR  I/O   A    EXPLICIT PARTIALS OF FORCE MODEL PARAMETERS
!   NEQN     I    S    NUMBER OF FORCE MODEL PARAMETERS
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES
!   II      I/O   A    DYNAMIC ARRAY FOR INTEGER VARIABLES
!   LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL VARIABLES
!
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
!
      PARAMETER ( ZERO = 0.D0 )
      PARAMETER ( ONE =  1.D0 )
!
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/PLMINI/ IYMDP0(5),IHMP0(5),MJDP0(5),IMNUNT(5)
      COMMON/PLMINR/ RMNMIN(5),SECP0(5),FSECP0(5),                      &
     &               XMNCHG(5),XINT(5),XJDT1(5),XJDT2(5),               &
     &               XKEP1(6,5),XKEP2(6,5)
      COMMON/PLMONI/IMNNUM(5,2),NXPLMI
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION AA(1),II(1),LL(1)
      DIMENSION XTEMP(6),VMAT(3,6),XDDTMP(3),EXPPAR(NEQN,3)
      DIMENSION PARTOD(3)
!
!*************************************************************
!
      DIMENSION XMN(6), DMN(3), ACCMN(3)
      DIMENSION XKMN(6)
!
!*************************************************************
!
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
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
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CRMBI/RMBI(9)
      COMMON/CRMI/RMI(9)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
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
      COMMON/SETADJ/LSADRG(4),LSASRD(4),LSAGA(4),LSAGP,LSATID,LSAGM,    &
     &              LSADPM,LSAKF,LSADNX,LSADJ2,LSAPGM
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
      COMMON/SETIOR/IORDRG,IORSRD,IORGA,IPDDRG,IPDSRD,IPDGA
      COMMON/SETOPT/LSDRG,LSSRD,LSGA,LSORB,LTHDRG,LROCK4
      COMMON/SETPPT/JSPDRG(3),JSPSRD(3),JSPGA(3),JCSAT
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
!
      DATA LDEBUG/.false./
      DATA THREE/3.0D0/
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      XDDTMP(1)=ZERO
      XDDTMP(2)=ZERO
      XDDTMP(3)=ZERO
!
      DO 1000 IMN = 1, NPVAL(IXPMGM)
!
      GMMOON = AA( KPLMGM + IMN -1 )
!      PRINT *,' MNGRAV: MOON ## AND GMMOON ARE: ',IMN,GMMOON
!
! Check if IDSATS = MOON ## ( if the integrated body is the
! natural satellite of the central body)
      NUMMON = IMNNUM(IMN,1)
!      PRINT *,' MNGRAV: IDSATS,NUMMON IS: ',IDSATS,NUMMON
!
      IF(IDSATS.EQ.NUMMON) GO TO 1000
!
!        ....GET POSITIONS OF MOON
!
         CALL MOONPO(IMN, GM, MJDSEC, FSEC, XMN, XKMN,                  &
     &                   AA, 6)
!
!        ....COMPUTE DISTANCES BETWEEN SATELLITE AND MOON
!        ....DO IN TRUE OF INTEGRATION STEP
!
         RMN2 = ZERO
         XMND2 = ZERO
         DO 3002 IP=1,3
            DMN(IP) = XTEMP(IP) - XMN(IP)
            RMN2 = RMN2 + DMN(IP)**2
!>>>>>>
!           ....indirect effect -- dist**2 of MOON from coord center
            XMND2 = XMND2 + XMN(IP)**2
!<<<<<<
 3002    CONTINUE
!
         RMN   = SQRT( RMN2 )
         RMN3  = RMN * RMN2
         GMPR3 = GMMOON / RMN3
!>>>>>
!           ....indirect effect -- GM/dist**3 of moon from coord center
         XMND = SQRT( XMND2 )
         XMND3 = XMND * XMND2
         GMMNB3 = GMMOON / XMND3
!<<<<<
!
         CALL YMDHMS(MJDSEC, FSEC, IYPD, IHPD, SECPD, 1)
!
         IF( LDEBUG ) THEN
            WRITE(6,*) ' MNGRAV:TIME ', MJDSEC, FSEC, IYPD, IHPD, SECPD
            WRITE(6,3500) (XMN(IPH),IPH=1,3),                           &
     &        (XTEMP(IP),IP=1,3),                                       &
     &              DMN,                                                &
     &              RMN,RMN3
 3500    FORMAT(' MNGRAV: XMN ',3D15.5,                                 &
     &       ' MNGRAV: XSAT T O INT',3D20.10/                           &
     &       ' MNGRAV: DMN ',3D15.5,                                    &
     &       ' MNGRAV: RMN,RMN3 ',2D15.5)
         ENDIF
!
!        ....COMPUTE ACCELERATIONS DUE TO MOON
!
         if( ldebug )WRITE(6,3501) (XDDTMP(IP),IP=1,3)
 3501    FORMAT(' MNGRAV: XDDTMP BEFORE ',3D20.10)
!
         if( ldebug )WRITE(6,*) 'MNGRAV: VMAT BEFORE P,D',VMAT
!
         DO 3003 IP=1,3
!
!           ....direct effect
            ACCMN(IP) = -GMPR3 * DMN(IP)
!>>>>>>
!           ....indirect effect
            ACCMN(IP) = ACCMN(IP) - GMMNB3 * XMN(IP)
!<<<<<<
            XDDTMP(IP) = XDDTMP(IP) + ACCMN(IP)
!
!           ....COMPUTE CONTRIBUTION TO TRUE OF INTEG V-MATRIX
!
         VMAT(IP,1)=VMAT(IP,1)+THREE*(GMPR3/RMN2)*DMN(IP)*DMN(1)
         VMAT(IP,2)=VMAT(IP,2)+THREE*(GMPR3/RMN2)*DMN(IP)*DMN(2)
         VMAT(IP,3)=VMAT(IP,3)+THREE*(GMPR3/RMN2)*DMN(IP)*DMN(3)
         VMAT(IP,IP)=VMAT(IP,IP)-GMPR3
!
 3003    CONTINUE
!
!
         IF( LDEBUG ) THEN
            WRITE(6,3502) ACCMN, (XDDTMP(IP),IP=1,3)
 3502       FORMAT(' MNGRAV: ACCMN ',3D15.5,                            &
     &             ' MNGRAV: XDDTMP AFTER ',3D20.10)
         ENDIF
!
         CALL YMDHMS(MJDSEC,FSEC,IYMD,IHM,SEC,1)
!
!        ....SAVE MINIMUM DISTANCE AND TIME OF MINIMUM DISTANCE
!
         IF( RMNMIN(IMN) .GT. RMN ) THEN
            RMNMIN(IMN) = RMN
            IYMDP0(IMN) = IYMD
            IHMP0(IMN)  = IHM
            SECP0(IMN)  = SEC
            MJDP0(IMN)  = MJDSEC
            FSECP0(IMN) = FSEC
         ENDIF
!
!  ....COMPUTE EXPLICIT PARTIALS OF GM IF NECESSARY
!
      IF(.NOT.LSAPGM) GOTO 1000
      NUMMON = IMNNUM(IMN,1)
!      PRINT *,' MNGRAV: NUMMON IS: ',NUMMON
      DO 950 IKAT=1,NPVAL0(IXPMGM)
       IF(NUMMON.EQ.IMNNUM(IKAT,2)) THEN
!      PRINT *,' MNGRAV: ikat,imnnum(ikat,2): ',ikat,imnnum(ikat,2)
       INDPAR=IKAT
       GOTO 960
       ENDIF
  950 END DO
! THIS MOON'S GM IS NOT ADJUSTED
      GOTO 1000
! THIS MOON'S GM IS ADJUSTED
  960 CONTINUE
! COMPUTE EXPLICIT PARTIALS IN TOD
      PARTOD(1)=ACCMN(1)/GMMOON
      PARTOD(2)=ACCMN(2)/GMMOON
      PARTOD(3)=ACCMN(3)/GMMOON
! ROTATE PARTIALS TO TOR
      INDPAR=INDPAR+JSAPGM-1
!      PRINT *,' MNGRAV: INDPAR AND JSAPGM ARE: ',INDPAR,JSAPGM
      EXPPAR(INDPAR,1)=RMI(1)*PARTOD(1) + RMI(2)*PARTOD(2) +            &
     &                 RMI(3)*PARTOD(3)
      EXPPAR(INDPAR,2)=RMI(4)*PARTOD(1) + RMI(5)*PARTOD(2) +            &
     &                 RMI(6)*PARTOD(3)
      EXPPAR(INDPAR,3)=RMI(7)*PARTOD(1) + RMI(8)*PARTOD(2) +            &
     &                 RMI(9)*PARTOD(3)
 1000 END DO
!
!*********************************************************************
!
      RETURN
      END
