!$VMATHD
      SUBROUTINE VMATHD(AA,MJDSB,FSECB,IDSAT,NSAT,HSTEP,IORDER,HVSTEP,  &
     &                  IORDRV,MJDSND,FSECND,JSAFRC,PARML0,SCRTCH,      &
     &                  MJDVMS,FSCVMS,NTIME,LEVMF,II)
!********1*********2*********3*********4*********5*********6*********7**
! VMATHD           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA
!   MJDSB
!   FSECB
!   IDSAT
!   NSAT
!   HSTEP
!   IORDER
!   HVSTEP
!   IORDRV
!   MJDSND
!   FSECND
!   JSAFRC
!   PARML0
!   SCRTCH
!   MJDVMS
!   FSCVMS
!   NTIME
!   LEVMF
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( MPG = 92)
      PARAMETER ( MGI = 30)
      PARAMETER ( MINMIL = -1000000)
!
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
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
      COMMON/CONTRL/LSIMDT,LOBS  ,LORB  ,LNOADJ,LNADJL,LORFST,LORLST,   &
     &              LORALL,LORBOB,LOBFST,LOBLST,LOBALL,LPREPO,LORBVX,   &
     &              LORBVK,LACC3D,LSDATA,LCUTOT,LACCEL,LDYNAC,LFRCAT,   &
     &              LNIAU, NXCONT
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/EMAT  /EMTNUM,EMTPRT,EMTCNT,VMATRT,VMATS,VMATFR,FSCVMA,    &
     &              XEMAT
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      COMMON/G2SINF/G2SVER,G2SDAT,G2SRTM,G2SCOM,TDFVER,TDFRTM,SETCDS,   &
     &              GPCKSM,GPDEG ,GPORD ,XG2SIN
      COMMON/GPSINT/IGPSBW,NGPSBW
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/NFORCE/NHRATE,NSURF,MPXHDT,MXHRG,MXFMG,NFSCAC,NXFORC
      COMMON/NHINPT/NH1VOC,NXNHIN
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
      COMMON/PLNETI/IPLNET(999),IPLNIN(999),MPLNGD(999),MPLNGO(999),   &
     &              IPLNZ(999),NXPLNI
      COMMON/VMATFI/MJDVMA
      COMMON/VMATFL/LVMTF,LVMTFO
      COMMON/TRAJI/MJDSTR,NTRAJC,MTRAJC,NXTRAJ
!
      DIMENSION AA(1)
      DIMENSION II(*)
      DIMENSION MJDSB(NSETA),FSECB(NSETA),NSAT(NSETA),IDSAT(NSATA)
      DIMENSION HSTEP(NSETA),IORDER(NSETA),HVSTEP(NSETA),IORDRV(NSETA)
      DIMENSION MJDSND(NSETA),FSECND(NSETA),JSAFRC(MXFMG,NSETA)
      DIMENSION FSCVMS(NSETA),MJDVMS(NSETA),NTIME(NSETA),LEVMF(NSETA)
      DIMENSION PARML0(3,1)
      DIMENSION SCRTCH(1)
      DIMENSION DWORD(30)
      DIMENSION FSECOT(1),IYMD(1),IHM(1),SEC(1)
!C LOCAL ARRAYS DIMENSIONED BY MPG
      DIMENSION LNFR(MPG)
!C LOCAL ARRAYS DIMENSIONED BY MGI
      DIMENSION XGI(MGI,2),GLABLE(MGI),IPTLBL(MGI),NUMPAR(MGI)
!C
      DATA ZERO/0.D0/,C1D6/1.D6/,XLARGE/999999999999.D0/,XSMALL/-99.0D0/
      DATA EPS /1.D-6/
      DATA WORDL/2.D14/
      DATA GLABLE/                                                      &
        0.D0,               10000200000000.D0,  10000400000000.D0,      &
        10000800000000.D0,  20000000000000.D0,                          &
        30002000000000.D0,  30003000000000.D0,  30004000000000.D0,      &
        30005000000000.D0,  30006000000000.D0,  30007000000000.D0,      &
        30008000000000.D0,  30009000000000.D0,  30010000000000.D0,      &
        31000000000000.D0,  50020000000000.D0,                          &
                                                50400000000000.D0,      &
        50500000000000.D0,                                              &
        60000000000000.D0,                                              &
        70000000000000.D0,  71000000000000.D0,  80000000000000.D0,      &
        88000000000000.D0,  90000000000000.D0,  90001000000000.D0,      &
        95000000000001.D0,  95000000000002.D0,  95000000000003.D0,      &
        97000000000001.D0,  110000000002000.D0/
!       IXSATP              IXDRAG              IXSLRD
!       IXACCL              IXGPCA
!       IXAREA              IXSPRF              IXDFRF
!       IXEMIS              IXTMPA              IXTMPC
!       IXTIMD              IXTIMF              IXTHTX
!       IXTHDR              IXFAFM
!                                               IXDXYZ
!       IXGPSBW
!       IXGPC
!       IXTIDE              IXETDE              IXLOCG
!       IXKF                IXGM                IXPMGM
!       IXPOLX              IXPOLY              IXUT1
!       IXXTRO              IXFGFM
!
!  THE FOLLWING HAVE BEEN DATA INITALIZED FOR NOW
!  THEY SHOULD BE COMING FROM 2S
!******************************************************************
      DATA IUNEMT/71/,IUNVMT/80/,ISURFG/0/,IFXSTP/0/
!
!     ....LNFR IS .TRUE. IF A PARAMETER IS NOT A FORCE MODEL PARAMETER
!     ....EACH ELEMENT IS MATCHED WITH A POINTER IN /NPCOMX/
!
!            IXARC   IXSATP  IXDRAG  IXSLRD  IXACCL  IXGPCA  IXGPSA
!            IXAREA  IXSPRF  IXDFRF  IXEMIS  IXTMPA  IXTMPC  IXTIMD
!            IXTIMF  IXTHTX  IXTHDR  IXOFFS  IXBISA  IXFAGM  IXFAFM
!            IXATUD  IXRSEP  IXACCB  IXDXYZ  IXGPSBW IXCAME  IXBURN
!            IXGLBL  IXGPC   IXGPS,  IXTGPC  IXTGPS  IXGPCT  IXGPST
!            IXTIDE  IXETDE  IXOTDE  IXOTPC  IXOTPS  IXLOCG  IXKF
!            IXGM    IXSMA   IXFLTP  IXFLTE  IXPLTP  IXPLTV  IXPMGM
!            IXPMJ2  IXVLIT  IXEPHC  IXEPHT  IXH2LV  IXL2LV  IXOLOD
!            IXPOLX  IXPOLY  IXUT1   IXPXDT  IXPYDT  IXUTDT  IXVLBI
!            IXVLBV  IXXTRO  IXBISG  IXSSTF  IXFGGM  IXFGFM  IXLNTM
!            IXLNTA  IX2CCO  IX2SCO  IX2GM   IX2BDA  IXRELP  IXJ2SN
!            IXGMSN  IXPLNF  IXPSRF  IANTD   IXTARG
!            IXSTAP  IXSSTC  IXSSTS  IXSTAV  IXSTL2
!            IXSTH2  IXDPSI  IXEPST  IXCOFF  IXTOTL
      DATA LNFR/                                                        &
             .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,   &
             .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,   &
             .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.FALSE.,   &
             .TRUE. ,.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.TRUE. ,.FALSE.,   &
             .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,   &
             .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,   &
             .FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.FALSE.,   &
             .TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,   &
             .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,   &
             .TRUE. ,.FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.FALSE.,.TRUE. ,   &
             .TRUE. ,.true. ,.true. ,.true. ,.true. ,.true. ,.true. ,   &
             .true. ,.true. ,.true. ,.TRUE. ,.TRUE. ,                   &
             .TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,                   &
             .TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. /
      ! Lowercase .true. indicates unsure
!
!******************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************

      IF (LDYNAC) THEN
          LNFR(22) = .FALSE.
          LNFR(24) = .FALSE.
      END IF

      ! INITIALIZATION
      MJDVMA = 0
      FSCVMA = ZERO

      ! INITIALIZE PARAMETER LABEL POINTERS
      DO I = 1, MGI
          IPTLBL(I) = MINMIL
      END DO

      ! SAT
      IPTLBL(1) = IPVAL0(IXSATP)
      ! DRAG
      IPTLBL(2) = IPVAL0(IXDRAG)
      ! SOLRAD
      IPTLBL(3) = IPVAL0(IXSLRD)
      ! GENACC
      IPTLBL(4) = IPVAL0(IXACCL)
      ! ARC GEOPOTENTIAL
      IPTLBL(5) = IPVAL0(IXGPCA)

      ! PANEL AREA PARAMETER
      IPTLBL(6) = IPVAL0(IXAREA)
      ! PANEL SPECULAR REFLECTIVITY PARAMETER
      IPTLBL(7) = IPVAL0(IXSPRF)
      ! PANEL DIFFUSE REFLECTIVITY PARAMETER
      IPTLBL(8) = IPVAL0(IXDFRF)
      ! PANEL EMISSIVITY PARAMETER
      IPTLBL(9) = IPVAL0(IXEMIS)
      ! PANEL TEMPERATURE A PARAMETER
      IPTLBL(10) = IPVAL0(IXTMPA)
      ! PANEL TEMPERATURE C PARAMETER
      IPTLBL(11) = IPVAL0(IXTMPC)
      ! PANEL TEMPERATURE TIME D PARAMETER
      IPTLBL(12) = IPVAL0(IXTIMD)
      ! PANEL TEMPERATURE TIME F PARAMETER
      IPTLBL(13) = IPVAL0(IXTIMF)
      ! PANEL TEMPERATURE THETAX PARAMETER
      IPTLBL(14) = IPVAL0(IXTHTX)
      ! ATTITUDE PARAMETERS
      IPTLBL(15) = IPVAL0(IXTHDR)
      ! ARC FORCE MODEL FANTOM PARAMETERS
      IPTLBL(16) = IPVAL0(IXFAFM)
      ICT = 16
      ! ATTITUDE PARAMETERS -- FORCE MODEL ONLY WITH DYNAMIC ACCELEROMETR
      IF (LDYNAC) THEN
          IPTLBL(ICT+1) = IPVAL0(IXATUD)
          ICT = ICT+1
      END IF
      ! ARC FORCE MODEL CROSSOVER DYNAMIC SEPARATOR
      IPTLBL(ICT+1) = IPVAL0(IXFAFM)
      ICT = ICT+1
      ! ACC BIAS PARAMETERS -- FORCE MODEL ONLY WITH DYNAMIC ACCELEROMETR
      IF (LDYNAC) THEN
          IPTLBL(ICT+1) = IPVAL0(IXACCB)
          ICT = ICT+1
      END IF
      ! DELTA STATE PARAMETERS
      IPTLBL(ICT+1) = IPVAL0(IXDXYZ)
      ICT = ICT+1
      ! GPS BOX-WING PARAMETERS
      IPTLBL(ICT+1) = IPVAL0(IXGPSBW)
      ICT = ICT+1

      ! GLOBAL GEOPOTENTIAL
      IPTLBL(ICT+1) = IPVAL0(IXGPC)
      ! STANDARD TIDE
      IPTLBL(ICT+2) = IPVAL0(IXTIDE)
      ! EXTENDED TIDES
      IPTLBL(ICT+3) = IPVAL0(IXETDE)
      ! LOCAL GRAVITY
      IPTLBL(ICT+4) = IPVAL0(IXLOCG)
      ! FIGURE AXIS SCALE FACTOR
      IPTLBL(ICT+5) = IPVAL0(IXKF)
      ! GM
      IPTLBL(ICT+6) = IPVAL0(IXGM)
      ! PLANETARY MOON GM
      IPTLBL(ICT+7) = IPVAL0(IXPMGM)
      ! DYNAMIC POLAR MOTION
      IPTLBL(ICT+8) = IPVAL0(IXPOLX)
      IPTLBL(ICT+9) = IPVAL0(IXPOLY)
      IPTLBL(ICT+10) = IPVAL0(IXUT1)
      ! PLANET ORIENTATION
      IPTLBL(ICT+11) = IPVAL0(IXXTRO)
      ! GLOBAL FORCE MODEL
      IPTLBL(ICT+12) = IPVAL0(IXFGFM)

      ! MAIN HEADER RECORD
      DWORD(1) = -EMTNUM
      DWORD(2) = -EMTNUM
      DWORD(3) = VMATRT
      DWORD(4) = ZERO
      ISMALL = NSETA + 1
      DO ISET = 1,NSETA
          NSTEPS = 2 * (MAX(IORDER(ISET),IORDRV(ISET))-1)
          NH = NSTEPS - IORDER(ISET) + 2
          NHD = NH1VOC + 1 - NH
          NHD = MAX(NHD,0)
          NH = NH + NHD
          NSTEPV = 2 * (IORDRV(ISET)-1)
          NHV = NSTEPV - IORDRV(ISET) + 2
          NHD = NH1VOC + 1 - NHV
          NHD = MAX(NHD,0)
          NHV = NHV + NHD
          TIMES = DBLE(NH) * HSTEP(ISET)
          TIMEV = DBLE(NHV) * HVSTEP(ISET)
          TIMEM = MIN(TIMES,TIMEV)
          NTIME(ISET) = 1
          IF (.NOT. LVMTFO) THEN
              NTIME(ISET) = 1 + TIMEM/VMATRT
          END IF
          LEVMF(ISET) = .FALSE.

          ! SET THE FIRST VMAT OUTPUT TIME
          IF (NTRAJC > 0) THEN
              ! USE THE FIRST ORBFIL START TIME IF ORBFIL OUTPUT IS
              ! REQUESTED
              MJDVMS(ISET)=II(KMSTRC)
              FSCVMS(ISET)=AA(KFSTRC)
          ELSE
              ! USE THE EPOCH START TIME
              MJDVMS(ISET)=MJDSB(ISET)
              FSCVMS(ISET)=FSECB(ISET)
          END IF

          XCOMP = DBLE(MJDSB(ISET)) + FSECB(ISET)
          IF (XCOMP < XLARGE) THEN
              XLARGE = XCOMP
              ISMALL = ISET
          END IF
      END DO
      IF (ISMALL > NSETA) THEN
          WRITE(IOUT6,11000)
          WRITE(IOUT6,11001)
          WRITE(IOUT6,*) 'VMATHD: ISMALL, NSETA ', ISMALL, NSETA
          STOP
      END IF

      DWORD(5) = MJDSB(ISMALL) + REPDIF
      DWORD(6) = FSECB(ISMALL)
      DWORD(7) = VMATS + REPDIF
      DWORD(8) = VMATFR

      DWORD(9) = DBLE(NSATA)
      DWORD(10) = DBLE(IPLNET(ICBDGM))
      DWORD(11) = G2SRTM
      DWORD(12) = G2ERTM
      DWORD(13) = DBLE(ISURFG)
      DWORD(14) = G2EVER
      NNP = NXNPCX - 1
      IF (NNP > MPG) THEN
          WRITE(IOUT6,11020)
          WRITE(IOUT6,11021)
          WRITE(IOUT6,*) 'VMATHD: NNP, MPG ', NNP, MPG
          STOP
      END IF
      NGI = 0
      DO I = 1, NNP
          IF (.NOT. LNFR(I) .AND. NPVAL0(I) > 0) THEN
              NGI = NGI + 1
          END IF
      END DO
      IF (NPVAL0(IXGPCA) > 0 .AND. NPVAL0(IXGPSA) > 0) THEN
          NGI = NGI - 1
      END IF
      NXGEOP = MIN(NPVAL0(IXGPC),1) + MIN(NPVAL0(IXGPS),1) &
             + MIN(NPVAL0(IXTGPC),1) + MIN(NPVAL0(IXTGPS),1) &
             + MIN(NPVAL0(IXGPCT),1) + MIN(NPVAL0(IXGPST),1) - 1
      NXGEOP = MAX(NXGEOP,0)
      NGI = NGI - NXGEOP
      IF (NPVAL0(IXETDE) > 0 .AND. NPVAL0(IXOTDE) > 0) THEN
          NGI = NGI - 1
      END IF
      IF (NGI > MGI) THEN
          WRITE(IOUT6,11030)
          WRITE(IOUT6,11031)
          WRITE(IOUT6,*) 'VMATHD: NGI, MGI ', NGI, MGI
          STOP
      END IF
      DWORD(15) = DBLE(NGI)
      DWORD(16) = VLIGHT

      DWORD(20) = 0.D0

      WRITE(IUNVMT) DWORD

      ! SATELLITE PARAMETERS RECORD
      ! FOLLOWED BY GROUP IDENTIFIERS RECORD
      ! FOLLOWED BY PARM LABELS RECORD
      ! LOOP ON NUMBER OF SETS GETTING EVERY SATELLITE IN SET

      ISATC = 0
      DO ISET = 1, NSETA
          NSATS = NSAT(ISET)
          DO ISAT = 1, NSATS
              ISATC = ISATC + 1
              DWORD(1) = DBLE(IDSAT(ISATC))
              DWORD(2) = DBLE(IFXSTP)
              DWORD(3) = HSTEP(ISET)
              DWORD(4) = DBLE(IORDER(ISET))
              DWORD(5) = HVSTEP(ISET)
              DWORD(6) = DBLE(IORDRV(ISET))
              CALL UTCET(.FALSE., 1, MJDSB(ISET), FSECB(ISET), &
                         FSECOT, AA(KA1UT))
              CALL YMDHMS(MJDSB(ISET), FSECOT, IYMD, IHM, SEC, 1)
              ISEC = SEC(1)
              SEC(1) = SEC(1) - ISEC
              DWORD(7) = DBLE(IYMD(1))
              DWORD(8) = DBLE(ISEC+100*IHM(1)) + SEC(1)
              DWORD(9) = DBLE(MJDSB(ISET)) + REPDIF
              DWORD(10) = FSECB(ISET)
              DWORD(11) = DWORD(7)
              DWORD(12) = DWORD(8)
              DWORD(13) = DWORD(9)
              DWORD(14) = DWORD(10)
              IHOLD = VMATS + EPS
              DWORD(15) = VMATFR
              CALL UTCET(.FALSE., 1, IHOLD, DWORD(15), &
                         FSECOT, AA(KA1UT))
              CALL YMDHMS(IHOLD, FSECOT, IYMD, IHM, SEC, 1)
              ISEC = SEC(1)
              SEC(1) = SEC(1) - ISEC
              DWORD(15) = DBLE(IYMD(1))
              DWORD(16) = DBLE(ISEC+100*IHM(1)) + SEC(1)
              DWORD(17) = VMATS + REPDIF
              DWORD(18) = VMATFR
              WRITE(IUNVMT) DWORD

              ! SAT PARM FINISHED ; GROUP IDENTIFIERS START

              IGCT = 0
              NI = 0
              ! SAT
              NUMPAR(1) = 6
              IGCT = IGCT + 1
              XGI(IGCT,1) = GLABLE(1)
              XGI(IGCT,2) = DBLE(NUMPAR(1))
              NI = NI + NUMPAR(1)
              ! DRAG
              NUMPAR(2) = JSAFRC(2,ISET) - JSAFRC(1,ISET)
              IF (NUMPAR(2) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(2)
                  XGI(IGCT,2) = DBLE(NUMPAR(2))
                  NI = NI + NUMPAR(2)
              END IF
              ! SOLRAD
              NUMPAR(3) = JSAFRC(3,ISET) - JSAFRC(2,ISET)
              IF (NUMPAR(3) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(3)
                  XGI(IGCT,2) = DBLE(NUMPAR(3))
                  NI = NI + NUMPAR(3)
              END IF
              ! GENACC
              NUMPAR(4) = JSAFRC(4,ISET) - JSAFRC(3,ISET)
              IF (NUMPAR(4) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(4)
                  XGI(IGCT,2) = DBLE(NUMPAR(4))
                  NI = NI + NUMPAR(4)
              END IF
              ! ARC GEOPOTENTIAL
              NUMPAR(5) = NPVAL0(IXGPCA) + NPVAL0(IXGPSA)
              IF (NUMPAR(5) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(5)
                  XGI(IGCT,2) = DBLE(NUMPAR(5))
                  NI = NI + NUMPAR(5)
              END IF
              ! PANEL AREA PARAMETER
              NUMPAR(6) = NPVAL0(IXAREA)
              IF (NUMPAR(6) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(6)
                  XGI(IGCT,2) = DBLE(NUMPAR(6))
                  NI = NI + NUMPAR(6)
              END IF
              ! PANEL SPECULAR REFLECTIVITY PARAMETER
              NUMPAR(7) = NPVAL0(IXSPRF)
              IF (NUMPAR(7) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(7)
                  XGI(IGCT,2) = DBLE(NUMPAR(7))
                  NI = NI + NUMPAR(7)
              END IF
              ! PANEL DIFFUSE REFLECTIVITY PARAMETER
              NUMPAR(8) = NPVAL0(IXDFRF)
              IF (NUMPAR(8) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(8)
                  XGI(IGCT,2) = DBLE(NUMPAR(8))
                  NI = NI + NUMPAR(8)
              END IF
              ! PANEL EMISSIVITY PARAMETER
              NUMPAR(9) = NPVAL0(IXEMIS)
              IF (NUMPAR(9) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(9)
                  XGI(IGCT,2) = DBLE(NUMPAR(9))
                  NI = NI + NUMPAR(9)
              END IF
              ! PANEL TEMPERATURE A PARAMETER
              NUMPAR(10) = NPVAL0(IXTMPA)
              IF (NUMPAR(10) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(10)
                  XGI(IGCT,2) = DBLE(NUMPAR(10))
                  NI = NI + NUMPAR(10)
              END IF
              ! PANEL TEMPERATURE C PARAMETER
              NUMPAR(11) = NPVAL0(IXTMPC)
              IF (NUMPAR(11) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(11)
                  XGI(IGCT,2) = DBLE(NUMPAR(11))
                  NI = NI + NUMPAR(11)
              END IF
              ! PANEL TEMPERATURE TIME D PARAMETER
              NUMPAR(12) = NPVAL0(IXTIMD)
              IF (NUMPAR(12) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(12)
                  XGI(IGCT,2) = DBLE(NUMPAR(12))
                  NI = NI + NUMPAR(12)
              END IF
              ! PANEL TEMPERATURE TIME F PARAMETER
              NUMPAR(13) = NPVAL0(IXTIMF)
              IF (NUMPAR(13) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(13)
                  XGI(IGCT,2) = DBLE(NUMPAR(13))
                  NI = NI + NUMPAR(13)
              END IF
              ! PANEL TEMPERATURE THETAX PARAMETER
              NUMPAR(14) = NPVAL0(IXTHTX)
              IF (NUMPAR(14) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(14)
                  XGI(IGCT,2) = DBLE(NUMPAR(14))
                  NI = NI + NUMPAR(14)
              END IF
              ! THERMAL DRAG
              NUMPAR(15) = NPVAL0(IXTHDR)
              IF (NUMPAR(15) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(15)
                  XGI(IGCT,2) = DBLE(NUMPAR(15))
                  NI = NI + NUMPAR(15)
              END IF
              ! ARC FORCE MODEL PARAMETER
              NUMPAR(16) = NPVAL0(IXFAFM)
              IF (NUMPAR(16) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(16)
                  XGI(IGCT,2) = DBLE(NUMPAR(16))
                  NI = NI + NUMPAR(16)
              END IF

              IPT = 16

              ! DELTA STATE PARAMETERS
              NUMPAR(IPT+1) = NPVAL0(IXDXYZ)
              IF (NUMPAR(IPT+1) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+1)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+1))
                  NI = NI + NUMPAR(IPT+1)
              END IF
              IPT = IPT + 1

              ! GPS BOX-WING PARAMETERS
              NUMPAR(IPT+1) = 0
              IF (NPVAL0(IXGPSBW) > 0) THEN
                  IGPS = 0
                  CALL FNDNUM(IDSAT(ISET), II(KGPSID), IGPSBW/9, IGPS)
                  IF (IGPS > 0) THEN
                      NUMPAR(IPT+1) = 9
                  END IF
              END IF
              IF (NUMPAR(IPT+1) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+1)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+1))
                  NI = NI + NUMPAR(IPT+1)
              END IF
              IPT = IPT + 1

              ! GLOBAL GEOPOTENTIAL
              NUMPAR(IPT+1) = NPVAL0(IXGPC) + NPVAL0(IXGPS) &
                            + NPVAL0(IXTGPC) + NPVAL0(IXTGPS) &
                            + NPVAL0(IXGPCT) + NPVAL0(IXGPST)
              IF (NUMPAR(IPT+1) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+1)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+1))
                  NI = NI + NUMPAR(IPT+1)
              END IF
              ! STANDARD TIDE
              NUMPAR(IPT+2) = NPVAL0(IXTIDE)
              IF (NUMPAR(IPT+2) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+2)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+2))
                  NI = NI + NUMPAR(IPT+2)
              END IF
              ! EXTENDED TIDES
              NUMPAR(IPT+3) = NPVAL0(IXETDE)+NPVAL0(IXOTDE)
              IF (NUMPAR(IPT+3) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+3)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+3))
                  NI = NI + NUMPAR(IPT+3)
              END IF
              ! LOCAL GRAVITY
              NUMPAR(IPT+4) = NPVAL0(IXLOCG)
              IF (NUMPAR(IPT+4) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+4)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+4))
                  NI = NI + NUMPAR(IPT+4)
              END IF
              ! FIGURE AXIS SCALE FACTOR
              NUMPAR(IPT+5) = NPVAL0(IXKF)
              IF (NUMPAR(IPT+5) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+5)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+5))
                  NI = NI + NUMPAR(IPT+5)
              END IF
              ! GM
              NUMPAR(IPT+6) = NPVAL0(IXGM)
              IF (NUMPAR(IPT+6) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+6)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+6))
                  NI = NI + NUMPAR(IPT+6)
              END IF
              ! PLANETARY MOON GM
              NUMPAR(IPT+7) = NPVAL0(IXPMGM)
              IF (NUMPAR(IPT+7) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+7)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+7))
                  NI = NI + NUMPAR(IPT+7)
              END IF
              ! DYNAMIC POLAR MOTION (X)
              NUMPAR(IPT+8) = NPVAL0(IXPOLX)
              IF (NUMPAR(IPT+8) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+8)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+8))
                  NI = NI + NUMPAR(IPT+8)
              END IF
              ! DYNAMIC POLAR MOTION (Y)
              NUMPAR(IPT+9) = NPVAL0(IXPOLY)
              IF (NUMPAR(IPT+9) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+9)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+9))
                  NI = NI + NUMPAR(IPT+9)
              END IF
              ! DYNAMIC POLAR MOTION (UT1)
              NUMPAR(IPT+10) = NPVAL0(IXUT1)
              IF (NUMPAR(IPT+10) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+10)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+10))
                  NI = NI + NUMPAR(IPT+10)
              END IF
              ! PLANET ORIENTATION PARAMETERS
              NUMPAR(IPT+11) = NPVAL0(IXXTRO)
              IF (NUMPAR(IPT+11) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+11)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+11))
                  NI = NI + NUMPAR(IPT+11)
              END IF
              ! FANTOM GLOBAL FORCE PARAMETERS
              NUMPAR(IPT+12) = NPVAL0(IXFGFM)
              IF (NUMPAR(IPT+12) > 0) THEN
                  IGCT = IGCT + 1
                  XGI(IGCT,1) = GLABLE(IPT+12)
                  XGI(IGCT,2) = DBLE(NUMPAR(IPT+12))
                  NI = NI + NUMPAR(IPT+12)
              END IF
              XNI = DBLE(NI)
              WRITE(IUNVMT) (XGI(JGCT,1), JGCT=1,IGCT), &
                            (XGI(JGCT,2), JGCT=1,IGCT), &
                            XNI

              ! GROUP IDENTIFIERS END ; PARM LABELS START

              NUMEQ = 0
              DO IGRP = 1, MGI
                  NL = NUMPAR(IGRP)
                  IF (NL <= 0) THEN
                      CYCLE
                  END IF
                  IF (IPTLBL(IGRP) < -1) THEN
                     igrp0 = igrp
                     WRITE(IOUT6,*) 'VMATHD: BAD VALUE OF IPTLBL ', &
                                    'IN PARAM LABELS LOOP '
                     WRITE(IOUT6,*) 'VMATHD: IGRP = ', igrp0
                     WRITE(IOUT6,*) 'VMATHD: CHECK THAT ALL IPTLBL ', &
                                    'ELEMENTS WERE SET'
                     WRITE(IOUT6,*) 'VMATHD: AFTER 50 LOOP'
                     STOP
                  END IF
                  K = IPTLBL(IGRP) - 1
                  DO N = 1, NL
                      SCRTCH(NUMEQ+N) = PARML0(1,K+N)
                      SCRTCH(NUMEQ+N+NI) = PARML0(2,K+N)
                      SCRTCH(NUMEQ+N+NI+NI) = PARML0(3,K+N)
                  END DO
                  NUMEQ = NUMEQ + NL
              END DO
              IF (NUMEQ /= NI) THEN
                  WRITE(IOUT6,11040) IDSAT(ISATC)
                  WRITE(IOUT6,11041) NUMEQ, NI
                  WRITE(IOUT6,*) 'VMATHD: NUMEQ, NI ', NUMEQ, NI
                  STOP
              END IF
              WORD1 = 3*NI + 1
              NI3 = 3 * NI
              ! OUTPUT PARAMETER LABELS
              WRITE(IUNVMT) WORD1, (SCRTCH(IP),IP=1,NI3), WORDL
              ! INCREMENT THE PARAMETER LABELS THAT ARE SATELLITE DEPENDE
              IPTLBL(1) = IPTLBL(1) + NUMPAR(1)
              IPTLBL(2) = IPTLBL(2) + NUMPAR(2)
              IPTLBL(3) = IPTLBL(3) + NUMPAR(3)
              IPTLBL(4) = IPTLBL(4) + NUMPAR(4)
          END DO
      END DO

11000 FORMAT(' EXECUTION TERMINATING IN VMATHD CAN NOT GET WORD(5) ')
11001 FORMAT(' OF THE HEADER RECORD                                ')
11020 FORMAT(' EXECUTION TERMINATING IN VMATHD  NUMBER OF          ')
11021 FORMAT(' PARAMETER GROUPS LARGER THAN LOCAL ARRAYS           ')
11030 FORMAT(' EXECUTION TERMINATING IN VMATHD  NUMBER OF FORCE    ')
11031 FORMAT(' MODEL GROUPS LARGER THAN LOCAL ARRAYS               ')
11040 FORMAT(' EXECUTION TERMINATING IN VMATHD. SAT NO ',I10)
11041 FORMAT(' DISAGREEMENT IN COUNTING NO OF FORCE EQ ',I6,' VS ',I6)
      END SUBROUTINE
