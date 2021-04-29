!$RD50A1
       SUBROUTINE RD50A1
!********1*********2*********3*********4*********5*********6*********7**
! RD50A1           00/00/00            0000.0    PGMR - EDDY
!
! FUNCTION:       . DETERMINE START AND STOP TIME FOR RUN.
!                 . DETERMINE MAXIMUM NUMBER OF SATS PER ARC.
!                 . DETERMINE ATMOSPHERIC DENSITY MODEL TO BE USED.
!                 . DETERMINE ALLOCATION REQUIREMENTS FOR OPTIONS
!                   RELATING TO READING OF DATA SUCH AS:
!                         MBIAS,PBIAS
!                         SELECT/DELETE
!                         STATION POSITION
!                         SEA SURFACE TOPOGRAPHY
!                 . COUNT NUMBER OF POLEUT CARDS
!
!********1*********2*********3*********4*********5*********6*********7**
!
!  Modification by Zach Waldron  2/2/2021
!       - added MSIS00 to the IDEN list
!
!
      use  antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/ACCELR/NSATAC,NACCB,NSATDY,NDYNAC,NXACC,                   &
     &       MXACC,NXACCX,MXACCX,NXISSM,MXISSM,NXS1,MXS1,NXS2,MXS2,     &
     &       NXPPN,MXPPN,NXSBS,MXSBS,NSATHR,NFACCB(2,200),NFATIT(2,200),&
     &       MRAT,NBAPPD(200),NDYNPD(200),IACSAT(200),NACB(200),        &
     &       IDNSAT(200),NDYN(200)
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999),               &
     &              NANTMT(99), NNUMMT, NXAPHA
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/BINBUF/NBUFIN,NLENI ,MAXBUF,MAXOBS,NBUFOT,NLENO ,NMAXOT,   &
     &              KBUFOT,MXBLEN,MAXTDD,NLENDR,MOBSBK,MXTIMB,          &
     &              MXOBSB,IIOBDR,IPSMAX,ILIMBK,NPASS ,MVERSN,NXBINB
      COMMON/BURN  /NBGRP,NBMODE,NBURN
      COMMON/BWINTG/NMSLAB,NAREPT,NSPRPT,NDFRPT,NEMIPT,NTMAPT,NTMCPT,   &
     &      NTMDPT,NTMFPT,NTHXPT,MXSLAB,MXARPT,MXSRPT,MXDFPT,MXEMPT,    &
     &      MXTAPT,MXTCPT,MXTDPT,MXTFPT,MXTXPT,NXBWIN
      COMMON/CA1UTC/RID1,RID2,YMD1,YMD2,HMS1,HMS2,DAYS1,DAYS2,          &
     &              EPAUT1(6),EPAUT2(6)
      COMMON/CAMSAT/NCAM,NSCID(30),NXSCID
      COMMON/CASPER/NFGGLB,NFFGLB,NFGARC,NFFARC,NGGA,NGFA,NAGA,NAFA,    &
     &              NFANEP,NFDIR(4),NSATFA(2,200),NFANTM,NXCASP
      COMMON/CEGREL/LGRELE,LRLCLK,LGRELR,LXPCLK,LBPCLK,NXEGRL
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CGVTMI/NCDT,NSDT,NCPD,NSPD,NCDTA,NSDTA,NCPDA,NSPDA,NGRVTM, &
     &       KCDT,KCPDA,KCPDB,KSDT,KSPDA,KSPDB,KOMGC,KOMGS,KCOMGC,      &
     &       KSOMGC,KCOMGS,KSOMGS,NXGVTI
      COMMON/CITERG/MARC  ,MGLOBL,MGLOBM,                               &
     &              MGLOCV,NXITRG
      COMMON/CITERM/MAXINR,MININR,MAXLST,IHYPSW,NXITER
      COMMON/CLGEOP/LGEOPL,LGPLOR,NXCLGP
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CLNORM/LNRMGI,NXNORM
      COMMON/CLVIEW/LNPRNT(18),NXCLVI
      COMMON/CLSSTC/LSTOPO,LSTPRD,LSSCOF,LSTTIM
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CSTIER/LSTIER
      COMMON/CTIDES/NTIDE ,NSTADJ,NET   ,NETADJ,NETUN ,NOT   ,NOTADJ,   &
     &              NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3), &
     &              ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY,XCONST
      COMMON/CONTRL/LSIMDT,LOBS  ,LORB  ,LNOADJ,LNADJL,LORFST,LORLST,   &
     &              LORALL,LORBOB,LOBFST,LOBLST,LOBALL,LPREPO,LORBVX,   &
     &              LORBVK,LACC3D,LSDATA,LCUTOT,LACCEL,LDYNAC,LFRCAT,   &
     &              LNIAU ,NXCONT
      COMMON/COPNUM/NGLTTL,NGLBOP,NSTAGO,NARTTL,NARCMN,NARCOP,NSELDL,   &
     &              NDXSTA,NDXNDG,NDXNDS,NDXDAT,NDXNDA,NDXEND,NDXALL,   &
     &              NDXCYB,NDXIBM,NDXPUT,NDXPVU,NDXTVU,NDXQUA,NDXLGR,   &
     &              NDXANO,NDXSDN,NDXGHT,NDXGRD,NDXGRE,NDXBRN,NDXFLO,   &
     &              NDXACC
      COMMON/CORBNF/MAXINF,NORBNF,INFSTA(15),INFNDX(15),INFSTR,INFSTP,  &
     &              INFRT ,NXCORB
      COMMON/COREQ /NCHRLY,MAXUTC,MAXUT1,MAXFLX,MAXDSC,KORSUM(3),       &
     &              KORSM2(3),NXCORQ
      COMMON/CPARTI/MAXLP ,KORSIZ,MAXWRK,                               &
     &              IUNTP1,IUNTP2,IUNTPF,ISIZPF,IRECPF,NRECPF,          &
     &              NPDONE,NP1   ,NP2   ,NPAR1 ,NPARTL,NXPARI
      COMMON/CPARTL/LPART ,LPARTI,NXPARL
      COMMON/DEFVAL/DEFGM(999),DEFSMA(999),DEFFLT(999),DEFTID(3,999),  &
     &              DEFLOV(2,999),DEFDRG,DEFVLT,DEFTDN(10,2)
      COMMON/DELAY /NTDL
      COMMON/DSTAT /NSATDS,NFDSTA(200),NXDELT(2,200)
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/DWNWGT/LDWNWT,LDNWMT(999),NXDWNW
      COMMON/EXTAGL/LEXTHC,NXTAGL
      COMMON/EXATGL/LEXATT,NXEAGL
      COMMON/FANTOM/LFGLB,LFARC,LFTARC,LFTGLB,LENDGL,LPHNTM(4),LPHTIM(4)&
     &             ,LXTRAF,LSATFA(4,3),NXFANT
      COMMON/GCMMOD/GCMNPS,GCMT0,GCMPER(10),GCMAMP(30),GCMPHA(30),      &
     &              XXGCM
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/GEOMCS/MDEGEO,MORGEO,NPGEOM,NXGEOM
      COMMON/GLBPAR/NARCS,NDPOLE,NDCSA,MXDEGG,MXORDG,NSRA,              &
     &              NDUMMY,MAXPAS,NXGLBP
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      COMMON/GRANI/NSUBLG,NXGRAN
      COMMON/IDELTX/MXSTAT,NEVENTS,NDSTAT,IDSYST,NDELTX
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988),  &
     &              NXEPH2
      COMMON/GPSINT/IGPSBW,NGPSBW
      COMMON/INTFLD/IFIELD(18),IC78,IC910,IC1112,IC1314,IC2122,IC2324,  &
     &              IC1517,IC1820,IC710,IC1114,IC2124,IC1824,IC714,     &
     &              NXINTF
      COMMON/INVRSA/EPSIN1,EPSIN2,TOLRNC,XINVRS
      COMMON/INVRSL/LSYMSW,LMATP,NXINVR
      COMMON/ION2EL/LIONC,LFRQOP,lFRQOF,NXIONL
      COMMON/IPLMOD/NFMOD,NSMOD,NFTOT,NSTOT,IDFMOD(1000),IDSMOD(1000), &
     &             NFPARAM(1000),&
     &             NPMIND(1000),NSPARAM(1000),NPSIND(1000),NXPLMD
      COMMON/KNTSTS/KNTACC(9,40),MKNTAC,KNTDRG,KNTSLR,MAXNGA,MAXNDR,    &
     &              MAXNSR,NXKNTS
      COMMON/LAPLOD/LAPL,NXLAPL
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
      COMMON/LDUAL/LASTSN
      COMMON/LICASE/LINT(2),LCASE(18),NXLCAS
      COMMON/LSWTCH/LFLXPR,LMALL,LPGRAV,LST50,LST50I,LTERMV,LST17,      &
     &              NXLSWT
      COMMON/MBIAS /NBIASK,NBIASA,NBIASG,NBCDP,NBCDT,                   &
     &              NUMPAS,NPSFIL,NARCBS,NEBKNT,NXMBIA
      COMMON/MBIAST/MBTRAD,MBTRUN,ICONBIA,IPRCTP,IPREND,NXMBST
      COMMON/MBSGLB/MBIASK,MBIASA,MBIASG,MBCDP,MBCDT,MARCBS,            &
     &              NBIST2,MEBKNT,NBIAST,NXMBGB
      COMMON/NEQINT/NINTOT,MEMTOT,NEQNIM,NXNEQS
      COMMON/MULTDM/MLTDEM,MEMDEM,NXMDEM
      COMMON/NLOCGR/INTEGG,IOPTLG,ICUTLG,MAXDO,NLOCAL,NLATLG,NLONLG,    &
     &              NLOCGU, NLOCGA, NXLOCG
      COMMON/NUTINF/NNUT,NNUTAA,NXNUTA
      COMMON/OFACRD/XNOFFA,CRDOFA(10,500)
      COMMON/OFFADJ/LADJOF
      COMMON/OFFSIG/SIGOFF(3,500)
      COMMON/OFFSTL/LOFFST
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/OLOADA/LXYZCO,LEOPTO
      COMMON/PLNRAJ/LPLNRA
      COMMON/PMARS /DTM74 ,XPMARS
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/RAMPR/LRAMPR
      COMMON/RLOCGR/RLATLG,RLONLG,GAMMA,HANOM,XRLOC
      COMMON/SATCUT/CUTSAT,XSATCU
      COMMON/SATPIN/MAXDS,NGSAT1,NXSATP
      COMMON/SOLTUM/INDTUM,NDTUM,ITUMSC(32),IFLAG1,IFLAG3,NXSOLT
      COMMON/SPBIAS/NCTSTA,NXSPB
      COMMON/SHIRTS/LNSNS,NXNSNS
      COMMON/SSLIST/MXBAS,MXSTA,MXSAT,MXBSTA,MXSTV,MXQUA,NXSSLI
      COMMON/SSTCOM/NCBAR, NSBAR ,NCDOT ,NSDOT ,NCAPRD,NCBPRD,NSAPRD,   &
     &              NSBPRD,NCBARA,NSBARA,NCDOTA,NSDOTA,NCAPDA,NCBPDA,   &
     &              NSAPDA,NSBPDA,ISSTEP,NXSST
      COMMON/TETHER/LTETHR,NXTETH
      COMMON/TIMDAT/KYMD,KHMS,ICPU,IO,IRUNB,IRUNE,ITBLB,ITBLE,IARCT1,   &
     &              IARCT2,IDATAB,IDATAE,NXTIME
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/TRPOUT/LTROUT,NXLTPO
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      COMMON/XPCNT2/NXTRAC2,NXFLAG2,NXALLC2,NCPPER2,NMINRT2,NDPOR2,    &
     & NXXTR2
      COMMON/UNITS /IUNT01,IUNT02,IUNT03,IUNT05,IUNT06,IUNT07,IUNT11,   &
     &              IUNT12,IUNT15,IUNT16,                               &
     &              IUNT30,IUNT31,IUNT39,IUNT40,IUNT41,IUNT42,IUNT43,   &
     &              IUNT50,IUNT52,IUNT53,IUNT60,IUNT90,IUNT91,IUNT99,   &
     &              ILINE(2),IPAGE(2),MLINEP(2),MAXCOL(2),IUNT17,       &
     &              IUNT70,IUNT71,                                      &
     &              IUNT88,NXUNIT
      COMMON/VLBI/NQUA,NADJQ,NADJQV,NQUAB,IYMDV,NVOPT,NXVLBI
      COMMON/XTRPLN/LXTRAP
      COMMON/YARSCH/YARTYP,YARMOD,AYAR,TAU,RISYEN,FSEYEN,RISYEX,FSEYEX, &
     &              SMEANM,XYARMD
      COMMON/LSFSHD/LSLFSH, NXSLFS
      COMMON/SLFSHD/NSHSAT,IDSASH(200),NSPANL(200),IFLGSH(200),        &
     &              IATYSH(200),NSHRCD(200),MNSHPL,NXSFSD
      COMMON/SSHDFI/LBISHF
      COMMON/EXCGCR/NEXCG,IDEXCG(200),IFTPCG(200),NRCDCG(200),NXEXCG
      COMMON/SLRATL/LRATIO,NXSLRL
!
      CHARACTER*99 CARDG
      CHARACTER*80 CARDH
      CHARACTER(8)      :: crd2

      DIMENSION IDEN(6)
      DIMENSION STACDS(14),FIELD(4),FA3(3)
      DIMENSION BUFIN(200,10,11)
      DIMENSION MASPTR(2,4)
      DIMENSION ANTDUM(1)
      DIMENSION IDUM(1)
      DIMENSION CARDAR(10)
!
      DIMENSION TOPTEM(9)

      DIMENSION ICFMOD(1000)
      DIMENSION ICSMOD(1000)
!
      DATA NOPTS/109/
      DIMENSION OPTION(109)
      DATA ZERO/0.D0/
      DATA NCRDUM/0/
      CHARACTER(8)      :: CDNAME
      CHARACTER(8)      :: OPTION
!

      DATA OPTION/'ATMDEN','DATA  ','DRGTAB','EBIAS ','EBIAS1',         &
     &            'EBIAS2','EBIAS3','ENDARC','ENDGLB','MBIAS ',         &
     &            'MBIAS1','MBIAS2','MBIAS3','PBIAS ','PBIAS1',         &
     &            'PBIAS2','PBIAS3','REFSYS','SATPAR','SOLTAB',         &
     &            'STAPOS','VLIGHT','LOCALG','POLEUT','ATITUD',         &
     &            'VECOPT','ORBINF','TOLS  ','PRNTVU','GEOPOL',         &
     &            'EMATRX','INVERT','OLOAD ','EARTH ','PLANET',         &
     &            'NORMPT','SIMDAT','REL300','GRVTIM','SSCOEF',         &
     &            'SSTOPO','SSTPRD','SSTTIM','TRKBDY','INTCOM',         &
     &            'QUAPOS','PANEL ','PLNEPH','EBIASM','MBIASM',         &
     &            'PBIASM','FANTOM','FANTIM','TETHER','IONCOR',         &
     &            'PLANOR','CON9PA','CONDRG','CONSOL','TOPLOV',         &
     &            'TSTLOV','SATCUT','TRPOUT','OFFADJ','NONSNS',         &
     &            'OFFSET','ANTPHC','YAWBIA','YAWPOL','GDYNEP',         &
     &            'EPHEM2','SCBODY','ACCBIA','DYNSEL','HRATEF',         &
     &            'CONBIA','DSTATE','GPSMOD','OTIDES','ETIDES',         &
     &            'APLOAD','STIERS','YARKOV','OPVLBI','VLBSTR',         &
     &            'ENDOPV','CONSPL','STEP  ','SLFSHD','CGMASS',         &
     &            'TOPATT','NUTATE','INTGCB','SOLRAD','GPSBXW',         &
     &            'CAMERA','TUMSOL','ANTPH2','ELEVWT','GCMMOD',         &
     &            'BINAST','G2COFC','G2COFS' ,                          &
     &            'MLTDEM','RELPPN','PLNGLF','PLNSRF','ADELAY','FNBURN'/
      CHARACTER(8)      :: BLNK
      DATA LGLOBL/.TRUE./,BLNK/'      '/
! SUBGROUP "SELECT/DELETE" CARDS
      CHARACTER(8)      :: XMETDT,DELETE,ENDARC
      DATA XMETDT,DELETE,ENDARC/'METDAT','DELETE','ENDARC'/
! SUBGROUP "DRGTAB" AND "SOLTAB" CARDS
      CHARACTER(8)      :: FIN
      DATA FIN/'FIN   '/
! REFSYS CARDS
      CHARACTER(8)      :: REFSYS,SATPAR,EPOCH,ELEMS1,ELEMS2
      DATA REFSYS,SATPAR,EPOCH,ELEMS1,ELEMS2/'REFSYS','SATPAR',         &
     &     'EPOCH ','ELEMS1','ELEMS2'/
      CHARACTER(8)      :: SLAVE
      DATA SLAVE/'SLAVE '/
! END GLOBAL CARD
      CHARACTER(8)      :: ENDGLB
      DATA ENDGLB/'ENDGLB'/
! SUBGROUP "STAPOS" CARDS
      DATA NSTACD/14/
      CHARACTER(8)      :: STACDS
      DATA STACDS/'ADJUSTED','CORREL  ','CONSTADJ','CONSTEND',          &
     &            'FIXED   ','GEODETIC','EXTRAGEO','ELCUTOFF',          &
     &            'INSTRMNT','STAVEL  ','TIMVEL  ','SIGVEL  ',          &
     &            'STATL2  ','STATH2  '/
      CHARACTER(8)      :: ENDSTA
      DATA ENDSTA/'ENDSTA'/
      CHARACTER(8)      :: ENDQUA
      DATA ENDQUA/'ENDQUA  '/
! "BIAS" CARDS
      CHARACTER(8)      :: BIAS2 , BIAS3
      CHARACTER(8)      :: PBIAS2 , PBIAS3
      CHARACTER(8)      :: EBIAS2,EBIAS3
      DATA BIAS2 , BIAS3/'MBIAS2','MBIAS3'/
      DATA PBIAS2,PBIAS3/'PBIAS2','PBIAS3'/
      DATA EBIAS2,EBIAS3/'EBIAS2','EBIAS3'/
! DENSITY MODELS
      DATA IDEN/65,71,77,87,86,92/,NDMODL/6/
! VLIGHT
      DATA CTOL/1.0D2/
! CONSTANTS
      DATA C0/0.0D0/,EPS/1.D-4/
      DATA HDR/-9000000./
      DATA HDRTMG/-9999999./
      DATA ICARD/0/
      DATA TGDN0/2430000.5D0/
      EQUIVALENCE(CARDH,CARDAR(1))

      INTEGER :: num_antphc
      INTEGER :: num_antph2
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
! INITIALIZE GLOBAL VARIABLES
!
      do i=1,1000
      ICFMOD(i)=0
      ICSMOD(i)=0
      enddo

      num_antphc = 0
      num_antph2 = 0

      NX=0
      NEVENTS=0
      ISIM=0
      MAXSTA=50
      NMBGLB=0
      MXSTA =0
      NBIASG=0
      MXSAT =0
      NSTLVS=0
      TGTMD =0.D0
      VLIGHT=DEFVLT
      LNPT14=.FALSE.
      LVOP14=.FALSE.
! TROP BIAS COUNTERS
      MBTRAD=0
      MBTRUN=0
! MAXIMUM BIAS VALUES
      MEBKNT=0
      MBIASK=0
      MBIASG=0
      MBIASA=0
      MBCDP =0
      MBCDT =0
! MAXIMUM BOX-WING PARAMETER VALUES
      MXARPT=0
      MXSRPT=0
      MXDRPT=0
      MXEMPT=0
      MXTAPT=0
      MXTCPT=0
      MXTDPT=0
      MXTFPT=0
      MXTXPT=0
  200 CONTINUE
!
! INITIALIZE ARC VARIABLES
!
      NSEL=0
      NDEL=0
      NMETDT=0
      NUMSAT=0
      NBIASA=0
     
      NBCDT=0
      NBIASK=0
      NEBKNT=0
      IGPSB=0
  400 CONTINUE
      IF(LGLOBL) GO TO 500
!
! READ ARC TITLE CARDS
!
      DO 410 I=1,NARTTL
      READ(IUNT50,71100,END=420)
  410 END DO
! ARC SET TITLE CARDS MUST BE FOLLOWED BY REFSYS CARD
      GO TO 9000
! END OF FILE; IF GLOBAL SET-ERROR; IF ARC SET- NORMAL END OF JOB
  420 IF(LGLOBL) GO TO 60100
      GO TO 60000
  500 CONTINUE
!
! READ GLOBAL TITLE CARDS
!
      DO 550 I=1,3
      READ(IUNT50,71100,END=420)
  550 END DO
!
      IDSGA9=0
      IDGA9=0
! READ OPTION CARDS
!
  600 CONTINUE
      READ(IUNT50,71001,END=60000) CARDH
      READ(CARDH,71000,END=60000) CDNAME,IC78,IC910,IC1112,IC1314,      &
     &   IC1517,FA3,IC2122,IC2324,FIELD
!      write(6,'(1x,a,1x,a)')'RD50A1:600 CDNAME ', CDNAME
      MAXDSA = IC1517
      CALL A3TOI6(FA3,I6,IC1820)
! SET INTEGER FIELD COLUMNS
      CALL MAKFLD
!
! TEST FOR OPTION CARDS
!
      DO 700 I=1,NOPTS
      ICARD=I
      IF(CDNAME.EQ.OPTION(I)) GO TO                                     &
     &   ( 1500,10500, 3000, 4500, 4500, 4500, 4500, 4000, 5000, 7000,  &
     &     7000, 7000, 7000, 8000, 8000, 8000, 8000, 9000,10000,11000,  &
     &    12000,13000, 6000, 8200, 1400,13300, 7200,12395,8300 ,5100 ,  &
     &     3500, 5500, 7100, 3200, 8100,7050,10560,10200,5300,12100,    &
     &    12200,12300,12350,12500, 5550, 8500, 7500, 8110, 4600, 7040,  &
     &     8050, 5050, 5040,12400, 5510, 8105, 2000, 2020, 2040,2050,   &
     &     2050,10300,12700, 7060, 7045, 7070, 1300,13400,13500,5075,   &
     &     5010,10400,1200,3100,5460,1900,3050,5200,7300,5020,          &
     &     1350,12394,13350,7110,7110,7110,1950,12070,10700,1700,12450, &
     &     7777, 5465,13600,5150,1600,12800, 1310, 1320, 5070, 1550 ,   &
     &     5060,5065,7041,8600,8112,8114,1250,5055)I
  700 END DO
! READ NEXT CARD
      GO TO 600
!*********************************************************************
! ** ACCBIA **                                               ACCBIA **
!*********************************************************************
 1200 CONTINUE
      NACCB=NACCB+1
! READ AND CHECK SECOND ACCBIA CARD
      READ(IUNT50,71001,END=60000) CARDH
      READ(CARDH,71000,END=60000) CDNAME,IC78,IC910,IC1112,IC1314,      &
     &   IC1517,FA3,IC2122,IC2324,FIELD
      IF(CDNAME.NE.'ACCBI2') THEN
      WRITE(6,1201)
 1201 FORMAT(5X,'CARD ',A8,' DOES NOT BELONG IN ACCLRM SUBGROUP')
      WRITE(6,1202)
 1202 FORMAT(5X,'EXECUTION TERMINATING IN RD50A1 ')
      ENDIF
      NSATAC=NUMSAT
      GOTO 600
!*********************************************************************
! ** ADELAY **                                               ADELAY **
!*********************************************************************
 1250 CONTINUE
      NTDL=NTDL+1
      GOTO 600
!*********************************************************************
! ** ANTPHC **                                               ANTPHC **
!*********************************************************************
 1300 CONTINUE

      L_old_antphc = .TRUE.

      write(6,*) 'rd50a1: num_antphc = ', num_antphc

      if( num_antphc == 0 ) then

          write(6,'(/A)') 'rd50a1: call rdantp for ANT '

          CALL RDANTP('ant', ANTDUM,ANTDUM,ANTDUM,IDUM)

          write(6,'(A/)') 'rd50a1: aft all calls to rdantp'
      endif

      num_antphc = num_antphc + 1

      write(6,*) 'rd50a1:2 num_antphc = ', num_antphc


      IF( NNUMMT.GT.0 ) THEN
          NNUMMT=NNUMMT+1
          NANTMT(NNUMMT)=IC1517
          GOTO 600
      ENDIF
      NNUMMT=NNUMMT+1
      NANTMT(NNUMMT)=IC1517
      !!!!!!!CALL RDANTP('ant', ANTDUM,ANTDUM,ANTDUM,IDUM )

      GOTO 600
!*********************************************************************
! ** ANTPH2 **                                               ANTPH2 **
!*********************************************************************
 1310 CONTINUE

      L_old_antphc = .FALSE.

      write(6,*) 'rd50a1: num_antph2 = ', num_antph2

      if( num_antph2 == 0 ) then

          write(6,'(/A)') 'rd50a1: call rdantp for SAT for card ANTPH2'

          CALL RDANTP('sat', ANTDUM,ANTDUM,ANTDUM,IDUM)

          write(6,'(/A)') 'rd50a1: call rdantp for STA for card ANTPH2'

          CALL RDANTP('sta', ANTDUM,ANTDUM,ANTDUM,IDUM)

          !write(6,'(/A)') 'rd50a1: call rdantp for ANT for card ANTPH2 '
          !CALL RDANTP('ant', ANTDUM,ANTDUM,ANTDUM,IDUM)

          write(6,'(A/)') 'rd50a1: aft all calls to rdantp'
      endif

      num_antph2 = num_antph2 + 1

      write(6,*) 'rd50a1:2 num_antph2 = ', num_antph2

      GOTO 600
!*********************************************************************
! ** APLOAD **                                               APLOAD **
!*********************************************************************
 1350 CONTINUE
      LAPL=.TRUE.
      GOTO 600
!*********************************************************************
! ** ATITUD **                                               ATITUD **
!*********************************************************************
 1400 CONTINUE
!     IF(IFIELD(2) .EQ.0) GOTO 600
! SKIP ATITUD PARAMETER CARDS AS THEY ARE IN A DIFFERENT FORMAT
!     IC8   =IFIELD( 2)
!     DO 1410 IX=1,IC8
!     READ(IUNT50,71100)
!1410 CONTINUE
      GOTO 600
!*********************************************************************
! ** ATMDEN **                                             * ATMDEN **
!*********************************************************************
 1500 CONTINUE
      IATDEN=20
      IC15=IFIELD(9)
      IC16=IFIELD(10)
!     ....IDRAGM selects atmosphere models for Venus and Mars
      IDRAGM=IC1112
!      IF(IC1112.EQ.0.OR.IC1112.GT.4) IDRAGM=1
      IF(IC1112.EQ.0.OR.IC1112.GT.5) IDRAGM=1
      DO 1505 I=1,NDMODL
      IF(IC910.EQ.IDEN(I)) THEN
      IATDEN=I*10
      GOTO 1506
      ENDIF
 1505 END DO
 1506 CONTINUE
      IF(IC910.EQ.IDEN(4)) THEN
        IATDEN=42
        IF((IC15.EQ.1).AND.(IC16.EQ.1)) IATDEN=41
        IF((IC15.EQ.1).AND.(IC16.EQ.0)) IATDEN=43
      ELSEIF(IC910.EQ.IDEN(5))THEN
        IATDEN=52
        IF((IC15.EQ.1).AND.(IC16.EQ.1)) IATDEN=51
        IF((IC15.EQ.1).AND.(IC16.EQ.0)) IATDEN=53
      ELSEIF(IC910.EQ.IDEN(6))THEN
        WRITE(6,*) 'CHECK RD50A1: made it to msis2 selection'
        IATDEN=62
        IF((IC15.EQ.1).AND.(IC16.EQ.1)) IATDEN=61
        IF((IC15.EQ.1).AND.(IC16.EQ.0)) IATDEN=63
      ENDIF
      IF(IDRAGM.EQ.4) IATDEN=42
      IF(IDRAGM.EQ.5) IATDEN=42
      DTM74=FIELD(2)
      GO TO 600
!*********************************************************************
! ** BINAST **                                             * BINAST **
!*********************************************************************
 1550 CONTINUE
      LBINAST=.TRUE.
! THE BINAST OPTION SHOULD CONTAIN THE GM AND THE SMA FOR THE SECOND BODY
      IF(FIELD(1).NE.0.D0.AND.FIELD(3).NE.0.D0) GO TO 1551
      IF(FIELD(1).EQ.0.D0) WRITE(6,*)' SPECIFY GM FOR BINARY STAR'
      IF(FIELD(3).EQ.0.D0) &
     & WRITE(6,*)' SPECIFY SEMI MAJOR AXIS FOR BINARY STAR'
        STOP
 1551 CONTINUE
      GO TO 600
!*********************************************************************
! ** CAMERA **                                             * CAMERA **
!*********************************************************************
 1600 CONTINUE
! COUNT GROUPS OF CAMERA PARAMETERS
      IF(IFIELD(1).EQ.1) THEN
      NCAM=NCAM+1
      NSCID(NCAM)=IC1824
      ENDIF
      GO TO 600
!*********************************************************************
! ** CGMASS **                                             * CGMASS **
!*********************************************************************
 1700 CONTINUE
! IFIELD(9) = col. 15  = flag for external cgmass file
!           = 0 -- no file
!           = 1 -- ASCII file
!           = 2 -- binary file
      IF(IFIELD(9).GT.0) THEN
! FIND IF THE SATELLITE HAS BEEN SETUP FOR EXTERNAL CGMASS COORDINATES
         CALL FNDNUM(IC1824,IDEXCG,NEXCG,IRET)
         IF(IRET.EQ.0) THEN
! IF THE ID HAS NOT BEEN FOUND, SAVE IT
           NEXCG=NEXCG+1
           IFTPCG(NEXCG)=IFIELD(9)
           IDEXCG(NEXCG)=IC1824
!        print*, 'dbg excg: ',nexcg,iftpcg(nexcg),idexcg(nexcg)
         ELSE
! IF THE ID HAS BEEN FOUND, CHECK FOR ERROR
           IF(IFIELD(9).NE.IFTPCG(IRET)) THEN
             WRITE(6,*) 'INCONSISTENCY IN EXTERNAL CGMASS FILE TYPE ',  &
     &         'REQUIREMENT FOR SATELLITE ', IC1824
             STOP
           ENDIF
         ENDIF
      ENDIF
      GO TO 600
!*********************************************************************
! ** CONBIA **                                             * CONBIA **
!*********************************************************************
 1900 CONTINUE
! COUNT CONBIA CARDS FOR TROPOSPHERIC BIASES
      IF(IC1517.NE.500) THEN
      WRITE(6,*)' ONLY TROPOSPHERIC BIASES CAN BE CONSTRAINED TYPE=500'
      STOP 16
      ENDIF
      ICONBIA=ICONBIA+1
      GO TO 600
!*********************************************************************
! ** CONSPL **                                             * CONSPL **
!*********************************************************************
 1950 CONTINUE
      NCTSTA=NCTSTA+1
      GO TO 600
!*********************************************************************
! ** CON9PA **                                             * CON9PA **
!*********************************************************************
 2000 CONTINUE
      IF(IDSGA9.NE.IC1824) IDGA9=IDGA9+1
! COUNT SETS OF CONTINUOUS TIME PERIOD CONSTRAINT CARDS
      IDIR=IC78/10
      ICON=IC78-(IDIR*10)
      JJ=-1
      DO 2010 I=1,3
      DO 2009 J=1,3
      IF(IDIR.EQ.I.AND.ICON.EQ.J) THEN
      KNTACC(IDIR+ICON+JJ,IDGA9)=KNTACC(IDIR+ICON+JJ,IDGA9)+1
      MKNTAC=MAX(MKNTAC,KNTACC(IDIR+ICON+JJ,IDGA9))
      ENDIF
 2009 END DO
      JJ=JJ+2
 2010 END DO
      IDSGA9=IC1824
      GO TO 600
!*********************************************************************
! ** CONDRG **                                             * CONDRG **
!*********************************************************************
 2020 CONTINUE
! COUNT SETS OF CONTINUOUS TIME PERIOD CONSTRAINT CARDS
      KNTDRG=KNTDRG+1
      GO TO 600
!*********************************************************************
! ** CONSOL **                                             * CONSOL **
!*********************************************************************
 2040 CONTINUE
! COUNT SETS OF CONTINUOUS TIME PERIOD CONSTRAINT CARDS
      KNTSLR=KNTSLR+1
      GO TO 600
!*********************************************************************
! ** TOPLOV(TSTLOV)                                        * CONSOL **
!*********************************************************************
 2050 CONTINUE
! COUNT THE NUMBER OF TOPLOV(TSTLOV) CARDS
      NSTLVS=NSTLVS+1
      GO TO 600
!*********************************************************************
! ** DRGTAB **                                             * DRGTAB **
!*********************************************************************
 3000 READ(IUNT50,71100,END=60100) CDNAME
      IF(CDNAME.NE.FIN) GO TO 3000
      GO TO 600
!*********************************************************************
! ** DSTATE **                                             * DSTATE **
!*********************************************************************
 3050 CONTINUE
      ISATID=IC1820*10000+IC2122*100+IC2324
! IS THE SATELLITE THE SAME AS THE PREVIOUS DSTATE CARD?
      IF(IDPREV.NE.ISATID) THEN
      TPREV=-99999.D0
      NX=0
      NSATDS=NSATDS+1
      NXDELT(1,NSATDS)=ISATID
      ENDIF
      IF(TPREV.NE.FIELD(2)) THEN
      NX=NX+1
      NXDELT(2,NSATDS)=NX
      NEVENTS=NEVENTS+1
      ENDIF
      MXSTAT=MAX(MXSTAT,NX)
      IDPREV=ISATID
      TPREV=FIELD(2)
      GOTO 600
!*********************************************************************
! ** DYNSEL **                                             * DYNSEL **
!*********************************************************************
 3100 CONTINUE
      LDYNAC=.TRUE.
      NDYNAC=NDYNAC+1
      NSATDY=NUMSAT
      GO TO 600
!*********************************************************************
! ** EARTH **                                              * EARTH  **
!*********************************************************************
 3200 CONTINUE
      IF(IC1517.GT.0) CALL BDPT(300,1)
      IF(IC1820.GT.0) CALL BDPT(300,2)
      GO TO 600
!*********************************************************************
! ** ELEVWT **                                             * ELEVWT **
!*********************************************************************
 1320 CONTINUE
      IF(IC1112.EQ.99) THEN
        LDWNWT=.TRUE.
        LDNWMT(IC1517)=.TRUE.
      ENDIF
      GO TO 600
!*********************************************************************
! ** EMATRX **                                             * EMATRX **
!*********************************************************************
 3500 CONTINUE
! SET VECOPT OPTIONS (WHICH CAN BE OVERWRITTEN IN RD50A2)
      LNPNM=.TRUE.
      LPART=.TRUE.
! IF THE NORMPT CARD AND THE VECOPT CARD DID NOT SET MAXOBS THEN SET
! IT HERE TO 40
      IF((.NOT.LNPT14).AND.(.NOT.LVOP14)) MAXOBS=40
      GO TO 600
!*********************************************************************
! ** ENDARC **                                             * ENDARC **
!*********************************************************************
 4000 NARCS=NARCS+1
      MARC=NARCS
! DETERMINE MAXIMUM NUMBER FOR VARIOUS ARC OPTIONS
      MAXSEL= MAX(MAXSEL,NSEL)
      MAXDEL= MAX(MAXDEL,NDEL)
      MAXMET= MAX(MAXMET,NMETDT)
      MXSAT= MAX(MXSAT,NUMSAT)
      MSATA=MXSAT
      MXPLT= MAX(MXPLT,NUMPLT)
      NUMPLT=0
! DETERMINE MAXIMUM NUMBER FOR TOPEX OPTIONS
      MXSLAB= MAX(MXSLAB,NMSLAB)
      MXARPT= MAX(MXARPT,NAREPT)
      MXSRPT= MAX(MXSRPT,NSPRPT)
      MXDFPT= MAX(MXDFPT,NDFRPT)
      MXEMPT= MAX(MXEMPT,NEMIPT)
      MXTAPT= MAX(MXTAPT,NTMAPT)
      MXTCPT= MAX(MXTCPT,NTMCPT)
      MXTDPT= MAX(MXTDPT,NTMDPT)
      MXTFPT= MAX(MXTFPT,NTMFPT)
      MXTXPT= MAX(MXTXPT,NTHXPT)
! DETERMINE MAXIMUM NUMBER FOR BIAS ARC OPTIONS
      MBIASA= MAX(MBIASA,NBIASA)
      MBIASK= MAX(MBIASK,NBIASK)
      MBCDT = MAX(MBCDT,NBCDT)
      MBCDP = MAX(MBCDP,NBCDP)
      MEBKNT= MAX(MEBKNT,NEBKNT)
      GO TO 200
!*********************************************************************
!** EBIAS  **                                              * EBIAS  **
!*********************************************************************
 4500 CONTINUE
      IF(CDNAME.EQ.EBIAS2) GOTO 600
      IF(CDNAME.EQ.EBIAS3) GOTO 600
      LEBIA=.TRUE.
      IF(LSIMDT.AND.LSDATA)THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      NEBKNT=NEBKNT+1
      GOTO 600
!*********************************************************************
!** EBIASM  **                                             * EBIASM **
!*********************************************************************
 4600 CONTINUE
      LEBIA=.TRUE.
      IF(LSIMDT.AND.LSDATA)THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      GOTO 600
!*********************************************************************
!** ENDGLB **                                              * ENDGLB **
!*********************************************************************
 5000 LGLOBL=.FALSE.
!     write(6,*) 'rd50a1: below 5000 -- read endglb'
      MBIASG= MAX(MBIASG,NBIASG)
      MAXDSC= MAX(MAXDSC,MXBSTA*2 )
      LENDGL=.TRUE.
      IF(NLOCAL.GT.0) THEN
        INQUIRE(FILE='SUBBLK',EXIST=LEXIST)
        IF(.NOT.LEXIST) GO TO 60800
        OPEN(UNIT=23,FILE='SUBBLK',FORM='FORMATTED',STATUS='OLD')
        NSUBLG=0
 5005   CONTINUE
        READ(23,71401,END=5006) CARDG
        NSUBLG=NSUBLG+1
        IF(NSUBLG.EQ.1) THEN
          READ(CARDG(75:99),71402) TESTO
          IF(TESTO.LT.0.D0) IOPTLG=3
        ENDIF
        GO TO 5005
 5006   CONTINUE
        CLOSE(23)
        IF(NSUBLG.LT.NLOCAL) GO TO 60900
      ENDIF
      GO TO 400
!*********************************************************************
! ** EPHEM2 **                                             * EPHEM2 **
!*********************************************************************
 5010 CONTINUE
      ISUPL=1
      IC7=IC78/10
      IC8=IC78-IC7*10
      ICBODY=IC7
      ISEQB(1)=IC8
      IC9=IC910/10
      ISEQB(2)=IC9
      IF(IC7.EQ.0) THEN
      ICBODY=IC1517
      ENDIF
      GOTO 600
!*********************************************************************
! ** ETIDES **                                             * ETIDES **
!*********************************************************************
 5020 CONTINUE
      NTIDE=NTIDE+1
      GO TO 600
!*********************************************************************
! ** FANTIM **                                             * FANTIM **
!*********************************************************************
 5040 CONTINUE
      IF(FIELD(1).EQ.ZERO.AND.FIELD(2).EQ.ZERO.AND.FIELD(3).EQ.ZERO     &
     &.AND.FIELD(4).EQ.ZERO) THEN
      WRITE(6,83000)
      STOP
      ENDIF
      IC7=(IC78/10)+0.9D0
      IC8=IC78-IC7
      IF(LENDGL) THEN
! ARC FANTOM
      LFTARC=.TRUE.
      ELSE
! GLOBAL FANTOM
      LFTGLB=.TRUE.
      ENDIF
      IF(.NOT.LFTARC.AND.LFTGLB) THEN
      IF(IC7.EQ.1) THEN
      LPHTIM(1)=.TRUE.
      NFDIR(1)= MAX(NFDIR(1),IC8)
      ENDIF
      IF(IC7.EQ.2) THEN
      LPHTIM(2)=.TRUE.
      NFDIR(2)= MAX(NFDIR(2),IC8)
      ENDIF
      ELSE
      IF(IC7.EQ.1) THEN
      LPHTIM(3)=.TRUE.
      NFDIR(3)= MAX(NFDIR(3),IC8)
      ENDIF
      IF(IC7.EQ.2) THEN
      LPHTIM(4)=.TRUE.
      NFDIR(4)= MAX(NFDIR(4),IC8)
      ENDIF
      ENDIF
      GO TO 600
!*********************************************************************
! ** FANTOM **                                             * FANTOM **
!*********************************************************************
 5050 CONTINUE
      IC7=(IC78/10)+0.9D0
      IC8=IC78-IC7
      IF(LENDGL) THEN
      LFARC=.TRUE.
      ELSE
      LFGLB=.TRUE.
      ENDIF
      IF(.NOT.LFARC.AND.LFGLB) THEN
      IF(IC7.EQ.1) THEN
      LPHNTM(1)=.TRUE.
      NFGGLB=NFGGLB+1
      ENDIF
      IF(IC7.EQ.2) THEN
      LPHNTM(2)=.TRUE.
      NFFGLB=NFFGLB+1
      ENDIF
      ELSE
      IF(IC7.EQ.1) THEN
      LPHNTM(3)=.TRUE.
      NFGARC=NFGARC+1
      ENDIF
      IF(IC7.EQ.2) THEN
      LPHNTM(4)=.TRUE.
      NFFARC=NFFARC+1
      ENDIF
      ENDIF
      GO TO 600
!*********************************************************************
! ** FNBURN **                                             * FNBURN **
!*********************************************************************
! THE FINITE BURN MODEL
 5055 CONTINUE
      NBGRP=NBGRP+1
      GO TO 600
!*********************************************************************
! ** G2COFC  **                                           * G2COFC  **
!*********************************************************************
 5060 CONTINUE
      GO TO 600
!*********************************************************************
! ** G2COFS  **                                           * G2COFS  **
!*********************************************************************
 5065 CONTINUE
      GO TO 600
!*********************************************************************
! ** GCMMOD **                                             * GCMMOD **
!*********************************************************************
 5070 CONTINUE
      !convert amplitudes from mm to m
      !convert phase from deg to rad
      IF(IC78.EQ.1) THEN
        GCMNPS=GCMNPS+1.0D0
        IF (GCMNPS.EQ.1.0D0) GCMT0 = FIELD(1)
        GCMPER(INT(GCMNPS))=FIELD(2)
      ELSE IF (IC78.EQ.2) THEN
        IGCM=INT(GCMNPS)
        GCMAMP((IGCM-1)*3+1)=FIELD(1)/1000.0D0
        GCMPHA((IGCM-1)*3+1)=FIELD(2)*DEGRAD
      ELSE IF (IC78.EQ.3) THEN
        IGCM=INT(GCMNPS)
        GCMAMP((IGCM-1)*3+2)=FIELD(1)/1000.0D0
        GCMPHA((IGCM-1)*3+2)=FIELD(2)*DEGRAD
      ELSE IF (IC78.EQ.4) THEN
        IGCM=INT(GCMNPS)
        GCMAMP((IGCM-1)*3+3)=FIELD(1)/1000.0D0
        GCMPHA((IGCM-1)*3+3)=FIELD(2)*DEGRAD
      END IF
      GO TO 600
!*********************************************************************
! ** GDYNEP **                                             * GDYNEP **
!*********************************************************************
 5075 CONTINUE
      IF(FIELD(1).EQ.0.D0)GO TO 600
!...????
!     IF(FIELD(1).GE.1000000.D0)FIELD(1)=FIELD(1)/1.D6
!...????
      TGTMD=FIELD(1)
      ITYMD=INT(TGTMD+0.1)
      CALL MJDYMD(MJDTGT,ITYMD,IQ1HMS,2)
      TMGDN=DBLE(MJDTGT)+2400000.5D0
      IF(TGTMD.NE.TGTYMD)THEN
      TGTYMD=TGTMD
      TMGDN1=TMGDN
      TMGDN2=DBLE(MJDTGT)
      REPDIF = (TMGDN2 - 30000.D0) * 86400.0D0
      ENDIF
      GO TO 600
!*********************************************************************
! ** GEOPOL **                                             * GEOPOL **
!*********************************************************************
 5100 CONTINUE
      LGEOPL=.TRUE.
!
!     ....LGPLOR = .TRUE. allows the input of any KF value
!     ....on the POLDYN card.  Otherwise only positive KF values
!     ....are allowed if the GEOPOL card is ppresent
!
      IF( IFIELD(1) .GT. 0 ) LGPLOR = .TRUE.
      GO TO 600
!*********************************************************************
! ** GPSBXW **                                             * GPSBXW **
!*********************************************************************
 5150 CONTINUE
      IF(IC1824.NE.IGPSB) THEN
      IGPSBW=IGPSBW+9
      IGPSB=IC1824
      ENDIF
!     write(6,*)' dbg RD50A1 IGPSBW ',IGPSBW,IC1824
      GO TO 600
!*********************************************************************
! ** GPSMOD **                                             * GPSMOD **
!*********************************************************************
 5200 CONTINUE
      IF(IFIELD(2).GT.0) LSPL85=.TRUE.
      GO TO 600
!*********************************************************************
! ** GRVTIM **                                             * GRVTIM **
!*********************************************************************
 5300 CONTINUE
      IC7=IFIELD(1)
      IC8=IFIELD(2)
      IF(IC7.NE.1) GOTO 5400
      GOTO(5310,5320,5330,5340,5350), IC8
           GOTO 600
 5310 NCDT=NCDT+1
      IF(FIELD(2).GT.C0) NCDTA=NCDTA+1
      GOTO 600
 5320 NCPD=NCPD+1
      GOTO 600
 5330 NCPDA=NCPDA+1
      GOTO 600
 5340 NCDT=NCDT+1
      NCPD=NCPD+1
      GOTO 600
 5350 NCDTA=NCDTA+1
      NCPDA=NCPDA+1
      GOTO 600
 5400 CONTINUE
      IF(IC7.NE.2) GOTO 60500
      GOTO(5410,5420,5430,5440,5450), IC8
           GOTO 600
 5410 NSDT=NSDT+1
      IF(FIELD(2).GT.C0) NSDTA=NSDTA+1
      GOTO 600
 5420 NSPD=NSPD+1
      GOTO 600
 5430 NSPDA=NSPDA+1
      GOTO 600
 5440 NSDT=NSDT+1
      NSPD=NSPD+1
      GOTO 600
 5450 NSDTA=NSDTA+1
      NSPDA=NSPDA+1
      GOTO 600
!*********************************************************************
! ** HRATEF **                                             * HRATEF **
!*********************************************************************
 5460 CONTINUE
      NSATHR=NSATHR+1
      GOTO 600
!*********************************************************************
! ** INTGCB **                                             * INTGCB **
!*********************************************************************
 5465 CONTINUE
      LASTSN=.TRUE.
      LASTR(1)=.TRUE.
      IF(IFIELD(1).GT.0) LASTR(2)=.TRUE.
      GOTO 600
!*********************************************************************
! ** INVERT **                                             * INVERT **
!*********************************************************************
 5500 CONTINUE
      IF(IFIELD(1).GT.0) LSYMSW=.TRUE.
      IF(IFIELD(2).GT.0) LMATP=.TRUE.
      IF(IFIELD(3).GT.0) LNRMGI=.TRUE.
      IF(FIELD(1).GT.C0) EPSIN1=FIELD(1)
      IF(FIELD(2).GT.C0) EPSIN2=FIELD(2)
      IF(FIELD(3).GT.C0) TOLRNC=FIELD(3)
      GO TO 600
!*********************************************************************
! ** IONCOR **                                             * IONCOR **
!*********************************************************************
 5510 CONTINUE
! SET LOGICAL INDICATING IONOSPHERIC OPTION HAS BEEN SELECTED TO TRUE.
      LIONC=.TRUE.
      GO TO 600
!*********************************************************************
! ** LICASE **                                             * LICASE **
!*********************************************************************
 5550 CONTINUE
      IF(IC78.EQ.0) LINT(1)=.TRUE.
      GOTO 600
!*********************************************************************
!** LOCALG **                                              * LOCALG **
!*********************************************************************
 6000 CONTINUE
      NLOCAL=NLOCAL+1
      READ(IUNT50,71100,END=60000) CRD2
      IF(CRD2.NE. blnk  ) GOTO 60300
!     ....use default for integg in patched version
      IF(IC2122.NE.0)NLATLG=IC2122
      IF(IC2324.NE.0)NLONLG=IC2324
      INTEGG=NLATLG*NLONLG
      GOTO 600
!*********************************************************************
!** MBIAS **                                               * MBIAS ***
!*********************************************************************
 7000 IF(CDNAME.EQ.BIAS2) GOTO 600
      IF(CDNAME.EQ.BIAS3) GOTO 600
      LMBIA=.TRUE.
      IF(LSIMDT.AND.LSDATA)THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      IF(.NOT.LGLOBL) GOTO 7020
!
      IF(FIELD(4).EQ.C0) GOTO 7010
      NBIASG=NBIASG+1
      GOTO 600
 7010 CONTINUE
! CARD IGNORED IF A GLOBAL MBIAS CARD IS MISSING TIME OR SIGMAS
      WRITE(IUNT06,80700) CDNAME,IC78,IC910,IC1112,IC1314,              &
     &   IC1517,FA3,IC2122,IC2324,FIELD
      WRITE(IUNT88,80700) CDNAME,IC78,IC910,IC1112,IC1314,              &
     &   IC1517,FA3,IC2122,IC2324,FIELD
      GOTO 600
! ARC MBIAS CARD
 7020 CONTINUE
      IF(FIELD(4).EQ.C0) GOTO 7030
      IF(IC1517.NE.801) NBIASA=NBIASA+1
      IF(IC1517.NE.801) NBCDT =NBCDT+1
      IBT=IC1517/100
      IF(IBT.EQ.5) MBTRAD=MBTRAD+1
      GO TO 600
! NO  SIGMAS
 7030 CONTINUE
      IF(IC1517.NE.801)NBIASK=NBIASK+1
      IF(IC1517.NE.801)NBIASA=NBIASA+1
      IF(IC1517.EQ.500) MBTRUN=MBTRUN+1
      GO TO 600
!*********************************************************************
!** MBIASM **                                              * MBIASM **
!*********************************************************************
 7040 CONTINUE
      LMBIA=.TRUE.
      IF(LSIMDT.AND.LSDATA)THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      GO TO 600
!*********************************************************************
!** MLTDEM **                                              * MLTDEM **
!*********************************************************************
 7041 CONTINUE
      MLTDEM=1
      IF(FIELD(1).GT.0.D0) MEMDEM=INT(FIELD(1))
      GO TO 600
!*********************************************************************
!** NONSNS **                                              * NONSNS **
!*********************************************************************
 7045 CONTINUE
      LNSNS=.FALSE.
      GOTO 600
!*********************************************************************
!** NORMPT **                                              * NORMPT **
!*********************************************************************
 7050 CONTINUE
! MAXOBS IS SET WITH THE FOLLOWING HIERARCHY: NORMPT - VECOPT - EMATRX
      LNPT14=IC1114.GE.11
      IF(LNPT14) MAXOBS=IC1114
!     IF(IC1820.GT.10.AND.IC1820.LT.101) MAXBUF=IC1820+1
      IF(IC1114.GT.0)MXOBSB=IC1114
      GOTO 600
!*********************************************************************
!** NUTATE **                                              * NUTATE **
!*********************************************************************
 7777 CONTINUE
      IF(IFIELD(1).GT.0) THEN

! TOTAL NUMBER OF NUTATE CARDS
         NNUT=NNUT+1

! TOTAL NUMBER OF ADJUSTED NUTATE CARDS
! FOR NOW SHOULD BE SAME AS NNUT
         NNUTAA=NNUTAA+1

         write(6,*) "RD50A1: IN NUTATE", NNUT
         write(6,*) "RD50A1: ADJUST NUTATE", NNUTAA

!CHECK TO SEE IF SECOND NUTATE CARD IS VALID (CRD2 is BLANK)
         READ(IUNT50,71100) CRD2
         IF(CRD2.NE.BLNK) GOTO 60401

      ELSE
         GO TO 600

      ENDIF
      GO TO 600
!*********************************************************************
!** OFFADJ **                                              * OFFADJ **
!*********************************************************************
 7060 CONTINUE
      LADJOF=.TRUE.
      XNOFFA=XNOFFA+1.D0
      NHOLD=(XNOFFA+.2D0)
      DO 7065 IQP=1,10
      CRDOFA(IQP,NHOLD)=CARDAR(IQP)
 7065 END DO
      GOTO 600
!*********************************************************************
!** OFFSET **                                              * OFFSET **
!*********************************************************************
 7070 CONTINUE
      LOFFST=.TRUE.
      GOTO 600
!*********************************************************************
!** OLOAD  **                                              * OLOAD  **
!*********************************************************************
 7100 CONTINUE
      NTOLMD=NTOLMD+1
      IF(FIELD(3).GT.C0.OR.FIELD(4).GT.C0) NTOLAJ=NTOLAJ+1
      IF(IC1114.EQ.1) LXYZCO=.TRUE.
      IF(IC1114.EQ.2) LEOPTO=.TRUE.
      GO TO 600
!*********************************************************************
!** OPVLBI **                                              * OPVLBI **
!*********************************************************************
 7110 CONTINUE
      JJ=0
 7111  CONTINUE
       READ(IUNT50,88000) CDNAME
       IF(CDNAME.EQ.OPTION(86)) GOTO 7112
       IF(CDNAME.EQ.OPTION(85)) THEN
       JJ=JJ+1
      ENDIF
       GOTO 7111
 7112  CONTINUE
       NVOPT=JJ
       GOTO 600
!*********************************************************************
!** ORBINF **                                              * ORBINF **
!*********************************************************************
 7200 CONTINUE
!
!     ....add 60 days to run end date to get enough tables for
!     ....the orbinf polar motion data in header (5 points at 10 day
!     ....intervals)  4/4/88  jjmcc
!
      call addymd( irune, 60)
!
      NORBNF= MIN(IC78,15)
      GO TO 600
!*********************************************************************
! ** OTIDES **                                             * OTIDES **
!*********************************************************************
 7300 CONTINUE
      NTIDE=NTIDE+1
      GO TO 600
!*********************************************************************
!** PANEL **                                               * PANEL **
!*********************************************************************
 7500 CONTINUE
      GO TO (7510,7520,7530,7540,7550,7560,7570,7580,7590,7600),IC1314
 7510    NMSLAB=NMSLAB+1
         GO TO 600
 7520    IF(FIELD(2).NE.C0) THEN
           NAREPT=NAREPT+1
         ENDIF
         GO TO 600
 7530    IF(FIELD(2).NE.C0) THEN
           NSPRPT=NSPRPT+1
         ENDIF
         GO TO 600
 7540    IF(FIELD(2).NE.C0) THEN
           NDFRPT=NDFRPT+1
         ENDIF
         GO TO 600
 7550    IF(FIELD(2).NE.C0) THEN
           NEMIPT=NEMIPT+1
         ENDIF
         GO TO 600
 7560    IF(FIELD(2).NE.C0) THEN
           NTMAPT=NTMAPT+1
         ENDIF
         GO TO 600
 7570    IF(FIELD(2).NE.C0) THEN
            NTMCPT=NTMCPT+1
         ENDIF
         GO TO 600
 7580    IF(FIELD(2).NE.C0) THEN
           NTMDPT=NTMDPT+1
         ENDIF
         GO TO 600
 7590    IF(FIELD(2).NE.C0) THEN
           NTMFPT=NTMFPT+1
         ENDIF
         GO TO 600
 7600    IF(FIELD(2).NE.C0) THEN
           NTHXPT=NTHXPT+1
         ENDIF
         GO TO 600
!
!*********************************************************************
!** PBIAS **                                               * PBIAS ***
!*********************************************************************
 8000 IF(CDNAME.EQ.PBIAS2) GOTO 600
      IF(CDNAME.EQ.PBIAS3) GOTO 600
      LPBIA=.TRUE.
      IF(LSIMDT.AND.LSDATA)THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      NBIASA=NBIASA+1
      NBCDP =NBCDP+1
      GO TO 600
!*********************************************************************
!** PBIASM **                                             * PBIASM ***
!*********************************************************************
 8050 CONTINUE
      LPBIA=.TRUE.
      IF(LSIMDT.AND.LSDATA)THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      GO TO 600
!*********************************************************************
!** PLANET**                                               * PLANET***
!*********************************************************************
 8100 CONTINUE
      IF(FIELD(4).GT.0.D0) THEN
         WRITE(6,88005)
         WRITE(6,88006)
         WRITE(6,88007)
         STOP
      ENDIF
      IF(IC1517.GT.0) CALL BDPT(IC1114,1)
      IF(IC1820.GT.0) CALL BDPT(IC1114,2)
      IF(IC1114.EQ.1) LXTRAP=.TRUE.
      GO TO 600
!*********************************************************************
!** PLANOR**                                               * PLANOR***
!*********************************************************************
 8105 CONTINUE
      IF(FIELD(4).GT.1.D-80) LPLNRA=.TRUE.
      IND7=IC78/10
      IND17=IC1517
      IND8=MOD(IC78,10)
! BEGIN JBN
        IF(IND17.LE.1) THEN
      ! COL7 1-7 assumed to be present (both ANALYTIC and DYNAMIC case)
      IF (.NOT. LASTR(2)) THEN ! ANALYTIC
!         IF (IND7 == 1) THEN
          IF (IC78 == 11) THEN
              IF (NXFLAG /= 0) THEN
                  NXTRAC = NXTRAC + 6
              END IF
              NXFLAG = 1
          END IF
      ELSE ! DYNAMIC
          IF (IND7 == 0 .AND. IND8 > 0) THEN
              NMINRT = NMINRT + 1
              NXTRAC = NXTRAC + 1
              print*, 'JBNDEBUG:  NMINRT = ', NMINRT, '  NXTRAC = ', NXTR
          END IF
      END IF
        ENDIF

        IF(IND17.EQ.2) THEN
      ! COL7 1-7 assumed to be present (both ANALYTIC and DYNAMIC case)
      IF (.NOT. LASTR(2)) THEN ! ANALYTIC
!         IF (IND7 == 1) THEN
          IF (IC78 == 11) THEN
              IF (NXFLAG2 /= 0) THEN
                  NXTRAC2 = NXTRAC2 + 6
              END IF
              NXFLAG2 = 1
          END IF
      ELSE ! DYNAMIC
          IF (IND7 == 0 .AND. IND8 > 0) THEN
              NMINRT2 = NMINRT2 + 1
              NXTRAC2 = NXTRAC2 + 1
              print*, 'JBNDEBUG:  NMINRT2= ',NMINRT2, ' NXTRAC2 = ',NXTRA
          END IF
      END IF
        ENDIF

      GO TO 600
! END JBN
!*********************************************************************
! PLNEPH **                                                * PLNEPH **
!*********************************************************************
 8110 CONTINUE
!  *** GET AND TEST BODY ID FROM OPTION CARD ****
      IC912=IC910*100+IC1112
      IRID=IC912
!  ***  CODE FOR CENTRAL BODY CASE  ****
      IF (IRID.EQ.0) GO TO 600
      IF (RID1.EQ.C0) THEN
      RID1=IRID
      ELSE
      IF (ABS(DBLE(IRID)-RID1).GT.0.01D0) RID2=IRID
      END IF
      IC2124=IC2122*100+IC2324
      IF (IC2124.EQ.0.AND.FIELD(1).EQ.C0) GOTO 600
!  LOAD REQUESTED EPOCH INTO COMMON BLOCK /CA1UTC/
      IMMDD=FIELD(1)/1000000
      IYYMD=IC2124*10000+IMMDD
      IFLD1=FIELD(1)
      IHMS=MOD(IFLD1,1000000)
      FFF=FIELD(1)-IFLD1
      IC2D=MOD(IC2124,100)
      IYMD2D=IC2D*10000+IMMDD
      IF (RID2.EQ.C0) THEN
      YMD1=IYMD2D
      HMS1=IHMS
      ELSE
      YMD2=IYMD2D
      HMS2=IHMS
      END IF
      GOTO 600
!*********************************************************************
!** PLNGLF **                                              * PLNGLF **
!*********************************************************************
 8112 CONTINUE
      IMOD=IC1517

       IF(NFMOD.EQ.0) THEN
       NFMOD=NFMOD+1
       ICFMOD(NFMOD)=IMOD
       NFPARAM(NFMOD)=NFPARAM(NFMOD)+1
       NPMIND(NFMOD)=1

           ELSE

        IF(ICFMOD(NFMOD).NE.IMOD) THEN
!       a different model
        NFMOD=NFMOD+1
        ICFMOD(NFMOD)=IMOD
        NFPARAM(NFMOD)=NFPARAM(NFMOD)+1
        NPMIND(NFMOD)=NPMIND(NFMOD-1)+NFPARAM(NFMOD-1)
        ELSE
        NFPARAM(NFMOD)=NFPARAM(NFMOD)+1
        ENDIF

          ENDIF
       IDFMOD(NFMOD)=IMOD
       NFTOT=0
       DO I=1,NFMOD
       NFTOT=NFTOT+NFPARAM(i)
       ENDDO



      GOTO 600
!*********************************************************************
!** PLNSRF **                                              * PLNSRF **
!*********************************************************************
 8114 CONTINUE
      IMOD=IC1517

       IF(NSMOD.EQ.0) THEN
       NSMOD=NSMOD+1
       ICSMOD(NSMOD)=IMOD
       NSPARAM(NSMOD)=NSPARAM(NSMOD)+1
       NPSIND(NSMOD)=1

           ELSE

        IF(ICSMOD(NSMOD).NE.IMOD) THEN
!       a different model
        NSMOD=NSMOD+1
        ICSMOD(NSMOD)=IMOD
        NSPARAM(NSMOD)=NSPARAM(NSMOD)+1
        NPSIND(NSMOD)=NPSIND(NSMOD-1)+NSPARAM(NSMOD-1)
        ELSE
        NSPARAM(NSMOD)=NSPARAM(NSMOD)+1
        ENDIF

          ENDIF
       IDSMOD(NSMOD)=IMOD
       NSTOT=0
       DO I=1,NSMOD
       NSTOT=NSTOT+NSPARAM(i)
       ENDDO
      GOTO 600

!*********************************************************************
!** POLEUT **                                              * POLEUT **
!*********************************************************************
 8200 CONTINUE
      NPOLE=NPOLE+1
      IF(IFIELD(1) .GT.0 ) NPOLEA=NPOLEA+1
      READ(IUNT50,71100) CRD2
      IF(CRD2.NE.BLNK) GOTO 60400
      IF(IFIELD(1) .NE.2.AND. IFIELD(1) .NE.3) GOTO 8210
      READ(IUNT50,71100) CRD2
      IF(CRD2.NE.BLNK) GOTO 60400
 8210 CONTINUE
      IF(IFIELD(1).NE.4) GOTO 600
      NMODEL=1
      READ(IUNT50,71100) CRD2
      IF(CRD2.NE.BLNK) GOTO 60400
      GOTO 600
!*********************************************************************
!** PRNTVU **                                              * PRNTVU **
!*********************************************************************
 8300 CONTINUE
      DO 8400 I=1,16
      J=I+2
      IF(IFIELD(J).EQ.1) LNPRNT(I)=.TRUE.
      IF(IFIELD(J).EQ.2) LNPRNT(I)=.FALSE.
 8400 END DO
      LPGRAV=.NOT.LNPRNT(4)
      GOTO 600
!*********************************************************************
! QUAPOS **                                                * QUAPOS **
!*********************************************************************
 8500 CONTINUE
      MXQUA=0
 8510 READ(IUNT50,71150)CDNAME
      IF(CDNAME.EQ.ENDQUA)GOTO 600
      MXQUA=MXQUA+1
      GOTO 8510
!*********************************************************************
! RELPPN **                                                * RELPPN **
!*********************************************************************
 8600 CONTINUE
      GOTO 600

!*********************************************************************
! REFSYS **                                                * REFSYS **
!*********************************************************************
 9000 READ(IUNT50,71200,END=60100) CDNAME,MREFSY,IRYMD
      IF(IRYMD.LE.491231)IRYMD=IRYMD+1000000
      IF(CDNAME.NE.REFSYS) GO TO 60200
      IF(MREFSY.NE.1) THEN
         IRUNB= MIN(IRUNB,IRYMD)
      ELSE
       IRUNB=1000101
         IRUNE= 991231
      ENDIF
!** SATPAR ** EPOCH ** SLAVE ** ELEMS1 ** ELEMS2
! SATPAR
      READ(IUNT50,71105,END=60100) CDNAME,IFIELD(6),IFIELD(8),MAXDSA
      IF(CDNAME.NE.SATPAR) GO TO 60200
! if any sat. in any arc requests external attitude then set flag
! IFIELD(6) = col. 12
10000 IF(IFIELD(6).eq.9) LEXATT=.TRUE.
! if any sat. in any arc requests external thermal accel then set flag
! IFIELD(8) = col. 14
10001 IF(IFIELD(8).eq.1) LEXTHC=.TRUE.
! increment number of sats. in arc
      NUMSAT=NUMSAT+1
! IF MAX DEGREE FROM SATPAR IS ZERO, SET MAXDS TO A VERY LARGE NO. TO BE
!  REEVALUATED IN RD50A2
      IF (MAXDSA.EQ.0)THEN
       MAXDS=99999
      ELSE
       MAXDS=MAXDSA
      ENDIF
! EPOCH OR SLAVE
10010 READ(IUNT50,71220,END=60100) CDNAME,IYMD,IYMDS,IYMDE
      IF(IYMD.LE.491231)IYMD=IYMD+1000000
      IF(IYMDS.LE.491231)IYMDS=IYMDS+1000000
      IF(IYMDE.LE.491231)IYMDE=IYMDE+1000000
      IY1=IYMD/10000
      IY2=IYMDS/10000
      IY3=IYMDE/10000
      IF(CDNAME.NE.EPOCH.AND.CDNAME.NE.SLAVE) GO TO 60200
      IF(CDNAME.EQ.SLAVE) GOTO 10015
      IYMD= MIN(IYMD,IYMDS)
      IRUNB= MIN(IRUNB,IYMD)
      IF(IYMDE.LT.IYMD) IYMDE=IYMD+010000
!
      IRUNE= MAX(IRUNE,IYMDE)
10015 CONTINUE
!** ELEMS1 **
10020 READ(IUNT50,71100,END=60100) CDNAME
      IF(CDNAME.NE.ELEMS1) GO TO 60200
!** ELEMS2 **
      READ(IUNT50,71100,END=60100) CDNAME
      IF(CDNAME.NE.ELEMS2) GO TO 60200
      GO TO 600
!*********************************************************************
!** REL300 **                                              * REL300 **
!*********************************************************************
10200 CONTINUE
      LGRELE=.TRUE.
      IF(IFIELD(1) .GT.0 ) LGRELR=.FALSE.
      GOTO 600
!*********************************************************************
!** SATCUT **                                              * SATCUT **
!*********************************************************************
10300 CONTINUE
      CUTSAT=FIELD(1)
      GOTO 600
!*********************************************************************
!** SCBODY **                                              * SCBODY **
!*********************************************************************
10400 CONTINUE
      ! INCREASE THE BODY ID BY ONE SO THAT IT WORKS WITH THE INTGCB
      ! CARD.  THE INTGCB CARD USES SLOT 12, SO START EXTERNAL
      ! EPHEMERIS BODIES AT 13.
          IF(LASTSN) THEN
           IC910 = IC910 + 1
          ENDIF
      IC2123=IC2122*10+(IC2324/10)
      IC7=IC78/10
      IC8=IC78-IC7*10
      ICSC=IC8*100+IC910
      IF(IC1820.GT.0) CALL BDPT(ICSC,1)
      IF(IC2123.GT.0) CALL BDPT(ICSC,2)
      GOTO 600
!*********************************************************************
! ** SELECT/DELETE OPTION *                                * SELECT **
!*********************************************************************
! COUNT METDAT,DELETE AND(SELECT,SIGMA,OBSCOR,PREPRO) CARDS
10500 LOBS=.TRUE.
! IF ORBGEN FOR INTERCOMPARISON  AND DATA SUBGROUP CO-EXIST
! THEN STOP
      IF(LOBS.AND.LINT(1)) GOTO 60700
10510 READ(IUNT50,71100,END=60100) CDNAME
      IF(CDNAME.EQ.ENDARC) GO TO 4000
      IF(CDNAME.EQ.DELETE) GO TO 10540
      IF(CDNAME.EQ.XMETDT) GO TO 10520
! CARD MUST BE A SELECT,SIGMA,OBSCOR OR PREPRO CARD
      NSEL=NSEL+1
      GO TO 10510
!****  METDAT ****
10520 NMETDT=NMETDT+1
      GO TO 10510
!**** DELETE ****
10540 NDEL=NDEL+1
      GO TO 10510
!**********************************************************************
!** SIMDAT **                                              ** SIMDAT **
!**********************************************************************
10560 CONTINUE
      ISIM=ISIM+1
! ISIM=2 THERE ARE 2 SIMDAT CARDS SKIP THE SECOND
      IF(ISIM.GT.1) GOTO 600
      LSIMDT=.TRUE.
      LOBS=.TRUE.
      LSDATA=.FALSE.
      IF(IC78.EQ.1)LSDATA=.TRUE.
      IF(LSDATA.AND.(LMBIA.OR.LPBIA.OR.LEBIA))THEN
      WRITE(6,84000)
      STOP 16
      ENDIF
      IF(LSDATA)LST17=.TRUE.
      GO TO 600
!*********************************************************************
! SLFSHD **                                                * SLFSHD **
! ********************************************************************
10700 CONTINUE
! COUNT THE NUMBER OF SLFSHD CARDS
      IC7=IC78/10
      IC8=IC78-IC7*10
      IF(IC8.EQ.1) THEN
! SET THE LOGICAL FLAG FOR SELF SHADOWING
        LSLFSH=.TRUE.
        NSHSAT=NSHSAT+1
! SET THE MODEL (SOLAR AND DRAG) INDICATOR FOR THE SAT
        IFLGSH(NSHSAT)=IC910
        IDSASH(NSHSAT)=IC1820*10000+IC2122*100+IC2324
! SET THE FILE INDICATOR
        IF(IC7.EQ.1) THEN
          LBISHF=.TRUE.
        ELSE IF(IC7.GT.1) THEN
          WRITE(6,*) ' WRONG CHOICE AT COLUMN 7 OF SLFSHD '
          STOP 16
        ENDIF
      ENDIF
      GO TO 600
!*********************************************************************
! SOLTAB **                                                * SOLTAB **
! ********************************************************************
11000 CONTINUE
      GO TO 3000
!*********************************************************************
!  STAPOS **                                               * STAPOS **
!*********************************************************************
12000 CONTINUE
      IF(IC1114.GT.50) MXBSTA=IC1114

      if( ifield(4) > 0 ) MXBSTA = IC1114 + 10000*ifield(4)

      !write(6,'(A,3(1x,I12))') &
      !      'rd50a1: stapos IC1114, ifield(4), mxbsta', &
      !                      IC1114, ifield(4), mxbsta


      IF(IFIELD(1).GT.0) LSTADJ=.TRUE.
      IF(IFIELD(2).GT.0) GO TO 12050
! COUNT NUMBER OF STATIONS ON GEODETIC FILE
12030 READ(IUNT16,71100,END=12040) DUM
      MXBAS=MXBAS+1
      GO TO 12030
12040 REWIND IUNT16
! COUNT NUMBER OF CARDS BETWEEN STAPOS & ENDSTA
12050 READ(IUNT50,71100,END=60100) CDNAME
      IF(CDNAME.EQ.ENDSTA) GOTO 600
      DO 12060 I=1,NSTACD
      IF(CDNAME.EQ.STACDS(I)) GOTO 12050
12060 END DO
      MXSTA=MXSTA+1
      IF(CDNAME.EQ.ENDGLB) GOTO 5000
      GO TO 12050
!*********************************************************************
!  STEP   **                                               * STEP   **
!*********************************************************************
12070 CONTINUE
      IND14=IC1314
      IF(IND14.GT.0) NINTOT=NINTOT+1
      GOTO 600
!*********************************************************************
!  SSCOEF **                                               * SSCOEF **
!*********************************************************************
12100 CONTINUE
! INCREMENT NUMBER OF PARAMETERS
! CBAR TERM
      IF(FIELD(1).NE.C0 .OR. FIELD(3).NE.C0) THEN
       NCBAR=NCBAR+1
       IF(FIELD(3).GT.C0) THEN
        NCBARA=NCBARA+1
       ENDIF
      ENDIF
!
! SBAR TERM
      IF(FIELD(2).NE.C0 .OR. FIELD(4).NE.C0) THEN
       NSBAR=NSBAR+1
       IF(FIELD(3).GT.C0) THEN
        NSBARA=NSBARA+1
       ENDIF
      ENDIF
      GOTO 600
!
!*********************************************************************
!  SSTOPO **                                               * SSTOPO **
!*********************************************************************
12200 CONTINUE
      LSTOPO=.TRUE.
! SET PRINT CONTROL FLAG
      IF(IFIELD(2).EQ.1.OR.IFIELD(2).EQ.3) LNPRNT(7)=.FALSE.
! FIND MAXIMUM DEGREE AND ORDER FOR SST MODEL
      MDEGEO= MAX(IC1517,3)
      MORGEO= MAX(IC1820,2)
      GOTO 600
!
!*********************************************************************
!  SSTPRD **                                               * SSTPRD **
!*********************************************************************
12300 CONTINUE
      IC8=IFIELD(2)
! INCREMENT NUMBER OF PARAMETERS
      IF(IC8 .EQ. 1) THEN
! Aci TERM
        NCAPRD=NCAPRD+1
! ADJ. Aci
        IF(FIELD(3).GT.C0) THEN
         NCAPDA=NCAPDA+1
        ENDIF
! Bci TERM
        NCBPRD=NCBPRD+1
! ADJ. Bci
        IF(FIELD(4).GT.C0) THEN
         NCBPDA=NCBPDA+1
        ENDIF
!
      ELSE
!
! Asi TERM
        NSAPRD=NSAPRD+1
! ADJ. Asi
        IF(FIELD(3).GT.C0) THEN
         NSAPDA=NSAPDA+1
        ENDIF
! Bsi TERM
        NSBPRD=NSBPRD+1
! ADJ. Bsi
        IF(FIELD(4).GT.C0) THEN
         NSBPDA=NSBPDA+1
        ENDIF
       ENDIF
       GOTO 600
!
!*********************************************************************
!  SSTTIM **                                               * SSTTIM **
!*********************************************************************
12350 CONTINUE
      IC8=IFIELD(2)
! SET NUMBER OF PARAMETERS FOR MEAN TERM
      IF(FIELD(1).EQ.C0 .AND.FIELD(3).EQ.C0) GOTO 12375
! CBAR TERM
      IF(IC8 .EQ. 1) THEN
        NCBAR=NCBAR+1
        IF(FIELD(3).GT.C0) THEN
         NCBARA=NCBARA+1
        ENDIF
      ELSE
! SBAR TERM
        NSBAR=NSBAR+1
        IF(FIELD(3).GT.C0) THEN
         NSBARA=NSBARA+1
        ENDIF
      ENDIF
!
12375 CONTINUE
      IF(FIELD(2).EQ.C0 .AND.FIELD(4).EQ.C0) GOTO 12390
! CDOT TERM
      IF(IC8 .EQ. 1) THEN
        NCDOT=NCDOT+1
        IF(FIELD(4).GT.C0) THEN
         NCDOTA=NCDOTA+1
        ENDIF
      ELSE
! SDOT TERM
        NSDOT=NSDOT+1
        IF(FIELD(4).GT.C0) THEN
         NSDOTA=NSDOTA+1
        ENDIF
      ENDIF
12390 CONTINUE
      GO TO 600
!*********************************************************************
!  STIERS   **                                             * STIERS **
!*********************************************************************
12394 CONTINUE
      LSTIER=.TRUE.
      GO TO 600
!*********************************************************************
!  TOLS   **                                               * TOLS   **
!*********************************************************************
12395 CONTINUE
      IF(IC1114.GT.0) NTOLD=IC1114
      GO TO 600
!*********************************************************************
!  TETHER **                                               * TETHER **
!*********************************************************************
12400 CONTINUE
      LTETHR=.TRUE.
      GOTO 600
!*********************************************************************
!  TOPATT **                                               * TOPATT **
!*********************************************************************
12450 CONTINUE
! STORE THE SAT IDS in TEMPORARY ARRAY
      IF (NMTPAT.EQ.0) THEN
        NMTPAT=NMTPAT+1
        TOPTEM(NMTPAT)=IC1824
        WRITE (6,*) "RD50A1: NMTPAT=",NMTPAT
      ELSE
        NMTPAT=NMTPAT+1
        TOPTEM(NMTPAT)=IC1824
        WRITE (6,*) "RD50A1: NMTPAT=",NMTPAT
      ENDIF
      write (6,*) "RD50A1: TOPTEM=",TOPTEM
      GO TO 600
!*********************************************************************
!  TRKBDY **                                               * TRKBDY **
!*********************************************************************
12500 CONTINUE
!
!     ....SET THE PLANET NUMBER OF THE TRACKING BODY (ITBDGM)
!     ....DEFAULT ITBDGM = 3
!
      IF(IC1114.GT.0) ITBDGM = IC1114
      GO TO 600
!*********************************************************************
!  TRPOUT **                                               * TRPOUT **
!*********************************************************************
12700 CONTINUE
      LTROUT=.TRUE.
      GO TO 600
!*********************************************************************
!  TUMSOL **                                               * TUMSOL **
!*********************************************************************
12800 CONTINUE
      NDTUM=NDTUM+1
      IF(IFIELD(2).EQ.2) IFLAG1=0
      IF(IFIELD(3).EQ.2) IFLAG3=0
      IF(IFIELD(1).EQ.0)  THEN
! APPLY TUM TO ALL SATELLITES IN THE RUN
      INDTUM=1
      ELSE
      ITUMSC(NDTUM)=IC1824
      INDTUM=2
        IF(IC1824.EQ.0) THEN
        WRITE(6,*)'IIS TERMINATING READING OPTION TUMSOL IN RD50A1'
        WRITE(6,*)'PLEASE REVIEW THE TUMSOL OPTIONS '
        ENDIF
      ENDIF
      GO TO 600
!*********************************************************************
!  VLIGHT **                                               * VLIGHT **
!*********************************************************************
13000 CONTINUE
      VLOW  =DEFVLT-CTOL
      VHIGH =DEFVLT+CTOL
      IF(FIELD(2).GE.VLOW.AND.FIELD(2).LE.VHIGH) DEFVLT=FIELD(2)
      IF(FIELD(1).GE.VLOW.AND.FIELD(1).LE.VHIGH) DEFVLT=FIELD(1)
      VLIGHT=DEFVLT
      GOTO 600
!**********************************************************************
!** VECOPT **                                              ** VECOPT **
!**********************************************************************
13300 CONTINUE
! MAXOBS IS SET WITH THE FOLLOWING HIERARCHY: NORMPT - VECOPT - EMATRX
      IF(IC1114.GE.11.AND..NOT.LNPT14) THEN
      LVOP14=.TRUE.
      MAXOBS=IC1114
      ENDIF
      GO TO 600
!**********************************************************************
!** YARKOV **                                              ** YARKOV **
!**********************************************************************
13350 CONTINUE
      IC=IC78/10
      IF(IC.EQ.0) YARTYP=1.D0
      GOTO 600
!**********************************************************************
!** YAWBIA **                                              ** YAWBIA **
!**********************************************************************
13400 CONTINUE
! COUNT YAWBIA CARDS
      NYWBIA=NYWBIA+1
      GOTO 600
!**********************************************************************
!** YAWPOL **                                              ** YAWPOL **
!**********************************************************************
13500    CONTINUE
         CALL PASYAW(NCRDUM)
         GO TO 600
!*********************************************************************
! SOLRAD **                                                * SOLRAD **
! ********************************************************************
13600    CONTINUE


      IF(IC1314.EQ.1) LRATIO=.TRUE.

         GO TO 600
!*********************************************************************
! NORMAL END OF FILE - REWIND UNIT 50
60000 REWIND IUNT50
      NBIAST=MBIASK+MBIASG+500*MBCDP+MBCDT
      NGRVTM=NCDT+NCPD*3+NSDT+NSPD*3
!

! TOPTEM - TEMPORARY ARRAY FOR TOPATT SAT IDS
! NLENTP - TOPTEM ARRAY LENGTH
! NSATTP - NUMBER OF UNIQUE SATILLITES WITH TOPATT CARDS
      IF(NMTPAT.GT.0) THEN
        CALL UNIQNUM(TOPTEM,9,NSATTP)
        NSATTP=NSATTP-1
      ELSE
        CONTINUE
      ENDIF
!
      IF(LSDATA)GOTO 140
      IF(.NOT.LOBS)GOTO 140
! READ MASTER HEADER RECORD AND SPECIFY MAXBUF FROM THE EIGHTH WORD
!
      DO 110 I=1,4
      MASPTR(1,I)=0
      MASPTR(2,I)=0
  110 END DO
      NBUFIN=NBUFIN+1
      MARKER=1
      IF(NBUFIN.GT.MAXBUF) NBUFIN=NBUFIN-MAXBUF
!
! NOTE: THE FOLLOWING CALL TO RDBUF WILL READ THE FIRST ARC'S UNIT 40
!       TO COMPUTE THE MAXIMUM NUMBER OF PHYSICAL BUFFERS FOR A LOGICAL
!       BLOCK.  SUBSEQUENT UNIT 40'S MUST HAVE MAXBUF .LE. THE FIRST UNI
!       40'S MAXBUF
! OPEN UNIT 40 TO READ MASTER BLOCK HEADER RECORD
!
      OPEN(UNIT=IUNT40,FILE='ftn40',FORM='UNFORMATTED',STATUS='OLD')
!
      CALL RDBUF(LEND,BUFIN(1,1,NBUFIN),IOERR)
      IF(IOERR.NE.0) GOTO 60600
!
!  * DEAL WITH NEW DEFAULT GEODYN REFERENCE TIME( NOT 410106=JD2430000.5
!
      IF(NBUFIN.EQ.1)THEN
      TGDN=BUFIN(1,1,NBUFIN)
      TGHDR=BUFIN(1,10,NBUFIN)
      IF(TGDN.NE.TGDN0.AND.TDHDR.EQ.HDR) GO TO 114
      ENDIF
!
!
!     ** LOOK FOR MASTER BLOCK HEADER RECORD **
!
      IF(MARKER.GT.200) MARKER=1
!
  114 MARKER=1
!
  115 CONTINUE
!
      J=MARKER
      DO 120 I=J,200
      IF(BUFIN(I,10,NBUFIN).NE.HDR) GOTO 120
      MARKER=I
      GOTO 130
  120 END DO
!     ** NO MASTER BLOCK HEADER IN THIS BUFFER WHERE ONE WAS EXPECTED **
      IOERR=-2
      WRITE(IUNT06,61000)
      GOTO 60600
!     ** FOUND MASTER BLOCK HEADER **
  130 CONTINUE
      MASPTR(1,4)=NBUFIN
      MASPTR(2,4)=MARKER
!     ** MREC=MASTER HEADER RECORD # **
!     ** MBUF=MASTER HEADER BUFFER # **
      MREC=MASPTR(2,4)
      MBUF=MASPTR(1,4)
      KNTAUX=BUFIN(MREC,8,MBUF)
      HB=BUFIN(MREC,8,MBUF)-KNTAUX
      RBUF=HB*1.D5+0.2D0
      RBUF=HB*1.D5+0.02D0
      RBUFR=HB*1.D10+0.02D0
      RBUFR=MOD(RBUFR,100000.D0)
      LRAMPR=.FALSE.
      IF(RBUFR.GT.1.D0) LRAMPR=.TRUE.
      MAXBUF=INT(RBUF)
      IF(MAXBUF.LE.0) MAXBUF=11
      REWIND IUNT40
! CLOSE UNIT 40 TO SETUP REOPENING IN MAIN2
      CLOSE(IUNT40)
      GO TO 150
!
  140 CONTINUE
!
! take care of the Geodyn reference time
!
      IF(TGTMD.NE.0.D0)THEN
      TGTYMD=TGTMD
      TMGDN1=TMGDN
      TMGDN2=TMGDN1-2400000.5D0
      ENDIF
!
  150 CONTINUE
      IF(TGTMD.NE.0.D0)THEN
      IF(TGTYMD.NE.TGTMD.OR.TMGDN1.NE.TMGDN)THEN
      WRITE(IUNT06,85000)TGTMD,TMGDN,TGTYMD,TMGDN1
      ENDIF
      ENDIF
      RETURN
! UNEXPECTED END OF FILE
60100 CONTINUE
      if( icard .lt. 1 ) then
         WRITE(IUNT06,80001)
      else
         WRITE(IUNT06,80000) OPTION(ICARD)
      endif
      WRITE(IUNT06,80900)
      STOP 16
!
60200 WRITE(IUNT06,80100) CDNAME
      WRITE(IUNT06,80900)
      STOP 16
!
60300 WRITE(IUNT06,80200) CRD2
      WRITE(IUNT06,80900)
      STOP 16
!
60400 WRITE(IUNT06,80300) CRD2
      WRITE(IUNT06,80900)
      STOP 16
60401 WRITE(IUNT06,80301) CRD2
      WRITE(IUNT06,80900)
      STOP 16
60500 WRITE(IUNT06,81000)
      STOP 16
60600 CONTINUE
      WRITE(IUNT06,62000)IOERR
      STOP 16
60700 CONTINUE
      WRITE(IUNT06,82000)
      STOP 16
60800 CONTINUE
      WRITE(IUNT06,86000)
      WRITE(IUNT06,86001)
      STOP 16
60900 CONTINUE
      WRITE(IUNT06,87000)
      WRITE(IUNT06,87001) NSUBLG
      WRITE(IUNT06,87002) NLOCAL
      STOP 16
! FORMATS
61000 FORMAT(1X,'RD50A1 - NO MASTER BLOCK HEADER RECORD FOUND ',        &
     &     ' EXECUTION TERMINATED')
62000 FORMAT('0** RD50A1 **  I/O ERROR OR UNEXPECTED EOF ON UNIT 40 .', &
     &   '  IOERR =',I3,/,'  PLEASE CHECK YOUR INPUT DATA FILE')
71000 FORMAT(BZ,A6,4I2,I3,3A1,2I2,D20.8,D15.3,D13.1,D8.1)
71001 FORMAT(BZ,A80)
71100 FORMAT(BZ,A6)
!71105 FORMAT(BZ,A6,5X,I1,2X,I3)
71105 FORMAT(BZ,A6,5X,I1,1X,I1,I3)
71150 FORMAT(BZ,A8)
71200 FORMAT(BZ,A6,4X,I1,9X,I6)
71220 FORMAT(BZ,A6,14X,I6,14X,I6,14X,I6)
71300 FORMAT(///)
71400 FORMAT(A80)
71401 FORMAT(A99)
71402 FORMAT(D25.16)
80000 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED WHILE SEARCHING',   &
     &   ' FOR ',A8 / 5X,' OR RELATED SUBGROUP CARDS')
80001 FORMAT(1X,'UNEXPECTED END OF FILE ENCOUNTERED '/                  &
     &   ' BEFORE READING ANY OPTION CARDS')

80100 FORMAT(5X,'CARD ',A8,' DOES NOT BELONG IN SATPAR SUBGROUP')
80200 FORMAT(5X,'CARD ',A8,' DOES NOT BELONG IN LOCALG SUBGROUP')
80300 FORMAT(5X,'CARD ',A8,' DOES NOT BELONG IN POLEUT SUBGROUP')
80301 FORMAT(5X,'CARD ',A8,' DOES NOT BELONG IN NUTATE SUBGROUP')
80700 FORMAT(5X,'CARD IN GLOBAL SECTION IGNORED',/,5X,                  &
     &     A6,4I2,I3,3A1,2I2,D20.8,D15.3,D13.1,D8.1)
80900 FORMAT(5X,'EXECUTION TERMINATING IN SUBROUTINE RD50A1')
81000 FORMAT(1X,'EXECUTION TERMINATED DUE TO BOTH C & S COEF NOT ',     &
     &          'EXIST ON GRVTIM CARD')
82000 FORMAT(1X,'RD50A1 - THIS IS AN ORBIT GENERATION - REMOVE   ',     &
     &          'DATA SUBGROUP FROM YOUR UNIT 5 SETUP')
83000 FORMAT(1X,'FANTIM OPTION MUST CONTAIN AT LEAST ONE OPTION ',      &
     &          'EXECUTION TERMINATED IN RD50A1 ')
84000 FORMAT(1X,'ALL BIAS RELATED INPUT CARDS MUST BE REMOVED FROM A',&
     &'SIMULATED DATA GENERATION',/,' RUN, ', &
     &' EXECUTION TERMINATED IN RD50A1')
85000 FORMAT(/'** NOTE**'//'    YOUR SELECTED GEODYN TIME FROM GDYNEP',&
     &'CARD IS : DATE =',F8.1,2X,'JD=',F10.1,1X/'  IT DOES NOT MATCH ',&
     &'THE ONE FROM YOUR DATA : DATE= ',F8.1,2X,'JD=',F10.1,1X/,  &
     &' GEODYN TAKES THE ONE FROM YOUR DATA AS IT REFEFENCE TIME.'/,  &
     &' ALSO SEE LAST LINE OF THIS OUTPUT')
86000 FORMAT(' EXECUTION TERMINATING IN RD50A1. LOCALG PARAMETERS')
86001 FORMAT(' INPUT BIT FILE SUBBLK NOT FOUND')
87000 FORMAT(' EXECUTION TERMINATING IN RD50A1 ')
87001 FORMAT(' NUMBER OF SUB BLOCKS: ',I10,' SMALLER THAN NUMBER OF ')
87002 FORMAT(' LOCAL GRAVITY PARAMETERS ')
88000 FORMAT(BZ,A6)
88005 FORMAT(' EXECUTION TERMINATING IN RD50A1. NON-ZERO PARAMETER')
88006 FORMAT(' OR SIGMA VALUE FOUND FOR EQUATORIAL FLATTENING ')
88007 FORMAT(' FOUND ON A PLANET CARD ')
      END
