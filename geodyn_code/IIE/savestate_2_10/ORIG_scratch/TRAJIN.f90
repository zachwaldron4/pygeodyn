!$TRAJIN
      SUBROUTINE TRAJIN(AA,II,LL,NSAT,NTMPB,NTMCR,MJDSTC,MJDSTE,        &
     &    ISECT,FSECTC,FSECTE,DSECT,BFTRAJ,IUNTRJ,LENDTT,LWSET)
! *******1*********2*********3*********4*********5*********6*********7**
!
! NAME  TRAJIN                          PGMR- BILL EDDY
!
! FUNCTION          WRITE OUT THE HEADER RECORD OF TRAJECTORY FILE TO
!                   THE SPECIFIED UNIT.
! INPUT PARAMETERS
!                   AA     - ARRAY FOR REAL NUMBERS
!                   II     - ARRAY FOR INTEGERS
!                   LL     - ARRAY FOR LOGICALS
!                   NSAT   - NUMBER OF SATELLITES
!                   NTMPB  - MAXIMUM BUFFER SIZE
!                   NTMCR  -
!                   MJDSTC - TRAJECTORY START DATE & TIME IN MJD SEC
!                   MJDSTE - TRAJECTORY STOP DATE & TIME IN MJD SEC
!                   ISECT  -
!                   FSECTE - FRACTIONAL SECONDS OF STOP TIME
!                   DSECT  -
!                   FSECTI -
!                   BFTRAJ - ARRAY CONTAINING TRAJECTORY INFORMATION
!                   IUNTRJ - UNIT CONTAINING TRAJECTORY INFO
!                   LENDTT -
!                   LWSET  -
!
! COMMENTS:
!
! NOTE:
!
! RESTRICTIONS:
!
! *******1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!>>>>>
!     ....XNWSAT = number of words in satellite packet
!C      PARAMETER( XNWSAT = 31.D0 )
      PARAMETER( XNWSAT = 24.D0 )
      PARAMETER( ZERO = 0.D0        )
      PARAMETER( ONE = 1.D0         )
      PARAMETER( TWO = 2.D0         )
      PARAMETER( X2043 = 2043.D0    )
      PARAMETER( X1PE6 = 1000000.D0 )
      PARAMETER( EPS = 0.0005D0     )
      PARAMETER( C1D6 = 1.0D6       )
!
!
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
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
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      COMMON/G2SINF/G2SVER,G2SDAT,G2SRTM,G2SCOM,TDFVER,TDFRTM,SETCDS,   &
     &              GPCKSM,GPDEG ,GPORD ,XG2SIN
      COMMON/GLBPAR/NARCS,NDPOLE,NDCSA,MXDEGG,MXORDG,NSRA,              &
     &              NDUMMY,MAXPAS,NXGLBP
      COMMON/TRAJA/FSECTR
      COMMON/TRAJI/MJDSTR,NTRAJC,MTRAJC,NXTRAJ
      COMMON/TRJREF/NORBTV,NORBFL,NCOFM,NXTREF
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION AA(1),II(1),LL(1),NSAT(NTRAJC),NTMPB(NTRAJC),           &
     &   NTMCR(NTRAJC),MJDSTC(NTRAJC),MJDSTE(NTRAJC),                   &
     &   ISECT(NTRAJC),FSECTC(NTRAJC),FSECTE(NTRAJC),DSECT(NTRAJC),     &
     &   LENDTT(NTRAJC),LWSET(NTRAJC),FSECOT(1),                        &
     &   IYMD(1),IHM(1),SEC(1),BFTRAJ(2048,NTRAJC),IUNTRJ(NTRAJC),      &
     &   CARDS(10)
!
      DATA STBF1/-9000000000.0D0/,STBF2/-8000000000.0D0/
      DATA XADBTF/1.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!   INITIALIZATION
      MJDSTR=-99.D0
      FSECTR=0.D0
!
!
      DO 1000 I=1,NTRAJC
!
      DO 5 J=1,2048
         BFTRAJ(J,I)=ZERO
    5 END DO
      LENDTT(I)=.FALSE.
      NTMCR(I)=0
!
!   PERFORM SOME CALCULATIONS
      TIMESP=DBLE(MJDSTE(I)-MJDSTC(I))+FSECTE(I)-FSECTC(I)
      DENOM=DBLE(ISECT(I))+DSECT(I)
      XTIME=TIMESP/DENOM+1.5D0
      NTIMET=XTIME
      BFTRAJ(1,I)=STBF1
      NC=SETCDS+EPS
      NADBTF=1+(NC-1)/200
      BFTRAJ(2,I)=DBLE(NADBTF)
      BFTRAJ(3,I)=SETCDS
      BFTRAJ(4,I)=NARC
      BFTRAJ(5,I)=NGLOBL
      BFTRAJ(6,I)=NINNER
!
      IF(.NOT.LWSET(I)) GO TO 10
      NSET=NSAT(I)
      XNSAT=II(KNSAT-1+NSET)
      BFTRAJ(7,I)=XNSAT
      GO TO 30
   10 CONTINUE
!
      NSATC=0
      DO 15 ISET=1,NSETA
      NSATC=NSATC+II(KNSAT-1+ISET)
!
      IF(NSAT(I).LE.NSATC) GO TO 20
   15 END DO
      write(6,12345) NSAT(I),NSATC
12345 FORMAT(' PROGRAM TERMINATING IN TRAJIN: ',                        &
     &    'ISAT = ',I10,'    TOTAL NUMBER OF SATS IN SET = ',I10)
      STOP
   20 CONTINUE
!
      NSET=ISET
      XNSAT =ONE
      BFTRAJ(7,I)=XNSAT
!
   30 CONTINUE
!
!     ....number of words per satellite trajectory packet
      BFTRAJ(8,I)=XNWSAT
!     ....total number of words per time point
      BFTRAJ(9,I)=XNSAT*XNWSAT
!
      XNTMBF=X2043/(BFTRAJ(9,I)+TWO)
      NTIMBF=XNTMBF
      NTMPB(I)=NTIMBF
!
!cc   write(6,*) 'trajin: i, ntmpb(i), ntimbf ',
!cc  1                    i, ntmpb(i), ntimbf
!
!     ....number of time points in this buffer
      BFTRAJ(10,I)=DBLE(NTIMBF)
      CALL UTCET(.FALSE.,1,MJDSTC(I),FSECTC(I),FSECOT,AA(KA1UT))
      CALL YMDHMS(MJDSTC(I),FSECOT,IYMD,IHM,SEC,1)
      SEC1=AINT( SEC(1) )
      BFTRAJ(11,I)= SEC1 + DBLE(100*IHM(1))+X1PE6*DBLE(IYMD(1))
      BFTRAJ(12,I)=SEC(1) - SEC1
      CALL UTCET(.FALSE.,1,MJDSTE(I),FSECTE(I),FSECOT,AA(KA1UT))
      CALL YMDHMS(MJDSTE(I),FSECOT,IYMD,IHM,SEC,1)
      SEC1=AINT( SEC(1) )
      BFTRAJ(13,I)= SEC1 + DBLE(100*IHM(1))+X1PE6*DBLE(IYMD(1))
      BFTRAJ(14,I)=SEC(1) - SEC1
!     mjds from default reference time
      BFTRAJ(15,I)=DBLE(MJDSTC(I)) + REPDIF
      BFTRAJ(16,I)=FSECTC(I)
      BFTRAJ(17,I)=DBLE(MJDSTE(I)) + REPDIF
      BFTRAJ(18,I)=FSECTE(I)
      BFTRAJ(19,I)=DBLE(ISECT(I))+DSECT(I)
      XNMNT=BFTRAJ(17,I)+BFTRAJ(18,I)-BFTRAJ(15,I)-BFTRAJ(16,I)
      XNMNT=XNMNT/BFTRAJ(19,I)+ONE+EPS
      NMNT=XNMNT
      BFTRAJ(20,I)=DBLE(NMNT)
!
!     BFTRAJ(21,I)=SHOULD BE ALWAYS ZERO (TVM COMPATIBILITY)
!     BFTRAJ(22,I)=REF FRAME OUTPUT 0=TOD 1=TOR 2=MJ2000
!
      BFTRAJ(21,I)=0.D0
      BFTRAJ(22,I)=DBLE(NORBFL)
      BFTRAJ(23,I)= 0.0D0
!
! SPARES
!
! SECOND GROUP
!
      BFTRAJ(101,I)=VLIGHT
      BFTRAJ(102,I)=GM
      BFTRAJ(103,I)=AE
      BFTRAJ(104,I)=FE
      BFTRAJ(105,I)=GPCKSM
      NMAX=II(KNMAX-1+NSET)
      BFTRAJ(106,I)=DBLE(MXDEGG)
      BFTRAJ(107,I)=DBLE(MXORDG)
!
! SPARES
!
! THIRD GROUP
!
!     ....switches to indicate which data items are written out in
!     ....the satellite data packet
!     ....right now, only the first 20 words of satellite packet are
!     ....filled
!
      NWSAT = XNWSAT
      DO 200 JJ=1,NWSAT
      if( jj .le. 24 ) then
         BFTRAJ(200+JJ,I)=ONE
      else
         BFTRAJ(200+JJ,I)=ZERO
      endif
  200 END DO
!
! SPARES
!
! FOURTH GROUP
!
      IF(LWSET(I)) GO TO 300
      BFTRAJ(301,I)=DBLE(NSAT(I))
      GO TO 360
  300 CONTINUE
      NSATC=-1
      IF(NSAT(I).GT.1) GO TO 320
      MSET=NSAT(I)-1
      DO 310 ISET=1,MSET
      NSATC=NSATC+II(KNSAT-1+ISET)
  310 END DO
  320 CONTINUE
      JMAX=BFTRAJ(7,I)+EPS
      DO 350 J=1,JMAX
      BFTRAJ(300+J,I)=II(KISATN+NSATC+J)
  350 END DO
  360 CONTINUE
!
! SPARES
!
! FIFTH GROUP
!
!     ....TOPEX yaw steering mode beta prime threshold angles
!
!     BFTRAJ(401,I) = beta11
!     BFTRAJ(402,I) = beta12
!     BFTRAJ(403,I) = beta13
!     BFTRAJ(404,I) = beta14
!     BFTRAJ(405,I) = beta15
!     BFTRAJ(406,I) = beta16
!
!     BFTRAJ(407,I) = beta21
!     BFTRAJ(408,I) = beta22
!     BFTRAJ(409,I) = beta23
!     BFTRAJ(410,I) = beta24
!     BFTRAJ(411,I) = beta25
!     BFTRAJ(412,I) = beta26
!
!     BFTRAJ(413,I) = beta31
!     BFTRAJ(414,I) = beta32
!     BFTRAJ(415,I) = beta33
!     BFTRAJ(416,I) = beta34
!     BFTRAJ(417,I) = beta35
!     BFTRAJ(418,I) = beta36
!
!     BFTRAJ(419,I) = beta41
!     BFTRAJ(420,I) = beta42
!     BFTRAJ(421,I) = beta43
!     BFTRAJ(422,I) = beta44
!     BFTRAJ(423,I) = beta45
!     BFTRAJ(424,I) = beta46
!
! WRITE OUT INFO TO TERM VIEW
!
      WRITE(IOUT9,9010) IUNTRJ(I)
      JMAX=BFTRAJ(7,I)+EPS
      WRITE(IOUT9,9020) JMAX
      WRITE(IOUT9,9030)
      WRITE(IOUT9,9040) (BFTRAJ(300+J,I),J=1,JMAX)
      WRITE(IOUT9,9050) BFTRAJ(11,I)
      WRITE(IOUT9,9060) BFTRAJ(13,I)
      WRITE(IOUT9,9070) BFTRAJ(19,I)
!
!   WRITE OUT FIRST BUFFER
!
      IUNIT=IUNTRJ(I)
      WRITE(IUNIT) (BFTRAJ(J,I),J=1,2048)
!
!>>> debug
!cc   WRITE(6,*) 'TRAJIN: FIRST BUFFER '
!cc   WRITE(6,6678) (IJJJ, (BFTRAJ(IJJJ+IKKK,I),IKKK=0,3),
!cc  1                        IJJJ=1,2048,4)
 6678 FORMAT(' BFTRAJ: '/(1X,I4,1X,4D18.8))
!<<< debug
!
!   START ON SECOND BUFFER
!
      IANSEQ=0
      NCR=NC
      REWIND IUNT14
  500 CONTINUE
      NCRTB=MIN(200,NCR)
      NCR=NCR-200
      IANSEQ=IANSEQ+1
      BFTRAJ(1,I)=STBF2
      BFTRAJ(2,I)=IANSEQ
      XI3=AINT( G2SRTM/1000000.D0 )
      XI4=MOD( G2SRTM, 1000000.D0 )
!
!      write(6,*) 'g2s run date, time ', xi3, xi4
!
!     ....G2S RUN DATE
      BFTRAJ(3,I)= XI3
!     ....G2S RUN TIME
      BFTRAJ(4,I)= XI4
!     ....GEODYN 2S version number
      BFTRAJ(5,I)=G2SVER
!     ....GEODYN 2E version number
      BFTRAJ(6,I)=G2EVER
!
!  GET DATE AND TIME OF RUN (G2E) FOR HEADER IN TRAJECTORY FILE
!
      CALL SYSTIM
!
!      write(6,*) 'g2e run date, time ', xi7, xi8
!
!     ....G2E RUN DATE
      BFTRAJ(7,I)=G2EDAT
!     ....G2E RUN TIME
      BFTRAJ(8,I)=G2ERTM
!
!
!     ....read setup cards and write into second buffer
!
      DO 600 J=1,NCRTB
      READ(IUNT14,5000) CARDS
      IPT=48+(J-1)*10
      DO 550 JJ=1,10
      BFTRAJ(IPT+JJ,I)=CARDS(JJ)
  550 END DO
  600 END DO
!
!   WRITE OUT SECOND BUFFER
!
      WRITE(IUNIT) (BFTRAJ(J,I),J=1,2048)
!
!>>> debug
!cc   WRITE(6,*) 'TRAJIN: SECOND BUFFER '
!cc   WRITE(6,6678) (IJJJ, (BFTRAJ(IJJJ+IKKK,I),IKKK=0,3),
!ccc 1                        IJJJ=1,45,4)
!
!      DO 6671 J=1,NCRTB
!         IPT=48+(J-1)*10
!         do 6672 jj=1,10
!         cards(jj) =        bftraj(ipt+jj,i)
!6672     continue
!
!c       write(6,6679) j,(  bftraj(ipt+jj,i)  ,jj=1,10)
!c       write(6,6679) j,( cards(jj),jj=1,10)
 6679    FORMAT(' BFTRAJ: CARDS ',1X,I4,1X,10A8)
 6671 CONTINUE
!<<< debug
!
      IF(NCR.GT.0) GO TO 500
!
 1000 END DO
      REWIND IUNT14
      RETURN
 5000 FORMAT(BZ,10A8)
 9010 FORMAT('- A TRAJECTORY FILE IS BEING WRITTEN TO UNIT ',I6)
 9020 FORMAT('  THERE WILL BE ',I5,' SATELLITES ON THIS FILE')
 9030 FORMAT('  THEIR IDS ARE AS FOLLOWS ')
 9040 FORMAT(' ',5D25.16)
 9050 FORMAT('  START TIME ',F15.0)
 9060 FORMAT('  STOP  TIME ',F15.0)
 9070 FORMAT('  INTERVAL IN SECONDS ',F15.3)
      END
