!$GLBBHD
      SUBROUTINE GLBBHD
!********1*********2*********3*********4*********5*********6*********7**
! GLBBHD           85/03/25            8504.0    PGMR - D. ROWLANDS
!                  89/09/20            8906.0    PGMR - J. MCCARTHY
!
!  FUNCTION:  WRITE OUT GLOBAL HEADER INFO FOR BINRES FILE
!             & CARD DECK
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CITERG/MARC,MGLOBL,MGLOBM,                                 &
     &              MGLOCV,NXITRG
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      COMMON/G2SINF/G2SVER,G2SDAT,G2SRTM,G2SCOM,TDFVER,TDFRTM,SETCDS,   &
     &              GPCKSM,GPDEG ,GPORD ,XG2SIN
      COMMON/CITERM/MAXINR,MININR,MAXLST,IHYPSW,NXITER
      COMMON/GLBPAR/NARCS,NDPOLE,NDCSA,MXDEGG,MXORDG,NSRA,              &
     &              NDUMMY,MAXPAS,NXGLBP
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
      DIMENSION CARDS(10)
      DATA ZERO/0.D0/
      DATA W12,W13/2*0.D0/
      DATA ONEM/-1.D0/,ONE/1.D0/,TWO/2.D0/,EPS/.001D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
      W1=SETCDS
      W2=DBLE(MARC)
      W3=VLIGHT
      W4=GM
      W5=AE
      W6=FE
      W7=GPCKSM
      W8=GPDEG
      W9=GPORD
      W10=DBLE(NSTA)
      M17NM=17*MINTIM
      I11=MAX(43,M17NM)
      W11=DBLE(I11)
!
!     ....IF INTERPLANETARY RUN, W12 = ONE
!     ....INTERPLANETARY DEFINITION --> ITBDGM NOT = ICBDGM,
!     ....THAT IS, TRACKING BODY IS NOT EQUAL TO THE CENTRAL BODY
!
      W12 = ZERO
      IF( ITBDGM .NE. ICBDGM ) W12 = ONE
!
      W13 = 0.0D0
!
!  Note fixes for 32 bit machine
      I14=TDFRTM/1000000
!CC   I15=TDFRTM
!CC   I15=I15-I14*1000000
      W14=DBLE(I14)
!CC   W15=DFLOAT(I15)
      W15=AINT(TDFRTM-W14)
      W16=TDFVER
      I17=G2SRTM/1000000
!CC   I18=G2SRTM
!CC   I18=I18-I17*1000000
      W17=DBLE(I17)
!CC   W18=DFLOAT(I18)
      W18=AINT(G2SRTM-W17)
      W19=G2SVER
      W20=G2EVER
      WRITE(IUNT19) W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,                 &
     &                 W12,W13,W14,W15,W16,W17,W18,W19,W20
!
!     ....WRITE NC RECORDS ON UNIT 19 CONTAINING UNIT 5 CARD INPUT
!
      REWIND IUNT14
      NC=W1+EPS
      DO 500 I=1,NC
      READ(IUNT14,5000) CARDS
      WRITE(IUNT19) CARDS
  500 END DO
      REWIND IUNT14
      RETURN
 5000 FORMAT(BZ,10A8)
      END
