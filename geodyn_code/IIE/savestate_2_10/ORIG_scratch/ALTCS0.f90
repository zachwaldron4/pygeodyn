!$ALTCS0
      SUBROUTINE ALTCS0(PXPFNP,PXPFNM,NEQN,NM,N3,LNPNM)
!********1*********2*********3*********4*********5*********6*********7**
! ALTCS0           87/03/30            8704.0    PGMR - ?
!
! FUNCTION:  ZERO OUT PARTIALS OF SAT POS-VEL WRT C & S COEFS (THIS IS
!            IF ONE WANTS ONLY THE GEOMETRIC PORTION OF THE C & S
!            MEASURED PARTIALS)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PXPFNP  I/O   A    PARTIALS OF SAT POS-VEL WRT FORCE MODEL
!                      ZEROED IN LOCATIONS CORRESPONDING
!                      TO C &S FOR OUTPUT
!   PXPFNM  I/O   A    PARTIALS OF SAT POS-VEL WRT FORCE MODEL
!                      ZEROED IN LOCATIONS CORRESPONDING
!                      TO C &S FOR OUTPUT
!   NEQN     I    S    NUMBER OF FORCE MODEL EQUATIONS
!   NM       I    S    NUMBER OF MEASUREMENTS IN BLOCK
!   N3       I    S    NUMBER OF SATS TIMES 3
!   LNPNM    I    S    TRUE IF NP IS THE FIRST DIMENSION (NOT NM)
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PXPFNP(NEQN,NM,N3),PXPFNM(NM,NEQN,N3)
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
      DATA ZERO/0.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      NCCOEF=NPVAL0(IXGPC)
      NSCOEF=NPVAL0(IXGPS)
      ISTRTC=IPVAL0(IXGPC)
      ISTRTS=IPVAL0(IXGPS)
!
      IF(LNPNM) GO TO 1000
      IFINC=ISTRTC+NCCOEF
      IFINS=ISTRTS+NSCOEF
!
      DO 300 K=1,N3
      DO 100 J=ISTRTC,IFINC
      DO 100 I=1,NM
      PXPFNM(I,J,K)=ZERO
  100 CONTINUE
      DO 200 J=ISTRTS,IFINS
      DO 200 I=1,NM
      PXPFNM(I,J,K)=ZERO
  200 CONTINUE
  300 END DO
      RETURN
!
 1000 CONTINUE
!
      IFINC=ISTRTC+NCCOEF-1
      IFINS=ISTRTS+NSCOEF-1
      DO 1300 K=1,N3
      DO 1300 J=1,NM
      DO 1100 I=ISTRTC,IFINC
      PXPFNP(I,J,K)=ZERO
 1100 END DO
      DO 1200 I=ISTRTS,IFINS
      PXPFNP(I,J,K)=ZERO
 1200 END DO
 1300 CONTINUE
      RETURN
      END
