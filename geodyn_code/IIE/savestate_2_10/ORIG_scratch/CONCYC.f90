!$CONCYC
      SUBROUTINE CONCYC(PMPP,RESID,WT,NP,NRMTOT,NM,ATPA,ATPL,SCRTCH,    &
     &                  PARMVC,AA,LAVOID)
!********1*********2*********3*********4*********5*********6*********7**
! GA9PCN
!
! FUNCTION:  CALL SUMNM WITH THE RIGHT CONSTRAINT INFORMATION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPP     I         FULL ARRAY OF PARTIALS(INCLUDING ZEROS WHERE
!                      NECCESSARY) FOR EACH MEASUREMENT.SOMETIMES A
!                      INCLUDES ONLY ARC,SOMTIMES ARC&COMMON.
!   RESID    I         RESIDUAL ARRAY
!   WT       I         MEASUREMENT WEIGHT ARRAY
!   NP       I         NUMPER OF PARAMETERS
!   NRMTOT   I         MAXIMUM DIMENSION OF NORMAL MATRIX
!   NM       I         NUMBER OF MEASUREMENTS
!   ATPA     O         NORMAL MATRIX
!   ATPL     O         RIGHT HAND SIDE OF NORMAL EQUATIONS
!   SCRTCH       A     SCRATCH ARRAY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
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
      COMMON/ZPARMB/MPARMZ,ITPRMZ(1000),ISTABZ(3,1000),                 &
     &             ISATBZ(3,1000),IPRMPY(1000),IPRMZ(1000),IUSECY(1000)
      COMMON/ZSIGB /PARVRZ(1000)
!
      DIMENSION PMPP(NP),RESID(1),WT(1),ATPA(1),ATPL(NP),SCRTCH(NP)
      DIMENSION PARMVC(NP)
      DIMENSION AA(1)
      DIMENSION LAVOID(1)
!
!
      DATA ZERO/0.0D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
      DO 1000 ICB=1,NCYCB
!
      IF(IUSECY(ICB).LE.0) GO TO 1000
      DO KK=1,NP
        LAVOID(KK)=.TRUE.
        PMPP(KK)=ZERO
      ENDDO
!
      J1=IPVAL0(IXBISA)-1+NAMBBH+ICB
      IF(IPRMPY(ICB).GT.0) THEN
        K2P=IPVAL0(IXBISA)-1+IPRMPY(ICB)
        K2=IPRMPY(ICB)
        IF(NAMBB.LE.0) K2=K2P
      ENDIF
      IF(IPRMZ(ICB).GT.0) THEN
        K2P=IPRMZ(ICB)
        K2=IPRMZ(ICB)
      ENDIF
!
      WT(1)=PARVRZ(ICB)
      RESID(1)=PARMVC(K2P)-PARMVC(J1)
      LAVOID(K2)=.FALSE.
      LAVOID(J1)=.FALSE.
      PMPP(K2)=-ONE
      PMPP(J1)=ONE
!
      CALL SUMNM(PMPP,RESID,WT,NP,NRMTOT,1,ATPA,ATPL,SCRTCH,LAVOID)
 1000 END DO
      RETURN
      END
