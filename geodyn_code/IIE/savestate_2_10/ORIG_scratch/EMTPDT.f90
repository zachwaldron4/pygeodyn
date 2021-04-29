!$EMTPDT
      SUBROUTINE EMTPDT(SUM1,SUM2,LPRTD,PARML0,NRECS,IUNTMT)
!********1*********2*********3*********4*********5*********6*********7**
! EMTPDT           90/04/10            0000.0    PGMR - SBL
!
! FUNCTION: TO OUTPUT THE NORMAL MATRIX DURING A PARTITIONING RUN
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SUM1     I    A    ATWA PART OF THE NORMAL MATRIX
!   SUM2     I    A    ATWR PART OF THE NORMAL MATRIX
!   LPRTD    I    S    TRUE WHEN PRINT NORMAL MATRIX ON UNIT 6
!   PARML0   I    A    ADJUSTED PARAMETER LABEL ARRAY
!   NRECS    I    S    NUMBER OF RECORDS WRITTEN TO EMAT FILE
!   IUNTMT   I    S    UNIT NUMBER FOR EMAT FILE
!
! COMMENTS: IF AN EMAT IS PRESENT THEN THIS IS NOT A MULTI-ARC RUN THUS
!           THE NUMBER OF ADJUSTED PARAMETERS IS THE MAXIMUM NUMBER OF
!           ADJUSTED PARAMETERS WHICH IS MAPARM
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/EMAT  /EMTNUM,EMTPRT,EMTCNT,VMATRT,VMATS,VMATFR,FSCVMA,    &
     &              XEMAT
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
      COMMON/CPARTI/MAXLP ,KORSIZ,MAXWRK,                               &
     &              IUNTP1,IUNTP2,IUNTPF,ISIZPF,IRECPF,NRECPF,          &
     &              NPDONE,NP1   ,NP2   ,NRECND,NPDIM ,NXPARI
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DIMENSION PARML0(3,1)
      DIMENSION SUM1(1),SUM2(1)
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! OUTPUT EMATRIX MEMORY PARTITION
!
      IBEG=0
      IEND=0
      DO 1000 NROW=NP1,NP2
      ROW=NROW
      RSIZE=MAPARM-NROW+1
      NRECS=NRECS+1
      IBEG=IEND+1
      IEND=IBEG+RSIZE-1
      WRITE(IUNTMT) ROW,RSIZE,(SUM1(I),I=IBEG,IEND),SUM2(NROW)
      IF(LPRTD)  WRITE(IOUT6,80200) PARML0(1,NROW),SUM2(NROW),          &
     &          (SUM1(I),I=IBEG,IEND)
 1000 END DO
      RETURN
80200 FORMAT(1X,F16.0,1PD14.6,(T32,1P,6D17.9))
      END
