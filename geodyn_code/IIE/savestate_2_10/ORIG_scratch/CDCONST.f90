!$CDCONST
      SUBROUTINE CDCONST(PMPP,RESID,WT,NP,NRMTOT,NM,ATPA,ATPL,SCRTCH,   &
     &                 PARMVC,LAVOID,NX,ISTART)
!********1*********2*********3*********4*********5*********6*********7**
! CDCONST
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
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CDSTAT/CDX(1000,2,3,6)
      COMMON/ICDSTA/ICDX(1000,3,6)
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
!
      DIMENSION PMPP(NP),RESID(1),WT(1),ATPA(1),ATPL(NP),SCRTCH(NP)
      DIMENSION PARMVC(NP)
      DIMENSION LAVOID(1)
      DIMENSION FSEC(1),IYMD(1),IHM(1),SEC(1)
      DIMENSION NX(6)
!
!
      DATA ZERO/0.0D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************




      WRITE(6,*)'******************************************************'
      WRITE(6,*)'       CONSTRAINED DSTATE REPORT                      '
      WRITE(6,*)'******************************************************'
      WRITE(6,2222)WT(1)
 2222 FORMAT(8X,'WEIGHT :  ',D18.6)
      WRITE(6,*)'******************************************************'
      WRITE(6,*)'                                                 '


 1000  CONTINUE

           DO IDIR=1,3

           DO IJ=1,6


              DO  KK=1,NX(IJ)-1

              DO  JJ=KK+1,NX(IJ)

              RATIO=CDX(KK,2,IDIR,IJ)/CDX(JJ,2,IDIR,IJ)
              RESID(1)= CDX(KK,1,IDIR,IJ)-CDX(JJ,1,IDIR,IJ)*RATIO

              DO 60 KJ=1,NP
              LAVOID(KJ)=.TRUE.
              PMPP(KJ)=ZERO
   60         CONTINUE

      WRITE(6,*)' CONSTRAINING PARAMETER ',ICDX(KK,IDIR,IJ),'WITH',     &
     &' PARAMETER ',ICDX(JJ,IDIR,IJ), 'DIRECTION ',IDIR,' AXIS ',IJ
      WRITE(6,*)' WITH CURRENT VALUES ',CDX(KK,1,IDIR,IJ),' AND ',      &
     &            CDX(JJ,1,IDIR,IJ)
      WRITE(6,*)' AND DURATION IN SEC ',CDX(KK,2,IDIR,IJ),' AND ',      &
     &            CDX(JJ,2,IDIR,IJ)
      PROD1=CDX(KK,1,IDIR,IJ)*CDX(JJ,2,IDIR,IJ)
      PROD2=CDX(JJ,1,IDIR,IJ)*CDX(KK,2,IDIR,IJ)
      WRITE(6,*)' VERIFICATION: VAL1 X T2 = ',PROD1,' VAL2 X T1 = ',    &
     &     PROD2
      WRITE(6,*)'                                                 '

              LAVOID(ICDX(KK,IDIR,IJ))=.FALSE.
              LAVOID(ICDX(JJ,IDIR,IJ))=.FALSE.
              PMPP(ICDX(KK,IDIR,IJ))=-ONE
              PMPP(ICDX(JJ,IDIR,IJ))=RATIO
              CALL SUMNM(PMPP,RESID,WT,NP,NRMTOT,1,ATPA,ATPL,SCRTCH,    &
     &                   LAVOID)

                !    DO JJ=KK+1,NX(1)
      ENDDO
                !    DO KK=1,NX(1)-1
      ENDDO

                     ! IJ=1,3
           ENDDO
                     ! IDIR=1,3
           ENDDO

      RETURN
      END
