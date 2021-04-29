!$TROPCN
      SUBROUTINE TROPCN(PMPP,RESID,WT,NP,NRMTOT,NM,ATPA,ATPL,SCRTCH,    &
     &                 PARMVC,INDEX,TIMES,WEIGHT,CORREL,                &
     &                 LAVOID)
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
!   INDEX    I   A     INDEX SHOWING THE CONSTRAINED PARAMETERS IN THE
!                      PARMV0 ARRAY
!   TIMES    I   A     TIMES FOR CONSTRAINED TROP BIASES
!   WEIGHT   I   A     WEIGHTS FOR CONSTRAINS
!   CORREL   I   A     CORRELATION COEFFICIENTS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CGLBAR/LGDRAG,NXGLAR
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
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
      COMMON/MBIAST/MBTRAD,MBTRUN,ICONBIA,IPRCTP,IPREND,NXMBST
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/EMAT  /EMTNUM,EMTPRT,EMTCNT,VMATRT,VMATS,VMATFR,FSCVMA,    &
     &              XEMAT
!
      DIMENSION PMPP(NP),RESID(1),WT(1),ATPA(1),ATPL(NP),SCRTCH(NP)
      DIMENSION PARMVC(NP)
      DIMENSION INDEX(MBTRAD),TIMES(ICONBIA,2),WEIGHT(ICONBIA),         &
     &          CORREL(ICONBIA)
      DIMENSION LAVOID(1)
      DIMENSION IPTR(50)
      DIMENSION FSEC(1),IYMD(1),IHM(1),SEC(1)
!
!
      DATA ZERO/0.0D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
! DEBUG
!     WRITE(6,*)' DBG ICONBIA MBTRAD ',ICONBIA,MBTRAD
!     DO I=1,MBTRAD
!     WRITE(6,*)' DBG INDICES ',INDEX(I),I
!     ENDDO
!     DO I=1,ICONBIA
!     WRITE(6,*)' DBG TIME WEIGHT CORRELATION ',
!    .TIMES(I,1),TIMES(I,2),WEIGHT(I),CORREL(I),I
!     ENDDO
      ISTART=IPRCTP
!     WRITE(6,*)' DBG MBTRAD,IPRCTP,IPREND',MBTRAD,IPRCTP,IPREND
!     DO I=IPRCTP,IPREND
!     WRITE(6,*)' DBG CURRENT PARAMETER VALUES ',PARMVC(I)
!     ENDDO

      WRITE(6,*)'******************************************************'
      WRITE(6,*)'       CONSTRAINED TROPOSPHERIC BIASES REPORT'
      WRITE(6,*)'******************************************************'

      SCAL=EXP(1.D0)

! DEBUG
      IPARAMS=1
        K=1
        JJ=1
 1000  CONTINUE
        DO 500 I=K,MBTRAD
        IF(INDEX(I).EQ.ZERO) GOTO 500
        IF(INDEX(I).EQ.INDEX(I+1)) THEN
        IPARAMS=IPARAMS+1
        write(6,*)' dbg for I= ',I,' IPARAMS= ',IPARAMS
        GOTO 500
        ELSE
      WRITE(6,*)'                                                     '
      WRITE(6,*)' THE FOLLOWING ADJUSTED PARAMETERS WILL BE ', &
     &'CONSTRAINED TOGETHER'
      DO KK=1,IPARAMS
      WRITE(6,*)' ADJUSTED PARAM NUM: ',ISTART-2+K+KK,'WITH CUR VALUE:'&
     &,PARMVC(ISTART-2+K+KK)
      IPTR(KK)=ISTART-2+K+KK
      ENDDO

      WRITE(6,*)' TIME BEGIN / TIME END ',TIMES(JJ,1),'/',TIMES(JJ,2)
      IT1=TIMES(JJ,1)
      IT2=TIMES(JJ,2)

      CALL YMDHMS(IT1,FSEC,IYMD,IHM,SEC,1)
      IF(IYMD(1).GT.1000000)IYMD(1)=IYMD(1)-1000000
      ITAG=SEC(1)
      IHMS=IHM(1)*100+ITAG
      WRITE(6,*)' TIME BEGIN :',IYMD,IHMS

      CALL YMDHMS(IT2,FSEC,IYMD,IHM,SEC,1)
      IF(IYMD(1).GT.1000000)IYMD(1)=IYMD(1)-1000000
      ITAG=SEC(1)
      IHMS=IHM(1)*100+ITAG
      WRITE(6,*)' TIME END   :',IYMD,IHMS

      WRITE(6,*)' WEIGHT ',WEIGHT(JJ)
      WRITE(6,*)' CORREL ',CORREL(JJ)
              T2=TIMES(JJ,2)
              T1=TIMES(JJ,1)
              WTB=SCAL/(WEIGHT(JJ)*WEIGHT(JJ))
              EARG=-ABS(T2-T1)/CORREL(JJ)
              WT(1)=WTB*EXP(EARG)

              DO  KK=1,IPARAMS-1
              RESID(1)=PARMVC(IPTR(KK+1))-PARMVC(IPTR(KK))
              DO 60 KJ=1,NP
              LAVOID(KJ)=.TRUE.
              PMPP(KJ)=ZERO
   60         CONTINUE
              LAVOID(IPTR(KK))=.FALSE.
              LAVOID(IPTR(KK+1))=.FALSE.
              PMPP(IPTR(KK))= ONE
              PMPP(IPTR(KK+1))=-ONE
              CALL SUMNM(PMPP,RESID,WT,NP,NRMTOT,1,ATPA,ATPL,SCRTCH,    &
     &                   LAVOID)

                !    DO KK=1,IPARAMS
      ENDDO
        IPARAMS=1
        K=I+1
        ENDIF
  500 END DO
        JJ=JJ+1
        IF(K.GT.MBTRAD) GOTO 1000
      RETURN
      END
