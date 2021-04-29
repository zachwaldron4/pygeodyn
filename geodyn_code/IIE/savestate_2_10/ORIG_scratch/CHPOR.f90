!$CHPOR
      SUBROUTINE CHPOR(PPOR,PMPA,NM,NDM1,NDM2,LNPNM,                    &
!... Begin TJS...
!... &           TIME,LAVOID,ANGWT,WT,PPER)
     &           TIME,LAVOID,ANGWT,WT,PPER,DADA0,DADIC,IAST)
!... End TJS...
!********1*********2*********3*********4*********5*********6*********7**
! CHPOR              00/00/00            0000.0    PGMR -DAVE ROWLANDS
!
! FUNCTION:
!              SUM IN THE GEOMETRIC PARTIALS OF PLANETARY OR (INCLUDING
!              LINEAR,QUADRATIC AND PERIODIC) FOR THE CASE (FOR EXAMPLE,
!              OPTICAL CONSTRAINTS)
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!     PPOR          GEO PARTIALS OF MEASUREMENT WRT, RA,DEC ANF HR ANG
!     PMPA          FULL ARRAY OF MEASUREMENT PARTIALS
!     NM            NUMBER OF MEASUREMENTS TO BE PROCESSED
!     NDM1          FIRST DIMENSION OF PPMPA ARRAY (NP IF LNPNM IS TRUE
!                   NM OTHERWISE)
!     NDM2          SECOND DIMENSION OF PPMPA ARRAY
!     LNPNM         IF TRUE PARTIALS HAVE NP  AS FIRST DIMENSION
!     TIME          TIMES AT WHICH THE ARTIALS ARE BEING APPLIED
!     LAVOID        ARRAY TO ACCOUNT IF PARAMTER APPLIES TO THIS MEAS
!     ANGWT         ARRAY OF FREQUENCIES FOR PLANETARY ROTATION
!     WT            ARRAY OF ANGLES WT
!     PPER          ARRAY OF PERIODS
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DPLNOR/DXGEO1,DXGEO2,DXFRC1,DXFRC2,XPLNOR
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
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      COMMON/XTRAOR/T0XTRO,T0XTRO2,CVENUS,CLIBMN,CMARS,CMERC,XXTRAO
!... Begin TJS...
      COMMON/AXIS/LINTAX
!... End TJS...
!
      DIMENSION PPOR(3,NM)
      DIMENSION PMPA(NDM1,NDM2)
      DIMENSION TIME(NM)
      DIMENSION LAVOID(1)
      DIMENSION ANGWT(1),WT(1),PPER(1)
!... Begin TJS...
      DIMENSION DADA0(3,6,NM),DADIC(3,6,NM)
!... End TJS...

!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      NOP=NPVAL0(IXXTRO)
      IPT=IPVAL0(IXXTRO)
!======================== ATTENTION !!! ============================
!...Begin TJS
! Variable IXMOMI introduced as pointer to partials in PMPA that
! correspond to the independent elements of I_c, the moment of
! inertia tensor in the body-fixed system, and it must be placed
! into common block NPCOMX, etc.
!     KPT=IPVAL0(IXMOMI)
!     KPT=IPVAL0(IXFGFM)
      KPT = IPVAL0(IXXTRO)-1 + NXTRAC-NMINRT
      KPT = IPVAL0(IXXTRO)-1 + NOP-NMINRT
      IF(LBINAST) KPT = IPVAL0(IXXTRO)-1 + NOP/2-NMINRT
!...End TJS
!======================== ATTENTION !!! ============================
      IF(IAST.GT.1) THEN
        NOP=NPVAL0(IXXTRO)/2
        IPT=IPT+NOP
        KPT=KPT+NOP
        IF(.NOT.LINTAX) THEN
        IPT=IPVAL0(IXXTRO)+(NPVAL0(IXXTRO)-6)/2
        ENDIF
      ENDIF
      DO IQP=1,NOP
        LAVOID(IPT-1+IQP)=.FALSE.
      ENDDO
!
      IF(LNPNM) GO TO 300
      DO 100 I=1,NM
!...Begin TJS
         IF (LINTAX) THEN
            PTMP=PPOR(1,I)*DADA0(1,1,I)+                                &
     &           PPOR(2,I)*DADA0(2,1,I)+                                &
     &           PPOR(3,I)*DADA0(3,1,I)
            PMPA(I,IPT  )=PMPA(I,IPT  )+PTMP
            PTMP=PPOR(1,I)*DADA0(1,4,I)+                                &
     &           PPOR(2,I)*DADA0(2,4,I)+                                &
     &           PPOR(3,I)*DADA0(3,4,I)
            PMPA(I,IPT+1)=PMPA(I,IPT+1)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,2,I)+                                &
     &           PPOR(2,I)*DADA0(2,2,I)+                                &
     &           PPOR(3,I)*DADA0(3,2,I)
            PMPA(I,IPT+2)=PMPA(I,IPT+2)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,5,I)+                                &
     &           PPOR(2,I)*DADA0(2,5,I)+                                &
     &           PPOR(3,I)*DADA0(3,5,I)
            PMPA(I,IPT+3)=PMPA(I,IPT+3)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,3,I)+                                &
     &           PPOR(2,I)*DADA0(2,3,I)+                                &
     &           PPOR(3,I)*DADA0(3,3,I)
            PMPA(I,IPT+4)=PMPA(I,IPT+4)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,6,I)+                                &
     &           PPOR(2,I)*DADA0(2,6,I)+                                &
     &           PPOR(3,I)*DADA0(3,6,I)
            PMPA(I,IPT+5)=PMPA(I,IPT+5)+PTMP
            DO J=1,6
               PTMP=PPOR(1,I)*DADIC(1,J,I)+                             &
     &              PPOR(2,I)*DADIC(2,J,I)+                             &
     &              PPOR(3,I)*DADIC(3,J,I)
               PMPA(I,KPT+J)=PMPA(I,KPT+J)+PTMP
            END DO

         ELSE
!...End TJS
!
      SCAL=TIME(I)-T0XTRO
      SCALSQ=SCAL**2.D0
      PMPA(I,IPT)=PMPA(I,IPT)+PPOR(1,I)
      PMPA(I,IPT+1)=PMPA(I,IPT+1)+PPOR(1,I)*SCAL
      PMPA(I,IPT+2)=PMPA(I,IPT+2)+PPOR(2,I)
      PMPA(I,IPT+3)=PMPA(I,IPT+3)+PPOR(2,I)*SCAL
      PMPA(I,IPT+4)=PMPA(I,IPT+4)+PPOR(3,I)
      PMPA(I,IPT+5)=PMPA(I,IPT+5)+PPOR(3,I)*SCAL
      PMPA(I,IPT+6)=PMPA(I,IPT+6)+PPOR(3,I)*SCALSQ
!
!
!     NXALLC IS THE NUMBER OF PAIRS FOR PERIODIC TERMS
!     FOR THE SIMPLE MODEL WITH ONE FREQUENCY NXALLC=1
      JPT=IPT+7
      K=1
      DO IJ=1,NXALLC
        ANGWT(IJ)=TWOPI/PPER(IJ)
        WT(IJ)=ANGWT(IJ)*SCAL
        WT(IJ)=MOD(WT(IJ),TWOPI)
        PMPA(I,JPT)=PMPA(I,JPT)+PPOR(K,I)*COS(WT(IJ))
        JPT=JPT+1
        PMPA(I,JPT)=PMPA(I,JPT)+PPOR(K,I)*SIN(WT(IJ))
        JPT=JPT+1
        K=K+1
        IF(K.GT.3) K=1
      END DO
!...Begin TJS
      END IF
!...End TJS
  100 END DO
      RETURN
!
  300 CONTINUE
      DO 400 I=1,NM
!...Begin TJS
         IF (LINTAX) THEN
            PTMP=PPOR(1,I)*DADA0(1,1,I)+                                &
     &           PPOR(2,I)*DADA0(2,1,I)+                                &
     &           PPOR(3,I)*DADA0(3,1,I)
            PMPA(IPT,I  )=PMPA(IPT,I  )+PTMP
            PTMP=PPOR(1,I)*DADA0(1,4,I)+                                &
     &           PPOR(2,I)*DADA0(2,4,I)+                                &
     &           PPOR(3,I)*DADA0(3,4,I)
            PMPA(IPT+1,I)=PMPA(IPT+1,I)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,2,I)+                                &
     &           PPOR(2,I)*DADA0(2,2,I)+                                &
     &           PPOR(3,I)*DADA0(3,2,I)
            PMPA(IPT+2,I)=PMPA(IPT+2,I)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,5,I)+                                &
     &           PPOR(2,I)*DADA0(2,5,I)+                                &
     &           PPOR(3,I)*DADA0(3,5,I)
            PMPA(IPT+3,I)=PMPA(IPT+3,I)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,3,I)+                                &
     &           PPOR(2,I)*DADA0(2,3,I)+                                &
     &           PPOR(3,I)*DADA0(3,3,I)
            PMPA(IPT+4,I)=PMPA(IPT+4,I)+PTMP
            PTMP=PPOR(1,I)*DADA0(1,6,I)+                                &
     &           PPOR(2,I)*DADA0(2,6,I)+                                &
     &           PPOR(3,I)*DADA0(3,6,I)
            PMPA(IPT+5,I)=PMPA(IPT+5,I)+PTMP
            DO J=1,6
               PTMP=PPOR(1,I)*DADIC(1,J,I)+                             &
     &              PPOR(2,I)*DADIC(2,J,I)+                             &
     &              PPOR(3,I)*DADIC(3,J,I)
               PMPA(KPT+J,I)=PMPA(KPT+J,I)+PTMP
            END DO

         ELSE
!...End TJS
!
      SCAL=TIME(I)-T0XTRO
      SCALSQ=SCAL**2.D0
      PMPA(IPT,I)=PMPA(IPT,I)+PPOR(1,I)
      PMPA(IPT+1,I)=PMPA(IPT+1,I)+PPOR(1,I)*SCAL
      PMPA(IPT+2,I)=PMPA(IPT+2,I)+PPOR(2,I)
      PMPA(IPT+3,I)=PMPA(IPT+3,I)+PPOR(2,I)*SCAL
      PMPA(IPT+4,I)=PMPA(IPT+4,I)+PPOR(3,I)
      PMPA(IPT+5,I)=PMPA(IPT+5,I)+PPOR(3,I)*SCAL
      PMPA(IPT+6,I)=PMPA(IPT+6,I)+PPOR(3,I)*SCALSQ
!
!
!     NXALLC IS THE NUMBER OF PAIRS FOR PERIODIC TERMS
!     FOR THE SIMPLE MODEL WITH ONE FREQUENCY NXALLC=1
      JPT=IPT+7
      K=1
      DO IJ=1,NXALLC
        ANGWT(IJ)=TWOPI/PPER(IJ)
        WT(IJ)=ANGWT(IJ)*SCAL
        WT(IJ)=MOD(WT(IJ),TWOPI)
        PMPA(JPT,I)=PMPA(JPT,I)+PPOR(K,I)*COS(WT(IJ))
        JPT=JPT+1
        PMPA(JPT,I)=PMPA(JPT,I)+PPOR(K,I)*SIN(WT(IJ))
        JPT=JPT+1
        K=K+1
        IF(K.GT.3) K=1
      END DO
!...Begin TJS
      END IF
!...End TJS
  400 END DO
      RETURN
      END
