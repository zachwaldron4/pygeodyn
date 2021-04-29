!$CONFORM
      SUBROUTINE CONFORM(MTYPE,NM,NADJST,NDIM1,NDIM2,XBNC,NDM1,NDM2,   &
     &   LNPNM,PXEPA,OBS,OBSSIG,PMPA,RESID,AA,LL,LAVSV,MXLASR,ISAT,ILAS)
!********1*********2*********3*********4*********5*********6*********7**
! CONFORM           00/00/00            0000.0    PGMR - ?
! FUNCTION: FORM THE CONSTRAINT EQUATION FOR BOUNCE POINTS CONSTRAINTS
!
! I/O PARAMETERS:
!  MTYPE       I        S        MEASUREMENT TYPE (110 or 111)
!  NM          I        S        NUMBER OF MEASUREMENT
!  NADJST      I        S        NUMBER OF ADJUSTED PARAMETERS
!  XBNC        I        A        THE ECF BOUNCE POINTS VECTOR FOR
!                                A PAIR OF BLOCKS
!  NDM1        I        S        FIRST DIMENSION OF PXEPA
!  NDM2        I        S        SECOND DIMENSION OF PXEPA
!  LNPNM       I        S        IF TRUE, THE FIRST DIMENSION OF PXEPA
!                                IS NADJUST, THE SECOND DIMENSION OF PXEPA
!                                IS NMMAX. IF FALSE OTHERWISE.
!  PXEPA       I        A        PARTIALS OF ECF BOUNCE POINTS WRT PARAMETERS
!  OBS         I        A        THE CONSTRAINT OBSERVATIONS
!  OBSSIG      I        A        THE CONSTRAINT SIGMAS
!  PMPA        O        A        THE PARTIALS OF COMPUTED OBSERVATIONS WRT
!                                PARAMETERS
!  RESID       O        A        RESIDUALS DEFINED AS OBS-OBSC
!  AA         I/O       A
!  LL         I/O       A
!  LAVSV       I        A        SAVE LAVOID INFORMATION FOR THE PAIR
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
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
      DIMENSION XBNC(NM,3,2)
      DIMENSION PXEPA(NDM1,NDM2,3)
      DIMENSION OBSSIG(NM),RESID(NM)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION AA(1),LL(1)
      DIMENSION LAVSV(NADJST)
!
! THE ARRAYS BELOW ARE NOT DYNAMICALLY ALLOCATED. ASSUME NM=1
      DIMENSION XBN12(1,3)
      DIMENSION DIST(NM),SIGMA(NM)
      DATA NOK1/0/
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************

!     write(6,*) PXEPA(1,1,1),PXEPA(1,1,2),PXEPA(1,1,3)
!     write(6,*) PXEPA(1,2,1),PXEPA(1,2,2),PXEPA(1,2,3)
!
! CLEAR PMPA ARRAY

      DO K=1,NDIM1
      DO I=1,NDIM2
        PMPA(K,I)=0.D0
      ENDDO
      ENDDO

! CALCULATE PMPA
      IF(MTYPE.EQ.110) THEN

        DO I=1,NM
! CALCULATE THE VECTOR FROM POINT 2 TO POINT 1
          XBN12(I,1)=XBNC(I,1,1)-XBNC(I,1,2)
          XBN12(I,2)=XBNC(I,2,1)-XBNC(I,2,2)
          XBN12(I,3)=XBNC(I,3,1)-XBNC(I,3,2)
! CALCULATE THE DISTANCE BETWEEN THE TWO POINTS
          DIST(I)=XBN12(I,1)*XBN12(I,1)+XBN12(I,2)*XBN12(I,2)
          DIST(I)=DIST(I)+XBN12(I,3)*XBN12(I,3)
          DIST(I)=SQRT(DIST(I))
! CALCULATE THE RESIDUAL
! RES=OBS-OBSC
! DIST IS THE COMPUTED OBSERVATION
          RESID(I)=(OBS-DIST(I))
      write(6,*)'-----------------------------------------------------'
        write(6,21212)obs,dist(i),resid(i)
21212   format('  OBS DIST RESID ',3F15.1)
      write(6,*)'-----------------------------------------------------'
!           print*,xbnc(i,1,1),xbnc(i,2,1),xbnc(i,3,1)
!           print*,xbnc(i,1,2),xbnc(i,2,2),xbnc(i,3,2)
!           if(dist(i).eq.0.d0) stop
! CALCULATE SIGMAS FOR SUMNPF
          SIGMA(I)=1.D0/(OBSSIG(I)*OBSSIG(I))
! ZERO PROTECTION
          IF(DIST(I).LT.1.D-30) THEN
            XBN12(I,1)=0.D0
            XBN12(I,2)=0.D0
            XBN12(I,3)=0.D0
            DIST(I)=1.D0
            NOK1=NOK1+1
          ELSE
!!          DUM=1.D0/DIST(I)
            DUM=1.D0
            XBN12(I,1)=XBN12(I,1)*DUM
            XBN12(I,2)=XBN12(I,2)*DUM
            XBN12(I,3)=XBN12(I,3)*DUM
          ENDIF
        ENDDO

          NCOO=6*NSATA
! CALCULATE PMPA
      NQ=NDIM1
      IF(.NOT.LNPNM) NQ=NDIM2
      DO K=1,NQ
       PMPA(K,1)=PMPA(K,1)+((PXEPA(K,1,1)-PXEPA(K,2,1))                 &
     &                      *XBN12(1,1)                                 &
     &                     +(PXEPA(K,1,2)-PXEPA(K,2,2))                 &
     &                      *XBN12(1,2)                                 &
     &                     +(PXEPA(K,1,3)-PXEPA(K,2,3))                 &
     &                      *XBN12(1,3))/DIST(1)
      ENDDO
!!!!! END OF MEASUREMENT 110
      ELSE
        DO I=1,NM
! CALCULATE THE VECTORS RADIAL SEPARATION
!DEP modify
          D2=XBNC(I,1,2)*XBNC(I,1,2)+XBNC(I,2,2)*XBNC(I,2,2)+      &
     &       XBNC(I,3,2)*XBNC(I,3,2)
          D1=XBNC(I,1,1)*XBNC(I,1,1)+XBNC(I,2,1)*XBNC(I,2,1)+      &
     &       XBNC(I,3,1)*XBNC(I,3,1)
          D1=SQRT(D1)
          D2=SQRT(D2)
! CALCULATE THE RADIAL SEPARATION
          DIST(I)=D1-D2
! CALCULATE THE RESIDUAL
! RES=OBS-OBSC
! DIST IS THE COMPUTED OBSERVATION
          RESID(I)=(OBS-DIST(I))
      write(6,*)'-----------------------------------------------------'
        write(6,21212)obs,dist(i),resid(i)
      write(6,*)'-----------------------------------------------------'
!           print*,xbnc(i,1,1),xbnc(i,2,1),xbnc(i,3,1)
!           print*,xbnc(i,1,2),xbnc(i,2,2),xbnc(i,3,2)
!           if(dist(i).eq.0.d0) stop
! CALCULATE SIGMAS FOR SUMNPF
          SIGMA(I)=1.D0/(OBSSIG(I)*OBSSIG(I))
      ENDDO

! CALCULATE PMPA
      NQ=NDIM1
      IF(.NOT.LNPNM) NQ=NDIM2
      DO K=1,NQ
       PMPA(K,1)=PMPA(K,1)+(PXEPA(K,1,1)*XBNC(1,1,1)                    &
     &                     +PXEPA(K,1,2)*XBNC(1,2,1)                    &
     &                     +PXEPA(K,1,3)*XBNC(1,3,1))/D1                &
     &                    -(PXEPA(K,2,1)*XBNC(1,1,2)                    &
     &                     +PXEPA(K,2,2)*XBNC(1,2,2)                    &
     &                     +PXEPA(K,2,3)*XBNC(1,3,2))/D2
      ENDDO

      ENDIF
!
! SET UP LAVOID
      DO K=1,NADJST
        IF(.NOT.LAVSV(K).OR..NOT.LL(KLAVOI+K-1)) THEN
          LL(KLAVOI+K-1)=.FALSE.
        ELSE
          LL(KLAVOI+K-1)=.TRUE.
        ENDIF
      ENDDO
! DO THE SUMMATION FOR NORMAL MATRIX
      CALL SUMNPF(PMPA,RESID,SIGMA,NADJST,MAPARM,NM,                &
     &            AA(KSUM1),AA(KSUM2),AA(KPDLTA),LL(KLAVOI))
      RETURN
      END
