!$PSUBNM
      SUBROUTINE PSUBNM(ICON  ,DXSDPM,COSTHG,SINTHG,PMPXI ,XM    ,      &
     &    PXSPXP,PXSPLV,NMP   ,NADJP ,NM    ,NINTPL,                    &
     &    PMPXE ,PMPA  ,ISTA  ,LESTA ,LESTP ,ISTAMT,PXSPAB)
!********1*********2*********3*********4*********5*********6*********7**
! PSUBNM           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ICON
!   DXSDPM
!   COSTHG
!   SINTHG
!   PMPXI
!   XM
!   PXSPXP
!   PXSPLV
!   NMP
!   NADJP
!   NM
!   NINTPL
!   PMPXE
!   PMPA
!   ISTA
!   LESTA
!   LESTP
!   ISTAMT
!   PXSPAB
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CTHDOT/THDOT
      COMMON/LOLOAD/LOLMD,LOLAJ,L2FOLT,LAPLOD
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
      COMMON/OLDADJ/NPV0OL(3,3),JPV0OL(3,3),NYZ0OL(3,3),JYZ0OL(3,3),    &
     &              NEO0OL(3,3),JEO0OL(3,3)
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      DIMENSION ICON  (NSTAE ),DXSDPM(     3,     3,NSTAE ),            &
     &          COSTHG(NM    ),SINTHG(NM    ),                          &
     &          PMPXI (NM    ,     3),PMPXE (NM    ,     3),            &
     &          XM    (     3),PXSPXP(NM    ,     3,     2),            &
     &          PXSPLV(NM    ,     3,     2),                           &
     &          NMP   (NINTPL),NADJP (NINTPL),                          &
     &          PMPA  (NM    ,MAPARM),PXSPAB(NM,3,1)
      DIMENSION PXTPUT(     2)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE NEGATIVE OF CARTESIAN E.C.FIXED STATION PARTIALS
      CALL ECFIXP(PMPXI ,COSTHG,SINTHG,PMPXE ,NM    ,NM    ,NM    )
      IF(.NOT.LESTA) GO TO 2500
! ROTATE STATION PARTIALS INTO SYSTEM OF ADJUSTMENT AND
!        SUM INTO MEASUREMENT PARTIAL ARRAY KEEPING SIGN TO NEGATIVE
!
! DETERMINE MASTER STATION
      MSTA  =ICON  (ISTA  )
! CALCULATE STARTING LOCATION IN NORMAL MATRIX
      INDEX =IPVAL0(IXSTAP)+(MSTA  -1)*3
      DO 2000 J=1,3
      DO 1000 I=1,3
      DO 800 N=1,NM
      PMPA  (N,INDEX)=PMPA  (N,INDEX)+PMPXE (N,I)*DXSDPM(J,I,ISTA)
  800 END DO
 1000 END DO
      INDEX=INDEX+1
 2000 END DO
 2500 CONTINUE
      IF(.NOT.LESTP) GO TO 7500
! COMPUTE PARTIALS OF INSTANTANEOUS STATION LOCATION W.R.T. DELTA U.T.
      PXTPUT(1)= XM(2)*THDOT
      PXTPUT(2)=-XM(1)*THDOT
! CHAIN AND SUM POLAR MOTION AND UT PARTIALS WITHOUT SIGN CHANGE
      NM1   =1
      NM2   =0
      INDEX1=IPVAL0(IXPOLX)
      DO 7000 M=1,NINTPL
      NMI   =NMP   (M)
      NM2   =NM2   +NMI
      K     =NADJP (M)
      IF(K.LE.0) GO TO 6000
! CALCULATE STARTING LOCATION IN NORMAL MATRIX
      INDEX =INDEX1+(K-1)*3
! POLAR MOTION
      DO 4000 J=1,2
      DO 3000 I=1,3
      DO 2800 N=NM1,NM2
      PMPA  (N,INDEX)=PMPA  (N,INDEX)+PMPXE (N,I)*PXSPXP(N,I,J)
 2800 END DO
 3000 END DO
      INDEX=INDEX+1
 4000 END DO
! U.T.
      DO 5000 I=1,2
      DO 4800 N=NM1,NM2
      PMPA  (N,INDEX)=PMPA  (N,INDEX)+PMPXE (N,I)*PXTPUT(I)
 4800 END DO
 5000 END DO
 6000 CONTINUE
      NM1=NM2+1
 7000 END DO
 7500 CONTINUE
! STATION TIDAL DISPLACEMENT COEFFICIENTS H2 & L2
      IXLOVE=IXH2LV
      DO 9000 J=1,2
      IF(NPVAL0(IXLOVE).LE.0) GO TO 9000
      INDEX=IPVAL0(IXLOVE)
      DO 8000 I=1,3
      DO 7800 N=1,NM
      PMPA  (N,INDEX)=PMPA  (N,INDEX)+PMPXE (N,I)*PXSPLV(N,I,J)
 7800 END DO
 8000 END DO
 9000 IXLOVE=IXL2LV
! OCEAN  LOADING
      IF(.NOT.LOLAJ) GO TO 14000
      JNDEX=0
      NDADJ=0
! K --> DIRECTION E N V
      DO 13000 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NPV0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 13000
      INDEX=JPV0OL(K,ISTAMT)-1
      DO 12000 J=1,NDADJ
      INDEX=INDEX+1
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 11000 I=1,3
      DO 10000 N=1,NM
      PMPA  (N,INDEX)=PMPA  (N,INDEX)+PMPXE(N,I)*PXSPAB(N,I,JNDEX)
      PMPA  (N,INDEX+NDADJ)=PMPA  (N,INDEX+NDADJ)                       &
     &                     +PMPXE(N,I)*PXSPAB(N,I,JNDEX+NDADJ)
10000 END DO
11000 END DO
12000 END DO
13000 END DO
14000 CONTINUE
      RETURN
      END
