!$PROUD
      SUBROUTINE PROUD(PF,DLAT,DLON,EL,CL,P,LSDATA,NM,MODIND,NPF,NMESH, &
     &                 SSTS,PARMV,PMPA,NDIM1,NDIM2,IPTRUA,IPTRAU,OBS)
!********1*********2*********3*********4*********5*********6*********7**
! PROUD            00/00/00            0000.0    PGMR -  R. RAY
!                                                MODIF.  D. PAVLIS
!
! FUNCTION:  EVALUATION OF PROUDMAN FUNCTIONS AT A GIVEN LOCATION.
!            fIRST CALL REQUIRES READS FROM UNIT 'IUN'.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!   PF       O    A    ARRAY OF LENGTH NPF TO HOLD ThE EVALUATED
!                      PROUDMAN FUNCTIONS.
!
!   DLAT     I    A    ARRAY OF LATITUDES OF SUB-SATELLITE POINT
!                      WHERE FUNCTIONS ARE TO BE EVALUATED
!   DLON     I    A    ARRAY OF LONGITUDES OF SUB-SATELLITE POINT
!                      WHERE FUNCTIONS ARE TO BE EVALUATED
!
!   LSDATA   O    A    LOGICAL ARRAY FOR DATA EDITING
!
!   MODIND   I     S   INDEX FOR MODEL USED
!                      =1 SEA SURFACE TOPOGRAPHY
!                      =2 OCEAN TIDE
!                      DESIRED LOCATION.  iF FALSE, PF ARRAY IS NOT CHAN
!
! COMMENTS:
!   R. Ray     8 Nov 1990
!      - reduced minimum allowed SUM2 to allow for more border data.
!      - changed code handling polygon edits.
!   R. Ray    12 Apr 1991
!      - fixed bug handling multiple polygon edits.
!   R. Ray     8 Jun 1992
!      - allow for wrap-around in global grids.
!   R. Ray    18 Aug 1993
!      - added arguments NMESH & CRAY, and code for converting IEEE/Cray
!      - use direct access reads for input data.
!      - inserted a unit-5 read to get dataset name.
!   R. Ray    05 Nov 1993
!      - modified do-loop 90 to allow vectorization.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      LOGICAL LWRAP
      SAVE
!
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CEDIT /EDITX ,EDTRMS,EDLEVL,CONVRG,GLBCNV,EBLEVL,EDITSW,   &
     &              ENPX  ,ENPRMS,ENPCNV,EDBOUN,FREEZI,FREEZG,FREEZA,   &
     &              XCEDIT
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CIOTMD/NOTPT,NOTPRF,NODEG,NXCIOT
      COMMON/CISSTM/NSSTPT,NSSTF,NSDEG,NXCISS
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
!
!   NPF IS NSSTF OR NOTPRF DEPENDING ON THE MODEL
!   CANNOT BE GREATER THAN PARAMETER MXF AS SET BELOW.
!   SHOULD NOT BE CHANGED AFTER FIRST CALL.
!   N.B.:  Parameter MXF is the largest NPF expected.
!
!   NMESH IS NSSTPT OR NOTPT DEPENDING ON THE MODEL
!
      PARAMETER (MXP=8608,MXF=1848,RAD=1.745329252D-2)
      DIMENSION OBS(NM)
      DIMENSION JP(200,100)
      DIMENSION PARMV(1)
      DIMENSION IPTRUA(1),IPTRAU(1)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION SSTS(NM)
      DIMENSION P(NPF,1)
      DIMENSION DLAT(NM),DLON(NM)
      DIMENSION PF(NPF,1)
      DIMENSION LSDATA(1)
      DIMENSION EL(1),CL(1)
      DOUBLE PRECISION LATMIN,LATMAX,LONMIN,LONMAX
      DATA LINIT/.TRUE./
      DATA LATMIN,LATMAX,LONMIN,LONMAX/1.D3,-1.D3,1.D3,-1.D3/
      DATA DX,DY/1.D4,1.D4/
!
! INITIALIZE POINTER FOR PARTIALS
      IBEGF=IPVAL0(IXSSTF)
      DO  100 I=1,NPF
      DO  100 J=1,NM
      PF(I,J)=0.D0
  100 CONTINUE
!
! INITIALIZE EDITING ARRAY
      DO 150 I=1,NM
      LSDATA(I)=.TRUE.
  150 END DO
      IF(LINIT) THEN
      LINIT=.FALSE.
      IF(NPF.GT.MXF) THEN
      WRITE(6,4) NPF,MXF
    4 FORMAT(' Subroutine PROUD: number of requested functions',I5,     &
     &       ' is greater than parameter MXF',I5)
      STOP 364
      ENDIF
      IF(NMESH.GT.MXP) THEN
      WRITE(6,6) NMESH,MXP
    6 FORMAT(' Subroutine PROUD: number of expected mesh points',       &
     &       I5,' is greater than parameter MXP',I5)
      STOP 365
      ENDIF
!
!  Read locations and Proudman functions.
!
!     DLONF is EL for tides
!     DLATF is CL for tides, 1,notp
!     DLONS is EL for sst
!     DLATS is CL for sst 1,nsstp
!
!  Skip first constant function, then save NF functions.
!
!  P(I,J) is P(J,I)
!     DO 12 IF=1,NPF
!     READ(IUN,END=112) (P(I,IF),I=1,NMESH)
!  12 CONTINUE
! 112 CONTINUE
!
!  Load pointer array JP
!
      E1=EL(1)
      C1=CL(1)
      DO 18 I=1,NMESH
!     RLAT=90.D0-CL(I)
      RLAT=CL(I)
      LATMIN=MIN(LATMIN,RLAT)
      LATMAX=MAX(LATMAX,RLAT)
      LONMIN=MIN(LONMIN,EL(I))
      LONMAX=MAX(LONMAX,EL(I))
      IF(EL(I).NE.E1) DX=MIN(DX,ABS(EL(I)-E1))
      IF(CL(I).NE.C1) DY=MIN(DY,ABS(CL(I)-C1))
   18 END DO
      WRITE(6,20) NPF,NMESH,LATMIN,LATMAX,LONMIN,LONMAX,DX,DY
   20 FORMAT(/1X,I4,' Proudman functions loaded for area:'/             &
     &        8X,'Number of mesh points in each fct:',I8/               &
     &        8X,'Lat:',2F11.3/8X,'Lon:',2F11.3/                        &
     &        8X,'Grid Interval:',F8.3,'  x',F8.3)
      NX=NINT((LONMAX - LONMIN)/DX)+1
      NY=NINT((LATMAX - LATMIN)/DY)+1
      WRITE(6,23) NX,NY
   23 FORMAT(8X,'Grid Size  NX,NY =',2I6)
      IF(NX.GT.200.OR.NY.GT.100) STOP 313
      LWRAP=ABS(LONMAX-LONMIN-360.D0).LT.1.5D0*DX.AND.                  &
     &         LONMIN.NE.(LONMAX-360.D0)
      IF(LWRAP) WRITE(6,*)'Wrap-around in longitude assumed for grid'
      DO 31 J=1,NY
      DO 31 I=1,NX
      JP(I,J)=0
   31 CONTINUE
      IREJEC=0
      DO 38 III=1,NMESH
!     RLAT=90.D0-CL(III)
      RLAT=CL(III)
      RLON=EL(III)
      II=NINT((RLON-LONMIN)/DX)+1
      JJ=NINT((RLAT-LATMIN)/DY)+1
      JP(II,JJ)=III
   38 END DO
!     do 50900 if=1,200
!     write(6,*)(jp(if,ig),ig=1,100)
!50900 continue
!-----------------------------------------------------------------------
!  SKIP STATISTICS AND PRINTOUT
!-----------------------------------------------------------------------
!     DO 60 JJJ=1,NPF
!     IF(JJJ.EQ.26) WRITE(6,44)
!  44 FORMAT(/8X,'Skip printing...'/)
!     IF(JJJ.GT.25.AND.JJJ.LT.(NPF-10)) GO TO 60
!     S1=0.D0
!     S2=0.D0
!     AR=0.D0
!     PX=-1.D30
!     PN= 1.D30
!     DO 50 J=1,NY
!     DO 50 I=1,NX
!     IF(JP(I,J).EQ.0) GO TO 50
!     WW=SIN(RAD*CL(JP(I,J)))
!     S1=S1+WW*P(JP(I,J),JJJ)
!     S2=S2+WW*P(JP(I,J),JJJ)**2
!     AR=AR+WW
!     PX=MAX(PX,P(JP(I,J),JJJ))
!     PN=MIN(PN,P(JP(I,J),JJJ))
!  50 CONTINUE
!     PMEAN=S1/AR
!      PVAR=S2/AR-PMEAN**2
!      PSTD=SQRT(PVAR)
!      PRMS=SQRT(S2/AR)
!     WRITE(6,55) JJJ,PMEAN,PRMS,PSTD,PN,PX
!  55 FORMAT(3X,'Proudman fct',I4,4X,'Mean, RMS, St.Dev, Min/Max = ',
!    $          3F11.4,2F9.3)
!  60 CONTINUE
!-----------------------------------------------------------------------
      ENDIF
!
!  For desired area, get nearest valid Proudman-fct locations.
!
      DO 20000 N=1,NM
      SLAT=DLAT(N)
      SLON=DLON(N)
! Add the following code because the convention in GEODYN for longitude
! is 0-360 instead of -180 - +180
      IF(SLON.GT.180.D0)SLON=SLON-360.D0
      IX=INT((SLON-LONMIN)/DX)+1
      IY=INT((SLAT-LATMIN)/DY)+1
      IX2=IX+1
      IF(LWRAP.AND.IX.EQ.NX) IX2=1
      IF(IX.LT.1.OR.IX2.GT.NX.OR.IY.LT.1.OR.IY.GE.NY) THEN
      LSDATA(N) = .FALSE.
      GOTO 10000
      ENDIF
      IF(JP(IX,IY  ).EQ.0.AND.JP(IX2,IY  ).EQ.0.AND.                    &
     &   JP(IX,IY+1).EQ.0.AND.JP(IX2,IY+1).EQ.0     )   THEN
      LSDATA(N)=.FALSE.
      GOTO 10000
      ENDIF
!
!  Compute weights for averaging.
!
      SX=ABS( SLON - (LONMIN+(IX-1)*DX) )/DX
      SY=ABS( SLAT - (LATMIN+(IY-1)*DY) )/DY
      W1=(1.D0-SX)*(1.D0-SY)
      W2=(     SX)*(1.D0-SY)
      W3=(1.D0-SX)*(     SY)
      W4=(     SX)*(     SY)
      IF(JP(IX, IY  ).EQ.0) W1=0.D0
      IF(JP(IX2,IY  ).EQ.0) W2=0.D0
      IF(JP(IX, IY+1).EQ.0) W3=0.D0
      IF(JP(IX2,IY+1).EQ.0) W4=0.D0
      SUM2=W1+W2+W3+W4
!     IF(W1.EQ.0.d0.or.w2.eq.0.d0.or.w3.eq.0.d0.or.w4.eq.0.d0) then
!     WRITE(6,*)' DBG SUM2 ',SUM2,W1,W2,W3,W4,SLAT,SLON
!     ENDIF
      IF(SUM2.LT.EDBOUN) THEN
!     write(6,*)' OBSERVATION EDITED SUM2 BEYOND BOUNDARIES '
      LSDATA(N)=.FALSE.
      GOTO 10000
      ENDIF
!
!  Do weighted averaging.
!
!     do 65654 if=1,npf
!     do 65654 ig=1,nmesh
!     write(6,*)p(if,ig),if,ig
!5654 continue
      IF(LSDATA(N)) THEN
      DO 90 IF=1,NPF
!     SUM1=W1*P(JP(IX,IY  ),IF)+W2*P(JP(IX2,IY  ),IF)+
!    $     W3*P(JP(IX,IY+1),IF)+W4*P(JP(IX2,IY+1),IF)
      SUM1=W1*P(IF,JP(IX,IY  ))+W2*P(IF,JP(IX2,IY  ))+                  &
     &     W3*P(IF,JP(IX,IY+1))+W4*P(IF,JP(IX2,IY+1))
      IF(W1.EQ.0.D0.or.w2.eq.0.D0.or.w3.eq.0.D0.or.w4.eq.0.D0) then
!     write(6,*)p(if,jp(ix,iy)),if,jp(ix,iy),ix,iy
!     write(6,*)p(if,jp(ix2,iy)),if,jp(ix2,iy),ix2,iy
!     write(6,*)p(if,jp(ix,iy+1)),if,jp(ix,iy+1),ix,iy+1
!     write(6,*)p(if,jp(ix2,iy+1)),if,jp(ix2,iy+1),ix2,iy+1
      endif
      IF(SUM1.EQ.0.D0) THEN
      LSDATA(N)=.FALSE.
      ENDIF
      PF(IF,N)=SUM1/SUM2
   90 END DO
      ENDIF
!     LSDATA(N)=.TRUE.
10000 CONTINUE
! Compute partials
! ATTENTION THIS NEEDS MORE WORK
      IF(.NOT.LSDATA(N)) GOTO 20000
      IF(LNPNM) THEN
      DO 200 JK=1,NPVAL0(IXSSTF)
      IND2=IPTRAU(IPVAL0(IXSSTF)+JK-1)
      IND3=IND2-IPVAL(IXSSTF)+1
      PMPA(IBEGF-1+JK,N)=PF(IND3,N)
  200 END DO
      ELSE
      DO 300 JK=1,NPVAL0(IXSSTF)
      IND2=IPTRAU(IPVAL0(IXSSTF)+JK-1)
      IND3=IND2-IPVAL(IXSSTF)+1
      PMPA(N,IBEGF-1+JK)=PF(IND3,N)
  300 END DO
      ENDIF
20000 END DO
! check multiplication
!     H=0.d0
!     do 910 i=1,nmesh
!     do 911 j=1,npf
!     H=H+parmv(ipval(ixsstf)+j-1)*p(j,i)
!911   continue
!     write(6,*)' height at grid loc ',i,H
!910   continue
! check multiplication
!     stop
! EVALUATE HEIGHTS
      DO 913 I=1,NM
      SSTS(I)=0.D0
      DO 914 J=1,NPF
      SSTS(I)=SSTS(I)+PARMV(IPVAL(IXSSTF)+J-1)*PF(J,I)
  914 END DO
  913 END DO
!     DO 7171 n=1,NM
!     IF(.NOT.LSDATA(N))THEN
!     DL=DLON(N)
!     DLA=DLAT(N)
!     DUM=OBS(N)
!     WRITE(21)DLA,DL,DUM
!     ENDIF
!7171  continue
      RETURN
      END
