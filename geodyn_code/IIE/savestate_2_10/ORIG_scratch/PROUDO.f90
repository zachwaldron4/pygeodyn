!$PROUDO
      SUBROUTINE PROUDO(PF,DLAT,DLON,EL,CL,P,LSDATA,NM,NPF,NMESH,       &
     &                 HEIGHT,PARMV,PMPA,NDIM1,NDIM2,IPTRUA,IPTRAU,     &
     &                 OBS,NCON,COSAR,SINAR,MJDSBL,FSECN,TEMP,AA,II)
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
!   PF       O    A    ARRAY OF LENGTH NPF TO HOLD THE EVALUATED
!                      PROUDMAN FUNCTIONS.
!
!   DLAT     I    A    ARRAY OF LATITUDES OF SUB-SATELLITE POINT
!                      WHERE FUNCTIONS ARE TO BE EVALUATED
!   DLON     I    A    ARRAY OF LONGITUDES OF SUB-SATELLITE POINT
!                      WHERE FUNCTIONS ARE TO BE EVALUATED
!
!   EL       I    A    LONGITUDE ARRAY FOR GRID POINTS
!   CL       I    A    LATITUDE ARRAY FOR GRID POINTS
!   P        I    A    PROUDMAN FUNCTIONS
!   LSDATA   O    A    LOGICAL ARRAY FOR DATA EDITING
!   NM       I    S    NUMBER OF OBSERVATIONS IN THIS BLOCK
!   NPF      I    S    TOTAL NUMBER OF FUNCTIONS PER GRID POINT
!   NMESH    I    S    TOTAL NUMBER OF GRID POINTS
!   HEIGHT   O    A    GEOMETRIC  HEIGHT CORRECTION DUE TO TIDES
!   PARMV    I    A    ARRAY OF PARAMETERS
!   PMPA     O    A    ARRAY OF PARTIALS
!   NDIM1    I    S    FIRST DIMENSION OF PMPA
!   NDIM2    I    S    SECOND DIMENSION OF PMPA
!   IPTRUA   I    A    INDEX UNADJUSTED TO ADJUSTED PARAMETERS
!   IPTRAU   I    A    INDEX ADJUSTED TO UNADJUSTED PARAMETERS
!   OBS      I    A    ALTIMETRIC OBSERVATION ARRAY (USED FOR DEBUGGING)
!   NCON     I    A    INDICES  TO THE CONSTITUENTS PRESENT FOR THIS RUN
!   MJDSBL   I    S    TIME OF BLOCK START IN MJDSEC
!   FSECN    I    A    SECONDS ELAPSED SINCE THE BEGINNING OF THE BLOCK
!   TEMP    I/O   A    TEMPORARY ARRAY FOR CALCULATIONS
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
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
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CEDIT /EDITX ,EDTRMS,EDLEVL,CONVRG,GLBCNV,EBLEVL,EDITSW,   &
     &              ENPX  ,ENPRMS,ENPCNV,EDBOUN,FREEZI,FREEZG,FREEZA,   &
     &              XCEDIT
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CIOCON/NUMCON(15),NESCON(15),NUMIND,NXCON
      COMMON/CIOTMD/NOTPT,NOTPRF,NODEG,NXCIOT
      COMMON/CISSTM/NSSTPT,NSSTF,NSDEG,NXCISS
      COMMON/CORA04/KABIAS,KABSTR,KABSTP,KCBIAS,KCBSTR,KCBSTP,          &
     &       KRNM  ,KRKM  ,KRKJ  ,KRIJ  ,KRKPM ,                        &
     &       KRRNM ,KRRKM ,KRRKJ ,KRRIJ ,KRRKPM,                        &
     &       KURNM ,KURKM ,KURKJ ,KURIJ ,KURKPM,                        &
     &       KPRRNM,KPRRKM,KPRRKJ,KPRRIJ,KPRRKP,                        &
     &       KPMPXI,KPMPXE,KRA   ,KRELV ,KUHAT ,KWORK ,KTMPCR,KU    ,   &
     &       KS0   ,KS1   ,KXTP  ,KPXSXP,KPXDXP,KXUTDT,KRPOLE,KPXPOL,   &
     &       KXDPOL,KOBBUF,KOBSC ,KRESID,KSIGMA,KRATIO,KELEVS,          &
     &       KPMPA ,KPXSLV,KTPRTL,                                      &
     &       KFRQOF,KXYZOF,KFRQST,KFRQSA,KSTDLY,KSADLY,KXYZCG,KSCMRA,   &
     &       KEBVAL,KEBSIG,KEBNAM,KBTWB ,KTBTWA,KBTWD ,KPMPE ,          &
     &       KFSEB1,KFSEB2,KSTAMT,KPXSPA,KDIAGN,KOBOUT,KGRLT1,KGRLT2,   &
     &       KGRLT3,KGRLT4,KGRLT5,KPSTAT,KRSUNS,KCHEMR,KCHMOR,KEPOSR,   &
     &       KCHEMT,KCHMOT,KEPOST,KCHCBS,KCPOSS,KSCRTM,KXSTT ,KXSTR ,   &
     &       KXSS  ,KRANGT,KRANGR,KCHB1 ,KCHB2 ,KCHBV1,KCHBV2,KCHEB ,   &
     &       KSSTFQ,KSSTCC,KSSTSS,KSSTWT,KRLCOR,KWVLBI,KQUINF,KBRTS ,   &
     &       KBRTSV,KLRARC,KXEPBF,KXEPBR,KPRCBD,KPRTBD,KXPMPA,KXL,      &
     &       KXSIG ,KXEDTS,KXALO1,KXALO2,KXYOF2,KDLATF,KDLATS,KDLONF,   &
     &       KDLONS,KPF   ,KPS   ,                                      &
     &       KXOBSV,KXOBSW,KXEDSW,                                      &
     &       KXSIGW,KPSF  ,KDNUM ,KCNUM ,KFCOR ,KCOSAR,KSINAR,KSABIA,   &
     &       KSBTM1,KSBTM2,KYAWBS,KVLOUV,KACMAG,KOBSTR,                 &
     &       KPRL1 ,KPRL2, KRL1  ,KT1SE ,KTCP  ,                        &
     &       KRATDR,KFQT1S,KFQT1E,KT1STR,KFTTSE,KFRSTR,KFRRAT,KSV1  ,   &
     &       KSV2  ,KTSLOV,                                             &
     &       KGLGR1,KGLGR2,KGLFR1,KGLFR2,                               &
     &       KARGR1,KARGR2,KARFR1,KARFR2,                               &
     &       KRDS1L,KFT1AV,KDFQP ,KFREQ1,KFREQ3,KSAVD1,KSAVD2,          &
     &       KANTOU,KFM3CF,KF2CF,KTMG,KLTMG,KX2TIM,KX2OBS,KXRNDX,KX2SCR,&
     &       KALTWV,KXXBM ,KX2PAR,KATIME,KPMPAT,KPMATT,KX2PAT,          &
     &       KPXEXI,KPXEPA,KPV,KPXEP2,KX2COF,KACOF2,KACOF3,KBCOF,KBCOF2,&
     &       KDDDA ,KX2AUX,KX2VPR,KX2VPA,KEXTRA,KVARAY,KATROT,KATPER,   &
     &       KLTAR ,KXHOLD,KANTBL,KPHC  ,KOFDRV,KGNAME,KGRSIZ,KGRCNT,   &
     &       KGRDAT,KACCDT,KCTBTI,KCTBWE,KCTCTM,KANTUV,KANTOR,KW1PU ,   &
     &       KPYSQH,KSIGSP,KXYOF3,KXVTM1,KXDIST,KXDST0,KTMSE ,KDSDP ,   &
     &       KEXCST,KEXCDT,KEXCGX,KEXCGY,KEXCGZ,KIMNDX,KIMOBS,KIMTIM,   &
     &       KIMSAT,KM2VPA,KDCDD ,KDWNWT,KDPOR ,KC2PAR,KBOUNC,KBPART,   &
     &       NXCA04
      COMMON/CORI04/KNMP  ,KINDP ,KNDPAR,KNPVCT,KISATN,KISET ,KIANTO,   &
     &              KISATO,KIANTD,KISATD,KISTAD,KISATC,KMJDCG,KISTAT,   &
     &              KMJDEB,KMJDBN,KNDBIN,KNWTDB,KIXPAR,KNDBUF,KSSTNM,   &
     &              KSSTNA,KMBSAT,KMBSTA,KXKEY ,KXVKEY,KXFLAG,KIPNTF,   &
     &              KIPNTS,KNCON ,KKF   ,KIDATB,KNSTLV,KSLVID,KLLBIA,   &
     &              KTMRA1,KTMRA2,KIND1 ,KTARID,KATARD,KYAWID,KXOBLK,   &
     &              KDSCWV,KATRSQ,KATSAT,KKVLAS,KPARTP,KLTMSC,KLTASC,   &
     &              KCTBST,KSTBST,KANCUT,KANGPS,KANTYP,                 &
     &              KANTOF,                                             &
     &              KYSAT, KMBDEG,                                      &
     &              KMBNOD,KMBSST,KBSPLN,KSSPLN,KXIOBS,KIDLAS,KIAVP ,   &
     &              KXNAVP,KNEXCG,KIDEXC,KNREXC,KTELEO,KIKEY ,KIMKEY,   &
     &              KIMBLK,KIMPRT,KCN110,KCN111,KC110,KC111,KTDSAT,     &
     &              KTDANT,NXCI04
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
! The values below apply to the Sea Surface Topography model
      PARAMETER (MXP=8608,MXF=1848,RAD=1.745329252D-2)
      DIMENSION OBS(NM)
      DIMENSION JP(200,100)
      DIMENSION PARMV(1)
      DIMENSION IPTRUA(1),IPTRAU(1)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION HEIGHT(NM)
      DIMENSION P(NPF,1)
      DIMENSION DLAT(NM),DLON(NM)
      DIMENSION PF(NPF,1)
      DIMENSION LSDATA(1)
      DIMENSION EL(1),CL(1)
      DIMENSION COSAR(1,NUMIND),SINAR(1,NUMIND)
      DIMENSION FSECN(1),TEMP(1)
      DIMENSION AA(1),II(1)
      DOUBLE PRECISION LATMIN,LATMAX,LONMIN,LONMAX
      DATA LINIT/.TRUE./
      DATA LATMIN,LATMAX,LONMIN,LONMAX/1.D3,-1.D3,1.D3,-1.D3/
      DATA DX,DY/1.D4,1.D4/
      DATA D2000/51544.5D0/,CENTUR/36525.0D0/
      DATA IZERO/0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      write(6,*)' EVALUATE PROUDMAN FUNCTIONS FOR THIS BLOCK '
! Initialize LADJ
      LADJ=(NPVAL0(IXOTPC).GT.0.OR.NPVAL0(IXOTPS).GT.0)
! Initialize TEMP
!
      DO 10 N=1,NM
      TEMP(N)=0.D0
   10 END DO
!
! COMPUTE TIMES
! The following calculations will produce T and DT
! T is the time at the beginning of the day expressed in Julian
! centuries. DT is the fraction of the day also expressed in Julian
! centuries. DT will be stored in TEMP
!
      CALL MJDYMD(MJDSBL,IYMDAY,IHMSEC,4)
      CALL MJDYMD(MJDS,IYMDAY, IZERO, 2)
      T=(DBLE(MJDS)-D2000)/CENTUR
      ITMP1=(IHMSEC/10000)+0.5
      ITMP2=MOD(IHMSEC,10000)
      ITMP3=(ITMP2/100)+0.5
      ITMP4=MOD(ITMP2,100)
      SECS=DBLE(ITMP4+ITMP3*60+ITMP1*3600)
!
      DO 11 N=1,NM
      TEMP(N)=(SECS+FSECN(N))/86400.D0
      TEMP(N)=TEMP(N)/CENTUR
!
   11 END DO
!
!   COMPUTE DOODSON VARIABLES, THEIR RATES AND THE ARGUMENTS NEEDED FOR
!   NODAL ANGLE COMPUTATION FOR THE DESIRED EPOCH
      CALL DOODVA(T,BETA,BETAP,U,RI,RNIU,RP)
!
! INITIALIZE POINTER FOR PARTIALS
      IBEGC=IPVAL0(IXOTPC)
      IBEGS=IPVAL0(IXOTPS)
      INDX1=IPVAL(IXOTPC)
      INDX2=IPVAL(IXOTPS)
!
! INITIALIZE PF
      DO  100 I=1,NPF
      DO  100 J=1,NM
      PF(I,J)=0.D0
  100 CONTINUE
!
! INITIALIZE EDITING ARRAY
      DO 150 I=1,NM
      LSDATA(I)=.TRUE.
  150 END DO
!
!   FOR THE FIRST CALL ONLY
!
      IF(LINIT) THEN
      LINIT=.FALSE.
!
      IF(NPF.GT.MXF) THEN
      WRITE(6,4) NPF,MXF
    4 FORMAT(' Subroutine PROUD: number of requested functions',I5,     &
     &       ' is greater than parameter MXF',I5)
      STOP 364
      ENDIF
!
      IF(NMESH.GT.MXP) THEN
      WRITE(6,6) NMESH,MXP
    6 FORMAT(' Subroutine PROUD: number of expected mesh points',       &
     &       I5,' is greater than parameter MXP',I5)
      STOP 365
      ENDIF
!
!  READ LOCATIONS AND PROUDMAN FUNCTIONS.
!
!  LOAD POINTER ARRAY JP
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
!
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
      DO 38 III=1,NMESH
!     RLAT=90.D0-CL(III)
      RLAT=CL(III)
      RLON=EL(III)
      JI=NINT((RLON-LONMIN)/DX)+1
      JJ=NINT((RLAT-LATMIN)/DY)+1
      JP(JI,JJ)=III
   38 END DO
!
!     DO 50900 IF=1,200
!     WRITE(6,*)(JP(IF,IG),IG=1,100)
!50900 CONTINUE
!
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
!   FOR THE FIRST CALL ONLY  *** END  ***
!
!  FOR DESIRED AREA, GET NEAREST VALID PROUDMAN-FCT LOCATIONS.
!
      DO 20000 N=1,NM
      SLAT=DLAT(N)
      SLON=DLON(N)
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
!  COMPUTE WEIGHTS FOR AVERAGING.
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
      IF(SUM2.LT.EDBOUN) THEN
      LSDATA(N)=.FALSE.
      GOTO 10000
      ENDIF
!
!  DO WEIGHTED AVERAGING.
!
      IF(LSDATA(N)) THEN
      DO 90 IF=1,NPF
!     SUM1=W1*P(JP(IX,IY  ),IF)+W2*P(JP(IX2,IY  ),IF)+
!    $     W3*P(JP(IX,IY+1),IF)+W4*P(JP(IX2,IY+1),IF)
      SUM1=W1*P(IF,JP(IX,IY  ))+W2*P(IF,JP(IX2,IY  ))+                  &
     &     W3*P(IF,JP(IX,IY+1))+W4*P(IF,JP(IX2,IY+1))
!     IF(W1.EQ.0.d0.or.w2.eq.0.d0.or.w3.eq.0.d0.or.w4.eq.0.d0) then
!     IF(SUM1.EQ.0.D0) THEN
!     write(6,*)p(if,jp(ix,iy)),if,jp(ix,iy),ix,iy
!     write(6,*)p(if,jp(ix2,iy)),if,jp(ix2,iy),ix2,iy
!     write(6,*)p(if,jp(ix,iy+1)),if,jp(ix,iy+1),ix,iy+1
!     write(6,*)p(if,jp(ix2,iy+1)),if,jp(ix2,iy+1),ix2,iy+1
!     endif
      IF(SUM1.EQ.0.D0) THEN
!     LSDATA(N)=.FALSE.
      ENDIF
      PF(IF,N)=SUM1/SUM2
   90 END DO
      ENDIF
!     LSDATA(N)=.TRUE.
10000 CONTINUE
!
20000 END DO
!
! EVALUATE HEIGHTS
      DO 914 I=1,NM
      HEIGHT(I)=0.D0
      IADV=0
      NUM=0
      NUM2=0
      DO 913 K=1,NUMIND
      KFL=II(KKF-1+K)
!
      AA(KFCOR-1+K)=FACTOR(KFL,RI,RNIU,RP)
!
!  COMPUTE THE PHASE AND SPEED OF EACH CONSTITUENT FOR THE DESIRED
!  EPOCH, TAKING INTO ACCOUNT THE NODAL ANGLES (FOR LUNAR TIDES).
!
      CALL PHSPED(PHASE,SPEED,AA(KDNUM),AA(KCNUM),BETA,BETAP,U,K)
!
!  COMPUTE THE ARGUMENT OF EACH CONSTITUENT FOR THE DESIRED EPOCH,
!  AND THE CORRESPONDING COSINE AND SINE MODULATING TERMS.
!
      TEMP(I)=SPEED*TEMP(I)+PHASE
!   COMPUTE THE NODAL MODULATION FACTOR FOR EACH CONSTITUENT
! dt should be an nm array
      COSAR(I,K)=AA(KFCOR-1+K)*COS(TEMP(I))
      SINAR(I,K)=AA(KFCOR-1+K)*SIN(TEMP(I))
!
      DO 912 J=1,NUMCON(K)
!
!  SHIFT THE PHASES OF THE DIURNAL CONSTITUENTS TO COMPLY WITH THE
!  CONVENTION USED BY SCHWIDERSKI.
!
      IF(K.EQ.4) THEN
      COSAR(I,K)=-SINAR(I,K)
      SINAR(I,K)= COSAR(I,K)
      ENDIF
      IF(K.EQ.5.OR.K.EQ.6.OR.K.EQ.7) THEN
      COSAR(I,K)= SINAR(I,K)
      SINAR(I,K)=-COSAR(I,K)
      ENDIF
!
!
      HEIGHT(I)=HEIGHT(I)+                                              &
     &PARMV(INDX1+J-1)*PF(J,I)*COSAR(I,K)+                              &
     &PARMV(INDX2+J-1)*PF(J,I)*SINAR(I,K)
!
! Compute Partials Here
!
       IF(LADJ) THEN
      NUM=NUM2
         IF(NESCON(K).GT.0) THEN
         DO 220 JK=1,NESCON(K)
         NUM=NUM+1
         IND2=IPTRAU(IPVAL0(IXOTPC)+NUM-1)
         IND4=IPTRAU(IPVAL0(IXOTPS)+NUM-1)
         IND3=IND2-IPVAL(IXOTPC)+1-IADV
         IND5=IND4-IPVAL(IXOTPS)+1-IADV
      IF(J.EQ.IND3) THEN
        IF(LNPNM) THEN
         PMPA(IBEGC-1+NUM,I)=PMPA(IBEGC-1+NUM,I)+PF(IND3,I)*COSAR(I,K)
         PMPA(IBEGS-1+NUM,I)=PMPA(IBEGS-1+NUM,I)+PF(IND5,I)*SINAR(I,K)
         ELSE
         PMPA(I,IBEGC-1+NUM)=PMPA(I,IBEGC-1+NUM)+PF(IND3,I)*COSAR(I,K)
         PMPA(I,IBEGS-1+NUM)=PMPA(I,IBEGS-1+NUM)+PF(IND5,I)*SINAR(I,K)
        ENDIF
       ENDIF
  220    CONTINUE
         ENDIF
       ENDIF
  912 END DO
      NUM2=NUM2+NESCON(K)
      IADV=IADV+NUMCON(K)
  913 END DO
  914 END DO
      RETURN
      END
