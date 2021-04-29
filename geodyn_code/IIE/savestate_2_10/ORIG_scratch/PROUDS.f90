!$PROUDS
      SUBROUTINE PROUDS(AA,PF,DLAT,DLON,LSDATA,LAVOID,                  &
     & NM,NPF,NMESH,XLTMNS,XLTMXS,XLNMNS,XLNMXS,SSTS,                   &
     & PARMV,PMPA,NDIM1,NDIM2,IPTRUA,IPTRAU,OBS,IUNPDS,                 &
     & MJDSBL,FSECN)
!********1*********2*********3*********4*********5*********6*********7**
! PROUD            00/00/00            0000.0    PGMR -  R. RAY
!                                                MODIF.  D. PAVLIS
!
! FUNCTION:  EVALUATION OF PROUDMAN FUNCTIONS AT A GIVEN LOCATION.
!
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
!   LSDATA   O    A    LOGICAL ARRAY FOR DATA EDITING
!
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
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
      COMMON/CLSSOT/LSSTMO,LOTMOD,LPRDES,NXLSST
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
      COMMON/MODEL /NPRM(10,2,5),ICROSS,MJD01,ISSTMD,NXMODL
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
      COMMON/RMODEL/PERMOD(3,12),XRMOD
!
!
!   NPF IS NSSTF FROM FIRST READ OF SST PROUDMAN FUNCTION FILE
!   CANNOT BE GREATER THAN PARAMETER MXF AS SET BELOW.
!   SHOULD NOT BE CHANGED AFTER FIRST CALL.
!   N.B.:  Parameter MXF is the largest NPF expected.
!
!   NMESH IS PIECE OF NSSTPT FOR THIS BLOCK
!
      PARAMETER (MXP=8608,MXF=3000,RAD=1.745329252D-2)
      DIMENSION AA(1)
      DIMENSION FSECN(NM)
      DIMENSION NA(7),NU(7),SUMA(7),SUMU(7),TWT(6)
      DIMENSION OBS(NM)
      DIMENSION JP(200,100)
      DIMENSION PARMV(1)
      DIMENSION IPTRUA(1),IPTRAU(1)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION PF(NPF,NM)
      DIMENSION SSTS(NM)
      DIMENSION DLAT(NM),DLON(NM)
      DIMENSION LSDATA(1),LAVOID(1)
      REAL PFPT(3002)
      REAL ,DIMENSION (:,:),ALLOCATABLE::P,PFO
      REAL ,DIMENSION (:),ALLOCATABLE::EL,CL,PDLAT,PDLON
      REAL ,DIMENSION (:),ALLOCATABLE::INDX
!
      DOUBLE PRECISION LATMIN,LATMAX,LONMIN,LONMAX
      INTEGER :: sumai1
      INTEGER :: sumai2
      INTEGER :: sumai3
      INTEGER :: sumai4
      DATA LINIT/.TRUE./
!     DATA LATMIN,LATMAX,LONMIN,LONMAX/1.D3,-1.D3,1.D3,-1.D3/
!     DATA DX,DY/1.D4,1.D4/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
! INITIALIZE POINTER FOR PARTIALS
      NPF2=NPF+2
      IBEGF=IPVAL0(IXSSTF)
!
      DO J=1,NPVAL0(IXSSTF)
      LAVOID(IBEGF-1+J)=.FALSE.
      ENDDO
!
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
      ENDIF
!
!CCC  IF(LPRDES)GOTO 30000
!
! FILL IN THE SST PROUDMAN FUNCTION DATA FOR THIS BLOCK FROM READING
! PROUDMAN FUNCTION FILE (UNIT 28)
!
       IF(.NOT.LPRDES)THEN
           IF(LINIT)THEN
              ALLOCATE (P(NPF,NMESH),EL(NMESH),CL(NMESH),&
                        PFO(NPF,NSSTPT),       &
                        PDLAT(NSSTPT),PDLON(NSSTPT),INDX(NMESH) )
           ELSE
              ALLOCATE (P(NPF,NMESH),EL(NMESH),CL(NMESH),INDX(NMESH) )
           ENDIF
       ELSE
           IF(LINIT)ALLOCATE (P(NPF,NMESH),EL(NMESH),CL(NMESH))
       ENDIF
!
      IF(LPRDES)GOTO 30000
!
!
       KK=0
      DO  110 J=1,NMESH
      INDX(J)=0
  110 END DO
       IF(LINIT)THEN
       READ(IUNPDS,ERR=9990)(PDLON(J),J=1,NSSTPT)
       READ(IUNPDS,ERR=9991)(PDLAT(J),J=1,NSSTPT)
       DO J=1,NSSTPT
       PDLAT(J)=90.0-PDLAT(J)
       ENDDO
       ENDIF
!
! IUNT27 IS FOR THE DIRECT ACCESS FILE OF PROUDMAN FUNCTION FOR SST
! IT COMES FROM A SEQUENTAIL PROUDMAN FUNCTION FILE (IUNT28)
!
        IUNT27=27
       IF(LINIT)THEN
!CC     DO 500 I=1,NPF
!CC        READ(IUNPDS,ERR=9993)(PFO(I,J),J=1,NSSTPT)
! 500  CONTINUE
        CLOSE(IUNPDS)
!
! WRITE OUT TO IUNT27 THE DIRECT ACCESS PROUDMAN FUNCTION BY POINT
!
!CC        DO 510 IPTS=1,NSSTPT
!CC        PFPT(1)=PDLON(IPTS)
!CC        PFPT(2)=PDLAT(IPTS)
!CC         DO IF=3,NPF+2
!CC         PFPT(IF)=PFO(IF-2,IPTS)
!CC      ENDDO
!CC         CALL PDFWRT(IUNT27,IPTS,PFPT)
! 510  CONTINUE
!CCLOSE(IUNT27)
      OPEN(UNIT=IUNT27,STATUS='OLD',ACCESS='DIRECT',RECL=12008)
       ENDIF
!  FINDING THE MAPPING INDXs FOR THE BLOCK AREA
       DO 400 J=1,NSSTPT
         IF(PDLAT(J).GE.XLTMNS.AND.PDLAT(J).LE.XLTMXS)THEN
           IF(PDLON(J).GE.XLNMNS.AND.PDLON(J).LE.XLNMXS)THEN
              KK=KK+1
              INDX(KK)=J
           ENDIF
         ENDIF
  400  CONTINUE
        IF(KK.LT.4.OR.KK.GT.NMESH)THEN
        WRITE(6,*)' LAND AREA: XLTMNS,XLTMXS,XLNMNS,XLNMXS',            &
     &        XLTMNS,XLTMXS,XLNMNS,XLNMXS
        GO TO 9999
        ELSE
!       WRITE(6,*)'NMESH *  KK* INDX(1) ',NMESH,KK,INDX(1)
!            PRINT *,'*CL(1),CL(KK),EL(1),EL(KK)',
!    *        CL(1),CL(KK),EL(1),EL(KK)
        ENDIF
!
! READ THE DIRECT ACCESS FILE OF PROUDMAN FUNCTON FOR AREA OF THIS BLOCK
!
        DO 520 K=1,KK
        IREC = INDX(K)
        CALL PDREAD(IUNT27,IREC,PFPT)
        EL(K)=PFPT(1)
        CL(K)=PFPT(2)
        DO J=1,NPF
        P(J,K)=PFPT(J+2)
         ENDDO
  520  CONTINUE
      GOTO 10100
30000 CONTINUE
!
!   PROUDMAN FUNCTION FILE WAS INPUT FROM IIS
      IF(LINIT)THEN
      KK=NMESH
      KCL=KDLATS
      KEL=KDLONS
       K0=0
        DO 550 I=1,NPF
         DO 550 K=1,KK
          K0=K0+1
        P(I,K)=AA(KPS+K0-1)
  550  CONTINUE
         DO 560 K=1,KK
         CL(K)=90.0-AA(KCL+K-1)
         EL(K)=AA(KEL+K-1)
  560  CONTINUE
      ENDIF
10100 CONTINUE
      E1=EL(1)
      C1=CL(1)
      LATMIN=1.D3
      LATMAX=-1.D3
      LONMIN=1.D3
      LONMAX=-1.D3
      DX=1.D4
      DY=1.D4
      DO 18 I=1,KK
      RLAT=CL(I)
      LATMIN=MIN(LATMIN,RLAT)
      LATMAX=MAX(LATMAX,RLAT)
      RLON=EL(I)
      LONMIN=MIN(LONMIN,RLON )
      LONMAX=MAX(LONMAX,RLON )
      IF(EL(I).NE.E1) DX=MIN(DX,ABS(EL(I)-E1))
      IF(CL(I).NE.C1) DY=MIN(DY,ABS(CL(I)-C1))
   18 END DO
      WRITE(6,20) NPF,NMESH,LATMIN,LATMAX,LONMIN,LONMAX,DX,DY
   20 FORMAT(/1X,I4,' Sea Surf Topo functions loaded for area:'/        &
     &        8X,'Number of mesh points in each fct:',I8/               &
     &        8X,'Lat:',2F11.3/8X,'Lon:',2F11.3/                        &
     &        8X,'Grid Interval:',F8.3,'  x',F8.3)
      NX=NINT((LONMAX - LONMIN)/DX)+1
      NY=NINT((LATMAX - LATMIN)/DY)+1
      WRITE(6,23) NX,NY
   23 FORMAT(8X,'Grid Size  NX,NY =',2I6)
      IF(NX.GT.200.OR.NY.GT.100) STOP 313
!
      LWRAP=ABS(LONMAX-LONMIN-360.D0).LT.1.5D0*DX.AND.                  &
     &         LONMIN.NE.(LONMAX-360.D0)
      IF(LWRAP) WRITE(6,*)'Wrap-around in longitude assumed for grid'
      DO 31 J=1,NY
      DO 31 I=1,NX
      JP(I,J)=0
   31 CONTINUE
      IREJEC=0
      DO 38 III=1,KK
      RLAT=CL(III)
      RLON=EL(III)
      II=NINT((RLON-LONMIN)/DX)+1
      JJ=NINT((RLAT-LATMIN)/DY)+1
      JP(II,JJ)=III
   38 END DO
!
!  For desired area, get nearest valid Proudman-fct locations.
!
!     CVAL=LONMAX+DX
!
! NEW CODE INCLUDING PERIODIC TERMS THE REFERENCE TIME T IS ALWAYS THE
! FIRST OF JANUARY OF THE CURRENT YEAR. WE NEED TO CHECK IF THE BLOCK
! CROSSES THE END OF THE YEAR AND ADJUST MJD01
      TIME=DBLE(MJDSBL-MJD01)
      IF(ICROSS.EQ.1) THEN
! CHECK IF THE BLOCK IS CROSSING FROM DECEMBER 31 TO JANUARY 01
       CALL MJDYMD(MJDSBL,IYMD,IHMS,3)
       IY=IYMD/10000
       IMD=IYMD-IY*10000
         IF(IMD.EQ.1231) THEN
         IYMDL=IY*10000+1231
         IHMSL=236000
         CALL YMDTIS(IYMDL,IHMSL,MJDSL)
         T1=DBLE(MJDSBL)+FSECN(1)
         T2=DBLE(MJDSL)
         IF(T1.GE.T2) MJD01=MJDSL
         ENDIF
      ENDIF
!
      DO 20000 N=1,NM
! COMPUTE TIMES
      TIME=TIME+FSECN(N)
      J=0
      DO I=1,3
      IF(PERMOD(I,8).GT.0.D0) THEN
      WT=TIME/PERMOD(I,8)
      TWT(I+J)=COS(WT)
      TWT(I+J+1)=SIN(WT)
      ENDIF
      J=J+1
      ENDDO
      SLAT=DLAT(N)
      SLON=DLON(N)
! Add the following code because the convention in GEODYN for longitude
! is 0-360 instead of -180 - +180
!     IF(SLON.GE.CVAL)SLON=SLON-360.D0
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
      ENDIF
!     IF(SUM1.EQ.0.D0) THEN
!     LSDATA(N)=.FALSE.
!     ENDIF
      PF(IF,N)=SUM1/SUM2
   90 END DO
      ENDIF
!     LSDATA(N)=.TRUE.
10000 CONTINUE
! Compute partials
! ATTENTION THIS NEEDS MORE WORK
      IF(.NOT.LSDATA(N)) GOTO 20000
      NU(1)=NPRM(1,1,1)
      SUMU(1)=NU(1)
      NA(1)=NPRM(1,2,1)
      SUMA(1)=NA(1)
      DO I=2,7
      NU(I)=NPRM(3+I,1,1)
      SUMU(I)=SUMU(I-1)+NU(I)
      NA(I)=NPRM(3+I,2,1)
      SUMA(I)=SUMA(I-1)+NA(I)
      ENDDO
      IF(LNPNM) THEN
      sumai1 = suma(1)
      DO 200 JK=1,sumai1     !!!!SUMA(1)
      IND2=IPTRAU(IPVAL0(IXSSTF)+JK-1)
      IND3=IND2-IPVAL(IXSSTF)+1
      PMPA(IBEGF-1+JK,N)=-PF(IND3,N)
  200 END DO
       IF(NA(2).EQ.0.AND.NA(3).EQ.0) GOTO 210
      DO 208 I=1,6
      sumai1 = SUMA(I)+1
      sumai2 = SUMA(I+1)
      DO 201 JK=sumai1, sumai2  !!!!!!!SUMA(I)+1,SUMA(I+1)
      IND2=IPTRAU(IPVAL0(IXSSTF)+JK-1)
      IND3=IND2-IPVAL(IXSSTF)-SUMU(I)+1
      PMPA(IBEGF-1+JK,N)=-PF(IND3,N)*TWT(I)
  201 END DO
  208 END DO
  210 CONTINUE
      ELSE
      sumai1 = suma(1)
      DO 300 JK=1,sumai1  !!!!!!!SUMA(1)
      IND2=IPTRAU(IPVAL0(IXSSTF)+JK-1)
      IND3=IND2-IPVAL(IXSSTF)+1
      PMPA(N,IBEGF-1+JK)=-PF(IND3,N)
  300 END DO
       IF(NA(2).EQ.0.AND.NA(3).EQ.0) GOTO 310
      DO 308 I=1,6
      sumai1 = SUMA(I)+1
      sumai2 = suma(i+1)
      DO 301 JK= sumai1   ,sumai2
      IF(SUMA(I+1).EQ.SUMA(I)) GOTO 308
      IND2=IPTRAU(IPVAL0(IXSSTF)+JK-1)
      IND3=IND2-IPVAL(IXSSTF)-SUMU(I)+1
      PMPA(N,IBEGF-1+JK)=-PF(IND3,N)*TWT(I)
  301 END DO
  308 END DO
  310 CONTINUE
      ENDIF
20000 END DO
! EVALUATE HEIGHTS
      DO I=1,NM
      SSTS(I)=0.D0
      DO J=1,NPF
      SSTS(I)=SSTS(I)+PARMV(IPVAL(IXSSTF)+J-1)*PF(J,I)
      ENDDO
      ENDDO
! COMPLETE THE MODEL WITH PERIODIC TERMS
      DO 916 I=1,NM
! COMPUTE TIMES
      TIME=TIME+FSECN(N)
      J=0
      DO K=1,3
      IF(PERMOD(I,8).GT.0.D0) THEN
      WT=TIME/PERMOD(K,8)
      TWT(K+J)=COS(WT)
      TWT(K+J+1)=SIN(WT)
      ENDIF
      J=J+1
      ENDDO
!
       DO 918 K=1,6
       IF(NU(K+1).GT.0) THEN
      DO 915 J=1,NU(K+1)
      ISUM=INT(SUMU(K))
      SSTS(I)=SSTS(I)+PARMV(IPVAL(IXSSTF)+ISUM+J-1)*TWT(K)*PF(J,I)
  915 END DO
       ENDIF
  918  CONTINUE
  916 END DO
      GO TO 11000
 9999 DO I=1,NM
      LSDATA(I)=.FALSE.
      SSTS(I)=0.D0
      ENDDO
11000 CONTINUE
      IF(.NOT.LPRDES)THEN
      IF(LINIT)THEN
        LINIT=.FALSE.
      DEALLOCATE (P,EL,CL,PFO,INDX)
      ELSE
      DEALLOCATE (P,EL,CL,INDX)
      ENDIF
                    ELSE
        LINIT=.FALSE.
                    ENDIF
      RETURN
 9990 STOP 9990
 9991 STOP 9991
 9993 STOP 9993
      END
