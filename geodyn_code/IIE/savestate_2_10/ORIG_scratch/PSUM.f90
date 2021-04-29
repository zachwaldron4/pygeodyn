!$PSUM
      SUBROUTINE PSUM  (ICON,ICNL2,ICNH2,                               &
     &    DXSDPM,COSTHG,SINTHG,PMPXI ,XM ,                              &
     &    PXSPXP,PXSPLV,NMP   ,NADJP,NM,MTYPE,NINTPL,PMPXE ,PMPA  ,     &
     &    NDIM1 ,NDIM2 ,ISTA  ,LESTA ,LESTP ,ISTAMT,PXSPAB,LNPNM ,      &
     &    LNEG  ,PSCALE,MJDSBL,FSEC,LVELA,STAVT,LSTL2,LSTH2,            &
     &    PARCBD,PARTBD,PMPXI2,                                         &
     &    XDFTR,PXSPA1,PXSPA2,ICNV,XMM,KTARNO,PARTRG,COTTHG,SITTHG,     &
     &    UMV,PPER,ANGWT,WT,LAVOID,DWRKDO,SCALE,LOFF,PXSPXD,UTDT,INDPI, &
     &    DXDNUT)
!********1*********2*********3*********4*********5*********6*********7**
! PSUM             00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: CHAIN AND STORE THE PARTIALS OF THE MEASUREMENT WRT TO
!           THE STATION COORDINATES IN THE SYSTEM OF ADJUSTMENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ICON     I    A    POINTERS TO INTERNAL MASTER STATION NUMBER
!   DXSDPM   I    A    PARTIALS OF SLAVE COORD. X,Y,Z WRT THE MASTER
!                      STATION COORDINATES IN THE SYSTEM OF ADJUSTMENT
!   COSTHG   I    A    COSINE OF THE GREENWICH HOUR ANGLE
!   SINTHG   I    A    SINE OF THE GREENWICH HOUR ANGLE
!   PMPXI    I    A    PARTIALS OF THE MEASUREMENT WRT INERTIAL S/C
!                      POSITION
!   XM       I    A    STATION CARTESIAN CRUST FIXED COORDINATES
!   PXSPXP   I    A    PARTIALS OF THE INSTANTANEOUS POLE STATION
!                      COORDINATES WRT POLAR MOTION
!   PXSPLV   I    A    PARTIALS OF THE INSTANTANEOUS POLE STATION
!                      COORDINATES WRT LOVE NUMBERS
!   NMP      I    A    INDICES OF THE LAST MEASUREMENT IN THE BLOCK
!                      ASSOCIATED WITH EACH INTERPOLATION INTERVAL
!   NADJP    I    A    NUMBER OF ADJUSTED POLAR MOTION VALUES ASSOCIATED
!                      WITH EACH INTERPOLATION INTERVAL
!   NM       I    S    NUMBER OF MEASUREMENTS IN THE BLOCK
!   NINTPL   I    S    NUMBER OF INTERPOLATION INTERVALS FOR POLAR MOTIO
!   PMPXE    I    A    PARTIALS OF MEASUREMENT WRT ECF STATION POSITION
!   PMPA    I/O   A    PARTIALS OF MEASUREMENTS WRT PARAMETERS
!   NDIM1    I    S    FIRST DIMENSION OF MATRIX PMPA
!   NDIM2    I    S    SECOND DIMENSION OF MATRIX PMPA
!   ISTA     I    S    INTERNAL STATION NUMBER (FROM INDSTA)
!   LESTA    I    S    TRUE WHEN ISTA< THE # OF TOTAL ADJUSTED STATIONS
!   LESTP    I    S    TRUE  FOR ADJUSTING POLAR MOTION
!   ISTAMT   I    S    STATION NUMBER INDICATING MEASUREMENT CONFIGURAT.
!                      COMPUTED FROM ARRAY ISTAFN(5,3)
!   PXSPAB   I    A    PARTIALS OF STATIONS WRT OCEAN LOADING PARAMETERS
!   LNPNM    I    S    TRUE IF THE FIRST DIMENSION OF PMPA IS THE NUMBER
!                      OF MEASUREMENTS IN THE BLOCK
!   LNEG     I    S    TRUE TO COMPUTE NEGATIVE PARTIALS (MEASUR. LINKS)
!   PSCALE   I    S    SCALE FACTOR OF THE PARTIAL OF THE MEASUREMENT
!                      WRT ECF STATION COORD., APPL. FOR DOPPLER TYPE
!                      FOR VLBI DELAY = 1/SPEED OF LIGHT
!   MJDSBL   I    S    MJD IN SECONDS OF THE FINAL RECEIVED TIME OF THE
!                      FIRST OBSERVATION OF THE BLOCK
!   FSEC     I    A    ELAPSED SECONDS FROM MJDSBL OF THE OBSERVATION
!                      TIMES ASSOCIATED WITH THE STATION
!   LVELA    I    A    INDICATORS FOR ADJUSTING STATION VELOCITIES
!   STAVT    I    A    TIMES FOR STATION VELOCITIES
!   LSTL2    I    A    INDICATORS FOR ADJUSTING STATION ETIDE L2
!   LSTH2    I    A    INDICATORS FOR ADJUSTING STATION ETIDE H2
!   PMPXI2   I    A    UNIT RANGE VECTOR IN TRUE OF SATELLITE SYSTEM
!   XDFTR    I    A    TRUE OF REF DIFFERENCE BETWEEN CENTER OF CENTRAL
!                      BODY AND TRACKING STATION
!   XMM      I    A    BODY FIXED COORDINATES OF LANDER
!   KTARNO   I    S    INTERNAL NUMBER OF PLANETARY TARGET
!   PARTRG   O    A    UNIT VECTOR OF STATION IN A MARS FIXED SYSTEM
!   COTTHG   I    A    COSINES OF MARS THETAG
!   SITTHG   I    A    SINES OF MARS THETAG
!   UMV      I    A    TOD UNIT VECTOR OF STATION IN THE MARS TOD INERTI
!                      FRAME
!   PPER     I    A    ARRAY OF PERIODS
!   ANGWT    I    A    ARRAY OF FREQUENCIES
!   PXSPXD   I    A    PARTIALS OF THE INSTANTANEOUS POLE STATION
!                      COORDINATES WRT POLAR MOTION RATE
!
! COMMENTS:            XDFTR WILL BE FILLED WITH SENSIBLE VALUES
!                      ONLY WHEN PARTIALS FOR RA & DEC OF THE CENTRAL
!                      BODY ARE BEING CUT.
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      PARAMETER(NPTYPE=10)
      COMMON/ADJGEO/LADATT,LADEBS,LADABS,LADGBS,LADLOV,LADPOL
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/ARCPR /FSECRF,XARCPR
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/COREPH/EMJDS0(2),EFSEC0(2),EMODE(2),ECORR(2),              &
     &              PSTAE0(12),DEBC(12),SGMEBC(12),BODYID(2),XCOEPH
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
      COMMON/OFFASI/IOFFAP
      COMMON/OLDADJ/NPV0OL(3,3),JPV0OL(3,3),NYZ0OL(3,3),JYZ0OL(3,3),    &
     &              NEO0OL(3,3),JEO0OL(3,3)
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/PTAROT/TAROT(3,3),RADLAN
      COMMON/REFDRV/DREFDA(9),DREFDB(9),DR3TDW(9),RDATR(9),RDBTR(9),    &
     &              DDA(9,2,50),DDB(9,2,50),DDR(9,50)
      COMMON/SETDI /NSETDP,NFRSED,NFRSDI,NFRSLN,NXSETI
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/TARGET/NLXYZ(200000),NPLTAR,NTARG,NATARG,NXTARG
      COMMON/VLBI/NQUA,NADJQ,NADJQV,NQUAB,IYMDV,NVOPT,NXVLBI
      COMMON/XTRAOR/T0XTRO,T0XTRO2,CVENUS,CLIBMN,CMARS,CMERC,XXTRAO
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      COMMON/INPOL /NMN(500),NINTVN
      COMMON/NUTHLD /NUTLST
      DIMENSION ICON  (NSTAE ),DXSDPM(     3,     3,NSTAE ),            &
     &          COSTHG(NM    ),SINTHG(NM    ),                          &
     &          PMPXI (NM    ,     3),PMPXE (NM    ,     3),            &
     &          XM    (     3),PXSPXP(NM    ,     3,     2),            &
     &          PXSPLV(NM    ,     3,NSETDP),                           &
     &          NMP   (NINTPL),NADJP (NINTPL),                          &
     &          PMPA  (NDIM1 ,NDIM2 ),PXSPAB(NM,3,1),                   &
     &          PARCBD(NM ,36 ),PARTBD(NM,36),PMPXI2(NM,3),             &
     &          XDFTR (NM,3),PXSPA1(NM,3,1),PXSPA2(NM,3,1),             &
     &          PXSPXD(NM,3,2)
      DIMENSION PXTPUT(    2)
      DIMENSION FSEC(NM)
      DIMENSION LVELA(1),STAVT(1)
      DIMENSION LSTL2(1),LSTH2(1)
      DIMENSION ICNL2(NSTA),ICNH2(NSTA)
      DIMENSION ICNV(NSTA)
      DIMENSION PARTRG(NM,3)
      DIMENSION XMM(3)
      DIMENSION COTTHG(NM),SITTHG(NM)
      DIMENSION UMV(NM,3)
      DIMENSION PPER(1),ANGWT(1),WT(1)
      DIMENSION LAVOID(MAPARM)
      DIMENSION DWRKDO(NM,3)
      DIMENSION DA(9),DD1(9)
      DIMENSION DB(9),DD2(9)
      DIMENSION SCALE(NM),PSCALE(NM)
      DIMENSION UTDT(NM)
      DIMENSION IPTYPE(NPTYPE)
      DIMENSION INDPI(1)
      DIMENSION DXDNUT(3,2,NM)
      DATA IPTYPE/37,38,47,48,69,70,83,84,91,92/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!  ***  SET LOGICAL FLAG FOR VLBI DATA PROCESSING ***
      LVLBI=MTYPE.EQ.31.OR.MTYPE.EQ.32
!
      FACTX=1.D0
      IF(LNEG) FACTX=-1.D0
        DO I=1,NM
        SCALE(I)=PSCALE(I)*FACTX
      ENDDO

!---------------------------------------------------------------------------

      !write(6,*) 'psum: ioffap, LOFF ', ioffap, LOFF

      IF(IOFFAP.GT.0.AND.LOFF) THEN

        IPT=IOFFAP+NAMBB

        !write(6,*) 'psum: ioffap, NAMBB, IPT ',  &
        !                  ioffap, NAMBB, IPT
        !write(6,*) 'psum: SCALE(1) ', SCALE(1)

        LAVOID(IPT  )=.FALSE.
        LAVOID(IPT+1)=.FALSE.
        LAVOID(IPT+2)=.FALSE.

!      WRITE(6,34568) IPVAL0(IXOFFS),DWRKDO(1,1),DWRKDO(1,2),DWRKDO(1,3)
34568 FORMAT(' IPT & PARS1',I6,3D16.7)
!      WRITE(6,34569) NM,DWRKDO(NM,1),DWRKDO(NM,2),DWRKDO(NM,3)
34569 FORMAT(' NM  & PARSL',I6,3D16.7)

        IF(LNPNM) THEN
          DO 10 I=1,NM
          PMPA(IPT,  I)=PMPA(IPT,  I)+SCALE(I)*DWRKDO(I,1)
          PMPA(IPT+1,I)=PMPA(IPT+1,I)+SCALE(I)*DWRKDO(I,2)
          PMPA(IPT+2,I)=PMPA(IPT+2,I)+SCALE(I)*DWRKDO(I,3)

!          !WRITE(6,34570) I,PMPA(IPT,I),PMPA(IPT+1,I),PMPA(IPT+2,I)
!34570 FORMAT(' I,PMPA ',I5,3D16.7)

   10     CONTINUE

        ELSE

          DO 20 I=1,NM
          PMPA(I,IPT  )=PMPA(I,IPT  )+SCALE(I)*DWRKDO(I,1)
          PMPA(I,IPT+1)=PMPA(I,IPT+1)+SCALE(I)*DWRKDO(I,2)
          PMPA(I,IPT+2)=PMPA(I,IPT+2)+SCALE(I)*DWRKDO(I,3)

!          WRITE(6,34570) I,PMPA(I,IPT),PMPA(I,IPT+1),PMPA(I,IPT+2)

   20     CONTINUE


        ENDIF ! LNPNM
      ENDIF !  IOFFAP.GT.0.AND.LOFF

!---------------------------------------------------------------------------


      IF(.NOT.LSTINR) RETURN

      IF(ISTAMT.LE.0) RETURN
!     WRITE(6,*) ' ISTA,LESTA,LESTP ',ISTA,LESTA,LESTP
      IF(LVLBI)   GOTO 100
! COMPUTE NEGATIVE OF CARTESIAN E.C.FIXED STATION PARTIALS
!     WRITE(6,*) '  PSUM**  CALL TO ECFIXP '
!     WRITE(6,*) '  PSUM**  LVLBI  ',LVLBI
      CALL ECFIXP(PMPXI ,COSTHG,SINTHG,PMPXE ,NM    ,NM    ,NM    )
  100 CONTINUE
      IF(.NOT.LESTA) GO TO 2000
! LESTA IS TRUE WHEN ISTA.LE.NSTAE
! ROTATE STATION PARTIALS INTO SYSTEM OF ADJUSTMENT AND
!        SUM INTO MEASUREMENT PARTIAL ARRAY CHANGING SIGN TO POSITIVE
!
! DETERMINE MASTER STATION
  101 CONTINUE
      MSTA  =ICON  (ISTA  )
!     LVADJ=LVELA(MSTA)
! CALCULATE STARTING LOCATION IN NORMAL MATRIX FOR STATION POSITION
!  AND VELOCITY PARTIALS
      INDEX =IPVAL0(IXSTAP)+(MSTA  -1)*3
!     INDEXV=IPVAL0(IXSTAV)+(MSTA  -1)*3
      IF(LNPNM) GO TO 1000
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
! *****  STATION POSITION COORDINATES  ****
!        ---------------------------
      DO 600 J=1,3
      LAVOID(INDEX)=.FALSE.
      DO 400 I=1,3
      TEMP=DXSDPM(J,I,ISTA)
      DO 200 N=1,NM
      PMPA(N,INDEX)   =PMPA(N,INDEX)   -PMPXE(N,I)   *TEMP*SCALE(N)
  200 END DO
!     WRITE(6,*) '  PSUM**  PMPA ',PMPA(1,1)
  400 END DO
      INDEX=INDEX+1
  600 END DO
 1999 CONTINUE
!     IF(.NOT.LVADJ) GOTO 2000
      IF(ICNV(ISTA).GT.NSTAV) GO TO 2000
      IF(NPVAL0(IXSTAV).LE.0) GO TO 2000
      IF(KCONAD.EQ.1) GO TO 19991
      MSTA = ICNV(ISTA)
19991 INDEXV=IPVAL0(IXSTAV)+(MSTA  -1)*3
!  ****  STATION VELOCITY PARTIALS ********
!        -------------------------
      DT=MJDSBL-STAVT(MSTA)
      DO 800 I=1,3
      LAVOID(INDEXV)=.FALSE.
      DO 700 N=1,NM
      PMPA(N,INDEXV)  =PMPA(N,INDEXV)  -PMPXE(N,I)*DT * SCALE(N)
  700 END DO
      INDEXV=INDEXV+1
  800 END DO
      GO TO 2000
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
 1000 CONTINUE
      DO 1600 J=1,3
      LAVOID(INDEX)=.FALSE.
      DO 1400 I=1,3
      TEMP=DXSDPM(J,I,ISTA)
      DO 1200 N=1,NM
      PMPA(INDEX,N)   =PMPA(INDEX,N)   -PMPXE(N,I)   *TEMP*SCALE(N)
 1200 END DO
 1400 END DO
      INDEX=INDEX+1
 1600 END DO
!     IF(.NOT.LVADJ) GOTO 2000
      IF(ICNV(ISTA).GT.NSTAV) GO TO 2000
      IF(NPVAL0(IXSTAV).LE.0) GO TO 2000
      IF(KCONAD.EQ.1) GO TO 19992
      MSTA = ICNV(ISTA)
19992 INDEXV=IPVAL0(IXSTAV)+(MSTA  -1)*3
      DT=MJDSBL-STAVT(MSTA)
      DO 1800 I=1,3
      LAVOID(INDEXV)=.FALSE.
      DO 1700 N=1,NM
      PMPA(INDEXV,N)  =PMPA(INDEXV,N)  -PMPXE(N,I)*DT * SCALE(N)
 1700 END DO
      INDEXV=INDEXV+1
 1800 END DO
 2000 CONTINUE

! ADD HERE THE PARTIALS FOR COM COF (Center of Mass -Center of Gravity offset)
!
      IF(NPVAL0(IXCOFF).EQ.0) GOTO 2090
      INDEX =IPVAL0(IXCOFF)
      IF(LNPNM) GO TO 2050
        LAVOID(INDEX)=.FALSE.
       DO I=1,3
        LAVOID(INDEX)=.FALSE.
      DO N=1,NM
      PMPA(N,INDEX)   =PMPA(N,INDEX)   -PMPXE(N,I)   *SCALE(N)
      ENDDO
        INDEX=INDEX+1
        ENDDO
      GOTO 2090
2050  CONTINUE
       DO I=1,3
        LAVOID(INDEX)=.FALSE.
      DO N=1,NM
      PMPA(INDEX,N)   =PMPA(INDEX,N)   -PMPXE(N,I)   *SCALE(N)
      ENDDO
        INDEX=INDEX+1
        ENDDO
2090  CONTINUE

! CODE HERE FOR PLANETARY TARGET PARTIALS
      IF(NPLTAR.EQ.0) GOTO 2500
      IF(NPVAL0(IXTARG).EQ.0) GOTO 2500
      INDEXT=IPVAL0(IXTARG)+3*(KTARNO-1)
      IF(LNPNM) GO TO 2100
      CALL ECFIXP(UMV ,COTTHG,SITTHG,PARTRG ,NM    ,NM    ,NM    )
      DO 2150 I=1,3
      LAVOID(INDEXT)=.FALSE.
      DO 2140 N=1,NM
      IF(NLXYZ(KTARNO).GT.1) THEN
      PV=PARTRG(N,1)*TAROT(I,1)+PARTRG(N,2)*TAROT(I,2)+                 &
     &      PARTRG(N,3)*TAROT(I,3)
      PMPA(N,INDEXT)   =PMPA(N,INDEXT)+PV*SCALE(N)
      ELSE
      PMPA(N,INDEXT)   =PMPA(N,INDEXT) + PARTRG(N,I) * SCALE(N)
      ENDIF
 2140 END DO
      INDEXT=INDEXT+1
 2150 END DO
      GOTO 2500
 2100 CONTINUE
      CALL ECFIXP(UMV ,COTTHG,SITTHG,PARTRG ,NM    ,NM    ,NM    )
      DO 2170 I=1,3
      LAVOID(INDEXT)=.FALSE.
      DO 2160 N=1,NM
      IF(NLXYZ(KTARNO).GT.1) THEN
      PV=PARTRG(N,1)*TAROT(I,1)+PARTRG(N,2)*TAROT(I,2)+                 &
     &      PARTRG(N,3)*TAROT(I,3)
      PMPA(INDEXT,N)   =PMPA(INDEXT,N) +PV*SCALE(N)
      ELSE
      PMPA(INDEXT,N)   =PMPA(INDEXT,N) + PARTRG(N,I) * SCALE(N)
      ENDIF
 2160 END DO
      INDEXT=INDEXT+1
 2170 END DO
 2500 CONTINUE
!CCC
!CCC
!CCC
      IF(.NOT.LESTP) GO TO 6500
! COMPUTE PARTIALS OF INSTANTANEOUS STATION LOCATION W.R.T. DELTA U.T.
      PXTPUT(1)= XM(2)*THDOT
      PXTPUT(2)=-XM(1)*THDOT
! CHAIN AND SUM POLAR MOTION AND UT PARTIALS WITH SIGN CHANGE
      INDEX1=IPVAL0(IXPOLX)
! CALCULATE STARTING LOCATION IN NORMAL MATRIX
! ***  SUM IN POLAR MOTION  PARTIALS  ********
!      ------------------------------
!  FIND THE FIRST NON ZERO INDPI
      IF(LNPNM) GO TO 3000
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 2600 J=1,2
      DO 2400 I=1,3
      DO 2200 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+J
      LAVOID(INDEX)=.FALSE.
      PMPA(N  ,INDEX)    =PMPA(N  ,INDEX)                           &
     &         -SCALE(N)*(PMPXE(N  ,I)    *PXSPXP(N  ,I,J)    )
      ENDIF
 2200 END DO
 2400 END DO
 2600 END DO
      GO TO 4000
 3000 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 3600 J=1,2
      DO 3400 I=1,3
      DO 3200 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+J
      LAVOID(INDEX)=.FALSE.
      PMPA(INDEX,  N)    =PMPA(INDEX,  N)                               &
     &         -SCALE(N)*(PMPXE(N  ,I)    *PXSPXP(N  ,I,J)    )
      ENDIF
 3200 END DO
 3400 END DO
 3600 END DO
 4000 CONTINUE
      INDEX1=IPVAL0(IXPOLX)
! **** PARTIALS OF U.T. ARE SUMMED IN PMPA *****
!      -----------------------------------
      IF(LNPNM) GO TO 4600
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 4400 I=1,2
      DO 4200 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+3
      LAVOID(INDEX)=.FALSE.
      PMPA(N,INDEX)      =PMPA (N,INDEX)                                &
     &                   -PMPXE(N,      I)    *PXTPUT(I)*SCALE(N)
      ENDIF
 4200 END DO
 4400 END DO
      GO TO 5500
 4600 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 5000 I=1,2
      DO 4800 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+3
      LAVOID(INDEX)=.FALSE.
      PMPA(INDEX,N)      =PMPA (INDEX,N)                                &
     &                   -PMPXE(N,      I)    *PXTPUT(I)*SCALE(N)
      ENDIF
 4800 END DO
 5000 END DO
 5500 CONTINUE
 6500 CONTINUE
!CCC
!CCC
!CCC
!CCC
!CCC
!CCC
!     IF(.NOT.LESTP.AND.NMODEL.NE.1) GO TO 16500
      IF(LESTP.AND.NMODEL.EQ.1) THEN
! COMPUTE PARTIALS OF INSTANTANEOUS STATION LOCATION W.R.T. DELTA U.T.
      PXTPUT(1)= XM(2)*THDOT
      PXTPUT(2)=-XM(1)*THDOT
! CHAIN AND SUM POLAR MOTION AND UT PARTIALS WITH SIGN CHANGE
      INDEX1=IPVAL0(IXPXDT)
! CALCULATE STARTING LOCATION IN NORMAL MATRIX
! ***  SUM IN POLAR MOTION  PARTIALS  ********
!      ------------------------------
      IF(LNPNM) GO TO 30000
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 22600 J=1,2
      DO 12400 I=1,3
      DO 12200 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+J
      LAVOID(INDEX)=.FALSE.
      PMPA(N  ,INDEX)    =PMPA(N  ,INDEX)                               &
     &         -SCALE(N)*(PMPXE(N  ,I)    *PXSPXD(N  ,I,J)    )
      ENDIF
12200 END DO
12400 END DO
22600 END DO
      GO TO 40000
30000 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 13600 J=1,2
      DO 13400 I=1,3
      DO 13200 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+J
      LAVOID(INDEX)=.FALSE.
      PMPA(INDEX,  N)    =PMPA(INDEX,  N)                               &
     &         -SCALE(N)*(PMPXE(N  ,I)    *PXSPXD(N  ,I,J)    )
      ENDIF
13200 END DO
13400 END DO
13600 END DO
40000 CONTINUE
! **** PARTIALS OF U.T. ARE SUMMED IN PMPA *****
!      -----------------------------------
      IF(LNPNM) GO TO 14600
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 14400 I=1,2
      DO 14200 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+3
      LAVOID(INDEX)=.FALSE.
      PMPA(N,INDEX)      =PMPA (N,INDEX)                                &
     &                   -PMPXE(N,      I) *PXTPUT(I)*UTDT(N)*SCALE(N)
      ENDIF
14200 END DO
14400 END DO
      GO TO 15500
14600 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 25000 I=1,2
      DO 14800 N=1,NM
      M=NMN(N)
      K=INDPI(M)
      IF(K.GT.0) THEN
      INDEX =INDEX1+(M-2)*3-1+3
      LAVOID(INDEX)=.FALSE.
      PMPA(INDEX,N)      =PMPA (INDEX,N)                                &
     &                   -PMPXE(N,      I) *PXTPUT(I)*UTDT(N)*SCALE(N)
      ENDIF
14800 END DO
25000 END DO
15500 CONTINUE
      ENDIF
!16500 CONTINUE
!CCC
! **** NUTATION PARAMETERS DPSI EPST PARTIALS                  *****
!      -------------------------------------------------------
      IF(.NOT.LNUTAD) GO TO  26800

      INDEX1=IPVAL0(IXDPSI)
! ***  SUM NUTATION  PARTIALS  ********
      IF(LNPNM) GO TO 26400
      DO 26110 I=1,3
      DO 26100 N=1,NM
      INDEX=INDEX1+(NUTLST-2)
      LAVOID(INDEX)=.FALSE.
      PMPA(N  ,INDEX)    =PMPA(N  ,INDEX)                           &
     &         -SCALE(N)*(PMPXE(N  ,I)    *DXDNUT(I  ,1,N)    )
!     write(6,*)' dbg PMPA 1 ', PMPA(N  ,INDEX)
26100 CONTINUE
26110 CONTINUE
      GOTO 26500
!
26400 CONTINUE
      DO 26140 I=1,3
      DO 26130 N=1,NM
      INDEX=INDEX1+(NUTLST-2)
      LAVOID(INDEX)=.FALSE.
      PMPA(INDEX,  N)    =PMPA(INDEX,  N)                               &
     &         -SCALE(N)*(PMPXE(N  ,I)    *DXDNUT(I  ,1,N)    )
26130 CONTINUE
26140 CONTINUE

26500 CONTINUE

      INDEX1=IPVAL0(IXEPST)
      IF(LNPNM) GO TO 26700
      DO 26170 I=1,3
      DO 26160 N=1,NM
      INDEX=INDEX1+(NUTLST-2)
      LAVOID(INDEX)=.FALSE.
      PMPA(N  ,INDEX)    =PMPA(N  ,INDEX)                           &
     &         -SCALE(N)*(PMPXE(N  ,I)    *DXDNUT(I  ,2,N)    )
!     write(6,*)' dbg PMPA 1 ', PMPA(N  ,INDEX)
26160 CONTINUE
26170 CONTINUE
      GOTO 26800
!
26700 CONTINUE
      DO 26191 I=1,3
      DO 26190 N=1,NM
      INDEX=INDEX1+(NUTLST-2)
      LAVOID(INDEX)=.FALSE.
      PMPA(INDEX,  N)    =PMPA(INDEX,  N)                               &
     &         -SCALE(N)*(PMPXE(N  ,I)    *DXDNUT(I  ,2,N)    )
26190 CONTINUE
26191 CONTINUE

26800 CONTINUE

!CCC
!CCC
!      -------------------------------------------------------
! **** STATION TIDAL DISPLACEMENT COEFFICIENT H2 & L2 PARTIALS *****
!      -------------------------------------------------------
!     PRINT 12346,IXH2LV,IXL2LV,NPVAL0
!2346 FORMAT(' ** PSUM **  IXH2LV,IXL2LV=',2I12/' NPVAL0='/(1X,5I12))
!  ** IXH2LV : POINTER FOR H2 !!!
      IF(.NOT.LADLOV)GO TO 8010
      IXLOVE=IXH2LV
      INDEX=IPVAL0(IXLOVE)
      DO 8000 J=1,NSETDP
      LAVOID(INDEX)=.FALSE.
      IF(LNPNM) GO TO 7500
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 7400 I=1,3
      DO 7200 N=1,NM
      PMPA(N,INDEX)   =PMPA (N,INDEX)                                   &
     &               -(PMPXE(N,    I)   *PXSPLV(N,I,J)   )*SCALE(N)
 7200 END DO
 7400 END DO
      GO TO 8000
 7500 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 7800 I=1,3
      DO 7600 N=1,NM
      PMPA(INDEX,N)   =PMPA (INDEX,N)                                   &
     &               -(PMPXE(N,    I)   *PXSPLV(N,I,J)   )*SCALE(N)
 7600 END DO
 7800 END DO
 8000 INDEX=INDEX+1
 8010 CONTINUE
! **** STATION ETIDE L2 PARTIALS *****
!      -------------------------------------------------------
      IF(ICNL2(ISTA).GT.NSTEL2)GO TO 8180
      IF(NPVAL0(IXSTL2).LE.0) GO TO 8180
!....
      IL2 = ICNL2(ISTA)
      INDEX=IPVAL0(IXSTL2)+IL2-1
      LAVOID(INDEX)=.FALSE.
      IF(LNPNM) GO TO 8150
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 7410 I=1,3
      DO 7210 N=1,NM
      PMPA(N,INDEX)   =PMPA (N,INDEX)                                   &
     &               -(PMPXE(N,    I)   *PXSPLV(N,I,2)   )*SCALE(N)
 7210 END DO
 7410 END DO
      GO TO 8180
 8150 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 7810 I=1,3
      DO 7610 N=1,NM
      PMPA(INDEX,N)   =PMPA (INDEX,N)                                   &
     &               -(PMPXE(N,    I)   *PXSPLV(N,I,2)   )*SCALE(N)
 7610 END DO
 7810 END DO
! **** STATION ETIDE H2 PARTIALS *****
!      -------------------------------------------------------
 8180 CONTINUE
!C    WRITE(6,*)'PSUM * ETIDE-H2 *ISTA,LSTH2*',ISTA,LSTH2(ISTA)
      IF(ICNH2(ISTA).GT.NSTEH2)GO TO 8210
      IF(NPVAL0(IXSTH2).LE.0) GO TO 8210
!....
      IH2 = ICNH2(ISTA)
      INDEX=IPVAL0(IXSTH2)+IH2-1
      LAVOID(INDEX)=.FALSE.
      IF(LNPNM) GO TO 8170
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 7430 I=1,3
      DO 7230 N=1,NM
      PMPA(N,INDEX)   =PMPA (N,INDEX)                                   &
     &               -(PMPXE(N,    I)   *PXSPLV(N,I,1)   )*SCALE(N)
 7230 END DO
 7430 END DO
      GO TO 8210
 8170 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 7830 I=1,3
      DO 7630 N=1,NM
      PMPA(INDEX,N)   =PMPA (INDEX,N)                                   &
     &               -(PMPXE(N,    I)   *PXSPLV(N,I,1)   )*SCALE(N)
 7630 END DO
 7830 END DO
! ****  OCEAN  LOADING PARAMETER PARTIALS  *****
!       ---------------------------------
 8210 CONTINUE
      IF(.NOT.LOLAJ) GO TO 10000
      JNDEX=0
      NDADJ=0
      IF(LNPNM) GO TO 9000
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
! K --> DIRECTION E N V
      DO 8800 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NPV0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 8800
      INDEX=JPV0OL(K,ISTAMT)-1
      DO 8600 J=1,NDADJ
      INDEX=INDEX+1
      LAVOID(INDEX)=.FALSE.
      LAVOID(INDEX+NDADJ)=.FALSE.
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 8400 I=1,3
      DO 8200 N=1,NM
      PMPA(N,INDEX      )   =PMPA  (N,  INDEX      )                    &
     &       -(PMPXE(N,I)   *PXSPAB(N,I,JNDEX      )   )*SCALE(N)
      PMPA(N,INDEX+NDADJ)   =PMPA  (N,  INDEX+NDADJ)                    &
     &       -(PMPXE(N,I)   *PXSPAB(N,I,JNDEX+NDADJ)   )*SCALE(N)
 8200 END DO
 8400 END DO
 8600 END DO
 8800 END DO
      JNDEX=0
      NDADJ=0
! K --> DIRECTION X Y Z CENTER OF MASS
      DO 8900 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NYZ0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 8900
      INDEX=JYZ0OL(K,ISTAMT)-1
      DO 8860 J=1,NDADJ
      INDEX=INDEX+1
      LAVOID(INDEX)=.FALSE.
      LAVOID(INDEX+NDADJ)=.FALSE.
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 8840 I=1,3
      DO 8820 N=1,NM
      PMPA(N,INDEX      )   =PMPA  (N,  INDEX      )                    &
     &       -(PMPXE(N,I)   *PXSPA1(N,I,JNDEX      )   )*SCALE(N)
      PMPA(N,INDEX+NDADJ)   =PMPA  (N,  INDEX+NDADJ)                    &
     &       -(PMPXE(N,I)   *PXSPA1(N,I,JNDEX+NDADJ)   )*SCALE(N)
 8820 END DO
 8840 END DO
 8860 END DO
 8900 END DO
      JNDEX=0
      NDADJ=0
! K --> DIRECTION EOP PARAMETERS
      DO 8990 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NEO0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 8990
      INDEX=JEO0OL(K,ISTAMT)-1
      DO 8960 J=1,NDADJ
      INDEX=INDEX+1
      LAVOID(INDEX)=.FALSE.
      LAVOID(INDEX+NDADJ)=.FALSE.
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 8940 I=1,3
      DO 8920 N=1,NM
      PMPA(N,INDEX      )   =PMPA  (N,  INDEX      )                    &
     &       -(PMPXE(N,I)   *PXSPA2(N,I,JNDEX      )   )*SCALE(N)
      PMPA(N,INDEX+NDADJ)   =PMPA  (N,  INDEX+NDADJ)                    &
     &       -(PMPXE(N,I)   *PXSPA2(N,I,JNDEX+NDADJ)   )*SCALE(N)
 8920 END DO
 8940 END DO
 8960 END DO
 8990 END DO
      GO TO 10000
 9000 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
! K --> DIRECTION E N V
      DO 9800 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NPV0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 9800
      INDEX=JPV0OL(K,ISTAMT)-1
      DO 9600 J=1,NDADJ
      INDEX=INDEX+1
      LAVOID(INDEX)=.FALSE.
      LAVOID(INDEX+NDADJ)=.FALSE.
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 9400 I=1,3
      DO 9200 N=1,NM
      PMPA(INDEX      ,N)   =PMPA  (    INDEX      ,N)                  &
     &       -(PMPXE(N,I)   *PXSPAB(N,I,JNDEX      )   )*SCALE(N)
      PMPA(INDEX+NDADJ,N)   =PMPA  (    INDEX+NDADJ,N)                  &
     &       -(PMPXE(N,I)   *PXSPAB(N,I,JNDEX+NDADJ)   )*SCALE(N)
 9200 END DO
 9400 END DO
 9600 END DO
 9800 END DO
      JNDEX=0
      NDADJ=0
! K --> DIRECTION X Y Z CENTER OF MASS
      DO 9900 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NYZ0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 9900
      INDEX=JYZ0OL(K,ISTAMT)-1
      DO 9860 J=1,NDADJ
      INDEX=INDEX+1
      LAVOID(INDEX)=.FALSE.
      LAVOID(INDEX+NDADJ)=.FALSE.
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 9840 I=1,3
      DO 9820 N=1,NM
      PMPA(INDEX      ,N)   =PMPA  (    INDEX      ,N)                  &
     &       -(PMPXE(N,I)   *PXSPA1(N,I,JNDEX      )   )*SCALE(N)
      PMPA(INDEX+NDADJ,N)   =PMPA  (    INDEX+NDADJ,N)                  &
     &       -(PMPXE(N,I)   *PXSPA1(N,I,JNDEX+NDADJ)   )*SCALE(N)
 9820 END DO
 9840 END DO
 9860 END DO
 9900 END DO
      JNDEX=0
      NDADJ=0
! K --> DIRECTION EOP PARAMETERS
      DO 9990 K=1,3
      JNDEX=JNDEX+NDADJ
      NDADJ=NEO0OL(K,ISTAMT)
      IF(NDADJ.LE.0) GO TO 9990
      INDEX=JEO0OL(K,ISTAMT)-1
      DO 9960 J=1,NDADJ
      INDEX=INDEX+1
      LAVOID(INDEX)=.FALSE.
      LAVOID(INDEX+NDADJ)=.FALSE.
      JNDEX=JNDEX+1
! I--> XYZ OF STATION
      DO 9940 I=1,3
      DO 9920 N=1,NM
      PMPA(INDEX      ,N)   =PMPA  (    INDEX      ,N)                  &
     &       -(PMPXE(N,I)   *PXSPA2(N,I,JNDEX      )   )*SCALE(N)
      PMPA(INDEX+NDADJ,N)   =PMPA  (    INDEX+NDADJ,N)                  &
     &       -(PMPXE(N,I)   *PXSPA2(N,I,JNDEX+NDADJ)   )*SCALE(N)
 9920 END DO
 9940 END DO
 9960 END DO
 9990 END DO
10000 CONTINUE
! **** PLANETARY EPHEMERIS CORRECTION PARTIALS FOR CENTRAL BODY *****
!      -------------------------------------------------------
!     WRITE(6,*) ' PSUM : EPHEM CORR CALCULATIONS'
!     PRINT 12347,IXEPHC,IXEPHT,NPVAL0(IXEPHC)
12347 FORMAT(' ** PSUM **  IXEPHC,IXEPHT=',2I12/' NPVAL0='/(1X,5I12))
!    *IXEPHC : POINTER FOR CENTRAL BODY EPHEMERIS CORRECTION
      IF(NPVAL0(IXEPHC).LE.0) GO TO 10100
      INDEX=IPVAL0(IXEPHC)
      IF(LNPNM) GO TO 10200
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 10500 K=1,6
      IF (SGMEBC(K).LE.0.0D0) GO TO 10500
      LAVOID(INDEX)=.FALSE.
      DO 10400 I=1,3
      M=(I-1)*6+K
      DO 10300 N=1,NM
      PMPA(N,INDEX)   =PMPA (N,INDEX)                                   &
     &               + PMPXI2(N,    I)  * PARCBD(N,M)  * SCALE(N)
!     WRITE(6,*) ' PSUM : N M PARCBD  ',N,M,PARCBD(N,M)
10300 END DO
10400 END DO
      INDEX=INDEX+1
10500 END DO
      GO TO 10100
10200 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 10900 K=1,6
      IF (SGMEBC(K).LE.0.0D0) GO TO 10900
      LAVOID(INDEX)=.FALSE.
      DO 10800 I=1,3
      M=(I-1)*6+K
      DO 10600 N=1,NM
      PMPA(INDEX,N)   =PMPA (INDEX,N)                                   &
     &               + PMPXI2(N,    I)   *PARCBD(N,M)  * SCALE(N)
10600 END DO
10800 END DO
      INDEX=INDEX+1
10900 END DO
10100 CONTINUE
! **** PLANETARY EPHEMERIS CORRECTION PARTIALS FOR TRACKING  BODY *****
!      -------------------------------------------------------
!     WRITE(6,*) ' PSUM : EPHEM CORR CALCULATIONS'
!     WRITE(6,*) ' PSUM : EPHEM SCALE VALUE  ', SCALE
      IF(NPVAL0(IXEPHT).LE.0) GO TO 10102
      INDEX=IPVAL0(IXEPHT)
      IF(LNPNM) GO TO 10202
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NM,NADJST)
      DO 10502 K=1,6
      IF (SGMEBC(6+K).LE.0.0D0) GO TO 10502
      LAVOID(INDEX)=.FALSE.
      DO 10402 I=1,3
      M=(I-1)*6+K
      DO 10302 N=1,NM
      PMPA(N,INDEX)   =PMPA (N,INDEX)                                   &
     &               - PMPXI2(N,    I)  * PARTBD(N,M) * SCALE(N)
10302 END DO
!     WRITE(6,*) ' PSUM : PMPXI  ',I,PMPXI(1,I)
!     WRITE(6,*) ' PSUM :  M PARTBD  ',M,PARTBD(1,M)
10402 END DO
!     WRITE(6,*) ' PSUM : INDEX,PMPA ',INDEX,PMPA(1,INDEX)
      INDEX=INDEX+1
10502 END DO
      GO TO 10102
10202 CONTINUE
!.....PARTIAL DERIVATIVE ARRAY DIMENSIONED (NADJST,NM)
      DO 10902 K=1,6
!     WRITE(6,*) ' PSUM : SGMEBC  ',K,SGMEBC(6+K)
      IF (SGMEBC(6+K).LE.0.0D0) GO TO 10902
      LAVOID(INDEX)=.FALSE.
      DO 10802 I=1,3
      M=(I-1)*6+K
      DO 10602 N=1,NM
      PMPA(INDEX,N)   =PMPA (INDEX,N)                                   &
     &               - PMPXI2(N,    I)   *PARTBD(N,M) * SCALE(N)
10602 END DO
!     WRITE(6,*) ' PSUM : PMPXI  ',I,PMPXI(1,I)
!     WRITE(6,*) ' PSUM :  M PARTBD  ',M,PARTBD(1,M)
10802 END DO
!     WRITE(6,*) ' PSUM : INDEX,PMPA ',INDEX,PMPA(INDEX,1)
      INDEX=INDEX+1
10902 END DO
10102 CONTINUE
!
! PLANETARY ORIENTATION PARTIALS
!
      IF(NPVAL0(IXXTRO).LE.0) RETURN
      LPT=.FALSE.
      DO I=1,NPTYPE
       IF(IPTYPE(I).EQ.MTYPE) LOT=.TRUE.
      ENDDO
      IF(.NOT.LPT) RETURN
      DO 11000 J=1,NXALLC
      ANGWT(J)=TWOPI/PPER(J)
11000 END DO
      INDX1=IPVAL0(IXXTRO)
      INDX2=INDX1+1
      INDX3=INDX2+1
      INDX4=INDX3+1
      INDX5=INDX4+1
      INDX6=INDX5+1
      INDX7=INDX6+1
      LAVOID(INDX1)=.FALSE.
      LAVOID(INDX2)=.FALSE.
      LAVOID(INDX3)=.FALSE.
      LAVOID(INDX4)=.FALSE.
      LAVOID(INDX5)=.FALSE.
      LAVOID(INDX6)=.FALSE.
      LAVOID(INDX7)=.FALSE.
      DTRJ2K=DBLE(MJDSRF)+FSECRF-T0XTRO
      DTOJ2K=DBLE(MJDSBL)-T0XTRO
      DO 11100 J=1,NXALLC
      WT(J)=ANGWT(J)*DTOJ2K
      WT(J)=MOD(WT(J),TWOPI)
11100 END DO
!
      IF(LNPNM) GO TO 12600
! NM IS 1ST DIMENSION
      DO 12500 N=1,NM
        DO I=1,9
        DA(I)=DDA(I,1,N)+DDA(I,2,N)
        DB(I)=DDB(I,1,N)+DDB(I,2,N)
        DD1(I)=DDA(I,1,N)*DTRJ2K+DDA(I,2,N)*(DTOJ2K+FSEC(N))
        DD2(I)=DDB(I,1,N)*DTRJ2K+DDB(I,2,N)*(DTOJ2K+FSEC(N))
        ENDDO
! IF THIS IS NOT PLANETARY TARGET OPTION THE PORTION BELOW WILL BE ZERO
!    &          +SCALE(N)*(
      PMPA(N,INDX1)=PMPA(N,INDX1)+SCALE(N)*(                            &
     &           PMPXI2(N,1)*(RDATR(1)*XDFTR(N,1)+RDATR(4)*XDFTR(N,2)   &
     &                       +RDATR(7)*XDFTR(N,3))                      &
     &          +PMPXI2(N,2)*(RDATR(2)*XDFTR(N,1)+RDATR(5)*XDFTR(N,2)   &
     &                       +RDATR(8)*XDFTR(N,3))                      &
     &          +PMPXI2(N,3)*(RDATR(3)*XDFTR(N,1)+RDATR(6)*XDFTR(N,2)   &
     &                       +RDATR(9)*XDFTR(N,3)))                     &
     &          +SCALE(N)*(                                             &
     &    PMPXI2(N,1)*(DA(1)*XMM(1)+DA(4)*XMM(2)+DA(7)*XMM(3))          &
     &   +PMPXI2(N,2)*(DA(2)*XMM(1)+DA(5)*XMM(2)+DA(8)*XMM(3))          &
     &   +PMPXI2(N,3)*(DA(3)*XMM(1)+DA(6)*XMM(2)+DA(9)*XMM(3))          &
     &          )
! WITH A LITTLE SCRATCH SPACE, THE ABOVE LINE COULD BE EXPLOITED
!
      PMPA(N,INDX2)=PMPA(N,INDX2)                                       &
     &          +(PMPXI2(N,1)*(RDATR(1)*XDFTR(N,1)+RDATR(4)*XDFTR(N,2)  &
     &                        +RDATR(7)*XDFTR(N,3))                     &
     &           +PMPXI2(N,2)*(RDATR(2)*XDFTR(N,1)+RDATR(5)*XDFTR(N,2)  &
     &                        +RDATR(8)*XDFTR(N,3))                     &
     &           +PMPXI2(N,3)*(RDATR(3)*XDFTR(N,1)+RDATR(6)*XDFTR(N,2)  &
     &                        +RDATR(9)*XDFTR(N,3)))*DTRJ2K*SCALE(N)    &
     & + (PMPXI2(N,1)*(DD1(1)*XMM(1)+DD1(4)*XMM(2)+DD1(7)*XMM(3))       &
     &   +PMPXI2(N,2)*(DD1(2)*XMM(1)+DD1(5)*XMM(2)+DD1(8)*XMM(3))       &
     &   +PMPXI2(N,3)*(DD1(3)*XMM(1)+DD1(6)*XMM(2)+DD1(9)*XMM(3))       &
     &  )*SCALE(N)
      PMPA(N,INDX3)=PMPA(N,INDX3)+SCALE(N)*(                            &
     &           PMPXI2(N,1)*(RDBTR(1)*XDFTR(N,1)+RDBTR(4)*XDFTR(N,2)   &
     &                       +RDBTR(7)*XDFTR(N,3))                      &
     &          +PMPXI2(N,2)*(RDBTR(2)*XDFTR(N,1)+RDBTR(5)*XDFTR(N,2)   &
     &                       +RDBTR(8)*XDFTR(N,3))                      &
     &          +PMPXI2(N,3)*(RDBTR(3)*XDFTR(N,1)+RDBTR(6)*XDFTR(N,2)   &
     &                       +RDBTR(9)*XDFTR(N,3)))                     &
     &          +SCALE(N)*(                                             &
     &    PMPXI2(N,1)*(DB(1)*XMM(1)+DB(4)*XMM(2)+DB(7)*XMM(3))          &
     &   +PMPXI2(N,2)*(DB(2)*XMM(1)+DB(5)*XMM(2)+DB(8)*XMM(3))          &
     &   +PMPXI2(N,3)*(DB(3)*XMM(1)+DB(6)*XMM(2)+DB(9)*XMM(3))          &
     &          )
! WITH A LITTLE SCRATCH SPACE, THE ABOVE LINE COULD BE EXPLOITED
!
      PMPA(N,INDX4)=PMPA(N,INDX4)                                       &
     &          +(PMPXI2(N,1)*(RDBTR(1)*XDFTR(N,1)+RDBTR(4)*XDFTR(N,2)  &
     &                        +RDBTR(7)*XDFTR(N,3))                     &
     &           +PMPXI2(N,2)*(RDBTR(2)*XDFTR(N,1)+RDBTR(5)*XDFTR(N,2)  &
     &                        +RDBTR(8)*XDFTR(N,3))                     &
     &           +PMPXI2(N,3)*(RDBTR(3)*XDFTR(N,1)+RDBTR(6)*XDFTR(N,2)  &
     &                        +RDBTR(9)*XDFTR(N,3)))*DTRJ2K*SCALE(N)    &
     &  +(PMPXI2(N,1)*(DD2(1)*XMM(1)+DD2(4)*XMM(2)+DD2(7)*XMM(3))       &
     &   +PMPXI2(N,2)*(DD2(2)*XMM(1)+DD2(5)*XMM(2)+DD2(8)*XMM(3))       &
     &   +PMPXI2(N,3)*(DD2(3)*XMM(1)+DD2(6)*XMM(2)+DD2(9)*XMM(3))       &
     &          )*SCALE(N)
! ROTATION PARTIALS
      PMPA(N,INDX5)=PMPA(N,INDX5)                                       &
     &             +SCALE(N)*(                                          &
     &    PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
! IF THIS IS NOT PLANETARY TARGET OPTION THE PARTIALS  BELOW WILL BE ZER
! ROTATION RATE PARTIALS
      PMPA(N,INDX6)=PMPA(N,INDX6)                                       &
     &             +DTOJ2K*SCALE(N)*                                    &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )

! IF THIS IS NOT PLANETARY TARGET OPTION THE PARTIALS  BELOW WILL BE ZER
! ROTATION RATE PARTIALS
      PMPA(N,INDX7)=PMPA(N,INDX7)                                       &
     &             +(DTOJ2K**2.D0)*SCALE(N)*                            &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
!
      DO 13000 I=1,NXALLC
      INDX8=INDX7+2*(I-1)+1
      LAVOID(INDX8)=.FALSE.
      PMPA(N,INDX8)=PMPA(N,INDX8)+(COS(WT(I))*SCALE(N))*               &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
!
      INDX9=INDX8+1
      LAVOID(INDX9)=.FALSE.
      PMPA(N,INDX9)=PMPA(N,INDX9)+(SIN(WT(I))*SCALE(N))*               &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
!
13000 END DO
12500 END DO
      GO TO 15000
12600 CONTINUE
!
! NM IS 2ND DIMENSION
      DO 12700 N=1,NM
        DO I=1,9
        DA(I)=DDA(I,1,N)+DDA(I,2,N)
        DB(I)=DDB(I,1,N)+DDB(I,2,N)
        DD1(I)=DDA(I,1,N)*DTRJ2K+DDA(I,2,N)*(DTOJ2K+FSEC(N))
        DD2(I)=DDB(I,1,N)*DTRJ2K+DDB(I,2,N)*(DTOJ2K+FSEC(N))
        ENDDO
      PMPA(INDX1,N)=PMPA(INDX1,N)+SCALE(N)*(                            &
     &           PMPXI2(N,1)*(RDATR(1)*XDFTR(N,1)+RDATR(4)*XDFTR(N,2)   &
     &                       +RDATR(7)*XDFTR(N,3))                      &
     &          +PMPXI2(N,2)*(RDATR(2)*XDFTR(N,1)+RDATR(5)*XDFTR(N,2)   &
     &                       +RDATR(8)*XDFTR(N,3))                      &
     &          +PMPXI2(N,3)*(RDATR(3)*XDFTR(N,1)+RDATR(6)*XDFTR(N,2)   &
     &                       +RDATR(9)*XDFTR(N,3)))                     &
     &          +SCALE(N)*(                                             &
     &    PMPXI2(N,1)*(DA(1)*XMM(1)+DA(4)*XMM(2)+DA(7)*XMM(3))          &
     &   +PMPXI2(N,2)*(DA(2)*XMM(1)+DA(5)*XMM(2)+DA(8)*XMM(3))          &
     &   +PMPXI2(N,3)*(DA(3)*XMM(1)+DA(6)*XMM(2)+DA(9)*XMM(3))          &
     &          )
! WITH A LITTLE SCRATCH SPACE, THE ABOVE LINE COULD BE EXPLOITED
!
      PMPA(INDX2,N)=PMPA(INDX2,N)                                       &
     &          +(PMPXI2(N,1)*(RDATR(1)*XDFTR(N,1)+RDATR(4)*XDFTR(N,2)  &
     &                        +RDATR(7)*XDFTR(N,3))                     &
     &           +PMPXI2(N,2)*(RDATR(2)*XDFTR(N,1)+RDATR(5)*XDFTR(N,2)  &
     &                        +RDATR(8)*XDFTR(N,3))                     &
     &           +PMPXI2(N,3)*(RDATR(3)*XDFTR(N,1)+RDATR(6)*XDFTR(N,2)  &
     &                        +RDATR(9)*XDFTR(N,3)))*DTRJ2K*SCALE(N)    &
     &   +(PMPXI2(N,1)*(DD1(1)*XMM(1)+DD1(4)*XMM(2)+DD1(7)*XMM(3))      &
     &    +PMPXI2(N,2)*(DD1(2)*XMM(1)+DD1(5)*XMM(2)+DD1(8)*XMM(3))      &
     &    +PMPXI2(N,3)*(DD1(3)*XMM(1)+DD1(6)*XMM(2)+DD1(9)*XMM(3))      &
     &          )*SCALE(N)
      PMPA(INDX3,N)=PMPA(INDX3,N)+SCALE(N)*(                            &
     &           PMPXI2(N,1)*(RDBTR(1)*XDFTR(N,1)+RDBTR(4)*XDFTR(N,2)   &
     &                       +RDBTR(7)*XDFTR(N,3))                      &
     &          +PMPXI2(N,2)*(RDBTR(2)*XDFTR(N,1)+RDBTR(5)*XDFTR(N,2)   &
     &                       +RDBTR(8)*XDFTR(N,3))                      &
     &          +PMPXI2(N,3)*(RDBTR(3)*XDFTR(N,1)+RDBTR(6)*XDFTR(N,2)   &
     &                       +RDBTR(9)*XDFTR(N,3)))                     &
     &          +SCALE(N)*(                                             &
     &    PMPXI2(N,1)*(DB(1)*XMM(1)+DB(4)*XMM(2)+DB(7)*XMM(3))          &
     &   +PMPXI2(N,2)*(DB(2)*XMM(1)+DB(5)*XMM(2)+DB(8)*XMM(3))          &
     &   +PMPXI2(N,3)*(DB(3)*XMM(1)+DB(6)*XMM(2)+DB(9)*XMM(3))          &
     &          )
! WITH A LITTLE SCRATCH SPACE, THE ABOVE LINE COULD BE EXPLOITED
!
      PMPA(INDX4,N)=PMPA(INDX4,N)                                       &
     &          +(PMPXI2(N,1)*(RDBTR(1)*XDFTR(N,1)+RDBTR(4)*XDFTR(N,2)  &
     &                        +RDBTR(7)*XDFTR(N,3))                     &
     &           +PMPXI2(N,2)*(RDBTR(2)*XDFTR(N,1)+RDBTR(5)*XDFTR(N,2)  &
     &                        +RDBTR(8)*XDFTR(N,3))                     &
     &           +PMPXI2(N,3)*(RDBTR(3)*XDFTR(N,1)+RDBTR(6)*XDFTR(N,2)  &
     &                        +RDBTR(9)*XDFTR(N,3)))*DTRJ2K*SCALE(N)    &
     &  +(PMPXI2(N,1)*(DD2(1)*XMM(1)+DD2(4)*XMM(2)+DD2(7)*XMM(3))       &
     &   +PMPXI2(N,2)*(DD2(2)*XMM(1)+DD2(5)*XMM(2)+DD2(8)*XMM(3))       &
     &   +PMPXI2(N,3)*(DD2(3)*XMM(1)+DD2(6)*XMM(2)+DD2(9)*XMM(3))       &
     &          )*SCALE(N)
! ROTATION PARTIALS
      PMPA(INDX5,N)=PMPA(INDX5,N)                                       &
     &             +SCALE(N)*                                           &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
! IF THIS IS NOT PLANETARY TARGET OPTION THE PARTIALS BELOW WILL BE ZERO
! ROTATION RATE PARTIALS
      PMPA(INDX6,N)=PMPA(INDX6,N)                                       &
     &             +DTOJ2K*SCALE(N)*                                    &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &          )
! IF THIS IS NOT PLANETARY TARGET OPTION THE PARTIALS BELOW WILL BE ZERO
! ROTATION RATE PARTIALS
      PMPA(INDX7,N)=PMPA(INDX7,N)                                       &
     &             +(DTOJ2K**2.D0)*SCALE(N)*                            &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &          )
!
      DO 14000 I=1,NXALLC
      INDX8=INDX7+2*(I-1)+1
      LAVOID(INDX8)=.FALSE.
      PMPA(INDX8,N)=PMPA(INDX8,N)+ (COS(WT(I))*SCALE(N))*              &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
!
      INDX9=INDX8+1
      LAVOID(INDX9)=.FALSE.
      PMPA(INDX9,N)=PMPA(INDX9,N)+ (SIN(WT(I))*SCALE(N))*              &
     &   (PMPXI2(N,1)*(DDR(1,N)*XMM(1)+DDR(4,N)*XMM(2)+DDR(7,N)*XMM(3)) &
     &   +PMPXI2(N,2)*(DDR(2,N)*XMM(1)+DDR(5,N)*XMM(2)+DDR(8,N)*XMM(3)) &
     &   +PMPXI2(N,3)*(DDR(3,N)*XMM(1)+DDR(6,N)*XMM(2)+DDR(9,N)*XMM(3)) &
     &           )
!
14000 END DO
12700 END DO
15000 CONTINUE
      RETURN
      END
