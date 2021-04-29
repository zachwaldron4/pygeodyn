!$ESTIMG
      SUBROUTINE ESTIMG(AA,II,LL,SUM1,SUM2,PDELTA,PARMV0,PARMVP,        &
     &   PARMVC,PARMSG,PARVAR,PNAME ,IPTRAU,PRMSG0)
!********1*********2*********3*********4*********5*********6*********7**
! ESTIMG           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CONTROLS GLOBAL PARAMETER ADJUSTMENT AFTER THE ARC PORTION
!            OF THE NORMAL EQUATIONS HAVE BEEN FOLDED INTO THE GLOBAL
!            PORTION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   SUM1    I/O   A    NORMAL EQUATION MATRIX
!   SUM2    I/O   A    RIGHT HAND SIDE OF NORMAL EQUATIONS
!   PDELTA  I/O   A    CHANGE IN PARAMETER VALUE (APRIORI - CURRENT)
!   PARMV0   I    A    APRIORI PARAMETER VALUE
!   PARMVP  I/O   A    PREVIOUS PARAMETER VALUE
!   PARMVC  I/O   A    CURRENT PARAMETER VALUE
!   PARMSG  I/O   A    STANDARD DEVIATION OF ADJUSTED PARAMETERS
!   PARVAR  I/O   A    DIAGONAL PORTION OF THE INVERTED APRIORI
!                      PARAMETER VAR. - COV. MATRIX
!   PNAME    I    A    PARAMETER NAME
!   IPTRAU   I    A    POINTER MAPPING ARRAY FROM ADJUSTED TO UNADJUSTED
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL(L)
      SAVE
      INCLUDE 'COMMON_DECL.inc'
      COMMON/SAV_VLB/VARC(10000),VGLB(10000),EST_VEC(10000)
      COMMON/AXIS/LINTAX
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CLVIEW/LNPRNT(18),NXCLVI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/CPARTI/MAXLP ,KORSIZ,MAXWRK,                               &
     &              IUNTP1,IUNTP2,IUNTPF,ISIZPF,IRECPF,NRECPF,          &
     &              NPDONE,NP1   ,NP2   ,NRECND,NPDIM ,NXPARI
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/CUNIT9/L9SET ,L9RES ,L9RSST,L9RSTY,L9EBS ,L9APAR,L9SUMP,   &
     &              L9GPAR,L9UPDT,L9LAST,L9OUT ,NXUNT9
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
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
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/TARGET/NLXYZ(200000),NPLTAR,NTARG,NATARG,NXTARG
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      COMMON/XPCNT2/NXTRAC2,NXFLAG2,NXALLC2,NCPPER2,NMINRT2,NDPOR2,     &
     &              NXXTR2
      COMMON / VLBI_BLOCK / ADR_MEM, ADR_EST_GLO, ADR_EST_ARC,  &
     & ADR_EST_VEC

      DIMENSION AA(1),II(1),LL(1),     SUM1  (MAXWRK),SUM2  (MAPARM),   &
     &   PDELTA(MAPARM),PARMV0(MAPARM),PARMVP(MAPARM),PARMVC(MAPARM),   &
     &   PARMSG(MAPARM),PARVAR(MAPARM),IPTRAU(MAPARM),PNAME(2,MPARM),   &
     &   PRMSG0(MAPARM)
      DIMENSION TP(15),TC(15),T0(15)
      DIMENSION VP(30)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      INDXNO(I)=MAPARM*(I-1)-(I*(I-1))/2
!
      NT=(MAPARM*(MAPARM+1))/2

      NGLBAL=NPVAL0(IXGLBL)
      NSTART=MAPARM-NGLBAL+1
      ISTART=INDXNO(NSTART)+NSTART
      JJJ=MAPARM
      KKK=NSTART
!
! COMPUTE APRIORI MINUS CURRENT
      DO 1000 I=NSTART,MAPARM
      PDELTA(I)=PARMV0(I)-PARMVC(I)
 1000 END DO
!
!  CALL WTADD TO ADD APRIORI INFORMATION TO ADJUSTED PARAMETERS
      NTA=NETADJ+NOTADJ
      ISPOL=IPVAL0(IXPOLX)-NSTART+1
      ISTID=IPVAL0(IXTIDE)-NSTART+1
      ISTAS=IPVAL0(IXSTAP)-NSTART+1
      ISTAV=IPVAL0(IXSTAV)-NSTART+1
      IF(ICLINK.GT.0) KKK=NSTART+ICLINK

      CALL WTADDG(NGLBAL,PDELTA(KKK),PARVAR(NSTART),NPOLEA,             &
     &            ISPOL,AA(KPOLCV),NTA,ISTID,                           &
     &            AA(KTIDCV),NMSTA,NSTAV,ISTAS,AA(KSTACV),              &
     &            SUM1(ISTART),SUM2(NSTART),ISTAV)
!
!  CALL DSINV TO GET ARC NORMAL INVERSE
      CALL DSINV(SUM1  (ISTART),NGLBAL,NGLBAL,PDELTA(NSTART))
!  CALL SOLVE TO SOLVE FOR ARC PARAMETERS ON INNER ITERATION
! DEBUG SUM1
      CALL SOLVE ( SUM1(ISTART), SUM2(NSTART), NGLBAL, NGLBAL,      &
     &             PDELTA(NSTART) )
!     IF ( MTYPE .EQ. 31 .OR.MTYPE.EQ.36) THEN
!
! -------- If VLBI, save vector of adjustments
!
           CALL COPY_R8 ( NGLBAL, PDELTA(NSTART), VGLB )
            do JW=2,5
!              write  ( 6, * ) 'PDELTA ',PDELTA(NSTART-1+JW)
!              write  ( 6, * ) 'PARMV0 ',PARMV0(NSTART-1+JW)
               VNEW=PARMVC(NSTART-1+JW)-PDELTA(NSTART-1+JW)
       IF(NSTART-1+JW.EQ.35)     &
     &   CALL DEGOUT(PARMV0(NSTART-1+JW),IH,IM,SECONDS,2)
        if(ABS(IH).GT.24) IH=MOD(IH,24)
       IF(NSTART-1+JW.EQ.35)write(6,*)' RADap 1',IH,IM,SECONDS
       IF(NSTART-1+JW.EQ.35)     &
     &   CALL DEGOUT(PDELTA(NSTART-1+JW),IH,IM,SECONDS,2)
        if(ABS(IH).GT.24) IH=MOD(IH,24)
       IF(NSTART-1+JW.EQ.35)write(6,*)' RAD 1',IH,IM,SECONDS
       IF(NSTART-1+JW.EQ.35)CALL DEGOUT(VNEW,IH,IM,SECONDS,2)
       IF(NSTART-1+JW.EQ.35)write(6,*)' RA 1',IH,IM,SECONDS
        if(ABS(IH).GT.24) IH=MOD(IH,24)
       IF(NSTART-1+JW.EQ.35)write(6,*)' RA 1',IH,IM,SECONDS



       IF(NSTART-1+JW.EQ.37)     &
     &   CALL DEGOUT(PARMV0(NSTART-1+JW),IH,IM,SECONDS,2)
        if(ABS(IH).GT.24) IH=MOD(IH,24)
       IF(NSTART-1+JW.EQ.37)write(6,*)' RADap 2',IH,IM,SECONDS
       IF(NSTART-1+JW.EQ.37)     &
     &   CALL DEGOUT(PDELTA(NSTART-1+JW),IH,IM,SECONDS,2)
        if(ABS(IH).GT.24) IH=MOD(IH,24)
       IF(NSTART-1+JW.EQ.37)write(6,*)' RAD 2',IH,IM,SECONDS
       IF(NSTART-1+JW.EQ.37)CALL DEGOUT(VNEW,IH,IM,SECONDS,2)
       IF(NSTART-1+JW.EQ.37)write(6,*)' RA 2',IH,IM,SECONDS
        if(ABS(IH).GT.24) IH=MOD(IH,24)
       IF(NSTART-1+JW.EQ.37)write(6,*)' RA 2',IH,IM,SECONDS



       IF(NSTART-1+JW.EQ.36)     &
     &   CALL DEGOUT(PARMV0(NSTART-1+JW),ID,IM,SECONDS,1)
        if(ABS(ID).GT.360) ID=MOD(ID,360)
       IF(NSTART-1+JW.EQ.36)write(6,*)' DECDap 1',ID,IM,SECONDS
       IF(NSTART-1+JW.EQ.36)     &
     &   CALL DEGOUT(PDELTA(NSTART-1+JW),ID,IM,SECONDS,1)
        if(ABS(ID).GT.360) ID=MOD(ID,360)
       IF(NSTART-1+JW.EQ.36)write(6,*)' DECD 1',ID,IM,SECONDS
       IF(NSTART-1+JW.EQ.36)CALL DEGOUT(VNEW,ID,IM,SECONDS,1)
       IF(NSTART-1+JW.EQ.36)write(6,*)' DEC 1',ID,IM,SECONDS
        if(ABS(IH).GT.360) IH=MOD(IH,360)
       IF(NSTART-1+JW.EQ.36)write(6,*)' DEC 1',ID,IM,SECONDS



       IF(NSTART-1+JW.EQ.38)     &
     &   CALL DEGOUT(PARMV0(NSTART-1+JW),ID,IM,SECONDS,1)
        if(ABS(ID).GT.360) ID=MOD(ID,360)
       IF(NSTART-1+JW.EQ.38)write(6,*)' DECDap 2',ID,IM,SECONDS
       IF(NSTART-1+JW.EQ.38)     &
     &   CALL DEGOUT(PDELTA(NSTART-1+JW),ID,IM,SECONDS,1)
        if(ABS(ID).GT.360) ID=MOD(ID,360)
       IF(NSTART-1+JW.EQ.38)write(6,*)' DECD 2',ID,IM,SECONDS
       IF(NSTART-1+JW.EQ.38)CALL DEGOUT(VNEW,ID,IM,SECONDS,1)
       IF(NSTART-1+JW.EQ.38)write(6,*)' DEC 2',ID,IM,SECONDS
        if(ABS(ID).GT.360) ID=MOD(ID,360)
       IF(NSTART-1+JW.EQ.38)write(6,*)' DEC 2',ID,IM,SECONDS

            enddo
!              call print_vec ( maparm, pdelta ) ! %%%%%%
!      END IF
!
! RE-MAKE DELTA IF LINKS ARE PRESENT
      IF(ICLINK.GT.0) CALL REDELT(PDELTA,MAPARM,II(KLINK),ICLINK)
      NSTART=NSTART+ICLINK
! UPDATE PREVIOUS PARAMETER VALUE ARRAY
      IF(ICLINK.GT.0) JJJ=MAPARM+ICLINK
      DO 2000 I=NSTART,JJJ
      PARMVP(I)=PARMVC(I)
 2000 END DO


! ADD DIFFERENTIAL CORRECTION TO CURRENT PARAMETER VALUE ARRAY
      IPLANOR_MIN_INDEX = IPVAL0(IXXTRO)
      IF(.NOT.LBINAST) THEN
      IPLANOR_MAX_INDEX = IPVAL0(IXXTRO)-1 + NXALLC*2 + 7
      ELSE
      IPLANOR_MAX_INDEX=IPVAL0(IXXTRO)-1+NXALLC*2+7+NXALLC2*2+7+NMINRT2
      ENDIF
      DO 3000 I=NSTART,JJJ
      IF(NPVAL0(IXXTRO).GT.0) THEN
          IF (I >= IPLANOR_MIN_INDEX .AND. I <= IPLANOR_MAX_INDEX) THEN
              ! FOR PLANOR PARAMETERS, THE DELTAS NEED TO BE CONVERTED
              ! FROM DEGREES TO RADIANS
              PARMVC(I) = PARMVC(I) + PDELTA(I)*DEGRAD
          ELSE
              PARMVC(I) = PARMVC(I) + PDELTA(I)
          END IF
      ELSE
! HERE IF THERE ARE NO PLANOR PARAMETERS
              PARMVC(I) = PARMVC(I) + PDELTA(I)
      ENDIF
 3000 END DO




! COMPUTE TOTAL ADJUSTMENT
      DO 4000 I=NSTART,JJJ
      SUM2  (I)=PARMVC(I)-PARMV0(I)
 4000 END DO
! COMPUTE STANDARD DEVIATION OF ADJUSTED PARAMETERS
      INCR=NGLBAL
      J=ISTART
      DO 5000 I=NSTART,JJJ
      PARMSG(I)=SQRT(SUM1(J))
      J=J+INCR
      INCR=INCR-1
 5000 END DO
! CONVERT TARGET PARAMETERS TO PHI, LAMBDA, R IF NECESSARY
      IF(NPVAL0(IXTARG).GT.0) THEN
      IF(NLXYZ(KTARNO).EQ.1) THEN
      IPTT=IPVAL0(IXTARG)
      IND=1
      DO 5100 I=1,NATARG
      CALL PLROUT(PARMV0(IPTT-1+IND),T0(IND))
      CALL PLROUT(PARMVC(IPTT-1+IND),TC(IND))
      CALL PLROUT(PARMVP(IPTT-1+IND),TP(IND))
      IND=IND+3
 5100 END DO
      ELSE
      IF(NLXYZ(KTARNO).EQ.2) THEN
      IPTT=IPVAL0(IXTARG)
      IND=1
      DO 5110 I=1,NATARG
      DO 5115 J=1,2
      PARMVC(IPTT-2+IND+J)=MOD(PARMVC(IPTT-2+IND+J),TWOPI)
      PARMV0(IPTT-2+IND+J)=PARMV0(IPTT-2+IND+J)/DEGRAD
      PARMVC(IPTT-2+IND+J)=PARMVC(IPTT-2+IND+J)/DEGRAD
      PARMVP(IPTT-2+IND+J)=PARMVP(IPTT-2+IND+J)/DEGRAD
 5115  CONTINUE
      IND=IND+3
 5110  CONTINUE
      ENDIF
      IF(NLXYZ(KTARNO).EQ.3) THEN
      IPTT=IPVAL0(IXTARG)
      IND=1
      DO 5120 I=1,NATARG
      PARMVC(IPTT-2+IND+3)=MOD(PARMVC(IPTT-2+IND+3),TWOPI)
      PARMV0(IPTT-2+IND+3)=PARMV0(IPTT-2+IND+3)/DEGRAD
      PARMVC(IPTT-2+IND+3)=PARMVC(IPTT-2+IND+3)/DEGRAD
      PARMVP(IPTT-2+IND+3)=PARMVP(IPTT-2+IND+3)/DEGRAD
      IND=IND+3
 5120  CONTINUE
      ENDIF
      ENDIF
      ENDIF


! ***** PLANOR ******
! THE CODE BELOW CONTAINS EXPLANATIONS ON HOW WE PRINT OUT VALUES FOR
! PLANOR IN THE CORRECT UNITS FOR THE CASE
!PLANOR1         1                                                 0.0000 .1D-40
!PLANOR2         1                                                        .1D-40
!PLANOR3         1                                                90.0000 .1D-40
!PLANOR4         1                                                        .1D-40
!PLANOR5         1                                               0.0      .1D-40
!PLANOR6         1                                               360.0    .1D-40
!PLANOR7         1                                                        .1D-40
!PLANOR11        1         18973.4002                                     .1D-40
!PLANOR12        1                                                        .1D-40
!PLANOR21        1         18973.4002                                     .1D-40
!PLANOR22        1                                                        .1D-40
!PLANOR31        1         18973.4002                                     .1D-40
!PLANOR32        1                                                        .1D-40
!PLANOR1         2                                                0.0     .1D-40
!PLANOR2         2                                                        .1D-40
!PLANOR3         2                                                90.0000 .1D-40
!PLANOR4         2                                                        .1D-40
!PLANOR5         2                                                 0.0    .1D-40
!PLANOR6         2                                               360.0    .1D-40
!PLANOR7         2                                                        .1D-40
!PLANOR11        2         18973.4002                                     .1D-40
!PLANOR12        2                                                        .1D-40
!PLANOR21        2         18973.4002                                     .1D-40
!PLANOR22        2                                                        .1D-40
!PLANOR31        2         18973.4002                                     .1D-40
!PLANOR32        2                                                        .1D-40
!PLANOR 1        2                                           1.460096E+151.0D-40
!PLANOR 2        2                                           1.517356E+151.0D-40
!PLANOR 3        2                                           1.640224E+151.0D-40
!PLANOR 4        2                                           6.630338E+101.0D-40
!PLANOR 5        2                                           2.955852E+101.0D-40
!PLANOR 6        2                                          -2.040129E+111.0D-40

!
! SAVE PLANOR PARAMETERS

!NXALLC is the number of periodic terms / for the first or only body
! example in the following case NXALLC is 6/2=3
!PLANOR11        1         18973.4002                                     .1D-40
!PLANOR12        1                                                        .1D-40
!PLANOR21        1         18973.4002                                     .1D-40
!PLANOR22        1                                                        .1D-40
!PLANOR31        1         18973.4002                                     .1D-40
!PLANOR32        1                                                        .1D-40
! Remember for a second asteroid the variable is called NXALLC2

              IF(NXALLC.GT.0) THEN ! periodic terms present
      IF(NPVAL0(IXXTRO).GT.0) THEN

! IPT is the starting location of the first body RA
          IPT=IPVAL0(IXXTRO)

! Saving RA after converting from radians back  to degrees
          V1 =  PARMVC(IPT-1+1)/DEGRAD
! Saving DEC after converting from radians back to degrees
          V3 =  PARMVC(IPT-1+3)/DEGRAD
! Saving ROT after converting from radians back to degrees
          V5 =  PARMVC(IPT-1+5)/DEGRAD
! Saving RA RATE after converting from rad/sec to deg/Julian cent of 36525 days
          V2 = (PARMVC(IPT-1+2)/DEGRAD)*86400.D0*36525.D0
! Saving DEC RATE after converting from rad/sec to deg/Julian cent of 36525 days
          V4 = (PARMVC(IPT-1+4)/DEGRAD)*86400.D0*36525.D0
! Saving ROT RATE after converting from rad/sec to deg/day
          V6 = (PARMVC(IPT-1+6)/DEGRAD)*86400.D0
! Saving ROT ACC after converting from rad/sec**2 to deg/day*22
          V7 = (PARMVC(IPT-1+7)/DEGRAD)*(86400.D0**2.D0)
! Print values on unit 6
          write(6,77770)
          write(6,77771)V1
          write(6,77772)V2
          write(6,77773)V3
          write(6,77774)V4
          write(6,77775)V5
          write(6,77776)V6
          write(6,77777)V7

! Below analytical model
          IF (NMINRT == 0) THEN
              K=0
              M=0
              DO J=1,NXALLC
                  VP(1+J-1+M)=PARMVC(IPT-1+8+K)/DEGRAD
                  VP(2+J-1+M)=PARMVC(IPT-1+9+K)/DEGRAD
                  K=K+2
                  M=M+1
              ENDDO
!  FOR NXALLC = 3 in the example above the loop above has filled up
!  after converting to degrees
!  VP(1)=PARMVC(IPT+7)
!  VP(2)=PARMVC(IPT+7)
!  VP(3)=PARMVC(IPT+9)
!  VP(4)=PARMVC(IPT+10)
!  VP(5)=PARMVC(IPT+11)
!  VP(6)=PARMVC(IPT+12)
! IF NXALLC goes up to 6 (2 sets of coefficients) then the loop above will
! fill up VP(7) to VP(12)

              write(6,*)' estimg nxallc ',NXALLC
              K=0
! The loop below if NXALLC is 3 will write out VP(1),VP(2) k=0
!                                              VP(3),VP(4) k=1
!                                              VP(5),VP(6) k=2
              DO KJJ=1,NXALLC
!!!!              if(KJJ.gt.3) K=K+6
                  write(6,77778)VP(KJJ+K)
                  write(6,77779)VP(KJJ+1+K)
                  K=K+1
              ENDDO
          ENDIF ! NMINRT

      ENDIF ! NPVAL0(IXXTRO)
              ENDIF ! NXALLC

! If there is only one body then there is nothing else to be done in PLANOR
! If there are moments of inertia for the first body then there is no need
! to convert units.
! IF THERE IS NO SECOND BODY THEN SKIP THE CODE

           IF(LBINAST) THEN
        IF(NMINRT2.EQ.0.AND.NXALLC2.EQ.0) GO TO 888

! NOW we must consider the second body PLANOR parameters


              IF(NXALLC2.GT.0) THEN ! periodic terms present for second a
      IF(NPVAL0(IXXTRO).GT.0) THEN

! FIND THE LOCATION OF THE RA PARAMETER FOR THE SECOND BODY
!         RA OF THE SECOND ASTEROID WILL START AT IPT+  7+NXALLC*2
          IPT=IPVAL0(IXXTRO) + 7+NXALLC*2
          IF(NXALLC.EQ.0.AND.NMINRT.EQ.6) IPT=IPVAL0(IXXTRO)+6
          IF(NXALLC.NE.0.AND.NMINRT.EQ.6) IPT=IPT+6

          V1 =  PARMVC(IPT-1+1)/DEGRAD
          V3 =  PARMVC(IPT-1+3)/DEGRAD
          V5 =  PARMVC(IPT-1+5)/DEGRAD
          V2 = (PARMVC(IPT-1+2)/DEGRAD)*86400.D0*36525.D0
          V4 = (PARMVC(IPT-1+4)/DEGRAD)*86400.D0*36525.D0
          V6 = (PARMVC(IPT-1+6)/DEGRAD)*86400.D0
          V7 = (PARMVC(IPT-1+7)/DEGRAD)*(86400.D0**2.D0)
          write(6,77760)
          write(6,77771)V1
          write(6,77772)V2
          write(6,77773)V3
          write(6,77774)V4
          write(6,77775)V5
          write(6,77776)V6
          write(6,77777)V7
          IF (NMINRT2 == 0) THEN
              K=0
              M=0
              DO J=1,NXALLC2
                  VP(1+J-1+M)=PARMVC(IPT-1+8+K)/DEGRAD
                  VP(2+J-1+M)=PARMVC(IPT-1+9+K)/DEGRAD
                  K=K+2
                  M=M+1
              ENDDO
              write(6,*)' estimg nxallc ',NXALLC2
              K=0
              DO KJJ=1,NXALLC2
!!!!!             if(KJJ.gt.6) K=K+6
                  write(6,77778)VP(KJJ+K)
                  write(6,77779)VP(KJJ+1+K)
                  K=K+1
              ENDDO
          ENDIF
      ENDIF
              ENDIF ! NXALLC2

77770 FORMAT(//,1X,' RECOVERED PLANOR VALUES IN THE GEODYN INPUT',  &
     &' SYSTEM ')
77760 FORMAT(//,1X,' RECOVERED PLANOR VALUES IN THE GEODYN INPUT',  &
     &' SYSTEM FOR SECOND ASTEROID ')
77771 FORMAT(1X,' PLANOR1 : ',F40.15)
77772 FORMAT(1X,' PLANOR2 : ',F40.15)
77773 FORMAT(1X,' PLANOR3 : ',F40.15)
77774 FORMAT(1X,' PLANOR4 : ',F40.15)
77775 FORMAT(1X,' PLANOR5 : ',F40.15)
77776 FORMAT(1X,' PLANOR6 : ',F40.15)
77777 FORMAT(1X,' PLANOR7 : ',D40.10)
77778 FORMAT(1X,' PLANOR ACOEF : ',F40.15)
77779 FORMAT(1X,' PLANOR BCOEF : ',F40.15,//)

  888 CONTINUE
          ENDIF ! IF LBINAST

! PRINT PARAMETER ADJUSTMENT SUMMARY
      IF(ICLINK.GT.0) JJJ=MAPARM+ICLINK
      ILINE6=MLINE6
      DO I=NSTART,JJJ
          N=IPTRAU(I)
          JLINE6=ILINE6+4
          IF (JLINE6 > MLINE6) THEN
              IPAGE6=IPAGE6+1
              JLINE6=9
              WRITE(IOUT6,10000) NGLOBL,IPAGE6
              WRITE(IOUT6,20000)
          END IF
          ILINE6=JLINE6

      IF(NPVAL0(IXXTRO).GT.0) THEN
          ! CONVERT UNITS FOR PLANOR PARAMETER PRINTOUT
          IF (I >= IPLANOR_MIN_INDEX .AND. I <= IPLANOR_MAX_INDEX) THEN
              PV0 = PARMV0(I) / DEGRAD
              PVP = PARMVP(I) / DEGRAD
              PVC = PARMVC(I) / DEGRAD
              SM2 = SUM2(I) / DEGRAD
              IF (.NOT. LINTAX) THEN
                  SELECT CASE (I-IPLANOR_MIN_INDEX+1)
                      CASE (2, 4)
                          ! RA_DOT and DEC_DOT
                          PV0 = PV0 * 86400.D0 * 36525.D0
                          PVP = PVP * 86400.D0 * 36525.D0
                          PVC = PVC * 86400.D0 * 36525.D0
                      CASE (6)
                          ! W_DOT
                          PV0 = PV0 * 86400.D0
                          PVP = PVP * 86400.D0
                          PVC = PVC * 86400.D0
                      CASE (7)
                          ! W_DOT_DOT
                          PV0 = PV0 * 86400.D0**2
                          PVP = PVP * 86400.D0**2
                          PVC = PVC * 86400.D0**2
                  END SELECT
              END IF
          ELSE
              PV0 = PARMV0(I)
              PVP = PARMVP(I)
              PVC = PARMVC(I)
              SM2 = SUM2(I)
          END IF

          WRITE(IOUT6,30000) (PNAME(J,N),J=1,2), PV0, PVP,              &
     &            SM2, PRMSG0(I), PVC, PDELTA(I), PARMSG(I)

      ELSE
! HERE IF THERE ARE NO PLANOR PARAMETERS
      WRITE(IOUT6,30000) (PNAME(J,N),J=1,2),PARMV0(I),                  &
     &   PARMVP(I),SUM2  (I),PRMSG0(I),PARMVC(I),PDELTA(I),             &
     &   PARMSG(I)
      ENDIF
      END DO
      IF(.NOT.L9GPAR) GO TO 9000
! SEND PARAMETER ADJUSTMENT SUMMARY TO TERMINAL
      IF(ICLINK.GT.0) JJJ=MAPARM+ICLINK
      ILINE9=MLINE9
      DO I=NSTART,JJJ
          N=IPTRAU(I)
          JLINE9=ILINE9+4
          IF (JLINE9 > MLINE9) THEN
              IPAGE9=IPAGE9+1
              JLINE9=9
              WRITE(IOUT9,10009) NGLOBL,IPAGE9
              WRITE(IOUT9,20000)
          END IF
          ILINE9=JLINE9

      IF(NPVAL0(IXXTRO).GT.0) THEN
          ! CONVERT UNITS FOR PLANOR PARAMETER PRINTOUT
          IF (I >= IPLANOR_MIN_INDEX .AND. I <= IPLANOR_MAX_INDEX) THEN
              PV0 = PARMV0(I) / DEGRAD
              PVP = PARMVP(I) / DEGRAD
              PVC = PARMVC(I) / DEGRAD
              SM2 = SUM2(I) / DEGRAD
              IF (.NOT. LINTAX) THEN
                  SELECT CASE (I-IPLANOR_MIN_INDEX+1)
                      CASE (2, 4)
                          ! RA_DOT and DEC_DOT
                          PV0 = PV0 * 86400.D0 * 36525.D0
                          PVP = PVP * 86400.D0 * 36525.D0
                          PVC = PVC * 86400.D0 * 36525.D0
                      CASE (6)
                          ! W_DOT
                          PV0 = PV0 * 86400.D0
                          PVP = PVP * 86400.D0
                          PVC = PVC * 86400.D0
                      CASE (7)
                          ! W_DOT_DOT
                          PV0 = PV0 * 86400.D0**2
                          PVP = PVP * 86400.D0**2
                          PVC = PVC * 86400.D0**2
                  END SELECT
              END IF
          ELSE
              PV0 = PARMV0(I)
              PVP = PARMVP(I)
              PVC = PARMVC(I)
              SM2 = SUM2(I)
          END IF

          WRITE(IOUT9,30000) (PNAME(J,N),J=1,2), PV0, PVP, &
     &            SM2, PRMSG0(I), PVC, PDELTA(I), PARMSG(I)
      ELSE
! HERE IF THERE ARE NO PLANOR PARAMETERS
      WRITE(IOUT9,30000) (PNAME(J,N),J=1,2),PARMV0(I),                  &
     &   PARMVP(I),SUM2  (I),PRMSG0(I),PARMVC(I),PDELTA(I),             &
     &   PARMSG(I)
      ENDIF

      END DO
 9000 CONTINUE
      IF(NPVAL0(IXTARG).GT.0) THEN
          IF(NLXYZ(KTARNO).EQ.1) THEN
              DO I=1,NATARG
                  WRITE(6,9498)I
 9498             FORMAT('     TARGET ',I3,'COORDINATES IN PHI, LAMBDA, &
                         &RADIUS')
                  WRITE(6,*)'            A-PRIORI          PREVIOUS     &
                            &CURRENT'
                  IND=1
                  DO J=1,3
                      WRITE(6,9499) T0(IND-1+J),                        &
     &                              TP(IND-1+J),                        &
     &                              TC(IND-1+J)
 9499                 FORMAT(1X,3F20.10)
                  END DO
                  IND=IND+3
              END DO
          ENDIF
      ENDIF
! UPDATE STATION VELOCITY ARRAYS
      IVX=1
      ISTVV=IPVAL0(IXSTAV)
      DO 9040 I=1,NSTAV
      IVX=(I-1)*3
      ISTV=ISTVV+IVX
      AA(KSTVEL+IVX)        =PARMVC(ISTV)
      AA(KSTVEL+IVX+1)        =PARMVC(ISTV+1)
      AA(KSTVEL+IVX+2)        =PARMVC(ISTV+2)
 9040 END DO
! UPDATE STATION ARRAYS AND GET BASELINES
      ISTASS=IPVAL0(IXSTAP)
      DO 9050 I=1,NMSTA
      ISTAS=ISTASS+(I-1)*3
      IPT1=INDXNO(ISTAS)+ISTAS
      IPT2=INDXNO(ISTAS+1)+ISTAS+1
      IPT3=INDXNO(ISTAS+2)+ISTAS+2
      ICRD=MOD(II(KICRD+I-1),10)-1
      JSTAS=KSPSIG+(I-1)*18+ICRD*6
      AA(JSTAS)=SUM1(IPT1)
      AA(JSTAS+1)=SUM1(IPT1+1)
      AA(JSTAS+2)=SUM1(IPT1+2)
      AA(JSTAS+3)=SUM1(IPT2)
      AA(JSTAS+4)=SUM1(IPT2+1)
      AA(JSTAS+5)=SUM1(IPT3)
!************************************PATCH
      ISPCRD=KSPCRD+(I-1)*3
      IF(ICRD.NE.1) GO TO 9010
      KSTAS=KSXYZ+(I-1)*3
      PARMVC(ISTAS)=AA(KSTAS)+PDELTA(ISTAS)
      PARMVC(ISTAS+1)=AA(KSTAS+1)+PDELTA(ISTAS+1)
      PARMVC(ISTAS+2)=AA(KSTAS+2)+PDELTA(ISTAS+2)
      AA(ISPCRD)=PARMVC(ISTAS)
      AA(ISPCRD+1)=PARMVC(ISTAS+1)
      AA(ISPCRD+2)=PARMVC(ISTAS+2)
      GO TO 9050
 9010 CONTINUE
      IF(ICRD.NE.0) GO TO 9020
      KSTAS=KSTAIN+(I-1)*NSTAIN
      PARMVC(ISTAS)=AA(KSTAS)+PDELTA(ISTAS)
      PARMVC(ISTAS+1)=AA(KSTAS+3)+PDELTA(ISTAS+1)
      PARMVC(ISTAS+2)=AA(KSTAS+6)+PDELTA(ISTAS+2)
      AA(ISPCRD)=PARMVC(ISTAS)
      AA(ISPCRD+1)=PARMVC(ISTAS+1)
      AA(ISPCRD+2)=PARMVC(ISTAS+2)
      GO TO 9050
 9020 CONTINUE
      KSTAS=KSXYZ+(I-1)*3
      SAD=AA(KSTAS)*AA(KSTAS)+AA(KSTAS+1)*AA(KSTAS+1)
      SAD=SQRT(SAD)
      XLONG=ATAN2(AA(KSTAS+1),AA(KSTAS))
      PARMVC(ISTAS)=SAD+PDELTA(ISTAS)
      PARMVC(ISTAS+1)=XLONG+PDELTA(ISTAS+1)
      PARMVC(ISTAS+2)=AA(KSTAS+2)+PDELTA(ISTAS+2)
      AA(ISPCRD)=PARMVC(ISTAS)
      AA(ISPCRD+1)=PARMVC(ISTAS+1)
      AA(ISPCRD+2)=PARMVC(ISTAS+2)
 9050 END DO
! GET CONSTRAINED STATIONS
      NXCON=NSTAE-NMSTA
      IF(NXCON.LE.0) GO TO 9090
      NMSTA1=NMSTA+1
      DO 9080 I=NMSTA1,NSTAE
      IMSTA=II(KICON-1+I)
      ISTAS=ISTASS+(IMSTA-1)*3
      ICRD=MOD(II(KICRD+IMSTA-1),10)-1
      ISPCRD=KSPCRD+(I-1)*3
      IF(ICRD.NE.1) GO TO 9060
      KSTAS=KSXYZ+(I-1)*3
      AA(ISPCRD)=AA(KSTAS)+PDELTA(ISTAS)
      AA(ISPCRD+1)=AA(KSTAS+1)+PDELTA(ISTAS+1)
      AA(ISPCRD+2)=AA(KSTAS+2)+PDELTA(ISTAS+2)
      GO TO 9080
 9060 CONTINUE
      IF(ICRD.NE.0) GO TO 9070
      KSTAS=KSTAIN+(I-1)*NSTAIN
      KPHLP1=KDPSDP+(I-1)*9
      KPHLP2=KPHLP1+3
      AA(ISPCRD)=AA(KSTAS)+PDELTA(ISTAS)*AA(KPHLP1)
      AA(ISPCRD+1)=AA(KSTAS+3)+PDELTA(ISTAS+1)+PDELTA(ISTAS)*AA(KPHLP2)
      AA(ISPCRD+2)=AA(KSTAS+6)+PDELTA(ISTAS+2)
      GO TO 9080
 9070 CONTINUE
      KSTAS=KSXYZ+(I-1)*3
      SAD=AA(KSTAS)*AA(KSTAS)+AA(KSTAS+1)*AA(KSTAS+1)
      SAD=SQRT(SAD)
      XLONG=ATAN2(AA(KSTAS+1),AA(KSTAS))
      AA(ISPCRD)=SAD+PDELTA(ISTAS)
      AA(ISPCRD+1)=XLONG+PDELTA(ISTAS+1)
      AA(ISPCRD+2)=AA(KSTAS+2)+PDELTA(ISTAS+2)
 9080 END DO
!************************************PATCH
 9090 CONTINUE
      CALL STACRD(II(KICRD),II(KICON),II(KISTNO),AA(KSTNAM),            &
     &            AA(KSPCRD),AA(KSPSIG),AA(KSXYZ),AA(KDXSDP),           &
     &            AA(KDPSDP),AA(KSTAIN),AA(KXLOCV),AA(KSPWT),           &
     &            AA(KPLTIN),AA(KPLTVL),AA(KPLTDV))
      IF(.NOT.LNPRNT(13)) CALL BSLINE(2,II(KICRD),II(KICON),II(KISTNO), &
     &   AA(KSTNAM),ISTASS,MAPARM,AA(KSPSIG),AA(KSXYZ),AA(KDXSDP),SUM1)
      CALL ADSTPR(AA(KSPWT),AA(KSXYZ),AA(KSTAIN),AA(KSPSIG),AA(KSTNAM), &
     &            II(KISTNO),AA(KSAVST))
      RETURN
10000 FORMAT('1PARAMETER ADJUSTMENT SUMMARY FOR GLOBAL ITERATION',I2,   &
     &   51X,'UNIT  6 PAGE NO.',I6)
10009 FORMAT('1PARAMETER ADJUSTMENT SUMMARY FOR GLOBAL ITERATION',I2,   &
     &    6X,'UNIT  9 PAGE NO.',I4)
20000 FORMAT('0 ','PARAMETER NAME',                                     &
     &    7X,'APRIORI  VALUE'/                                          &
     &   23X,'PREVIOUS VALUE', 9X,'TOTAL   DELTA', 5X,'APRIORI SIGMA'/  &
     &   23X,'CURRENT  VALUE', 9X,'CURRENT DELTA', 5X,'CURRENT SIGMA')
30000 FORMAT('0',2A8,G24.16/17X,G24.16,G20.12,G16.8/                    &
     &       17X,G24.16,G20.12,G16.8)
      END
