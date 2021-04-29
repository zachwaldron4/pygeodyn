!$ACCELR
      SUBROUTINE ACCELR(AA,II,LL,MJDS,FSEC,INDSAT,INDSET,INPSAT,INDSTA, &
     &           LELEVS,ACCTIM,ACCPER,NPERAC,PARMVC,PARSIG,PART,LNPNM,  &
     &           LAVOID,OBSC,OBS,ACCDOT,MTYPE,NM,LFORCE,NDIM1,NDIM2,FRC,&
     &           IAC,XYZDOT,F1,SBFTOD_TOT,ISATS,ISATID)
!********1*********2*********3*********4*********5*********6*********7**
! ACCELR           00/00/00            0000.0    PGMR - D. E. Pavlis
!
! FUNCTION:  ACCELEROMETER DATA PROCESSING
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES.
!   II      I/O   A    DYNAMIC ARRAY FOR INTEGER VARIABLES
!   LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL VARIABLES
!   INDSAT   I    A    FOR A GIVEN INTERNAL SAT NO (1,2,3) INDSAT
!                      GIVES THE ACTUAL LOCATION IN THE SATELLITE
!                      RELATED ARRAYS FOR THIS SATELLITE.
!                      (EG.   SAT ID=ISATNO(INDSAT(1,2,OR 3))
!   INDSET   I    A    FOR A GIVEN INTERNAL SATELLITE NUMBER INDSET TELL
!                      WHICH SAT SET THAT THE GIVEN SAT BELONGS TO.
!   INPSAT   I    A    POINTER ARRAY THAT RELATES THE INTERNAL SATELLITE
!                      NUMBER (1,2,3) TO THE SATELLITES DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISATP=INPSAT(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE SATELLITES DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL SATELLITE NO. 1)
!    LNPNM   I    S    TELLS WHETHER VECTORIZING OVER THE NUMBER OF
!                      ESTIMATED PARAMETERS OR OVER THE NUMBER
!                      OF MEASUREMENTS. (DEFAULT = FALSE)
!                       =T  VECTORIZE OVER PARTIALS
!                       =F  VECTORIZE OVER NUMBER OF MEAS.
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ACCELO/IDYNDL(3),IDYNSN(3),IGEOSN(3),NXACCO
      COMMON/ACCLRM/MDYNPD,MBAPPD,MACOBS,IDYNSC(200),MDYNST,IACCSC(200),&
     &              MACCSC,NACPRM(200),NATPRM(200),NXCLRM
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CORA02/KFSCTB,KFSCTC,KFSCTE,KDSECT,                        &
     &              KFSECT,KS    ,KCIP  ,KCIV  ,                        &
     &       KXTN  ,KXSM  ,KXTK  ,KXSJ  ,KXTI  ,KXTKP ,                 &
     &       KVTN  ,KVSM  ,KVTK  ,KVSJ  ,KVTI  ,KVTKP ,                 &
     &       KSATLT,KSATLN,KSATH ,KCOSTH,KSINTH,                        &
     &       KPXPFM,KPXPFK,KPXPFJ,KTRBF ,KFSTRC,                        &
     &       KFSTRE,KDSCTR,KFSCVS,KXSMBF,KXSKBF,KXSJBF,                 &
     &       KRSSV1,KRSSV2,KRSSV3,KTPMES,KACOEF,KACTIM,                 &
     &       KXTNPC,KXSMPC,KXTKPC,KXSJPC,KXTIPC,KXTKPP,                 &
     &       KRLRNG,KASTO,KASTP,NXCA02
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
      COMMON/CORA05/KOBTYP,KDSCRP,KGMEAN,KGLRMS,KWGMEA,KWGRMS,KTYMEA,   &
     &       KTYRMS,KWTMTY,KWTYRM,KTMEAN,KTRMS ,KWMEAN,KWTRMS,KWTRND,   &
     &       KPRVRT,KEBSTT,KVLOPT,NXCA05
      COMMON/CORI02/KNTIME,KMJSTB,KMJSTC,KMJSTE,KISECT,                 &
     &              KINDH ,KNMH  ,KIVSAT,KIPXPF,KNTRTM,KMSTRC,          &
     &              KMSTRE,KISCTR,KISATR,KITRUN,KTRTMB,KTRTMC,          &
     &              KMJDVS,KNTMVM,NXCI02
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CRMI/RMI(9)
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

      DIMENSION AA(1),II(1),LL(1),INDSET(1),INDSAT(1),INDSTA(1),        &
     &   INPSAT(1),FSEC(1)
      DIMENSION LELEVS(3,4)
      DIMENSION ACCTIM(MBAPPD,2,NSATA),ACCPER(1,NSATA)
      DIMENSION NPERAC(1,NSATA),NRAT(1)
      DIMENSION ISATAC(1)
      DIMENSION PARMVC(1),PARSIG(1)
      DIMENSION TERM(5),TERMF(4,5)
      DIMENSION LAVOID(MAPARM),PART(NDIM1,NDIM2)
      DIMENSION BIAS(3),ACBIAS(4,3)
      DIMENSION IPTP(5),IPTPF(5,4)
      DIMENSION OBS(NM),OBSC(NM)
      DIMENSION ACCDOT(MINTIM,4),XYZDOT(3)
      DIMENSION FRC(3),F1(3)
      DIMENSION SBFTOD_TOT(3,3)
      DIMENSION ROTPAR(3,3)
      DIMENSION ISATS(NSATA)
      DIMENSION BIASP(5,3)
      LOGICAL, DIMENSION(:), ALLOCATABLE :: LEDIT_EXTRA

      DATA ZERO/0.D0/

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
      IADD=0
      DO I=1,MACCSC
      IF(ISATID.EQ.IACCSC(I)) THEN
      KTSAT=I
      IF(KTSAT.GT.1) IADD=NACPRM(I-1)
      ENDIF
      ENDDO
!
!
      IF(NPVAL0(IXACCB).GT.ZERO.AND.LFORCE) THEN
         ROTPAR(1,1)=RMI(1)*SBFTOD_TOT(1,1)+RMI(2)*SBFTOD_TOT(2,1)      &
     &              +RMI(3)*SBFTOD_TOT(3,1)
         ROTPAR(1,2)=RMI(1)*SBFTOD_TOT(1,2)+RMI(2)*SBFTOD_TOT(2,2)      &
     &              +RMI(3)*SBFTOD_TOT(3,2)
         ROTPAR(1,3)=RMI(1)*SBFTOD_TOT(1,3)+RMI(2)*SBFTOD_TOT(2,3)      &
     &              +RMI(3)*SBFTOD_TOT(3,3)
         ROTPAR(2,1)=RMI(4)*SBFTOD_TOT(1,1)+RMI(5)*SBFTOD_TOT(2,1)      &
     &              +RMI(6)*SBFTOD_TOT(3,1)
         ROTPAR(2,2)=RMI(4)*SBFTOD_TOT(1,2)+RMI(5)*SBFTOD_TOT(2,2)      &
     &              +RMI(6)*SBFTOD_TOT(3,2)
         ROTPAR(2,3)=RMI(4)*SBFTOD_TOT(1,3)+RMI(5)*SBFTOD_TOT(2,3)      &
     &              +RMI(6)*SBFTOD_TOT(3,3)
         ROTPAR(3,1)=RMI(7)*SBFTOD_TOT(1,1)+RMI(8)*SBFTOD_TOT(2,1)      &
     &              +RMI(9)*SBFTOD_TOT(3,1)
         ROTPAR(3,2)=RMI(7)*SBFTOD_TOT(1,2)+RMI(8)*SBFTOD_TOT(2,2)      &
     &              +RMI(9)*SBFTOD_TOT(3,2)
         ROTPAR(3,3)=RMI(7)*SBFTOD_TOT(1,3)+RMI(8)*SBFTOD_TOT(2,3)      &
     &              +RMI(9)*SBFTOD_TOT(3,3)
      ENDIF
!
!
!
! 10000 IS THE MAIN LOOP OF THE SUBROUTINE
!
       DO 10000 INM=1,NM
       IF(.NOT.LFORCE) THEN
!
         FACTS=1.D0
         IF(MTYPE.GE.201.AND.MTYPE.LE.203) FACTS=DBLE(IGEOSN(MTYPE-200))
!
       DO IQP=1,5
         BIASP(IQP,1)=0.D0
         BIASP(IQP,2)=0.D0
         BIASP(IQP,3)=0.D0
       ENDDO
       IF(MTYPE.EQ.200) THEN
           IL=1
       ELSE
           K=KOBS
           K1=KOBS+NM*5
           K2=KOBS+NM*6
           TOTAL=AA(K+INM-1)**2.D0+AA(K1+INM-1)**2.D0+AA(K2+INM-1)**2.D0
           TOTAL=SQRT(TOTAL)
           IF(MTYPE.EQ.201)  THEN
              IL=2
           ENDIF
           IF(MTYPE.EQ.202)  THEN
              IL=3
           ENDIF
           IF(MTYPE.EQ.203)  THEN
           IL=4
           ENDIF
       ENDIF
!
!
             ! IF(.NOT.LFORCE)
       ENDIF
!
!
!**************************************************************
!
! *** ACCBIA COMPUTATION THROUGH 5000
!
!

! FIGURE OUT IF THE OBSERVATION LIES WITHIN AN ACCBIA PERIOD
      IK=NPERAC(1,1)
      IFOUND=0
      T=DBLE(MJDS)+FSEC(INM)
      DO J=1,IK
        IF(T.LE.ACCTIM(J,2,1).AND.T.GE.ACCTIM(J,1,1)) IFOUND=J
      ENDDO
      T=DBLE(MJDS)
      J=IFOUND
!
!  NO BIAS PERIOD APPLIES
!

      IF(IFOUND.EQ.0) THEN
        IF(LFORCE) THEN
! THE FRC ARRAY DOES NOT TO BE CORRECTED FOR BIASES IF IFOUND=0
! ALSO NM=1 IN THIS CASE AND PROCES DOES NOT NEED TO BE CALLED ;
! SO RETURN
          RETURN
        ELSE
! IN GEOMETRIC MODE SET THE BIASES TO DEFAULTS IF IFOUND=0
          BIAS(1)=0.D0
          BIAS(2)=1.D0
          BIAS(3)=0.D0
! GO STRAIGHT TO THE COMPUTATION OF OBSC
          GO TO 6000
        ENDIF
      ENDIF
!
!   A BIAS PERIOD DOES APPLY
!
      T1=ACCTIM(J,1,1)
      T2=ACCTIM(J,2,1)
      PER=ACCPER(J,1)
! FIND THE POINTER TO PARAMETER VALUES ARRAY
!     IPT=IPVAL(IXACCB)+(J-1)*60
      IPT0=IPVAL0(IXACCB)+(J-1)*60
      IFADD=(J-1)*60

!
!
!
                             !  TYPE OF BIAS (ABS, SCALE, TIME)
      DO 5000 IJ=1,3
!
!
!
! IL IS DEFINED ABOVE ACCORDING TO MTYPE FOR OBS MODELING
!       if(il.eq.1) write(6,*)' dbg TOTAL ACCEL '
!       if(il.eq.2) write(6,*)' dbg X COMPONENT '
!       if(il.eq.3) write(6,*)' dbg Y COMPONENT '
!       if(il.eq.4) write(6,*)' dbg Z COMPONENT '
      IF(NPVAL0(IXACCB).GT.ZERO) THEN
         IF(.NOT.LFORCE) THEN
                            !  COEFFICIENT
            DO K=1,5
               IPTP(K)=IPT0+(IL-1)*5-1+K +(IJ-1)*20  +(KTSAT-1)*IADD
               LAVOID(IPTP(K))=.FALSE.
               TERM(K)=PARMVC(IPTP(K))
            ENDDO
          ELSE
                      ! TOT X Y Z
            DO IL=1,4
                               !  COEFFICIENT
               DO K=1,5
               IPTPF(K,IL)=IPT0+(IL-1)*5-1+K +(IJ-1)*20 +(KTSAT-1)*IADD
               TERMF(IL,K)=PARMVC(IPTPF(K,IL))
! RESET IPTPF TO POINT TO PXDDT(PART)
               IF(LFORCE) IPTPF(K,IL)=IAC+IFADD+(IL-1)*5-1+K +(IJ-1)*20
               ENDDO
            ENDDO
                  ! IF(.NOT.LFORCE)
         ENDIF
               ! IF(NPVAL0..
      ENDIF
!
!
        SINWT=ZERO
        COSWT=ZERO
        DT= T+FSEC(INM)-T1
        IF(PER.EQ.ZERO) GOTO 1000
          WT= (TWOPI/PER)*DT
          WT=MOD(WT,TWOPI)
          SINWT=SIN(WT)
          COSWT=COS(WT)
 1000   CONTINUE

! BIAS IS THE COMPUTED VALUE OF THE ACCELEROMETER BIAS FOR THIS OBSERVAT
          IF(.NOT.LFORCE) THEN
         BIASP(1,IJ)=TERM(1)
         BIASP(2,IJ)=TERM(2)
         BIASP(3,IJ)=TERM(3)
         BIASP(4,IJ)=TERM(4)
         BIASP(5,IJ)=TERM(5)
         BIAS(IJ)= TERM(1) + TERM(2)*DT + TERM(3)*(DT**2) +             &
     &         TERM(4)*SINWT + TERM(5)*COSWT
           ELSE
         DO IK=1,4
         ACBIAS(IK,IJ)= TERMF(IK,1) + TERMF(IK,2)*DT +                  &
     &         TERMF(IK,3)*(DT**2)+                                     &
     &         TERMF(IK,4)*SINWT + TERMF(IK,5)*COSWT
           ENDDO
                 ! IF(.NOT.LFORCE)
           ENDIF
!
!
! *** ACCBIA PARTIALS **************************************************
      IF(NPVAL0(IXACCB).LE.ZERO) GO TO 5000
!     if(ij.eq.1) write(6,*)' dbg ABSOLUTE BIAS '
!     if(ij.eq.2) write(6,*)' dbg SCALE BIAS '
!     if(ij.eq.3) write(6,*)' dbg TIMING BIAS '
      SCALE=-1.D0
      IF(LNPNM) THEN
         DT= T+FSEC(INM)-T1
         IF(PER.EQ.ZERO) GOTO 2000
           WT= (TWOPI/PER)*DT
           WT=MOD(WT,TWOPI)
           SINWT=SIN(WT)
           COSWT=COS(WT)
 2000    CONTINUE
         IF(IJ.EQ.2) SCALE=-FACTS*OBS(INM)
         IF(IJ.EQ.3) SCALE=-ACCDOT(INM,IL)
         PART(IPTP(1),INM)=PART(IPTP(1),INM)+SCALE
         PART(IPTP(2),INM)=PART(IPTP(2),INM)+SCALE*DT
         PART(IPTP(3),INM)=PART(IPTP(3),INM)+SCALE*(DT**2.D0)
         PART(IPTP(4),INM)=PART(IPTP(4),INM)+SCALE*SINWT
         PART(IPTP(5),INM)=PART(IPTP(5),INM)+SCALE*COSWT
             ! IF(.NOT.LNPNM)
      ELSE
! HERE FOR LFORCE
         IF(.NOT.LFORCE) THEN
            DT= T+FSEC(INM)-T1
            IF(PER.EQ.ZERO) GOTO 3000
              WT= (TWOPI/PER)*DT
              WT=MOD(WT,TWOPI)
              SINWT=SIN(WT)
              COSWT=COS(WT)
 3000       CONTINUE
            IF(IJ.EQ.2) SCALE=-FACTS*OBS(INM)
            IF(IJ.EQ.3) SCALE=-ACCDOT(INM,IL)
            PART(INM,IPTP(1))=PART(INM,IPTP(1))+SCALE
            PART(INM,IPTP(2))=PART(INM,IPTP(2))+SCALE*DT
            PART(INM,IPTP(3))=PART(INM,IPTP(3))+SCALE*(DT**2.D0)
            PART(INM,IPTP(4))=PART(INM,IPTP(4))+SCALE*SINWT
            PART(INM,IPTP(5))=PART(INM,IPTP(5))+SCALE*COSWT
                  ! IF(.NOT.LFORCE)
          ELSE
            DO 3600 IK=1,3
!  IK HERE IS THE COMPONENT OF THE EXPLICIT PARTIAL ARRAY (XYZ)
            IF(IJ.EQ.1) SCALE=1.D0
            IF(IJ.EQ.2) SCALE=FRC(IK)
            IF(IJ.EQ.3) SCALE=XYZDOT(IK)
            DO 3500 J=1,4
!  J HERE IS THE TYPE OF THE BIAS (TOTAL, X Y OR Z )
!  HERE THE TYPE IS NOT ABS, SCALE, TIME
!  (THAT IS THE IJ 5000 LOOP)
            TT=1.D0
            IF(J.EQ.2.AND.IK.NE.1) TT=0.D0
            IF(J.EQ.3.AND.IK.NE.2) TT=0.D0
            IF(J.EQ.4.AND.IK.NE.3) TT=0.D0
            IF(J.GT.1.AND.IDYNDL(J-1).EQ.0) TT=0.D0
            PART(IPTPF(1,J),IK)=PART(IPTPF(1,J),IK)+TT*SCALE
            PART(IPTPF(2,J),IK)=PART(IPTPF(2,J),IK)+TT*SCALE*DT
            PART(IPTPF(3,J),IK)=PART(IPTPF(3,J),IK)+TT*SCALE*(DT**2.D0)
            PART(IPTPF(4,J),IK)=PART(IPTPF(4,J),IK)+TT*SCALE*SINWT
            PART(IPTPF(5,J),IK)=PART(IPTPF(5,J),IK)+TT*SCALE*COSWT
 3500       CONTINUE
 3600       CONTINUE
            DO 3800 IT=1,5
!   IT HERE IS THE TIME COMPONENT OF BIAS
!   (CONSTANT, LINEAR, QUAD, AB PER)
            DO 3700 J=1,4
! J HERE IS THE TYPE OF THE BIAS (TOTAL, X Y OR Z )
! HERE THE TYPE IS NOT ABS, SCALE, TIME
! (THAT IS THE IJ 5000 LOOP)
            F1(1)=ROTPAR(1,1)*PART(IPTPF(IT,J),1) +                     &
     &            ROTPAR(1,2)*PART(IPTPF(IT,J),2) +                     &
     &            ROTPAR(1,3)*PART(IPTPF(IT,J),3)
            F1(2)=ROTPAR(2,1)*PART(IPTPF(IT,J),1) +                     &
     &            ROTPAR(2,2)*PART(IPTPF(IT,J),2) +                     &
     &            ROTPAR(2,3)*PART(IPTPF(IT,J),3)
            F1(3)=ROTPAR(3,1)*PART(IPTPF(IT,J),1) +                     &
     &            ROTPAR(3,2)*PART(IPTPF(IT,J),2) +                     &
     &            ROTPAR(3,3)*PART(IPTPF(IT,J),3)
            PART(IPTPF(IT,J),1)=F1(1)
            PART(IPTPF(IT,J),2)=F1(2)
            PART(IPTPF(IT,J),3)=F1(3)
 3700      CONTINUE
 3800      CONTINUE
                  ! IF(.NOT.LFORCE)
         ENDIF
                ! IF(LNPNM)
      ENDIF
!
!
                ! IJ TYPE (ABS, SCALE, TIME)
 5000 END DO
!
! *** END ACCBIA PARTIALS **********************************************
!
!
! *** END ACCBIA COMPUTATION *******************************************
!
!
 6000 CONTINUE
!
! *** CALCULATION OF COMPUTED OBSERVATIONS AND DYNAMIC FORCES ***
!
!
       IF(.NOT.LFORCE) THEN
! ADD BIASES TO THE OBSERVATIONS
!         OBSC(INM)=OBSC(INM)*(1.D0+BIAS(2))+ACCDOT(INM,IL)*BIAS(3)+BIAS
!         OBSC(INM)=OBSC(INM)/BIAS(2)
!         OBSC(INM)=OBSC(INM)+ACCDOT(INM,IL)*BIAS(3)+BIAS(1)
!         RR=OBS(INM)-OBSC(INM)
!
!  IN THIS VERSION OBSC ACTUALLY CONTAINS THE RESIDUAL
!
          OBSC(INM)=-OBSC(INM)+FACTS*OBS(INM)*BIAS(2)+BIAS(1)           &
     &                        +ACCDOT(INM,IL)*BIAS(3)
!
!         DYNEQ=(OBS(INM)-BIAS(1)-BIAS(3)*ACCDOT(INM,IL))*BIAS(2)
          DYNEQ=FACTS*OBS(INM)*BIAS(2)+BIAS(1)+BIAS(3)*ACCDOT(INM,IL)
          IF(LSTINR) WRITE(91) BIASP,BIAS,DYNEQ
       ELSE
          DO N=1,3
! FIRST CORRECT FRC FOR TIMING AND ABSOLUTE BIASES
!            CORRF=FRC(N)-XYZDOT(N)*ACBIAS(N+1,3)-ACBIAS(N+1,1)
! THEN CORRECT FOR SCALE BIAS
!            FRC(N)=CORRF*ACBIAS(N+1,2)
!
             FRC(N)=FRC(N)*ACBIAS(N+1,2)+ACBIAS(N+1,1)                  &
     &             +XYZDOT(N)*ACBIAS(N+1,3)
!
          ENDDO
              ! IF(.NOT.LFORCE)
       ENDIF
!
! *** END CALCULATION OF COMPUTED OBSERVATIONS AND DYNAMIC FORCES ***
!
!
10000 END DO
      IF(LFORCE) RETURN

      ALLOCATE(LEDIT_EXTRA(NM))
      LEDIT_EXTRA(:) = .FALSE.
      CALL PROCES(AA,II,LL,AA(KOBSC ),AA(KRESID),AA(KSIGMA),AA(KRATIO), &
     &          AA(KPMPA ),AA(KOBS  ),AA(KSMCOR),AA(KOBTIM),AA(KOBSIG), &
     &          AA(KIAUNO),AA(KEDSIG),II(KINDH ),II(KNMH  ),AA(KS    ), &
     &          AA(KS0   ),AA(KS1   ),LL(KLEDIT),AA(KELEVS),AA(KDSCRP), &
     &          0         ,MTYPE     ,INDSTA    ,INDSAT    ,LELEVS    , &
     &          II(KISTNO),AA(KSTNAM),AA(KXPMPA),LL(KLSDAT), 1,1      , &
     &          LL(KLRDED),LL(KLFEDT),LEDIT_EXTRA)
       DEALLOCATE(LEDIT_EXTRA)

      RETURN
!     IF(MTYPE.EQ.203) STOP
      END
