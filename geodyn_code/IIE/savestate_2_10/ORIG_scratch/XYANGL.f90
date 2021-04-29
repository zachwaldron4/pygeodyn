!$XYANGL
! GEODYN 2E
      SUBROUTINE XYANGL(MJDSBL,FSECN,FSECM,OBS,OBSCOR,ENV,              &
     &   XTN,XSM,VSM,XMN,RNM,URNM,UHAT,DIRCOS,TROP,                     &
     &   COSE,ELCOR,EZ,RESID,OBRATE,PMPXI,PMPXE,                        &
     &   NMP,INDP,RPOLE,PXPOLE,NINTPL,MINTPL,                           &
     &   S,S1,XTP,PXSPXP,COSTHG,SINTHG,REZ,RELV,RDOT,WORK,ELEVSC,       &
     &   INDSAT,NM, nm1,                                                &
     &   LNRATE,LPOLAD,LIGHT,LXY,IY,INDSTA,                             &
     &   DELCTN,DELCTM,DELCTK,LPTS,LPSBL,LACC,MTYPE,INDSET,XDPOLE,      &
     &   LASER,LNUTAD,AA,II,LL)
!********1*********2*********3*********4*********5*********6*********7**
! XYANGL           83/05/03            8304.0    PGMR - TOM MARTIN
!
! FUNCTION:  COMPUTE X AND Y ANGLE COSINE MEASUREMENTS.
!            COMPUTATIONS START AT THE FINAL RECEIVED TIME
!            MJDSn+FSECn AND PROCEED BACKWARDS UNTIL THE MODEL
!            IS COMPLETE. IF THE "LIGHT" SWITCH IS .FALSE.
!            ITERATIVE SOLUTIONS FOR THE LIGHT TIME ARE
!            PERFORMED. OTHERWISE, THE MEASUREMENT EVENT TIMES
!            ARE CONSIDERED TO BE KNOWN.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSBL   I         MODIFIED JULIAN DAY SECONDS OF THE FINAL
!                      RECEIVED TIME OF THE FIRST OBSERVATION IN
!                      THE BLOCK
!   FSECN    I         ELAPSED SECONDS FROM MJDSBL OF THE EVENT
!                      TIMES ASSOCIATED WITH TRACKING STATION Tn
!   FSECM   I/O        ELAPSED SECONDS FROM MJDSBL OF THE EVENT
!                      TIMES ASSOCIATED WITH SATELLITE Sm
!   OBS      I         THE OBSERVED X OR Y ANGLE
!   OBSCOR   O         SUM OF OBSERVATION CORRECTIONS
!   ENV      I         STATION EAST,NORTH,VERTICAL VECTORS IN E.C.F.
!   XTN           A    TRUE OF DATE INERT. COORD. OF TRACKING STA. Tn
!   XSM     I/O        TRUE OF DATE INERT. COORD. OF SATELLITE Sm
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   VSM
!   XMN      I         MEAN POLE COORDINATES OF TRACKING STATION Tn
!   RNM     I/O        SLANT RANGE FROM Tn TO Sm
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   URNM    I/O        UNIT VECTOR FROM Tn TO Sm
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   UHAT    I/O        E.C.F. UNIT VECTOR FROM STATION TO S/C
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   DIRCOS  I/O        S/C EAST,NORTH,VERTICAL DIRECTION COSINES
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   TROP
!   COSE    I/O        COSINES OF ELEVATION
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   ELCOR   I/O        ELEVATION REFRACTION CORRECTIONS
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   EZ      I/O        Y ANGLE DIRECTION SINES
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   RESID    O         OBSERVED MINUS COMPUTED OBSERVATION
!   OBRATE   O         TIME DERIVATIVE OF OBSERVATION
!   PMPXI    O         PARTIAL OF OBS. W.R.T. INERTIAL S/C POS.
!   PMPXE    O         PARTIAL OF OBS. W.R.T. E.C.F. STATION POS.
!   NMP      O         INDEX OF THE LAST MEASUREMENT ASSOCIATED WITH
!                      EACH INTERPOLATION INTERVAL
!   INDP     O         POINTER TO ADJUSTED POLAR MOTION VALUES
!                      ASSOCIATED WITH EACH INTERPOLATION INTERVAL
!   RPOLE         A    POLAR MOTION ROTATION MATRICES AT EACH END
!                      OF EACH INTERPOLATION INTERVAL
!   PXPOLE        A    PARTIALS OF THE TRUE POLE STATION LOCATION
!                      W.R.T. THE POLAR MOTION X & Y PARAMETERS AT
!                      EACH END OF EACH INTERPOLATION INTERVAL
!   XDPOLE        A    PARTIALS OF THE TRUE POLE STATION RATE
!                      W.R.T. THE POLAR MOTION X & Y PARAMETERS AT
!                      EACH END OF EACH INTERPOLATION INTERVAL
!   NINTPL
!   MINTPL             MAXIMUM NUMBER OF INTERPOLATION INTERVALS
!   S                  INTERPOLATION FRACTIONS
!   S1                 1.0-S
!   XTP                TRUE POLE COORDINATES OF A TRACKING STATION
!   PXSPXP   O         PARTIALS OF TRUE POLE STATION COORDINATES
!                      W.R.T. POLAR MOTION FOR EACH OF "NM"
!                      MEASUREMENT TIMES
!   COSTHG   O         COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   O         SINES  OF RIGHT ASCENSION OF GREENWICH
!   REZ                WORKING ARRAY OF LENGTH "NM"
!   RELV    I/O        E.C.F. VEL OF S/C W.R.T. STATION
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   RDOT    I/O        RANGE RATE FROM STATION TO SPACECRAFT
!                      OUTPUT WHEN LXY=.F. AND INPUT WHEN LXY=.T.
!   WORK               WORKING ARRAY OF LENGTH "NM"
!   ELEVSC
!   INDSAT
!   NM       I         NUMBER OF MEASUREMENTS
!   LNRATE   I         .T.-OBS. TIME DERIVATIVES NOT REQUIRED
!                      .F.-COMPUTE OBSERVATION TIME DERIVATIVES
!   LPOLAD   I         SWITCH INDICATING IF POLAR MOTION ADJUST.
!                      PARTIALS ARE REQUIRED FOR EACH INTERP.
!                      INTERVAL
!   LIGHT    I         .TRUE. IF LIGHT TIMES HAVE BEEN DETERMINED
!   LXY      I         MEASUREMENT SWITCH; F=X ANGLE, T=Y ANGLE
!   IY       I         INDICATES ALIGNMENT OF X ROTATION AXIS
!                        2=X ROT. AXIS ALIGNED POSITIVE NORTH(N-S MOUNT)
!                          X ANGLE MEASURED POSITIVE EAST
!                          Y ANGLE MEASURED POSITIVE NORTH
!                        1=X ROT. AXIS ALIGNED POSITIVE EAST (E-W MOUNT)
!                          X ANGLE MEASURED POSITIVE SOUTH
!                          Y ANGLE MEASURED POSITIVE EAST
!   INDSTA   I         INTERNAL STATION POINTER
!                     ....elevation correction flags
!                     ....LPRE(3,1)   if = T,  no dry tropo  correction
!                     ....LPRE(5,1)   if = T,  no wet tropo  correction
!                     ....LPRE(7,1)   if = T,  no ionosphere correction
!
!   DELCTN   I         DELTA ET CET AT EVENT TIMES ASSOCIATED
!                      WITH TRACKING Tn
!   DELCTM   I         DELTA ET CET AT EVENT TIMES ASSOCIATED
!                      WITH SATELLITE Sm
!   DELCTK   I         DELTA ET CET AT EVENT TIMES ASSOCIATED
!                      WITH TRACKING TK
!   LPTS
!   LPSBL
!   LACC
!   AA     I/O    A
!   II     I/O    A
!   LL     I/O    A
!   MTYPE              MEASUREMENT TYPE
!   INDSET   I         FOR A GIVEN INTERNAL SAT. NUMBER THIS TELLS
!                      WHICH SET THE SAT. BELONGS TO.
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
!      COMMON/CBLOKL/
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COPARM/LOP1
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
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/SIMLM/OBSINT,VINT,ELCUTO,XSMLM

!jtw add CORA03 and CMETI and CBLOKL
!
      DIMENSION FSECN(NM1),FSECM(NM1),                                  &
     &   XTN(MINTIM,3),XSM(MINTIM,3),VSM(MINTIM,3),                     &
     &   OBS(NM1),OBSCOR(NM1),ENV(3,3),                                 &
     &   XMN(3),RNM(NM1),EZ(NM1),ELEVSC(NM1),TROP(NM1,2),               &
     &   PMPXE(NM1,3),UHAT(NM1,3),DIRCOS(NM1,3),COSE(NM1),ELCOR(NM1),   &
     &   URNM(NM1,3,1),NMP(MINTPL,3),                                   &
     &   RESID(NM1),PMPXI(NM1,3),OBRATE(NM1),NINTPL(3),                 &
     &   INDP(MINTPL,3),RPOLE(3,3,MINTPL),PXPOLE(3,2,MINTPL),           &
     &   S(NM1),S1(NM1),XTP(NM1,3),PXSPXP(NM1,3,2,3),                   &
     &   COSTHG(NM1,3),SINTHG(NM1,3),REZ(NM1),                          &
     &   WORK(NM1),RELV(NM1,3),RDOT(NM1),                               &
     &   INDSAT(3),INDSTA(3),DELCTN(NM1),DELCTM(NM1),DELCTK(NM1),       &
     &   INDSET(3),XDPOLE(3,2,MINTPL),AA(*),II(*),LL(*)
      DIMENSION LPTS(NM1)
                              !! debug
      dimension xobscp(100)
!
      DATA ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! COMPUTE X ANGLE INDEX AND SENSE OF X DIRECTION
      IX=MOD(IY,2)+1
      SIGNX=DBLE(2*IY-3)
!
!-------------------------------------------------------------------
! IF Y ANGLES BEING PROCESSED, BYPASS CERTAIN OPERATIONS,
!    EITHER NOT REQUIRED OR PREVIOUSLY DONE FOR X ANGLES
      LOP1=.NOT.LXY
      IF(LXY) GO TO 10000
!-------------------------------------------------------------------
!
!     COMPUTE VECTORS AND RANGES BETWEEN STATION AND SPACECRAFT
      lyara = .false.
      isnumb = 0
      LADD=.TRUE.
      CALL MTSTST(MJDSBL,FSECN,FSECM,REZ,REZ,REZ,XTN,XSM,REZ,           &
     &   REZ,REZ,REZ,VSM,REZ,REZ,REZ,XMN,REZ,REZ,                       &
     &   AA(KXTNPC),AA(KXSMPC),AA(KXTKPC),AA(KXSJPC),AA(KXTIPC),        &
     &   RNM,REZ,REZ,REZ,REZ,REZ,REZ,REZ,URNM,REZ,                      &
     &   REZ,REZ,REZ,REZ,REZ,REZ,NMP,INDSTA,INDP,RPOLE,PXPOLE,          &
     &   NINTPL,MINTPL,S,S1,XTP,REZ,PXSPXP,AA(KPXSLV),COSTHG,SINTHG,    &
     &   INDSAT,SIGNL,NM,LPOLAD,LIGHT,.TRUE.,.FALSE.,1,.FALSE.,         &
     &   AA(KGRLT1),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),DELCTN,            &
     &   DELCTK,DELCTK,AA(KPSTAT),AA(KRSUNS),AA(KCHEMR),                &
     &   AA(KCHMOR),AA(KEPOSR),AA(KCHEMT),AA(KCHMOT),AA(KEPOST),        &
     &   AA(KCHCBS),AA(KCPOSS),AA(KSCRTM),AA(KXSTT) ,AA(KXSTR) ,        &
     &   AA(KXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,       &
     &   lyara, isnumb, LADD,XDPOLE,AA(KPXDXP),AA(KXUTDT),.FALSE.,      &
     &   FSECN,LNUTAD)

!
! ROTATE UNIT POS VECTOR FROM INERTIAL TO E.C.F.
      CALL ECFIXP(URNM,COSTHG,SINTHG,UHAT,NM,NM,NM)

!
! COMPUTE DIRECTION COSINES
      CALL MATPRD(UHAT,ENV,DIRCOS,NM,3,3)
!
! COMPUTE SIGNED X DIRECTION COSINE
      DO 200 N=1,NM
         REZ(N)=SIGNX*DIRCOS(N,IX)
  200 END DO
!
!      write(6,98882) REZ(1)
98882 FORMAT(' REZ AT 200   = signx * dircos(,ix) '/(1x,4d24.16))
!-------------------------------------------------------------------
!
! COMPUTE THE ELEVATION ANGLE IN DEGREES
!     ....WORK = SINE SQUARED OF ELEVATION
!     ....=  n**2
!     ....COSE = COSINE OF ELEVATION
!     ....=  1-n**2
!
      DO 400 N=1,NM
         WORK(N)=DIRCOS(N,3)*DIRCOS(N,3)
         COSE(N)= SQRT( ONE-WORK(N) )
  400 END DO
!
!      write(6,98883) WORK
98883 FORMAT(' WORK AT 400  = n**2 '/(1x,4d24.16))
!
!
      IF(.NOT.LACC.AND.LPSBL)LNELEV=.FALSE.
      CALL ELEV(WORK,WORK,WORK,WORK,WORK,WORK,WORK,DIRCOS(1,3),COSE,    &
     &   WORK,WORK,ELEVSC,WORK,NM,.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE., &
     &   .TRUE.,.TRUE.)
!
!----------------------------------------------------------------------
!
      IF(.NOT.LACC)CALL SDELEV(LPTS,ELEVSC,NM,ELCUTO,.TRUE.)
      IF(.NOT.LITER1) GO TO 2000
!
!      ....if no dry tropo (lpre(3,1) and no iono (lpre(7,1) go to 2000
      IF(LPRE(3,1).AND.LPRE(7,1)) GO TO 2000
!
! COMPUTE REFRACTION CORRECTIONS
!...SINE SQUARED OF ELEVATION + COSINE SQUARED OF X DIR.
      DO 1000 N=1,NM
!        ....SINE SQUARED OF ELEVATION + COSINE SQUARED OF X DIR.
!        ....NS: n**2 + l**2     EW: n**2 + m**2
         WORK(N)=WORK(N)+DIRCOS(N,IX)*DIRCOS(N,IX)
!
!        ....TIMES COSINE OF ELEVATION
!        ....NS: (-l/(n**2 + l**2))*cos e   EW: (m/(n**2 + m**2))*cos e
!        .... = dx/de
         WORK(N)=-( REZ(N)/WORK(N) ) * COSE(N)
 1000 END DO
!
!     ....work is now PARTIAL OF X-ANGLE W.R.T. ELEVATION
!-------------------------------------------------------------------
!
!
!...OBTAIN ELEVATION REFRACTION CORRECTIONS
      BLKMET=BLKDAT(1,1)
      JMET  =KOBCOR(1,1)
!
!     ....troposphere correction returned in RDOT
!     ....ionosphere  correction returned in EZ
!
!     ....LPRE(3,1)    if = T,  no dry tropo  correction
!     ....LPRE(5,1)    if = T,  no wet tropo  correction
!     ....LPRE(7,1)    if = T,  no ionosphere correction
!
!jtw replace call to refrac with call to seperate models
!      CALL REFRAC(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,     &
!     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
!     & .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL)
!
      IWPR=IRFPRT(MODEL(MTYPE))

      IF(MODEL(MTYPE).EQ.0) THEN

      CALL HOPFLD(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,     &
     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
     &   .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL,FSECN,IWPR,NSTA0)


      ELSE IF(MODEL(MTYPE).EQ.1) THEN

           IF(LASER) THEN

                CALL MARMUR(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),   &
               &    EZ,NM,3,LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,       &
               &    AA(JMET),LPRE(1,1),.FALSE.,EZ,EZ,EZ,EZ,MTYPE,       &
               &    MJDSBL,FSECN,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL HOPFLD(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),   &
               &    EZ,NM,3,LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,       &
               &    AA(JMET),LPRE(1,1),.FALSE.,EZ,EZ,EZ,EZ,MTYPE,       &
               &    MJDSBL,FSECN,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.2) THEN

      CALL VLBGPS(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,     &
     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
     &   .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL,FSECN,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.3) THEN

      CALL GPSNIL(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,     &
     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
     &   .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL,FSECN,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.4) THEN

           IF(LASER) THEN

                CALL MARMUR(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),   &
               &    EZ,NM,3,LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,       &
               &    AA(JMET),LPRE(1,1),.FALSE.,EZ,EZ,EZ,EZ,MTYPE,       &
               &    MJDSBL,FSECN,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL GPSNIL(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),   &
               &    EZ,NM,3,LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,       &
               &    AA(JMET),LPRE(1,1),.FALSE.,EZ,EZ,EZ,EZ,MTYPE,       &
               &    MJDSBL,FSECN,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.5) THEN

           IF(LASER) THEN

                CALL MENDEZ(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),   &
               &    EZ,NM,3,LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,       &
               &    AA(JMET),LPRE(1,1),.FALSE.,EZ,EZ,EZ,EZ,MTYPE,       &
               &    MJDSBL,FSECN,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL HOPFLD(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),   &
               &    EZ,NM,3,LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,       &
               &    AA(JMET),LPRE(1,1),.FALSE.,EZ,EZ,EZ,EZ,MTYPE,       &
               &    MJDSBL,FSECN,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.6) THEN

      CALL GMFHOP(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,     &
     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
     &   .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL,FSECN,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.7) THEN

      CALL GMFSAS(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,     &
     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
     &   .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL,FSECN,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.8) THEN

      CALL VMF1(COSE,DIRCOS(1,3),RDOT,RNM,TROP,TROP(1,2),EZ,NM,3,       &
     &   LPRE(3,1),LPRE(5,1),LPRE(7,1),BLKMET,AA(JMET),LPRE(1,1),       &
     &   .FALSE.,EZ,EZ,EZ,EZ,MTYPE,MJDSBL,FSECN,IWPR,NSTA0,AA(KA1UT))

      END IF


!...SUM WET & DRY TROPOSPHERIC AND IONOSPHERIC
!
      DO 1500 N=1,NM
         ELCOR(N)=TROP(N,1)+TROP(N,2)+EZ(N)
 1500 END DO
!
!-------------------------------------------------------------------
!
!...CHAIN DX/DE * DELTA E AND SUM INTO OBSERVATION CORRECTIONS SUMS
!     ....dx/de = work   delta e = elcor
      DO 1600 N=1,NM
         OBSCOR(N)=OBSCOR(N)+WORK(N)*ELCOR(N)
 1600 END DO
!
      CALL XRFRAC(AA,TROP,EZ,WORK,KOBCOR,LPRE,LSWTCH(1,2),NM)
 2000 CONTINUE
!
!-------------------------------------------------------------------
!
! COMPUTE X ANGLES AND RESIDUALS
!     ....NS: l/n    EW: -m/n
      DO 2800 N=1,NM
         rezxx = rez(n)
                                                    ! delta elevation
         deltae = 0.0005
                                                    ! sin(elev+deltae)
         dircxx = SIN( ASIN(dircos(n,3)) + deltae )
         REZ(N)=REZ(N)/DIRCOS(N,3)
         OBSCP=ATAN(REZ(N))
         OBSCPx=ATAN(rezxx/dircxx)
         dxde = (obscpx - obscp) / deltae
         RESID(N)=OBS(N)-ATAN(REZ(N))

 2800 END DO
!
!
! COMPUTE Y ANGLE SINES SQUARED
      DO 4800 N=1,NM
!        ....COMPUTE Y ANGLE SINES SQUARED
!        .....NS: 1-m**2   EW: 1-l**2
         IF( .NOT. LNRATE ) THEN
!! jjm fix 11/8/95
           OBRATE(N)=ONE-DIRCOS(N,IY)*DIRCOS(N,IY)
         ENDIF
!! jjm fix 11/8/95
!
!        ....COMPUTE Y ANGLE SINES SQUARED
!        ....NS: 1-m**2   EW: 1-l**2
         REZ(N) = ONE-DIRCOS(N,IY)*DIRCOS(N,IY)
!! good jjm fix 11/8/95
!
!        ....COMPUTE Y ANGLE SINES
!        ....ez  NS: sqrt(1-m**2)   EW: sqrt(1-l**2)
         EZ(N)=SQRT( REZ(N) )
!! good   jjm fix 11/8/95
!
!        ....NS:  rho * (1-m**2)   EW: -rho * (1-l**2)
         REZ(N)=SIGNX * REZ(N) * rnm(N)
 4800 END DO
!
!-------------------------------------------------------------------
!
! COMPUTE E.C.F. STATIONS PARTIALS FOR X ANGLES
      DO 8000 I=1,3
!
         DO 7200 N=1,NM
!           ....NS: Zhat * l   EW: Zhat * m
            WORK(N)=ENV(I,3)*DIRCOS(N,IX)

!
!           ....NS: Zhat*l - Ehat*n   EW: Zhat*m - Nhat*n
            WORK(N)=WORK(N)-ENV(I,IX)*DIRCOS(N,3)

!
!           ....NS: (Zhat*l - Ehat*n) / (rho * (1-m**2))
!           ....EW: (Nhat*n - Zhat*m) / (rho * (1-l**2))
            PMPXE(N,I)=WORK(N)/REZ(N)

 7200    CONTINUE
 8000 END DO
!
! ROTATE PARTIALS TO INERTIAL
!
      CALL INERTP(PMPXE,COSTHG,SINTHG,PMPXI,NM,NM,NM)
!
! SATELLITE PARTIALS ARE NEGATIVE OF STATION PARTIALS
      DO 9000 I=1,3
         DO 8800 N=1,NM
            PMPXI(N,I)=-PMPXI(N,I)
 8800    CONTINUE
 9000 END DO
!
!----------------------------------------------------------------------
!
      IF(LNRATE) GO TO 9500
      DO 9100 N=1,NM
         WORK(N)=RNM(N)*OBRATE(N)
         REZ(N)=WORK(N)*SIGNX
 9100 END DO
!
      CALL RELVEL(VSM,COSTHG,SINTHG,UHAT,RNM,OBRATE,RELV,RDOT,NM,       &
     &   LNRATE,LNRATE)
      CALL MATPRD(RELV,ENV(1,IX),OBRATE,NM,3,1)
      CALL MATPRD(RELV,ENV(1,3),WORK,NM,3,1)
      DO 9200 N=1,NM
         OBRATE(N)=DIRCOS(N,3)*OBRATE(N)
         OBRATE(N)=OBRATE(N)-DIRCOS(N,IX)*WORK(N)
         OBRATE(N)=OBRATE(N)/REZ(N)
 9200 END DO
 9500 CONTINUE
!
!
      RETURN
!
!-----------------------------------------------------------------
!
! PROCESS Y ANGLE OBSERVATIONS
10000 CONTINUE
      IF(.NOT.LITER1) GO TO 11300
!
!
      IF(LPRE(3,1).AND.LPRE(7,1)) GO TO 11300
!
! COMPUTE REFRACTION CORRECTIONS FOR Y ANGLES
!...ONE MINUS +Y DIRECTION COSINES SQUARED
      DO 10200 N=1,NM
!
!
!
!        ....NS: -(n / m) * cos(e) / sqrt( 1 - m**2 )
!        ....EW: -(n / l) * cos(e) / sqrt( 1 - l**2 )
!
         WORK(N) = -(DIRCOS(N,3)/DIRCOS(N,IY))                          &
     &             * COSE(N) /SQRT(1.0-DIRCOS(N,IY)**2)
!
10200 END DO
!
!...CHAIN DY/DE * DELTA E AND SUM INTO OBSERVATION CORRECTIONS SUMS
      DO 11200 N=1,NM
         OBSCOR(N)=OBSCOR(N)+WORK(N)*ELCOR(N)
11200 END DO
!
      CALL YRFRAC(AA,WORK,KOBCOR,LPRE,LSWTCH(1,2),NM)
11300 CONTINUE
!
!-------------------------------------------------------------------
!
! COMPUTE Y ANGLES
!
      DO 11400 N=1,NM
      REZ(N)=DIRCOS(N,IY)/EZ(N)
!        ....NS:  arctan( m/sqrt(1-m**2) ) = arcsin( m )
!        ....EW:  arctan( l/sqrt(1-l**2) ) = arcsin( l )
!
         WORK(N)=ATAN( DIRCOS(N,IY)/EZ(N) )
!
!        ....COMPUTE RESIDUALS
         RESID(N)=OBS(N)-WORK(N)
!
!        ....COMPUTE RANGES TIMES Y ANGLE SINES
!        ....NS: rho * sqrt(1-m**2)   EW: rho * sqrt(1-l**2)
         REZ(N)=EZ(N)*RNM(N)
11400 END DO
!
! COMPUTE PARTIALS OF Y ANGLES W.R.T. E.C.F. STATION POSITIONS
      DO 13000 I=1,3
         DO 12400 N=1,NM
!           ....NS: m * Uhat - Nhat     EW: l * Uhat - Ehat
            WORK(N)=DIRCOS(N,IY)*UHAT(N,I)-ENV(I,IY)
!
!           ....NS: (m * Uhat - Nhat) / (rho * sqrt(1-m**2))
!           ....EW: (l * Uhat - Ehat) / (rho * sqrt(1-l**2))
            PMPXE(N,I)=WORK(N)/REZ(N)
12400    CONTINUE
13000 END DO
!-------------------------------------------------------------------
!
! ROTATE PARTIALS TO INERTIAL
      CALL INERTP(PMPXE,COSTHG,SINTHG,PMPXI,NM,NM,NM)
!
! SATELLITE PARTIALS ARE NEGATIVE OF STATION PARTIALS
      DO 14000 I=1,3
         DO 13800 N=1,NM
            PMPXI(N,I)=-PMPXI(N,I)
13800    CONTINUE
14000 END DO
!
!--------------------------------------------------------------------
!
      IF(LNRATE) GO TO 17000
      CALL MATPRD(RELV,ENV(1,IY),OBRATE,NM,3,1)
      DO 15000 N=1,NM
         OBRATE(N)=OBRATE(N)-RDOT(N)*DIRCOS(N,IY)
         OBRATE(N)=OBRATE(N)/REZ(N)
15000 END DO
17000 CONTINUE
!
!---------------------------------------------------------------------
!
      RETURN
      END
