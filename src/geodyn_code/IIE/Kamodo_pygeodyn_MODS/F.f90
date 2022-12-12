!$F   f0
      SUBROUTINE F(MJDSEC,FSEC,XP,XDDOT,PXDDEX,NSAT,NEQN,NCCALL,        &
     &   XDDTMC,TOPXAT,LTPXAT,AA,II,LL,ISATID,                          &
     &   LFHIRT, LSURFF,                                                &
     &   XDDTHR,                                                        &
     &   XDDTCA,                                                        &
     &   PXDDEXA,                                                       &
     &   NEQN_ACC,                                                      &
     &   IPTFBS,                                                        &
     &   NPRMAC,                                                        &
     &   LHIRATE,                                                       &
     &   LALL,SFRAC,                                                    &
     &   DYNTIM,                                                        &
     &   NPERDY,                                                        &
     &   LACCELS,                                                       &
     &   EXACCT,                                                        &
     &   EXACIN,                                                        &
     &   EXACOB,                                                        &
     &   ACCTIM,                                                        &
     &   ACCPER,                                                        &
     &   NPERAC,                                                        &
     &   IACCP,PACCL,NRAT,XDDRC,                                        &
     &   XDDGP,XSA,LSBURN,IPSBRN,LFBURN,XDDBRN,SSTEP)
                 ! high rate accels
                 ! computed accelerometer accels.
                  ! computed accelerometer partials
                   ! computed accelerometer accels.
                 ! pointer array from big->small force model partials
                             !%ACC !  dynamic accel. periods / sat
                             !%ACC !  number of dynamic accel. periods /
                             ! TRUE if acc data present
                             ! external acceleration file times
                             ! external acceleration intervals
                             ! external acceleration observations
                             ! times for accelerometer biases
                             ! period for periodic terms of acc biases
                             ! number of acc periods for each satellite
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      INTEGER*4 DELTAL
      CHARACTER side(2)
      DATA SIDE/'R','L'/
      SAVE
!
!********1*********2*********3*********4*********5*********6*********7**
! F                MM/DD/YY            VVVV.V    PGMR - ?
!
! FUNCTION         CALCULATE ACCELERATIONS ON SATELLITE AND
!                  VARIATIONAL PARTIALS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         MEASUREMENT BLOCK START TIME IN MJD SECONDS
!   FSEC     I         FRACTIONAL SECONDS FOR EACH POINT IN BLOCK
!   XP       I         SATELLITE STATE VECTOR IN TRUE OF REFERENCE
!   XDDOT    O         TRUE OF REFERENCE ACCELERATIONS
!   PXDDEX   O         VARIATIONAL PARTIALS
!   NSAT     I         NUMBER OF SATELLITES IN THE CURRENT SET
!   NEQN     I         NUMBER OF FORCE MODEL PARAMETERS
!   NCCALL   I
!   XDDTMC   O         TRUE OF REF. ACCELERATION MINUS CENTRAL BODY ACC.
!   TOPXAT   O    A    TOPEX ATT. BACK VALUES (BETAPRIME & ORBIT ANGLE)
!   LTPXAT   O    A    TOPEX ATT. BACK VALUES (RAMPING LOGICALS)
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES
!   II      I/O   A    DYNAMIC ARRAY FOR INTEGER VARIABLES
!   LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL VARIABLES
!   ISATID   I    A    SAT ID ARRAY
!   LFHIRT   L    A    switches indicating forces to use for high rate
!                      integration
!   LSURFF   L    A    switches indicating surface forces to use when
!                      in a dynamic acceleration period
!   LHIRATE  L    S    indicates this call is for high rate integration
!   NEQN_ACC L    S    number of parameters in the acceleration partial
!   LALL     L    S    indicates that this call loads XDDOT and XDDTMC
!                      with all forces (if .TRUE.) and with all forces
!                      minus the XDDTHR (high rate accels) (if .FALSE.)
!   SFRAC    I    S    INTERPOLATION FRACTION FOR COORDINATE SYSTEM
!                      CALLS
!   DYNTIM   I    A    USER REQUESTED START AND STOP TIMES FOR
!                      DYNAMIC ACCELERATION ACCELEROMETER PERIODS
!   NPERDY   I    A    NUMBER OF USER REQUESTED
!                      DYNAMIC ACCELERATION ACCELEROMETER PERIODS
!
!   XDDGP    I    A    J2000 HELIOCENTRIC GRAVITATIONAL ACCELS OF
!                      PRIMARY ASTEROID DUE TO SECONDARY ASTEROID
!   XSA      I    A    J2000 PRIMARY ASTEROID CENTERD POSITION OF
!                      SECONDAY ASTEROID
!
! COMMENTS
!-----------------------------------------------------------------------------
!   POSITIONS FOR PLANETS
!     COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
!
!           *CBDSTA*  PLANETARY EPHEMERIS INFORMATION
!
!            BDSTAT - PLANETARY POSITIONS, VELOCITIES, GM'S
!                     PLANET STATE VECTORS ARE FROM
!                     THE JPL PLANETARY EPHEMERIS TAPE.
!
!                     BDSTAT(1-3,J) = POSITIONS FOR PLANET J
!                     BDSTAT(4-6,J) = VELOCITIES FOR P
!
!             J=1,7
!     VENUS          BDSTAT(J,1)
!     MARS           BDSTAT(J,2)
!     JUPITER        BDSTAT(J,3)
!     SATERN         BDSTAT(J,4)
!     URANUS         BDSTAT(J,5)
!     NEPTUNE        BDSTAT(J,6)
!     PLUTO          BDSTAT(J,7)
!     SUN            BDSTAT(J,8)
!     EARTH          BDSTAT(J,9)
!     MERCURY        BDSTAT(J,10)
!     EARTH's MOON   BDSTAT(J,11)
!     SCBODY STARTS AT BDSTAT(J,12) IF(NOT.LASTSN) (NO INTGCB)
!     SCBODY STARTS AT BDSTAT(J,13) IF(LASTSN)     (INTGCB)
!
!-----------------------------------------------------------------------------
!     S/C positions
!     DO I=1,NSAT --> total number of satellites
!     XP(1,I,1),XP(2,I,1),XP(3,I,1)
!     ENDDO

!     S/C velocities
!     DO I=1,NSAT --> total number of satellites
!     XP(1,I,1),XP(2,I,2),XP(3,I,2)
!     ENDDO
!
!     To get S/C information at a different time other than the current time
!     is not easy because GEODYN may call ORBIT at any time BUT, during this
!     call the integrator will go safely back and forth for the time:
!     Integration Order times Stepsize, which is usually a few minutes.
!     1. For past times if you know them ahead you may save them
!     2. For future times you may want to run GEODYN once and save the
!        information but this becomes complicated.
!     3. If you want to get the state at a reachable time, you just need to
!        change the first two arguments in calling ORBIT. MJDSEC and FSEC.
!
!-----------------------------------------------------------------------------
!
!          ***********   ROTATION MATRICES  **************
!
!
!
!     COMMON/CEFMAT/EFMAT(9)
!
!           *CEFMAT*  ROTATION MATRIX FROM TRUE OF DATE TO BODY FIXED
!
!     COMMON/CREFMT/REFMT(9)
!
!           *CREFMT* ROTATION MATRIX FROM MEAN OF 2000 TO TRUE OF REFEREENCE
!
!
!     COMMON/CRMB/RMB(9), rmb0(9)
!           *CRMB/
!
!            RMB    -  ROTATION MATRIX FROM TRUE OF REF. TO SATELLITE
!                      BODY FIXED XYZ including GEOPOL rotation
!            rmb0   -  ROTATION MATRIX FROM TRUE OF REF. TO SATELLITE
!                      BODY FIXED XYZ without geopol rotation
!     COMMON/CRMB2/RMB2(9), rmb02(9)
!
!           *CRMB2/R*  THE SAME AS CRMB FOR A SECOND ASTEROID IN A
!                      BINARY ASTEROID RUN
!
!            RMB2   -  ROTATION MATRIX FROM TRUE OF REF. TO SATELLITE
!                      BODY FIXED XYZ including GEOPOL rotation
!            rmb02  -  ROTATION MATRIX FROM TRUE OF REF. TO SATELLITE
!                      BODY FIXED XYZ without geopol rotation
!
!     COMMON/CRMBI/RMBI(9)
!
!           *CRMBI*
!
!     RMBI MATRIX TO GO FROM RFL = (RADIUS, LAT(PHI), LON(LAMBDA))
!                 TO TRUE OF REF
!
!
!
!     SUBROUTINE J2000_2_TOD(MJDSEC,FSEC,AA,NM,NDIM,II,XA,VA)
!***********************************************************************
! FUNCTION: TRANSFORM MEAN OF J2000 COORDINATE SYSTEM TO
!           TRUE OF DATE SYSTEM. THIS IS NECESSARY WHEN
!           THE INTEGRATION IS DONE IN THE MEAN OF J200 SYSTEM
!           AND ONE WANTS OUTPUT IN THE TRUE OF DATE SYSTEM
!***********************************************************************
!
!     COMMON/CBARYC/CBODY(6),TBODY(6),SUNRF(6)
!
!           *CBARYC*
!
!            CBODY - TRUE OF REFERENCE CENTRAL BODY POS AND VEL
!            TBODY - TRUE OF REFERENCE TRACKING BODY POS AND VEL
!            SUNRF - TRUE OF REFERENCE SUN BODY POS AND VEL
! The variables above are not matrices, they are vectors, but once computed
! they are approximately  valid for the data block  in proces.

!-----------------------------------------------------------------------------
!
!   ******************** CONNECT NEW FORCE MODELS *************************
!
!New GLOBAL option PLNGLF to include global planetary force models on demand
!New GLOBAL OPTION PLNSFR to include global planetary surface models on demand

!     COMMON/IPLMOD/NFMOD,NSMOD,NFTOT,NSTOT,IDFMOD(1000),IDSMOD(1000), &
!    &             NFPARAM(1000),&
!    &             NPMIND(1000),NSPARAM(1000),NPSIND(1000),NXPLMD
!
!           *IPLMOD*
!            NFMOD -  NUMBER OF FORCE MODELS
!            NSMOD -  NUMBER OF SURFACE MODELS
!            IDFMOD-  FORCE MODEL INDICATOR (101 etc)
!            IDSMOD-  SURFACEMODEL INDICATOR (101 etc)
!            NFPARAM- NUMGER OF PARAMETERS PER FORCE MODEL
!            NSPARAM- NUMGER OF PARAMETERS PER SURFACE  MODEL
!            NPMIND - STARTING POINT FOR THE FORCE MODEL'S PARAMETERS
!            NPSIND - STARTING POINT FOR THE SURFACE MODEL'S PARAMETERS
!            NXPLMD - WORDS IN THIS CB


!      COMMON NPCOMX INDICES
!      IXPLNF,IXPSRF
!      Paameter Values sarting at :         AA(KPRMV+IPVAL(IXPLNF)-2)
!                                           AA(KPRMV+IPVAL(IXPSRF)-2)
!      Adjusted param. values starting at:  AA(KPRMV0+IPVAL0(IXPLNF)-2)
!      Sigma  values starting at:           AA(KPRMSG+IPVAL0(IXPLNF)-2)

!      We will assume that all the parameters are adjustable and it will
!      be up to the user to tighten the sigma

!      EMAT LABELS:

!      11aaaaabmmmnnnn

!      aaaa=0000
!      b=5  Force model
!      b=6  Surface model

!      mmm=model id (ex 101)
!      nnnn= parameter number from the input card (1,2,3 etc)

!      SAMPLE SETUP with 3 models
!PLNGLF   1    101                        1.0         1.D-30
!PLNGLF   2    101                        2.0         2.D-30
!PLNGLF   1    102                        3.0         3.D-30
!PLNGLF   2    102                        4.0         4.D-30
!PLNGLF   3    102                        5.0         5.D-30

!      To find the pointer of a particular parameter in the arrays PXDDOT
!      (PXDDEX in F) which host the explicit partials you need: to start at:
!      DO I=1,NPVAL0(IXPLNF)
!      PXDDOT(JLPLFM-1+I,1)
!      PXDDOT(JLPLFM-1+I,2)
!      PXDDOT(JLPLFM-1+I,3)
!      ENDDO
!     JLPLFM is set in the CB SETAPT for this parameters

!********1*********2*********3*********4*********5*********6*********7**
!
      PARAMETER (  ZERO = 0.D0 )
      PARAMETER (  ONE  = 1.D0 )
      PARAMETER (  TWO  = 2.D0 )
      PARAMETER (  CM999= -999.0D0)
      PARAMETER (  NPMSA=39)
!
      dimension tempv(3,6)
      DIMENSION INTBD(11)
      DIMENSION XP(3,NSAT,2),XDDOT(3,NSAT)
      DIMENSION XDDRC(NSAT)

      DIMENSION PXDDEX(NEQN,3,NSAT)
      DIMENSION PXDDEXA(NEQN_ACC,3,NSAT)
                                ! index array for accel partials
      DIMENSION IPTAT(NEQN_ACC)

      DIMENSION AA(1),II(1),LL(1)
      DIMENSION XTEMP(6),VMAT(3,6),XDDTMP(3),HOLD(3,3),DXDD(3)
      DIMENSION XDDTMC(3,NSAT)
      DIMENSION XPV(3)
      DIMENSION SATLAT(2),SATLON(2),DUMAR(3,3)
      DIMENSION DRGACC(3),SOLACC(3),ALBACC(3),THMACC(3)
      DIMENSION TOPXAT(2,NSAT),LTPXAT(3,NSAT)
      DIMENSION HCLXYZ(3,3),DUM(3)
      DIMENSION ISATID(NSAT)
      DIMENSION ROT(9),ATROT(3,3),SBFTOD_TOT(3,3)
      DIMENSION SBF(3,3)
                             ! rot s/c body fixed -> TOD
      DIMENSION SBFTOD(3,3)
      DIMENSION DDTEXT_TOD(3)
      DIMENSION DUM1(3,4),VAL(1)
      DIMENSION DUM2(3)
      DIMENSION NPARAM(27)
      DIMENSION IACCP(2,NSAT),PACCL(MACC,3,NSAT)

      DIMENSION PSTAT(6),PMOON(6),PSUN(3)
      DIMENSION CHEB(1,IORTB),CHEBV(1,IORTB),                         &
     &          CHEBS(1,IORSUN),CHEBSV(1,IORSUN),                     &
     &          CHEBMO(1,IORMO),CHBVMO(1,IORMO)
      DIMENSION UNTSUN(3)

      INTEGER, parameter :: Ntotforces = 18
      INTEGER, parameter :: Nsurfforce = 11
      dimension LCALL(Ntotforces)
      dimension LFHIRT(Ntotforces)
      dimension LFILLREG(Ntotforces)
      dimension LFILLHR(Ntotforces)
      dimension LFILLCAC(Ntotforces)
      dimension LSURFF(NSURF)
      dimension tmp_acc(3)
      dimension pxddexa_bf(3)
      INTEGER,dimension(27*MSETA) :: IPTFBS
      DIMENSION NPRMAC(MXHRG)

      DIMENSION XPS_AST_REF(3,2)

!     index for surface forces in high rate array
      INTEGER,dimension(Nsurfforce) :: ims2r
      DATA IMS2R/ 3,1,2,6,4,5,7,8,9,10,11 /
      DATA INTBD/10,1,9,11,2,3,4,5,6,7,8/

      LOGICAL ::   LHIRATE
                          !%ACC !  max number of dynamic accel. periods

      DIMENSION NPERDY(NSAT),DYNTIM(MDYNPD,2,NSAT)
      DIMENSION ACCTIM(MBAPPD,2,NSAT),ACCPER(MBAPPD,NSAT),NPERAC(NSAT)
      DIMENSION EXACCT(2,NSAT),EXACIN(NSAT),EXACOB(MACOBS,NSAT)

                                  ! high rate accels (true of ref.)
      DIMENSION XDDTHR(3,NSAT)
                                  ! high rate accels (true of date)
      DIMENSION DDTHR_TOD(3)
                                  ! surface accels (s/c body fixed)
      DIMENSION DDTSRF_SBF(3)

!  proper dimensions for XDDTCA
                                       ! computed accelerometer accels.
      DIMENSION XDDTCA(3,NFSCAC, NSAT)

                               ! computed accelerometer accels. (TOD)
      DIMENSION DDTCAD(3)
                             ! computed accelerometer accels. (TOD)   !%
      DIMENSION ROTI(9)
      DIMENSION SUN_PROBE(3) ! Unit vector from sun to satellite in TOD
      DIMENSION XDDGP(3),XDDGS(3),XSA(3),XPSA(3)
      DIMENSION HGCOM(73)
      DIMENSION CSA(NPMSA),SSA(NPMSA)
!
      DIMENSION XSATR(3),DXDDR(3)
      DIMENSION REL_PM_ACCEL_SUM(3)
      DIMENSION REL_PM_ACCEL_SUM2(3)
      DIMENSION DBETA_1(3),DBETA_2(3)
      DIMENSION DGAMMA_1(3),DGAMMA_2(3)
      DIMENSION XPTEMP(3,2)
      DIMENSION XSAT_AST_DIFF(3)
      DIMENSION DGM_1(3),DGM_2(3)
      DIMENSION XDDSUN(3)
      DIMENSION XDDBRN(3),BRNTOD(3)
      DIMENSION WPU(3,2)



!
      INCLUDE 'COMMON_DECL.inc'
      COMMON/ACCEL9/MXACC9,ITOTGA,NUMGA,ITYPGA,MACC,NXACC9
      COMMON/ACCELO/IDYNDL(3),IDYNSN(3),IGEOSN(3),NXACCO
      COMMON/ACCLRM/MDYNPD,MBAPPD,MACOBS,IDYNSC(200),MDYNST,IACCSC(200),&
     &              MACCSC,NACPRM(200),NATPRM(200),NXCLRM
      COMMON/AGRAV /NACSET,NADEG,NAORD,IAGCB,IAGCE,IDATB,IDATE,INTAG,   &
     &KCOUNT,KSKIP,INTC21,NXAGRA
      COMMON/ALBMIS/ALBON,ALBCON,EMSCON,ALBTUM,XALBMI
      COMMON/ASTSUN/BDAST(6),GMSAT,GEOPTS(3)
      COMMON/BWINTG/JBFNRM,JTDNRM,JBWARE,JBWSPC,JBWDIF,JBWEMS,JBWTPA,   &
     &              JBWTPC,JBWTMD,JBWTMF,JBWTHX,JARADJ,JSPADJ,JDFADJ,   &
     &              JEMADJ,JTAADJ,JTCADJ,JTDADJ,JTFADJ,JTXADJ,JARUA ,   &
     &              JSPUA ,JDFUA ,JEMUA ,JTAUA ,JTCUA ,JTDUA ,JTFUA ,   &
     &              JTXUA ,NFACE ,NMOVE,NTFACE,JBFNR2,JBFNR3,JTDNR2,    &
     &              JTDNR3
      COMMON/BWLOGC/LVAREA,LTOPEX,LSPOT2,LGPSVA,LERS1,LMO,LMOC,LTDRSA,  &
     &              LMAGNA,LGFO,LTRMM,LEUVE,LVCL,LENVS,LCRYO,LGRAIL,    &
     &              LHY2A,LSARAL
      COMMON/BUFOPT/LADRAG,LXTIDE,LPOLTI,NXBUFO
      COMMON/CBDACC/BD_ACCEL(10,999)
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CEGREL/LGRELE,LRLCLK,LGRELR,LXPCLK,LBPCLK,NXEGRL
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CGVTMI/NCDT,NSDT,NCPD,NSPD,NCDTA,NSDTA,NCPDA,NSPDA,NGRVTM, &
     &       KCDT,KCPDA,KCPDB,KSDT,KSPDA,KSPDB,KOMGC,KOMGS,KCOMGC,      &
     &       KSOMGC,KCOMGS,KSOMGS,NXGVTI
      COMMON/CHCOEF/ COEFM(3,15),COEFJ(3,15),COEFSA(3,15),COEFS(3,15),  &
     &               COEFEM(3,15),COEFMO(3,15),COEF(3,15)
      COMMON/CLGEOP/LGEOPL,LGPLOR,NXCLGP
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLGVTM/LGVTM,LDPM,LGVTPM,LOPT,LCSDPM,L_MP_IERS2003,        &
     &              L_CS_IERS2003,L_MP_IERS2010,L_CS_IERS2010
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CONTRL/LSIMDT,LOBS  ,LORB  ,LNOADJ,LNADJL,LORFST,LORLST,   &
     &              LORALL,LORBOB,LOBFST,LOBLST,LOBALL,LPREPO,LORBVX,   &
     &              LORBVK,LACC3D,LSDATA,LCUTOT,LACCEL,LDYNAC,LFRCAT,   &
     &              LNIAU, NXCONT
      COMMON/CORA01/KFSEC0,KFSECB,KFSEC ,KFSECV,KH    ,KHV   ,KCTOL ,   &
     &              KRSQ  ,KVMATX,KCPP  ,KCPV  ,KCCP  ,KCCV  ,KCCPV ,   &
     &              KCCVV ,KXPPPP,KX    ,KPX   ,KSUMX ,KXDDOT,KSUMPX,   &
     &              KPXDDT,KAB   ,KPN   ,KAORN ,KSINLM,KCOSLM,KTANPS,   &
     &              KCRPAR,KVRARY,KXM   ,KXNP1 ,KXPRFL,KXM2  ,KXNNP1,   &
     &              KWRK  ,KFI   ,KGE   ,KB0DRG,KBDRAG,KAPGM ,KAPLM ,   &
     &              KCN   ,KSN   ,KSTID ,KTIDE ,KSTDRG,KSTSRD,KSTACC,   &
     &              KLGRAV,KGM   ,KAE   ,KFPL  ,KFEQ  ,KPLNPO,KPLNVL,   &
     &              KXEPOC,KCD   ,KCDDOT,KCR   ,KGENAC,KACN  ,KASN  ,   &
     &              KTHDRG,KCKEP ,KCKEPN,KXNRMZ,KXNRMC,KFSCEP,KFSCND,   &
     &              KAREA ,KXMASS,KRMSPO,KTCOEF,KTXQQ ,KTIEXP,KTXMM ,   &
     &              KTXLL1,KTXSN1,KTS2QQ,KT2M2H,KT2MHJ,KTXKK ,KTSCRH,   &
     &              KPXPK ,KAESHD,KCSAVE,KSSAVE,KCGRVT,KSGRVT,KXDTMC,   &
     &              KDNLT ,KTXSN2,KTNORM,KTWRK1,KTWRK2,KUNORM,KAERLG,   &
     &              KSINCO,KPARLG,KCONST,KBFNRM,KTDNRM,KCSTHT,KTPSTR,   &
     &              KTPSTP,KTPFYW,KPLMGM,KTPXAT,KEAQAT,KEAFSS,KEAINS,   &
     &              KACS  ,KECS  ,KSOLNA,KSOLNE,KSVECT,KSFLUX,KFACTX,   &
     &              KFACTY,KADIST,KGEOAN,KPALB ,KALBCO,KEMMCO,KCNAUX,   &
     &              KSNAUX,KPPER ,KACOSW,KBSINW,KACOFW,KBCOFW,KANGWT,   &
     &              KWT   ,KPLNDX,KPLANC,KTGACC,KTGDRG,KTGSLR,KWTACC,   &
     &              KWTDRG,KWTSLR,KTMACC,KTMDRG,KTMSLR,KATTUD,KDYACT,   &
     &              KACCBT,KACPER,KXDDNC,KXDDAO,KXNC  ,KXPPNC,KSMXNC,   &
     &              KXDDTH,KPDDTH,KXSSBS,KCPPNC,KEXACT,KXACIN,KXACOB,   &
     &              KPXHDT,KTPXTH,KPACCL,KTXSTA,KDELXS,KSMRNC,KPRX  ,   &
     &              KSMRNP,KDSROT,KXUGRD,KYUGRD,KZUGRD,KSUMRC,KXDDRC,   &
     &              KTMOS0,KTMOS, KTMOSP,KSMXOS,KSGTM1,KSGTM2,KSMPNS,   &
     &              KXGGRD,KYGGRD,KZGGRD,KXEGRD,KYEGRD,KZEGRD,KSSDST,   &
     &              KSDINS,KSDIND,KSSDSR,KSSDDG,KTATHM,KTAINS,KTAFSS,   &
     &              KSRAT ,KTRAT ,KHLDV ,KHLDA1,KHLDA4,KHLDA7,KQAST1,   &
     &              KQAST2,KQAST3,KQAST4,KQAST5,KQAST6,NXCA01
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
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
      COMMON/CORI01/KMJDS0,KMJDSB,KMJDSC,KMJDSV,KIBACK,KIBAKV,KIORDR,   &
     &              KIORDV,KNOSTP,KNOCOR,KNSAT ,KN3   ,KNEQN ,KNEQN3,   &
     &              KNH   ,KNHV  ,KNSTPS,KNSTPV,KICPP ,KICPV ,KICCP ,   &
     &              KICCV ,KICCPV,KICCVV,KISUMX,KIXDDT,KISMPX,KIPXDD,   &
     &              KNMAX ,KNTOLD,KNTOLO,KICNT ,KISNT ,KMM   ,KKK   ,   &
     &              KJJ   ,KHH   ,KIBDY ,KSIGN1,KSIGN2,KLL   ,KQQ   ,   &
     &              KIORFR,KIPDFR,KITACC,KJSAFR,KJSPFR,KMJDEP,KMJDND,   &
     &              KNVSTP,KNSTRN,KNDARK,KTIPPT,KTJBDY,                 &
     &              KICNTA,KISNTA,KISHDP,KIPTC ,KIPTS ,KIGTSR,KIXTMC,   &
     &              KTIPTT,KTNOSD,KTQNDX,KTLL1 ,KTJJBD,KTITDE,KTCNTR,   &
     &              KTNN  ,KITACX,KNMOVE,KPANEL,KPLPTR,KNADAR,KNADSP,   &
     &              KNADDF,KNADEM,KNADTA,KNADTC,KNADTD,KNADTF,KNADTX,   &
     &              KITPMD,KSCATT,KITPAT,KILTPX,KEASBJ,KEANMP,KEANAN,   &
     &              KEAPMP,KEAPAN,KEAMJS,KEAPPP,KEAAAA,KICNTT,KISNTT,   &
     &              KTPGRC,KTPGRS,KTPC  ,KTPS  ,KALCAP,KEMCAP,KNSEG ,   &
     &              KICNTP,KISNTP,KNRDGA,KNRDDR,KNRDSR,KIRDGA,KIRDRG,   &
     &              KIRSLR,KSTRTA,KSTRTD,KSTRTS,KDYNPE,KACCPE,KIBCKN,   &
     &              KNRAT ,KIXDDN,KISMXN,KDXDDN,KDSMXN,KICPPN,KACSID,   &
     &              KNEQNH,KHRFRC,KPTFBS,KPTFSB,KIPXDA,KIACCP,KXSTAT,   &
     &              KPXST ,KSALST,KMAPLG,KNMBUF,KSTEPS,KSGMNT,KSATIN,   &
     &              KMEMST,KNEQNI,KBUFIN,KWEMGA,KWEMDR,KTPATS,KTANMP,   &
     &              KTAPPP,KTAMJS,KTASID,KGPSID,KNSSVA,KPNALB,KBRAX1,   &
     &              KBRAX2,KBRAX3,NXCI01
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
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
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CRMBI/RMBI(9)
      COMMON/CRMI/RMI(9)
      COMMON/CTIDES/NTIDE,NSTADJ,NET,NETADJ,NETUN,NOT,NOTADJ,           &
     &   NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3),            &
     &   ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/EXATAC/LEXTAC
      COMMON/EXATGI/MEASAT,MEAFIL,MEAMEM,MEAPLA,MEAANT,NXEAGI
      COMMON/EXTAGI/MTASAT,MTAFIL,MTAMEM,MTAPLA,NXTAGI
      COMMON/EXTAGL/LEXTHC,NXTAGL
      COMMON/EXATFL/LEXATO
      COMMON/EXPGRV/DGRV(3,4,2)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      COMMON/GRANI/NSUBLG,NXGRAN
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/IORCHB/ IORM,IORJ,IORSA,IORSUN,IOREM,IORMO,IORCB,IORTB,    &
     &               IGRP,ICHBOG(2,14),IORSCB(988),NXORCH
      COMMON/ITHERM/IDGSTN,NARC0
      COMMON/LASTER/LASTR(2),LBINAST,NXASTR
      COMMON/LCBODY/LEARTH,LMOON,LMARS
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/LICASE/LINT(2),LCASE(18),NXLCAS
      COMMON/LSFSHD/LSLFSH, NXSLFS
      COMMON/LSTRT/LSTART
      COMMON/LYARKO/LYARK,LTYP1,LTYP2,LMOD1,LMOD2,LYSHAD,LYE1,LSUN
      COMMON/MASCNI/NMASSC,NMASSS,NPMASS,ICPNTM(11475),ISPNTM(11325)
      COMMON/MASCNR/CMASS(11775),SMASS(11775)
      COMMON/MGSSBF/SBFMGS(3,3)
      COMMON/MNSNSP/XMNSNS(7,2)
      COMMON/NFORCE/NHRATE,NSURF,MPXHDT,MXHRG,MXFMG,NFSCAC,NXFORC
      COMMON/NLOCGR/INTEGG,IOPTLG,ICUTLG,MAXDO,NLOCAL,NLATLG,NLONLG,    &
     &              nlocgu, nlocga, nxlocg
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
      COMMON/RLOCGR/RLATLG,RLONLG,GAMMA,HANOM,XRLOC
      COMMON/SETADJ/LSADRG(4),LSASRD(4),LSAGA(4),LSAGP,LSATID,LSAGM,    &
     &              LSADPM,LSAKF,LSADNX,LSADJ2,LSAPGM
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
      COMMON/SETAGP/NAJCA,NAJSA,NAJCG,NAJSG,NAJCT,NAJST
      COMMON/SETIOR/IORDRG,IORSRD,IORGA,IPDDRG,IPDSRD,IPDGA
      COMMON/SETOPT/LSDRG,LSSRD,LSGA,LSORB,LTHDRG,LROCK4
      COMMON/SETPPT/JSPDRG(3),JSPSRD(3),JSPGA(3),JCSAT
      COMMON/SOLTUM/INDTUM,NDTUM,ITUMSC(32),IFLAG1,IFLAG3,NXSOLT
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/SUBACC/DDTEXT(3),XYZDOT(3)
      COMMON/SUNJ2 /DXDDRJ2,PXDDJ2,SVXDDSN(3)
      COMMON/TELEN/NTELEM,NXTELEN
      COMMON/THERMD/DRGSAT,DTHSC,ZETA,SPAXT,SPAXL,THRMFL,XTHERM
      COMMON/TIDALC/LRAY,LOLDST,NXTIDL
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/TPGRAV/NTPGCU,NTPGCA,NTPGSU,NTPGSA,ITPGRV,NTIMEC,NTIMES,   &
     &              NXTPGR
      COMMON/UCLJAI/IUCLSAT(50),NUCLJAI
      COMMON/UCLJAL/LJBUS(50),LJPAN(50),LJTRR(50),LUGFO,LENVI,NUCLJA
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      COMMON/XPRNT/ISATIX
      COMMON/YARINT/NYACRD,NXYARK
      COMMON/YARSCH/YARTYP,YARMOD,AYAR,TAU,RISYEN,FSEYEN,RISYEX,FSEYEX, &
     &              SMEANM,XYARMD
      character(40),dimension(Ntotforces) :: highrate_label

       data highrate_label(1 ) / 'Drag' /
       data highrate_label(2 ) / 'Solar Radiation' /
       data highrate_label(3 ) / 'Gen. acceleration'/
       data highrate_label(4 ) / 'Topex Thermal Rad.'/
       data highrate_label(5 ) / 'Topex Louver Acc.'/
       data highrate_label(6 ) / 'Albedo Radiation'/
       data highrate_label(7 ) / 'LAGEOS Thermal Drag' /
       data highrate_label(8 ) / 'Geopotential Grav.' /
       data highrate_label(9 ) / 'Earth/ocean Tides'/
       data highrate_label(10) / 'Local Gravity'/
       data highrate_label(11) / 'Central Body Grav.'/
       data highrate_label(12) / 'Third body' /
       data highrate_label(13) / 'Phobos/Deimos'  /
       data highrate_label(14) / 'Relativity'  /
       data highrate_label(15) / 'Yukawa Gravity' /
       data highrate_label(16) / 'Planetary Moon Gravity' /
       data highrate_label(17) / 'Yarkovsky/Schach' /
       data highrate_label(18) / 'Finite Burn' /
!
      data kentry/0/
      data icbsun/11/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
! SUBROUTINE GETRPY WHEN CALLED FROM F SOHULD BE USING THE SPACECRAFT BODY ATTIT
! NOT THE INSTRUMENT (LASER) ATTITIDE
! DURING THE COURSE OF F ZERO OUT THE INSTRUMENT (LASER) ATTITIDE
       HLDLAS=BLKDAT(4,1)
       BLKDAT(4,1)=0.D0
!
!
       LJASON1=.FALSE.
       LGFO1=.FALSE.
       LENV1=.FALSE.
!!!    IF(LXPCLK) CALL CLKACC(MJDSEC,FSEC,NSAT,XP,XDDRC,AA,II,LL)
       ISATIX=ISATID(1)
       kentry = kentry + 1
!       write(6,*) 'f: kentry ', kentry
!       if( kentry .gt. 10 ) stop
!
      FSSTRT=MJDSEC+FSEC-ESSTRT
      TIME  =MJDSEC+FSEC

!      write(6,*) 'f:  FSSTRT ', FSSTRT

!
!   SET POINTERS FOR SATELLITE DEPENDENT FORCE MODEL PARAMETERS
!
      INDNP=0
      INDNMX=0
      INDNM1=0
      INDCR=KCRPAR
      INDVR=KVRARY
      IND18=0
      NP6=6*NP
      INDEXP=0
      IB0DRG=JSPDRG(1)
      IBDRG=JSPDRG(2)
      ISTDRG=JSPDRG(3)
      ILAJDP=ISTDRG-KSTDRG+KLAJDP
      IAPLM=JSPSRD(1)
      IAPGM=JSPSRD(2)
      ISTSRD=JSPSRD(3)
      ILAJSP=ISTSRD-KSTSRD+KLAJSP
      IGA=JSPGA(1)
      ISTGA=JSPGA(2)
      ILAJGP=ISTGA-KSTACC+KLAJGP
      ITGA=JSPGA(3)
      ILSTSN=KLSTSN+JCSAT
      INDARK=KNDARK+JCSAT
      INSTRN=KNSTRN+JCSAT
      NCARG=NCCALL-1

!     INITIALIZE NPARAM
      NPARAM(1)=NPVAL0(IXDRAG)
      NPARAM(2)=NPVAL0(IXSLRD)
      NPARAM(3)=NPVAL0(IXACCL)
      NPARAM(16)=NPVAL0(IXATUD)
!
! OTHER INITIALIZATIONS
      LTUMALB=.FALSE.

      IF(ALBTUM.GE.1.0D0)LTUMALB=.TRUE.



! INITIALIZE THE ATROT AND ATTROT ARRAYS
      DO  90 I=1,3
      DO  90 J=1,3
      ATROT(I,J)=0.D0
   90 CONTINUE
      DO I=1,3
      ATROT(I,I)=1.D0
      ENDDO

! GET TIME DEPENDENT/POSITION INDEP TRANSFORMATIONS BEFORE SAT LOOP
! THE POLE OF BODY FIXED FORCE ROUTINES DETERMINED WITHIN
!
!     lgeopl = .true.
!      write(6,*) 'f: CALL PREFCR '
      CALL PREFCR(NCCALL,SFRAC,LHIRATE,AA,COSTG,SINTG,ROTI)

!      write(6,*) 'f: CALL REFCRS '
      CALL REFCRS(LGEOPL,SINTG,COSTG,                                   &
     &   AA(KDPSR),AA(KXPUT),AA(KYPUT),MJDSEC,FSEC,ROTI(1),             &
     &   ROTI(2),ROTI(3),ROTI(4),ROTI(5),ROTI(6),ROTI(7),               &
     &   ROTI(8),ROTI(9),AA(KA1UT),                                     &
     &   II(KINDPI),AA(KXDPOL),AA(KXDOTP),0)
!
!c     write(6,*) 'f: rmb ', rmb
!c     write(6,*) 'f: rmi ', rmi
!
!  MODIFY C & S COEFFICIENTS FOR TIME DEPENDENT GRAVITY AND DYNAMIC
!  POLAR MOTION
!
      IEND=NPVAL(IXGPC)+NPVAL(IXGPS)
!     write(6,*) 'F: iend ',iend
      IF(KENTRY.EQ.1) THEN
         DO 421 J=1,IEND
         AA(KCNAUX+J-1)=AA(KCN+J-1)
  421    CONTINUE
      ELSE
         DO 422 J=1,IEND
         AA(KCN+J-1)=AA(KCNAUX+J-1)
  422    CONTINUE
      ENDIF
!
      IF(LGVTPM)  CALL GRVTIM(MJDSEC,FSEC,                              &
     &     II(KIPTC), II(KIPTS) ,AA(KCSAVE),AA(KSSAVE),                 &
     &     AA(KCDT)  ,AA(KCPDA) ,AA(KCPDB) ,AA(KOMGC) ,AA(KSDT)  ,      &
     &     AA(KSPDA) ,AA(KSPDB) ,AA(KOMGS) ,AA(KCOMGC),AA(KSOMGC),      &
     &     AA(KCOMGS),AA(KSOMGS),AA(KCN)   ,AA(KSN))
!
! GET INERTIAL PLANETARY POSITIONS BEFORE SATELLITE LOOP
!
      IF(LGRELE.OR.LXTIDE.OR.LASTSN) THEN
      LVELP=.TRUE.
      ELSE
      LVELP=.FALSE.
      ENDIF
      IF(NPVAL(IXRELP).GT.0) LVELP=.TRUE.

!
      CALL PLANPO(MJDSEC,FSEC,LVELP,.FALSE.,AA,II)
!
! IF DRAG IS TURNED ON GET THE POS INDEP PORTION OF THE DRAG MODEL
!
!     IF(LSDRG) CALL D71TM(MJDSEC,FSEC)
!***** PATCH TILL GET A MARTIAN ATMOSPHERIC DENSITY
      IF(LSDRG.AND.ICBDGM.EQ.3) CALL D71TM(MJDSEC,FSEC)
!*****
!
!   LOOP THRU THE SATELLITES IN THE CURRENT SATELLITE SET
!

      DO 2000 ISAT=1,NSAT
      DO IQP=1,NEQN
      PXDDEX(IQP,1,ISAT)=0.D0
      PXDDEX(IQP,2,ISAT)=0.D0
      PXDDEX(IQP,3,ISAT)=0.D0
      ENDDO

! FIGURE OUT IF THIS IS A DYNSEL PERIOD

! INITIALIZE LDYNACP

      LDYNACP=.FALSE.
      LSAT=.FALSE.

      DO I=1,MDYNST
      IF(IDYNSC(I).EQ.ISATIX) THEN
      LSAT=.TRUE.
      GOTO 101
      ENDIF
      ENDDO
  101 CONTINUE
! LDYNAC=DYNAMIC ACC
       IF(LDYNAC.AND.LSAT) THEN
! FIGURE OUT IF THIS IS A DYNSEL PERIOD
      CALL GETDYN(AA,MJDSEC,FSEC,DYNTIM(1,1,ISAT),NPERDY(ISAT),LDYNACP)
      IF(LDYNACP)                                                       &
     &CALL GETXAC(AA,MJDSEC,FSEC,EXACCT(1,ISAT),EXACIN(ISAT),II(KACSID),&
     &EXACOB(1,ISAT),LDYNACP)
! LDYNACP=TRUE THIS IS A DYNAMIC PERIOD
       ENDIF

!       write(6,*) 'f: LHIRATE ', LHIRATE

!-----------------------------------------------------------------------

!     Use of these forces during high rate acceleration is controlled
!     by LHIRATE and LFHIRT

!        Forces computed in F   Subroutine     INDEX          New DEP No
!        --------------------   ----------     -----          ----------
!        Geopotential Grav.     EGRAV              1               8
!        Central Body Grav.     F                  2              11
!        Third body &
!        indirect oblation      SUNGRV             3              12
!        (for lunar orbiter)    LUNFRC             3              12
!        Earth/ocean Tides      TIDCON             4               9
!        Gen. acceleration      GENAC3             5               3
!                               RESON              5               3
!                               GENACC             5               3
!        Drag                   DRAG               6               1
!        Local Gravity          GRANOM             7              10
!        Relativity             FEAREL             8              14
!        Yukawa Gravity         RELFRC             9              15
!        Planetary Moon         MNGRAV            10              16
!        Gravity
!        Phobos/Deimos          PDGRAV            11              13
!        Indirect Acc.
!        Solar Radiation        SOLRD             12               2
!                               SOLRK4            12               2
!        Albedo Radiation       ALBEDO            13               6
!        Topex Thermal Rad.     TOPTHM            14               4
!        Topex Louver Acc.      TOPLOV            15               5
!        LAGEOS Thermal Drag    THEDRG            16               7
!        YARKOVSKY EFFECT       YRKVSK            17              17
!        FINITE BURNS           FBURN             18              18
!-----------------------------------------------------------------------

!    use of these forces in the dynamic accel. period is controlled
!    by LSURFF and LDYNACP

!            List of surface forces:         Index
!            -----------------------         -----
!
!            General acceleration             1
!            Drag                             2
!            Solar Radiation                  3
!            Albedo                           4
!            Topex Thermal Rad.               5
!            Topex Louver Acc.                6
!            LAGEOS Thermal Drag              7

!-----------------------------------------------------------------------

! Set Default Flags
         LCALL(1:Ntotforces)  = .TRUE.
         LFILLREG(1:Ntotforces) = .NOT.LFHIRT(1:Ntotforces)
         LFILLHR(1:Ntotforces)=LFHIRT(1:Ntotforces)
         LFILLCAC(1:Nsurfforce)=.FALSE.
         LDYNHR=.FALSE.
         LDYNREG=.FALSE.
! Multirate integration
         IF( LHIRATE ) THEN
             LCALL(1:Ntotforces) = LFHIRT(1:Ntotforces)
             LFILLREG(1:Ntotforces) =.FALSE.
         ENDIF
         IF(LALL) THEN
             LCALL(1:Ntotforces) =.TRUE.
             LFILLREG(1:Ntotforces) =.TRUE.
         ENDIF
! Dynamic Acceleration
         IF(LDYNACP) THEN
            LFIND=.FALSE.
            DO IQP=1,Nsurfforce
               IF(LSURFF(IQP)) THEN
                  JQP=IMS2R(IQP)
                  IF(.NOT.LFIND) THEN
                    LDYNHR=LFHIRT(JQP)
                    LDYNREG=.NOT.LFHIRT(JQP)
                    IF(LALL) LDYNREG=.TRUE.
                  ENDIF
                  LFIND=.TRUE.
                  LCALL(JQP)=.FALSE.
                  LFILLREG(JQP)=.FALSE.
                  LFILLHR(JQP)=.FALSE.
                ENDIF
           ENDDO
         ENDIF
! Geometric Accelerometry
         IF(LACCELS) THEN
           DO IQP=1,Nsurfforce
             IF(LSURFF(IQP)) THEN
                  JQP=IMS2R(IQP)
                  LCALL(JQP)=.TRUE.
                  LFILLCAC(JQP)=.TRUE.
             ENDIF
           ENDDO
           ENDIF



!        write(6,*) 'f:  Ntotforces ',  Ntotforces
!        write(6,*) 'f:  LFHIRT ',  LFHIRT
!        write(6,*) 'f:  LCALL ', LCALL
!        write(6,*) 'f:  LFILLREG ',  LFILLREG
!        write(6,*) 'f:  LFILLHR  ',  LFILLHR
!        write(6,*) 'f:  LDYNHR ', LDYNHR
!        write(6,*) 'f:  LDYNREG ', LDYNREG

!        if( .not.LALL .or. kentry.eq.1) then
!        write(6,*) 'f:  iforce, name, LCALL, LFHIRT, LFILLREG '
!        do iforce=1,Ntotforces                                !fancy de
!          write(6,'(1x,i5,3x,A, 3x, L1,3x,L1,3x,L1)')
!     &              iforce, highrate_label(iforce),
!     &              LCALL(iforce), LFHIRT( iforce), LFILLREG(iforce)
!        enddo
!        endif
!
!  Initialize arrays
                              ! regular accels
      XDDOT(1:3,ISAT)  = ZERO
                              ! high rate accels
      XDDTHR(1:3,ISAT) = ZERO

!     Zero out only the first 3 slots in XDDTCA
                                       ! computed accelerometer accels.
      XDDTCA(1:3,1:NFSCAC,ISAT) = ZERO
                            ! high rate accels (true of date)
      DDTHR_TOD = ZERO
                     ! computed accelerometer (surf-forces) accels.
      DDTCAD = ZERO
                     ! (true of date)

!     ....set local switches to control the calculation of forces
!     ....for this call

!     GET THE SATELLITE ID
!
      IDSATS=ISATID(ISAT)
      LTUMSOL=.FALSE.
      IF(INDTUM.EQ.1) THEN
       LTUMSOL=.TRUE.
      ELSEIF(INDTUM.EQ.2)  THEN
       CALL FNDNUM(IDSATS,ITUMSC,NDTUM,IRET)
       IF(IRET.NE.0) THEN
           LTUMSOL=.TRUE.
       END IF
      ENDIF

      LGPSTL=.FALSE.
      IF(LTUMSOL.AND.LTXPRT) LGPSTL=.TRUE.
!
      RSQ=XP(1,ISAT,1)*XP(1,ISAT,1)+XP(2,ISAT,1)*XP(2,ISAT,1)
!
      RSQ=RSQ+XP(3,ISAT,1)*XP(3,ISAT,1)
      R=SQRT(RSQ)
      RPLOT=R
!
!   GET TRUE OF DATE COORDINATES OF SATELLITE
!
      XTEMP(1)=RMI(1)*XP(1,ISAT,1)+RMI(4)*XP(2,ISAT,1)+                 &
     &         RMI(7)*XP(3,ISAT,1)
      XTEMP(2)=RMI(2)*XP(1,ISAT,1)+RMI(5)*XP(2,ISAT,1)+                 &
     &         RMI(8)*XP(3,ISAT,1)
      XTEMP(3)=RMI(3)*XP(1,ISAT,1)+RMI(6)*XP(2,ISAT,1)+                 &
     &         RMI(9)*XP(3,ISAT,1)

!     WRITE(6,*) 'XTEMP(1)  ',XTEMP(1)  
!     WRITE(6,*) 'XTEMP(2)  ',XTEMP(2)  
!     WRITE(6,*) 'XTEMP(3)  ',XTEMP(3)  
!     WRITE(6,*) 'RMI(1)  ',RMI(1)  
!     WRITE(6,*) 'RMI(2)  ',RMI(2)  
!     WRITE(6,*) 'RMI(3)  ',RMI(3)  
!     WRITE(6,*) 'RMI(4)  ',RMI(4)  
!     WRITE(6,*) 'RMI(5)  ',RMI(5)  
!     WRITE(6,*) 'RMI(6)  ',RMI(6)  
!     WRITE(6,*) 'RMI(7)  ',RMI(7)  
!     WRITE(6,*) 'RMI(8)  ',RMI(8)  
!     WRITE(6,*) 'RMI(9)  ',RMI(9)  
!     write(6,*) XP


!
!   GET VARIOUS BODY FIXED QUANTITIES ASSOCIATED WITH SATELLITE
!
      call  ECFSET(.true., xp, indcr, indvr, r, rsq, isat, nsat,        &
     &             gmr3, AA, II, LL)
!
!     IF(LITER1)THEN
      IF(kentry.eq.1.and.liter1) then
         kc1=kcount
         ninlst=1
      endif
      if(ninner.ne.ninlst) then
         kcount=kc1
         ninlst=ninner
      endif
! *****************************************************************
!  ADD ATMOSPHERIC EFFECT ON GRAVITY COEFFICIENTS IF APPLICABLE BEG
! *****************************************************************
!
      LINTRP=.FALSE.
      IF(NACSET.LE.0) GO TO 1000
      FMJD=DBLE(MJDSEC)+FSEC
      XDIF=1.D20
      IPDQ=-1
      LEXTR=.FALSE.
      LINTRP=.FALSE.
      TINT=0.D0
      DO IQP=1,NACSET
        XTEST=ABS(FMJD-AA(KACTIM-1+IQP))
        IF(XTEST.LT.XDIF) THEN
           IPDQ=IQP
           XDIF=XTEST
        ENDIF
      ENDDO
      IF(IPDQ.LT.0) GO TO 1000
! FOUND THE PERIOD CLOSEST TO INTEGRATION TIME
! IF NOT INTERPOLATING PROCEED TO ATGCON
      IF(INTAG.NE.1) GO TO 900
      LINTRP=.TRUE.
      IF(FMJD.LT.AA(KACTIM).OR.FMJD.GT.AA(KACTIM-1+NACSET)) THEN
         LINTRP=.FALSE.
         GO TO 900
      ENDIF
      IF(FMJD.LT.AA(KACTIM-1+IPDQ)) IPDQ=IPDQ-1
      TINT=(AA(KACTIM+IPDQ)-FMJD)/(AA(KACTIM+IPDQ)-AA(KACTIM-1+IPDQ))
  900 CONTINUE
      CALL ATGCON(TINT,AA(KACOEF),AA(KCN),AA(KSN),AA(KCN   ),           &
     &     AA(KSN   ),NP,IPDQ,LINTRP,LEXTR)

 1000 CONTINUE
! *****************************************************************
!  ADD ATMOSPHERIC EFFECT ON GRAVITY COEFFICIENTS IF APPLICABLE END
! *****************************************************************
!
! *****************************************************************
!  UPDATE COEFFICIENT ARRAYS FOR TIME PERIOD GRAVITY         BEGIN
! *****************************************************************
!
      IF(NTIMEC.GT.0.OR.NTIMES.GT.0) THEN
!     IF(KENTRY.EQ.1) THEN
!     DO 419 J=1,IEND
!     AA(KCNAUX+J-1)=AA(KCN+J-1)
!419  CONTINUE
!     ENDIF
      IEND=NPVAL(IXGPC)+NPVAL(IXGPS)
      DO 420 J=1,IEND
      AA(KCN+J-1)=AA(KCNAUX+J-1)
  420 END DO
!     IF(NINNER.EQ.1) THEN
!
! First check times
      FMJD=DBLE(MJDSEC)+FSEC
      IBEG=IPVAL(IXTGPC)
      IEND=IBEG+NPVAL(IXTGPC)+NPVAL(IXTGPS)-1
      IC=NPVAL(IXTGPC)
      CALL CHTIME(AA(KPRMV),AA(KCN),AA(KSN),FMJD,II(KTPGRC),II(KTPGRS), &
     &II(KTPUC),II(KTPUS),II(KTPC),II(KTPS),II(KTPGPC),                 &
     &II(KTPGPS),II(KICNTP),II(KISNTP),II(KICNT),II(KISNT),II(KICNTT),  &
     &II(KISNTT),LC,LS,ISEQC,                                           &
     &ISEQS,IBEG,IEND,IC,NAJCG,NAJSG,NAJCT,NAJST)
!     ENDIF
      ENDIF
! *****************************************************************
!  UPDATE COEFFICIENT ARRAYS FOR TIME PERIOD GRAVITY           END
! *****************************************************************
!
! *****************************************************************
!   GET EARTH GEOPOTENTIAL ACCELERATION IN TRUE OF REF
! *****************************************************************
!
      IF( LCALL(8) ) THEN
         IF(NLOCAL.GT.0.AND.IOPTLG.EQ.3) THEN
            NPX=NPMASS
            IF(NPMASS.GT.NP) NPX=NP
            DO J=1,NPX
              AA(KCN+J-1)=AA(KCN+J-1)+CMASS(J)
              AA(KCN+J-1+NP)=AA(KCN+J-1+NP)+SMASS(J)
            ENDDO
         ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! START THIS IF LOOP IS NOT NEEDED IF ARTIFICAL SAT IS MASSLESS
      IF(LASTSN) THEN
         C10H=AA(KCN)
         C11H=AA(KCN+1)
         S11H=AA(KSN+1)
!
         AA(KCN)=0.D0
         AA(KCN+1)=0.D0
         AA(KSN+1)=0.D0
         CALL EGRAV(AA(KXNRMZ),AA(KXNRMC),AA(KPN+INDNP),AA(KCN),AA(KSN),&
     &      AA(KAORN+INDNMX),AA(KSINLM+INDNM1),AA(KCOSLM+INDNM1),       &
     &      AA(KTANPS+INDNM1),AA(KXM),AA(KXNP1),AA(KXPRFL+INDEXP),      &
     &      tmp_acc,AA(INDVR),AA(KTNORM))
!
         AA(KCN)=C10H
         AA(KCN+1)=C11H
         AA(KSN+1)=S11H
         GEOPTS(1)=tmp_acc(1)-GMR3*XP(1,ISAT,1)
         GEOPTS(2)=tmp_acc(2)-GMR3*XP(2,ISAT,1)
         GEOPTS(3)=tmp_acc(3)-GMR3*XP(3,ISAT,1)
      ENDIF
!!!!!! END THIS IF LOOP IS NOT NEEDED IF ARTIFICAL SAT IS MASSLESS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         CALL EGRAV(AA(KXNRMZ),AA(KXNRMC),AA(KPN+INDNP),AA(KCN),AA(KSN),&
     &      AA(KAORN+INDNMX),AA(KSINLM+INDNM1),AA(KCOSLM+INDNM1),       &
     &      AA(KTANPS+INDNM1),AA(KXM),AA(KXNP1),AA(KXPRFL+INDEXP),      &
     &      tmp_acc,AA(INDVR),AA(KTNORM))
         DGRV(1,1,1)=tmp_acc(1)
         DGRV(2,1,1)=tmp_acc(2)
         DGRV(3,1,1)=tmp_acc(3)
!!       IF(LXPCLK) CALL CLKACC(MJDSEC,FSEC,NSAT,XP,XDDRC,AA,II,LL)
       IF(LXPCLK) CALL CLKACC(MJDSEC,FSEC,NSAT,XP,XDDRC,AA,II,LL,       &
     &                        AA(KPN+INDNP),AA(KCOSLM+INDNM1),          &
     &                        AA(KSINLM+INDNM1),AA(KAORN+INDNMX),       &
     &                        AA(KCN),AA(KSN))



!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
         IF(LFILLREG(8)) XDDOT(1:3,ISAT) = tmp_acc(1:3)

!   LFILLHR = FILL THE HIGH RATE ARRAY
         IF(LFILLHR(8)) XDDTHR(1:3,ISAT)  = tmp_acc(1:3)

!***********************************  END ******************************

      IF(.NOT.LSAGM) GO TO 20
! GET GM PARTIALS IF REQUESTED
      PXDDEX(JSAGM,1,ISAT)=(XDDOT(1,ISAT)-GMR3*XP(1,ISAT,1))/GM
      PXDDEX(JSAGM,2,ISAT)=(XDDOT(2,ISAT)-GMR3*XP(2,ISAT,1))/GM
      PXDDEX(JSAGM,3,ISAT)=(XDDOT(3,ISAT)-GMR3*XP(3,ISAT,1))/GM
   20 CONTINUE
!
!      write(6, 12345) XDDOT(1,isat),XDDOT(2,isat),XDDOT(3,isat)
!12345 FORMAT(' TRUE OF REF GEOPOT',3D25.16)
!      write(6,*) 'f: tmp_acc ', tmp_acc
!      write(6,*) 'f: xddthr(1:3,ISAT) ', xddthr(1:3,ISAT)
!      write(6,*) 'f: xddot(1:3,ISAT) ', xddot(1:3,ISAT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
      IF(.NOT.LBINAST) GO TO 30
      NGH1=NMAX
      NGH2=NMAXP1
      NGH3=NP
! START CODE FOR GRAVITATIONAL ATTRACTION FROM SECONARY ASTEROID
      CALL CRDSET(1,HGCOM)
      GM=AA(KPRMV-1+IPVAL(IX2GM))
      AE=AA(KPRMV-1+IPVAL(IX2BDA))
      CALL PREFCS(MJDSEC,FSEC,AA,II,COSTG,SINTG,ROTI)
      CALL REFCRS(.FALSE.,SINTG,COSTG,                                  &
     &   AA(KDPSR),AA(KXPUT),AA(KYPUT),MJDSEC,FSEC,ROTI(1),             &
     &   ROTI(2),ROTI(3),ROTI(4),ROTI(5),ROTI(6),ROTI(7),               &
     &   ROTI(8),ROTI(9),AA(KA1UT),                                     &
     &   II(KINDPI),AA(KXDPOL),AA(KXDOTP),0)
      XPSA(1)=XP(1,1,1)-XSA(1)
      XPSA(2)=XP(2,1,1)-XSA(2)
      XPSA(3)=XP(3,1,1)-XSA(3)
      RSQSA=XPSA(1)*XPSA(1)+XPSA(2)*XPSA(2)+XPSA(3)*XPSA(3)
      RSA=SQRT(RSQSA)
      RSA3=RSQSA*RSA
      call  ECFSET(.FALSE.,XPSA, INDCR+9,INDVR+14,RSA,RSQSA,1,1,        &
     &             GMR3SA, AA, II, LL)
!
      NUP=NPVAL(IX2CCO)
      ITEST=NMAX
      NMAX=6
      IF(ITEST.LT.NMAX) NMAX=ITEST
      NMAXP1=NMAX+1
      NP=NMAX*NMAXP1/2+3*NMAX
      NUP=NPVAL(IX2CCO)
      IF(NUP.GT.NPMSA) THEN
        WRITE(6,60010)
        WRITE(6,60014)
        WRITE(6,60015) NUP,NPMSA
        STOP
      ENDIF
      DO IQP=1,NPMSA
        CSA(IQP)=0.D0
        SSA(IQP)=0.D0
        AA(KPN+NGH3-1+IQP)=0.D0
      ENDDO
      DO IQP=1,NUP
        CSA(IQP)=AA(KPRMV-2+IPVAL(IX2CCO)+IQP)
        SSA(IQP)=AA(KPRMV-2+IPVAL(IX2SCO)+IQP)
      ENDDO
      CALL EGRAV(AA(KXNRMZ),AA(KXNRMC),AA(KPN+NGH3),CSA,SSA,            &
     &      AA(KAORN+NGH1),AA(KSINLM+NGH2),AA(KCOSLM+NGH2),             &
     &      AA(KTANPS+NGH2),AA(KXM),AA(KXNP1),AA(KXPRFL+NGH3*6),        &
     &      tmp_acc,AA(INDVR+14),AA(KTNORM))
      tmp_acc(1)=tmp_acc(1)-XDDGP(1)
      tmp_acc(2)=tmp_acc(2)-XDDGP(2)
      tmp_acc(3)=tmp_acc(3)-XDDGP(3)
      DGRV(1,1,1)=DGRV(1,1,1)+tmp_acc(1)
      DGRV(2,1,1)=DGRV(2,1,1)+tmp_acc(2)
      DGRV(3,1,1)=DGRV(3,1,1)+tmp_acc(3)
      DGRV(1,1,2)=DGRV(1,1,1)
      DGRV(2,1,2)=DGRV(2,1,1)
      DGRV(3,1,2)=DGRV(3,1,1)
      XDDOT(1,ISAT)=XDDOT(1,ISAT)+tmp_acc(1)-XPSA(1)*GM/RSA3
      XDDOT(2,ISAT)=XDDOT(2,ISAT)+tmp_acc(2)-XPSA(2)*GM/RSA3
      XDDOT(3,ISAT)=XDDOT(3,ISAT)+tmp_acc(3)-XPSA(3)*GM/RSA3
      IF(LSTINR.AND.NPVAL0(IX2GM).GT.0) THEN
        IPGM=NEQN
        PXDDEX(IPGM,1,ISAT)=(tmp_acc(1)-XPSA(1)*GM/RSA3)/GM
        PXDDEX(IPGM,2,ISAT)=(tmp_acc(2)-XPSA(2)*GM/RSA3)/GM
        PXDDEX(IPGM,3,ISAT)=(tmp_acc(3)-XPSA(3)*GM/RSA3)/GM
      ENDIF
      CALL CRDSET(2,HGCOM)
! END CODE FOR GRAVITATIONAL ATTRACTION FROM SECONARY ASTEROID
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
   30 CONTINUE

            ! LCALL(8)
      ENDIF
!
!   THE REST OF THE FORCE MODEL ROUTINES WITH THE EXCEPTION OF TIDCON
!   DEAL EXCLUSIVELY IN TRUE OF DATE COORDINATES.
!   THE CONTRIBUTIONS FROM THESE EXTRA ROUTINES TO THE VMATRX
!   ARE MADE  IN THE ROUTINES IN TRUE OF DATE
!
!   IF NECESSARY,ZERO OUT TRUE OF DATE VMAT
!
      IF(LSORB) GO TO 200
      DO 100 I=1,18
  100 VMAT(I,1)=ZERO
  200 CONTINUE
!
!   GET THIRD BODY ACCELERATION & INDIRECT OBLATION ACCELERATION
!
! for debug
!     temporary  -- need call to sungrv to get BDTRUE
!     other code will provide BDTRUE later
! for debug

!!!      IF( LCALL(12) ) THEN

        CALL SUNGRV(ISAT,IDSATS,XTEMP,AA(KCN+4),LSORB,tmp_acc,VMAT)
!!        SGA1=RMI(1)*tmp_acc(1)+RMI(2)*tmp_acc(2)+RMI(3)*tmp_acc(3)
!!        SGA2=RMI(4)*tmp_acc(1)+RMI(5)*tmp_acc(2)+RMI(6)*tmp_acc(3)
!!        SGA3=RMI(7)*tmp_acc(1)+RMI(8)*tmp_acc(2)+RMI(9)*tmp_acc(3)
!!        WRITE(6,87654) SGA1,SGA2,SGA3
!!87654    FORMAT(' TOR SUNGRAV FROM F: ',3D25.11)
!
!c       write(6, 12346) DXDD
!c12346 FORMAT(' TRUE OF DATE SUNGRV',3D25.16)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


!      FROM FAST
!
      IF(NPVAL(IXRELP).GT.0) THEN

         DO JJ=1,3
         tmp_acc(JJ)=0.0D0
         END DO

      GMS=BDTRUE(4,8)
      GMSX=GMS+GM
      R2=BDAST(1)*BDAST(1)+BDAST(2)*BDAST(2)+BDAST(3)*BDAST(3)
      RS=SQRT(R2)
      R3=RS*R2

      DO I=1,3
      XDDSUN(I)=-GMSX*BDAST(I)/R3
!      write(6,*)' dbg XDDSUN', XDDSUN(I),BDAST(I),I
      ENDDO

!     Total number of bodies being considered
      IPPN_BODIES=12+ICBODY
!
! ADD ACC OF INTGCB BODY TO BD_ACCEL ARRAY
! SUM XDDSUN (ACC OF INTGCB BODY WITH RSPCT TO SUN)
! AND ACC OF SUN
!
! XDDSUN IS COMPUTED IN FAST. PUT IT IN A CB AND SEE IF IT COMES THROUGH

!     BD_ACCEL(7,12)=BD_ACCEL(7,8)+XDDSUN(1)
!     BD_ACCEL(8,12)=BD_ACCEL(8,8)+XDDSUN(2)
!     BD_ACCEL(9,12)=BD_ACCEL(9,8)+XDDSUN(3)
!       write(6,*)' dbgb', BD_ACCEL(7,12),BD_ACCEL(7,8),XDDSUN(1)
!       write(6,*)' dbgb', BD_ACCEL(8,12),BD_ACCEL(8,8),XDDSUN(2)
!       write(6,*)' dbgb', BD_ACCEL(9,12),BD_ACCEL(9,8),XDDSUN(3)

!         write(6,*)' dbg F LASTSN',LASTSN
       IF(LASTSN)  THEN

!!!  THE FIRST PART HAS BEEN MOVED FROM FAST  IN ORDER TO HAVE THE NEW
!!!  RELATIVISTIC MODEL FOR ALL GEODYN RUNS

      DO JJ=1,3
        XPS_AST_REF(JJ,1)=XP(JJ,ISAT,1)+BD_ACCEL(JJ,12)
        XPS_AST_REF(JJ,2)=XP(JJ,ISAT,2)+BD_ACCEL(JJ+3,12)
      END DO
!!
!!
      CALL REL_PM_ACC(XPS_AST_REF,REL_PM_ACCEL_SUM,IPPN_BODIES,1,DBETA_1&
     &,DGAMMA_1,DGM_1)
!
      DO JJ=1,3
        XPTEMP(JJ,1)=BD_ACCEL(JJ,12)
        XPTEMP(JJ,2)=BD_ACCEL(JJ+3,12)
      END DO

!     write(6,*)' dbg AFTER THE FIRST CALL TO REL_PM '
!     write(6,*)' dbg ACCEL OF S/C ORB PLANET ',REL_PM_ACCEL_SUM(1)
!     write(6,*)' dbg ACCEL OF S/C ORB PLANET ',REL_PM_ACCEL_SUM(2)
!     write(6,*)' dbg ACCEL OF S/C ORB PLANET ',REL_PM_ACCEL_SUM(3)

!
!!!
!!!   CALL REL_PM_ACC FOR PM ACCEL FOR AST
!!!
      CALL REL_PM_ACC(XPTEMP,REL_PM_ACCEL_SUM2,IPPN_BODIES,2,DBETA_2    &
     &,DGAMMA_2,DGM_2)
!
!     write(6,*)' dbg ACC aft 2nd call to REL_PM ',REL_PM_ACCEL_SUM2(1)
!     write(6,*)' dbg ACC aft 2nd call to REL_PM ',REL_PM_ACCEL_SUM2(2)
!     write(6,*)' dbg ACC aft 2nd call to REL_PM ',REL_PM_ACCEL_SUM2(3)
!!!
!!!   CALCULATE DIFFERENCE IN SAT-AST ACC
!!!   XSAT_AST_DIFF IS THE ACCELERATION ON THE SPACECRAFT
!!!
      DO JJ=1,3
       XSAT_AST_DIFF(JJ)=REL_PM_ACCEL_SUM(JJ)-REL_PM_ACCEL_SUM2(JJ)
      END DO

!       write(6,*)' dbg F XSAT_AST_DIFF ',XSAT_AST_DIFF

       XDDOT(1,ISAT)=XDDOT(1,ISAT)+XSAT_AST_DIFF(1)
       XDDOT(2,ISAT)=XDDOT(2,ISAT)+XSAT_AST_DIFF(2)
       XDDOT(3,ISAT)=XDDOT(3,ISAT)+XSAT_AST_DIFF(3)

! PARTIALS

!     DO JJ=1,3
!       PXDDEX(JLGMSN,JJ,ISAT)=DGM_1(JJ)-DGM_2(JJ)     !  GM partials
!       PXDDEX(JLRELT+1,JJ,ISAT)= DBETA_1(JJ)-DBETA_2(JJ)     ! BETA
!       PXDDEX(JLRELT  ,JJ,ISAT)=DGAMMA_1(JJ)-DGAMMA_2(JJ)  ! GAMMA
!     END DO

!!!  END OF THE BLOCK TO BE MOVED IN F

       ELSE
!     IPPN_BODIES=11

! ONLY S/C NO ASTEROID

! INTBD/10,1,9,11,2,3,4,5,6,7,8/
      INDX=INTBD(ICBDGM)
!      write(6,*)' dbg INDX FOR CB MOON ',INDX,ICBDGM
!      write(6,*)' dbg IPPN_BODIES ',IPPN_BODIES

!      write(6,*)' dbg S/C MOON CENTERED COO ',XP(1,ISAT,1)
!      write(6,*)' dbg S/C MOON CENTERED COO ',XP(2,ISAT,1)
!      write(6,*)' dbg S/C MOON CENTERED COO ',XP(3,ISAT,1)
!DEP CONVERT S/C POSITION TO SSB
      DO JJ=1,3
        XPS_AST_REF(JJ,1)=XP(JJ,ISAT,1)+BD_ACCEL(JJ,INDX)
        XPS_AST_REF(JJ,2)=XP(JJ,ISAT,2)+BD_ACCEL(JJ+3,INDX)
      END DO
!!
!     write(6,*)' dbg SBB S/C POSITION ',XPS_AST_REF(1,1)
!     write(6,*)' dbg SBB S/C POSITION ',XPS_AST_REF(2,1)
!     write(6,*)' dbg SBB S/C POSITION ',XPS_AST_REF(3,1)

      CALL REL_PM_ACC(XPS_AST_REF,REL_PM_ACCEL_SUM,IPPN_BODIES,5,DBETA_1&
     &,DGAMMA_1,DGM_1)

!     write(6,*)' dbg AFTER THE FIRST CALL TO REL_PM '
!     write(6,*)' dbg ACCEL OF S/C ORB PLANET ',REL_PM_ACCEL_SUM(1)
!     write(6,*)' dbg ACCEL OF S/C ORB PLANET ',REL_PM_ACCEL_SUM(2)
!     write(6,*)' dbg ACCEL OF S/C ORB PLANET ',REL_PM_ACCEL_SUM(3)
!
      DO JJ=1,3
        XPTEMP(JJ,1)=BD_ACCEL(JJ,INDX)
!       write(6,*)' dbg BD_ACCEL MOON SSB MOON  ', BD_ACCEL(JJ,INDX)
        XPTEMP(JJ,2)=BD_ACCEL(JJ+3,INDX)
      END DO


      CALL REL_PM_ACC(XPTEMP,REL_PM_ACCEL_SUM2,IPPN_BODIES,6,DBETA_2    &
     &,DGAMMA_2,DGM_2)
!     write(6,*)' dbg ACC aft 2nd call to REL_PM ',REL_PM_ACCEL_SUM2(1)
!     write(6,*)' dbg ACC aft 2nd call to REL_PM ',REL_PM_ACCEL_SUM2(2)
!     write(6,*)' dbg ACC aft 2nd call to REL_PM ',REL_PM_ACCEL_SUM2(3)

!!!
!!!   CALCULATE DIFFERENCE IN SAT-ORBITED BODY ACC
!!!   XSAT_AST_DIFF IS THE ACCELERATION ON THE SPACECRAFT
!!!
      DO JJ=1,3
       XSAT_AST_DIFF(JJ)=REL_PM_ACCEL_SUM(JJ)-REL_PM_ACCEL_SUM2(JJ)
      END DO


!      write(6,*)' dbg TOT ACC ',XSAT_AST_DIFF
!      write(6,*)' dbg XDDOT 1',XDDOT(1,ISAT),XDDOT(2,ISAT),XDDOT(3,ISAT)

       XDDOT(1,ISAT)=XDDOT(1,ISAT)+XSAT_AST_DIFF(1)
       XDDOT(2,ISAT)=XDDOT(2,ISAT)+XSAT_AST_DIFF(2)
       XDDOT(3,ISAT)=XDDOT(3,ISAT)+XSAT_AST_DIFF(3)
!      write(6,*)' dbg XDDOT ',XDDOT(1,ISAT),XDDOT(2,ISAT),XDDOT(3,ISAT)

! PARTIALS

      DO JJ=1,3
!       PXDDEX(JLGMSN,JJ,ISAT)=DGM_1(JJ)        !  GM partials
        PXDDEX(JLRELT+1,JJ,ISAT)= DBETA_1(JJ) - DBETA_2(JJ)  ! BETA
        PXDDEX(JLRELT  ,JJ,ISAT)= DGAMMA_1(JJ) - DGAMMA_2(JJ) ! GAMMA
      END DO

       ENDIF    ! IF(LASTSN)

      ENDIF    ! IF(NPVAL(IXRELP).GT.0)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

        IF(NPVAL(IXJ2SN).GE.1) THEN

! C20S IS IN 2ND SLOT GOF GLOBAL PHANTOM FM
!       C20S=AA(KPRMV+IPVAL(IXFGFM))                                    ! Sun's
        C20S=AA(KPRMV+IPVAL(IXJ2SN)-1)

        XSATR(1)=XP(1,ISAT,1)
        XSATR(2)=XP(2,ISAT,1)
        XSATR(3)=XP(3,ISAT,1)
        CALL SJ2(C20S,XSATR,DXDDR,NEQN,PXDDEX(1,1,1),MJDSEC,FSEC,AA,II)
        XDDOT(1,ISAT)=XDDOT(1,ISAT)+DXDDR(1)
        XDDOT(2,ISAT)=XDDOT(2,ISAT)+DXDDR(2)
        XDDOT(3,ISAT)=XDDOT(3,ISAT)+DXDDR(3)
        ENDIF

!
!c       write(6, 12346) DXDD
!c12346 FORMAT(' TRUE OF DATE SUNGRV',3D25.16)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
          IF(LFILLREG(12))  DXDD(1:3) = tmp_acc(1:3)

!   LFILLHR = FILL THE HIGH RATE ARRAY
          IF(LFILLHR(12)) DDTHR_TOD(1:3)  = tmp_acc(1:3)

!***********************************  END ******************************


        IF(LMOON) THEN
!
!         GET EARTH J2 PERTURBATIONS AND MOON INDIRECT J2 PERTURBATION O
!         A LUNAR ORBITER
!
          CALL LUNFRC(MJDSEC,FSEC,XTEMP,XDDOT(1,ISAT),XDDOT(2,ISAT),    &
     &     XDDOT(3,ISAT),AA,AA(KCN+4),XDDTMP(1),XDDTMP(2),XDDTMP(3),II)
!
!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
         IF(LFILLREG(12))  DXDD(1:3)=DXDD(1:3)+XDDTMP(1:3)

!   LFILLHR = FILL THE HIGH RATE ARRAY
         IF(LFILLHR(12)) DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)

!***********************************  END ******************************

        ENDIF

!        ENDIF  ! LCALL(12)   !%ACC
!  temporary  -- need call to sungrv to get BDTRUE
!  other code will provide BDTRUE later

!
!     ....SECTION FOR TOPEX INTERCOMPARISON CASES
!
      IF( LCASE(13) .OR. LCASE(14) .OR. LCASE(16) ) THEN
         DXDD(1)=0.D0
         DXDD(2)=0.D0
         DXDD(3)=0.D0
      ENDIF
!
!   DXDD   WILL NOW ACCUMULATE TRUE OF DATE ACCELERATION FROM SOURCES
!   XDDTMP WILL NOW CONTAIN TRUE OF DATE ACCELERATION FROM EACH SOURCE
!          AS IT IS COMPUTED
!
!------------------------------------------------------------------
!
!     ....reset the coordinate system rotation matrix
!     ....to be without the geopol rotation
!     ....use matrix rmb0 instead of rmb
!
!   GET VARIOUS BODY FIXED QUANTITIES ASSOCIATED WITH SATELLITE
!
      call  ECFSET(.false., xp, indcr, indvr, r, rsq, isat, nsat,       &
     &             gmr3, AA, II, LL)
!
!------------------------------------------------------------------
!

      if( LCALL(9)) then

!   GET TIDAL ACCELERATIONS:
!   (TRUE OF DATE FOR K2 & K3; TRUE OF REF FOR EXPANDED)
!
! SET UP SCRATCH POINTERS
!
!
!     ....NOTE! -- new args ntide1 and net1
!
      ntide1 = MAX( ntide, 1)
      net1 = MAX( net, 1)
      JAJ=KTWRK1
      JBJ=JAJ+2*NTIDE
      JAT=JBJ+2*NTIDE
      JBT=JAT+NTIDE+1
      JAB=JBT+NTIDE+1
      JBA=JAB+NTIDE
      JTEMP=JBA+NTIDE
      JNEXT=JTEMP+NTIDE*10
!
! GET TIDAL ACCELERATIONS
!
!     ....NOTE: TIDCON PUTS ACCELERATIONS FROM K2 AND K3 INTO XDDTMP
!     ....      AS TRUE OF DATE ACCELERATIONS, BUT
!     ....      TIDCON PUTS ACCELERATIONS FROM THE EXTENDED TIDE
!     ....      MODEL INTO XDDOT AS TRUE OF REFERENCE ACCELERATIONS

!
      ntide1 = MAX( ntide, 1)
      net1 = MAX( net, 1)
      IF(.NOT.LRAY) THEN
      CALL TIDCON(ntide1,net1,                                          &
     &           MJDSEC,FSEC,XTEMP,AA(INDVR+3),ISAT,AA(KAORN+INDNMX),   &
     &           AA(KTHG+NCARG),AA(KTIDE),AA(KFI),AA(KGE),II(KMM),      &
     &           II(KKK),II(KHH),II(KJJ),II(KSIGN1),                    &
     &           II(KSIGN2),II(KLL),II(KQQ),AA(KPN+INDNP),AA(KTNORM),   &
     &           NEQN,                                                  &
     &           PXDDEX(1,1,ISAT),XDDTMP,AA(KTCOEF),                    &
     &           AA(KTXQQ),II(KTIPPT),AA(KTIEXP),AA(KTXMM),AA(KTXLL1),  &
     &           AA(KTXSN1),AA(KTS2QQ),AA(KT2M2H),AA(KT2MHJ),AA(KTXKK), &
     &           II(KTLL1) ,II(KTIPTT),II(KTQNDX),AA(KTXSN2),II(KTNN),  &
     &           II(KTJJBD),II(KTNOSD),II(KTITDE),II(KTCNTR),AA(KTSCRH),&
     &           AA(JAJ),AA(JBJ),AA(JAT),AA(JBT),AA(JAB),AA(JBA),       &
     &           AA(JTEMP),IDSATS,AA)
      ELSE
      NTDDOO=NTIDE
      IF(NDOODN.GT.NTIDE) NTDDOO=NDOODN
      CALL TDCONR(MJDSEC,FSEC,AA(KCONAM),AA(KANGT),AA(KTWRK1),          &
     &            AA(KGRDAN),AA(KUANG),AA(KFAMP),II(KPTDN),II(KOTFLG),  &
     &            AA(KTIDE),AA(KOFACT),AA(KXPRFL+INDEXP),II(KPRESP),    &
     &            II(KMP2RS),II(KJDN),AA(INDVR),AA(INDCR),AA(KXM),      &
     &            AA(KXNP1),AA(KPN+INDNP),AA(KCOSLM+INDNM1),            &
     &            AA(KSINLM+INDNM1),AA(KAORN+INDNMX),AA(KWRKRS),        &
     &            AA(KSPDT),AA(KOTSGN),PXDDEX(1,1,ISAT),NEQN,XDDTMP,    &
     &            II(KPHINC),NTDDOO)
      ENDIF
!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(9)) THEN
              DXDD(1)=DXDD(1)+XDDTMP(1)
              DXDD(2)=DXDD(2)+XDDTMP(2)
              DXDD(3)=DXDD(3)+XDDTMP(3)
                 ! LFILLREG(9)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF( LFILLHR(9)) THEN
              DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 ! LFILLHR(9)
           ENDIF

!***********************************  END ******************************

            ! LCALL(9)    !%ACC
      ENDIF
!
!------------------------------------------------------------------
!
!     ....reset the coordinate system rotation matrix
!     ....to be with the geopol rotation
!     ....use matrix rmb instead of rmb0
!
!   GET VARIOUS BODY FIXED QUANTITIES ASSOCIATED WITH SATELLITE
!
      call ECFSET(.true., xp, indcr, indvr, r, rsq, isat, nsat,         &
     &            gmr3, AA, II, LL)
!
!------------------------------------------------------------------
!
!
!
      IF(.NOT.LSDRG.AND..NOT.LSGA.AND..NOT.LGRELE) GO TO 300
!
!   NEED TRUE OF DATE VELOCITY FOR DRAG & GENERAL ACCELERATION
!
      XTEMP(4)=RMI(1)*XP(1,ISAT,2)+RMI(4)*XP(2,ISAT,2)+                 &
     &         RMI(7)*XP(3,ISAT,2)
      XTEMP(5)=RMI(2)*XP(1,ISAT,2)+RMI(5)*XP(2,ISAT,2)+                 &
     &         RMI(8)*XP(3,ISAT,2)
      XTEMP(6)=RMI(3)*XP(1,ISAT,2)+RMI(6)*XP(2,ISAT,2)+                 &
     &         RMI(9)*XP(3,ISAT,2)
!
      IF(.NOT.LSGA) GO TO 250
!
      IF( LCALL(3)) THEN

!       GET GENERAL ACCELERATION
!
        IF(LACC3D) THEN
!          ....3-DIMENSIONAL ACCELERATION
!      write(6,*) 'f: call GENAC3'
           CALL GENAC3(MJDSEC,FSEC,XTEMP,XDDTMP,PXDDEX(1,1,ISAT),VMAT,  &
     &               LSORB,NEQN,II(ITGA),AA(IGA),AA(ISTGA),LL(ILAJGP))
           GOTO 230
        ENDIF
        IF(MXACC9.GT.0) THEN
!          ....9-PARAMETER ACCELERATION
           IDIFQ=ISTGA-KSTACC
           CALL RESON(AA(IGA+1),XTEMP(1),XTEMP(4),XDDTMP,               &
     &                PXDDEX(1,1,ISAT),                                 &
     &                II(KITACX+IDIFQ),NEQN,AA(ISTGA),VMAT,IACCP)
        ELSE
!          ....1-DIMENSIONAL ACCELERATION

           CALL GENACC(MJDSEC,FSEC,XTEMP,XDDTMP,PXDDEX(1,1,ISAT),VMAT,  &
     &               LSORB,NEQN,II(ITGA),AA(IGA),AA(ISTGA),LL(ILAJGP))
        ENDIF
  230   CONTINUE

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
               IF(LFILLREG(3)) THEN
                 DXDD(1)=DXDD(1)+XDDTMP(1)
                 DXDD(2)=DXDD(2)+XDDTMP(2)
                 DXDD(3)=DXDD(3)+XDDTMP(3)
                     !   LFILLREG(3)
               ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
               IF(LFILLHR(3)) THEN
                 DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 NPARQ=IACCP(2,ISAT)-IACCP(1,ISAT)+1
                 IF(IACCP(1,ISAT).LE.0.OR.IACCP(2,ISAT).LE.0) NPARQ=0
                 IF(NPARQ.GT.0.AND.NRAT.GT.1) THEN
                    IQP=IACCP(1,ISAT)
                    IF(LHIRATE) THEN
                       DO JQP=1,NPARQ
                         PACCL(JQP,1,ISAT)=PACCL(JQP,1,ISAT)            &
     &                  +PXDDEX(IQP,1,ISAT)
                         PACCL(JQP,2,ISAT)=PACCL(JQP,2,ISAT)            &
     &                  +PXDDEX(IQP,2,ISAT)
                         PACCL(JQP,3,ISAT)=PACCL(JQP,3,ISAT)            &
     &                  +PXDDEX(IQP,3,ISAT)
                         IQP=IQP+1
                       ENDDO
                    ELSE
                       DENOM=DBLE(NRAT)
                       DO JQP=1,NPARQ
                         PXDDEX(IQP,1,ISAT)=PACCL(JQP,1,ISAT)/DENOM
                         PXDDEX(IQP,2,ISAT)=PACCL(JQP,2,ISAT)/DENOM
                         PXDDEX(IQP,3,ISAT)=PACCL(JQP,3,ISAT)/DENOM
                         IQP=IQP+1
                       ENDDO
                    ENDIF
                 ENDIF
                     !   LFILLHR(3)
               ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
               IF(LFILLCAC(3)) THEN
                 DDTCAD = DDTCAD + XDDTMP(1:3)
                     !   LFILLCAC(3)
               ENDIF

!     write(6,*)'dbg GACL FLAGS ',LFILLREG(3),LFILLHR(3),
!    .                        LFILLCAC(3)

!***********************************  END ******************************

            !   LCALL(3)
      ENDIF

  250 CONTINUE
!
!  GET THE SATELLITE ID
      IDSATS=ISATID(ISAT)
!     write(6,*)'f: IDSATS ', IDSATS

!>>> jjm 20130313
      ! provide the beta angle
!     if( ICBDGM == 3 )then
          !write(6,'(A)') 'f: call iceatt2'
!         CALL ICEATT2(MJDSEC,FSEC,XTEMP(1),XTEMP(4) )
!     endif
!<<< jjm 20130313


! IF USING VARIABLE AREA MODEL(I.E. BOX-WING FOR TOPEX) COMPUTE S/C ATT.
      IF(LVAREA.OR.LEXTAC.OR.LGPSTL) THEN

! INITIALIZE ACCELERATIONS
        DO 260 I=1,3
        DRGACC(I) = ZERO
        SOLACC(I) = ZERO
        ALBACC(I) = ZERO
        THMACC(I) = ZERO
  260   CONTINUE

      SBFMGS(1,1)=1.D0
      SBFMGS(1,2)=0.D0
      SBFMGS(1,3)=0.D0
      SBFMGS(2,1)=0.D0
      SBFMGS(2,2)=1.D0
      SBFMGS(2,3)=0.D0
      SBFMGS(3,1)=0.D0
      SBFMGS(3,2)=0.D0
      SBFMGS(3,3)=1.D0
        LINATO=.FALSE.
        IF(LTOPEX) THEN
!
        CALL TOPATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &  AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),         &
     &              AA(KCSTHT),NFACE,NMOVE,AA(KTPSTR),AA(KTPSTP),       &
     &              II(KITPMD),AA(KTPFYW),.TRUE.,TOPXAT(1,ISAT),        &
     &              LTPXAT(1,ISAT),DUM,DUM,BDTRUE(1,8),                 &
     &              IDSATS,II(KIDATB),                                  &
     &   AA(KSABIA),AA(KSBTM1),AA(KSBTM2),AA(KYAWBS),II(KISATN),        &
     &   AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,ISATID,         &
     &   ROT,II(KYAWID),ATROT,II(KTPATS))
!
        LINATO=.TRUE.
        ELSE IF(LSPOT2) THEN
        CALL SPOTAT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,                      &
     &              IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2), &
     &              AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,     &
     &              ISATID,ROT,ATROT)
        LINATO=.TRUE.
        ELSE IF(LGPSVA.OR.LGPSTL) THEN
        CALL GPSATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,                      &
     &              IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2), &
     &           AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,II,     &
     &           ISATID,ROT,ATROT,LGPSTL,1,.FALSE.,1,WPU)
        LINATO=.TRUE.
        ELSE IF(LERS1) THEN
        CALL ERSATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &          AA(KCSTHT),NFACE,NMOVE,.TRUE.,                          &
     &          IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2),     &
     &          AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,ISATID,  &
     &          ROT,ATROT)
        LINATO=.TRUE.
        ELSE IF(LMO) THEN
        CALL MGSAT1(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,                      &
     &              IDSATS,                                             &
     &           AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,ISATID, &
     &           BDTRUE(1,9),BDTRUE(1,8),SBF)
        LINATO=.TRUE.
        ELSE IF(LMOC) THEN
        CALL MOCATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &   AA(KCSTHT),NFACE,NMOVE,.TRUE.,IDSATS,                          &
     &   AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,                &
     &              ISATID)
        LINATO=.TRUE.
        ELSE IF(LTDRSA) THEN
        CALL TDRSAT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &  AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),         &
     &  AA(KCSTHT),NFACE,NMOVE,.TRUE.,IDSATS,II(KIDATB),AA(KSABIA),     &
     &  AA(KSBTM1),AA(KSBTM2),AA(KVLOUV),II(KNSTLV),II(KSLVID),         &
     &  AA(KTSLOV), AA,ISATID,ROT,ATROT)
        LINATO=.TRUE.
        ELSE IF(LMAGNA) THEN
        CALL MAGATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &  AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),         &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,ISATID,               &
     &                         II(KITPMD),                              &
     &   AA(KSABIA),AA(KSBTM1),AA(KSBTM2),                              &
     &   AA,ISATID,ROT,ATROT,II)
        LINATO=.TRUE.
        ELSE IF(LGFO) THEN
        CALL GFOATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &   AA(KCSTHT),NFACE,NMOVE,.TRUE.,BDTRUE(1,8),AA,IDSATS,           &
     &   II(KYAWID),ROT,AA(KYAWBS),DUM)
        LINATO=.TRUE.
        ELSE IF(LTRMM) THEN
        CALL TRMMAT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,                      &
     &              IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2), &
     &              AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,II,  &
     &              ISATID,ROT)
        LINATO=.TRUE.
        ELSE IF(LEUVE) THEN
!
        CALL EUVEAT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,                      &
     &              IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2), &
     &              AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,II,  &
     &              ISATID,ROT)
        LINATO=.TRUE.
        ELSE IF(LVCL) THEN
!  VCL being used for ICESat
!       CALL VCLATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
!    &  AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),         &
!    &  AA(KCSTHT),NFACE,NMOVE,.TRUE.,IDSATS,II(KIDATB),AA(KSABIA),     &
!    &  AA(KSBTM1),AA(KSBTM2),AA(KVLOUV),II(KNSTLV),II(KSLVID),         &
!    &  AA(KTSLOV), AA,ISATID,ROT,ATROT)

        CALL ICEATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
        AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),         &
        AA(KCSTHT),NFACE,NMOVE,.TRUE.,IDSATS,II(KIDATB),AA(KSABIA),     &
        AA(KSBTM1),AA(KSBTM2),AA(KVLOUV),II(KNSTLV),II(KSLVID),         &
        AA(KTSLOV), AA,ISATID,ROT,ATROT)
        LINATO=.TRUE.

      ELSE IF(LENVS) THEN
        CALL ENVATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &          AA(KCSTHT),NFACE,NMOVE,.TRUE.,                          &
     &          IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2),     &
     &          AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,ISATID,  &
     &          ROT,ATROT)
        LINATO=.TRUE.
      ELSE IF (LCRYO) THEN
        CALL CRYOATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),          &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &   AA(KCSTHT),NFACE,NMOVE,.TRUE.,BDTRUE(1,8),AA,IDSATS,           &
     &   II(KYAWID),ROT,AA(KYAWBS),DUM)
        LINATO=.TRUE.

      ELSE IF(LHY2A) THEN
        CALL HY2ATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &   AA(KCSTHT),NFACE,NMOVE,.TRUE.,IDSATS,II(KIDATB),               &
     &   AA(KSABIA),AA(KSBTM1),AA(KSBTM2),AA(KVLOUV),II(KNSTLV),        &
     &   II(KSLVID),AA(KTSLOV),AA,ISATID,ROT,ATROT)
        LINATO=.TRUE.

      ELSE IF(LSARAL) THEN
        CALL SARLAT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &   AA(KCSTHT),NFACE,NMOVE,.TRUE.,IDSATS,II(KIDATB),               &
     &   AA(KSABIA),AA(KSBTM1),AA(KSBTM2),AA(KVLOUV),II(KNSTLV),        &
     &   II(KSLVID),AA(KTSLOV),AA,ISATID,ROT,ATROT)
        LINATO=.TRUE.

      ELSE IF (LGRAIL) THEN
        CALL GRLATT(MJDSEC,FSEC,XTEMP(1),XTEMP(4),AA(JBFNRM),           &
     &   AA(JBFNR2),AA(JBFNR3),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
     &          AA(KCSTHT),NFACE,NMOVE,.TRUE.,                          &
     &          IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2),     &
     &          AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,ISATID,  &
     &          ROT,ATROT)
        LINATO=.TRUE.
      ENDIF

      SBFTOD(1,1)=1.D0
      SBFTOD(1,2)=0.D0
      SBFTOD(1,3)=0.D0
      SBFTOD(2,1)=0.D0
      SBFTOD(2,2)=1.D0
      SBFTOD(2,3)=0.D0
      SBFTOD(3,1)=0.D0
      SBFTOD(3,2)=0.D0
      SBFTOD(3,3)=1.D0
! COMPUTE TOD UNIT VECTORS USING EXTERNAL ATTITUDE
        IF(LEXATO) THEN

         CALL EXTATT(MJDSEC,FSEC,                                       &
     &              AA(JBFNRM),AA(JBFNR2),AA(JBFNR3),                   &
     &              AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),                   &
     &              AA(KCSTHT),NFACE,NMOVE,.TRUE.,AA(KEAQAT),           &
     &              II(KEAPPP),MEAMEM,MEAPLA,LINATO,IDSATS,             &
     &              AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),        &
     &              SBFTOD, XTEMP, LSLFSH,AA(KSDINS),AA(KSDIND),        &
     &              AA(KSSDST),AA(KSSDSR),AA(KSSDDG) )

!      write(6,*)' dbg SBFTOD  ',SBFTOD(1,1),SBFTOD(1,2),SBFTOD(1,3)
!      write(6,*)' dbg SBFTOD  ',SBFTOD(2,1),SBFTOD(2,2),SBFTOD(2,3)
!      write(6,*)' dbg SBFTOD  ',SBFTOD(3,1),SBFTOD(3,2),SBFTOD(3,3)


        ELSE IF(LTXPRT .AND. .NOT.LSTART ) THEN

!!!!8/06   WRITE(97) ZERO,ZERO,ZERO,ZERO,ZERO
!!!!       include 4 more zeroes to make this record the same length as
!!!!       as the record written in EXTATT
         WRITE(97) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO
        ENDIF
!
          IF(LDYNACP.OR.LEXATO) THEN
!         COMPUTE THE ROLL PITCH AND YAW ROTATION MATRIX HERE

      IF(NPVAL(IXATUD).GT.0) THEN
      AA(KSRTCH)=XP(1,1,1)
      AA(KSRTCH+MINTIM)=XP(2,1,1)
      AA(KSRTCH+2*MINTIM)=XP(3,1,1)
      AA(KSRTCH+1)=XP(1,1,2)
      AA(KSRTCH+1+MINTIM)=XP(2,1,2)
      AA(KSRTCH+1+2*MINTIM)=XP(3,1,2)
      CALL GETRPY(AA,II,MJDSEC,FSEC,II(KATSAT),II(KKVLAS),AA(KATIME),   &
     &           II(KATRSQ),ISATID,0,ATROT,AA(KATPER),1,ROLL,PITCH,YAW, &
     &           TDIFF,SINWT,COSWT,.FALSE.,.TRUE.,AA(KSRTCH),           &
     &           AA(KSRTCH+1))
      ENDIF

!     write(6,*)' dbg ATROT ',ATROT(1,1),ATROT(1,2),ATROT(1,3)
!     write(6,*)' dbg ATROT ',ATROT(2,1),ATROT(2,2),ATROT(2,3)
!     write(6,*)' dbg ATROT ',ATROT(3,1),ATROT(3,2),ATROT(3,3)

!       write(6,*)' dbg ROLL P Y ', ROLL , PITCH , YAW

                ! IF(LDYNACP...
        ENDIF
                ! IF(LVAREA..
      ENDIF
!
      CALL FNDNUM(ISATID(ISAT),II(KTELEO),NTELEM,IRET)
      IF(IRET.NE.0)  THEN
        IF(LTXPRT.AND. II(KTELEO-1+IRET).EQ.ISATID(ISAT)) THEN

        IF(.NOT.LSTART) THEN
!!!!8/06   WRITE(97) ZERO,ZERO,ZERO,ZERO,ZERO
!!!!       include 4 more zeroes to make this record the same length as
!!!!       as the record written in EXTATT
         WRITE(97) ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO
      ENDIF
      ENDIF
      ENDIF
!
      IF(.NOT.LSDRG) GO TO 275

      IF( LCALL(1))THEN

!   GET DRAG ACCELERATION
!
      RASAT=ATAN2(XTEMP(2),XTEMP(1))
!
! IN SUBROUTINE DRAG, AA(INDVR+8) CONTAINS THE BODY FIXED COORDINATES
! OF THE SATELLITE NEEDED IN THE COMPUTATION OF THE GEODETIC LATITUDE
!
      CALL DRAG(MJDSEC,FSEC,XTEMP,XTEMP(4),RASAT,DRGACC,                &
     &          PXDDEX(1,1,ISAT),VMAT,AA(INDCR),AA(INDVR),LSORB,NEQN,   &
     &          AA(IB0DRG),AA(IBDRG),AA(ISTDRG),LL(ILAJDP),AA(INDVR+8), &
     &          AA,II,LL,ISATID(ISAT))

!       write(6, 12348) DRGACC
!12348 FORMAT(' DRAG ACCEL',3D25.16)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(1)) THEN
              DXDD(1)=DXDD(1)+DRGACC(1)
              DXDD(2)=DXDD(2)+DRGACC(2)
              DXDD(3)=DXDD(3)+DRGACC(3)
                 !    LFILLREG(1)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF(LFILLHR(1)) THEN
              DDTHR_TOD = DDTHR_TOD + DRGACC(1:3)
                 !    LFILLHR(1)
           ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
           IF(LFILLCAC(1)) THEN
              DDTCAD = DDTCAD + DRGACC(1:3)
                 !    LFILLCAC(1)
           ENDIF

!     write(6,*)'dbg DRAG FLAGS ',LFILLREG(1),LFILLHR(1),
!    .                        LFILLCAC(1)

!***********************************  END ******************************

            !    LCALL(1)   !%ACC
      ENDIF

  275 CONTINUE
!
!-----------------------------------------------------------------
!
!
      IF(nlocal .gt. 0 .and. LCALL(10).AND.IOPTLG.LT.3) THEN

!        ....GET LOCAL GRAVITY ANOMALY ACCELERATION
!
! XDDTMP temporary array for the three components of acceleration
!        FCT in GRANOM
! ANOMLY gravity anomaly values to get from aa(ipval(ixlocg))
! AREALG array containing the area of sub-blocks should be fixed
!        integg*nlocal(9*nlocal)
! SINCOS sines and cosines of latitude and longitude for each sub-block
!        In IIS two arrays are loaded GRLLAT for the latitude and GRLLON
!        for the longitude of the master block. SINCOS needs to be
!        allocated and loaded in IIS.
! PARTLG direct partials for each block (NLOCALx3)
! CONSTR contains constraints information
! SATLAT cosine of latitude of the sub-satellite point
! SATLON sine of latitude of the sub-satellite point
! AA(INDV3+3) various satellite related earth fixed quantities.
!
! TO GET SATLAT AND SATLON CALL PLHOUT
      XPV(1)=AA(INDVR+8)
      XPV(2)=AA(INDVR+9)
      XPV(3)=AA(INDVR+10)
      CALL PLHOUT(XPV,PHI,COSPHI,SINPHI,XLAMB,COSLAM,SINLAM,H,DUMAR,    &
     &.TRUE.,.FALSE.)
      SATLAT(1)=SINPHI
      SATLAT(2)=COSPHI
      SATLON(1)=SINLAM
      SATLON(2)=COSLAM
!
      CALL GRANOM(XDDTMP,NEQN,PXDDEX(1,1,ISAT),JSALG,AA(INDVR+3),       &
     &            AA(KSINCO),AA(KSINCO+NSUBLG),AA(KSINCO+2*NSUBLG),     &
     &            AA(KAERLG),AA(KPARLG),II(KMAPLG))
!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(10)) THEN
              DXDD(1)=DXDD(1)+XDDTMP(1)
              DXDD(2)=DXDD(2)+XDDTMP(2)
              DXDD(3)=DXDD(3)+XDDTMP(3)
                 !   LFILLREG(10)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF( LFILLHR(10)) THEN
              DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 !  LFILLHR(10)
           ENDIF

!***********************************  END ******************************

            !  nlocal .gt. 0 .and. LCALL(10) .AND.IOPTLG.LT.3
      ENDIF

!-----------------------------------------------------------------
!
! ONLY FOR EARTH ORBITING SATELLITES
      IF(ICBDGM.NE.3) GO TO 255
      IF(.NOT.LYARK)  GO TO 255

      LAPPYA=.FALSE.
      IF( LCALL(17) ) THEN

      DO JJ=1,NYACRD
      IF(ISATID(ISAT).EQ.II(KYSAT-1+JJ)) LAPPYA=.TRUE.
      ENDDO

! DEBUG
!     WRITE(6,*)'F dbg LAPPYA ISATID II(KYSAT) ',LAPPYA,ISATID,II(KYSAT)
! DEBUG

      IF(.NOT.LAPPYA) GO tO 255

      IKEPLR=(ISAT-1)*6+KCKEP
      SMEANM=SQRT(AA(IKEPLR)**3/GM)
      CALL YRKVSK(MJDSEC,FSEC,XTEMP(1),XTEMP(4),XDDTMP,                 &
     &            BDTRUE(1,8),ISATID,AA,II)

!      write(6, 12389) XDDTMP
12389 FORMAT(' YRKVSK ACCEL',3D25.16)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(17)) THEN
             DXDD(1)=DXDD(1)+XDDTMP(1)
             DXDD(2)=DXDD(2)+XDDTMP(2)
             DXDD(3)=DXDD(3)+XDDTMP(3)
                 !    LFILLREG(17)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF( LFILLHR(17)) THEN
             DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 !    LFILLHR(17)
           ENDIF

!***********************************  END ******************************

            !    LCALL(17)
      ENDIF
  255 CONTINUE
!
      NBURN=NPVAL(IXBURN)/25
      CALL FBURN(TIME,ISATIX,NEQN,NBURN,II(KBRAX1),II(KBRAX2),          &
     &           II(KBRAX3),AA(KPRMV),PXDDEX(1,1,ISAT),LSBURN,IPSBRN,   &
     &           LFBURN,XDDBRN,AA(KA1UT),SSTEP)
!
! XDDBRN IS IN J2000 COORDINATE SYSTEM WHERE IT MAY BE USED TO RESET
! INTEGRATION SUMS IN THE VARIATIONAL EQUATIONS
! SUBROUTINE F NEEDS IT IN TOD (ESPECIALLY IF HIGH RATE)
      BRNTOD(1)=RMI(1)*XDDBRN(1)+RMI(4)*XDDBRN(2)+RMI(7)*XDDBRN(3)
      BRNTOD(2)=RMI(2)*XDDBRN(1)+RMI(5)*XDDBRN(2)+RMI(8)*XDDBRN(3)
      BRNTOD(3)=RMI(3)*XDDBRN(1)+RMI(6)*XDDBRN(2)+RMI(9)*XDDBRN(3)
!
      IF( LFILLREG(18)) THEN
        DXDD(1)=DXDD(1)+BRNTOD(1)
        DXDD(2)=DXDD(2)+BRNTOD(2)
        DXDD(3)=DXDD(3)+BRNTOD(3)
!    LFILLREG(18)
      ENDIF
!
!   LFILLHR = FILL THE HIGH RATE ARRAY
      IF( LFILLHR(18)) THEN
        DDTHR_TOD = DDTHR_TOD + BRNTOD(1:3)
!    LFILLHR(18)
      ENDIF
!
      IF(NPVAL(IXRELP).GT.0) GO TO 300
      IF(.NOT.LGRELE) GO TO 300

      IF( LCALL(14) ) THEN
!
!
!   GET GENERAL RELATIVISTIC POINT MASS (PLANET FRAME)
! The Relativistic effect for the spacecraft is now computed by
! REL_PM_ACC.f IF(NPVAL(IXRELP).GT.0)

!   GET GENERAL RELATIVISTIC POINT MASS (PLANET FRAME)
!
      CALL FEAREL(XTEMP,R,RSQ,XDDTMP,AA(KPRMV),AA(KPRMV0),              &
     &            AA(KPRMVC),PXDDEX(1,1,ISAT),NEQN,II(KLTMSC),ISATID)

!       write(6, 12388) XDDTMP
!12388 FORMAT(' FEAREL ACCEL',3D25.16)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(14)) THEN
             DXDD(1)=DXDD(1)+XDDTMP(1)
             DXDD(2)=DXDD(2)+XDDTMP(2)
             DXDD(3)=DXDD(3)+XDDTMP(3)
                 !    LFILLREG(14)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF( LFILLHR(14)) THEN
             DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 !    LFILLHR(14)
           ENDIF

!***********************************  END ******************************

            !    LCALL(14)
      ENDIF


!     NEW YUKAWA TYPE FORCE
!
      IF(NPVAL(IXLNTA).GT.0 .AND.  LCALL(15)  ) THEN

        CALL RELFRC(XTEMP,R,RSQ,XDDTMP,AA(KPRMV),AA(KPRMV0),            &
     &              AA(KPRMVC),PXDDEX(1,1,ISAT),NEQN,AA(KLTAR),         &
     &              II(KLTASC),ISATID)

!        write(6, 82388) XDDTMP
82388   FORMAT(' RELFRC ACCEL',3D25.16)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(15)) THEN
             DXDD(1)=DXDD(1)+XDDTMP(1)
             DXDD(2)=DXDD(2)+XDDTMP(2)
             DXDD(3)=DXDD(3)+XDDTMP(3)
                 !    LFILLREG(15)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF( LFILLHR(15)) THEN
             DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 !    LFILLHR(15)
           ENDIF

!***********************************  END ******************************

            ! NPVAL(IXLNTA).GT.0 .AND.  LCALL(15)     !%ACC
      ENDIF
!
  300 CONTINUE
!


!*********************************************************************
      IF(NPVAL(IXPMGM).GT.0 .AND. LCALL(16) ) THEN
!
!     ....COMPUTE GRAVITY EFFECTS OF PLANETARY MOONS
!
!       WRITE(6,*) 'F: CALL MNGRAV '
!      WRITE(6,*) 'F: dxdd before MNGRAV ', dxdd
!      WRITE(6,*) 'F: xtemp before MNGRAV ', xtemp
      CALL MNGRAV(MJDSEC,FSEC,XTEMP,XDDTMP,VMAT,PXDDEX(1,1,ISAT),       &
     &            NEQN,IDSATS,AA,II,LL)

!      WRITE(6,*) 'F: XDDTMP FROM MNGRAV ', XDDTMP

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(16)) THEN
             DXDD(1)=DXDD(1)+XDDTMP(1)
             DXDD(2)=DXDD(2)+XDDTMP(2)
             DXDD(3)=DXDD(3)+XDDTMP(3)
                 !    LFILLREG(16)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF(LFILLHR(16)) THEN
             DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 !    LFILLHR(16)
           ENDIF

!***********************************  END ******************************
!
            !!  NPVAL(IXPMGM).GT.0 .AND. LCALL(16)
      ENDIF

!*********************************************************************
!
!  Jump to 350 IF THIS RUN IS NOT the one for Phobos (Deimos) Orbit
!

       IF(IDSATS.NE.401.AND.IDSATS.NE.402)GO TO 350


!*********************************************************************
        IF( LCALL(13) ) THEN

!      CALCULATE Phobos (Deimos) accel. relative to Mars mass center
!      from its 4X4 gravity field itself.
!
       XDDTMP(1)=0.D0
       XDDTMP(2)=0.D0
       XDDTMP(3)=0.D0
!  Phobos : KPD=1 , Deimos : KPD=2
       IF(IDSATS.EQ.401)KPD=1
       IF(IDSATS.EQ.402)KPD=2
       WRITE(18,301)MJDSEC
  301 FORMAT(1X,I10)
!
! CALL PDPNCN to FILL TRANSFORMATION MATRICES PDTOJ2(3,3)
!
      CALL PDPNCN(KPD,MJDSEC,FSEC,ALPHA,ADOT,DELTAL,DDOT,W,WDOT)
!
!       WRITE(6,*) 'F: CALL PDGRAV'
      CALL PDGRAV(KPD,AA(KXMASS),XTEMP,AA(KXNRMZ),AA(KXNRMC),AA(KXM),   &
     &            XDDTMP)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF( LFILLREG(13)) THEN
              DXDD(1)=DXDD(1)+XDDTMP(1)
              DXDD(2)=DXDD(2)+XDDTMP(2)
              DXDD(3)=DXDD(3)+XDDTMP(3)
                 !   LFILLREG(13)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF( LFILLHR(13)) THEN
             DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                 !   LFILLHR(13)
           ENDIF

!***********************************  END ******************************

            !   LCALL(13)
      ENDIF


  350 CONTINUE
!
!*********************************************************************
!
!   GET SOLAR RADIATION ACCELERATION

      IF(.NOT.LSSRD) GO TO 400

        IF( LCALL(2)) THEN
      do jj=1,3
      do kk=1,6
      tempv(jj,kk)=vmat(jj,kk)
      enddo
      enddo

! *** CHECK WHICH SOLAR RADIATION MODEL TO USE
      IF(LROCK4.OR.LTUMSOL) GO TO 51515
!
         CALL SOLRD(MJDSEC,FSEC,XTEMP,XP(1,ISAT,1),                     &
     &           AA(INDVR+3),AA(INDVR+4),AA(KTHG),                      &
     &           AA(IAPLM),AA(IAPGM),LSORB,NEQN,SOLACC,VMAT,            &
     &           PXDDEX(1,1,ISAT),AA(ISTSRD),LL(ILSTSN),II(INSTRN),     &
     &           II(INDARK),LL(ILAJSP),AA,II,ISATID(ISAT),ALAPGM,       &
     &           RATIOP)
!     INDSH=1
!     if(SOLACC(1).eq.0.d0) INDSH=0
      sev1=SOLACC(1)
      sev2=SOLACC(2)
      sev3=SOLACC(3)
!     write(6,*)'dbg LJBUS LJPAN LJTRR ',LJBUS,LJPAN,LJTRR
!     write(6,*)'dbg F LUGFO ',LUGFO
!     write(6,*)'dbg F LENVI ',LENVI
!       LJASON1=(IDSATS.EQ.1055001.AND.LJBUS.OR.LJPAN)

!     LJBUS/LJPAN NOW HAVE 50 LOGICALS (UP TO 5 SATELLITES) FOR UCL MODEL

      Lsatucl=.FALSE.
      NUCL=1
      DO WHILE(.NOT.Lsatucl)
         IF(IDSATS.EQ.IUCLSAT(NUCL)) THEN
           Lsatucl=.TRUE.
           EXIT
         ENDIF
         NUCL=NUCL+1
         IF(NUCL.GT.50) EXIT
      END DO

      IF(Lsatucl) THEN
         LJASON1=(LJBUS(NUCL).OR.LJPAN(NUCL))
       END IF

       LGFO1=(IDSATS.EQ.9800701.AND.LUGFO)
       LENV1=(IDSATS.EQ.2009001.AND.LENVI)
       IF(LJASON1.OR.LGFO1.OR.LENV1) THEN
       SOLACC(1)=0.D0
       SOLACC(2)=0.D0
       SOLACC(3)=0.D0
       PXDDEX(JSASRD(1),1,ISAT)=0.D0
       PXDDEX(JSASRD(1),2,ISAT)=0.D0
       PXDDEX(JSASRD(1),3,ISAT)=0.D0
       ENDIF
! GET THE CR COEFFICIENTS. ONE CAN REFER TO INITI TO FIND THE
! RELATION OF AA(IAPGM) AND AA(IAPLM). THIS WILL MAKE THE CR
! FOR MULTIPLE SATELLITES IN A RUN FOR THE UCL MODEL.
        SCALU=AA(IAPGM)/AA(IAPLM)

       GO TO 61616

51515 CONTINUE

        IF(LROCK4)                                                      &
     &   CALL SOLRK4(MJDSEC,FSEC,XTEMP,R,RSQ,AA(IAPLM),AA(IAPGM),       &
     &            LSORB,NEQN,SOLACC,PXDDEX(1,1,ISAT),AA(ISTSRD),        &
     &            LL(ILAJSP),LL(ILSTSN),II(INSTRN),II(INDARK),AA,II,LL, &
     &            ISATID,ALAPGM,VMAT)

        IF(LTUMSOL)                                                     &
     &   CALL TUMSOLRD(MJDSEC,FSEC,XTEMP,R,RSQ,AA(IAPLM),AA(IAPGM),     &
     &            LSORB,NEQN,SOLACC,PXDDEX(1,1,ISAT),AA(ISTSRD),        &
     &            LL(ILAJSP),LL(ILSTSN),II(INSTRN),II(INDARK),AA,II,LL, &
     &            ISATID,ALAPGM,VMAT,II(KGPSID),LL(KLAVOI))

61616 CONTINUE

!        write(6, 12349) SOLACC
!12349 FORMAT('TUM ACC',3D25.16)

!
!     write(6,*)' '
!     call ymdhms(MJDSEC,FSEC,IYMD,IHM,SECS,1)
!     IF(IYMD.GT.1000000)IYMD=IYMD-1000000
!     WRITE(6,*)'dbg IYMD IHM SEC  ',IYMD,IHM,SECS

!    JASON RADIATION PRESSURE AND THERMAL RE-RADIATION MODELS
       IF(LJASON1.OR.LGFO1.OR.LENV1)THEN
      do jj=1,3
      do kk=1,6
      vmat(jj,kk)=tempv(jj,kk)
      enddo
      enddo

! debug
!  ROT is the total rotation matrix fron SBF to TOD
!     write(6,*)' dbg ROT at UCL ',ROT(1),ROT(4),ROT(7)
!     write(6,*)' dbg ROT at UCL ',ROT(2),ROT(5),ROT(8)
!     write(6,*)' dbg ROT at UCL ',ROT(3),ROT(6),ROT(9)
! debug

! *** GET SUN COORDINATES IN TOD
      XSN1_TOD=XMNSNS(1,2)*XMNSNS(4,2)
      XSN2_TOD=XMNSNS(2,2)*XMNSNS(4,2)
      XSN3_TOD=XMNSNS(3,2)*XMNSNS(4,2)
!
! *** GET SUN COORDINATES INTO BFS
!
      XSN1_BFS=ROT(1)*XSN1_TOD + ROT(2)*XSN2_TOD + ROT(3)*XSN3_TOD
      XSN2_BFS=ROT(4)*XSN1_TOD + ROT(5)*XSN2_TOD + ROT(6)*XSN3_TOD
      XSN3_BFS=ROT(7)*XSN1_TOD + ROT(8)*XSN2_TOD + ROT(9)*XSN3_TOD
!
! debug
!     write(6,*)' dbg F SUN TOD ',XSN1_TOD,XSN2_TOD,XSN3_TOD
!     write(6,*)' dbg F SUN MAG ',XMNSNS(4,2)
! debug
!
! *** GET SUN LAT LON IN BFS
!
      XYSN=XSN1_BFS*XSN1_BFS+XSN2_BFS*XSN2_BFS
      SNLON=ATAN2(XSN2_BFS,XSN1_BFS)
      SNLAT=ATAN2(XSN3_BFS,SQRT(XYSN))
      SLON=SNLON/DEGRAD
      SLAT=SNLAT/DEGRAD

! debug
!     WRITE(6,*)'dbg SUNLON SUNLAT(deg) ',SLON,SLAT
! debug
!
! *** GET DISTANCE SUN-PROBE IN TOD
!
      D1=XTEMP(1)-XSN1_TOD
      D2=XTEMP(2)-XSN2_TOD
      D3=XTEMP(3)-XSN3_TOD
      distance=SQRT(D1**2.D0+D2**2.D0+D3**2.D0)

!
! *** GET SUN-PROBE UNIT VECTOR IN TOD
!
      S1=D1/distance
      S2=D2/distance
      S3=D3/distance
!     write(6,*)' dbg SUNUVTOD',s1,s2,s3

! debug
!     write(6,*)' dbg SUN-PROBE vector in TOD ',S1,S2,S3
! debug
!
! *** ROTATE SUN-PROBE UNIT VECTOR INTO SBF
!
      XSN1=ROT(1)*S1 + ROT(2)*S2 + ROT(3)*S3
      XSN2=ROT(4)*S1 + ROT(5)*S2 + ROT(6)*S3
      XSN3=ROT(7)*S1 + ROT(8)*S2 + ROT(9)*S3
!     write(6,*)' dbg ROT ',ROT(1),ROT(2),ROT(3)
!     write(6,*)' dbg ROT ',ROT(4),ROT(5),ROT(6)
!     write(6,*)' dbg ROT ',ROT(7),ROT(8),ROT(9)

! debug
!     write(6,*)'dbg SUN-PROBE ',XSN1,XSN2,XSN3
! debug
       ENDIF

!       IF(IDSATS.EQ.1055001.AND.LJBUS)THEN
! LJBUS NOW HAS 5 LOGICALS FOR UP TO 5 SATELLITES
       IF(IDSATS.EQ.IUCLSAT(NUCL).AND.LJBUS(NUCL)) THEN
       CALL UCL_JASON_bus(AA(KXUGRD),AA(KYUGRD),AA(KZUGRD),XBUS,YBUS,  &
     &                    ZBUS,SLAT,SLON)

! debug
!     write(6,*)'dbg acc from BUS',XBUS,YBUS,ZBUS
! CONVERT BUS ACCELS FROM BFS TO TOD
       X1=ROT(1)*XBUS + ROT(4)*YBUS + ROT(7)*ZBUS
       Y1=ROT(2)*XBUS + ROT(5)*YBUS + ROT(8)*ZBUS
       Z1=ROT(3)*XBUS + ROT(6)*YBUS + ROT(9)*ZBUS
!     write(6,*)'dbg TOD acc from BUS',X1,Y1,Z1
! debug

       ENDIF
! GFO
       IF(LGFO1) THEN
       CALL UCL_GFO_bus(AA(KXGGRD),AA(KYGGRD),AA(KZGGRD),XBUS,YBUS,  &
     &                    ZBUS,SLAT,SLON)
!
! debug
!     write(6,*)'dbg acc from BUS',XBUS,YBUS,ZBUS
! CONVERT BUS ACCELS FROM BFS TO TOD
!      X1=ROT(1)*XBUS + ROT(4)*YBUS + ROT(7)*ZBUS
!      Y1=ROT(2)*XBUS + ROT(5)*YBUS + ROT(8)*ZBUS
!      Z1=ROT(3)*XBUS + ROT(6)*YBUS + ROT(9)*ZBUS
!     write(6,*)'dbg TOD acc from BUS',X1,Y1,Z1
       ENDIF
!
!       IF(IDSATS.EQ.1055001.AND.LJPAN)THEN
! LJPAN NOW HAS 5 LOGICALS (FOR 5 POSSIBLE SATELLITES)
       IF(IDSATS.EQ.IUCLSAT(NUCL).AND.LJPAN(NUCL)) THEN

! CALL THE PANEL MODEL SUBROUTINE FOR BOTH SIDES
!      bfn1=AA(JBFNRM+6)
!      bfn2=AA(JBFNR2+6)
!      bfn3=AA(JBFNR3+6)
       tdn1=AA(JTDNRM+6)
       tdn2=AA(JTDNR2+6)
       tdn3=AA(JTDNR3+6)
! convert normal from TOD to SBF
       bfn1=ROT(1)*tdn1 + ROT(2)*tdn2 + ROT(3)*tdn3
       bfn2=ROT(4)*tdn1 + ROT(5)*tdn2 + ROT(6)*tdn3
       bfn3=ROT(7)*tdn1 + ROT(8)*tdn2 + ROT(9)*tdn3
!      write(6,*)' dbg xsn ',xsn1,xsn2,xsn3
!      write(6,*)' dbg bfn ',bfn1,bfn2,bfn3
! compute angle theta according to the UCL model
       prod=ACOS(-xsn1*bfn1-xsn2*bfn2-xsn3*bfn3)
       thetanew=prod/degrad

! angle theta from TOPATT
       costheta=AA(KCSTHT+6)
       sintheta=SQRT(1-costheta**2.D0)
       theta=ACOS(costheta)
       dtheta=theta/degrad
!      write(6,*)' dbg costheta ',costheta

! debug
       dth=dtheta-thetanew
!      write(6,*)' dbg theta deg', dtheta ,thetanew,dth
!       write(6,*)' dbg unit vector normal to the sun facing panel ',  &
!    &  bfn1,bfn2,bfn3
! debug
       ACCTRR=0.D0
!       IF(LJTRR) THEN
       IF(LJTRR(NUCL)) THEN
       DST=distance
       CALL UCL_JASON_panel_TRR(DST,theta,ACCTRR)
!      write(6,*)' dbg called TRR acc in BF',ACCTRR
       ENDIF
!
       US1=XSN1
       US2=XSN2
       US3=XSN3
       CALL UCL_JASON_panel(XPR,YPR,ZPR,SNLON,SNLAT,SIDE(1),dummy,  &
     &                      theta,bfn1,bfn2,bfn3,US1,US2,US3,ACCTRR)
       XPL=XPR
       YPL=YPR
       ZPL=ZPR
!
!      US1=XSN1
!      US2=XSN2
!      US3=XSN3
!      CALL UCL_JASON_panel(XPL,YPL,ZPL,SNLON,SNLAT,SIDE(2),dummy,  &
!    &                      theta,bfn1,bfn2,bfn3,US1,US2,US3,ACCTRR)

! debug
!     write(6,*)'dbg acc right panel bf',XPR,YPR,ZPR
!     write(6,*)'dbg acc left  panel bf',XPL,YPL,ZPL
! CONVERT PANEL ACCELS TO TOD FROM BFS
!      X1=ROT(1)*XPR + ROT(4)*YPR + ROT(7)*ZPR
!      Y1=ROT(2)*XPR + ROT(5)*YPR + ROT(8)*ZPR
!      Z1=ROT(3)*XPR + ROT(6)*YPR + ROT(9)*ZPR
! debug

!     write(6,*)'dbg TOD acc left  panel',X1,Y1,Z1
!     write(6,*)'dbg TOD acc right panel',X1,Y1,Z1
!     write(6,*)'total dbg TOD from  panel',X1*2,Y1*2,Z1*2

       ENDIF


      IF(LJASON1)THEN
            IF(ABS(theta)<PI/2.0)THEN
!       write(6,*)' dbg calling jason panel all ',XBUS,YBUS,ZBUS
      CALL UCL_JASON_all(XBUS,YBUS,ZBUS,                                &
     &                   XPR,YPR,ZPR,                                   &
     &                   XPL,YPL,ZPL,                                   &
     &                   distance, SOLACC,MJDSEC,FSEC,RATIOP,           &
     &                   PXDDEX(1,1,ISAT),ROT,JSASRD(1),NEQN,           &
     &                   AA(KPRMVC),SCALU)

! debug
!     write(6,*)'dbg TOTAL ACCEL BF ',SOLACC
! GET TOTAL ACCEL TO TOD FROM BFS
       X1=ROT(1)*SOLACC(1) + ROT(4)*SOLACC(2) + ROT(7)*SOLACC(3)
       Y1=ROT(2)*SOLACC(1) + ROT(5)*SOLACC(2) + ROT(8)*SOLACC(3)
       Z1=ROT(3)*SOLACC(1) + ROT(6)*SOLACC(2) + ROT(9)*SOLACC(3)
!     write(6,*)'dbg TOTAL TOD ACCEL ',X1,Y1,Z1
! debug

! Temporarilly Off
!      IF(IDSATS.EQ.1055001.AND.LJTRR) THEN
!       write(6,*)' dbg calling jason panel TRR '
!      write(6,*)' dbg distance ',distance
!      DST=distance
!      CALL UCL_JASON_panel_TRR(DST,theta,ACCTRR)
!      SOLACC(3)=SOLACC(3)+ACCTRR
!     write(6,*)'dbg ACCTRR ',ACCTRR
!      ENDIF
! debug
! GET TOTAL ACCEL TO TOD FROM BFS
!      X1=ROT(1)*SOLACC(1) + ROT(4)*SOLACC(2) + ROT(7)*SOLACC(3)
!      Y1=ROT(2)*SOLACC(1) + ROT(5)*SOLACC(2) + ROT(8)*SOLACC(3)
!      Z1=ROT(3)*SOLACC(1) + ROT(6)*SOLACC(2) + ROT(9)*SOLACC(3)
!     write(6,*)'dbg TOTAL TOD ACCEL ',X1,Y1,Z1
! debug
       SOLACC(1)=X1*SCALU
       SOLACC(2)=Y1*SCALU
       SOLACC(3)=Z1*SCALU

!     write(6,*)'dbg SOLACC',SOLACC
            ENDIF

       ENDIF
!
       IF(LGFO1) THEN
!       write(6,*)' dbg calling gfo all ',SCALU,JSASRD(1)
       CALL UCL_GFO_all(XBUS,YBUS,ZBUS,RATIOP,DISTANCE,SOLACC,        &
     &                 PXDDEX(1,1,ISAT),ROT,JSASRD(1),NEQN,           &
     &                 AA(KPRMVC),SCALU)

! debug
      write(6,*)'dbg TOTAL ACCEL BF ',SOLACC
! GET TOTAL ACCEL TO TOD FROM BFS
       X1=ROT(1)*SOLACC(1) + ROT(4)*SOLACC(2) + ROT(7)*SOLACC(3)
       Y1=ROT(2)*SOLACC(1) + ROT(5)*SOLACC(2) + ROT(8)*SOLACC(3)
       Z1=ROT(3)*SOLACC(1) + ROT(6)*SOLACC(2) + ROT(9)*SOLACC(3)
!     write(6,*)'dbg TOTAL TOD ACCEL ',X1,Y1,Z1
! debug
       SOLACC(1)=X1*SCALU
       SOLACC(2)=Y1*SCALU
       SOLACC(3)=Z1*SCALU
       ENDIF
!
! UCL MODEL FOR ENVISAT
      IF(LENV1) THEN
        SUN_PROBE(1) = S1
        SUN_PROBE(2) = S2
        SUN_PROBE(3) = S3
        CALL UCL_ENVISAT(MJDSEC,FSEC,SLON,SLAT,RATIOP,AA(KXEGRD),   &
     &          AA(KYEGRD),AA(KZEGRD),ROT,SUN_PROBE,distance,SOLACC,&
     &          PXDDEX(1,1,ISAT),JSASRD(1),NEQN,AA(KPRMVC),SCALU,  &
     &          LSASRD(1))
        SOLACC(1) = SOLACC(1)*SCALU
        SOLACC(2) = SOLACC(2)*SCALU
        SOLACC(3) = SOLACC(3)*SCALU
      ENDIF
!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
            IF( LFILLREG(2)) THEN
             DXDD(1)=DXDD(1)+SOLACC(1)
             DXDD(2)=DXDD(2)+SOLACC(2)
             DXDD(3)=DXDD(3)+SOLACC(3)
                  ! LFILLREG(2)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
               IF( LFILLHR(2)) THEN
                 DDTHR_TOD = DDTHR_TOD + SOLACC(1:3)
                     !  LFILLHR(2)
               ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
               IF( LFILLCAC(2)) THEN
                 DDTCAD = DDTCAD + SOLACC(1:3)
                     !  LFILLCAC(2)
               ENDIF

!     write(6,*)' dbg SOL FLAGS ',LFILLREG(2),LFILLHR(2),
!    .                        LFILLCAC(2)

!***********************************  END ******************************

              !  LCALL(2)
        ENDIF
!
!     ....ALBEDO ACCELERATION
!

      IF(ALBON.LE.ZERO) GOTO 400

        IF( LCALL(6)) THEN

        IF(.NOT.LTUMALB) THEN

          CALL ALBEDO(AA(INDVR+8),ALBACC,R,RSQ,COSTG,                   &
     &            SINTG,MJDSEC,FSEC,ALAPGM,                             &
     &            PXDDEX(1,1,ISAT),NEQN,AA(KSVECT),AA(KSFLUX),          &
     &            AA(KADIST),II(KNSEG),II(KALCAP),II(KEMCAP),           &
     &            AA(KGEOAN),AA(KACS),AA(KECS),AA(KSOLNA),AA(KSOLNE),   &
     &            AA(KALBCO),AA(KEMMCO),AA,II,ISATID(ISAT),II(KPNALB))

          ELSE

         CALL TUMALBEDO(MJDSEC,FSEC,ALBACC,XTEMP,ISATID,AA,II,LL,&
     &                   COSTG,SINTG)

          ENDIF


!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
             IF( LFILLREG(6)) THEN
               DXDD(1)=DXDD(1)+ALBACC(1)
               DXDD(2)=DXDD(2)+ALBACC(2)
               DXDD(3)=DXDD(3)+ALBACC(3)
                   !  LFILLREG(6)
             ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
             IF( LFILLHR(6)) THEN
               DDTHR_TOD = DDTHR_TOD + ALBACC(1:3)
                    !  LFILLHR(6)
             ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
             IF( LFILLCAC(6)) THEN
               DDTCAD = DDTCAD + ALBACC(1:3)
                    !  LFILLCAC(6)
             ENDIF

!***********************************  END ******************************

              !  LCALL(6)
        ENDIF

  400 CONTINUE
      If(LEXTHC.AND.LCALL(4).AND..NOT.LTOPEX) THEN
      CALL GENTHM(MJDSEC,FSEC,AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),NFACE,   &
     &            AA(KTATHM),AA(KTAPPP),AA(KTANMP),II(KTAMJS),          &
     &            AA(KTAFSS),AA(KTAINS),AA(JBWARE),II(KTASID),IDSATS,   &
     &            THMACC,AA,II)
!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF(LFILLREG(4)) THEN
            DXDD(1)=DXDD(1)+THMACC(1)
            DXDD(2)=DXDD(2)+THMACC(2)
            DXDD(3)=DXDD(3)+THMACC(3)
                  ! LFILLREG(4)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF(LFILLHR(4)) THEN
            DDTHR_TOD = DDTHR_TOD + THMACC(1:3)
                  ! LFILLHR(4)
           ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
           IF(LFILLCAC(4)) THEN
            DDTCAD = DDTCAD + THMACC(1:3)
                  ! LFILLCAC(4)
           ENDIF
      ENDIF
!
! THERMAL IMBALANCE MODEL (CURRENTLY TOPEX SPECIFIC)


      IF(LTOPEX .and.  LCALL(4)) THEN

       CALL TOPTHM(XTEMP(1),AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),           &
     &             AA(JBWARE),AA(JBWEMS),AA(JBWTPA),                    &
     &             AA(JBWTPC),AA(JBWTMD),AA(JBWTMF),AA(JBWTHX),THMACC,  &
     &             PXDDEX(1,1,ISAT),JARADJ,JEMADJ,JTAADJ,JTCADJ,        &
     &             JTDADJ,JTFADJ,JTXADJ,II(JARUA),                      &
     &             II(JEMUA) ,II(JTAUA), II(JTCUA) ,II(JTDUA),          &
     &             II(JTFUA), II(JTXUA),AA(KCSTHT),NFACE,NEQN,LSORB)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF(LFILLREG(4)) THEN
            DXDD(1)=DXDD(1)+THMACC(1)
            DXDD(2)=DXDD(2)+THMACC(2)
            DXDD(3)=DXDD(3)+THMACC(3)
                  ! LFILLREG(4)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF(LFILLHR(4)) THEN
            DDTHR_TOD = DDTHR_TOD + THMACC(1:3)
                  ! LFILLHR(4)
           ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
           IF(LFILLCAC(4)) THEN
            DDTCAD = DDTCAD + THMACC(1:3)
                  ! LFILLCAC(4)
           ENDIF

!***********************************  END ******************************

      If(LEXTHC) THEN
      CALL GENTHM(MJDSEC,FSEC,AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),NFACE,   &
     &            AA(KTATHM),AA(KTAPPP),AA(KTANMP),II(KTAMJS),          &
     &            AA(KTAFSS),AA(KTAINS),AA(JBWARE),II(KTASID),IDSATS,   &
     &            THMACC,AA,II)

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
           IF(LFILLREG(4)) THEN
            DXDD(1)=DXDD(1)+THMACC(1)
            DXDD(2)=DXDD(2)+THMACC(2)
            DXDD(3)=DXDD(3)+THMACC(3)
                  ! LFILLREG(4)
           ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
           IF(LFILLHR(4)) THEN
            DDTHR_TOD = DDTHR_TOD + THMACC(1:3)
                  ! LFILLHR(4)
           ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
           IF(LFILLCAC(4)) THEN
            DDTCAD = DDTCAD + THMACC(1:3)
                  ! LFILLCAC(4)
           ENDIF
      ENDIF

       ELSE

       IF(LTXPRT.AND.LVAREA .AND. .NOT. LSTART) THEN
!......WRITE PANEL TEMPERATURES
         WRITE(97) CM999,CM999,CM999,CM999,    &
     &             CM999,CM999,CM999,CM999
!

       ENDIF

      CALL FNDNUM(ISATID(ISAT),II(KTELEO),NTELEM,IRET)
      IF(IRET.NE.0)  THEN
       IF(II(KTELEO-1+IRET).EQ.ISATID(ISAT)) THEN

            IF(LTXPRT.AND..NOT. LSTART)  THEN
         CALL UTCET(.FALSE.,1,MJDSEC,FSEC,FSECU,AA(KA1UT))
         CALL YMDHMS(MJDSEC,FSECU,IYMD,IHM,SEC,1)
         IF(IYMD.LE.999999) GO TO 428
         IYMD=MOD(IYMD,1000000)
         GO TO 430
  428 CONTINUE
  430 CONTINUE
         DIYMD=DBLE(IYMD)
         DIHM= DBLE(IHM)

         WRITE(97) DIYMD,DIHM,SEC,ZERO,    &
     &             ZERO,ZERO,ZERO,ZERO
!
         ENDIF
       ENDIF
      ENDIF

            ! LTOPEX .AND.  LCALL(4)
      ENDIF


! APPLY THERMAL ACCEL. EMITTED FROM  LOUVERS
!      ....line below changed 12/14/93 - jjm
!      ....this is needed for multi-satellite runs to keep louver
!      ....acceleration from being applied to sats other than TOPEX


       IF(IDSATS.EQ.9205201)THEN

          IF(NSTLVS.NE.0 .AND. LCALL(5))THEN
!           IF(NSTLVS.NE.0 )THEN

             CALL TOPLOV(AA(KACMAG),AA(KTSLOV),XDDTMP,II(KNSTLV),IDSATS,&
     &                   II(KSLVID))

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
               IF( LFILLREG(5))THEN
                  DXDD(1)=DXDD(1)+XDDTMP(1)
                  DXDD(2)=DXDD(2)+XDDTMP(2)
                  DXDD(3)=DXDD(3)+XDDTMP(3)
                      !  LFILLREG(5)
               ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
               IF(  LFILLHR(5))THEN
                  DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                      ! LFILLHR(5)
               ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
               IF(  LFILLCAC(5)) THEN
                  DDTCAD = DDTCAD + XDDTMP(1:3)
                      ! LFILLCAC(5)
               ENDIF

!***********************************  END ******************************

               ! NSTLVS.NE.0 .AND. LCALL(5)
        ENDIF

             ! IDSATS.EQ.9205201
      ENDIF
!
!
600   CONTINUE

       IF(LTXPRT.AND.LVAREA .AND. .NOT. LSTART) THEN

! ROTATE ACCELS INTO RCL FRAME (ASSUME ALONGTRACK = VELOCITY)
! USE XDDTMP AND XPV FOR TEMPORARY STORAGE
         RMAG = SQRT(XTEMP(1)**2+XTEMP(2)**2+XTEMP(3)**2)
         VMAG = SQRT(XTEMP(4)**2+XTEMP(5)**2+XTEMP(6)**2)
         DO 425 IPT=1,3
           HCLXYZ(IPT,1) = XTEMP(IPT+3)/VMAG
           HCLXYZ(IPT,3) = XTEMP(IPT)/RMAG
  425    CONTINUE
         HCLXYZ(1,2) = HCLXYZ(2,3)*HCLXYZ(3,1)-HCLXYZ(3,3)*HCLXYZ(2,1)
         HCLXYZ(2,2) = HCLXYZ(3,3)*HCLXYZ(1,1)-HCLXYZ(1,3)*HCLXYZ(3,1)
         HCLXYZ(3,2) = HCLXYZ(1,3)*HCLXYZ(2,1)-HCLXYZ(2,3)*HCLXYZ(1,1)
         XDDTMP(1) = SOLACC(1)*HCLXYZ(1,1)+SOLACC(2)*HCLXYZ(2,1)+       &
     &               SOLACC(3)*HCLXYZ(3,1)
         XDDTMP(2) = SOLACC(1)*HCLXYZ(1,2)+SOLACC(2)*HCLXYZ(2,2)+       &
     &               SOLACC(3)*HCLXYZ(3,2)
         XDDTMP(3) = SOLACC(1)*HCLXYZ(1,3)+SOLACC(2)*HCLXYZ(2,3)+       &
     &               SOLACC(3)*HCLXYZ(3,3)
         XPV(1)    = DRGACC(1)*HCLXYZ(1,1)+DRGACC(2)*HCLXYZ(2,1)+       &
     &               DRGACC(3)*HCLXYZ(3,1)
         XPV(2)    = DRGACC(1)*HCLXYZ(1,2)+DRGACC(2)*HCLXYZ(2,2)+       &
     &               DRGACC(3)*HCLXYZ(3,2)
         XPV(3)    = DRGACC(1)*HCLXYZ(1,3)+DRGACC(2)*HCLXYZ(2,3)+       &
     &               DRGACC(3)*HCLXYZ(3,3)
! WRITE ACCELS TO TELEM FILE
         WRITE(97) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
!        WRITE(97,6000) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
!
         XDDTMP(1) = ALBACC(1)*HCLXYZ(1,1)+ALBACC(2)*HCLXYZ(2,1)+       &
     &               ALBACC(3)*HCLXYZ(3,1)
         XDDTMP(2) = ALBACC(1)*HCLXYZ(1,2)+ALBACC(2)*HCLXYZ(2,2)+       &
     &               ALBACC(3)*HCLXYZ(3,2)
         XDDTMP(3) = ALBACC(1)*HCLXYZ(1,3)+ALBACC(2)*HCLXYZ(2,3)+       &
     &               ALBACC(3)*HCLXYZ(3,3)
!
         XPV(1)    = THMACC(1)*HCLXYZ(1,1)+THMACC(2)*HCLXYZ(2,1)+       &
     &               THMACC(3)*HCLXYZ(3,1)
         XPV(2)    = THMACC(1)*HCLXYZ(1,2)+THMACC(2)*HCLXYZ(2,2)+       &
     &               THMACC(3)*HCLXYZ(3,2)
         XPV(3)    = THMACC(1)*HCLXYZ(1,3)+THMACC(2)*HCLXYZ(2,3)+       &
     &               THMACC(3)*HCLXYZ(3,3)
! WRITE ACCELS TO TELEM FILE
         WRITE(97) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
!        WRITE(97,6000) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
       ENDIF

      CALL FNDNUM(ISATID(ISAT),II(KTELEO),NTELEM,IRET)
      IF(IRET.NE.0)  THEN
        IF(LTXPRT.AND. II(KTELEO-1+IRET).EQ.ISATID(ISAT).AND.&
     &.NOT.LSTART)   THEN

! ROTATE ACCELS INTO RCL FRAME (ASSUME ALONGTRACK = VELOCITY)
! USE XDDTMP AND XPV FOR TEMPORARY STORAGE
         RMAG = SQRT(XTEMP(1)**2+XTEMP(2)**2+XTEMP(3)**2)
         VMAG = SQRT(XTEMP(4)**2+XTEMP(5)**2+XTEMP(6)**2)
         DO 426 IPT=1,3
           HCLXYZ(IPT,1) = XTEMP(IPT+3)/VMAG
           HCLXYZ(IPT,3) = XTEMP(IPT)/RMAG
  426    CONTINUE
         HCLXYZ(1,2) = HCLXYZ(2,3)*HCLXYZ(3,1)-HCLXYZ(3,3)*HCLXYZ(2,1)
         HCLXYZ(2,2) = HCLXYZ(3,3)*HCLXYZ(1,1)-HCLXYZ(1,3)*HCLXYZ(3,1)
         HCLXYZ(3,2) = HCLXYZ(1,3)*HCLXYZ(2,1)-HCLXYZ(2,3)*HCLXYZ(1,1)
         XDDTMP(1) = SOLACC(1)*HCLXYZ(1,1)+SOLACC(2)*HCLXYZ(2,1)+       &
     &               SOLACC(3)*HCLXYZ(3,1)
         XDDTMP(2) = SOLACC(1)*HCLXYZ(1,2)+SOLACC(2)*HCLXYZ(2,2)+       &
     &               SOLACC(3)*HCLXYZ(3,2)
         XDDTMP(3) = SOLACC(1)*HCLXYZ(1,3)+SOLACC(2)*HCLXYZ(2,3)+       &
     &               SOLACC(3)*HCLXYZ(3,3)
         XPV(1)    = DRGACC(1)*HCLXYZ(1,1)+DRGACC(2)*HCLXYZ(2,1)+       &
     &               DRGACC(3)*HCLXYZ(3,1)
         XPV(2)    = DRGACC(1)*HCLXYZ(1,2)+DRGACC(2)*HCLXYZ(2,2)+       &
     &               DRGACC(3)*HCLXYZ(3,2)
         XPV(3)    = DRGACC(1)*HCLXYZ(1,3)+DRGACC(2)*HCLXYZ(2,3)+       &
     &               DRGACC(3)*HCLXYZ(3,3)
! WRITE ACCELS TO TELEM FILE
         WRITE(97) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
!        WRITE(97,6000) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
!
         XDDTMP(1) = ALBACC(1)*HCLXYZ(1,1)+ALBACC(2)*HCLXYZ(2,1)+       &
     &               ALBACC(3)*HCLXYZ(3,1)
         XDDTMP(2) = ALBACC(1)*HCLXYZ(1,2)+ALBACC(2)*HCLXYZ(2,2)+       &
     &               ALBACC(3)*HCLXYZ(3,2)
         XDDTMP(3) = ALBACC(1)*HCLXYZ(1,3)+ALBACC(2)*HCLXYZ(2,3)+       &
     &               ALBACC(3)*HCLXYZ(3,3)
!
         XPV(1)    = THMACC(1)*HCLXYZ(1,1)+THMACC(2)*HCLXYZ(2,1)+       &
     &               THMACC(3)*HCLXYZ(3,1)
         XPV(2)    = THMACC(1)*HCLXYZ(1,2)+THMACC(2)*HCLXYZ(2,2)+       &
     &               THMACC(3)*HCLXYZ(3,2)
         XPV(3)    = THMACC(1)*HCLXYZ(1,3)+THMACC(2)*HCLXYZ(2,3)+       &
     &               THMACC(3)*HCLXYZ(3,3)
! WRITE ACCELS TO TELEM FILE
         WRITE(97) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
!        WRITE(97,6000) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)
         WRITE(6,6000) (XDDTMP(IK),IK=1,3),(XPV(IJ),IJ=1,3)


       ENDIF
       ENDIF

!

      IF(.NOT.LTHDRG) GO TO 450

       IF(  LCALL(7)) THEN

!          THERMAL DRAG COMPUTATION FOR LAGEOS
!          partials for spin axis position now computed - ddr 11/92
           JXEPOC=KXEPOC+(IDGSTN-1)*6
           JCKEP=KCKEP+(IDGSTN-1)*6
           JMJDS0=KMJDS0+IDGSTN-1
           JFSEC0=KFSEC0+IDGSTN-1
!           write(6,*) 'f: call THEDRG  thrmfl ', thrmfl
!           write(6,*) 'f: before call THEDRG  DXDD  ', DXDD
           CALL THEDRG(XDDTMP,AA(JCKEP),II(JMJDS0),AA(JFSEC0),MJDSEC,   &
     &            FSEC,XTEMP,PXDDEX(1,1,ISAT),NEQN)


!          ....if thrmfl = 0 , model is not applied
!          ....if thrmfl = 1 , model is applied
!          ....if thrmfl = 2 , partials are calculated but force is not
!          ....               applied
           if( thrmfl .lt. TWO ) then

!**** FILL UP THE APPROPRIATE ARRAYS ACCORDING TO THE RUN SPECIFICATIONS

!   LFILLREG = FILL THE REGULAR XDDOT
              IF(  LFILLREG(7)) THEN
                   DXDD(1)=DXDD(1)+XDDTMP(1)
                   DXDD(2)=DXDD(2)+XDDTMP(2)
                   DXDD(3)=DXDD(3)+XDDTMP(3)
                     !  LFILLREG(7)
              ENDIF

!   LFILLHR = FILL THE HIGH RATE ARRAY
              IF(LFILLHR(7)) THEN
                                                      !%ACC
                DDTHR_TOD = DDTHR_TOD + XDDTMP(1:3)
                    !  LFILLHR(7)
              ENDIF

!   LFILLCAC = FILL THE COMPUTED FORCES ARRAY FOR ACCELEROMETER DATA
              IF(LFILLCAC(7)) THEN
                                                !%ACC
                DDTCAD = DDTCAD + XDDTMP(1:3)
                    !  LFILLCAC(7)
              ENDIF

!***********************************  END ******************************

                 ! thrmfl .lt. TWO
           endif

             !   LCALL(7)
      ENDIF

  450 CONTINUE

!      LHIRATE  L    S    indicates this call is for high rate integrati
!      LDYNACP  L    S    indicates that this call is in a dynamic

!  GET TOTAL SBF TO TOD MATRIX, INCLUDE ROLL PITCH YAW ROTATIONS

      CALL MATPRD(SBFTOD,ATROT,SBFTOD_TOT,3,3,3)

!       write(6,*) 'f:bef DXDD(1:3) ', DXDD(1:3),MJDSEC,FSEC
!       write(6,*) 'f: LDYNACP ', LDYNACP

                           !%ACC
      if( LDYNACP ) then

!       in dynamic accel. period,                         !%ACC
!       remove computed surface accelerations and         !%ACC
!       add in the external accelerometer accelerations   !%ACC

!        write(6,*) 'f:bef DDTHR_TOD(1:3) ', DDTHR_TOD(1:3)
!        write(6,*) 'f:bef DXDD(1:3) ', DXDD(1:3),MJDSEC,FSEC
!        write(6,*) 'f:bef DDTCAD(1:3) ', DDTCAD(1:3)


! CORRECT DDTEXT FOR BIASES
! COMPUTE PARTIALS ROTATE THEM TO TOD AND LOAD IN PXDDEX
! SUBROUTINE ACCELR IS CALLED BOTH FROM F (DYNAMIC MODE) AND THE
! OBSERVATION MODELING ROUTINE ACCSEL (GEOMETRIC MODE)
! INSIDE THE ROUTINE THE FLAG LFORCE REGULATES THE FORCE MODEL COMPUTATI

      CALL ACCELR(AA,II,LL,MJDSEC,FSEC,1,1,1,1,DUM1,ACCTIM,             &
     & ACCPER,NPERAC,AA(KPRMVC),AA(KPRSG0),PXDDEX(1,1,ISAT),            &
     & .FALSE.,LL(KLAVOI),VAL,VAL,XYZDOT,0,1,.TRUE.,NEQN,3,DDTEXT,      &
     & JACBIA,XYZDOT,DUM2,SBFTOD_TOT,II(KISATN),ISATID)

      ITEST=IDYNDL(1)*IDYNDL(2)*IDYNDL(3)
      IF(ITEST.EQ.0) THEN
         IF(.NOT.LACCELS) THEN
            WRITE(6,60010)
            WRITE(6,60011) IDYNDL
            WRITE(6,60012)
            WRITE(6,60013)
            STOP
          ENDIF
          DDTSRF_SBF(1)=SBFTOD_TOT(1,1)*DDTCAD(1)                       &
     &                 +SBFTOD_TOT(2,1)*DDTCAD(2)                       &
     &                 +SBFTOD_TOT(3,1)*DDTCAD(3)
          DDTSRF_SBF(2)=SBFTOD_TOT(1,2)*DDTCAD(1)                       &
     &                 +SBFTOD_TOT(2,2)*DDTCAD(2)                       &
     &                 +SBFTOD_TOT(3,2)*DDTCAD(3)
          DDTSRF_SBF(3)=SBFTOD_TOT(1,3)*DDTCAD(1)                       &
     &                 +SBFTOD_TOT(2,3)*DDTCAD(2)                       &
     &                 +SBFTOD_TOT(3,3)*DDTCAD(3)
      IF(IDYNDL(1).EQ.0) DDTEXT(1)=DDTSRF_SBF(1)
      IF(IDYNDL(2).EQ.0) DDTEXT(2)=DDTSRF_SBF(2)
      IF(IDYNDL(3).EQ.0) DDTEXT(3)=DDTSRF_SBF(3)
      ENDIF

!    TRANSFORM DDTEXT TO TOD HERE
!    HERE DDTEXT IS CORRECTED FOR BIASES

      DDTEXT_TOD(1) = SBFTOD_TOT(1,1)*DDTEXT(1)                         &
     &              + SBFTOD_TOT(1,2)*DDTEXT(2)                         &
     &              + SBFTOD_TOT(1,3)*DDTEXT(3)
      DDTEXT_TOD(2) = SBFTOD_TOT(2,1)*DDTEXT(1)                         &
     &              + SBFTOD_TOT(2,2)*DDTEXT(2)                         &
     &              + SBFTOD_TOT(2,3)*DDTEXT(3)
      DDTEXT_TOD(3) = SBFTOD_TOT(3,1)*DDTEXT(1)                         &
     &              + SBFTOD_TOT(3,2)*DDTEXT(2)                         &
     &              + SBFTOD_TOT(3,3)*DDTEXT(3)

             ! LDYNACP      !%ACC
      endif

!  FILL THE ACCELERATION ARRAYS

                                                                      !%
      IF(LDYNHR) DDTHR_TOD(1:3) = DDTHR_TOD(1:3) + DDTEXT_TOD(1:3)
                                                                   !%
      IF(LDYNREG)DXDD(1:3) = DXDD(1:3)  + DDTEXT_TOD(1:3)




!   ROTATE & SUM DXDD INTO TRUE OF REF ACCELERATION AND ADD TO XDDOT

!     ....DXDD  IS SUM OF TRUE OF DATE ACCELERATIONS
!     ....XDDOT IS SUM OF TRUE OF REFERENCE ACCELERATIONS
!     ....DDTHR_TOD  IS SUM OF THE HIGH RATE TRUE OF DATE ACCELERATIONS
!     ....XDDTHR IS SUM OF THE HIGH RATE TRUE OF REFERENCE ACCELERATIONS
!

      if( .not. LHIRATE             ) then

         XDDOT(1,ISAT)=XDDOT(1,ISAT)+RMI(1)*DXDD(1)+                    &
     &                               RMI(2)*DXDD(2)+RMI(3)*DXDD(3)
         XDDOT(2,ISAT)=XDDOT(2,ISAT)+RMI(4)*DXDD(1)+                    &
     &                               RMI(5)*DXDD(2)+RMI(6)*DXDD(3)
         XDDOT(3,ISAT)=XDDOT(3,ISAT)+RMI(7)*DXDD(1)+                    &
     &                               RMI(8)*DXDD(2)+RMI(9)*DXDD(3)

             !  .not. LHIRATE
       endif

! always compute XDDTHR

                                                                 !%ACC
         XDDTHR(1,ISAT)=XDDTHR(1,ISAT)+RMI(1)*DDTHR_TOD(1)+             &
     &              RMI(2)*DDTHR_TOD(2)+RMI(3)*DDTHR_TOD(3)
                                                                 !%ACC
                                                                 !%ACC
         XDDTHR(2,ISAT)=XDDTHR(2,ISAT)+RMI(4)*DDTHR_TOD(1)+             &
     &              RMI(5)*DDTHR_TOD(2)+RMI(6)*DDTHR_TOD(3)
                                                                 !%ACC
                                                                 !%ACC
         XDDTHR(3,ISAT)=XDDTHR(3,ISAT)+RMI(7)*DDTHR_TOD(1)+             &
     &              RMI(8)*DDTHR_TOD(2)+RMI(9)*DDTHR_TOD(3)
                                                                 !%ACC


!       write(6,*) 'f: XDDOT ', XDDOT(1:3,ISAT)
!       write(6,*) 'f: XDDTHR ', XDDTHR(1:3,ISAT)
!
!     ....XDDTMC CONTAINS THE TOTAL ACCELERATION MINUS THE
!     ....CENTRAL BODY ACCELERATION
!
!
!      LHIRATE  L    S    indicates this call is for high rate integrati
!      LDYNACP  L    S    indicates that this call is in a dynamic
!       write(6,*) 'f:  LHIRATE, LALL ', LHIRATE, LALL

      if( .not. LHIRATE  ) then

         XDDTMC(1,ISAT)=XDDOT(1,ISAT)
         XDDTMC(2,ISAT)=XDDOT(2,ISAT)
         XDDTMC(3,ISAT)=XDDOT(3,ISAT)

         XDDOT(1,ISAT)=XDDOT(1,ISAT)-GMR3*XP(1,ISAT,1)
         XDDOT(2,ISAT)=XDDOT(2,ISAT)-GMR3*XP(2,ISAT,1)
         XDDOT(3,ISAT)=XDDOT(3,ISAT)-GMR3*XP(3,ISAT,1)

                        ! .not. LHIRATE    !%ACC
      endif

!       write(6,*) 'f: XDDTMC ', xddtmc(1:3,ISAT)
!       write(6,*) 'f: XDDOT ', XDDOT(1:3,ISAT)
!       write(6,*) 'f: XDDTHR ', XDDTHR(1:3,ISAT)


!******************************************************************


!     convert tod accels to sbf for acceleration computations.

!       write(6,*) 'f: SBFTOD ', SBFTOD
!       write(6,*) 'f: LDYNACP ', LDYNACP

!      store sbf accels in array of back values


      IF(LACCELS) THEN

!      ....rotate TOD surface accels to ACC BF
!      ....multiply by SBFTOD_TOT TRANSPOSE

       DDTSRF_SBF(1)=SBFTOD_TOT(1,1)*DDTCAD(1)                          &
     &             +SBFTOD_TOT(2,1)*DDTCAD(2)                           &
     &             +SBFTOD_TOT(3,1)*DDTCAD(3)
       DDTSRF_SBF(2)=SBFTOD_TOT(1,2)*DDTCAD(1)                          &
     &             +SBFTOD_TOT(2,2)*DDTCAD(2)                           &
     &             +SBFTOD_TOT(3,2)*DDTCAD(3)
       DDTSRF_SBF(3)=SBFTOD_TOT(1,3)*DDTCAD(1)                          &
     &             +SBFTOD_TOT(2,3)*DDTCAD(2)                           &
     &             +SBFTOD_TOT(3,3)*DDTCAD(3)

!       write(6,*) 'f: DDTSRF_SBF ', DDTSRF_SBF

      XDDTCA(1:3,1,ISAT) = DDTSRF_SBF(1:3)
! PLOT QUANTITIES
      XDDTCA(1,17,1)=RPLOT
      PLTLAT=ASIN(SINPSI)/DEGRAD
      PLTLON=XLAMDA/DEGRAD
      XDDTCA(2,17,1)=PLTLAT
      XDDTCA(3,17,1)=PLTLON
      XDDTCA(1,18,1)=RATIOP

!      ....rotate TOD surface accels to SBF
!      ....multiply by SBFTOD TRANSPOSE TO BE USED IN LDATTP

       XDDTMP(1)=SBFTOD(1,1)*DDTCAD(1)                                  &
     &             +SBFTOD(2,1)*DDTCAD(2)                               &
     &             +SBFTOD(3,1)*DDTCAD(3)
       XDDTMP(2)=SBFTOD(1,2)*DDTCAD(1)                                  &
     &             +SBFTOD(2,2)*DDTCAD(2)                               &
     &             +SBFTOD(3,2)*DDTCAD(3)
       XDDTMP(3)=SBFTOD(1,3)*DDTCAD(1)                                  &
     &             +SBFTOD(2,3)*DDTCAD(2)                               &
     &             +SBFTOD(3,3)*DDTCAD(3)

             ! IF(LACCELS)
      ENDIF

!******************************************************************

! Compute the ATTITUDE parameter partials

                                         ! DYNAMIC ACCELERATION
          IF(LFRCAT.AND.LDYNACP) THEN
! LOAD THE ATTITUDE PARTIALS
      IFIRST=NEQN
      ISECND=3
! NOTE     DDTEXT IS IN THE ACC BF SYSTEM
      CALL LDATTP(PXDDEX(1,1,ISAT),ROLL,PITCH,YAW,DDTEXT,IFIRST,        &
     &TDIFF,SINWT,COSWT,ISECND,.TRUE.,SBFTOD)
          ENDIF
       IF(LACCELS.AND.NPVAL0(IXATUD).GT.0) THEN
      IFIRST=3
      ISECND=NFSCAC
! XDDTMP IS IN THE SBF SYSTEM
      CALL LDATTP(XDDTCA(1,1,ISAT),ROLL,PITCH,YAW,XDDTMP,IFIRST,        &
     &TDIFF,SINWT,COSWT,ISECND,.FALSE.,SBFTOD)
       ENDIF
!******************************************************************
!
!      write(6,*) 'f: gmr3, xp(kkqq, isat,1) ', gmr3, (xp(kkqq,isat,1),
!     1               kkqq=1,3)
!      write(6,12351) DXDD(1),DXDD(2),DXDD(3)
12351 FORMAT(' TOTAL TRUE OF DATE ACCEL ',3D25.16)
!      WRITE(6,12352) XDDTMC(1,ISAT),XDDTMC(2,ISAT),XDDTMC(3,ISAT)
12352 FORMAT(' TRUE OF REF ACCEL (no CB accel )',3D25.16)
!      WRITE(6,12350) XDDOT(1,ISAT),XDDOT(2,ISAT),XDDOT(3,ISAT)
12350 FORMAT(' TOTAL TRUE OF REF ACCEL ',3D25.16)
!      WRITE(6,12353) XDDTHR(1:3,ISAT)
12353 FORMAT(' highrate XDDTHR         ',3D25.16)
!

!
! ROTATE "BOX-WING" PARTIALS FROM TOD TO TOR
      IF (LVAREA) THEN
      CALL ROTPAR(PXDDEX(1,1,ISAT),NEQN,                                &
     &            II(JARUA), II(JEMUA) ,II(JSPUA),                      &
     &            II(JDFUA) ,II(JTAUA), II(JTCUA) ,II(JTDUA),           &
     &            II(JTFUA), II(JTXUA))
      ENDIF


!      ....save partials for high rate calls in accel. run
!      ....PXDDEX is used only on low rate calls so can be overwritten
!      ....on high rate calls
!      ....do while TOR
!      do ia=1,neqn
!      write(6,*) 'f: ia,PXDDEX(ia, 1:3, isat )',
!    . ia,pxddex(ia, 1:3, isat )
!      enddo

!       write(6,*) 'f: LACCELS', LACCELS
!       write(6,*) 'f: NEQN_ACC ', NEQN_ACC,NEQN
      if( LACCELS) then
         DO JJ=1,NEQN_ACC
            PXDDEXA(JJ,1,ISAT)=ZERO
            PXDDEXA(JJ,2,ISAT)=ZERO
            PXDDEXA(JJ,3,ISAT)=ZERO
         ENDDO
             do jj=1,MXFMG
              K=NPRMAC(JJ)
              if(k.eq.0) goto 21212
                do jk=1,k
                PXDDEXA(IPTFBS(JJ)-1+JK, 1:3, ISAT)                     &
     &          =PXDDEX(IPTFBS(jj)-1+JK, 1:3, ISAT)
                IF(LDYNACP) PXDDEX(iptfbs(jj)-1+JK, 1:3 ,ISAT) = ZERO
                enddo
21212         continue
           enddo
! Rotate PXDDEXA to SBF
! Multiply bt SBFTOD_TOT TRANSPOSE

      do Iacc=1, NEQN_ACC

       PXDDEXA_BF(1) =SBFTOD_TOT(1,1)*PXDDEXA(Iacc,1,ISAT)              &
     &               +SBFTOD_TOT(2,1)*PXDDEXA(Iacc,2,ISAT)              &
     &               +SBFTOD_TOT(3,1)*PXDDEXA(Iacc,3,ISAT)
       PXDDEXA_BF(2) =SBFTOD_TOT(1,2)*PXDDEXA(Iacc,1,ISAT)              &
     &               +SBFTOD_TOT(2,2)*PXDDEXA(Iacc,2,ISAT)              &
     &               +SBFTOD_TOT(3,2)*PXDDEXA(Iacc,3,ISAT)
       PXDDEXA_BF(3) =SBFTOD_TOT(1,3)*PXDDEXA(Iacc,1,ISAT)              &
     &               +SBFTOD_TOT(2,3)*PXDDEXA(Iacc,2,ISAT)              &
     &               +SBFTOD_TOT(3,3)*PXDDEXA(Iacc,3,ISAT)


       PXDDEXA(Iacc,1,ISAT)=PXDDEXA_BF(1)
       PXDDEXA(Iacc,2,ISAT)=PXDDEXA_BF(2)
       PXDDEXA(Iacc,3,ISAT)=PXDDEXA_BF(3)
         enddo
       else
         IF(LDYNACP) THEN
             do jj=1,MXFMG
              K=NPRMAC(JJ)
              if(k.eq.0) goto 21213
                do jk=1,k
                PXDDEX(iptfbs(jj)-1+JK, 1:3 ,ISAT) = ZERO
                enddo
21213         continue
           enddo
         ENDIF
       endif
!
!
!
!   TRANSFORM VMATRX INTO TRUE OF REF
!
      IF(LSORB) GO TO 500
      HOLD(1,1)=VMAT(1,1)*RMI(1)+VMAT(1,2)*RMI(2)+VMAT(1,3)*RMI(3)
      HOLD(2,1)=VMAT(2,1)*RMI(1)+VMAT(2,2)*RMI(2)+VMAT(2,3)*RMI(3)
      HOLD(3,1)=VMAT(3,1)*RMI(1)+VMAT(3,2)*RMI(2)+VMAT(3,3)*RMI(3)
      HOLD(1,2)=VMAT(1,1)*RMI(4)+VMAT(1,2)*RMI(5)+VMAT(1,3)*RMI(6)
      HOLD(2,2)=VMAT(2,1)*RMI(4)+VMAT(2,2)*RMI(5)+VMAT(2,3)*RMI(6)
      HOLD(3,2)=VMAT(3,1)*RMI(4)+VMAT(3,2)*RMI(5)+VMAT(3,3)*RMI(6)
      HOLD(1,3)=VMAT(1,1)*RMI(7)+VMAT(1,2)*RMI(8)+VMAT(1,3)*RMI(9)
      HOLD(2,3)=VMAT(2,1)*RMI(7)+VMAT(2,2)*RMI(8)+VMAT(2,3)*RMI(9)
      HOLD(3,3)=VMAT(3,1)*RMI(7)+VMAT(3,2)*RMI(8)+VMAT(3,3)*RMI(9)
      AA(KVMATX+IND18)=RMI(1)*HOLD(1,1)+RMI(2)*HOLD(2,1)+               &
     &                 RMI(3)*HOLD(3,1)
      AA(KVMATX+IND18+1)=RMI(4)*HOLD(1,1)+RMI(5)*HOLD(2,1)+             &
     &                   RMI(6)*HOLD(3,1)
      AA(KVMATX+IND18+2)=RMI(7)*HOLD(1,1)+RMI(8)*HOLD(2,1)+             &
     &                   RMI(9)*HOLD(3,1)
      AA(KVMATX+IND18+3)=RMI(1)*HOLD(1,2)+RMI(2)*HOLD(2,2)+             &
     &                   RMI(3)*HOLD(3,2)
      AA(KVMATX+IND18+4)=RMI(4)*HOLD(1,2)+RMI(5)*HOLD(2,2)+             &
     &                   RMI(6)*HOLD(3,2)
      AA(KVMATX+IND18+5)=RMI(7)*HOLD(1,2)+RMI(8)*HOLD(2,2)+             &
     &                   RMI(9)*HOLD(3,2)
      AA(KVMATX+IND18+6)=RMI(1)*HOLD(1,3)+RMI(2)*HOLD(2,3)+             &
     &                   RMI(3)*HOLD(3,3)
      AA(KVMATX+IND18+7)=RMI(4)*HOLD(1,3)+RMI(5)*HOLD(2,3)+             &
     &                   RMI(6)*HOLD(3,3)
      AA(KVMATX+IND18+8)=RMI(7)*HOLD(1,3)+RMI(8)*HOLD(2,3)+             &
     &                   RMI(9)*HOLD(3,3)
!
      IF(.NOT.LSDRG.AND..NOT.LSGA) GO TO 500
      HOLD(1,1)=VMAT(1,4)*RMI(1)+VMAT(1,5)*RMI(2)+VMAT(1,6)*RMI(3)
      HOLD(2,1)=VMAT(2,4)*RMI(1)+VMAT(2,5)*RMI(2)+VMAT(2,6)*RMI(3)
      HOLD(3,1)=VMAT(3,4)*RMI(1)+VMAT(3,5)*RMI(2)+VMAT(3,6)*RMI(3)
      HOLD(1,2)=VMAT(1,4)*RMI(4)+VMAT(1,5)*RMI(5)+VMAT(1,6)*RMI(6)
      HOLD(2,2)=VMAT(2,4)*RMI(4)+VMAT(2,5)*RMI(5)+VMAT(2,6)*RMI(6)
      HOLD(3,2)=VMAT(3,4)*RMI(4)+VMAT(3,5)*RMI(5)+VMAT(3,6)*RMI(6)
      HOLD(1,3)=VMAT(1,4)*RMI(7)+VMAT(1,5)*RMI(8)+VMAT(1,6)*RMI(9)
      HOLD(2,3)=VMAT(2,4)*RMI(7)+VMAT(2,5)*RMI(8)+VMAT(2,6)*RMI(9)
      HOLD(3,3)=VMAT(3,4)*RMI(7)+VMAT(3,5)*RMI(8)+VMAT(3,6)*RMI(9)
      AA(KVMATX+IND18+9)=RMI(1)*HOLD(1,1)+RMI(2)*HOLD(2,1)+             &
     &                   RMI(3)*HOLD(3,1)
      AA(KVMATX+IND18+10)=RMI(4)*HOLD(1,1)+RMI(5)*HOLD(2,1)+            &
     &                    RMI(6)*HOLD(3,1)
      AA(KVMATX+IND18+11)=RMI(7)*HOLD(1,1)+RMI(8)*HOLD(2,1)+            &
     &                    RMI(9)*HOLD(3,1)
      AA(KVMATX+IND18+12)=RMI(1)*HOLD(1,2)+RMI(2)*HOLD(2,2)+            &
     &                    RMI(3)*HOLD(3,2)
      AA(KVMATX+IND18+13)=RMI(4)*HOLD(1,2)+RMI(5)*HOLD(2,2)+            &
     &                    RMI(6)*HOLD(3,2)
      AA(KVMATX+IND18+14)=RMI(7)*HOLD(1,2)+RMI(8)*HOLD(2,2)+            &
     &                    RMI(9)*HOLD(3,2)
      AA(KVMATX+IND18+15)=RMI(1)*HOLD(1,3)+RMI(2)*HOLD(2,3)+            &
     &                    RMI(3)*HOLD(3,3)
      AA(KVMATX+IND18+16)=RMI(4)*HOLD(1,3)+RMI(5)*HOLD(2,3)+            &
     &                    RMI(6)*HOLD(3,3)
      AA(KVMATX+IND18+17)=RMI(7)*HOLD(1,3)+RMI(8)*HOLD(2,3)+            &
     &                    RMI(9)*HOLD(3,3)
  500 CONTINUE
!
      IF(ISAT.EQ.NSAT) GO TO 2000
      INDNP=INDNP+NP
      INDNMX=INDNMX+NMAX
      INDNM1=INDNM1+NMAXP1
      INDCR=INDCR+9
      INDVR=INDVR+14
      IND18=IND18+18
      INDEXP=INDEXP+NP6
      IB0DRG=IB0DRG+1
      IBDRG=IBDRG+IORDRG+IPDDRG
      ISTDRG=ISTDRG+IPDDRG
      ILAJDP=ILAJDP+IPDDRG
      IAPLM=IAPLM+1
      IAPGM=IAPGM+IORSRD+IPDSRD
      ISTSRD=ISTSRD+IPDSRD
      ILAJSP=ILAJSP+IPDSRD
      IGA=IGA+IORGA+IPDGA
      ISTGA=ISTGA+IPDGA
      ILAJGP=ILAJGP+IPDGA
      ITGA=ITGA+1
      ILSTSN=ILSTSN+1
      INDARK=INDARK+1
      INSTRN=INSTRN+1
 2000 END DO
 6000 FORMAT(6D22.12)
      BLKDAT(4,1)=HLDLAS
      RETURN
60010 FORMAT(' EXECUTION TERMINATING IN SUBROUTINE F')
60011 FORMAT(' AXIS DELETION SELECTED FOR DYNAMIC ACCELEROMETRY: ',3I2)
60012 FORMAT(' CURRENTLY THIS REQUIRES PRESENCE OF GEOMETRIC')
60013 FORMAT(' ACCELEROMETRY')
60014 FORMAT(' NUMBER OF C OR S GRAVITY COEFICIENTS FOR')
60015 FORMAT(' SECONDARY ASTEROID: ',I6,' EXDEDDS MAX ALLOWEDL ',I6)
      END
