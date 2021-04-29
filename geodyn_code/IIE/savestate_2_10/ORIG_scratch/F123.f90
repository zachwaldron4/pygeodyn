!$F123   f0
      SUBROUTINE F123(MJDSEC,FSEC,XP,XDDOT,PXDDEX,NSAT,NEQN,NCCALL,     &
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
     &   IACCP,PACCL,NRAT,XDDRC,JRO,                                    &
     &   XDDGP,XSA)
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
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      INTEGER*4 DELTAL
      CHARACTER side(2)
      DATA SIDE/'R','L'/
!      SAVE
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
!   LCL4SA   I    S    TRUE IF THIS CALL IS BEING MADE FOR SECONDARY
!                      ASTEROID OF A BINARY SYSTEM!
!   XDDGP    I    A    J2000 HELIOCENTRIC GRAVITATIONAL ACCELS OF
!                      PRIMARY ASTEROID DUE TO SECONDARY ASTEROID
!   XSA      I    A    J2000 PRIMARY ASTEROID CENTERD POSITION OF
!                      SECONDAY ASTEROID
!
!
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
      PARAMETER (  ZERO = 0.D0 )
      PARAMETER (  ONE  = 1.D0 )
      PARAMETER (  TWO  = 2.D0 )
      PARAMETER (  CM999= -999.0D0)
      PARAMETER (  NPMSA=39)
!
      dimension tempv(3,6)
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

!     index for surface forces in high rate array
      INTEGER,dimension(Nsurfforce) :: ims2r
      DATA IMS2R/ 3,1,2,6,4,5,7,8,9,10,11 /

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
      DIMENSION XDDGP(3),XSA(3),XPSA(3)
      DIMENSION HGCOM(73)
      DIMENSION CSA(NPMSA),SSA(NPMSA)


!
      INCLUDE 'COMMON_DECL.inc'
      COMMON/ACCEL9/MXACC9,ITOTGA,NUMGA,ITYPGA,MACC,NXACC9
      COMMON/ACCELO/IDYNDL(3),IDYNSN(3),IGEOSN(3),NXACCO
      COMMON/ACCLRM/MDYNPD,MBAPPD,MACOBS,IDYNSC(200),MDYNST,IACCSC(200),&
     &              MACCSC,NACPRM(200),NATPRM(200),NXCLRM
      COMMON/AGRAV /NACSET,NADEG,NAORD,IAGCB,IAGCE,IDATB,IDATE,INTAG,   &
     &KCOUNT,KSKIP,INTC21,NXAGRA
      COMMON/ALBMIS/ALBON,ALBCON,EMSCON,ALBTUM,XALBMI
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
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
      COMMON/CBDTRU/BDTRUE(7,999)
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
      COMMON/EXATFL/LEXATO
      COMMON/EXPGRV/DGRV(3,4,2)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
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
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/SUBACC/DDTEXT(3),XYZDOT(3)
      COMMON/THERMD/DRGSAT,DTHSC,ZETA,SPAXT,SPAXL,THRMFL,XTHERM
      COMMON/TIDALC/LRAY,LOLDST,NXTIDL
      COMMON/TOPEXA/BETAP,PRVBET,SOMEGA,PRVOMG,YAWANG,SGAMMA,           &
     &              BETAMX,TDLOUV(8,3)
      COMMON/TOPEXL/LTOPX1,LRAMP,LRDOWN,LRUP,LFIX0,LFIX90,LSIN,LTXPRT
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/TPGRAV/NTPGCU,NTPGCA,NTPGSU,NTPGSA,ITPGRV,NTIMEC,NTIMES,   &
     &              NXTPGR
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
!
      data kentry/0/
      data icbsun/11/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
       IF(.NOT.LSTINR) RETURN
       IF(NPVAL0(IXXTRO).LE.0) RETURN
       JRO1=JRO
       KRO1=JRO
       JRO2=1
       KRO2=0
       IF(JRO.GT.3) THEN
         JRO1=JRO-3
         KRO1=0
         JRO2=2
         KRO2=JRO-3
       ENDIF
       LJASON1=.FALSE.
       LGFO1=.FALSE.
       LENV1=.FALSE.
       IF(LXPCLK) CALL CLKACC(MJDSEC,FSEC,NSAT,XP,XDDRC,AA,II,LL)
       ISATIX=ISATID(1)
       kentry = kentry + 1
!       write(6,*) 'f123: kentry ', kentry
!       if( kentry .gt. 10 ) stop
!
      FSSTRT=MJDSEC+FSEC-ESSTRT
      TIME  =MJDSEC+FSEC

!      write(6,*) 'f123:  FSSTRT ', FSSTRT

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
!      write(6,*) 'f123: CALL PREFCR '
      CALL PREFCR(NCCALL,SFRAC,LHIRATE,AA,COSTG,SINTG,ROTI)

!      write(6,*) 'f123: CALL REFCRS '
      COSTGQ=COSTG
      SINTGQ=SINTG
      CALL REFCRS(LGEOPL,SINTGQ,COSTGQ,                                 &
     &   AA(KDPSR),AA(KXPUT),AA(KYPUT),MJDSEC,FSEC,ROTI(1),             &
     &   ROTI(2),ROTI(3),ROTI(4),ROTI(5),ROTI(6),ROTI(7),               &
     &   ROTI(8),ROTI(9),AA(KA1UT),                                     &
     &   II(KINDPI),AA(KXDPOL),AA(KXDOTP),KRO1)
!
!     write(6,*) 'f123: rmb ', rmb
!     write(6,*) 'f123: rmi ', rmi
!
!  MODIFY C & S COEFFICIENTS FOR TIME DEPENDENT GRAVITY AND DYNAMIC
!  POLAR MOTION
!
      IEND=NPVAL(IXGPC)+NPVAL(IXGPS)
!     write(6,*) 'f123: iend ',iend
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
      IF(LGVTPM)then
!     write(6,*) 'f123: call grvtim '
           CALL GRVTIM(MJDSEC,FSEC,                              &
     &     II(KIPTC), II(KIPTS) ,AA(KCSAVE),AA(KSSAVE),                 &
     &     AA(KCDT)  ,AA(KCPDA) ,AA(KCPDB) ,AA(KOMGC) ,AA(KSDT)  ,      &
     &     AA(KSPDA) ,AA(KSPDB) ,AA(KOMGS) ,AA(KCOMGC),AA(KSOMGC),      &
     &     AA(KCOMGS),AA(KSOMGS),AA(KCN)   ,AA(KSN))
      endif
!
! GET INERTIAL PLANETARY POSITIONS BEFORE SATELLITE LOOP
!
      IF(LGRELE.OR.LXTIDE) THEN
      LVELP=.TRUE.
      ELSE
      LVELP=.FALSE.
      ENDIF
!
!     write(6,*) 'f123: call planpo '
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

!       write(6,*) 'f123: LHIRATE ', LHIRATE

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



!        write(6,*) 'f123:  Ntotforces ',  Ntotforces
!        write(6,*) 'f123:  LFHIRT ',  LFHIRT
!        write(6,*) 'f123:  LCALL ', LCALL
!        write(6,*) 'f123:  LFILLREG ',  LFILLREG
!        write(6,*) 'f123:  LFILLHR  ',  LFILLHR
!        write(6,*) 'f123:  LDYNHR ', LDYNHR
!        write(6,*) 'f123:  LDYNREG ', LDYNREG

!        if( .not.LALL .or. kentry.eq.1) then
!        write(6,*) 'f123:  iforce, name, LCALL, LFHIRT, LFILLREG '
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
!
!   GET VARIOUS BODY FIXED QUANTITIES ASSOCIATED WITH SATELLITE
!
!      write(6,*)'f123: call  ECFSET '
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
         IF(NLOCAL.GT.0.AND.IOPTLG.EQ.3) THEN
            NPX=NPMASS
            IF(NPMASS.GT.NP) NPX=NP
            DO J=1,NPX
              AA(KCN+J-1)=AA(KCN+J-1)+CMASS(J)
              AA(KCN+J-1+NP)=AA(KCN+J-1+NP)+SMASS(J)
            ENDDO
         ENDIF
!      write(6,*)'f123: call  EGRAV  '
         CALL EGRAV(AA(KXNRMZ),AA(KXNRMC),AA(KPN+INDNP),AA(KCN),AA(KSN),&
     &      AA(KAORN+INDNMX),AA(KSINLM+INDNM1),AA(KCOSLM+INDNM1),       &
     &      AA(KTANPS+INDNM1),AA(KXM),AA(KXNP1),AA(KXPRFL+INDEXP),      &
     &      tmp_acc,AA(INDVR),AA(KTNORM))
         DGRV(1,JRO1+1,JRO2)=tmp_acc(1)
         DGRV(2,JRO1+1,JRO2)=tmp_acc(2)
         DGRV(3,JRO1+1,JRO2)=tmp_acc(3)
!      write(6,*)'f123: JRO = ', JRO
!      write(6,*)'f123: DGRV(1:3, JRO+1) ', DGRV(1:3, JRO+1)
 2000 CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
      IF(.NOT.LBINAST) GO TO 30
! START CODE FOR GRAVITATIONAL ATTRACTION FROM SECONARY ASTEROID
      CALL CRDSET(1,HGCOM)
      GM=AA(KPRMV-1+IPVAL(IX2GM))
      AE=AA(KPRMV-1+IPVAL(IX2BDA))
      CALL PREFCS(MJDSEC,FSEC,AA,II,COSTG,SINTG,ROTI)
      CALL REFCRS(.FALSE.,SINTG,COSTG,                                  &
     &   AA(KDPSR),AA(KXPUT),AA(KYPUT),MJDSEC,FSEC,ROTI(1),             &
     &   ROTI(2),ROTI(3),ROTI(4),ROTI(5),ROTI(6),ROTI(7),               &
     &   ROTI(8),ROTI(9),AA(KA1UT),                                     &
     &   II(KINDPI),AA(KXDPOL),AA(KXDOTP),KRO2)
      XPSA(1)=XP(1,1,1)-XSA(1)
      XPSA(2)=XP(2,1,1)-XSA(2)
      XPSA(3)=XP(3,1,1)-XSA(3)
      RSQSA=XPSA(1)*XPSA(1)+XPSA(2)*XPSA(2)+XPSA(3)*XPSA(3)
      RSA=SQRT(RSQSA)
      call  ECFSET(.FALSE.,XPSA, INDCR+9,INDVR+14,RSA,RSQSA,1,1,        &
     &             GMR3SA, AA, II, LL)
      NGH1=NMAX
      NGH2=NMAXP1
      NGH3=NP
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
!
      tmp_acc(1)=tmp_acc(1)-XDDGP(1)
      tmp_acc(2)=tmp_acc(2)-XDDGP(2)
      tmp_acc(3)=tmp_acc(3)-XDDGP(3)
      DGRV(1,JRO1+1,JRO2)=DGRV(1,JRO1+1,JRO2)+tmp_acc(1)
      DGRV(2,JRO1+1,JRO2)=DGRV(2,JRO1+1,JRO2)+tmp_acc(2)
      DGRV(3,JRO1+1,JRO2)=DGRV(3,JRO1+1,JRO2)+tmp_acc(3)
      CALL CRDSET(2,HGCOM)
! END CODE FOR GRAVITATIONAL ATTRACTION FROM SECONARY ASTEROID
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
   30 CONTINUE
!      write(6,*) 'f123: return'
      RETURN
 6000 FORMAT(6D24.16)
60010 FORMAT(' EXECUTION TERMINATING IN SUBROUTINE F123')
60011 FORMAT(' AXIS DELETION SELECTED FOR DYNAMIC ACCELEROMETRY: ',3I2)
60012 FORMAT(' CURRENTLY THIS REQUIRES PRESENCE OF GEOMETRIC')
60013 FORMAT(' ACCELEROMETRY')
60014 FORMAT(' NUMBER OF C OR S GRAVITY COEFICIENTS FOR')
60015 FORMAT(' SECONDARY ASTEROID: ',I6,' EXDEDDS MAX ALLOWEDL ',I6)
      END
