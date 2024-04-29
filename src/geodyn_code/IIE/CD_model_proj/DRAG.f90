!$DRAG
      SUBROUTINE DRAG(MJDSEC,FSEC,XS,XD,RASAT,DXDD,PXDDT,VMATRX,CORPAR, &
     &                VRARAY,LORBIT,NEQN,B0,B,STDRG,LAJDP,SATP,         &
     &                AA,II,LL,ISATID)
!********1*********2*********3*********4*********5*********6*********7**
! DRAG             83/08/04            8308.0    PGMR - D. ROWLANDS
!
! FUNCTION:    (1)  CALCULATE ACCELERATION DUE TO DRAG
!              (2)  IF AN ADJUSTMENT IS BEING DONE, CALCULATE THE
!                   CONTRIBUTION FROM DRAG TO THE V-MATRIX
!              (3)  IF DRAG COEFFCIENTS ARE BEING ADJUSTED, CALCULATE
!                   THEIR EXPLICIT PARTIALS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    TIME IN INTEGRAL SECONDS FROM GEODYN REF. TIME
!   FSEC     I    S    FRACTIONAL REMAINING SECONDS
!   XS       I    A    TRUE OF DATE POSITION OF SATELLITE
!   XD       I    A    TRUE OF DATE VELOCITY OF SATELLITE
!   RASAT    I    S    SATELLITE RIGHT ASCENSION
!   DXDD     O    A    TRUE OF DATE DRAG CONTRIBUTION TO ACCELERATION
!   PXDDT    O    A    EXPLICIT PARTIALS OF FORCE MODEL PARAMETERS
!                      IN TRUE OF REFERENCE
!   VMATRX  I/O   A    V-MATRIX IN TRUE OF DATE WITHOUT
!                      DRAG CONTRIBUTION AS INPUT
!                      V-MATRIX IN TRUE OF DATE WITH
!                      DRAG CONTRIBUTION AS OUTPUT
!   CORPAR   I    A    TRANSFORMATION MATRIX FOR BODY FIXED R,PSI,LAMDA
!                      TO BODY FIXED XYZ (CORPAR(7)=SINPSI)
!   VRARAY   I    A    BODY FIXED INFORMATION ABOUT SATELLITE
!                      INCLUDING R & R**2
!   LORBIT   I    S    .TRUE. IF ORBIT GENERATOR RUN
!   NEQN     I    S    NUMBER OF FORCE MODEL PARAMETERS
!   B0       I    S    .5*AREA/MASS
!   B        I    S    B0*CD
!   STDRG    I    A    START TIME (ET SECONDS SINCE GEODYN REF. TIME) OF
!                      DISCREET DRAG PERIODS
!   LAJDP    I    A    LOGICAL TELLING WHETHER A PARTICULAR PERIOD
!                      HAS BEEN ADJUSTED
!   SATP     I    A    BODY FIXED COORDINATES OF THE SATELLITE
!   AA      I/O   A    DYNAMICALLY ALLOCATED ARRAY
!   II      I/O   A    DYNAMICALLY ALLOCATED ARRAY
!   LL      I/O   A    DYNAMICALLY ALLOCATED ARRAY
!
! COMMENTS:
!
! Modifications by Zach Waldron 9/2020 - 12/2021
!       - separated the reading of DTM87, MSIS86, and JAACHIA71 into distinct IF THEN statements 
!       - added checks to print to UNIT6 (IIEOUT) that the correct model
!             is being used
!       - Added a write file to print out RHO, DRHODZ and primary
!             satellite ephemeris
!       - Added feature to SUMRY.f90 subroutine to show fix error where
!             MSIS was not being printed as the model being used if selected.
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO  = 0.D0 )
      PARAMETER ( ONE = 1.D0 )
      PARAMETER ( TWO = 2.D0 )
      PARAMETER ( SECHR = 3600.0D0 )
!     ....MAS IS DEFINED = 48
      PARAMETER ( MAS = 48 )
!     ....IDAY0 = Dec 4, 1978
!     IDAY0 =  2443846 - 2430000
!CC   PARAMETER ( IDAY0 =13846 )
      PARAMETER (  D24 = 24.D0 )
      PARAMETER (  D27 = 27.D0 )
      PARAMETER (  D16P08 = 16.08D0 )
      PARAMETER (  DP6165 = 0.616537D0 )
      PARAMETER (  D360 = 360.D0 )
      PARAMETER (  D1000 = 1000.D0 )
!
!        ....JPL density parameters
!
      PARAMETER (  D31213 = 3.12D-13 )
      PARAMETER (  D250   = 250.D0   )
      PARAMETER (  D230   = 230.D0   )
      PARAMETER (  D18    = 18.D0    )
      PARAMETER (  D25    = 25.D0    )
!
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/ATMPAR/XMAXHT,XATMPA
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/BWINTG/JBFNRM,JTDNRM,JBWARE,JBWSPC,JBWDIF,JBWEMS,JBWTPA,   &
     &              JBWTPC,JBWTMD,JBWTMF,JBWTHX,JARADJ,JSPADJ,JDFADJ,   &
     &              JEMADJ,JTAADJ,JTCADJ,JTDADJ,JTFADJ,JTXADJ,JARUA ,   &
     &              JSPUA ,JDFUA ,JEMUA ,JTAUA ,JTCUA ,JTDUA ,JTFUA ,   &
     &              JTXUA ,NFACE ,NMOVE,NTFACE,JBFNR2,JBFNR3,JTDNR2,    &
     &              JTDNR3
      COMMON/BWLOGC/LVAREA,LTOPEX,LSPOT2,LGPSVA,LERS1,LMO,LMOC,LTDRSA,  &
     &              LMAGNA,LGFO,LTRMM,LEUVE,LVCL,LENVS,LCRYO,LGRAIL,    &
     &              LHY2A,LSARAL
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
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
      COMMON/CRMI/RMI(9)
      COMMON/CTHDOT/THDOT
      COMMON/CTHDXT/THDXT
      COMMON/DELOFF/NOFFST,NSATDL,NSTADL,NCGMAS,NXDELO
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/DRGBLK/HT,SINPSD,SPSISQ,C(4),RHO
!
      COMMON/EPHM/FSECXY(1),XP(261),YP(261),A1UT(261),EP(261),EC(261),  &
     &            FSCFLX(1),FLUXS(36),AVGFLX(36),FLUXM(288),FSECEP(1),  &
     &            EPHP(816),EPHN(96),ELIB(120),BUFT(2700),FSECNP(4)
!
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
!
      COMMON/EPHMVN/FLUXSO(36),FSCFLO
!
      COMMON/HSCALE/HTSCAL
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/LSFSHD/LSLFSH, NXSLFS
      COMMON/MNSNSP/XMNSNS(7,2)
      COMMON/SETADJ/LSADRG(4),LSASRD(4),LSAGA(4),LSAGP,LSATID,LSAGM,    &
     &              LSADPM,LSAKF,LSADNX,LSADJ2,LSAPGM
      COMMON/SETAPT/                                                    &
     &       JSADRG(1),JSASRD(1),JSAGA(1),JFAADJ,JTHDRG,JACBIA,JATITD,  &
     &       JDSTAT,JDTUM ,                                             &
     &       JSACS, JSATID,JSALG,JSAGM,JSAKF,JSAXYP,JSADJ2,JSAPGM,      &
     &       JFGADJ,JLTPMU,JLTPAL,JL2CCO,JL2SCO,JL2GM,JLRELT,           &
     &       JLJ2SN,JLGMSN,JLPLFM,JL2AE,JSAXTO,JSABRN
      COMMON/SETIOR/IORDRG,IORSRD,IORGA,IPDDRG,IPDSRD,IPDGA
      COMMON/SLFSHD/NSHSAT,IDSASH(200),NSPANL(200),IFLGSH(200),         &
     &              IATYSH(200),NSHRCD(200),MNSHPL,NXSFSD
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
      COMMON/PMARS /DTM74 ,XPMARS
!
      DIMENSION IATDIM(6)
      DIMENSION XS(3),XD(3),DXDD(3),PXDDT(NEQN,3),VMATRX(3,6),          &
     &          CORPAR(9),VRARAY(14),TEMP(3),DHTDX(3),PARTII(3,3),      &
     &          XDRREF(3),B(1),TM(3),STDRG(1),LAJDP(1),XSUNN(2)
      DIMENSION SATP(3),DXMDMP(3,3)
      DIMENSION AA(1),LL(1),II(1)
!
!      ....VENUS ATMOSPHERE OUTPUTS
!
!      D(1) - TOTAL MASS DENSITY (GM/CM3)
!        2  - CO2 NUMBER DENSITY (CM-3)
!        3  - O
!        4  - CO
!        6  - N
!        7  - N2
!      T(1) - EXOSPHERIC TEMPERATURE
!        2  - TEMPERATURE AT ALTITUDE
!
      DIMENSION D(7),T(2)
!
      DATA IATDIM/1,1,1,8,8,8/
      DATA KENTRY/0/,SINHL/0.D0/,COSHL/0.D0/
      DATA IKPAP/1/,I324/3/,IDRV/1/
!     
!
      integer, dimension(5) :: optionsin
      CHARACTER(len=255) :: model_path
      CHARACTER(len=200) :: orbitcloud_file
!
      integer :: choose_model
      integer :: i  
      CHARACTER(len = 20) kamodo_model
      CHARACTER(len = 1)  dtmversion_model


     
      !!! INITIALIZE VARIABLES AND DATA FOR THE MDRIA CD PHYSICS MODEL
      !% amu's or molar mass:g/mol; He, O, N2, O2, Ar, H, N,O
      DATA He_amu/4.002602D0/
      DATA O_amu/15.9994D0/
      DATA N2_amu/28.0134D0/
      DATA O2_amu/31.9988D0/
      DATA Ar_amu/39.948D0/
      DATA H_amu/1.0079D0/
      DATA N_amu/14.0067D0/
      !
      DOUBLE PRECISION, allocatable :: n_dens_temp(:)
      !
      REAL(8) :: CD_drag
      REAL(8) :: TEMP_atm
      REAL(8) :: NOXA_atm
      REAL(8) :: MEANMOL_atm
      REAL(8) :: SCMASS
      REAL(8) :: SPEED_RATIO
      
      integer :: intFACE
      CHARACTER(len=255) :: PATH_IO_GEODYN
      CHARACTER(len=255) :: PATH_IIELOCAL
!
!**********************************************************************
!           START OF EXECUTABLE CODE              *********************
!**********************************************************************
!
!
! 
      KENTRY = KENTRY + 1
        IF(LSTINR) THEN !-----------------------------------
         !IF(IPDDRG.EQ.1) THEN
            kin_2 = kin_2 + 1
            !if(kin_2.eq.1) WRITE(6,*) "=============================="
            !if(kin_2.eq.1)  WRITE(6,*)  '     Last inner iteration     '
            !if(kin_2.eq.1) WRITE(6,*)  '                   search'
         !ENDIF
         !IF(IPDDRG.EQ.2) THEN
         !   IF(kin_2.GT.0).AND.IF(Log_Kin2) kin_2 = 0
         !   kin_2 = kin_2 + 1
         !ENDIF
         !IF(IPDDRG.EQ.3) THEN
         !   kin_2 = kin_2 + 1
         !ENDIF
         !IF(IPDDRG.EQ.4) THEN
         !   kin_2 = kin_2 + 1
         !ENDIF
        ENDIF           !-----------------------------------
      FLUSH(6)
      
    !if(kin_2.eq.1) WRITE(6,*) 'IPDDRG   ',IPDDRG
    !if(kin_2.eq.1) WRITE(6,*) 'LSADRG-- adjust drag coeff   ',LSADRG   
   ! if(kin_2.eq.1) WRITE(6,*) 'LAJDP    ',LAJDP    !LOGICAL TELLING WHETHER A PARTICULAR PERIOD HAS BEEN ADJUSTED
    !if(kin_2.eq.1) WRITE(6,*) 'LORBIT   ',LORBIT
    !if(kin_2.eq.1) WRITE(6,*) 'STDRG    ',STDRG
    

!-------------------------------------------------------------------------------------------------
!  
! Read in the options file from pygeodyn. We only need/want to do this once.
      if(kentry.eq.1)then

            !!! Read the path to the options file from am environment variable
            CALL get_environment_variable("PATH_IO_GEODYN", PATH_IO_GEODYN)      
            !     
            open (121,                                                    &
                  & file=trim(PATH_IO_GEODYN)//"/"//trim("geodyn_options.txt"),&
                  &       status='old')
                  do i=1,5   !#### Loop through the options file and save the values to an array.
                        read(121,*) optionsin(i)
                  end do
            close (121)     
             !the first  index contains the drhodz option (for use in MSIS routines)
                 !  (Zach W. later made the DRHODZ update the default)
             
             !the second index contains the choose_model option 
                 !     For each base model type (jaachia71, dtm86, and msis86), the 
                 !         choose_model selects versions of each of those series of 
                 !         models to be used in a SELECT CASE Statement.
             !the third index contains an optional float value that can be used for whatever the user wants
             
             !the fourth index contains the choice for which CD MODEL to USE
                 ! we haven't run across the alternative but the Variable Area Model must be ON (LVAREA==True)
                 ! optionsin(4) = 1 ---- use the original options in GEODYN (BWDRAG)
                 ! optionsin(4) = 2 ---- use the DRIA model by Visal and Zach
            choose_model=optionsin(2)          
      
            ! Read in the path for the location of the modeloutput that was
            ! used by Kamodo i.e. location of TIEGCM .nc files
          open (122,                                                           &
            & file=trim(PATH_IO_GEODYN)//"/"//trim("geodyn_modelpaths.txt"),   &
            &       status='old')
            read(122,"(A200)") model_path
            read(122,"(A200)") orbitcloud_file
          close (122)             
          
          WRITE(6,*) "======================= DRAG.F90 ======================="
          WRITE(6,*) '     * model_path                     ', trim(model_path)
          WRITE(6,*) '     * orbitcloud_file                '
          WRITE(6,*) '         ', trim(orbitcloud_file)
          WRITE(6,*) '     * Drag Coefficient Model (CD)    ', optionsin(4)

           if(optionsin(4).eq.1)then
             WRITE(6,*) '          - Using the Original CD Model in GEODYN'
           else if(optionsin(4).eq.2)then
             WRITE(6,*) '          - Using the Modifified DRIA Physical CD model'
             !!!!  IF WE ARE USING DRIA, there are a few things we need:
             !     1) MSIS2.f90 needs to return the consituent densities
           endif
           if(optionsin(5).eq.1)then
             WRITE(6,*) '          - Using a Scaling Factor'
             Ldrag_ScalingFactor = .TRUE.
           endif

           
      endif        
!-------------------------------------------------------------------------------------------------
!
      RATIO2=1.D0
      ITEST=MJDSEC+FSEC
      IF(NCGMAS.GT.0) THEN
        DO I=1,NCGMAS
           IF(ISATID.EQ.II(KISATC-1+I).AND.                          &  
     &      ITEST.GE.II(KMJDCG-1+I)) RATIO2=1.D0/AA(KSCMRA-1+I)
        ENDDO
      ENDIF
!
      TTEST=MJDSEC+FSEC
      TM(1)=ONE
      SINPSD=CORPAR(7)
      SPSISQ=SINPSD*SINPSD
      X=XS(1)
      Y=XS(2)
      Z=XS(3)
      XDOT=XD(1)
      YDOT=XD(2)
      ZDOT=XD(3)
      R=VRARAY(4)
      RSQ=VRARAY(5)
      RHO=ZERO
!     ....IDAY0 = Dec 4, 1978
!     IDAY0 =  2443846,5 - GEODYN REF.TIME
      IDAY0 = 43846-INT(TMGDN2+0.001)
!                                                                       &

!
! OBTAIN ATMOSPHERIC DENSITY
!
!     ....TEST IF CENTRAL BODY IS EARTH OR NOT
      IF(ICBDGM.NE.3) GO TO 170
!
!     ....COMPUTE FOR EARTH HERE
!
! DENSITY FOR EARTH & VELOCITY FOR EARTH
      HT=R-AE-(FSQ32*SPSISQ**2-FFSQ32*SPSISQ)


      !write(6,*)'drag: r, ae, ht ', r, ae, ht

      ! turn off density computations for ht > 10**7 meters
      if( ht > 1.0D7 ) then

          rho = 0.0D0
          go to 160

      endif ! ht > 1.0d7

! IF IATDEN=41,42,43  USE FRENCH DENSITY MODEL;
! IF IATDEN=31,32,33  USE JACCHIA MODEL
! IF IATDEN=51,52,53  USE MSIS86 MODEL

! IATDEN MUST BE DIVIDED BY 10 BECAUSE IT IS NOW A TWO DIGIT NUMBER
! ONE OF THE FOLLOWING: 10,20,30,41,42,43 ... 51,52,52 ...61,62,63
!
      IATDN=IATDEN/10
      IFLXDM=IATDIM(IATDN)
!                                                                       &

! TO OBTAIN PARAMETERS FOR FRENCH DTM MODEL
! AND THE MSIS MODEL
!
!
! FOR SATELLITES EARLIER THAN JANUARY 1 1978 THE ORIGINAL COMPUTATION
! FOR THE DAY OF THE YEAR WILL BE USED. fOR SATELLITES LATER THAN JAN
! 1 1978 GEODYN USES THE SAME METHOD OF COMPUTATIUONS AS IN UTOPIA.
! THIS WAS AGREED AS PART OF THE TOPEX SOFTWARE INTERCOMPARISON PLAN.
! Jan. 1,0h ,1978 = JD2443509.5 ..luo
!
      XJD0= 43509.D0
      XJD=(DBLE(MJDSEC)+FSEC)/86400.D0 + TMGDN2
      IF(XJD.LT.XJD0.OR.IATDN.EQ.5) THEN
      DAY=DOY(MJDSEC,FSEC)-ONE
      ELSE
      IMJD=(DBLE(MJDSEC)+FSEC)/SECDAY
      FDAY=(DBLE(MJDSEC-IMJD*86400)+FSEC)/SECDAY
      DAY=DBLE(IMJD)+FDAY
      ENDIF
      ALTI=HT
      XSEC=DBLE(MJDSEC)+FSEC
      XDAYS=XSEC-FSCFLX(1)+SECDAY
      NDAYS=XDAYS/SECDAY
!* **ALTER XDAYSB TO TAKE 0.71 DAY (61344 SEC) OTTAWA TIME INTO ACCOUNT.
      XDAYSB=XDAYS-SECDAY - 61344.
      YDAYSB=XDAYSB/SECDAY
      NDAYSB=XDAYSB/SECDAY
      ZDAYSB=DBLE(NDAYSB)
      IF(NDAYSB.LE.36) GOTO 100
         XDAYS=XSEC-FSCFLX(1+NTOT)+SECDAY
         NDAYS=XDAYS/SECDAY
!* **ALTER XDAYSB TO TAKE 0.71 DAY (61344 SEC) OTTAWA TIME INTO ACCOUNT.
         XDAYSB=XDAYS-SECDAY - 61344.
         YDAYSB=XDAYSB/SECDAY
         NDAYSB=XDAYSB/SECDAY
         ZDAYSB=DBLE(NDAYSB)
  100 FLUXIN=FLUXS(NDAYSB)*(1.0D0-YDAYSB+ZDAYSB)                        &
     &      +FLUXS(NDAYSB+1)*(YDAYSB-ZDAYSB)
      FLXAVG=AVGFLX(NDAYS)
!
! FOR THE MAGNETIC FLUX AT THREE HOURS AGO
!
      XDAYS=XSEC-FSCFLX(1)
      NDAY=XDAYS/SECDAY
      YDAYS=DBLE(NDAY)*SECDAY
      HRS=(XDAYS-YDAYS)/SECHR
      IND3HR=(HRS/3.0D0)+1
      IND3HR=IND3HR+NDAY*IFLXDM
      IF(NDAY.LE.36) GOTO 110
         XDAYS=XSEC-FSCFLX(1+NTOT)
         NDAY=XDAYS/SECDAY
         YDAYS=DBLE(NDAY)*SECDAY
         HRS=(XDAYS-YDAYS)/SECHR
         IND3HR=(HRS/3.0D0)+1
         IND3HR=IND3HR+NDAY*IFLXDM+NTOT
  110 FLUXKP=FLUXM(IND3HR)
      XYNORM=SQRT(X**2+Y**2)
      XX=X/XYNORM
      YY=Y/XYNORM
      XYNORM=SQRT(XMNSNS(1,2)**2+XMNSNS(2,2)**2)
      XSUNN(1)=XMNSNS(1,2)/XYNORM
      XSUNN(2)=XMNSNS(2,2)/XYNORM
      COSHL=XX*XSUNN(1)+YY*XSUNN(2)
      SINHL=YY*XSUNN(1)-XX*XSUNN(2)
!
! PARAMETER USED IN DTM IS LOCAL SOLAR TIME
! LOCAL SOLAR TIME = SOLAR HOUR ANGLE + 12 HOURS
! GET SINE AND COSINE OF (HL+PI)
!
      COSHL=-COSHL
      SINHL=-SINHL
!
!     COSLAT=COSPSI  THIS IS GEOCENTRIC LATITUDE
!     SINLAT=SINPSI
!
! GET GEODETIC LATITUDE AND HEIGHT ABOVE THE REFERENCE ELLIPSOID
! IN SUBROUTINE PLHOUT
! DXMDMP IS THE PARTIALS MATRIX, FOR THIS CALL IT IS A DUMMY MATRIX
! FUNCTION PLHOUT:  1) TO COMPUTE GEODETIC PHI, LAMBDA, H FROM
!                      GEOCENTRIC X, Y, Z
!                   2) TO COMPUTE PARTIAL DERIVATIVES OF GEOCENTRIC
!                      COORDINATES WITH RESPECT TO GEODETIC
!                      COORDINATES


      CALL PLHOUT(SATP,PHI,COSPHI,SINPHI,XLAMB,COSLAM,SINLAM,           &
     &            ALTI,DXMDMP,.TRUE.,.FALSE.)
      XLATD=PHI*180.D0/DACOS(-1.D0)
      XLOND=XLAMB*180.D0/DACOS(-1.D0)
      XLOND=XLOND+720.D0
      XLOND=DMOD(XLOND,360.D0)
!
!
      IJDSEC=MJDSEC+FSEC
      CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
      
!..DTM Models ...DTM Models ...DTM Models ...DTM Models ...DTM Models ...DTM Models ...DTM Models ...DTM Models 
!===============================================================================================================      
      IF(IATDN.EQ.4) THEN
       select case (choose_model)
           ! For IATDN=4 ------ Using DTM87 according to IIS 
           !    choose_model=0 ---- use dtm87
           !    choose_model=1 ---- use dtm2020
           
           
         ! DTM87 case
         case(0)!--------------------------------------------------------------------- case 0 -- DTM87
            if(kentry.eq.1) then
                WRITE(6,*) ' '
                WRITE(6,*) '     * model                           DTM87'
                WRITE(6,*) '          - DAY:     ', DAY
                WRITE(6,*) '          - FLUXIN:  ', FLUXIN
                WRITE(6,*) '          - FLXAVG:  ', FLXAVG
                WRITE(6,*) '          - FLUXKP:  ', FLUXKP
                WRITE(6,*) '          - ALTI:    ', ALTI
                WRITE(6,*) '          - COSHL:   ', COSHL
                WRITE(6,*) '          - SINHL:   ', SINHL
                WRITE(6,*) '          - COSPHI:  ', COSPHI
                WRITE(6,*) '          - SINPHI:  ', SINPHI
                WRITE(6,*) '          - RHO:     ', RHO
                WRITE(6,*) '          - DRHODZ:  ', DRHODZ
                WRITE(6,*) '          - MJDSEC:  ', MJDSEC
                WRITE(6,*) ' '
            endif
            CALL DTM(DAY,FLUXIN,FLXAVG,FLUXKP,    &
               &     ALTI,COSHL,SINHL,COSPHI,     &
               &     SINPHI,RHO,DRHODZ,MJDSEC)
            C(1)=DRHODZ
      
           
         ! DTM2020 Operational case
         case(1)!--------------------------------------------------------------------- case 1 -- DTM2020 Operational
            dtmversion_model='o'
            IMARK = IND3HR
            IKPAP = 1 ! tells FLUXM(1) to contain Kp values
            if(kentry.eq.1) then
                  WRITE(6,*) '     *  model                           DTM2020_oper'
            endif
            !! Allocate the Number density/temperature array to be used with DRIA
            if(kentry.eq.1) allocate(n_dens_temp(10))



            CALL DTM2020_call(MJDSEC,FSEC,                  &
              &               FLUXIN,FLXAVG,FLUXKP,         &
              &               ALTI,PHI,XLAMB,               &
              &               COSHL,SINHL,                &
              &               RHO,DRHODZ, dtmversion_model, &
              &               FLUXM(1),IMARK,IKPAP,I324, n_dens_temp)
            C(1)=DRHODZ

         ! DTM2020 Research case
         case(2)!--------------------------------------------------------------------- case 1 -- DTM2020 Research
            dtmversion_model='r'
            IMARK = IND3HR
            IKPAP = -1  ! tells FLUXM(1) to contain Ap values

            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                           DTM2020_res'
            endif

            !! Allocate the Number density/temperature array to be used with DRIA
            if(kentry.eq.1) allocate(n_dens_temp(10))

            CALL DTM2020_call(MJDSEC,FSEC,                  &
              &               FLUXIN,FLXAVG,FLUXKP,         &
              &               ALTI,PHI,XLAMB,               &
              &               COSHL,SINHL,                &
              &               RHO,DRHODZ, dtmversion_model, &
              &               FLUXM(1),IMARK,IKPAP,I324, n_dens_temp)
            C(1)=DRHODZ

         end select !------------------------------------------------------------------ End dtm model cases
      ENDIF ! end if for DTM Series of Models [IF(IATDN.EQ.4) THEN]





!..MSIS Models ...MSIS Models ...MSIS Models ...MSIS Models ...MSIS Models ...MSIS Models ...MSIS Models ...MSIS
!===============================================================================================================      
      ! MSIS cases
      ! If IATDEN = 5 we go thru this code, depend on the inputted values 
      !    of choose_model to select which model you want
      !          ADDED A CASE SWITCHING OPTION to for the MSIS models!

      IF(IATDN.EQ.5) THEN   !        ....CALL TO MSIS86 option INSTEAD OF DTM
         select case (choose_model)
                   !choose_model=2 ---- msis2
                   !choose_model=6 ---- Kamodo_tiegcm_oc
                ! MSIS 86 case
         case(0)!--------------------------------------------------------------------- case 0 -- MSIS86
            IMARK = IND3HR
            COSHLN = -COSHL
            SINHLN = -SINHL
            IJDSEC=MJDSEC+FSEC
            CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
            IYR=(IYMD/10000.D0)+0.5D0
            CALL MSIS86(DAY,IYR,ALTI,PHI,XLAMB,COSHLN,SINHLN,   &
             &       FLUXIN,FLXAVG,FLUXM(1),IMARK,IKPAP,        & !XKP,INDEX,IKPAP,I324
             &       I324,IDRV,RHO,DRHODZ,IERR)
            C(1)=DRHODZ
            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                           MSIS86'
            endif

         
         ! MSIS 00 case
         case(1)!--------------------------------------------------------------------- case 1 -- MSIS00
            IMARK = IND3HR
            COSHLN = -COSHL
            SINHLN = -SINHL
            IJDSEC=MJDSEC+FSEC
            CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
            IYR=(IYMD/10000.D0)+0.5D0
            CALL MSIS00(DAY,IYR,ALTI,PHI,XLAMB,COSHLN,SINHLN,   &
               &         FLUXIN,FLXAVG,FLUXM(1),IMARK,IKPAP,    &
               &         I324,IDRV,RHO,DRHODZ,IERR,optionsin)
            C(1)=DRHODZ    
            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                           MSISe00'
            endif
                 
                 
         ! MSIS 2 case
         case(2)!--------------------------------------------------------------------- case 2 -- MSIS2
            IMARK = IND3HR
            COSHLN = -COSHL
            SINHLN = -SINHL
            IJDSEC=MJDSEC+FSEC
            CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
            IYR=(IYMD/10000.D0)+0.5D0
            if(kentry.eq.1) allocate(n_dens_temp(11) )

            CALL MSIS2(DAY,IYR,ALTI,PHI,XLAMB,COSHLN,SINHLN,           &
              &        FLUXIN,FLXAVG,FLUXM(1),IMARK,IKPAP,             &
              &        I324,IDRV,RHO,DRHODZ,IERR,optionsin,MJDSEC,FSEC,&
              &        n_dens_temp)
            C(1)=DRHODZ          
            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                           MSIS 2.0'
            endif

         
         ! Kamodo command line (NO LONGER USED-- should return a STOP)
         case(3:5)!--------------------------------------------------------------------- case 3-5 -- Kamodo with Commandline
            ! Use Kamodo command line if case is greater than or equal to 3>=5
            if(choose_model.eq.3) kamodo_model='CTIPe'
            if(choose_model.eq.4) kamodo_model='TIEGCM'
            !if(choose_model.eq.5) kamodo_model='GITM'
                 
            if(kentry.eq.1) WRITE(6,*) '+------- CHECK-- DRAG.f90 -------'
            if(kentry.eq.1) WRITE(6,*) '|                                 '
            if(kentry.eq.1) WRITE(6,*) '|  Using the Command Line method. '
            if(kentry.eq.1) WRITE(6,*) '|     model=', kamodo_model 
            if(kentry.eq.1) WRITE(6,*) '+------- CHECK-- DRAG.f90 ------- '
            STOP 16
       
            !   IJDSEC=MJDSEC+FSEC
            !   CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
            !   IYR=(IYMD/10000.D0)+0.5D0
            !   CALL KamodoModels_cl(MJDSEC,FSEC,ALTI,XLATD,XLOND,    &
            !   &                   PHI,XLAMB,RHO,DRHODZ, DAY,IYR,    &
            !   &                   kamodo_model, model_path,         &
            !   &                   orbitcloud_file )
            !      C(1)=DRHODZ   
            
         ! Kamodo ORBIT CLOUD
         case(6:) !--------------------------------------------------------------------- case 6 -- Kamodo OC
                  ! Use Kamodo_orbit cloud method if case is greater than 6

            if(choose_model.eq.6) kamodo_model='TIEGCM'  
            if(choose_model.eq.7) kamodo_model='HASDM'  
            if(choose_model.eq.8) kamodo_model='CTIPe'  
            if(choose_model.eq.9) kamodo_model='GITM'  

            ! if(kentry.eq.1) WRITE(6,*) '+------- CHECK-- DRAG.f90 -----------------'
            ! if(kentry.eq.1) WRITE(6,*) '|  Using the Orbit Cloud method.           '
            ! if(kentry.eq.1) WRITE(6,*) '|  Expecting a file with initialized orbit '
            ! if(kentry.eq.1) WRITE(6,*) '|  and the orbit uncertainty around it     '
            ! if(kentry.eq.1) WRITE(6,*) '|     model=', kamodo_model 
            ! if(kentry.eq.1) WRITE(6,*) '+------- CHECK-- DRAG.f90 -----------------'
            if(kentry.eq.1) then
               WRITE(6,*) '     *  model                         ', kamodo_model
               WRITE(6,*) '          - Using the Orbit Cloud method. Expecting '
               WRITE(6,*) '            a file with initialized orbit and the   '
               WRITE(6,*) '            orbit uncertainty around it.            '
            endif

            !! Allocate the Number density/temperature array to be used with DRIA
            if(kentry.eq.1) allocate(n_dens_temp(6))

            !! Get the correct year and DOY 
            IJDSEC=MJDSEC+FSEC
            CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
            IYR=(IYMD/10000.D0)+0.5D0
            
            CALL KamodoModels_oc(MJDSEC,FSEC,ALTI,XLATD,XLOND,          &
                              &  PHI,XLAMB,RHO,DRHODZ,DAY,IYR,          &
                              &  kamodo_model,model_path,               &
                              &  orbitcloud_file,n_dens_temp )
            C(1)=DRHODZ
            
            


       end select !------------------------------------------------------------------ End MSIS/Kamodo cases
      ENDIF ! end MSIS86 conditional   [>> IF(IATDN.EQ.5)<<] !    ....CALL TO MSIS86 option INSTEAD OF DTM




      !..JACCHIA 71 case... ...JACCHIA 71 case... ...JACCHIA 71 case... ...JACCHIA 71 case... ...JACCHIA 71 case... 
      IF(IATDN.EQ.2) THEN
       select case (choose_model)
         
         ! jaachia71
         case(0) !--------------------------------------------------------------------- case 0 -- Use jacchia71  
            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                           jaachia71'
            endif

            ! Note- In the F.f90 subroutine, the D71 model was initialized 
            !       by firstcalling IF(LSDRG.AND.ICBDGM.EQ.3) CALL D71TM(MJDSEC,FSEC).
            !       The output from D71TM() is passed to D71() through the COMMON D71CUR
            !       COMMON/D71CUR/DECS,RAS,T3,PHASE,XKP3,EXPKP3
            RHO=D71(RASAT)
            DRHODZ=0.D0
         
         ! JB2008
         case(1) !--------------------------------------------------------------------- case 1 -- Use JB2008
            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                           JB2008'
            endif

            !! Allocate the Number density/temperature array to be used with DRIA
            if(kentry.eq.1) allocate(n_dens_temp(9) )
            
            CALL JB2008_call( MJDSEC,                 &  ! time in integral secs from geodyn ref time
                        &     FSEC,                   &  ! fractional remaining seconds
                        &     ALTI,                   &  ! atitude in meters
                        &     PHI,                    &  ! geodetic latitude in radians (ouptut from PHLOUT.f90)
                        &     XLAMB,                  &  ! geodetic longitude in radians (ouptut from PHLOUT.f90)
                        &     RHO,                    &  ! I/O RHO to be overwritten
                        &     DRHODZ,                 &  ! I/O pertial derivitive to be overwritten
                        &     COSPSI,                 &  ! cosine of geocentric latitude
                        &     SINPSI,                 &  ! sine of geocentric latitude
                        &     LSTINR,                 &  ! denotes OD is on last inner iteration
                        &     n_dens_temp)               ! I/O array of consitituent densities
            
            C(1)=DRHODZ
            


         !  --------------------------------------------------------------------
         !  CASE FOR MANUAL DENSITY READING FROM TEXT FILE
         case(2) 
            if(kentry.eq.1) then
                  WRITE(6,*) '     * model                    MANUAL from txt '
            endif
           
            !! Get the correct year and DOY 
            IJDSEC=MJDSEC+FSEC
            CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
            IYR=(IYMD/10000.D0)+0.5D0
            
            CALL ManualDensityInput(MJDSEC,FSEC,                   &
                   &                RHO,DRHODZ,DAY,IYR,            &
                   &                model_path                     )
            C(1) = 0.D0

         end select !----------------------------------------- End jaachia cases


      ENDIF  ! end jaachia71 [>> IF(IATDN.EQ.2) THEN <<]
      
      


  160 CONTINUE 
  
!#############################################################################
!     
!                      END DENSITY MODEL SAMPLING
!
!#############################################################################


      !!! CHECK THE VALUES OF THE DENSITY and DRHODZ

      if(kentry.eq.1)then
          WRITE(6,*) '     *  rho                             ', RHO
          WRITE(6,*) '     *  drhodz                          ', DRHODZ
      endif
      
      
      ! If rho is a nan, stop the program and tell the user
      if (RHO.NE.RHO) then
        WRITE(6,*) '-----   ERROR in DRAG.f90   -----'
        WRITE(6,*) '           RHO is NAN            '
        WRITE(6,*) ' '
        WRITE(6,*) '           RHO    = ', RHO
        WRITE(6,*) '           DRHODZ = ', DRHODZ
        WRITE(6,*) '           KENTRY = ', KENTRY
        WRITE(6,*) '================================='   
        STOP 16      
      endif
      if (RHO.LT.1.0D-30) then
        WRITE(6,*) '-----   ERROR in DRAG.f90   -----'
        WRITE(6,*) '           RHO is way too small  '
        WRITE(6,*) ' '
        WRITE(6,*) '           RHO    = ', RHO
        WRITE(6,*) '           DRHODZ = ', DRHODZ
        WRITE(6,*) '           KENTRY = ', KENTRY
        WRITE(6,*) '================================='  
        STOP 16        
      endif
      if (RHO.GT.1.D0) then
        WRITE(6,*) '-----   ERROR in DRAG.f90   -----'
        WRITE(6,*) '           RHO is way too big  '
        WRITE(6,*) ' '
        WRITE(6,*) '           RHO    = ', RHO
        WRITE(6,*) '           DRHODZ = ', DRHODZ
        WRITE(6,*) '           KENTRY = ', KENTRY
        WRITE(6,*) '================================='          
        STOP 16
      endif


      
      
      !!! Calculate the Relative velocity vector
      XDOTR=XDOT+THDOT*HTSCAL*Y
      YDOTR=YDOT-THDOT*HTSCAL*X

      GO TO 200
!      
!  not earth not earth not earth not earth not earth not earth not earth 
!
!     ....CALCULATE FOR NON-EARTH PLANETS HERE
!
  170 CONTINUE
!
      IDXREP=ICBDGM-11
      IF(IDXREP.LE.0)IDXREP=1
      IF(ICBDGM.EQ.5.OR.IREPL(IDXREP).EQ.5) THEN
!
!     ....MARS
!
!        DENSITY & VELOCITY FOR MARS
!
           IF(IDRAGM.LE.1) then
            CALL DMARS(VRARAY(9),MJDSEC,FSEC,RHO)
           endif

           IF(IDRAGM.EQ.3) then
            CALL D2MARS(VRARAY(9),MJDSEC,FSEC,RHO)
           endif

           IF(IDRAGM.EQ.2) then
            CALL D1MARS(VRARAY(9),MJDSEC,FSEC,RHO)
           endif

            IF(IDRAGM.EQ.4.OR.IDRAGM.EQ.5) THEN
          DAY=DOY(MJDSEC,FSEC)-ONE
          XSEC=DBLE(MJDSEC)+FSEC
          XDAYS=XSEC-FSCFLX(1)+SECDAY
          NDAYS=XDAYS/SECDAY
          IF(NDAYSB.GT.36) THEN
             XDAYS=XSEC-FSCFLX(1+NTOT)+SECDAY
             NDAYS=XDAYS/SECDAY
          ENDIF

          FLXAVG=AVGFLX(NDAYS)

           IF(IDRAGM.EQ.4) then
          CALL D3MARS(VRARAY(9),MJDSEC,FSEC,FLXAVG,RHO)

           ELSE IF(IDRAGM.EQ.5) then
          CALL D4MARS(VRARAY(9),MJDSEC,FSEC,FLXAVG,RHO)

           endif
           end if
         XDOTR=XDOT+THDXT*HTSCAL*Y
         YDOTR=YDOT-THDXT*HTSCAL*X
!        ....C(1) IS D( RHO ) / DZ  ( SEE DTM SUBROUTINE )
         C(1)=ZERO
!
      ELSE IF( ICBDGM .EQ. 2 ) THEN
!
!     ....VENUS
!
!
!        ....choose the selected Venus atmosphere model
!        ....IDRAGM is a flag on the ATMDEN card
!
         IF( IDRAGM .EQ. 1 ) then
!
!---------------------------------------------------------------------
!
!           ....JPL density
!
            if(kentry.eq.1)then
               WRITE(6,*) 'DRAG using JPL Atmosphere Model'
            endif
!
            ALT = ( R  - 6051.D3 ) / D1000
!
!  Check maximum height input by user if less than ALT, set RHO to zero
!
!CC         WRITE(6,*) 'IN DRAG (JPL): XMAXHT = ', XMAXHT
            IF (XMAXHT.GT.ZERO .AND. ALT.GT.XMAXHT) THEN
               RHO = ZERO
!CC            WRITE(6,*) 'IN DRAG: RHO = ', RHO
               GO TO 180
            ENDIF
!
!           ....JPL density for local noon
!cccccccccccRHO = D31213 * exp( ( D250 - alt ) / D18 )
!
!           ....JPL density from S. Nerem 9/27/90
            RHO = D31213 * EXP( ( D230 - alt ) / D25 )
!
            IF( MOD(kentry,2000) .eq. 1 ) THEN
               WRITE(6,*) 'DRAG: JPL Model DAY,ALT ',DAY,ALT
!     1                  ,  ' FLXAVG, F107 ', FLXAVG, F107
!               WRITE(6,*) 'DRAG: JPL Model TLOC, COSPSI,RHO ',
!     1                                     TLOC, COSPSI, RHO
            ENDIF
!
!---------------------------------------------------------------------
!
         else
!
!---------------------------------------------------------------------
!  not earth not earth not earth not earth not earth not earth not earth 
!
!           ....The Venus atmosphere model comes from "Global Empirical
!           ....Model of the Venus Thermosphere", by Hedin, et al., JGR,
!           ....Vol.88, No.A1, pp. 73-83, Jan. 1, 1983
!
            if(kentry.eq.1)then
               WRITE(6,*) 'DRAG using Hedin Atmosphere Model'
            endif
!
!           ....CHECK IATDEN TO STOP IF IT DOES NOT SPECIFY
!           ....CENTERED 81-DAY AVERAGES  (IATDEN = 42)
            IATDN=IATDEN/10
!
!           IF( IATDEN .NE. 42 ) THEN
            IF( IATDEN .EQ. 42 ) THEN
               WRITE(6,*) '  ******************************************'
               WRITE(6,*)                                               &
     &            '  THE DTM MODEL WITH 81-DAY CENTERED AVERAGED FLUX '
               WRITE(6,*)                                               &
     &            '  IS REQUIRED FOR THE VENUS ATMOSPHERE, '
               WRITE(6,*)                                               &
     &            '  BUT HAS NOT BEEN REQUESTED.  IATDEN = ', IATDEN
               WRITE(6,*)                                               &
     &            '  STOPPING PROGRAM IN SUBROUTINE DRAG.'
               WRITE(6,*) '  ******************************************'
               STOP 16
            ENDIF
!
            ALT = ( R  - 6051.D3 ) / D1000
!
!  Check maximum height input by user if less than ALT, set RHO to zero
!
!cc         WRITE(6,*) 'IN DRAG (Hedin): XMAXHT = ', XMAXHT
            IF (XMAXHT.GT.ZERO .AND. ALT.GT.XMAXHT) THEN
               RHO = ZERO
!cc            WRITE(6,*) 'IN DRAG: RHO = ', RHO
               GO TO 180
            ENDIF
!
!           ....PARAMETERS FOR THE HEDIN VENUS MODEL
!
            DAY=DOY(MJDSEC,FSEC)-ONE
            XSEC=DBLE(MJDSEC)+FSEC
            XDAYS=XSEC-FSCFLX(1)+SECDAY
            NDAYS=XDAYS/SECDAY
            IF(NDAYSB.GT.36) THEN
               XDAYS=XSEC-FSCFLX(1+NTOT)+SECDAY
               NDAYS=XDAYS/SECDAY
            ENDIF
!
!
            FLXAVG=AVGFLX(NDAYS)
!
!           ....MAS IS DEFINED = 48
!           ....IDAY0 = Dec 4, 1978
!           IDAY0 = Dec. 4,1978 - Geodyn Interval Ref. Time
!
            TD = MJDSEC / SECDAY
            NORBIT =  TD - IDAY0
            DELTD = D27 * ( D16P08 + DP6165 * NORBIT ) / D360
            DELTD = MOD( DELTD, D27 )
!
            NDAY1 = ( MJDSEC - DELTD*SECDAY                             &
     &                - FSCFLX(1) + SECDAY ) /SECDAY
            IF( NDAY1 .GT.36 ) THEN
               NDAY1 = ( MJDSEC - DELTD*SECDAY                          &
     &                  - FSCFLX(1+NTOT) + SECDAY ) /SECDAY
            ENDIF
!
            NDAY2 = ( MJDSEC - DELTD*SECDAY                             &
     &                - FSCFLX(1) + SECDAY ) /SECDAY  + D27
            IF( NDAY2 .GT.36 ) THEN
               NDAY2 = ( MJDSEC - DELTD*SECDAY                          &
     &                - FSCFLX(1+NTOT) + SECDAY ) /SECDAY  + D27
            ENDIF
!
            IF( NDAY1 .LE. 0 ) THEN
!              MJDSC1 = MJDSEC - DELTD*SECDAY + SECDAY
               MJDSC1 = MJDSEC - 27*SECDAY
!
               CALL  BUFXTR(MJDSC1,FSEC,MJDSEC,FSEC,+1,                 &
     &                      MJDR,FSECR,LRECAL,AA)
!
!              ....SAVE THE PREVIOUS FLUX BUFFER
!
               DO 172 IS=1,36
                  FLUXSO(IS) = FLUXS(IS)
  172          CONTINUE
               FSCFL1 = FSCFLX(1)
!
!              ....NOW RESTORE BUFFERS TO PREVIOUS VALUES
!
               MJDS27 = MJDSEC + 27 * SECDAY
               CALL  BUFXTR(MJDSEC,FSEC,MJDS27,FSEC,+1,                 &
     &                      MJDR,FSECR,LRECAL,AA)
!
            ELSE IF( NDAY2 .LE. 0 ) THEN
!
               MJDSC2 = MJDSEC - DELTD*SECDAY + (ONE+D27) * SECDAY
               CALL  BUFXTR(MJDSC2,FSEC,MJDSEC,FSEC,+1,                 &
     &                      MJDR,FSECR,LRECAL,AA)
!
!              ....SAVE THE PREVIOUS FLUX BUFFER
!
               DO 174 IS=1,36
                  FLUXSO(IS) = FLUXS(IS)
  174          CONTINUE
               FSCFL2 = FSCFLX(1)
!
!              ....NOW RESTORE BUFFERS TO PREVIOUS VALUES
!
               MJDS27 = MJDSEC + 27 * SECDAY
               CALL  BUFXTR(MJDSEC,FSEC,MJDS27,FSEC,+1,                 &
     &                      MJDR,FSECR,LRECAL,AA)
!
            ENDIF
!
            IF( NDAY1 .LE. 0 ) THEN
               NDAY1 = ( MJDSEC - DELTD*SECDAY                          &
     &                - FSCFL1 + SECDAY ) /SECDAY
            IF(NDAY1.le.0) NDAY1=1
               FLXS1 = FLUXSO(NDAY1)
            ELSE
               FLXS1 = FLUXS( NDAY1 )
            ENDIF
!
            IF( NDAY2 .LE. 0 ) THEN
               NDAY2 = ( MJDSEC - DELTD*SECDAY                          &
     &                - FSCFL2 + SECDAY ) /SECDAY + D27
               FLXS2 = FLUXSO(NDAY2)
            ELSE
               FLXS2 = FLUXS( NDAY2 )
            ENDIF
!C          NDAY1 = TD - DELTD
!C          NDAY2 = TD - DELTD + D27
!
!
!           ....F107 IS THE F10.7 FLUX AT EARTH ON DAY TD
!           ....AS COMPUTED BY HEDIN ET AL.
!
            F107 = ( (D27 - DELTD) * FLXS1                              &
     &             + DELTD * FLXS2  ) / D27
!
            XYNORM=SQRT(X**2+Y**2)
            XX=X/XYNORM
            YY=Y/XYNORM
            XYNORM=SQRT(XMNSNS(1,2)**2+XMNSNS(2,2)**2)
            XSUNN(1)=XMNSNS(1,2)/XYNORM
            XSUNN(2)=XMNSNS(2,2)/XYNORM
            COSHL=XX*XSUNN(1)+YY*XSUNN(2)
            SINHL=YY*XSUNN(1)-XX*XSUNN(2)
!
!           ....PARAMETER USED IS LOCAL SOLAR TIME
!           ....LOCAL SOLAR TIME = SOLAR HOUR ANGLE + 12 HOURS
!           ....GET SINE AND COSINE OF (HL+PI)
!
            COSHL=-COSHL
            SINHL=-SINHL
!
            TLOC = ATAN2( SINHL, COSHL ) * D24 / TWOPI
!
!
!           ....COMPUTE HEDIN MODEL
!
            CALL VTS3( ALT, YRD, SINPSI, COSPSI, TLOC,                  &
     &                  FLXAVG, F107, MAS , D, T )
!
!           ....CONVERT FROM GM/CM3 TO KG/M3
            RHO = D(1) * D1000
!
            IF( MOD(kentry,2000) .eq. 1 ) THEN
               WRITE(6,*) 'DRAG: HEDIN DAY,FLXAVG,F107,ALT ',           &
     &                                 DAY,FLXAVG,F107,ALT
               WRITE(6,*) 'DRAG: HEDIN TLOC, COSPSI,RHO ',              &
     &                                 TLOC, COSPSI, RHO
            ENDIF
!
!  not earth not earth not earth not earth not earth not earth not earth 

         endif
  180    continue
!        ....end of Venus atmosphere choosing if-then
!
         XDOTR=XDOT+THDXT*HTSCAL*Y
         YDOTR=YDOT-THDXT*HTSCAL*X
!
!        ....C(1) IS D( RHO ) / DZ  ( SEE DTM SUBROUTINE )
         C(1)=ZERO
!
      ENDIF
!
!  not earth not earth not earth not earth not earth not earth not earth 
!
!
!========================   CONTINUE THE MAIN DRAG ROUTINE   ========================!
!                                 by calcualating CD      
  200 CONTINUE
    if(kin_2.eq.1) WRITE(6,*) ' '
    if(kin_2.eq.1) WRITE(6,*) '      * PREPARE TERMS FOR COMPUTING DRAG CONTRIBUTION'
!     if(kin_2.eq.1) WRITE(6,*) '            ---------------------------------------------'

    !!!! Calculate the s/c vel relative to atmosphere
      VEL2 = XDOTR**2+YDOTR**2+ZDOT**2     
      VEL = SQRT(VEL2)
      C3=VEL*RHO
      IF(IORDRG.GT.1) TM(2)=FSSTRT
      IF(IORDRG.GT.2) TM(3)=TM(2)*TM(2)
      C1=ZERO
      IUP=IORDRG-1
      IF(IUP.LE.0) GO TO 311
      DO 310 I=1,IUP
       if(kin_2.eq.1) WRITE(6,*) '           - 310     ???     '
      C1=C1+C3*B(I)*TM(I)
  310 END DO
  311 IF(IPDDRG.GT.0) GO TO 312                  ! 311 is the no adjustment option
       if(kin_2.eq.1) WRITE(6,*) '           - 311- no drag adjust. option' 
      C1=C1+C3*B(IORDRG)*TM(IORDRG)
       if(kin_2.eq.1) WRITE(6,*) '           - 311- C1=C1+C3*B(IORDRG)*TM(IORDRG)'
       if(kin_2.eq.1) WRITE(6,*) '           - 311- C1        ' , C1
       if(kin_2.eq.1) WRITE(6,*) '           - 311- C3        ' , C3
       if(kin_2.eq.1) WRITE(6,*) '           - 311- IORDRG    ' , IORDRG
       if(kin_2.eq.1) WRITE(6,*) '           - 311- B(IORDRG) ' , B(IORDRG)
       if(kin_2.eq.1) WRITE(6,*) '           - 311- TM(IORDRG)' , TM(IORDRG)
       if(kin_2.eq.1) WRITE(6,*) '           - 311- TM        ', TM 


      GO TO 350
  312 CONTINUE
!
!  FIGURE WHICH PERIOD APPLIES
!
      DO 315 I=1,IPDDRG    !  IPPDRG is the time dep drag periods i think
      IF(TTEST.LT.STDRG(I)) GO TO 316
  315 END DO
      IPDN=IPDDRG
      IPD=0
      GO TO 320
  316 CONTINUE
      IPDN=I-1
      IPD=I
  320 CONTINUE
      T0=ESSTRT
      IF(IPDN.EQ.0) GO TO 340
      DO 330 I=1,IPDN
      TD=STDRG(I)-T0
      CALL POWER(IORDRG,TD,TDN,.FALSE.)
       if(kin_2.eq.1) WRITE(6,*) '           - 320      '
      C1=C1+C3*B(IORDRG+I)*TDN
      T0=STDRG(I)
  330 END DO
  340 CONTINUE
      TD=TTEST-T0
      CALL POWER(IORDRG,TD,TDN,.TRUE.)
       if(kin_2.eq.1) WRITE(6,*) '           - 340- DRAG IS BEING ADJUSTED'
       if(kin_2.eq.1) WRITE(6,*) '           - 340- C1=C1+C3*B(IORDRG+IPD)*TDN'
       if(kin_2.eq.1) WRITE(6,*) '           - 340- C1            ', C1
       if(kin_2.eq.1) WRITE(6,*) '           - 340- B(IORDRG+IPD) ',B(IORDRG+IPD)
                               !####    Need to hunt down where B is calculated
       if(kin_2.eq.1) WRITE(6,*) '           - 340- IORDRG        ',IORDRG
       if(kin_2.eq.1) WRITE(6,*) '           - 340- IPD           ',IPD
       if(kin_2.eq.1) WRITE(6,*) '           - 340- TDN           ', TDN
       !if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90]-340---------------------------'
      C1=C1+C3*B(IORDRG+IPD)*TDN
  350 CONTINUE
! 400 CONTINUE
      RTAREA=1.D0
    
    if(kin_2.eq.1) WRITE(6,*) ' '
    if(kin_2.eq.1) WRITE(6,*) '      * CALCULATE ACCELERATION DUE TO DRAG'
!     if(kin_2.eq.1) WRITE(6,*) '            ----------------------------------'

! CALCULATE ACCELERATION DUE TO DRAG
      !
      ! DETERMINE IF VARIABLE AREA MODEL SHOULD BE USED
      ! NOTE THAT IF LVAREA IS TRUE, THEN THE C1 VALUE WILL BE UPDATED TO
      ! REFLECT THE ACTUAL PROJECTED AREA COMPUTED IN THE VARIABLE AREA MODEL
      ! THEREFORE, TEMPORARILY STORE C1 IN TC1.
      TC1 = C1*RATIO2
 
      if(kin_2.eq.1) WRITE(6,*) '           - TC1     ', TC1
      if(kin_2.eq.1) WRITE(6,*) '           - C1      ', C1
      if(kin_2.eq.1) WRITE(6,*) '           - RATIO2  ', RATIO2

 
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
      !  VARIABLE AREA MODEL (LVAREA=flag for if used)
      IF(LVAREA) THEN   
        ! SELF SHADOWING INDICATOR
        ISHFLG=0
        IF(LSLFSH) THEN                                                     
            ! FIND THE INDEX FOR THIS SATELLITE
            CALL FNDNUM(ISATID,IDSASH,NSHSAT,IRET)
            ! ISHFLG=0 FOR THE DEFAULT MODEL WITHOUT SELF SHADOWING
            ! ISHFLG=1 FOR SELF SHADOWING WITH CROSS_SECTION
            ! ISHFLG=2 FOR SELF SHADOWING WITH RATIO
            IF(IFLGSH(IRET).EQ.10.OR.IFLGSH(IRET).EQ.11) ISHFLG=1      
            IF(ISHFLG.EQ.1 .AND. IATYSH(IRET).EQ.0)      ISHFLG=2     
      ENDIF ! end shadow indicator [IF(LSLFSH) THEN]                                                     
       
       
      ! remove the other values stored in CD and leave only the drag coeff.
      CD_check = (TC1*(2.0D0*SCMASS)/(SCAREA*C3)  )   
      CDprime  = (TC1*(2.0D0*SCMASS)/(SCAREA*C3)  )  !  This is the scaling factor if Scaling is on
      if(kin_2.eq.1) WRITE(6,*) '  '
      if(kin_2.eq.1) WRITE(6,*) '           - pre-check- TC1      ', TC1
      if(kin_2.eq.1) WRITE(6,*) '           - pre-check- CD_check ', CD_check

      IF(optionsin(4).ne.2 ) THEN  ! BWDRAG-------------------------------------- Pygeodyn options indicate to use BWDRAG
            
            ! AA(KSDIND) is the interpolated cross-section or ratio which is calculated in EXTATT.f
            CALL BWDRAG(AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),        &
                  &     VEL,XDOTR,YDOTR,ZDOT,SCAREA,TC1,         &
                  &     AA(JBWARE),DXDD,PXDDT,JARADJ,II(JARUA),  &
                  &     NFACE,NEQN,TOTARE,ISHFLG,AA(KSDIND))
            
            if(kin_2.eq.1) WRITE(6,*) '      * BWDRAG Outputs'
            if(kin_2.eq.1) WRITE(6,*) '           - TC1        ', TC1
            if(kin_2.eq.1) WRITE(6,*) '           - SCMASS     ', SCMASS
            if(kin_2.eq.1) WRITE(6,*) '           - TOTARE     ', TOTARE
            if(kin_2.eq.1) WRITE(6,*) '           - C3=VEL*RHO ', C3
            
            ! remove the other values stored in CD and leave only the drag coeff.
            CD_drag = (TC1*(2.0D0*SCMASS)/(TOTARE*C3)  )   
            if(kin_2.eq.1) WRITE(6,*) '           - CD_drag ', CD_drag



              !!!! If GEODYN is told to use Var.Area and pygeodyn says to use DRIA, then use the DRIA model
      ELSE IF(optionsin(4).eq.2) THEN  ! DRIA--------- Pygeodyn options indicate to use Modified DRIA Model
            
            if(kentry.eq.1) WRITE(6,*) '  '
            if(kentry.eq.1) WRITE(6,*) ' ======= ======= ======= ======= ======= ======= '
            if(kentry.eq.1) WRITE(6,*) ' =======  RUNNING DRIA CD PHYSICS MODEL  ======= '
            !!!! SUBROUTINE TO CALL THE DRIA MODEL ----------------------------------
            !   TW       A      I    SATELLITE PLATE TEMPERATURES
            !   MS       A      I    MOLAR MASS FOR EACH PANEL (g/mol)
            !   ALPHA    S      I    ENERGY ACCOMMODATION COEFFICIENT
            !   MEANMOL  S      I    MEAN MOLAR MASS OF THE ATMOSPHERE
            !   TEMP     S      I    AMBIENT TEMPERATURE
            !   NOXA     S      I    NUMBER DENSITY OF OXYGEN (m^-3)
            !   KL       S      I    LANGMUIR PARAMETER
            !   FRACOX   S      I    FRACTION OF SURFACE COVERED BY ATOMIC OXYGEN
            !   CD       S      O    DRAG-COEFFICIENT
         !===========================================================================================================   
         !===========================================================================================================
                  
         IF(IATDN.EQ.4) THEN  !  DTM case ------------------------------------------------------ BEGIN DTM DRIA
            select case (choose_model)
              
            case(0) !----------------------------------------------------------- dtm87 dria
                if(kentry.eq.1) WRITE(6,*) '***** ERROR *****'
                if(kentry.eq.1) WRITE(6,*) 'DRIA not setup to work with DTM87'
                if(kentry.eq.1) WRITE(6,*) '***** ERROR *****'
                STOP 16
                
                
            case(1) !-------------------------------------------------------------- DTM2020 dria
                if(kentry.eq.1) WRITE(6,*) ' =======       WITH DTM2020'
                
                ! Atmospheric temperature at satellite altitude
                TEMP_atm = n_dens_temp(8) !   temperature at altitude
                ! Number of atomic oxygens in atmosphere
                NOXA_atm = n_dens_temp(3) !   ! X(3) = n_den atomic oxygen (in #/m^3)
                ! Calculate the Mean Molecular Mass
                MEANMOL_atm = ((n_dens_temp(1)*H_amu)  +  & 
                         &     (n_dens_temp(2)*He_amu) +  &
                         &     (n_dens_temp(3)*O_amu)  +  & 
                         &     (n_dens_temp(4)*N2_amu) +  &
                         &     (n_dens_temp(5)*O2_amu) +  & 
                         &     (n_dens_temp(6)*N_amu) ) / & 
                         &           (n_dens_temp(1) +  & 
                             &        n_dens_temp(2) +  & 
                             &        n_dens_temp(3) +  & 
                             &        n_dens_temp(4) +  & 
                             &        n_dens_temp(5) +  & 
                             &        n_dens_temp(6) )    

               if(kentry.eq.1) WRITE(6,*) ' [DRAG.f90] - MEANMOL  ',  MEANMOL
               if(kentry.eq.1) WRITE(6,*) ' [DRAG.f90] - TEMP     ',  TEMP
               if(kentry.eq.1) WRITE(6,*) ' [DRAG.f90] - NOXA     ',  NOXA

                        
            end select 

         ENDIF  ! end jb2008 case for DRIA [>> IF(IATDN.EQ.2)---------------------------------------- end JB2008 DRIA 
         !===========================================================================================================         
         
         
         
         IF(IATDN.EQ.5) THEN  !  MSIS case ---------------------------------------------------------- BEGIN MSIS DRIA 
            select case (choose_model)
              
              
              case(2)  !------------------------------------------------------- MSIS2 dria
                  if(kentry.eq.1) WRITE(6,*) ' =======       WITH MSIS2'
                  ! Atmospheric temperature at satellite altitude
                  TEMP_atm = n_dens_temp(11)
                  ! Number of atomic oxygens in atmosphere
                  NOXA_atm = (n_dens_temp(2) + n_dens_temp(9))*0.000001D0 !convert to SI  

                  !    Calculate the Mean Molecular Mass of the atmosphere
                  MEANMOL_atm=((n_dens_temp(1)*He_amu) + &  ! He nden (cm-3)
                       &      (n_dens_temp(2)*O_amu)   +  & ! O nden (cm-3)
                       &      (n_dens_temp(3)*N2_amu)  +  & ! N2 nden (cm-3)
                       &      (n_dens_temp(4)*O2_amu)  +  & ! O2 nden (cm-3)
                       &      (n_dens_temp(5)*Ar_amu)  +  & ! Ar nden (cm-3)
                       &      (n_dens_temp(7)*H_amu)   +  & ! H nden (cm-3)
                       &      (n_dens_temp(8)*N_amu)   +  & ! N nden (cm-3)
                       &      (n_dens_temp(9)*O_amu))  /  & ! AnomO nden (cm-3) 
                       &         (n_dens_temp(1) +  &
                       &          n_dens_temp(2) +  &
                       &          n_dens_temp(3) +  &
                       &          n_dens_temp(4) +  &
                       &          n_dens_temp(5) +  &
                       &          n_dens_temp(7) +  &
                       &          n_dens_temp(8) +  &
                       &          n_dens_temp(9))
                       
                       
              case(6:)  !------------------------------------------------------- Kamodo TIEGCM dria  
                        ! Use Kamodo_orbit cloud method if case is greater than 6
                  if(kentry.eq.1) WRITE(6,*) ' =======       WITH TIEGCM Kamodo'
                                !  'rho_interpd      ',  n_dens_temp(1)
                                !  'ndenO1_interpd   ',  n_dens_temp(2)
                                !  'ndenO2_interpd   ',  n_dens_temp(3)
                                !  'ndenHE_interpd   ',  n_dens_temp(4)
                                !  'ndenN2_interpd   ',  n_dens_temp(5)
                                !  'Temp_interpd     ',  n_dens_temp(6)                  
                  ! Atmospheric temperature at satellite altitude
                  TEMP_atm = n_dens_temp(6)
                  ! Number of atomic oxygens in atmosphere
                  NOXA_atm = (n_dens_temp(2))*0.000001D0 !convert to meters  
                  ! Calculate the Mean Molecular Mass
                  MEANMOL_atm = ((n_dens_temp(4)*He_amu) +  &  ! He nden (cm-3)
                       &         (n_dens_temp(2)*O_amu)  +  &  ! O1 nden (cm-3)
                       &         (n_dens_temp(5)*N2_amu) +  &  ! N2 nden (cm-3)
                       &         (n_dens_temp(3)*O2_amu)) / &  ! O2 nden (cm-3) 
                       &            (n_dens_temp(2) +  &
                       &             n_dens_temp(3) +  &
                       &             n_dens_temp(4) +  &
                       &             n_dens_temp(5) )
              end select
              if(kentry.eq.1) WRITE(6,*) ' ======= ======= ======= ======= ======= ======= '
        
         endif  !  END MSIS case [>>IF(IATDN.EQ.5) THEN<<] -------------------------------------------- END MSIS DRIA    


         !===========================================================================================================
         
         
         IF(IATDN.EQ.2) THEN  !  JB2008 case ------------------------------------------------------ BEGIN JB2008 DRIA
            select case (choose_model)
              
            case(0) !----------------------------------------------------------- jacchia71 dria
                if(kentry.eq.1) WRITE(6,*) '***** ERROR *****'
                if(kentry.eq.1) WRITE(6,*) 'DRIA not setup to work with Jaachia71'
                if(kentry.eq.1) WRITE(6,*) '***** ERROR *****'
                STOP 16
                
                
            case(1) !-------------------------------------------------------------- JB2008 dria
                if(kentry.eq.1) WRITE(6,*) ' =======       WITH JB2008'
                
                ! Atmospheric temperature at satellite altitude
                TEMP_atm = n_dens_temp(9) !   TEMP(2): Temperature at Input Position (deg K)
                ! Number of atomic oxygens in atmosphere
                NOXA_atm = n_dens_temp(3) !   O !molecules/m**3)
                ! Calculate the Mean Molecular Mass
                MEANMOL_atm = ((n_dens_temp(1)*N2_amu) +  & ! N2 [molecules/m**3]
                         &     (n_dens_temp(2)*O2_amu) +  & ! O2 [molecules/m**3]
                         &     (n_dens_temp(3)*O_amu)  +  & ! O  [molecules/m**3]
                         &     (n_dens_temp(4)*Ar_amu) +  & ! Ar [molecules/m**3]
                         &     (n_dens_temp(5)*He_amu) +  & ! He [molecules/m**3]
                         &     (n_dens_temp(6)*H_amu) ) / & ! H  [molecules/m**3]
                         &            (n_dens_temp(1)+ & 
                             &        n_dens_temp(2) +  & 
                             &        n_dens_temp(3) +  & 
                             &        n_dens_temp(4) +  & 
                             &        n_dens_temp(5) +  & 
                             &        n_dens_temp(6) )    
                       
            end select 

         ENDIF  ! end jb2008 case for DRIA [>> IF(IATDN.EQ.2)---------------------------------------- end JB2008 DRIA 
         !===========================================================================================================
         if(kin_2.eq.1) WRITE(6,*) ' '
         if(kin_2.eq.1) WRITE(6,*) ' [DRAG_b4_DRIA] - TC1   ', TC1
         CD_drag=0.0D0
         
         if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] CALL MODDRIA'
         if(kin_2.eq.1) WRITE(6,*) '            ------------'

         !!!! In this case, TC1 is overwritten as the CLL-GSI drag coefficient
         CALL MODDRIA(AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),         &
               &      NFACE, VEL, XDOTR, YDOTR, ZDOT,           &
               &      AA(JBWARE),MEANMOL_atm,TEMP_atm,NOXA_atm, &
               &      CD_drag,DXDD,PXDDT,JARADJ,II(JARUA),NEQN, &
               &      ISHFLG,AA(KSDIND),B0,B,SCAREA,RHO,        &
               &      TC1,TOTARE, kin_2, CDprime,               &
               &      Ldrag_ScalingFactor, SPEED_RATIO,         &
               &                   ctheta_1, ctheta_2, ctheta_4, ctheta_14    )
          
         
         
         if(kin_2.eq.1) WRITE(6,*) ' [DRAG aft DRIA]- TC1           ', TC1
         if(kin_2.eq.1) WRITE(6,*) ' [DRAG aft DRIA]- CD updated  ', CD_drag
         ! the DXDD, PXDDT, and TC1 were all updated in MODDRIA
            

      end if ! end if for LVAREA to use either BWDRAG or MDORIA                                    end variable area model
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

      ELSE
        DXDD(1)=-TC1*XDOTR
        DXDD(2)=-TC1*YDOTR
        DXDD(3)=-TC1*ZDOT
      ENDIF                                                         
      
    ! just an example for ref.  
    ! IJDSEC=MJDSEC+FSEC
    ! CALL MJDYMD(IJDSEC,IYMD,IHMS,4)

    ! DEBUG PRINT THE DATE
    ! WRITE(6,*) ' [Date]  YYMMDD HHMMSS FSSTRT  ', IYMD, IHMS, FSSTRT
    ! WRITE(6,*) '         rho drhodz  ', RHO, DRHODZ

    if(kin_2.eq.1) WRITE(6,*) '      * Printing to DENSITY/DRAG files    '

! *****  PRINT THE DENSITY FILE ***********************************
      IF(LSTINR) THEN

        WRITE(99,7000) FSSTRT,IYMD,IHMS,         &  !  FSSTRT -- Elapsed seconds since initial epoch  
        &              XLATD,XLOND,ALTI,         &  !  IYMD   -- YYMMDD of current epoch  
        &              RHO,DRHODZ,               &  !  IHMS   -- HHMMSS of current epoch  
        &              FLUXIN,FLXAVG,FLUXM(1)       !  XLATD -- Lat of Space craft (degrees)  
        !                                           !  XLOND -- Lon of Space craft (degrees)  
7000   FORMAT(  F12.1,1X, 2(I0.6,1X),            &  !  ALTI -- Hieght of Space station above elipsoid (M)  
       &        2(F12.4,1X), F12.3,1X,           &  !  RHO  -- Density of atmosphere (KG/M^3)  
       &        2(D20.11,1X),                    &  !  DRHODZ -- Change in density of atmosphere with height (KG/M^3/M)  
       &        3(F6.2,1X)     ) 
      ENDIF                                                           
!***************************************************************************



!! ****   PRINT THE CD/DRAG FILE ******       # This still has the dates/data saved out at least twice,
!      IF(LSTINR) THEN
!        WRITE(103,7001) FSSTRT,IYMD,IHMS,              & 
!        !&              XLATD,XLOND,ALTI,               &  
!        &              TC1,CD_drag, TOTARE,             &  
!        &              DXDD(1),DXDD(2),DXDD(3)             
!        !                                                 
!7001   FORMAT(  F12.1,1X, 2(I0.6,1X),                  &  
!       !&        2(F12.4,1X),F12.3,1X,                 &  
!       &        3(D20.11,1X),                          &  
!       &        3(D20.11,1X)     ) 
!      ENDIF           
!
!--------------------------------------------------------------------------------------------------------
! ****   PRINT THE DRAG FILE and Spacecraft obervatory file
      IF(LSTINR) THEN
        WRITE(103,7003) IYMD,IHMS,                                & 
        &              CD_drag,TOTARE,VEL, SPEED_RATIO,           &  
        &              ctheta_1, ctheta_2, ctheta_4, ctheta_14
      !   &              DXDD(1),DXDD(2),DXDD(3)             
        !                                                 
7003   FORMAT(  2(I0.6,1X),      &  
       &        4(D20.11,1X),    & 
      !  &        3(D20.11,1X)     &
       &        4(D20.11,1X)     &
       &     ) 
       
       
       intFACE = int( NFACE )
       
       
       if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] AA(JTDNRM)',AA(JTDNRM)
       if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] AA(JTDNR2)',AA(JTDNR2)
       if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] AA(JTDNR3)',AA(JTDNR3)
       if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] AA(JBWARE)',AA(JBWARE)

       
       WRITE(104,7004) IYMD,IHMS,       &   !2 integers
                        !! Arrays with length NFACE containing [XYZ] components of panel Normal vectors in True of Date
        &               AA(JTDNRM),      &   
        &               AA(JTDNR2),      &
        &               AA(JTDNR3),      &  
        &               AA(JBWARE)         ! Flat plate areas
        !
        !                                                 
7004   FORMAT(  2(I0.6,1X),          &  
       &        14(D20.11,1X),    &              
       &        14(D20.11,1X),    &              
       &        14(D20.11,1X),    &              
       &        14(D20.11,1X)    )
       !
      ENDIF 
!***************************************************************************

! End drag routine if doing an orbgen run (simple orbit propagation.)
      IF(LORBIT) RETURN


!
! CALCULATE V-MATRIX IN TRUE OF DATE TIME
!
    if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] CALCULATE V-MATRIX IN TRUE OF DATE TIME'
    if(kin_2.eq.1) WRITE(6,*) '            ---------------------------------------'
      C1P=-TC1/VEL2
       if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - C1P=-TC1/VEL2         ', C1P
       if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - TC1                   ', TC1

!
! CALCULATE VELOCITY PART OF V-MATRIX
!
      PARTII(1,1)=XDOTR*XDOTR*C1P-TC1
      PARTII(2,2)=YDOTR*YDOTR*C1P-TC1
      PARTII(3,3)=ZDOT*ZDOT*C1P-TC1
      PARTII(1,2)=XDOTR*YDOTR*C1P
      PARTII(1,3)=XDOTR*ZDOT*C1P
      PARTII(2,3)=YDOTR*ZDOT*C1P
      PARTII(2,1)=PARTII(1,2)
      PARTII(3,1)=PARTII(1,3)
      PARTII(3,2)=PARTII(2,3)
!
! SUM IN POSITION PART OF V-MATRIX COMING FROM ATMOSPHERIC VELOCITY
!
      IF(ICBDGM.NE.ITBDGM) THEN
         THETMP=THDXT
      ELSE
         THETMP=THDOT
      ENDIF
      VMATRX(1,1)=VMATRX(1,1)-PARTII(1,2)*THETMP
      VMATRX(2,1)=VMATRX(2,1)-PARTII(2,2)*THETMP
      VMATRX(3,1)=VMATRX(3,1)-PARTII(3,2)*THETMP
      VMATRX(1,2)=VMATRX(1,2)+PARTII(1,1)*THETMP
      VMATRX(2,2)=VMATRX(2,2)+PARTII(2,1)*THETMP
      VMATRX(3,2)=VMATRX(3,2)+PARTII(3,1)*THETMP
!
! CALCULATE POSITION PART OF V-MATRIX COMING FROM
! CHANGE IN ATMOSPHERIC DENSITY
!
! FIRST CALCULATE DHT/DX
!
      TERM=-4.D0*FSQ32*SPSISQ*SPSISQ+2.D0*FFSQ32
      TERM=TERM*SINPSD/RSQ
      DHTDX(1)=X/R+TERM*(-X)*SINPSD
      DHTDX(2)=Y/R+TERM*(-Y)*SINPSD
      DHTDX(3)=SINPSD+TERM*(X**2+Y**2)/R
!
! CALCULATE (DACC/DDENS)*(DDENS/DHT)
!
      TEMP(1)=DXDD(1)*C(1)    ! C(1) is DRHODZ
      TEMP(2)=DXDD(2)*C(1)
      TEMP(3)=DXDD(3)*C(1)
!
! FINALLY SUM IN POSITION PART OF V-MATRIX COMING FROM
! CHANGE IN ATMOSPHERIC DENSITY
!
      DO 460 I=1,3
      DO 460 J=1,3
      VMATRX(I,J)=VMATRX(I,J)+TEMP(I)*DHTDX(J)
  460 VMATRX(I,J+3)=VMATRX(I,J+3)+PARTII(I,J)
!
! GET EXPLICIT PARTIALS IF DRAG ADJUSTED
    if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] GET EXPLICIT PARTIALS IF DRAG ADJUSTED'
    if(kin_2.eq.1) WRITE(6,*) '            --------------------------------------'
!
!  XDRREF IS THE RELATIVE VELOCITY VECTOR IN TRUE OF REFERENCE
!
      XDRREF(1)=RMI(1)*XDOTR+RMI(2)*YDOTR+RMI(3)*ZDOT
      XDRREF(2)=RMI(4)*XDOTR+RMI(5)*YDOTR+RMI(6)*ZDOT
      XDRREF(3)=RMI(7)*XDOTR+RMI(8)*YDOTR+RMI(9)*ZDOT
!
!  CALCULATE EXPLICIT PARTIALS
!
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - C3=VEL*RHO       ', C3
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - B0=.5*AREA/MASS  ', B0
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - RATIO2           ', RATIO2

      C2=-C3*B0*RATIO2
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - C2=-C3*B0*RATIO2 ', C2
      !WRITE(6,*) '     [drag.f90] - CDprime = ', CDprime


     !!!!   ZACH -- Added CDprime (the scaling factor) back into the drag acceleration
     !!!!           Now we must include CD-DRIA into the partials
     !!!!           this has the effect of actively scaling the drag acceleration by the scaling factor
     !!!!           and the partials include CD_dria since the derivitivbe is wrt CDprime

      if(Ldrag_ScalingFactor) then
          scaleCD_term = CD_drag
      else
          scaleCD_term=1.D0
      endif
      
      IF(LVAREA) THEN
      C2=(C2*RTAREA*TOTARE/SCAREA)*scaleCD_term
      ELSE
      C2=(C2*RTAREA)*scaleCD_term
      ENDIF
      
      if(kin_2.eq.1) WRITE(6,*) '   '
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - C2 again        ', C2
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - RTAREA        ', RTAREA
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - TOTARE        ', TOTARE
      if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] - SCAREA        ', SCAREA

!
      KPART=JSADRG(1)
      DO 750 J=1,IORDRG
      IF(.NOT.LSADRG(J)) GO TO 750
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] - PARTIALS 1 '
      PXDDT(KPART,1)=C2*XDRREF(1)*TM(J)
      PXDDT(KPART,2)=C2*XDRREF(2)*TM(J)
      PXDDT(KPART,3)=C2*XDRREF(3)*TM(J)
      KPART=KPART+1
  750 END DO
!
      IF(.NOT.LSADRG(4)) RETURN
      IF(.NOT.LSADRG(IORDRG)) GO TO 760
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] - PARTIALS 2 '
      PXDDT(KPART-1,1)=ZERO
      PXDDT(KPART-1,2)=ZERO
      PXDDT(KPART-1,3)=ZERO
  760 CONTINUE
!
      KPARTS=KPART
      DO 800 I=1,IPDDRG
      IF(.NOT.LAJDP(I)) GO TO 800
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -760- PARTIALS- period not adjusted?  '
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -760- I              ',I
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -760- LSADRG(IORDRG)                    ', LSADRG(IORDRG)
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -760- LAJDP(I)                          ', LAJDP(I)

      PXDDT(KPART,1)=ZERO
      PXDDT(KPART,2)=ZERO
      PXDDT(KPART,3)=ZERO
      KPART=KPART+1
  800 END DO
      KPART=KPARTS
      T0=ESSTRT
      IF(IPDN.LE.0) GO TO 900
      DO 850 I=1,IPDN
      IF(.NOT.LAJDP(I)) GO TO 840
      TD=STDRG(I)-T0
      CALL POWER(IORDRG,TD,TDN,.FALSE.)
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] - PARTIALS 4 '
      PXDDT(KPART,1)=C2*XDRREF(1)*TDN
      PXDDT(KPART,2)=C2*XDRREF(2)*TDN
      PXDDT(KPART,3)=C2*XDRREF(3)*TDN
      KPART=KPART+1
  840 CONTINUE
      T0=STDRG(I)
  850 END DO
  900 CONTINUE
      IF(IPD.GT.0) GO TO 910
      IF(.NOT.LSADRG(IORDRG)) RETURN
      KPART=KPARTS-1
      GO TO 920
  910 CONTINUE
      IF(.NOT.LAJDP(IPD)) RETURN
  920 CONTINUE
      TD=TTEST-T0
      CALL POWER(IORDRG,TD,TDN,.TRUE.)
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -920- PARTIALS- period has been adjusted  '
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -920- IORDRG         ',IORDRG
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -920- LSADRG(IORDRG)                    ', LSADRG(IORDRG)
      if(kin_2.eq.1) WRITE(6,*) '[DRAG.f90] -920- LAJDP(IPD)                        ', LAJDP(IPD)
      PXDDT(KPART,1)=C2*XDRREF(1)*TDN
      PXDDT(KPART,2)=C2*XDRREF(2)*TDN
      PXDDT(KPART,3)=C2*XDRREF(3)*TDN
!1000 CONTINUE

      if(kin_2.eq.1) WRITE(6,*) '  '   
      if(kin_2.eq.1) WRITE(6,*) ' ========================'
      if(kin_2.eq.1) WRITE(6,*) ' RETURN FROM DRAG ROUTINE'
      if(kin_2.eq.1) WRITE(6,*) ' ========================'

      RETURN


      END
