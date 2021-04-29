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
      DIMENSION IATDIM(5)
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
!        5  - HE
!        6  - N
!        7  - N2
!      T(1) - EXOSPHERIC TEMPERATURE
!        2  - TEMPERATURE AT ALTITUDE
!
      DIMENSION D(7),T(2)
!
      DATA IATDIM/1,1,1,8,8/
      DATA KENTRY/0/,SINHL/0.D0/,COSHL/0.D0/
      DATA IKPAP/1/,I324/3/,IDRV/1/
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
      KENTRY = KENTRY + 1
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

! IF IATDEN=41,42,43  USE FRENCH DENSITY MODEL; ELSE USE JACCHIA MODEL
! IATDEN MUST BE DIVIDED BY 10 BECAUSE IT IS NOW A TWO DIGIT NUMBER
! ONE OF THE FOLLOWING: 10,20,30,41,42,43
!
      IATDN=IATDEN/10
      IFLXDM=IATDIM(IATDN)
!     
      if(kentry.eq.1)then  
          WRITE(6,*) 'IN Drag.f90 IATDEN = ', IATDEN
          WRITE(6,*) 'IN Drag.f90 IATDN = ', IATDN
      endif
!
!      IF(IATDN.NE.4.AND.IATDN.NE.5) GOTO 150
!
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
!
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
      IF(IATDN.EQ.4) THEN
!
!        ....CALL DTM MODEL
!
         CALL DTM(DAY,FLUXIN,FLXAVG,FLUXKP,ALTI,COSHL,SINHL,COSPHI,     &
     &         SINPHI,RHO,DRHODZ,MJDSEC)
         C(1)=DRHODZ
!
         if(kentry.eq.1)then
              WRITE(6,*) 'CHECK DRAG.f90: DENSITY MODEL IS dtm87'
         endif
      ENDIF
      IF(IATDN.EQ.5) THEN
!
!        ....CALL TO MSIS INSTEAD OF DTM
!
         IMARK = IND3HR
         COSHLN = -COSHL
         SINHLN = -SINHL
         IJDSEC=MJDSEC+FSEC
         CALL MJDYMD(IJDSEC,IYMD,IHMS,4)
         IYR=(IYMD/10000.D0)+0.5D0
         CALL MSIS(DAY,IYR,ALTI,PHI,XLAMB,COSHLN,SINHLN,FLUXIN,FLXAVG,  &
     &          FLUXM(1),IMARK,IKPAP,I324,IDRV,RHO,DRHODZ,IERR)
         C(1)=DRHODZ
          if(kentry.eq.1)then
              WRITE(6,*) 'CHECK DRAG.f90: DENSITY MODEL IS msis86'
          endif
!
      ENDIF
!
!!!!  GO TO 160
!      GO TO 150
!
!  150 CONTINUE 
!
!     ....JACCHIA 71 MODEL
!
      IF(IATDN.EQ.2) THEN
          RHO=D71(RASAT)
          DRHODZ=0.D0
          if(kentry.eq.1)then
              WRITE(6,*) 'CHECK DRAG.f90: DENSITY MODEL IS jaachia71'
          endif        
      ENDIF
!      RHO=D71(RASAT)
!      DRHODZ=0.D0
!      if(kentry.eq.1)then
!          WRITE(6,*) 'CHECK DRAG.f90: DENSITY MODEL IS jaachia71'
!      endif
!
!
  160 CONTINUE 
      XDOTR=XDOT+THDOT*HTSCAL*Y
      YDOTR=YDOT-THDOT*HTSCAL*X
      IF(LSTINR.AND.FSSTRT.GT.200.D0) THEN
        WRITE(99,7000) FSSTRT,IYMD,IHMS,XLATD,XLOND,ALTI,RHO,DRHODZ,    &
     &                 X,Y,Z,XDOT,YDOT,ZDOT
 7000   FORMAT(F12.1,2I8,2F12.4,F12.3,2D20.11,3F15.3,3F15.6)
      ENDIF
      GO TO 200
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
!
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
!---------------------------------------------------------------------
!
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
!---------------------------------------------------------------------
!
  200 CONTINUE
!
      VEL2 = XDOTR**2+YDOTR**2+ZDOT**2
      VEL = SQRT(VEL2)
      C3=VEL*RHO
      IF(IORDRG.GT.1) TM(2)=FSSTRT
      IF(IORDRG.GT.2) TM(3)=TM(2)*TM(2)
      C1=ZERO
      IUP=IORDRG-1
      IF(IUP.LE.0) GO TO 311
      DO 310 I=1,IUP
      C1=C1+C3*B(I)*TM(I)
  310 END DO
  311 IF(IPDDRG.GT.0) GO TO 312
      C1=C1+C3*B(IORDRG)*TM(IORDRG)
      GO TO 350
  312 CONTINUE
!
!  FIGURE WHICH PERIOD APPLIES
!
      DO 315 I=1,IPDDRG
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
      C1=C1+C3*B(IORDRG+I)*TDN
      T0=STDRG(I)
  330 END DO
  340 CONTINUE
      TD=TTEST-T0
      CALL POWER(IORDRG,TD,TDN,.TRUE.)
      C1=C1+C3*B(IORDRG+IPD)*TDN
  350 CONTINUE
! 400 CONTINUE
      RTAREA=1.D0
!
! CALCULATE ACCELERATION DUE TO DRAG
!
! DETERMINE IF VARIABLE AREA MODEL SHOULD BE USED
! NOTE THAT IF LVAREA IS TRUE, THEN THE C1 VALUE WILL BE UPDATED TO
! REFLECT THE ACTUAL PROJECTED AREA COMPUTED IN THE VARIABLE AREA MODEL
! THEREFORE, TEMPORARILY STORE C1 IN TC1.
      TC1 = C1*RATIO2
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
        ENDIF
! AA(KSDIND) IS THE INTERPOLATED CROSS-SECTION OR RATIO WHICH IS
! CALCULATED IN EXTATT.f
        CALL BWDRAG(AA(JTDNRM),AA(JTDNR2),AA(JTDNR3),                   &
     &              VEL,XDOTR,YDOTR,ZDOT,SCAREA,TC1,                    &
     &              AA(JBWARE),DXDD,PXDDT,JARADJ,II(JARUA),NFACE,       &
     &              NEQN,TOTARE,ISHFLG,AA(KSDIND))
      ELSE
        DXDD(1)=-TC1*XDOTR
        DXDD(2)=-TC1*YDOTR
        DXDD(3)=-TC1*ZDOT
      ENDIF
      IF(LORBIT) RETURN
!
! CALCULATE V-MATRIX IN TRUE OF DATE TIME
!
      C1P=-TC1/VEL2
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
      TEMP(1)=DXDD(1)*C(1)
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
!
!  XDRREF IS THE RELATIVE VELOCITY VECTOR IN TRUE OF REFERENCE
!
      XDRREF(1)=RMI(1)*XDOTR+RMI(2)*YDOTR+RMI(3)*ZDOT
      XDRREF(2)=RMI(4)*XDOTR+RMI(5)*YDOTR+RMI(6)*ZDOT
      XDRREF(3)=RMI(7)*XDOTR+RMI(8)*YDOTR+RMI(9)*ZDOT
!
!  CALCULATE EXPLICIT PARTIALS
!
      C2=-C3*B0*RATIO2
!
      IF(LVAREA) THEN
      C2=C2*RTAREA*TOTARE/SCAREA
      ELSE
      C2=C2*RTAREA
      ENDIF
!
      KPART=JSADRG(1)
      DO 750 J=1,IORDRG
      IF(.NOT.LSADRG(J)) GO TO 750
      PXDDT(KPART,1)=C2*XDRREF(1)*TM(J)
      PXDDT(KPART,2)=C2*XDRREF(2)*TM(J)
      PXDDT(KPART,3)=C2*XDRREF(3)*TM(J)
      KPART=KPART+1
  750 END DO
!
      IF(.NOT.LSADRG(4)) RETURN
      IF(.NOT.LSADRG(IORDRG)) GO TO 760
      PXDDT(KPART-1,1)=ZERO
      PXDDT(KPART-1,2)=ZERO
      PXDDT(KPART-1,3)=ZERO
  760 CONTINUE
!
      KPARTS=KPART
      DO 800 I=1,IPDDRG
      IF(.NOT.LAJDP(I)) GO TO 800
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
      PXDDT(KPART,1)=C2*XDRREF(1)*TDN
      PXDDT(KPART,2)=C2*XDRREF(2)*TDN
      PXDDT(KPART,3)=C2*XDRREF(3)*TDN
!1000 CONTINUE
      RETURN
      END
