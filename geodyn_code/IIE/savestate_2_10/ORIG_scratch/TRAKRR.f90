!$TRAKRR
      SUBROUTINE TRAKRR(XSM,VSM,RNM,URNM,UHAT,RELV,OFFSET,              &
     &                  ELEVSC,FSECRA,U,RA,DRDOT,LASER,ICRSYS,AA,II,    &
     &                  LPTS,XYZOF2,ISET,URNM2,ICOOR,INDSTA,            &
     &                  TPMES,LTPMS,ISATID,MJDSEC,FSECIN,ATROT,LL,      &
     &                  XYZOF3)
!********1*********2*********3*********4*********5*********6*********7**
! TRAKRR           83/07/18            8307.0    PGMR - TOM MARTIN
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE RATE DUE TO
!            LOCATION OF TRACKING POINT NOT AT S/C C.G.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I         EARTH CENTERED INERTIAL POSITION VECTORS
!   VSM      I         EARTH CENTERED INERTIAL VELOCITY VECTORS
!   RNM      I         SLANT RANGE FROM STATION TO SPACECRAFT
!   URNM     I         UNIT INERTIAL TOPOCENTRIC S/C POSITION
!   UHAT     I         UNIT E.C.F. TOPOCENTRIC S/C POSITION
!   RELV     I         S/C E.C.F. RELATIVE VELOCITY
!   OFFSET   I         BODY CENTERED FIXED TRACKING POINT LOCATION
!   ELEVSC   I    A    S/C ELEVATION ANGLE
!   FSECRA
!   U             A    UNIT INERTIAL BODY CENTERED AXES DIR. COSINES
!   RA            A    OFFSET LOCATION MAPPED INTO INERTIAL COORD.
!   DRDOT    I         CORRECTIONS TO RANGE RATES
!   LASER    I           .T.-TRACKING OF LASER REFLECTOR RING
!                        .F.-TRACKING OF ANTENNA
!   ICRSYS   I    S    OFFSET CARD BODY-FIXED COORDINATE SYSTEM
!                         0 = POS & VEL VECTORS
!                         2 = POS & SUN VECTORS
!                         2 = TOPEX
!   XYZOF2   I    A    SECOND OFFSET LINK
!   XYZOF3   I    A    THIRD OFFSET LINK
!   ISET     I    S    SET NUMBER FOR THIS SAT.
!   URNM2    I    A    TOR UNIT VECTOR FROM TRACKING STATION TO S/C
!   MJDSEC   I    S    MJDSEC OF BLOCK START
!   FSECIN   I    A    FRACTIONAL SECODS FOR EACH OBSERVATION
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      use cgmass_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
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
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)
      COMMON/EXATGI/MEASAT,MEAFIL,MEAMEM,MEAPLA,MEAANT,NXEAGI
      COMMON/COFSTL/LOFEXT(2)
      COMMON/OFFASL/LOFFA1,LOFFA2

      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)




      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),RNM(NM),URNM(NM,3),         &
     &   OFFSET(3,2),U(NM,3,3),RA(NM,3),DRDOT(NM),UHAT(NM,3),           &
     &   RELV(NM,3),FSECRA(NM),ELEVSC(1),LPTS(NM)
      DIMENSION AA(1),II(1),LL(1),XYZOF2(3),URNM2(NM,3)
      DIMENSION XYZOF3(3)
      DIMENSION INDSTA(3),LTPMS(MINTIM,3),TPMES(MINTIM,3)
      DIMENSION ATROT(3,3,NM),DUM(1)
      DIMENSION WPU(NM,3,2)
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: RQ1,RQ2,RQ3,RQ4

!cg_par_array
      double precision, dimension( ndim_cgmass, MINTIM ) :: dummy




!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!       print *,'trakrr: start of trakrr'

        ALLOCATE(RQ1(NM),RQ2(NM),RQ3(NM),RQ4(NM))
!
! OBTAIN RANGE CORRECTION
!
        DO 700 N=1,NM
        DO 700 K=1,3
        DO 700 I=1,3
        ATROT(I,K,N)=1.D0
  700   CONTINUE
        !write(6,*)' dbg call to GETRPY TRAKRR ',ISATID
        IF(NASAT.GT.0) THEN
        CALL GETRPY(AA,II,MJDSBL,FSEC,II(KATSAT),II(KKVLAS),AA(KATIME), &
     &              II(KATRSQ),ISATID,0,ATROT,AA(KATPER),ISET1,ROLL,&
     &              PITCH,YAW,TDIF,SINWT,COSWT,.TRUE.,.TRUE.,XSM,VSM)
       ENDIF
!
! COMPUTE OFFSET CORRECTION USING EXTERNAL ATTITUDE ORIENTATION
! IF REQUESTED
!
         IF(LOFEXT(1)) THEN
         IANTNM=ICOOR-ICRSYS*100
         IF((IANTNM.LT.1).OR.(IANTNM.GT.4)) GOTO 9100
! debug extatt
!          print *,'robsum: iset1 is: ',iset1
! debug extatt
          ISBJ20 = II(KEASBJ+ISET-1)
          NUMAEA = II(KEANAN+ISET-1)
          IPANEA = II(KEAPAN+ISET-1)
          MJDSEA = II(KEAMJS+ISET-1)
          FSSCEA = AA(KEAFSS+ISET-1)
          RINTEA = AA(KEAINS+ISET-1)
          LINATO = .FALSE.
        DO I=1,NM
        RQ1(I)=0.D0
        RQ2(I)=0.D0
        RQ3(I)=0.D0
        RQ4(I)=0.D0
        ENDDO
          CALL TRKEXT(AA(KEAQAT),II(KEAAAA),MEAMEM,MEAANT,              &
     &                URNM2,OFFSET,                                     &
     &                XYZOF2,FSECRA,RA,DRDOT,IANTNM,ISBJ20,NUMAEA,      &
     &                IPANEA,MJDSEA,FSSCEA,RINTEA,LINATO,AA,II,         &
     &                ICRSYS,.FALSE.,NM,MJDSEC,ATROT,AA(KOFDRV),        &
     &                LOFFA1,RQ1,RQ2,RQ3,RQ4,.TRUE.,LL(KLANTC),         &
     &                II(KANCUT),.FALSE.,LWND,DUM,1,1,AA(KANTBL),       &
     &                AA(KPHC),XYZOF3,ISATID,AA(KSTAIN),INDSTA,         &
     &                dummy, lpre , .FALSE.)
          DEALLOCATE(RQ1,RQ2,RQ3,RQ4)
         IF(.NOT.LINATO) GOTO 3650
        ENDIF
      IF(ICRSYS .EQ. 1) THEN
           CALL TRAKPC(XSM,VSM,URNM,OFFSET,FSECRA,U,RA,DRDOT,LASER,     &
     &     .TRUE.,LOFFA1,AA(KOFDRV))
      ELSE IF (ICRSYS .EQ. 2) THEN
           CALL TRKTOP(XSM,VSM,URNM,OFFSET,ELEVSC,FSECRA,RA,            &
     &                 DRDOT,LPTS,AA,II,INDSTA,TPMES,                   &
     &                 LTPMS,.FALSE.,1,ISATID,.FALSE.,NM,ATROT,AA(KPHC),&
     &                 .FALSE.,.TRUE.,LOFFA1,AA(KOFDRV),LL(KLANTC),     &
     &                 II(KANCUT),.FALSE.,LWND,DUM,1,1,AA(KSTAIN),      &
     &                 AA(KANTBL))
      ELSE IF (ICRSYS .EQ. 3) THEN
           CALL TRKSPT(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,ISATID,.FALSE.,NM,ATROT,LL(KLANTC),        &
     &                 II(KANCUT),AA(KSTAIN),INDSTA,AA(KPHC),AA(KANTBL))
      ELSE IF (ICRSYS .EQ. 4) THEN
           CALL TRKGPS(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,ISATID,LL(KLANTC),II(KANCUT),NM,ATROT,     &
     &                 AA(KOFDRV),LOFFA1,1,.FALSE.,1,WPU,0,.FALSE.)
      ELSE IF (ICRSYS .EQ. 5 .OR. ICRSYS.EQ.14) THEN
           CALL TRKERS(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,ISATID,.FALSE.,NM,ATROT,LL(KLANTC),        &
     &                 II(KANCUT),.TRUE.,LOFFA1,AA(KOFDRV),AA(KSTAIN),  &
     &                 INDSTA,AA(KPHC),AA(KANTBL))
      ELSE IF (ICRSYS .EQ. 6) THEN
           CALL TRKMO(XSM,VSM,URNM2,OFFSET,FSECRA,RA,DRDOT,             &
     &                 XYZOF2,AA,II,.FALSE.,NM,ATROT,LL(KLANTC),        &
     &                 II(KANCUT),ISATID,AA(KOFDRV),LOFFA1)
      ELSE IF (ICRSYS .EQ. 7 .OR. ICRSYS .EQ. 8) THEN
           CALL TRKTDS(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,.FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),    &
     &                 ISATID,AA(KOFDRV),LOFFA1)
      ELSE IF (ICRSYS .EQ. 9) THEN
           CALL TRKMAG(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,ISATID,.FALSE.,NM,ATROT,LL(KLANTC),        &
     &                 II(KANCUT),AA(KOFDRV),LOFFA1)
      ELSE IF (ICRSYS .EQ. 10) THEN
           CALL TRKGFO(XSM,VSM,URNM,OFFSET,FSECRA,RA,RADOT,             &
     &                 AA,II,ISATID,.TRUE.,LOFFA1,AA(KOFDRV),.FALSE.,   &
     &                 NM,ATROT,LL(KLANTC),II(KANCUT))
      ELSE IF (ICRSYS .EQ. 11) THEN
           CALL TRKTRM(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,ISATID,LL(KLANTC),II(KANCUT),NM,           &
     &                 AA(KOFDRV),LOFFA1)
      ELSE IF (ICRSYS .EQ. 12) THEN
           CALL TRKEUV(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
     &                 AA,II,ISATID,LL(KLANTC),II(KANCUT),NM,           &
     &                 AA(KOFDRV),LOFFA1)

      ELSE IF (ICRSYS .EQ. 13) THEN

!trakrr  CALL TRKVCL(XSM,VSM,URNM,OFFSET,FSECRA,RA,DRDOT,             &
!     &              AA,II,.FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),    &
!     &              ISATID)
!robsum  CALL TRKVCL(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,&
!                    AA,II, .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),&
!                    ISATID)

! trakrr  CALL TRKTOP(XSM,VSM,URNM,OFFSET,ELEVSC,FSECRA,RA,            &
!     &                 DRDOT,LPTS,AA,II,INDSTA,TPMES,                   &
!     &                 LTPMS,.FALSE.,1,ISATID,.FALSE.,NM,ATROT,AA(KPHC),&
!     &                 .FALSE.,.TRUE.,LOFFA1,AA(KOFDRV),LL(KLANTC),     &
!     &                 II(KANCUT),.FALSE.,LWND,DUM,1,1,AA(KANTBL))
!robsum   CALL TRKTOP(XSM,VSM,URNM2,OFFSET,ELEVSC,FSECSA, RA,  &
!                     WORK,LPTS,AA,II,INDSTA,  TPMES1,
!                     LTPMS1,.TRUE.,2,ISATID,.FALSE.,NM,ATROT,  AA(KPHC),
!                     LGPSTP,.TRUE.,LOFFA1,AA(KOFDRV), LL(KLANTC),&
!                     II(KANCUT),LAPWND,L2PWND,W2PU,ISEQ, IDNW,ANTTAB)
           CALL TRKICE(XSM,VSM,URNM,OFFSET,FSECRA,RA,drdot,AA,II,       &
     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,    &
     &                .TRUE.,AA(KPHC),.false., lwnd, dum, 1, 1,         &
     &                 AA(KANTBL), LOFFA1,AA(KOFDRV))


      ELSE IF (ICRSYS .EQ. 15) THEN
           CALL TRKCR2(XSM,VSM,URNM,OFFSET,FSECRA,RA,RADOT,             &
     &                 AA,II,ISATID,.TRUE.,LOFFA1,AA(KOFDRV),.FALSE.,   &
     &                 NM,ATROT,LL(KLANTC),II(KANCUT),AA(KSTAIN),       &
     &                 INDSTA,AA(KPHC),AA(KANTBL))
      ELSE
         RETURN
      ENDIF
 3650 CONTINUE
! ADD RANGE CORRECTION TO E.C.F. STATION-SATELLITE VECTOR
      DO 1000 I=1,3
      DO  800 N=1,NM
      RA(N,I)=UHAT(N,I)*RNM(N)+RA(N,I)
  800 END DO
 1000 END DO
! COMPUTE MAGNITUDE SQUARED OF RESULTANT(CORRECTED RANGE VECTOR)
      CALL DOTPRD(RA,RA,U,NM,NM,NM,3)
! COMPUTE CORRECTED RANGE
      DO 1800 N=1,NM
      DRDOT(N)=SQRT(U(N,1,1))
 1800 END DO
! DIVIDE CORRECTION RANGE VECTOR BY CORRECTED RANGE
      DO 3000 I=1,3
      DO 2800 N=1,NM
      RA(N,I)=RA(N,I)/DRDOT(N)
 2800 END DO
 3000 END DO
! RANGE RATE CORRECTION IS RESULTANT DOT VELOCITY
      CALL DOTPRD(RELV,RA,DRDOT,NM,NM,NM,3)
      RETURN
! ERROR MESSAGES
 9100 CONTINUE
      WRITE(6,91000)
      STOP 16
91000 FORMAT(' TRAKRR: *** ERROR *** CHECK OFFSET CARD.'/               &
     &  'EXTERNAL ATTITUDE REQUESTED BUT NO ANTENNA NUMBER FOUND')
      END
