!$ROBSD
      SUBROUTINE ROBSD (AA,II,LL,TPARTL,INDSTA,INDSAT,INDSET,           &
     &   INPSTA,INPSAT,LELEVS,MTYPEX,ISEQ,MSEQ,LADD,L1RCVR,ILCORR,      &
     &   SAVEC1,SAVEC2,NM3,ILOOP,XMM,UMV,UM1,UM2, cg_par_array )
!********1*********2*********3*********4*********5*********6*********7**
! ROBSD            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   TPARTL  I/O   A    PARTIALS OF OBSERVATIONS W.R.T TIME
!   INDSTA   I    A    FOR A GIVEN INTERNAL STA NO (1,2,3) INDSTA
!                      GIVES THE ACTUAL LOCATION IN THE STATION
!                      RELATED ARRAYS FOR THIS STATION.
!                      (EG.   STA NO=ISTANO(INDSTA(1,2,OR 3))
!   INDSAT   I    A    FOR A GIVEN INTERNAL SAT NO (1,2,3) INDSAT
!                      GIVES THE ACTUAL LOCATION IN THE SATELLITE
!                      RELATED ARRAYS FOR THIS SATELLITE.
!                      (EG.   SAT ID=ISATNO(INDSAT(1,2,OR 3))
!   INDSET   I    A    FOR A GIVEN INTERNAL SATELLITE NUMBER INDSET TELL
!                      WHICH SAT SET THAT THE GIVEN SAT BELONGS TO.
!   INPSTA   I    A    POINTER ARRAY THAT RELATES THE INTERNAL STATION
!                      NUMBER (1,2,3) TO THE STATIONS DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISTAP=INPSTA(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE STATIONS DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL STATION NO. 1)
!   INPSAT   I    A    POINTER ARRAY THAT RELATES THE INTERNAL SATELLITE
!                      NUMBER (1,2,3) TO THE SATELLITES DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISATP=INPSAT(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE SATELLITES DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL SATELLITE NO. 1)
!   LELEVS   I    A    TELLS WHICH STATION  ELEVATIONS NEED TO BE
!                      COMPUTED FOR MULTI-STATION MEASUREMENTS.(.TRUE.
!                      MEANS COMPUTE THE ELEVATION FOR THIS STATION)
!   MTYPEX
!   ISEQ
!   MSEQ
!   LADD     I    S    CONTROLS WHETHER THE GIVEN SIDE OF A DIFFERENCE
!                      MEASUREMENT IS HANDLED AS POSITIVE(=.TRUE) OR
!                      NEGATIVE(=.FALSE.)
!   L1RCVR   I    S    LOGICAL FLAG FOR RECEIVING STATIONS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      use cgmass_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
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
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/IOBTYP/NDXST(2,35),NDXSAT(2,35),                           &
     &      NDRSTA(50),NDRSAT(50),NDRST2(50),NDRSA2(50),                &
     &      ITMBIT(999),IOETR(999),MTYPED(11),NTYPES,                   &
     &      NM0112(4),NM1314(4),NM1522(4),NM2330(4),                    &
     &      NM3138(4),NM3997(4),NM4098(4),NM9900(4),NMT199(4),NMT203(4),&
     &      NS0112(4),NS1314(4),NS1522(4),NS2330(4),                    &
     &      NS3138(4),NS3997(4),NS4098(4),NS9900(4),NST199(4),NST203(4),&
     &      ITYPD(12,2),ITYPDD(7,2),ILINKF(5),ISTAFN(5,3),              &
     &      ISATFN(5,3),ILINKT(5),ISTATN(5,3),ISATTN(5,3),              &
     &      MGOTO(12),MENTER(12),MEXIT(12),                             &
     &      NDST60(3,24),NDST82(6,12),NDST92(12,7),NTDRSS,              &
     &      NDSA60(3,24),NDSA82(6,12),NDSA92(12,7),                     &
     &      MTDRSS(7),KTDRSS(7),ITDRSS(4,7),JTDRSS(4,7),                &
     &      NLLC60(5,24),NLLC82(10,12),NLLC98(20,7),NNBIA,              &
     &      NXMTYP
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
      DIMENSION TPARTL(NM,2)
      DIMENSION AA(1),II(1),LL(1),INDSTA(3,2),INDSAT(3,2),INDSET(3,2),  &
     &   LELEVS(3,2),INPSTA(3,2),INPSAT(3,2)
      DIMENSION SAVEC1(1),SAVEC2(1)
      DIMENSION ILCORR(5)
      DIMENSION XMM(3)
      DIMENSION UM1(NM,3),UM2(NM,3),UMV(NM,3)

      double precision, dimension( ndim_cgmass, NM )  :: cg_par_array

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! DETERMINE WHICH COMPONENTS MAKE SINGLE DIFFERENCE
      JTYPE=(MTYPEX-59)/2
! DETERMINE IF RECEIVERS ARE DIFFERENT
      J =1
      J2=1
      IF(L1RCVR) GO TO 3000
      MTYPE2=(MTYPEX+1)/2
      J2STA=NDRST2(MTYPE2)
      IF(J2STA.LE.0) GO TO 1000
!...There is a secondary receiving station
      J1STA=NDRSTA(MTYPE2)
      IF(J1STA.EQ.0) GO TO 2000
!...There is a  primary  receiving station
!***************************************************************
!   A pointer error in array JSTANO demonstrated that this code is
!   not correct.  All I*STA and I*SAT values have been changed to
!   J*... variables.   JAM  10/28/91
!     I1STA=INDSTA(J1STA,1)
!     I2STA=INDSTA(J2STA,2)
      IF(J1STA.EQ.J2STA) GO TO 1000
!...They index different block header records.
!        Verify if  station  numbers are really different.
      IF(JSTANO(J1STA).NE.JSTANO(J2STA)) GO TO 2000
!***************************************************************
!...Station numbers are the same or no secondary receiving station
 1000 CONTINUE
      J2SAT=NDRSA2(MTYPE2)
      IF(J2SAT.LE.0) GO TO 3000
!...There is a secondary receiving satellite
      J1SAT=NDRSAT(MTYPE2)
      IF(J1SAT.EQ.0) GO TO 2000
!...There is a  primary  receiving satellite
!***************************************************************
!   A pointer error in array JSTANO demonstrated that this code is
!   not correct.  All I*STA and I*SAT values have been changed to
!   J*... variables.   JAM  10/28/91
!     I1SAT=INDSAT(J1SAT,1)
!     I2SAT=INDSAT(J2SAT,2)
      IF(J1SAT.EQ.J2SAT) GO TO 3000
!...They index different block header records.
!        Verify if satellite numbers are really different.
      IF(JSATNO(J1SAT).EQ.JSATNO(J2SAT)) GO TO 3000
!***************************************************************
!...A secondary receiver exists. Sum observation time tag partials
!     into separate arrays.
 2000 CONTINUE
      J2=2
 3000 CONTINUE
! LOOP THRU POSITIVE AND NEGATIVE SIDES OF DIFFERENCE
      DO 10000 I=1,2
! SET COMPONENT SPECIFIER
      ITYPE=ITYPD(JTYPE,I)
! INCREMENT DIFFERENCE SEQUENCE COUNTER
      ISEQ=ISEQ+1
! COMPUTE AND SUM LINK OBSERVATIONS, PARTIALS, AND CORRECTIONS
!     WRITE(6,*) '  ROBSD : CALL TO ROBS, LELEVS(1,I)  ',I,LELEVS(1,I)
!     WRITE(6,*) '  ROBSD : INDSTA, INDSAT  ',INDSTA(1,I),INDSAT(1,I)
!     WRITE(6,*) '  ROBSD : INPSTA, INPSAT  ',INPSTA(1,I),INPSAT(1,I)
      lyara = .false.
      isnumb = 0
      JTR=1
      IF(ISEQ.GE.3) JTR=2
      CALL ROBS  (AA,II,LL,TPARTL(1,JTR),AA(KSTAIN),II(KN3),II(KNEQN),  &
     &   II(KIPTFM),II(KILNFM),II(KNFMG ),INDSTA(1,I),INDSAT(1,I),      &
     &   INDSET(1,I),INPSTA(1,I),INPSAT(1,I),LELEVS(1,I),               &
     &   ITYPE,ISEQ,MSEQ,LADD,ILCORR,                                   &
     &   LYARA, ISNUMB ,ILOOP,                                          &
     &   II(KTMRA1),II(KTMRA2),AA(KFRSTR),AA(KFRRAT),AA(KPRL1) ,        &
     &   AA(KPRL2),                                                     &
     &   AA(KRL1)  ,AA(KT1SE) ,AA(KTCP)  ,AA(KRATDR),AA(KFQT1S),        &
     &   AA(KFQT1E),AA(KT1STR),AA(KFTTSE),II(KIND1) ,LL(KLRDED),        &
     &   SAVEC1    ,SAVEC2    ,NM3       ,AA(KRDS1L),AA(KFT1AV),        &
     &   AA(KDFQP) ,AA(KFREQ1),AA(KFREQ3),AA(KSAVD1),AA(KSAVD2),        &
     &   XMM,UMV,UM1,UM2, cg_par_array )

! REVERSE ALGEBRAIC SIGN INDICATOR
      LADD=.NOT.LADD
      J=J2
10000 END DO
      RETURN
      END
