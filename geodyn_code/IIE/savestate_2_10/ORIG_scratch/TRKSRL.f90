!$TRKSRL
      SUBROUTINE TRKSRL(XSM,VSM,UI,OFFSET,FSECRA,RA,DR,AA,II,LALT,      &
     &                  NDIMRA,ATROT,LANTCT,IANTSC,ISATID,STAINF,       &
     &                  INDSTA,PHC,ANTTAB,LDORANT,DWRKDO,LOFFAJ,LLRASRL)
!********1*********2*********3*********4*********5*********6*********7**
! TRKSRL            11/04/13            1212.1 PGMR - JESSE WIMERT
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!            TRACKING POINT AND S/C C.G. NOT AT BODY-FIXED
!            ORIGIN FOR SARAL
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I         EARTH CENTERED INERTIAL POSITION VECTORS
!   VSM      I         EARTH CENTERED INERTIAL VELOCITY VECTORS
!   UI       I         UNIT TOR TRACKING STATION TO S/C VECTOR
!   OFFSET   I         BODY CENTERED FIXED TRACKING POINT LOCATION
!   FSECRA   I    A    ANTENNA CORR. TIME SINCE BLOCK START
!   RA            A    OFFSET LOCATION MAPPED INTO INERTIAL COORD.
!   DR       O         CORRECTIONS TO RANGES
!   LANTCT   I    A    LOGICAL ANTENA CUTOFF
!   IANTSC   I    A    ANTENNA CUT SATELLITE IDS
!   ISATID   I    S    SATELLITE ID
!   LLRASRL  I    S    TRUE IF SARAL SLR CORRECTION IS SELECTED AND
!                      MTYPE=51
!
!
!***********************************************************************
!
! NOTES:
!            TOD = GEODYN TRUE OF DATE INERTIAL FRAME
!                  rotate to satellite alongtrack, crosstrack, radial sy
!            SPF = SATELLITE ALONGTRACK, CROSSTRACK, RADIAL FRAME
!                  rotate to satellite body-fixed frame (x,y,z)
!            SBF = SATELLITE BODY-FIXED FRAME
!                  apply solar array angle rotations
!            SA  = SOLAR ARRAY BODY-FIXED FRAME
!
!         1) SATELLLITE BODY FIXED FRAME:
!             X = ALONG NADAIR, TOWARDS CENTER OF EARTH
!             Y = ALONG VELOCITY, COMPLETES ORTHONORMAL BASIS
!             Z = OPPOSITE CROSS-TRACK, OPPOSITE ANG MOMENTUM
!          THE SOLAR ARRAY IS FIXED WRT THE SATELITE AND POINTS TOWARDS THE
!          SUN
!
! REFERENCES:
!            LUCA CERRI: " DORIS satellites models implemented  in POE
!            PROCESSING 24/01/2012
!
!*******************************************************************

!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/CANTEN/NCUT,NXANTE
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
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
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)

      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),OFFSET(3,2),                &
     &   RA(NDIMRA,3),DR(NM),FSECRA(NM),UI(NDIMRA,3)
      DIMENSION BDFOFF(3),UNTOFF(3),XPOS(3),VPOS(3)
      DIMENSION AA(1),II(1)
      DIMENSION ATROT(3,3,NM),ROT(9)
      DIMENSION LANTCT(1),IANTSC(1)
      DIMENSION STAINF(NSTAIN,*)
      DIMENSION INDSTA(3)
      DIMENSION DWRKDO(NM,3)
      DIMENSION PHC(NM)
      DIMENSION SLRLRA_CORR(NM)
      DIMENSION ANTTAB(1)
      DIMENSION UNTPOS(3)
      DIMENSION SARAL_LRA_RNG(19)
      DIMENSION SARAL_LRA_HDR(19)

! PARAMETER STATEMENT NOT VALID FOR ARRAY IN SUBROUTINE ARG. LIST.
! IT REMAINS HERE TO SHOW THAT THIS VALUE IS SET IN THIS ROUTINE IN THIS
! RELEASE.  FUTURE VERSIONS WILL GET THIS INFO FROM 2S INPUT
      DATA ZERO/0.0D0/
      DATA ONE/1.0D0/
      DATA L1ST/.TRUE./
      DATA SARAL_LRA_RNG/0.040308D0,0.039357D0,0.037618D0,0.036134D0,   &
     &                   0.035225D0,0.035043D0,0.035693D0,0.036812D0,   &
     &                   0.037884D0,0.038764D0,0.039482D0,0.039618D0,   &
     &                   0.038980D0,0.037723D0,0.035930D0,0.033688D0,   &
     &                   0.031013D0,0.027916D0,0.024408D0/
      DATA SARAL_LRA_HDR/0.0D0,5.0D0,10.0D0,15.0D0,20.0D0,25.0D0,30.0D0,&
     &                 35.0D0,40.0D0,45.0D0,50.0D0,55.0D0,60.0D0,65.0D0,&
     &                 70.0D0,75.0D0,80.0D0,85.0D0,90.0D0/
!!!
!!!   SARAL_LRA_RNG IS FOR A SLR RANGE CORRECTION COMPUTED BY DAVID ARNOLD
!!!   (INCIDENT-DEOEBDEBT SLR RANGE CORRECTION USING CNES DATA)
!!!   TABLE3 FROM DAVID ARNOLD DOCUMENT: NOMINAL SARAL TRANSFER
!!!   FUNCTION
!!!
!!!Incidence angle       Range Corr
!!!         Phi (deg)               (m)
!!!
!!!     0.000   0.040308
!!!     5.000   0.039357
!!!    10.000   0.037618
!!!    15.000   0.036134
!!!    20.000   0.035225
!!!    25.000   0.035043
!!!    30.000   0.035693
!!!    35.000   0.036812
!!!    40.000   0.037884
!!!    45.000   0.038764
!!!    50.000   0.039482
!!!    55.000   0.039618
!!!    60.000   0.038980
!!!    65.000   0.037723
!!!    70.000   0.035930
!!!    75.000   0.033688
!!!    80.000   0.031013
!!!    85.000   0.027916
!!!    90.000   0.024408
!!!
!
!***********************************************************************
! START OF EXACUTABLE CODE *********************************************
!***********************************************************************
!
      !write(6,*)'start trksrl'

      IF(L1ST.AND.XGPSTB.GT.0.D0) THEN
        L1ST=.FALSE.
        WRITE(6,60000)
        WRITE(6,60001)
        WRITE(6,60002)
        WRITE(6,60003)
        WRITE(6,60004)
        WRITE(6,60005)
        WRITE(6,60006)
      ENDIF
!
! COMPUTE BODY FIXED OFFSET UNIT VECTOR
      VMAG = SQRT(OFFSET(1,2)**2+OFFSET(2,2)**2+OFFSET(3,2)**2)
      VMAGR=1.D0
      IF(VMAG.GT..0001D0) VMAGR=1.D0/VMAG
      BDFOFF(1)=OFFSET(1,2)*VMAGR
      BDFOFF(2)=OFFSET(2,2)*VMAGR
      BDFOFF(3)=OFFSET(3,2)*VMAGR
!
! COMPUTE INERTIAL OFFSET UNIT VECTOR
      DO 100 I=1,NM
      POSMAG = SQRT(XSM(I,1)**2+XSM(I,2)**2+XSM(I,3)**2)
      UNTPOS(1) = XSM(I,1)/POSMAG
      UNTPOS(2) = XSM(I,2)/POSMAG
      UNTPOS(3) = XSM(I,3)/POSMAG

      XPOS(1) = XSM(I,1)
      XPOS(2) = XSM(I,2)
      XPOS(3) = XSM(I,3)
      VPOS(1) = VSM(I,1)
      VPOS(2) = VSM(I,2)
      VPOS(3) = VSM(I,3)
!
! ROTATE TOTAL OFFSET VECTOR FROM SBF TO INERTIAL FRAME
! COMPUTE INERTIAL OFFSET VECTOR
!
!
        CALL SARLAT(MJDSEC,FSEC,XPOS,VPOS,BDFOFF(1),BDFOFF(2),BDFOFF(3),&
     &              UNTOFF(1),UNTOFF(2),UNTOFF(3),DUM,1,0,.FALSE.,      &
     &              IDSATS,II(KIDATB),AA(KSABIA),AA(KSBTM1),AA(KSBTM2), &
     &              AA(KVLOUV),II(KNSTLV),II(KSLVID),AA(KTSLOV),AA,     &
     &              ISATID,ROT,ATROT)
!
      IF(LOFFAJ) THEN
         DWRKDO(I,1)=UI(I,1)*ROT(1)                                     &
     &              +UI(I,2)*ROT(2)                                     &
     &              +UI(I,3)*ROT(3)
         DWRKDO(I,2)=UI(I,1)*ROT(4)                                     &
     &              +UI(I,2)*ROT(5)                                     &
     &              +UI(I,3)*ROT(6)
         DWRKDO(I,3)=UI(I,1)*ROT(7)                                     &
     &              +UI(I,2)*ROT(8)                                     &
     &              +UI(I,3)*ROT(9)
      ENDIF

      IF(LALT) THEN
        UI(I,1)=-ROT(1)
        UI(I,2)=-ROT(2)
        UI(I,3)=-ROT(3)
      ENDIF

!!!ADD UNIT 22 STATION PHASE CORRECTION, jtw 20130416
         CALL FNDNUM(MTYPE,NANTMT,10,IDUM)
         IF (IDUM.GT.0) THEN
            IDORTAB=0
            IF(MTYPE.EQ.NANTMT(IDUM)) THEN
            IDORTAB=INT(STAINF(16,INDSTA(2)))
            ITAB=KANTPS(IDORTAB)
                 IF(IDORTAB.GT.0) THEN
                     CALL DORANT(ANTTAB(ITAB+6),ANTTAB(ITAB),    &
     &                    PHC(I),AA(KELEVS+I-1),                 &
     &                    INT(ANTTAB(ITAB+3)),INT(ANTTAB(ITAB)))
           LDORANT=.TRUE.
                 END IF
            END IF
         END IF

! COMPUTE INERTIAL OFFSET VECTOR
      RA(I,1) = UNTOFF(1)*VMAG
      RA(I,2) = UNTOFF(2)*VMAG
      RA(I,3) = UNTOFF(3)*VMAG


! COMPUTE SARAL SLR RANGE CORRECTION
      IF (LLRASRL) THEN
      !! COMPUTE INCIDENCE ANGLE (ui dot r)
       ANGINC=ACOS(UI(I,1)*UNTPOS(1)+UI(I,2)*UNTPOS(2)+UI(I,3)*UNTPOS(3))
       ANGINC=ANGINC/DEGRAD
       !write(6,*)'ANGINC',I,ANGINC
       DO JJ=1,18
       IF (ANGINC.GE.SARAL_LRA_HDR(JJ).AND.                            &
     &     ANGINC.LT.SARAL_LRA_HDR(JJ+1)) THEN
       ANGINC1=SARAL_LRA_HDR(JJ)
       ANGINC2=SARAL_LRA_HDR(JJ+1)
       SLRLRA_CORR(I)=SARAL_LRA_RNG(JJ)+                               &
     &               (SARAL_LRA_RNG(JJ+1)-SARAL_LRA_RNG(JJ))*          &
     &               ((ANGINC-ANGINC1)/(ANGINC2-ANGINC1))
      !IF (I.EQ.1.OR.I.EQ.NM) THEN
      !write(6,*)I,ANGINC1,ANGINC2,ANGINC
      !write(6,*)JJ,SARAL_LRA_RNG(JJ),SARAL_LRA_RNG(JJ+1),SLRLRA_CORR(I)
      !END IF
       GOTO 99
       END IF
       END DO
       WRITE(6,*)'INCIDENCE ANGLE ON SARAL LRA IS LT 0 OR GT 90'
       WRITE(6,*)'STOP RUN IN TRKSRL',ANGINC
       STOP
   99  CONTINUE
      END IF


  100 END DO
!
!...RANGE CORRECTION IS DOT PRODUCT OF INERTIAL OFFSET AND
!...UNIT INERTIAL TOPOCENTRIC VECTORS
      CALL DOTPRD(RA,UI,DR,NM,NM,NM,3)
!
      IF (IDORTAB.GT.0) THEN
           DO I=1,NM
              DR(I)=DR(I)+PHC(I)
           ENDDO
      END IF
!
      IF (LLRASRL) THEN
      !! APPLY SARAL SLR RANGE CORRECTION
       !write(6,*)'apply slr range correction'
        DO I=1,NM
              DR(I)=DR(I)-SLRLRA_CORR(I)
        ENDDO
      END IF

      RETURN
60000 FORMAT(' WARNING !!!!!!!!!!!!!!!!')
60001 FORMAT(' TRKSRL CALLED FOR GPS MEASUREMENTS:')
60002 FORMAT(' THIS ROUTINE DOES NOT COMPUTE:')
60003 FORMAT('   (1) PHASE WIND UP CORRECTION')
60004 FORMAT('   (2) ANTENNA MAP CORRECTIOINS')
60005 FORMAT('   (3) ANTENNA FRAME EDITING (ANTCUT)')
60006 FORMAT(' TO ADD THESE FEATURES SEE TRKEXT OR TRKTOP')
      END
