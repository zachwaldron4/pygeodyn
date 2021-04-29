!$TRKXTI
      SUBROUTINE TRKXTI(REAQAT,IEAAAA,MEAMEM,MEAANT,URNM2,              &
     &                  OFFSET,XYZOF2,FSECSA,RA,DR,IANTNM,              &
     &                  ISBJ20,NUMAEA,IPANEA,MJDSEA,FSSCEA,             &
     &                  RINTEA,LINATO,AA,II,ICRSYS,LALT,                &
     &                  NDIMRA,MJDSIN,ATROT3,DWRKDO,LOFFAJ,             &
     &                  RJQU1,RJQU2,RJQU3,RJQU4,L1ST,LANTCT,IANTSC,     &
     &                  LWNDI,LWNDO,WPU,ISEQ,IUPDN,ANTTAB,PHC,XYZOF3,   &
     &                  ISATID,SBFTOR)
!*******************************************************************
!  ROUTINE NAME:   trkxti   DATE: 07/12/93      PGMR: S.B. LUTHCKE
!
!  FUNCTION - COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF TRACKING
!             POINT AND S/C C.G. NOT AT BODY FIXED ORIGIN. USE EXTERNAL
!             QUATERNIONS TO COMPUTE THE ROTATIONS OF THE INPUT OFFSET
!             VECTORS.
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   REAQAT   A      I    QUATERNION INFORMATION ARRAY
!   IEAAAA   A      I    PANEL QUATERNION POINTER ARRAY
!   MEAMEM   S      I    4*MEAMEM IS THE MAXIMUM DIMENSION OF REAQAT
!   MEAANT   S      I    4*MEAPLA IS THE MAXIMUM DIMENSION OF IEAPPP
!   URNM2    A      I    UNIT VECTOR FROM TRACKING STATION TO S/C
!                        THIS VECTOR IS IN TRUE OF REFERENCE.
!   OFFSET   A      I    BODY CENTERED FIXED TRACKING POINT LOCATION
!                        LINK OFFSET WITH CG CORRECTION APPLIED.
!   XYZOF2   A      I    OFFSET LINK #2 IN MVA SYSTEM (PHASE OFFSET)
!   XYZOF3   A      I    OFFSET LINK #3 IN MVA SYSTEM (PHASE OFFSET)
!   FSECSA   A      I    ANTENNA CORRECTION TIME SINCE BLOCK START TIME
!   RA       A           OFFSET LOCATION MAPPED INTO TRUE OF REFERENCE
!   DR       A      O    CORRECTIONS TO RANGES
!   IANTNM   S      I    ANTENNA NUMBER FOR THIS CORRECTION
!   ISBJ20   S      I    POINTER TO SBF TO J2000 QUATERNIONS FOR THIS SAT
!   NUMAEA   S      I    NUMBER OF MOVEABLE ANTENNA SECOND LINK OFFSETS I
!   IPANEA   S      I    POINTER TO THESE   ANTENNA SECOND LINK OFFSETS I
!   MJDSEA   S      I    QUATERNION      START TIME FOR THIS SET
!   FSSCEA   S      I    QUATERNION FSEC START TIME FOR THIS SET
!   RINTEA   S      I    QUATERNION TIME INTERVAL   FOR THIS SET
!   LINATO   S      O    =.TRUE. THEN USE INTERNAL MODEL IF REQUESTED
!   AA       A     I/O   REAL DYNAMIC ARRAY
!   II       A     I/O   INTEGER DYNAMIC ARRAY
!   ICRSYS   S      I    OFFSET ROTATION COORD. SYS. INTERNAL MODEL INDEX
!   MJDSIN   S      I    MJDSEC FOR BLOCK START
!   LANTCT   I      A    LOGICAL ANTENNA CUTOFF
!   IANTSC   I      A    ANTENNA CUT SATELLITE IDS
!   ISATID   S      I    SATELLITE ID
!
!***********************************************************************
!
! NOTES:
!    TOR = GEODYN TRUE OF REFERENCE INERTIAL FRAME
!          When CB is not the EARTH TOR is IAU vector of reference
!    SBF = SATELLITE BODY-FIXED FRAME
!    MVA = MOVEABLE ANTENNA FRAME
!
!    SBFTOR = ROTATION FROM SBF TO TOR FRAME
!
! ** OFFSET IS ROTATED FROM SBF TO TOR,
! ** XYZOF2 IS ROTATED FROM MVA TO TOR  (TOD?)
! ** IF INTERNAL ATTITUDE IS NECESSARY FOR ANY POINT IN THE BLOCK
! ** THEN THE WHOLE BLOCK WILL BE COMPUTED WITH THE INTERNAL ATTITUDE
!
!**********************************************************************
      use antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!     COMMON/TRKQP/IMQP
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
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
      COMMON/CREFMT/REFMT(9)
      COMMON/CRM5OE/RM5OE(9)
      COMMON/LDATOR/LDQAT,LDVCT
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
!
      COMMON/SAVEIM/RIM(3,3),SBF1(3,3),SBF2(3,3),SBF3(3,3),SBF4(3,3),   &
     &       SCF2(3,3),SCF3(3,3),SCF4(3,3),RKEY
!
      DIMENSION Q0(4),Q1(4)
      DIMENSION SBFTOD(3,3)
      DIMENSION REAQAT(MEAMEM,4),IEAAAA(MEAANT,4),URNM2(NDIMRA,3),      &
     &          OFFSET(3,2),XYZOF2(3),FSECSA(NM),RA(NDIMRA,3),          &
     &          DR(MINTIM,11),XLINK2(3),TOTOFF(3),                      &
     &          SBFTOR(3,3),ROT(3,3),QSBF(4),Q(4)
      DIMENSION XYZOF3(3),XLINK3(3)
      DIMENSION ATROT3(3,3,MINTIM),ATROT4(3,3)
      DIMENSION AA(1),II(1)
      DIMENSION DWRKDO(NM,3)
      DIMENSION RJQU1(NM),RJQU2(NM),RJQU3(NM),RJQU4(NM)
      DIMENSION LANTCT(1),IANTSC(1)
      DIMENSION WPU(NM,3,2),VWND(3,2)
      DIMENSION ANTTAB(1)
      DIMENSION PHC(NM)
      DIMENSION VECTL(3),VECTLL(3)
      DIMENSION DUMQ(1)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,C1DM1/1.0D-1/
      DATA C1D6/1.0D6/,C1D2/100.0D0/,C1DM3/1.0D-3/
      DATA EPSN/-.001D0/

      LOGICAL :: L_sat_phc
      LOGICAL :: L_ant_phc
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!      print *,'trkxti: lxattd: ',lxattd
!      print *,'trkxti: lalt: ',lalt
!      print *,'trkxti: loffaj: ',loffaj
!      print *,'trkxti: latmod: ',latmod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!CODE FOR TESTING, CD
!               do i=1,nm
!                    print*,i,MJDSIN,FSECSA(i)
!               enddo
!               print*,aa(kexcst),aa(kexcdt)
!               print*,aa(kexcgx),aa(kexcgy),aa(kexcgz)
! get UTC for the start time
!               mjdts=aa(kexcst)
!               fts=aa(kexcst)-mjdts
!                print*,'start time: ',mjdts, fts
!               call utcet(.false.,1,mjdts,fts,fos,aa(ka1ut))
!                print*, mjdts, fos
!                call ymdhms(mjdts,fos,iymd,ihms,fs,1)
!                print*,iymd,ihms,fs
!               print*,knexcg,kidexc,knrexc
!               print*, ii(knexcg) , ii(kidexc),ii(knrexc)
!               print*, ii(knexcg) , ii(kidexc+1),ii(knrexc+1)
!               stop
!!!!!! END CODE FOR TESTING CD
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         print*, 'dbg in trkxti'


      !--------------------------------------------------------------
      IANT=-9999
      LPHC=.FALSE.
      L_sat_phc = .FALSE.
      L_ant_phc = .FALSE.

      PHC(1:NM) = 0.0D0  ! jjm 20120601


      !if( .not. L_sat_phc )then
      !    write(501,'(A,1x,I10)') &
      !          'trkxti: no PCV info for sat ', ISATID
      !endif ! .not. L_sat_phc


      IF( ( XGPSTB .GT. 0.D0  .AND.  IUPDN .EQ. 2 ) .or.           &
          MTYPE ==  51                              .or.           &
          MTYPE ==  39                                    ) then

          IANT=(XANTSL(ISEQ)+.001D0)

      endif ! xgpstb...


!     write(6,'(A,2(1x,I4 ))')    'trkxti: ISEQ, IANT    ', ISEQ, IANT
!     write(6,'(A,1x,I6,1x,I10)') 'trkxti: MTYPE, ISATID ', MTYPE, ISATID

!     COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
!                   nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
!                   NANTPT,  NANTPS(999),    KANTPS(999), NXAPHA

      if( nant_sat > 0 )then

          call get_index2( isatid, iant, II(KANTYP),  NANT_sat, &
                           index_sat, L_sat_phc )

!         write(6,'(A,2(1x,I10), 4x, L1)') &
!                'trkxti: at entry ISATID, index_sat, L_sat_phc   ', &
!                                  ISATID, index_sat, L_sat_phc

      endif ! nant_sat > 0

      if( nantpt > 0 )then

          call get_index( iant,  &
                          II(KANTYP+ 2*(NANT_sat + NANT_sta) ),   &
                          NANTPT, index_ant, L_ant_phc )

!         write(6,'(A,2(1x,I10), 4x, L1)') &
!                'trkxti: at entry IANT, index_ant, L_ant_phc   ', &
!                                  IANT, index_ant, L_ant_phc

      endif ! nantpt > 0


      !IF(IANT.GT.0.AND.IANT.LE.99) LPHC=.TRUE.
      !IF(LPHC) THEN
      !  IPOFF=KANTPS(IANT)
      !  NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
      !  NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)
      !ENDIF

      !IF( L_sat_phc ) LPHC=.TRUE.

      !IF( mtype == 85 .and.  &
      IF( XGPSTB > 0.0D0  .and.  iant > 0  .and. &
          ( L_sat_phc .or. L_ant_phc )            ) LPHC=.TRUE.

!     write(6,'(A,4x,L1)') 'trkxti: LPHC ', LPHC

      IF(LPHC) THEN

        if( NANTPT > 0 .and. L_ant_phc )then

            IPOFF=KANTPS(index_ant) + isum_sat + isum_sta

            NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
            NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

!           write(6,'(A,4(1x,I6))') &
!                 'trkxti: IANT, IPOFF, NDIMA, NDIMZ ', &
!                          IANT, IPOFF, NDIMA, NDIMZ

        endif ! L_ant_phc

        if( L_sat_phc )then

            IPOFF = KANTPS_sat(index_sat)

            NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
            NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

!           write(6,'(A,4(1x,I6))') &
!                 'trkxti: ISATID, IPOFF, NDIMA, NDIMZ ', &
!                          ISATID, IPOFF, NDIMA, NDIMZ

        endif ! L_sat_phc

      ENDIF ! LPHC



       LWNDO=LWNDI
       IF(LWNDI) THEN
          IF(IUPDN.EQ.1) THEN
            VWND(1,1)=VHIGH(1,1,ISEQ)
            VWND(2,1)=VHIGH(2,1,ISEQ)
            VWND(3,1)=VHIGH(3,1,ISEQ)
            VWND(1,2)=VHIGH(1,2,ISEQ)
            VWND(2,2)=VHIGH(2,2,ISEQ)
            VWND(3,2)=VHIGH(3,2,ISEQ)
          ELSE
            VWND(1,1)=VLOW(1,1,ISEQ)
            VWND(2,1)=VLOW(2,1,ISEQ)
            VWND(3,1)=VLOW(3,1,ISEQ)
            VWND(1,2)=VLOW(1,2,ISEQ)
            VWND(2,2)=VLOW(2,2,ISEQ)
            VWND(3,2)=VLOW(3,2,ISEQ)

          ENDIF ! IUPDN.EQ.1

       ENDIF !  LWNDI

! IF ORIENTATION DATA IS BEING PASSED ON DATA RECORDS, CHECK
! TO SEE IF IT IS A QUATERNION OR A POINTING VECTOR
      IF(LXATTD) THEN
        XTEST=(RJQU4(1)+.001D0)
        ITEST=INT(XTEST)
        IF(ITEST.EQ.32) LDVCT=.TRUE.
        IF(ITEST.LT.32) LDQAT=.TRUE.
        IF(LDVCT.AND.LDQAT) THEN
           WRITE(6,98001)
           WRITE(6,98002)
           WRITE(6,98003)
           WRITE(6,98004)
           STOP
        ENDIF !  LDVCT.AND.LDQAT

! CHECK TO SEE IF THERE ARE ACTUALLY QUATERNIONS PRESENT ON DATA RECORDS
        LOK=.FALSE.
        !original IF(LDVECT) LOK=.TRUE.
        ! jjm 20120531 changed "LDVECT" to "LDVCT" below

        IF(LDVCT) LOK=.TRUE.

        TMP1=RJQU1(1) - C1DM1
        IF(ITMP1.EQ.-9999999) LOK=.TRUE.
        IF(.NOT.LOK) THEN
           TEST=ABS(RJQU1(1)*RJQU1(1)+RJQU2(1)*RJQU2(1)                &
     &              +RJQU3(1)*RJQU3(1)+RJQU4(1)*RJQU4(1)-1.D0)
           IF(TEST.LT..2D0) LOK=.TRUE.
        ENDIF !  .NOT.LOK

        IF(.NOT.LOK) THEN
           WRITE(6,98001)
           WRITE(6,98005)
           WRITE(6,98006)
           STOP
        ENDIF !  .NOT.LOK

      ENDIF ! LXATTD
!
! FOR EXTERNAL CGMASS COORDINATES
! CHECK IF THIS SATELLITE IS REQUIRED FOR EXTERNAL CGMASS COOR

!       print*,'trkxti: ',isatid,ii(kidexc),ii(knexcg)

      CALL FNDNUM(ISATID,II(KIDEXC),II(KNEXCG),IRET)

      LEXCG=.FALSE.
      IF(IRET.GT.0) THEN
        LEXCG=.TRUE.
        IEXCG=IRET
! COMPUTE THE POINTER FOR THIS SATELLITE
        IPTCGX=KEXCGX
        IPTCGY=KEXCGY
        IPTCGZ=KEXCGZ
!           print*, kexcgx,kexcgy,kexcgz
        DO I=1,IRET-1
          IPTCGX=IPTCGX+II(KNREXC+I-1)
          IPTCGY=IPTCGY+II(KNREXC+I-1)
          IPTCGZ=IPTCGZ+II(KNREXC+I-1)
        ENDDO

!           print*, iptcgx,iptcgy,iptcgz

      ENDIF !  IRET.GT.0
!
      DO 1000 INMNDX=1,NM


!---------------------------------------------
      IF(LDQAT) THEN


       QSBF(1)=RJQU1(INMNDX)
       QSBF(2)=RJQU2(INMNDX)
       QSBF(3)=RJQU3(INMNDX)
       QSBF(4)=RJQU4(INMNDX)
       ITEST=INT(QSBF(4)+.001D0)
       IF(ITEST.GT.30) THEN
           WRITE(6,98001)
           WRITE(6,98002)
           WRITE(6,98003)
           WRITE(6,98004)
           STOP
       ENDIF !  ITEST.GT.30

       CALL QATROT(QSBF,ROT)

! HERE CORRECT ROT FOR ROLL PITCH AND YAW

      IF(MAXAT.GT.0) THEN
      CALL MATPRD(ROT,ATROT3(1,1,INMNDX),ATROT4,3,3,3)
      DO 15 IL=1,3
      DO 15 IK=1,3

!....SAVE ROTATION FOR FILE OUTPUT (IN LTALTO OR DIRALT)
      ATROT3(IL,IK,INMNDX)=ATROT4(IL,IK)

      ROT(IL,IK)=ATROT4(IL,IK)
   15 CONTINUE
      ENDIF !  MAXAT.GT.0


      DO 20 I=1,3
       SBFTOR(1,I) = REFMT(1)*ROT(1,I) +                                &
     &               REFMT(2)*ROT(2,I) +                                &
     &               REFMT(3)*ROT(3,I)
       SBFTOR(2,I) = REFMT(4)*ROT(1,I) +                                &
     &               REFMT(5)*ROT(2,I) +                                &
     &               REFMT(6)*ROT(3,I)
       SBFTOR(3,I) = REFMT(7)*ROT(1,I) +                                &
     &               REFMT(8)*ROT(2,I) +                                &
     &               REFMT(9)*ROT(3,I)
   20  CONTINUE
       IF(LWNDI) THEN
          DO J=1,2
          DO I=1,3
            WPU(INMNDX,I,J)=SBFTOR(I,1)*VWND(1,J)                       &
     &                     +SBFTOR(I,2)*VWND(2,J)                       &
     &                     +SBFTOR(I,3)*VWND(3,J)
          ENDDO
          ENDDO
      ENDIF !  LWNDI

       IF(LALT) THEN
          URNM2(INMNDX,1)=SBFTOR(1,3)
          URNM2(INMNDX,2)=SBFTOR(2,3)
          URNM2(INMNDX,3)=SBFTOR(3,3)
          TOTOFF(1) = OFFSET(1,2)
          TOTOFF(2) = OFFSET(2,2)
          TOTOFF(3) = OFFSET(3,2)
          RA(INMNDX,1) = SBFTOR(1,1)*TOTOFF(1)                          &
     &                 + SBFTOR(1,2)*TOTOFF(2)                          &
     &                 + SBFTOR(1,3)*TOTOFF(3)
          RA(INMNDX,2) = SBFTOR(2,1)*TOTOFF(1)                          &
     &                 + SBFTOR(2,2)*TOTOFF(2)                          &
     &                 + SBFTOR(2,3)*TOTOFF(3)
          RA(INMNDX,3) = SBFTOR(3,1)*TOTOFF(1)                          &
     &                 + SBFTOR(3,2)*TOTOFF(2)                          &
     &                 + SBFTOR(3,3)*TOTOFF(3)
          GO TO 1000
       ENDIF ! LALT

      IF(XGPSTB.GT.0.D0.AND.IUPDN.EQ.2) THEN
         XV=URNM2(INMNDX,1)
         YV=URNM2(INMNDX,2)
         ZV=URNM2(INMNDX,3)

!        ANTENNA CUTOFF FOR GPS SATELLITES

         !write(6,*)'trkxti: call antang'

         CALL ANTANG(ISEQ,ROT,XV,YV,ZV,LANTCT(INMNDX))

      ENDIF !  XGPSTB.GT.0.D0.AND.IUPDN.EQ.2


      IF(LPHC) THEN

!        write(6,'(A)') 'trkxti: first call to antsac'

         XV=-URNM2(INMNDX,1)
         YV=-URNM2(INMNDX,2)
         ZV=-URNM2(INMNDX,3)
         IMQP=INMNDX

         CALL ANTSAC(SBFTOR,XV,YV,ZV,ANTTAB(IPOFF+6),NDIMA,NDIMZ,      &
                     ANTTAB(IPOFF),ISEQ,PHC(INMNDX),2,.FALSE.)


         !!!!! DO NOT USE PCV CORRECTION IF NOT REQUESTED!!!!!
         !     ELSE
         ! IF NO PHASE TABLE FOR THE USER SATELLITE,
         ! THEN USE THE THE HARD CODED PHASE NU
         !        XV=-URNM2(INMNDX,1)
         !        YV=-URNM2(INMNDX,2)
         !        ZV=-URNM2(INMNDX,3)
         !        CALL ANTSJ2(SBFTOR,XV,YV,ZV,DUMQ,ISEQ,PHC(INMNDX))
         !!!!! DO NOT USE PCV CORRECTION IF NOT REQUESTED!!!!!


      ENDIF ! LPHC

! ELSE FOR ATTITUDE ON DATA RECORDS


!---------------------------------------------

      ELSE  !  .not. LDQAT    ! ELSE FOR ATTITUDE ON DATA RECORDS

!---------------------------------------------



! COMPUTE SBF TO J2000 ROTATION MATRIX

! debug extatt
!      print *,'trkxti: isbj20: ',isbj20
!      print *,'trkxti: sbf reaqat: ',reaqat(isbj20,1),reaqat(isbj20,2),
!     .         reaqat(isbj20,3),reaqat(isbj20,4)
!      print *,'trkxti: isbj20+1: ',isbj20+1
!      print *,'trkxti: sbf reaqat: ',reaqat(isbj20+1,1),
!     .         reaqat(isbj20+1,2),
!     .         reaqat(isbj20+1,3),reaqat(isbj20+1,4)
! debug extatt

! ... compute time difference between current time and
! ... start of quaternions

      MJDSDF=MJDSIN-MJDSEA

! debug extatt
!      print *,'trkxti: mjdsin: ',mjdsin
!      print *,'trkxti: fsecsa: ',fsecsa(inmndx)
! debug extatt
      FSECDF=FSECSA(INMNDX)-FSSCEA
      EADIFF=DBLE(MJDSDF)+FSECDF
      IF(EADIFF.LT.EPSN) GOTO 9100
! ... compute pointer to quaternions preceding the current time
      IPNT1=INT(EADIFF/RINTEA)+1
      IPNTM1=IPNT1-1
! .... (no minus 1 in index to reaqat below because a header is
! .... stored at isbj20)
      IQTPT1=ISBJ20+IPNT1
! debug extatt
!      print *,'trkxti: iqtpt1: ',iqtpt1
!      print *,'trkxti: ipnt1:',ipnt1
!      print *,'trkxti: reaqat3: ',reaqat(3,1),reaqat(iqtpt3,2)
!      print *,'trkxti: reaqat: ',reaqat(iqtpt1,1),reaqat(iqtpt1,2)
! debug extatt

      IQTPT2=IQTPT1+1

! debug extatt
!      print *,'trkxti: mjdsin,mjdsea,mjdsdf: ',mjdsin,mjdsea,mjdsdf
!      print *,' fsec,fsscea,fsecdf: ',fsecsa(inmndx),fsscea,fsecdf
!      print *,'trkxti: eadiff,rintea,ipnt1: ',eadiff,rintea,ipnt1
! debug extatt
! ... test to determine if either of the boundary quaternions is marked
! .... -9999999
      TMP1=REAQAT(IQTPT1,1) - C1DM1
      ITMP1=TMP1
      TMP2=REAQAT(IQTPT2,1) - C1DM1
      ITMP2=TMP2
! debug extatt
!      print *,'trkxti: tmp1,itmp1,tmp2,itmp2: ',tmp1,itmp1,tmp2,itmp2
! debug extatt

      IF((ITMP1.NE.-9999999).AND.(ITMP2.NE.-9999999)) THEN

! ... compute time of preceding boundary quaternion referenced to the st
! .... time of the quaternions of this set
       TIMPRE=DBLE(IPNTM1)*RINTEA
       TIMDIF=EADIFF-TIMPRE
! debug extatt
!      print *,'trkxti: ipntm1,rintea,timpre: ',ipntm1,rintea,timpre
!      print *,'trkxti: timdif: ',timdif
! debug extatt
       IF((TIMDIF.LT.EPSN).OR.(TIMDIF.GT.RINTEA)) THEN
           WRITE(6,76543) TIMDIF,RINTEA
76543      FORMAT('  TIMDIF RINTEA ',2D25.16)
           WRITE(6,76544) MJDSDF,FSECDF,EADIFF
76544      FORMAT('  MJDSDF,FSECDF EADIFF ',I10,2D25.16)
           WRITE(6,76545) MJDSIN,FSECSA(INMNDX)
76545      FORMAT('  MJDSIN,FSECSA ',I10,D25.16)
           WRITE(6,76546) MJDSEA,FSSCEA
76546      FORMAT('  MJDSEA,FSSCEA ',I10,D25.16)
           WRITE(6,76547) TIMPRE,IPNTM1
76547      FORMAT('  TIMPRE IPNTM1 ',D25.16,I12)
           GO TO 9300
       ENDIF !  (TIMDIF.LT.EPSN).OR.(TIMDIF.GT.RINTEA)

! ... compute interpolated SBF to J2000 quaternions at the requested tim
! .... J2000 or whatever the basis for the input GEODYN ephemeris file
! .... (could be B1950)
! debug extatt
!       rq1mag=reaqat(iqtpt1,1)**2+reaqat(iqtpt1,2)**2 +
!     .        reaqat(iqtpt1,3)**2+reaqat(iqtpt1,4)**2
!       rq2mag=reaqat(iqtpt2,1)**2+reaqat(iqtpt2,2)**2 +
!     .        reaqat(iqtpt2,3)**2+reaqat(iqtpt2,4)**2
!       print *,'trkxti: rq1mag,rq2mag: ',rq1mag,rq2mag
!       print *,'trkxti: rintea,timdif: ',rintea,timdif
!       print *,'trkxti: reaqat(iqtpt1: ',iqtpt1
!       print *,reaqat(iqtpt1,1),reaqat(iqtpt1,2),
!     .         reaqat(iqtpt1,3),reaqat(iqtpt1,4)
!       print *,'trkxti: reaqat(iqtpt2: ',iqtpt2
!       print *,reaqat(iqtpt2,1),reaqat(iqtpt2,2),
!    .         reaqat(iqtpt2,3),reaqat(iqtpt2,4)
! debug extatt

       DT=TIMDIF/RINTEA
       DO 100 I=1,4
       Q0(I)= REAQAT(IQTPT1,I)
       Q1(I)= REAQAT(IQTPT2,I)
!!!!   QSBF(I)=(REAQAT(IQTPT2,I)-REAQAT(IQTPT1,I))*DT +                 &
!!!! &        REAQAT(IQTPT1,I)
  100  CONTINUE
       CALL SLERP(DT,Q0,Q1,QSBF)
! test to see if quaternion magnitude is close to zero
       QATMAG=QSBF(1)**2+QSBF(2)**2+QSBF(3)**2+QSBF(4)**2
       QDIFF=ABS(ONE-QATMAG)
       IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,97000) REAQAT(IQTPT1,1),REAQAT(IQTPT1,2),               &
     &                 REAQAT(IQTPT1,3),REAQAT(IQTPT1,4),               &
     &                 REAQAT(IQTPT2,1),REAQAT(IQTPT2,2),               &
     &                 REAQAT(IQTPT2,3),REAQAT(IQTPT2,4),               &
     &                 QSBF(1),QSBF(2),QSBF(3),QSBF(4),QATMAG

       ENDIF !  QDIFF.GT.C1DM3
!
! debug extatt
!       print *,'trkxti: qatmag before norm.: ',qatmag
!       print *,'trkxti: qsbf before norm.: '
!       print *,qsbf(1),qsbf(2),qsbf(3),qsbf(4)
! debug extatt
! .... normalize the interpolated quaternion
       QNORM=SQRT(QATMAG)
       DO 101 I=1,4
       QSBF(I)=QSBF(I)/QNORM
  101  CONTINUE
! debug extatt
!       qatmag=qsbf(1)**2+qsbf(2)**2+qsbf(3)**2+qsbf(4)**2
!       print *,'trkxti: qatmag after norm.: ',qatmag
!       print *,'trkxti: qsbf after norm.: '
!       print *,qsbf(1),qsbf(2),qsbf(3),qsbf(4)
! debug extatt
!
       CALL QATROT(QSBF,ROT)
! debug extatt
!       print *,'trkxti: qsbf rotation matrix: '
!       print *,rot
! debug extatt
! .... compute rotation from SBF to TOR by multiplying SBF to J2000 (50)
! .... rotation J2000 to TOR.  If central body other than Earth then TOR
! .... IAU vector of reference.
! debug extatt
!       print*,'trkxti: REFMT: '
!       print *,refmt(1),refmt(2),refmt(3)
!       print *,refmt(4),refmt(5),refmt(6)
!       print *,refmt(7),refmt(8),refmt(9)
!       print*,'trkxti: ROT: '
!       print *,rot(1,1),rot(1,2),rot(1,3)
!       print *,rot(2,1),rot(2,2),rot(2,3)
!       print *,rot(3,1),rot(3,2),rot(3,3)
! debug extatt
!
!     write(6,*)' UNCORRECTED ROT ',ROT(1,1),ROT(2,1),ROT(3,1)
!     write(6,*)' UNCORRECTED ROT ',ROT(1,2),ROT(2,2),ROT(3,2)
!     write(6,*)' UNCORRECTED ROT ',ROT(1,3),ROT(2,3),ROT(3,3)
!
! USUALLY THE LASER POINTING VECTOR IN THE SBF IS (0,0,1)
! IF LDVCT=.TRUE., THEN COMPUTE THE POINTING VECTOR IN THE
! BENCH FRAME FROM J2000 POINTING VECTOR

       VECTL(1)=0.D0
       VECTL(2)=0.D0
       VECTL(3)=-1.D0
       IF( LDVCT) THEN
           VECTLL(1)=RJQU1(INMNDX)
           VECTLL(2)=RJQU2(INMNDX)
           VECTLL(3)=RJQU3(INMNDX)
           ITEST=INT(RJQU4(INMNDX)+.001D0)
           IF( ITEST.LT.30) THEN
               WRITE(6,98001)
               WRITE(6,98002)
               WRITE(6,98003)
               WRITE(6,98004)
               STOP
           ENDIF
           VECTL(1)=  ROT(1,1)*VECTLL(1)+ROT(2,1)*VECTLL(2)             &
                     +ROT(3,1)*VECTLL(3)
           VECTL(2)=  ROT(1,2)*VECTLL(1)+ROT(2,2)*VECTLL(2)             &
                     +ROT(3,2)*VECTLL(3)
           VECTL(3)=  ROT(1,3)*VECTLL(1)+ROT(2,3)*VECTLL(2)             &
                     +ROT(3,3)*VECTLL(3)
       ENDIF !  LDVCT


! HERE CORRECT ROT FOR ROLL PITCH AND YAW

      IF(MAXAT.GT.0) THEN
      CALL MATPRD(ROT,ATROT3(1,1,INMNDX),ATROT4,3,3,3)
      DO 105 IL=1,3
      DO 105 IK=1,3

!....SAVE ROTATION FOR FILE OUTPUT (IN LTALTO OR DIRALT)
      ATROT3(IL,IK,INMNDX)=ATROT4(IL,IK)

      ROT(IL,IK)=ATROT4(IL,IK)
  105 CONTINUE
      ENDIF !  MAXAT.GT.0

!     write(6,*)' CORRECTED ROT ',ROT(1,1),ROT(2,1),ROT(3,1)
!     write(6,*)' CORRECTED ROT ',ROT(1,2),ROT(2,2),ROT(3,2)
!     write(6,*)' CORRECTED ROT ',ROT(1,3),ROT(2,3),ROT(3,3)

       DO 110 I=1,3
       SBFTOR(1,I) = REFMT(1)*ROT(1,I) +                                &
     &               REFMT(2)*ROT(2,I) +                                &
     &               REFMT(3)*ROT(3,I)
       SBFTOR(2,I) = REFMT(4)*ROT(1,I) +                                &
     &               REFMT(5)*ROT(2,I) +                                &
     &               REFMT(6)*ROT(3,I)
       SBFTOR(3,I) = REFMT(7)*ROT(1,I) +                                &
     &               REFMT(8)*ROT(2,I) +                                &
     &               REFMT(9)*ROT(3,I)
  110  CONTINUE

!     write(6,*)' CORRECTED ROT ',ROT(1,1),ROT(1,2),ROT(1,3)
!     write(6,*)' CORRECTED ROT ',ROT(2,1),ROT(2,2),ROT(2,3)
!     write(6,*)' CORRECTED ROT ',ROT(3,1),ROT(3,2),ROT(3,3)

!     write(6,*)' dbg SBFTOR ',SBFTOR(1,1),SBFTOR(1,2),SBFTOR(1,3)
!     write(6,*)' dbg SBFTOR ',SBFTOR(2,1),SBFTOR(2,2),SBFTOR(2,3)
!     write(6,*)' dbg SBFTOR ',SBFTOR(3,1),SBFTOR(3,2),SBFTOR(3,3)

       DO 111 I=1,3
       SBFTOD(1,I) = RM5OE(1)*ROT(1,I) +                                &
     &               RM5OE(4)*ROT(2,I) +                                &
     &               RM5OE(7)*ROT(3,I)
       SBFTOD(2,I) = RM5OE(2)*ROT(1,I) +                                &
     &               RM5OE(5)*ROT(2,I) +                                &
     &               RM5OE(8)*ROT(3,I)
       SBFTOD(3,I) = RM5OE(3)*ROT(1,I) +                                &
     &               RM5OE(6)*ROT(2,I) +                                &
     &               RM5OE(9)*ROT(3,I)
 111  CONTINUE


!     write(6,*)' dbg SBFTOD ',SBFTOD(1,1),SBFTOD(1,2),SBFTOD(1,3)
!     write(6,*)' dbg SBFTOD ',SBFTOD(2,1),SBFTOD(2,2),SBFTOD(2,3)
!     write(6,*)' dbg SBFTOD ',SBFTOD(3,1),SBFTOD(3,2),SBFTOD(3,3)

      DO JJ=1,3
      DO KK=1,3
!     RIM(JJ,KK)=SBFTOD(JJ,KK)
!     SBF1(JJ,KK)=SBFTOD(JJ,KK)
!     write(6,*)' dbg in trkxti RIM ', RIM(JJ,KK),SBF1(JJ,KK)
      ENDDO ! kk
      ENDDO ! jj

       IF( LWNDI) THEN
           DO  J=1,2
           DO  I=1,3
               WPU(INMNDX,I,J)=SBFTOR(I,1)*VWND(1,J)                    &
                              +SBFTOR(I,2)*VWND(2,J)                    &
                              +SBFTOR(I,3)*VWND(3,J)
           ENDDO
           ENDDO
       ENDIF !  LWNDI

! SAVE THE TOTAL ROTATION FOR OUTPUT IN THE GEOLOCATION FILE

       DR(INMNDX,2)=SBFTOR(1,1)
       DR(INMNDX,3)=SBFTOR(2,1)
       DR(INMNDX,4)=SBFTOR(3,1)
       DR(INMNDX,5)=SBFTOR(1,2)
       DR(INMNDX,6)=SBFTOR(2,2)
       DR(INMNDX,7)=SBFTOR(3,2)
       DR(INMNDX,8)=SBFTOR(1,3)
       DR(INMNDX,9)=SBFTOR(2,3)
       DR(INMNDX,10)=SBFTOR(3,3)

       IF(LALT) THEN
          URNM2(INMNDX,1)=SBFTOR(1,1)*VECTL(1)+SBFTOR(1,2)*VECTL(2)     &
     &                   +SBFTOR(1,3)*VECTL(3)
          URNM2(INMNDX,2)=SBFTOR(2,1)*VECTL(1)+SBFTOR(2,2)*VECTL(2)     &
     &                   +SBFTOR(2,3)*VECTL(3)
          URNM2(INMNDX,3)=SBFTOR(3,1)*VECTL(1)+SBFTOR(3,2)*VECTL(2)     &
     &                   +SBFTOR(3,3)*VECTL(3)
          TOTOFF(1) = OFFSET(1,2)
          TOTOFF(2) = OFFSET(2,2)
          TOTOFF(3) = OFFSET(3,2)
          RA(INMNDX,1) = SBFTOR(1,1)*TOTOFF(1)                          &
     &                 + SBFTOR(1,2)*TOTOFF(2)                          &
     &                 + SBFTOR(1,3)*TOTOFF(3)
          RA(INMNDX,2) = SBFTOR(2,1)*TOTOFF(1)                          &
     &                 + SBFTOR(2,2)*TOTOFF(2)                          &
     &                 + SBFTOR(2,3)*TOTOFF(3)
          RA(INMNDX,3) = SBFTOR(3,1)*TOTOFF(1)                          &
     &                 + SBFTOR(3,2)*TOTOFF(2)                          &
     &                 + SBFTOR(3,3)*TOTOFF(3)
          GO TO 1000
       ENDIF !  LALT


      IF(XGPSTB.GT.0.D0.AND.IUPDN.EQ.2) THEN
         XV=URNM2(INMNDX,1)
         YV=URNM2(INMNDX,2)
         ZV=URNM2(INMNDX,3)

!        ANTENNA CUTOFF FOR GPS SATELLITES

         !write(6,*)'trkxti:2 call antang'

         CALL ANTANG(ISEQ,ROT,XV,YV,ZV,LANTCT(INMNDX))
      ENDIF !  XGPSTB.GT.0.D0.AND.IUPDN.EQ.2


      IF(LPHC) THEN

         write(6,'(A)') 'trkxti: second call to antsac'

         XV=-URNM2(INMNDX,1)
         YV=-URNM2(INMNDX,2)
         ZV=-URNM2(INMNDX,3)
         IMQP=INMNDX

         CALL ANTSAC(SBFTOR,XV,YV,ZV,ANTTAB(IPOFF+6),NDIMA,NDIMZ,      &
                     ANTTAB(IPOFF),ISEQ,PHC(INMNDX),2,.FALSE.)


         !!!!! DO NOT USE PCV CORRECTION IF NOT REQUESTED!!!!!
         !     ELSE
         ! IF NO PHASE MAP IN THE TABLE FOUND IN THE INTERNAL MODEL,
         ! USE THE HARD CODED NUMBERS
         !        XV=-URNM2(INMNDX,1)
         !        YV=-URNM2(INMNDX,2)
         !        ZV=-URNM2(INMNDX,3)
         !        CALL ANTSJ2(SBFTOR,XV,YV,ZV,DUMQ,ISEQ,PHC(INMNDX))
         !!!!! DO NOT USE PCV CORRECTION IF NOT REQUESTED!!!!!


      ENDIF ! LPHC

! debug extatt
!       print*,'trkxti: sbftor after 110'
!       print *,sbftor(1,1),sbftor(2,1),sbftor(3,1)
!       print *,sbftor(1,2),sbftor(2,2),sbftor(3,2)
!       print *,sbftor(1,3),sbftor(2,3),sbftor(3,3)
! debug extatt


      ELSE
          IF(.NOT.LDVCT) THEN
             LINATO=.TRUE.
             IF(ICRSYS.LE.0) GOTO 9400
             RETURN
          ELSE
             URNM2(INMNDX,1)=999.D0
             URNM2(INMNDX,2)=999.D0
             URNM2(INMNDX,3)=999.D0
             RA(INMNDX,1) = 0.D0
             RA(INMNDX,2) = 0.D0
             RA(INMNDX,3) = 0.D0
          ENDIF !  .NOT.LDVCT

      ENDIF  !  (ITMP1.NE.-9999999).AND.(ITMP2.NE.-9999999)



      ENDIF !  LDQAT
!---------------------------------------------


! COMPUTE MOVEABLE ANTENNA LINK ROTATION

! ...COMPUTE POINTERS TO THIS MOVABLE PLATE'S QUATERNIONS
!    IF NUMAEA=0 THEN ONLY ROTATE AND USE OFFSET LINK 1

! debug extatt
!      print *,'trkxti: numaea: ',numaea
! debug extatt

       IF(NUMAEA.LE.0) THEN
        TOTOFF(1) =  OFFSET(1,2)
        TOTOFF(2) =  OFFSET(2,2)
        TOTOFF(3) =  OFFSET(3,2)
        GOTO 128
       ENDIF ! NUMAEA.LE.0

       DO 125 I=1,NUMAEA
       ITPNDX=IPANEA+I-1
       IMVASB=IEAAAA(ITPNDX,2)

! debug extatt
!       print *,'trkxti: iantnm,ieaaaa,itpndx: ',iantnm,
!    .          ieaaaa(itpndx,1),itpndx
! debug extatt

       IF(IEAAAA(ITPNDX,1).EQ.IANTNM) GOTO 126
  125  CONTINUE


! .... NO MATCH FOR MOVABLE ANTENNA IN EXTERNAL QUATERNION FILE.  THEREF
! .... USE INTERNAL MODEL.  IF INTERNAL MODEL NOT SELECTED TERMINATE.

       IF(ICRSYS.LE.0) GOTO 9500
        LINATO=.TRUE.
        RETURN

  126  CONTINUE

! debug extatt
!       print *,'trkxti: after 126'
!      print *,'trkxti: mva,iantnm, reaqat: ',iantnm,reaqat(imvasb,1),
!     .         reaqat(imvasb,2),
!     .         reaqat(imvasb,3),reaqat(imvasb,4)
! debug extatt


! COMPUTE POINTERS TO PROPER QUATERNIONS AND THEN COMPUTE ROTATION MATRI


! .... MOVABLE PLATES AND ANTENNA FOR A PARTICULAR SET HAVE THE SAME INT
! .... AND START AND STOP TIMES AS THE SBF TO J2000 QUATERNIONS.  THEREF
! .... THE VARIOUS QUANTITIES COMPUTED ABOVE MAY BE USED HERE!..........
! .... ALSO, -9999999 INTERNAL ATTITUDE OVERRIDE MAY NOT BE USED FOR MOV
! .... PLATE QUATERNIONS.

      IQTPT1=IMVASB+IPNT1
      IQTPT2=IQTPT1+1

! ... test to determine if either of the boundary quaternions is marked
! .... -9999999

      TMP1=REAQAT(IQTPT1,1) - C1DM1
      ITMP1=TMP1
      TMP2=REAQAT(IQTPT2,1) - C1DM1
      ITMP2=TMP2

! debug extatt
!      print *,'trkxti: itmp1,itmp2: ',itmp1,itmp2
! debug extatt

      IF((ITMP1.EQ.-9999999).OR.(ITMP2.EQ.-9999999)) THEN
       LINATO=.TRUE.
       IF(ICRSYS.LE.0) GOTO 9600
       RETURN
      ENDIF !  (ITMP1.EQ.-9999999).OR.(ITMP2.EQ.-9999999)

       DO 127 I=1,4
       Q0(I)= REAQAT(IQTPT1,I)
       Q1(I)= REAQAT(IQTPT2,I)
!!!!   Q(I)=(REAQAT(IQTPT2,I)-REAQAT(IQTPT1,I))*DT +                    &
!!!! &        REAQAT(IQTPT1,I)
  127  CONTINUE
       CALL SLERP(DT,Q0,Q1,Q)

! test to see if quaternion magnitude is close to zero

       QATMAG=Q(1)**2+Q(2)**2+Q(3)**2+Q(4)**2
       QDIFF=ABS(ONE-QATMAG)
       IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,98000) REAQAT(IQTPT1,1),REAQAT(IQTPT1,2),               &
     &                 REAQAT(IQTPT1,3),REAQAT(IQTPT1,4),               &
     &                 REAQAT(IQTPT2,1),REAQAT(IQTPT2,2),               &
     &                 REAQAT(IQTPT2,3),REAQAT(IQTPT2,4),               &
     &                 Q(1),Q(2),Q(3),Q(4),QATMAG

       ENDIF !  QDIFF.GT.C1DM3

! .... normalize the interpolated quaternion

       QNORM=SQRT(QATMAG)
       DO 129 I=1,4
       Q(I)=Q(I)/QNORM
  129  CONTINUE

       CALL QATROT(Q,ROT)

! ROTATE 2ND LINK FROM MVA TO SBF FRAME

       XLINK2(1) = ROT(1,1)*XYZOF2(1)                                   &
     &           + ROT(1,2)*XYZOF2(2)                                   &
     &           + ROT(1,3)*XYZOF2(3)
       XLINK2(2) = ROT(2,1)*XYZOF2(1)                                   &
     &           + ROT(2,2)*XYZOF2(2)                                   &
     &           + ROT(2,3)*XYZOF2(3)
       XLINK2(3) = ROT(3,1)*XYZOF2(1)                                   &
     &           + ROT(3,2)*XYZOF2(2)                                   &
     &           + ROT(3,3)*XYZOF2(3)

! COMPUTE TOTAL SBF OFFSET VECTOR (LINK2+OFFSET)

      TOTOFF(1) = XLINK2(1) + OFFSET(1,2)
      TOTOFF(2) = XLINK2(2) + OFFSET(2,2)
      TOTOFF(3) = XLINK2(3) + OFFSET(3,2)
!
! CALCULATE LINK3 OFFSET IF REQUIRED
! WE NEED TO DO THIS ONLY IF XYZOF3 SQUARE IS LARGER THAN ZERO
!
      X3SQUA=XYZOF3(1)*XYZOF3(1)+XYZOF3(2)*XYZOF3(2)+XYZOF3(3)*XYZOF3(3)
      IF(X3SQUA.LE.0.D0) GOTO 128
      DO 210 I=1,NUMAEA
        ITPNDX=IPANEA+I-1
        IMVAS3=IEAAAA(ITPNDX,4)
        IF(IEAAAA(ITPNDX,3).EQ.IANTNM) GOTO 215
  210 CONTINUE
! NO QUATERNIONS FOUND FOR THIS LINK, STOP

      WRITE(6,*) &
            'NO EXTERNAL QUATERNIONS FOR LINK 3 OF ANTENNA ', IANTNM
      STOP
  215 CONTINUE
      IQTPT1=IMVAS3+IPNT1
      IQTPT2=IQTPT1+1
! ... test to determine if either of the boundary quaternions is marked
! .... -9999999
      TMP1=REAQAT(IQTPT1,1) - C1DM1
      ITMP1=TMP1
      TMP2=REAQAT(IQTPT2,1) - C1DM1
      ITMP2=TMP2
      IF((ITMP1.EQ.-9999999).OR.(ITMP2.EQ.-9999999)) THEN
        WRITE(6,*) 'NO EXTERNAL QUATERNIONS FOR LINK 3 OF ANTENNA ',    &
     &             IANTNM
      ENDIF !  (ITMP1.EQ.-9999999).OR.(ITMP2.EQ.-9999999)

! LINEAR INTERPOLATION
      DO I=1,4
       Q0(I)= REAQAT(IQTPT1,I)
       Q1(I)= REAQAT(IQTPT2,I)
!!!!    Q(I)=(REAQAT(IQTPT2,I)-REAQAT(IQTPT1,I))*DT +                   &
!!!! &        REAQAT(IQTPT1,I)
      ENDDO
      CALL SLERP(DT,Q0,Q1,Q)
! test to see if quaternion magnitude is close to zero
       QATMAG=Q(1)*Q(1)+Q(2)*Q(2)+Q(3)*Q(3)+Q(4)*Q(4)
       QDIFF=ABS(ONE-QATMAG)
       IF(QDIFF.GT.C1DM3) THEN
        WRITE(6,98000) REAQAT(IQTPT1,1),REAQAT(IQTPT1,2),               &
     &                 REAQAT(IQTPT1,3),REAQAT(IQTPT1,4),               &
     &                 REAQAT(IQTPT2,1),REAQAT(IQTPT2,2),               &
     &                 REAQAT(IQTPT2,3),REAQAT(IQTPT2,4),               &
     &                 Q(1),Q(2),Q(3),Q(4),QATMAG

       ENDIF !  QDIFF.GT.C1DM3

! .... normalize the interpolated quaternion
       QNORM=SQRT(QATMAG)
       DO I=1,4
         Q(I)=Q(I)/QNORM
       ENDDO
!
       CALL QATROT(Q,ROT)
!
! END OF LINK3 OFFSET CALCULATION
!
! ROTATE 3RD LINK FROM MVA TO SBF FRAME
!
       XLINK3(1) = ROT(1,1)*XYZOF3(1)                                   &
     &           + ROT(1,2)*XYZOF3(2)                                   &
     &           + ROT(1,3)*XYZOF3(3)
       XLINK3(2) = ROT(2,1)*XYZOF3(1)                                   &
     &           + ROT(2,2)*XYZOF3(2)                                   &
     &           + ROT(2,3)*XYZOF3(3)
       XLINK3(3) = ROT(3,1)*XYZOF3(1)                                   &
     &           + ROT(3,2)*XYZOF3(2)                                   &
     &           + ROT(3,3)*XYZOF3(3)
!
! COMPUTE TOTAL SBF OFFSET VECTOR BY ADDING LINK3 OFFSET
!
      TOTOFF(1) = XLINK3(1) + TOTOFF(1)
      TOTOFF(2) = XLINK3(2) + TOTOFF(2)
      TOTOFF(3) = XLINK3(3) + TOTOFF(3)
!
  128  CONTINUE
!
! ADD CGMASS COORDINATES
! NOTE THERE ARE TWO KINDS OF CGMASS COORDINATES.
! THE FIRST ONE IS THE CGMASS COORDINATES SPECIFIED IN CGMASS CARD.
! THE OTHER ONE IS THE TIME DEPENDENT CGMASS COORDINATES FROM
! AN EXTERNAL CGMASS FILE.

! THE FIRST COOR HAS ALREADY BEEN APPLIED.

! HERE ADD ALL THE TIME DEPENDENT CGMASS COORDINATES READ FROM
! THE EXTERNAL CGMASS FILE.

! THE FOLLOWING IS DONE ONLY WHEN EXTERNAL CGMASS COORDINATES
! ARE  REQUIRED FOR THIS SATELLITE

      IF(LEXCG) THEN

        ! LINEAR INTERPOLATION
        ! COMPUTE THE TIME DIFFERENCE FROM THE START TIME

        TCGDIF=MJDSIN+FSECSA(INMNDX)-AA(KEXCST+IEXCG-1)

        ! THE INTERPOLATED TIME LIES BETWEEN
        ! KEXCDT+INDCG     AND     KEXCDT+INDCG+1

        !print*, 'trkxti: ',aa(kexcdt+iexcg-1)

        INDCG=INT(TCGDIF/AA(KEXCDT+IEXCG-1))
        TCGDIF=MOD(TCGDIF,AA(KEXCDT+IEXCG-1))
        TCGDIF=TCGDIF/AA(KEXCDT+IEXCG-1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! CODE FOR TESTING
!         call utcet(.false.,1,mjdsin,fsecsa(inmndx),fos,aa(ka1ut))
!         call ymdhms(mjdsin,fos,iymd,ihms,fs,1)
!         print*,iymd,ihms,fs, aa(kexcgx+indcg)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ECGCRX =  AA(IPTCGX+INDCG) +                              &
                ( AA(IPTCGX+INDCG+1) - AA(IPTCGX+INDCG) )*TCGDIF

        ECGCRY =  AA(IPTCGY+INDCG) +                              &
                ( AA(IPTCGY+INDCG+1) - AA(IPTCGY+INDCG) )*TCGDIF

        ECGCRZ =  AA(IPTCGZ+INDCG) +                              &
                ( AA(IPTCGZ+INDCG+1) - AA(IPTCGZ+INDCG) )*TCGDIF

!       print*,'x: ',aa(iptcgx+indcg),ecgcrx,aa(iptcgx+indcg+1)
!       print*,'y: ',aa(iptcgy+indcg),ecgcry,aa(iptcgy+indcg+1)
!       print*,'z: ',aa(iptcgz+indcg),ecgcrz,aa(iptcgz+indcg+1)
! NOW WE ADD THE CGMASS CORRECTION FOR THE TOTAL OFFSET
        TOTOFF(1) = TOTOFF(1) - ECGCRX
        TOTOFF(2) = TOTOFF(2) - ECGCRY
        TOTOFF(3) = TOTOFF(3) - ECGCRZ

      ENDIF ! LEXCG

! ROTATE SBF OFFSET (LINK3+)LINK2+LINK1+CGMASS TO TOR FRAME
!
! debug extatt
!      totmag=totoff(1)**2+totoff(2)**2+totoff(3)
!      totmag=sqrt(totmag)
!      print *,'trkxti: inmndx,totmag: ',inmndx,totmag
!       print*,'trkxti: sbftor after 128'
!       print *,sbftor(1,1),sbftor(1,2),sbftor(1,3)
!       print *,sbftor(2,1),sbftor(2,2),sbftor(2,3)
!       print *,sbftor(3,1),sbftor(3,2),sbftor(3,3)
! debug extatt
!
       RA(INMNDX,1) = SBFTOR(1,1)*TOTOFF(1)                             &
     &              + SBFTOR(1,2)*TOTOFF(2)                             &
     &              + SBFTOR(1,3)*TOTOFF(3)
       RA(INMNDX,2) = SBFTOR(2,1)*TOTOFF(1)                             &
     &              + SBFTOR(2,2)*TOTOFF(2)                             &
     &              + SBFTOR(2,3)*TOTOFF(3)
       RA(INMNDX,3) = SBFTOR(3,1)*TOTOFF(1)                             &
     &              + SBFTOR(3,2)*TOTOFF(2)                             &
     &              + SBFTOR(3,3)*TOTOFF(3)
!
       IF(LOFFAJ) THEN
         IF(L1ST) THEN
           DWRKDO(INMNDX,1)=URNM2(INMNDX,1)*SBFTOR(1,1)                 &
     &                     +URNM2(INMNDX,2)*SBFTOR(2,1)                 &
     &                     +URNM2(INMNDX,3)*SBFTOR(3,1)
           DWRKDO(INMNDX,2)=URNM2(INMNDX,1)*SBFTOR(1,2)                 &
     &                     +URNM2(INMNDX,2)*SBFTOR(2,2)                 &
     &                     +URNM2(INMNDX,3)*SBFTOR(3,2)
           DWRKDO(INMNDX,3)=URNM2(INMNDX,1)*SBFTOR(1,3)                 &
     &                     +URNM2(INMNDX,2)*SBFTOR(2,3)                 &
     &                     +URNM2(INMNDX,3)*SBFTOR(3,3)
        ELSE
           DWRKDO(INMNDX,1)=-URNM2(INMNDX,1)*SBFTOR(1,1)                &
     &                      -URNM2(INMNDX,2)*SBFTOR(2,1)                &
     &                      -URNM2(INMNDX,3)*SBFTOR(3,1)
           DWRKDO(INMNDX,2)=-URNM2(INMNDX,1)*SBFTOR(1,2)                &
     &                      -URNM2(INMNDX,2)*SBFTOR(2,2)                &
     &                      -URNM2(INMNDX,3)*SBFTOR(3,2)
           DWRKDO(INMNDX,3)=-URNM2(INMNDX,1)*SBFTOR(1,3)                &
     &                      -URNM2(INMNDX,2)*SBFTOR(2,3)                &
     &                      -URNM2(INMNDX,3)*SBFTOR(3,3)
        ENDIF !  L1ST

       ENDIF !  LOFFAJ

! debug extatt
!      ramag=ra(inmndx,1)**2+ra(inmndx,2)**2+ra(inmndx,3)**2
!      ramag=sqrt(ramag)
!      print *,'trkxti: inmndx,ra: ',inmndx,ra(inmndx,1),ra(inmndx,2),
!     .         ra(inmndx,3)
!      print *,'trkxti: inmndx,ramag: ',inmndx,ramag
! debug extatt
 1000 END DO
!
      IF(LALT.AND..NOT.LATMOD) RETURN

!
! COMPUTE RANGE CORRECTION AS DOT PRODUCT OF TOR OFFSET AND UNIT
! TOR TRACKER TO S/C C.G. VECTOR

      !print*, 'ddd in trkxti: urnm2',urnm2(1,1),urnm2(1,2),urnm2(1,3)

      CALL DOTPRD(RA,URNM2,DR,NM,NM,NM,3)

!     print*, 'ddd in trkxti 0 dr(1)=',dr(1,1),phc(1)

!------------------------------------------------------------------------

      IF( LPHC ) THEN
          DO  I=1,NM
              DR(I,1)=DR(I,1)+PHC(I)
          ENDDO
      ENDIF ! LPHC

      write(6,*)'trkxti:  dr(1,1), phc(1) ', dr(1,1), phc(1)

!------------------------------------------------------------------------


      RETURN
!
! ERROR MESSAGES
!
 9100 WRITE(6,91000)
      STOP 16
 9300 WRITE(6,93000)
      STOP 16
 9400 WRITE(6,94000)
      STOP 16
 9500 WRITE(6,95000)
      STOP 16
 9600 WRITE(6,96000)
      STOP 16
91000 FORMAT(1X,'ABNORMAL TERMINATION IN TRKXTI, NEGATIVE TIME',        &
     &          'DIFFERENCE')
93000 FORMAT(1X,'ABNORMAL TERMINATION IN TRKXTI, INTERPOLATION',        &
     &          'DIFFERENCE IS LESS THAN ZERO')
94000 FORMAT(1X,'ABNORMAL TERMINATION IN TRKXTI, -9999999 DETECTED',    &
     &          'IN SBF QUATERNIONS',1X,/,                              &
     &           'BUT INTERNAL ATT MODEL NOT SELECTED')
95000 FORMAT(1X,'ABNORMAL TERMINATION IN TRKXTI, NO MATCH ',            &
     &          'FOR MOVEABLE ANTENNA',1X,/,                            &
     &          'IN QUATERNION FILE AND ',                              &
     &          'NO INTERNAL MODEL SELECTED')
96000 FORMAT(1X,'ABNORMAL TERMINATION IN TRKXTI, -9999999 DETECTED',    &
     &          'IN MVA QUATERNIONS',1X,/,                              &
     &           'BUT INTERNAL ATT MODEL NOT SELECTED')
97000 FORMAT(1X,'WARNING FROM TRKXTI: INTERPOLATED BODY-FIXED',         &
     &          ' QUATERNION MAGNITUDE',/,' IS DIFFERENT FROM ',        &
     &          ' ONE BY AT LEAST .001',/,' QUATERNION FOR ',           &
     &          ' INTERPOLATION BOUNDARY 1 IS: ',/,4D24.16,/,           &
     &          ' QUATERNION FOR INTERPOLATION BOUNDARY 2 IS: ',        &
     &          /,4D24.16,/,'THE INTERPOLATED QUATERNION IS: ',         &
     &          /,4D24.16,/,'THE MAGNITUDE IS: ',D24.16,/,              &
     &          'THE QUATERNION WILL BE NORMALIZED')
98000 FORMAT(1X,'WARNING FROM TRKXTI: INTERPOLATED ANTENNA',            &
     &          ' QUATERNION MAGNITUDE',/,' IS DIFFERENT FROM ',        &
     &          ' ONE BY AT LEAST .001',/,' QUATERNION FOR ',           &
     &          ' INTERPOLATION BOUNDARY 1 IS: ',/,4D24.16,/,           &
     &          ' QUATERNION FOR INTERPOLATION BOUNDARY 2 IS: ',        &
     &          /,4D24.16,/,'THE INTERPOLATED QUATERNION IS: ',         &
     &          /,4D24.16,/,'THE MAGNITUDE IS: ',D24.16,/,              &
     &          'THE QUATERNION WILL BE NORMALIZED')
98001 FORMAT(' EXECUTION TERMINATING IN TRKXTI. EXTERNAL ATTITUDE')
98002 FORMAT(' INFORMATION IS BEING PASSED ON THE DATA RECORDS.')
98003 FORMAT(' VECTORS AND QUATERNIONS ARE BEING PASSED IN THE')
98004 FORMAT(' SAME RUN.')
98005 FORMAT(' ON DATA RECORDS HAS BEEN REQUESTED, BUT NO ATTITUDE')
98006 FORMAT(' INFORMATION IS PRESENT ON DATA RECORDS')
      END
