!$RADEC
      SUBROUTINE RADEC(MJDSBL,FSECN,FSECM,FSECK,OBS,OBS2,OBSCOR,OBSCR2, &
     &   XTN,XSM,XTK,VSM,XMN,XMK,RNM,RKM,URNM,URKM,XYSQNM,UHAT,ENV,     &
     &   COSZEN,COSEL,ELEVSC,RELV,RDOT,RESID,OBRATE,PMPXI,              &
     &   NMP,INDP,RPOLE,PXPOLE,                                         &
     &   NINTPL,MINTPL,S,S1,XTP,PXSPXP,COSTHG,SINTHG,THETG,WORK,        &
     &   INDSAT,SIGNL,NM,LNRATE,LPOLAD,LIGHT,LDECLN,IEXIT,              &
     &   INDSTA,DELCTN,DELCTM,DELCTK,LPTS,LPSBL,LACC,MTYPE,INDSET,      &
     &   XDPOLE,LNUTAD,AA,II,LL)
!********1*********2*********3*********4*********5*********6*********7**
! RADEC            83/04/22            0000.0    PGMR - TVM
!                  89/06/12            0000.0    PGMR - JJM
!
! FUNCTION:  COMPUTE RIGHT ASCENSION AND DECLINATION MEASUREMENTS.
!            COMPUTATIONS START AT THE FINAL RECEIVED TIME
!            MJDSn+FSECn AND PROCEED BACKWARDS UNTIL THE MODEL
!            IS COMPLETE. IF THE "LIGHT" SWITCH IS .FALSE.
!            ITERATIVE SOLUTIONS FOR THE LIGHT TIME ARE
!            PERFORMED. OTHERWISE, THE MEASUREMENT EVENT TIMES
!            ARE CONSIDERED TO BE KNOWN.
!
!            THE IEXIT ARGUMENT IS USED TO DISCRIMINATE
!            BETWEEN SUB-PATTERNS
!
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSBL   I    S    MODIFIED JULIAN DAY SECONDS OF THE FINAL
!                      RECEIVED TIME OF THE FIRST OBSERVATION IN
!                      THE BLOCK
!   FSECN    I    A    ELAPSED SECONDS FROM MJDSBL OF THE EVENT
!                      TIMES ASSOCIATED WITH TRACKING STATION Tn
!   FSECM   I/O   A    ELAPSED SECONDS FROM MJDSBL OF THE EVENT
!                      TIMES ASSOCIATED WITH SATELLITE Sm
!   FSECK   I/O   A    ELAPSED SECONDS FROM MJDSBL OF THE EVENT
!                      TIMES ASSOCIATED WITH TRACKING STATION Tk
!   OBS      I    A    THE OBSERVED RIGHT ASCENSION
!   OBS2     I    A    THE OBSERVED DECLINATION
!   OBSCOR   I    A    OBSERVATION CORRECTION ARRAY FOR RIGHT ASCENSION
!   OBSCR2   I    A    OBSERVATION CORRECTION ARRAY FOR DECLINATION
!   XTN      I    A    TRUE OF DATE INERT. COORD. OF TRACKING STA. Tn
!   XSM     I/O   A    TRUE OF REF INERT. COORD. OF SATELLITE Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   XTK      I    A    TRUE OF DATE INERT. COORD. OF TRACKING STA. Tk
!   VSM      I    A
!   XMN      I    A    MEAN POLE COORDINATES OF TRACKING STATION Tn
!   XMK      I    A    MEAN POLE COORDINATES OF TRACKING STATION Tk
!   RNM     I/O   A    SLANT RANGE FROM Tn TO Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   RKM     I/O   A    SLANT RANGE FROM Tk TO Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   URNM    I/O   A    UNIT VECTOR FROM Tn TO Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   URKM    I/O   A    UNIT VECTOR FROM Tk TO Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   XYSQNM  I/O   A    URNM(N,1)**2+URNM(N,2)**2
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   UHAT    I/O   A    E.C.F. UNIT VECTOR FROM Tn TO Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   ENV           A    STATION EAST NORTH VERTICAL VECTORS
!   COSZEN        A    COSINES OF ZENITH ANGLE
!   COSEL         A    COSINES OF ELEVATION ANGLE
!   ELEVSC        A    ELEVATION ANGLES FOR EACH STATION
!   RELV    I/O   A    E.C.F. RELATIVE VEL. BETWEEN Tn AND Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!                      VELOCITY OF SATELLITE W.R.T. TRACKING STATION
!   RDOT    I/O   A    RANGE RATE FROM Tn TO Sm
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   RESID    O    A    OBSERVED MINUS COMPUTED OBSERVATION
!   OBRATE   O    A    TIME DERIVATIVE OF OBSERVATION
!   PMPXI    O    A    PARTIAL OF OBS. W.R.T. INERTIAL S/C POS.
!   NMP      O    S    INDEX OF THE LAST MEASUREMENT ASSOCIATED WITH
!                      EACH INTERPOLATION INTERVAL
!   INDP     O    S    POINTER TO ADJUSTED POLAR MOTION VALUES
!                      ASSOCIATED WITH EACH INTERPOLATION INTERVAL
!   RPOLE         A    POLAR MOTION ROTATION MATRICES AT EACH END
!                      OF EACH INTERPOLATION INTERVAL
!   PXPOLE        A    PARTIALS OF THE TRUE POLE STATION LOCATION
!                      W.R.T. THE POLAR MOTION X & Y PARAMETERS AT
!                      EACH END OF EACH INTERPOLATION INTERVAL
!   XDPOLE        A    PARTIALS OF THE TRUE POLE STATION RATE
!                      W.R.T. THE POLAR MOTION X & Y PARAMETERS AT
!                      EACH END OF EACH INTERPOLATION INTERVAL
!   NINTPL   I    S    NUMBER OF INTERPOLATION INTERVALS
!   MINTPL        S    MAXIMUM NUMBER OF INTERPOLATION INTERVALS
!   S             A    INTERPOLATION FRACTIONS
!   S1            A    1.0 - S
!   XTP           A    TRUE POLE COORDINATES OF A TRACKING STATION
!   PXSPXP   O    A    PARTIALS OF TRUE POLE STATION COORDINATES
!                      W.R.T. POLAR MOTION FOR EACH OF "NM"
!                      MEASUREMENT TIMES
!   COSTHG  I/O   A    COSINES OF RIGHT ASCENSION OF GREENWICH
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   SINTHG  I/O   A    SINES  OF RIGHT ASCENSION OF GREENWICH
!                      OUTPUT WHEN  LDECLN = .T. AND INPUT WHEN
!                      LDECLN=.F.
!   THETG         A    GREENWICH HOUR ANGLE
!   WORK    I/O   A    WORKING ARRAY OF LENGTH "NM"
!   INDSAT   O    A    FOR A GIVEN INTERNAL SAT NO. (1,2,3) INDSAT GIVES
!                      THE
!   SIGNL    I    S    SIGN OF THE LIGHT TIME (+1. OR -1.)
!   NM       I    S    NUMBER OF MEASUREMENTS
!   LNRATE   I    S    .T.-NO TIME DERIVATIVES REQUIRED
!                      .F.-COMPUTE OBS. TIME DERIVATIVES
!   LPOLAD   I    S    SWITCH INDICATING IF POLAR MOTION ADJUST.
!                      PARTIALS ARE REQUIRED FOR EACH INTERP.
!                      INTERVAL
!   LIGHT    I    S    .TRUE. IF LIGHT TIMES HAVE BEEN DETERMINED
!   LDECLN   I    S    .TRUE. IF CALL IS FOR DECLINATION
!   IEXIT    I    S    NUMBER OF LINKS IN MEASUREMENT
!   INDSTA   I    A    POINTERS TO INTERNAL STATION NUMBERS
!   DELCTN   O    A    DELTA ET CET AT EVENT TIMES ASSOCIATED
!                      WITH TRACKING Tn
!   DELCTM   O    A    DELTA ET CET AT EVENT TIMES ASSOCIATED
!                      WITH SATELLITE Sm
!   DELCTK   O    A    DELTA ET CET AT EVENT TIMES ASSOCIATED
!                      WITH TRACKING TK
!   LPTS    I/O   A    LOGICAL FLAGS FOR ACCEPTED ON NON-ACCEPTED
!                      OBSERVATIONS IN A PSEUDO BLOCK FOR SIMULATED DATA
!                      GENERATION CAPABILITY
!   LPSBL    I    S    .TRUE. IF SIMULATED DATA GENERATION CAPABILITY
!   LACC     I    S    FLAG FOR SIMULATED DATA GENERATION CAPABILITY
!                      .TRUE. IF THE PRESENT BLOCK OF OBSERVATIONS HAS
!                      BEEN ACCEPTED.
!                      .FALSE. IF THE PRESENT BLOCK OF OBSERVATIONS HAS
!                      BEEN REJECTED
!   MTYPE    I         MEASUREMENT TYPE
!   INDSET   I         FOR A GIVEN INTERNAL SAT. NUMBER THIS TELLS
!                      WHICH SET THE SAT. BELONGS TO.
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!
!
! COMMENTS:
!
! REFERENCES:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER (NMARIN = 500)
      PARAMETER (NPASCU = 200)
!
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
! /COBLOC/ OBSERVATION BLOCK DYNAMIC ARRAY POINTERS
!          STATION INFORMATION POINTERS TO COORDINATE SYSTEM ARRAYS
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/COPARM/LOP1
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



!  jjm added COMMON CORA02
!  call to mtstst refers to
!  AA(KXTNPC),AA(KXSMPC),AA(KXTKPC),AA(KXSJPC), AA(KXTIPC), etc.
!  but the kxtnpc, etc.  pointers were not in the routine since CORA02
!  was not present

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

      ! jjm 20120531 added CORI04 to provide KISATN
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
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/CREFMT/REFMT(9)
      COMMON/CTHDOT/THDOT
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/LMARIN/ LRADEC, LMARIN, LPASCU, NXLMAR
      COMMON/NEQINT/NINTOT,MEMTOT,NEQNIM,NXNEQS
      COMMON/SIMLM/OBSINT,VINT,ELCUTO,XSMLM
!
!
      DIMENSION FSECN(NM),FSECM(NM),FSECK(NM),                          &
     &   XTN(MINTIM,3),XSM(MINTIM,3),XTK(MINTIM,3),VSM(MINTIM,3),       &
     &   UHAT(NM,3),RELV(NM,3),RDOT(NM),OBS(NM),OBS2(NM),               &
     &   OBSCOR(NM),OBSCR2(NM),XMN(3),XMK(3),RNM(NM),RKM(NM),           &
     &   XYSQNM(NM),ENV(3,3),COSZEN(NM),COSEL(NM),ELEVSC(NM),           &
     &   URNM(NM,3,*),URKM(NM,3),NMP(MINTPL,3),                         &
     &   RESID(NM),OBRATE(NM),PMPXI(NM,3),NINTPL(3),                    &
     &   INDP(MINTPL,3),RPOLE(3,3,MINTPL),PXPOLE(3,2,MINTPL),           &
     &   S(NM),S1(NM),XTP(NM,3),PXSPXP(NM,3,2,3),THETG(NM),             &
     &   COSTHG(NM,3),SINTHG(NM,3),WORK(NM),INDSAT(3),                  &
     &   INDSTA(3),DELCTM(NM),DELCTN(NM),DELCTK(NM),INDSET(3),          &
     &   AA(1),II(1),LL(1),XDPOLE(3,2,MINTPL)
      DIMENSION JOC(8),LNC(8),OBC(8)
      DIMENSION LPTS(NM)
!*****************************************************************
!     ....MARINER ARRAYS
      DIMENSION XTEMP(3)
      DIMENSION OBSMR1(NMARIN),OBSMR2(NMARIN)
      DIMENSION UNITV(3)
      DIMENSION RAP(NMARIN),DECP(NMARIN),RAE(NMARIN),DECE(NMARIN)
      DIMENSION EPRAE(NMARIN),EPDECE(NMARIN)
!*****************************************************************
!     ....PASCU ARRAYS
      DIMENSION RAMARS(NPASCU), DECMAR(NPASCU), DUMMY(3)
!*****************************************************************
!
      DATA LNCOSZ/.FALSE./,LNCOSE/.FALSE./,LNELEV/.FALSE./,             &
     &     LRAD  /.TRUE. /,LNEDOT/.TRUE. /,LDUMMY/.FALSE./
      DATA LSKIP1/.FALSE./
      DATA ZERO/0.0D0/,ONE/1.0D0/
      DATA  LDEBUG/.FALSE./
!C    DATA ID6911/ 883180800/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
!
!
!     ....CHECK IF THE TRACKING BODY IS EQUAL TO THE CENTER BODY
!     ....IF YES, NOT INTERPLANETARY -- SO MARINER MODE IS OK
!     ....IF NO,  INTERPLANETARY IS IN EFFECT -- SO STOP PROGRAM
!
      IF ( LMARIN ) THEN
!
         IF( ITBDGM .NE. ICBDGM ) THEN
            WRITE(6, 1347)
 1347       FORMAT(' RADEC:ITBDGM .NE.ICBDGM '/                         &
     &             '       INCOMPATIBLE WITH MARINER MEASUREMENTS'/     &
     &             '       STOPPING PROGRAM IN SUBROUTINE RADEC'//)
            STOP
         ENDIF
      ENDIF
!
!**********************************************************************
!
!
! IF DECLINATIONS BEING PROCESSED:
!
!    BYPASS CERTAIN OPERATIONS, EITHER NOT REQUIRED
!    OR PREVIOUSLY DONE FOR RIGHT ASCENSIONS
!
      LOP1=.NOT.LDECLN
      IF(LDECLN) GO TO 5000
!
!**********************************************************************
!
      IF( LMARIN ) THEN
!
         IF( NM .GT. NMARIN ) THEN
            WRITE(6,1139) NM, NMARIN
 1139       FORMAT(///' ERROR IN PROCESSING MARINER/VIKING RA/DEC ',    &
     &             'DATA.'/'  NUMBER OF MEASUREMENTS = ',I10/1X,        &
     &             'DIMENSION OF MARINER ARRAYS = ',I10//' STOPPING ',  &
     &             'PROGRAM IN SUBROUTINE RADEC.'///)
            STOP 16
         ENDIF
!
!        ....GET STATION POSITIONS AT MEAS TIMES FOR MAR/VIK DATA
!
         CALL RDTB2(MJDSBL, FSECN, NM, MINTIM, XTN , OBS, OBS2 ,        &
     &           RAP, DECP, RAE, DECE )
!
!
!        ....XTN NOW HAS MAR/VIK POS REL TO MARS IN EEQX 1950.0
!        ....COORDINATE SYSTEM
!
!        ....CONVERT XTN TO TRUE OF REFERENCE MARS EQ EQX
!        ....BY MULTIPLYING BY (REFMT)**T
!
         DO 9003  K =1,NM
            XTEMP(1) =REFMT(1)*XTN(K,1)+REFMT(2)*XTN(K,2)               &
     &                                 +REFMT(3)*XTN(K,3)
            XTEMP(2) =REFMT(4)*XTN(K,1)+REFMT(5)*XTN(K,2)               &
     &                                 +REFMT(6)*XTN(K,3)
            XTEMP(3) =REFMT(7)*XTN(K,1)+REFMT(8)*XTN(K,2)               &
     &                                 +REFMT(9)*XTN(K,3)
            DO 9002 III = 1,3
               XTN(K,III) = XTEMP(III)
 9002       CONTINUE
 9003    CONTINUE
!
!
!----------------------------------------------------------------------
!
!     ....TRANSFORM RA DEC OBSERVATIONS TO MARS FRAME
!     ....TO COMPUTE RESIDUALS CORRECTLY
!
         CALL RRADEC(NM, OBS, OBS2, REFMT, OBSMR1, OBSMR2)
!
!----------------------------------------------------------------------
!
         IF( LITER1 .AND. LDEBUG ) THEN
!
!        ....TRANSFORM RA,DEC OF ORBIT NORMAL FROM 1950.0 EARTH
!        ....EQEQNX TO MARS FRAME
!
            IF( RAP(1) .NE. ZERO .AND. DECP(1) .NE. ZERO) THEN
               DO 8005 K=1,NM
                  RAP(K)  = RAP(K)  * DEGRAD
                  DECP(K) = DECP(K) * DEGRAD
 8005          CONTINUE
               CALL RRADEC(NM, RAP, DECP, REFMT, RAP, DECP )
            ENDIF
!
!        ....TRANSFORM RA,DEC OF EARTH POSITION FROM 1950.0 EARTH
!        ....EQEQNX TO MARS FRAME
!
            DO 8006 K=1,NM
               IF( RAE(K) .EQ. ZERO .AND. DECE(K) .EQ. ZERO) GO TO 8006
!C             WRITE(6,*) 'RADEC: EARTH RA DEC IN DEG ',K, RAE(K),DECE(K
               RAE(K)  = RAE(K)  * DEGRAD
               DECE(K) = DECE(K) * DEGRAD
               CALL RRADEC( 1, RAE(K), DECE(K), REFMT, RAE(K), DECE(K) )
!
!        ....GET EARTH POSITION IN 1950.0 E EQEQNX FROM EPHEMERIS,
!        ....COMPUTE ITS RA, DEC AND ROTATE TO MARS FRAME
!
               CALL EARTPO(1, MINTIM, MJDSBL, FSECN(K), REFMT,          &
     &                     XTN(K,1), EPRAE(K), EPDECE(K),AA,II )
!
               WRITE(6,*)' RAE   ,EPRAE   , DECE   , EPDECE  IN DEG '
!CC            DO 8007 K=1,NM
               RAD1   = EPRAE(K)  / DEGRAD
               DEC1   = EPDECE(K) / DEGRAD
               RAD2   = RAE(K)    / DEGRAD
               DEC2   = DECE(K)   / DEGRAD
               WRITE(6,*) RAD2  ,RAD1    , DEC2   , DEC1
 8006       CONTINUE
!CC8007  CONTINUE
!
!
      ENDIF
!     ....END (LITER1 .AND. LDEBUG ) IF-THEN
!
!**********************************************************************
!
!
!     ....MODIFIED PDXSM TO  *NOT*  ROTATE XTN FROM MCF TO MCI
!     ....USE XTN WHICH HAS TOR MARS EQ EQNX STATION COORDINATES
!     ....NEED CALL TO PDXSM FOR THE XSM SATELLITE POSITIONS
!
      ISATID=II(KISATN+INDSAT(1)-1)

      CALL PDXSM( MJDSBL, FSECN, FSECM, XTN, XSM, WORK, VSM,            &
     &   RNM, WORK, URNM, WORK, MINTPL, S, WORK, COSTHG, SINTHG,        &
     &   INDSAT, SIGNL, NM, LIGHT, .TRUE., LSKIP1, IEXIT, .FALSE.,      &
     &   AA(KGRLT1), DELCTN, AA(KPSTAT), AA(KRSUNS),                    &
     &   AA(KSCRTM), AA(KXSTR), ISATID,AA, II, LL )
!
!
!***********************************************************************
!
!     ....URNM(,,1) IS TOD IN EEQEQNX AND URNM(,,2) IS TOR MARS IAU
!     ....COPY URNM(,,2) TO URNM(,,1) SO THAT REST OF RADEC WILL WORK
!     ....CORRECTLY WITHOUT BEING CHANGED
!
      DO 9008  JJJ=1,3
      DO 9008  III=1,NM
      URNM(III,JJJ,1) = URNM(III,JJJ,2)
 9008 CONTINUE
!
!
!-----------------------------------------------------------------------
!
!     ....USE THESE VALUES FOR INPUTS TO ECFIXP TO PRODUCE NO ROTATION
!
         DO 9009 JJJ=1,NM
         COSTHG(JJJ,1) = ONE
         SINTHG(JJJ,1) = ZERO
 9009    CONTINUE
!
!-----------------------------------------------------------------------
!
         IF( LDEBUG ) THEN
!
!
!     ....PRINT MARINER/VIKING POSITIONS
!
!CC      WRITE(6,8992)
!8992  FORMAT(//' ORBITER POSITIONS: N   X   Y   Z    R    LAT    LON '/
!     1       '                       (M) (M) (M)  (M)  (DEG)  (DEG)'//)
         DO 9005 III=1,NM
            ORBXY  = XTN(III,1)**2 + XTN(III,2)**2
            ORBR   = SQRT( ORBXY + XTN(III,3)**2 )
            ORBXY  = SQRT( ORBXY )
            ORBLAT = ATAN2( XTN(III,3), ORBXY ) / DEGRAD
            ORBLON = ATAN2( XTN(III,2), XTN(III,1) ) / DEGRAD
!C            WRITE(6,8991) III, (XTN(III,JJJ),JJJ=1,3), ORBR, ORBLAT,
!C     1                         ORBLON
!C8991        FORMAT(1X,I4,3D20.10,3G15.5)
 9005    CONTINUE
!
!-----------------------------------------------------------------------
!
!     ....PRINT MOON POSITIONS
!
!C         WRITE(6,8993)
!C8993  FORMAT(//' MOON POSITIONS: N   X   Y   Z    R    LAT    LON '/
!C     1    '                    (M) (M) (M)  (M)  (DEG)  (DEG)'//)
         DO 9006 III=1,NM
            ORBXY  = XSM(III,1)**2 + XSM(III,2)**2
            ORBR   = SQRT( ORBXY + XSM(III,3)**2 )
            ORBXY  = SQRT( ORBXY )
            ORBLAT = ATAN2( XSM(III,3), ORBXY ) / DEGRAD
            ORBLON = ATAN2( XSM(III,2), XSM(III,1) ) / DEGRAD
!C            WRITE(6,8991) III, (XSM(III,JJJ),JJJ=1,3), ORBR, ORBLAT,
!C     1                          ORBLON
 9006    CONTINUE
!
!C         WRITE(6,8994)
!C8994     FORMAT(//)
!
!     ....CHECK THE URNM VALUES
!
         DO 9010 III=1,NM
            UNITV(1) = XSM(III,1) - XTN(III,1)
            UNITV(2) = XSM(III,2) - XTN(III,2)
            UNITV(3) = XSM(III,3) - XTN(III,3)
            UNITM = UNITV(1)**2 + UNITV(2)**2 + UNITV(3)**2
            UNITM = SQRT(UNITM)
            UNITV(1) = UNITV(1) / UNITM
            UNITV(2) = UNITV(2) / UNITM
            UNITV(3) = UNITV(3) / UNITM
!C            WRITE(6,8771) III, (UNITV(JJJ),JJJ=1,3)
!C8771        FORMAT(' RADEC:I,UNITV(I,) ',I10,3D24.16)
 9010    CONTINUE
!
         ENDIF
!     ....END OF DEBUG IF-THEN
!
!-----------------------------------------------------------------------
!
      ENDIF
!     ....END OF LMARIN IF-THEN
!***********************************************************************
!
!
! COMPUTE VECTORS AND RANGES BETWEEN STATION(S) AND SPACECRAFT
!
!
      IF( .NOT. LMARIN ) THEN
!
         lyara = .false.
         isnumb = 0
         LADD=.TRUE.
         CALL MTSTST(MJDSBL,FSECN,FSECM,FSECK,WORK,WORK,                &
     &      XTN,XSM,XTK,WORK,WORK,WORK,VSM,WORK,WORK,WORK,XMN,XMK,WORK, &
     &      AA(KXTNPC),AA(KXSMPC),AA(KXTKPC),AA(KXSJPC),AA(KXTIPC),     &
     &      RNM,RKM,WORK,WORK,WORK,WORK,WORK,WORK,URNM,URKM,WORK,WORK,  &
     &      WORK,WORK,WORK,WORK,NMP,INDSTA,INDP,RPOLE,PXPOLE,           &
     &      NINTPL,MINTPL,S,S1,XTP,WORK,PXSPXP,AA(KPXSLV),COSTHG,SINTHG,&
     &      INDSAT,SIGNL,NM,LPOLAD,LIGHT,.TRUE.,LSKIP1,IEXIT,.FALSE.,   &
     &      AA(KGRLT1),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),DELCTN,         &
     &      DELCTK,DELCTK,AA(KPSTAT),AA(KRSUNS),AA(KCHEMR),             &
     &      AA(KCHMOR),AA(KEPOSR),AA(KCHEMT),AA(KCHMOT),AA(KEPOST),     &
     &      AA(KCHCBS),AA(KCPOSS),AA(KSCRTM),AA(KXSTT) ,AA(KXSTR) ,     &
     &      AA(KXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,    &
     &      lyara, isnumb, LADD,XDPOLE,AA(KPXDXP),AA(KXUTDT),.FALSE.,   &
     &      FSECN,LNUTAD)

!
      ENDIF
!     ....END OF ( .NOT. LMARIN ) IF-THEN
!
! COMPUTE RIGHT ASCENSIONS
      DO 1000 N=1,NM
      WORK(N)=ATAN2(URNM(N,2,1),URNM(N,1,1))
 1000 END DO
!C    WRITE(6, 12346) WORK
!
!*******************************************************************
!
!     ....ADD CHANGES TO PROCESS PASCU RA AND DEC DATA
!
      IF( LPASCU ) THEN
         IF( LITER1 ) THEN
!
         IF( NM .GT. NPASCU ) THEN
            WRITE(6,1141) NM, NPASCU
 1141       FORMAT(///' ERROR IN PROCESSING PASCU RA/DEC ',             &
     &             'DATA.'/'  NUMBER OF MEASUREMENTS = ',I10/1X,        &
     &             'DIMENSION OF PASCU ARRAYS = ',I10//' STOPPING ',    &
     &             'PROGRAM IN SUBROUTINE RADEC.'///)
            STOP 16
         ENDIF
!
!
!     ....CALCULATE MARS POSITION AT SATELLITE TIME
!     ....BARYC COORD WITH MARS ORIENTATION
!     ....PASCU DATA IS AT SATELLITE TIME
!
         NM3=3*NM
         DO 8604 I=1,NM3
         RELV(I,1)=ZERO
 8604    CONTINUE
!
!
!     ....STATION POSITION   IN BARYC IS IN XTN
!     ....SATELLITE POSITION IN BARYC IS IN XSM
!
         DO 5001 K=1,NM
!
!        ....COMPUTE VECTOR FROM STATION TO MARS CENTER
!        ....REPLACE BARYC CALL WITH ARRAYS ALREADY CALCULATED
!        ....IN MTSTST
!        ....AA(KCPOSS) IS THE CENTRAL BODY POSITION AT SATELLITE TIME
!
            DUMMY(1) = AA( KCPOSS + K - 1        ) - XTN(K,1)
            DUMMY(2) = AA( KCPOSS + K - 1 + NM   ) - XTN(K,2)
            DUMMY(3) = AA( KCPOSS + K - 1 + 2*NM ) - XTN(K,3)
!
!     ....ROTATE DUMMY FROM MARS TOR FRAME TO EARTH MEAN 50
!
         DUMX = REFMT(1)*DUMMY(1) +REFMT(4)*DUMMY(2) +REFMT(7)*DUMMY(3)
         DUMY = REFMT(2)*DUMMY(1) +REFMT(5)*DUMMY(2) +REFMT(8)*DUMMY(3)
         DUMZ = REFMT(3)*DUMMY(1) +REFMT(6)*DUMMY(2) +REFMT(9)*DUMMY(3)
         DUMMY(1) = DUMX
         DUMMY(2) = DUMY
         DUMMY(3) = DUMZ
!
!
!     ....CALCULATE RA AND DEC OF MARS CENTER
!
            RAMARS(K) = ATAN2( DUMMY(2), DUMMY(1) )
            RAMARS(K) = MOD( RAMARS(K) + TWOPI, TWOPI)
            DECMAR(K) = ATAN( DUMMY(3) / SQRT( DUMMY(1)**2 +          &
     &                                           DUMMY(2)**2 ) )
!
!
!--------------------------------------------------------------------
!     ....CORRECT DATA AFTER 1967 FOR COS DECLINATION
!     ....ID6911 = 883180800 IS 1969/01/01 IN MJDSEC
!          883180800=(40222-30000)*86400
!
          ID6911=(40222-INT(TMGDN2))*86400
!
         IF( MJDSBL .GT. ID6911 ) OBS(K) = OBS(K) / COS(DECMAR(K))
!
!--------------------------------------------------------------------
!
!     ....ADD RA , DEC OF MARS CENTER TO DELTA RA AND DEC OF MOON
!
!--------------------------------------------------
!     ....CHANGED SIGNS TO PLUS
!....................................
!CCC      OBS(K) = +OBS(K) + RAMARS(K)
!CCC      OBS2(K) = +OBS2(K) + DECMAR(K)
!.....................................
!         WRITE(6, 8003) K, OBS(K), OBS2(K)
!8003     FORMAT(' N, +OBS,+OBS2 ',I4,2(2X,G24.16))
!--------------------------------------------------
!
!     ....FLIP SIGN TO TEST IF DELTA(RA) = RA(MARS) - RA(PHOB)
!....................................
         OBS(K) = -OBS(K) + RAMARS(K)
         OBS2(K) = -OBS2(K) + DECMAR(K)
!.....................................
!C         WRITE(6, 8003) K, OBS(K), OBS2(K)
!C8003     FORMAT(' N, OBS,OBS2 ',I4,2(2X,G24.16))
!
!--------------------------------------------------------------------
!
!
 5001    CONTINUE
         ENDIF
!     .... END OF ITER=1 IF-THEN
!
      ENDIF
!     .... END OF LPASCU IF-THEN
!
!*******************************************************************
!
! FORM RESIDUALS
      IF( LMARIN ) THEN
         DO 2000 N=1,NM
            RESID(N)=OBSMR1(N)-WORK(N)
 2000       CONTINUE
      ELSE
         DO 2001 N=1,NM
            RESID(N)=OBS(N)-WORK(N)
 2001       CONTINUE
         ENDIF
!***** BEGIN DEBUG *****
!C      WRITE(6, 12345)(OBSMR1(JJJ),JJJ=1,NM)
!C      WRITE(6, 12346)WORK
!C      WRITE(6, 12347)RESID
!C12345 FORMAT(' ** RADEC **  OBSMR1='/(1X,3G25.16))
!C12346 FORMAT(' ** RADEC **  CALC  OBSC1='/(1X,3G25.16))
!C12347 FORMAT(' ** RADEC **  RESID1='/(1X,3G25.16))
!***** END DEBUG *****
! RESIDUAL ABSOLUTE VALUES
      DO 2500 N=1,NM
      WORK(N)=ABS(RESID(N))
 2500 END DO
! BRING RESIDUALS WITHIN ACCEPTED RANGES
      DO 3000 N=1,NM
      IF(WORK(N).GT.PI) RESID(N)=RESID(N)-SIGN(TWOPI,RESID(N))
 3000 END DO
! SQUARE AND SUM X&Y COMPONENTS OF UNIT VECTORS
      CALL DOTPRD(URNM,URNM,XYSQNM,NM,NM,NM,2)
!***** BEGIN DEBUG *****
!     WRITE(6,12348) XYSQNM
!     WRITE(6, 12349) WORK
!     WRITE(6, 12350) RESID
!2348 FORMAT(' ** RADEC **  XYSQNM='/(1X,3G25.16))
!2349 FORMAT(' ** RADEC **  DABS(RESID1)='/(1X,3G25.16))
!2350 FORMAT(' ** RADEC **  RESID1='/(1X,3G25.16))
!***** END DEBUG *****
      DO 4000 N=1,NM
      WORK(N)=RNM(N)*XYSQNM(N)
 4000 END DO
! PARTIAL OF R.A. W.R.T. INERTIAL S/C X POS
      DO 4200 N=1,NM
      PMPXI(N,1)=-URNM(N,2,1)/WORK(N)
 4200 END DO
! PARTIAL OF R.A. W.R.T. INERTIAL S/C Y POS
      DO 4400 N=1,NM
      PMPXI(N,2)= URNM(N,1,1)/WORK(N)
 4400 END DO
! PARTIAL OF R.A. W.R.T. INERTIAL S/C Z POS
      DO 4600 N=1,NM
      PMPXI(N,3)=ZERO
 4600 END DO
!***** BEGIN DEBUG *****
!     WRITE(6, 12351) ((PMPXI(N,I),I=1,3),N=1,NM)
!2351 FORMAT(' ** RADEC **  PMPXI1='/(1X,3G25.16))
!***** END DEBUG *****
! UHAT IS TOPOCENTRIC S/C UNIT VECTOR
      CALL ECFIXP(URNM,COSTHG,SINTHG,UHAT,NM    ,NM,NM)
!
! *******  BEGIN INTERPLANETARY ****************************************
!
      IF(ITBDGM.EQ.ICBDGM) GO TO 4608
! MAKE SURE SATELLITE IS IN TRUE OF SAT REF CENTERED AT TRACKING
! BODY
      IF( .NOT. LPASCU ) THEN
         NM3=3*NM
         DO 4604 I=1,NM3
            RELV(I,1)=ZERO
 4604    CONTINUE
         CALL BARYC(MJDSBL,FSECN,AA(KPSTAT),RELV,RELV,RELV,RELV,RELV,   &
     &              MINTIM,NM,.FALSE.,.TRUE.,.TRUE.,.FALSE.,AA)
!
      DO 4607 I=1,NM
      XSM(I,1)=XSM(I,1)-RELV(I,1)
      XSM(I,2)=XSM(I,3)-RELV(I,2)
      XSM(I,3)=XSM(I,3)-RELV(I,3)
 4607 END DO
!
      ENDIF
!     ....END OF (.NOT.LPASCU) IF-THEN
!
!
      IF( LPASCU ) THEN
!        ....AA(KEPOSR) VALUES ARE EARTH POSITIONS
!        ....AT STATION RECEIVE TIME
!        ....ALREADY CALCULATED IN MTSTST CALL
!
         DO 4606 I=1,NM
            XSM(I,1)=XSM(I,1)-AA(KEPOSR+I-1)
            XSM(I,2)=XSM(I,3)-AA(KEPOSR+NM+I-1)
            XSM(I,3)=XSM(I,3)-AA(KEPOSR+2*NM+I-1)
 4606    CONTINUE
!
      ENDIF
!     .... END OF LPASCU IF-THEN
!
 4608 CONTINUE
!
! *******  END INTERPLANETARY *****************************************
!
!
! ROTATE SATELLITE FROM TRUE OF REF TO TRUE OF DATE (TRACKING BODY TIME)
      CALL TDORTR(MJDSBL,FSECN,XSM,XSM,AA,NM,MINTIM,.TRUE.,.FALSE.,II)
! USE RELV ARRAY AS TEMPORARY STORAGE FOR E.C.F. S/C POSITION
      CALL ECFIXP(XSM ,COSTHG,SINTHG,RELV,MINTIM,NM,NM)
      IF(.NOT.LACC.AND.LPSBL)LNELEV=.FALSE.
! OBTAIN S/C ELEVATION ABOVE HORIZON
      CALL ELEV  (UHAT  ,WORK  ,WORK  ,WORK  ,WORK  , WORK  ,ENV   ,    &
     &     COSZEN,COSEL ,WORK  ,WORK  ,ELEVSC,WORK  ,NM    ,            &
     &     LNCOSZ,LNCOSE,LNELEV,LRAD  ,LNEDOT,LDUMMY,LDUMMY)
      IF(.NOT.LACC) CALL SDELEV(LPTS,ELEVSC,NM,ELCUTO,.TRUE.)
!
!----------------------------------------------------------------------
!
!     ....FOR MARINER DATA, REMOVE THE EDITING ON ELEVATION
!     ....THERE IS NO NEED FOR AN ELEVATION CUTOFF FOR THIS
!     ....DATA TYPE
!
      IF( LMARIN ) THEN
         DO 4616 I=1,NM
            ELEVSC(I) = ABS( ELEVSC(I) )
 4616       CONTINUE
      ENDIF
!     ....END OF LMARIN IF-THEN
!
!----------------------------------------------------------------------
!
      IF(.NOT.LITER1) GO TO 4675
! PERFORM OPTICAL PREPROCESSING
      DO 4610 I=1,8
      JOC(I)=KOBCOR(I,1)
 4610 END DO
      DO 4620 I=1,8
      IODD=(I-1)/2*2+1
      LNC(I)=LPRE(IODD,1).OR..NOT.LSWTCH(I,2)
 4620 END DO
      DO 4650 N=1,NM
      CALL OPTPRE  (MJDSBL    ,FSECN ( N),OBS   ( N),OBS2  ( N),        &
     &   THETG     ,XMN       ,UHAT(N,1) ,UHAT(N,2) ,UHAT(N,3) ,        &
     &   RELV(N,1) ,RELV(N,2) ,RELV(N,3) ,RNM   ( N),ELEVSC( N),        &
     &   OBSCOR( N),OBSCR2( N),OBC(1)    ,OBC(2)    ,OBC(5)    ,        &
     &   OBC(6)    ,OBC(7)    ,OBC(8)    ,OBC(3)    ,OBC(4)    ,        &
     &   LPRE(1,1) ,LPRE(5,1) ,LPRE(7,1) ,LPRE(3,1) ,AA,II        )
      DO 4630 I=1,8
      IF(LNC(I)) GO TO 4630
      JOBCOR=JOC(I)
      AA(JOBCOR)=OBC(I)
      JOC(I)=JOBCOR+1
 4630 END DO
 4650 END DO
 4675 CONTINUE
! CONVERT S/C ELEVATION ABOVE HORIZON TO DEGREES
      DO 4685 N=1,NM
      ELEVSC(N)=ELEVSC(N)/DEGRAD
 4685 END DO
      IF(LNRATE) GO TO 4950
!***** BEGIN DEBUG *****
!     WRITE(6, 12352)
!2352 FORMAT(' ** RADEC **  LNRATE = .FALSE.')
!***** END DEBUG *****
      CALL RELVEL(VSM,COSTHG,SINTHG,UHAT,RNM,OBRATE,RELV,RDOT,NM,       &
     &   LNRATE,LNRATE)
      DO 4700 N=1,NM
      OBRATE(N)=UHAT(N,1)*RELV(N,2)
 4700 END DO
      DO 4750 N=1,NM
      OBRATE(N)=OBRATE(N)-UHAT(N,2)*RELV(N,1)
 4750 END DO
      DO 4850 N=1,NM
      OBRATE(N)=OBRATE(N)/WORK(N)
 4850 END DO
      DO 4900 N=1,NM
      OBRATE(N)=OBRATE(N)+THDOT
 4900 END DO
 4950 CONTINUE
      RETURN
!
!
! DECLINATION PROCESSING BEGINS HERE
!
!
 5000 CONTINUE
!***** BEGIN DEBUG *****
!     WRITE(6, 12353)
!2353 FORMAT(' ** RADEC **  DECLINATION PROCESSING.')
!***** END DEBUG *****
! SQUARE ROOT OF SUM OF X&Y COMPONENTS OF UNIT VECTORS
      DO 6000 N=1,NM
      XYSQNM(N)=SQRT(XYSQNM(N))
 6000 END DO
! COMPUTE TANGENT OF DECLINATION
      DO 6500 N=1,NM
      WORK(N)=URNM(N,3,1)/XYSQNM(N)
 6500 END DO
! COMPUTE DECLINATIONS
      DO 7000 N=1,NM
      RESID(N)=ATAN(WORK(N))
 7000 END DO
!
!----------------------------------------------------------------------
!
!     ....MARINER
!
! FORM DECLINATION RESIDUALS
      DO 8000 N=1,NM
      IF( LMARIN ) THEN
         RESID(N)=OBSMR2(N)-RESID(N)
      ELSE
         RESID(N)=OBS2(N)-RESID(N)
      ENDIF
 8000 END DO
!----------------------------------------------------------------------
!
!***** BEGIN DEBUG *****
!C      WRITE(6, 12356) (OBSMR2(JJJ),JJJ=1,NM)
!CC   WRITE(6, 12354)XYSQNM
!C      WRITE(6, 12357) (RESID(JJJ),JJJ=1,NM)
!C12354 FORMAT(' ** RADEC **  XYSQNM='/(1X,3G25.16))
!C12355 FORMAT(' ** RADEC **  CALC OBSC2='/(1X,3G25.16))
!C12356 FORMAT(' ** RADEC **  OBSMR2='/(1X,3G25.16))
!C12357 FORMAT(' ** RADEC **  RESID ='/(1X,3G25.16))
!***** END DEBUG *****
! DIVIDE TANGENT OF DECLINATION BY RANGE
      DO 9000 N=1,NM
      WORK(N)=WORK(N)/RNM(N)
 9000 END DO
! COMPUTE PARTIAL OF DEC. W.R.T. INERTIAL S/C X POS
      DO 9200 N=1,NM
      PMPXI(N,1)=-URNM(N,1,1)*WORK(N)
 9200 END DO
! COMPUTE PARTIAL OF DEC. W.R.T. INERTIAL S/C Y POS
      DO 9400 N=1,NM
      PMPXI(N,2)=-URNM(N,2,1)*WORK(N)
 9400 END DO
! COMPUTE PARTIAL OF DEC. W.R.T. INERTIAL S/C Z POS
      DO 9600 N=1,NM
      PMPXI(N,3)= XYSQNM(N)/RNM(N)
 9600 END DO
!***** BEGIN DEBUG *****
!     WRITE(6, 12357) ((PMPXI(N,I),I=1,3),N=1,NM)
!2357 FORMAT(' ** RADEC **  PMPXI2='/(1X,3G25.16))
!***** END DEBUG *****
      IF(LNRATE) GO TO 9950
!***** BEGIN DEBUG *****
!     WRITE(6, 12358)
!2358 FORMAT(' ** RADEC **  LNRATE = .FALSE.')
!***** END DEBUG *****
      DO 9700 N=1,NM
      OBRATE(N)=RELV(N,3)-UHAT(N,3)*RDOT(N)
 9700 END DO
      DO 9800 N=1,NM
      WORK(N)=RNM(N)*XYSQNM(N)
 9800 END DO
      DO 9900 N=1,NM
      OBRATE(N)=OBRATE(N)/WORK(N)
 9900 END DO
 9950 CONTINUE
      RETURN
      END
