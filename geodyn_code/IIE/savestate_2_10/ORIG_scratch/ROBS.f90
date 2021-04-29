!$ROBS
      SUBROUTINE   ROBS(AA,II,LL,TPARTL,STAINF,N3,NEQN,IPTFMG,          &
     &   ILNFMG,NFMG,INDSTA,INDSAT,INDSET,INPSTA,INPSAT,LELEVS,         &
     &   ITYPE,ISEQ,MSEQ,LADD,ILCORR,LYARA ,ISNUMB,ILOOP ,ITMRA1,       &
     &   ITMRA2,FRSTRT,FRRATE,PRL1  ,PRL2  ,RL1   ,T1SE  ,TCP   ,       &
     &   RATIO ,FQT1S ,FQT1E ,T1SMTR,FTT1SE,IND1  ,LRDEDT,SAVEC1,       &
     &   SAVEC2,NM3,RDS1LK,FT1AV,DFQP,FREQ1,FREQ3,SAVED1,SAVED2,        &
     &   XMM,UMV,UM1,UM2,cg_par_array)
!********1*********2*********3*********4*********5*********6*********7**
! ROBS             00/00/00            0000.0    PGMR - T. MARTIN
!
! FUNCTION:  CONTROLS THE COMPUTATION OF THE RANGE OBSERVABLE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY
!   TPARTL  I/O   A    PARTIALS OF OBSERVATIONS W.R.T TIME
!   STAINF   I    A    STATION INFORMATION ARRAY:
!                      1  LATITUDE    2  COS(LATITUDE)   3  SIN(LAT.)
!                      4  LONGITUDE   5  COS(LONGITUDE)  6  SIN(LONG.)
!                      7  SPHEROID HT 8  ANTENNA MT TYPE 9  ANTENNA DISP
!                      10 WAVLN REC   11 WAVLN TRM       12 ELEV CUTOFF
!                      13 PLATE NO.   14 OCN LOAD SITE NO
!                      15 LASER DETECTOR TYPE
!   N3
!   NEQN     I    A    NUMBER OF FORCE MODEL EQUATIONS IN EACH SET
!   IPTFMG   I    A    POINTER TO PMPF ARRAY FOR EACH FORCE MODEL GROUP
!                      FOR EACH SATELLITE
!   ILNFMG   I    A    LENGTH OF EACH FORCE MODEL GROUP
!   NFMG     I    A    NUMBER OF FORCE MODEL GROUPS FOR EACH SATELLITE
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
!   ITYPE    I    S    METRIC MEASUREMENT TYPE INDICATOR. ITYPE=(MEASURE
!                      TYPE - START OF METRIC MEASUREMENT TYPES+2)/2
!                      EG. MTYPES 57,58 = ITYPE 11
!   ISEQ     I    S    INTERNAL SEQUENCE NUMBER
!   MSEQ     I    S    INTERNAL LINK INDICATOR
!   LADD     I    S    CONTROLS WHETHER THE GIVEN SIDE OF A DIFFERENCE
!                      MEASUREMENT IS HANDLED AS POSITIVE(=.TRUE) OR
!                      NEGATIVE(=.FALSE.)
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      use cgmass_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER (MOBS=300)
      PARAMETER ( SBAND  = 0.15D0 )
      PARAMETER ( ZERO   = 0.0D0 )
      PARAMETER ( ONE    = 1.0D0 )
!
      COMMON/BRESDAT/ALEN(20),ALOC(20),ADAT(80)
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CEGREL/LGRELE,LRLCLK,LGRELR,LXPCLK,LBPCLK,NXEGRL
      COMMON/CHCOEF/ COEFM(3,15),COEFJ(3,15),COEFSA(3,15),COEFS(3,15),  &
     &               COEFEM(3,15),COEFMO(3,15),COEF(3,15)
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/COBDIR/DWORD(100),CDIREC,TDIREC
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
! /CMET  / ATMOSPHERIC PRESSURE(MBARS), TEMPERATURE(DEG K), AND PARTIAL
!          PRESSURE(MBARS) OF WATER VAPOR USED BY REFRACTION MODELS
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/COBBUF/IOBSBF,IBUF1 ,IBUF2 ,IWORD1,NHEADR,NWORDM
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COBREL/KDELCT(6,8)
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
      COMMON/COFSTL/LOFEXT(2)
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
      COMMON/CORA07/KELEVT,KSRFEL,KTINT,KFSCSD,KXTIM,KFSDM,             &
     & KSGM1,KSGM2,KNOISE,KEPHMS,NXCA07
      COMMON/CORI02/KNTIME,KMJSTB,KMJSTC,KMJSTE,KISECT,                 &
     &              KINDH ,KNMH  ,KIVSAT,KIPXPF,KNTRTM,KMSTRC,          &
     &              KMSTRE,KISCTR,KISATR,KITRUN,KTRTMB,KTRTMC,          &
     &              KMJDVS,KNTMVM,NXCI02
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
      COMMON/CORL02/KLNDTT,KLWSTR,KLNDTR,KLEVMF,KLTPMS,NXCL02
      COMMON/CORL03/KLVELA,KLSTL2,KLSTH2,NXCL03
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CORL07/KLPTS,NXCL07
      COMMON/CPARFI/IUNPF1,IUNPF2,IUNPFI,ISZPFI,NBLKPF,NWRDPF,MTCALL(4)
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
! /CSTA  / STATION GEODETIC INFORMATION USED IN MEASUREMENT CORRECTIONS
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/DELOFF/NOFFST,NSATDL,NSTADL,NCGMAS,NXDELO
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)

      COMMON/FANTOM/LFGLB,LFARC,LFTARC,LFTGLB,LENDGL,LPHNTM(4),LPHTIM(4)&
     &             ,LXTRAF,LSATFA(4,3),NXFANT

!           *FANTOM* LOGICAL FLAGS FOR IDENTIFYING THE FANTOM
!                    OPTION AS GLOBAL OR ARC
!            LFGLB  -.FALSE. IF NO FANTOM CARD EXISTS IN THE ARC  SECTIO
!            LFARC  -.FALSE. IF NO FANTOM CARD EXISTS IN THE GLOBAL SECT
!            LFTGLB  -.FALSE. IF NO FANTIM CARD EXISTS IN THE ARC  SECTI
!            LFTARC  -.FALSE. IF NO FANTIM CARD EXISTS IN THE GLOBAL SEC
!            LENDGL -.TRUE.  IF ENDGLB CARD HAS BEEN READ ALREADY
!            LPHNTM - SWITCHES FOR FANTOM PARAMETERS
!                     (1)- GLOBAL GEOMETRIC
!                     (2)- GLOBAL FORCE
!                     (3)- ARC GEOMETIRIC
!                     (4)- ARC FORCE
!            LPHTIM - SWITCHES FOR FANTOM EPOCH OR APPLICATION START TIM
!                     (1)- GLOBAL GEOMETRIC
!                     (2)- GLOBAL FORCE
!                     (3)- ARC GEOMETIRIC
!                     (4)- ARC FORCE
!            LXTRAF - TRUE IF ONE NEEDS TO ALLOCATE FOR EXTRA REAL INFOR
!                     USING COLS 60-72 and 73-80 ON THE FANTOM CARD
!            LSATFA - LOGICAL FOR  UNADJUSTED/ADJUSTED FANTOM ARC FORCE
!                     PARAMETERS
!            NXFANT - NUMBER OF WORDS IN THIS COMMON BLOCK
!


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
      COMMON/ION2EL/LIONC,LFRQOP,LFRQOF,NXIONL
      COMMON/IRAMPS/INDRAM,KKRAMP
      COMMON/IORCHB/ IORM,IORJ,IORSA,IORSUN,IOREM,IORMO,IORCB,IORTB,    &
     &               IGRP,ICHBOG(2,14),IORSCB(988),NXORCH
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/OFFASI/IOFFAP
      COMMON/OFFASL/LOFFA1,LOFFA2
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/OCLRA /LOCLRA, LOCLRS, LOCLRC(3), LRAM51
      COMMON/PLNTRR/JCHEMR,JCHMOR,JEPOSR,JCHEMT,JCHMOT,JEPOST,JCHCBS,   &
     &              JCPOSS,JXSTT,JXSTR,JXSS,JFSCN1,JFSCN2,JFSCK1,       &
     &              JFSCK2,JFSCM1,JFSCM2
      COMMON/RAMPA/SF1,SF2,FREQO,RBIAS
      COMMON/RAMPI/MJDRMP,NRAMPS
      COMMON/RAMPS/TRINT(1000,2,2),FRINT(1000,2,2),TRAMP(1000,2),       &
     &             FRAMP(1000),FDRAMP(1000),RSCALX(1000),SCALX(1000),   &
     &             XINTS(1000),XINTB(1000),XINTS2(1000),RSCALY(1000)
      COMMON/RRAMPS/RINDM1,RINDM2,RINDD,RININD,AVFREQ
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
      COMMON/SETDI /NSETDP,NFRSED,NFRSDI,NFRSLN,NXSETI
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/TARGET/NLXYZ(200000),NPLTAR,NTARG,NATARG,NXTARG
!
      DIMENSION PARTRG(1000)
      DIMENSION SAVEC1(1),SAVEC2(1)
      DIMENSION SAVED1(1),SAVED2(1)
      DIMENSION RDS1LK(1),FT1AV(1),DFQP(1),FREQ1(1),FREQ3(1)
      DIMENSION IND1(NM),LRDEDT(NM)
      DIMENSION PRL1(NM3,2),PRL2(NM3,2),RL1(NM,2),T1SE(NM,2),           &
     &TCP(NM),RATIO(NM,2),FQT1S(NM),FQT1E(NM),T1SMTR(NM),FTT1SE(NM,2)
      DIMENSION ITMRA1(1),ITMRA2(1),FRSTRT(1),FRRATE(1)
      DIMENSION AA(*),II(*),LL(*),N3(MSETA),NEQN(MSETA),NINTPL(4),      &
     &   INDSTA(3),INDSET(3),INDSAT(3),LELEVS(3),IPTFMG(MAXFMG,MSATA),  &
     &   ILNFMG(MAXFMG,MSATA),NFMG(MSATA),STAINF(NSTAIN,*),             &
     &   INPSTA(3),INPSAT(3),JDELCT(6),JGRLLT(4),TPARTL(NM),ILCORR(5)
      DIMENSION KPXPF(3),                                               &
     &   JFSEC(6),KXP(6),KV(6),JXM(3),JR(5),JRR(5),JUR(5),JPRR(5),      &
     &   STANFO(25)
      DIMENSION FRQALL(20)
      DIMENSION LSWADT(80)
      DIMENSION XMM(3)
      DIMENSION UM1(NM,3),UM2(NM,3),UMV(NM,3)
      DIMENSION FSECX(MOBS)
!
      EQUIVALENCE (JFSECN,JFSEC(1)),(JFSECM,JFSEC(2)),(JFSECK,JFSEC(3)),&
     &            (JFSECJ,JFSEC(4)),(JFSECI,JFSEC(5)),(JFSCKP,JFSEC(6))
      EQUIVALENCE (KGRLT1,JGRLLT(1))
      EQUIVALENCE (JDLCTN,JDELCT(1)),(JDLCTM,JDELCT(2)),                &
     &            (JDLCTK,JDELCT(3)),(JDLCTJ,JDELCT(4)),                &
     &            (JDLCTI,JDELCT(5)),(JDLCTP,JDELCT(6))
      EQUIVALENCE (KXTN,KXP(1)),(KXTM,KXPTM,KXSM),(KXTK,KXSK)
      EQUIVALENCE (KVTN,KV(1)),(KVTM,KVPTM,KVSM),(KVTK,KVSK)
!     EQUIVALENCE (JXMN,JXMKP,JXM(1)),(JXMK,JXM(2)),(JXMI,JXMM,JXM(3))
      EQUIVALENCE (JXMN,JXMKP,JXM(1)),(JXMK,JXM(2)),(JXMI,JXM(3))
      EQUIVALENCE (KPXPFM,KPXPF(1))
      EQUIVALENCE (JR(1),KRNM),(JRR(1),KRRNM),(JUR(1),KURNM),           &
     &            (JPRR(1),KPRRNM)


      ! this equivalences all the values in common block CSTA to the
      ! current values of the STAINF array which are loaded into STANFO
      ! below.

      EQUIVALENCE (STANFO(1),RLAT)
      EQUIVALENCE (LSWADT(1),LSWTCH(1,1))
!
      data kentry/0/, iprint/10/
!
      DATA HALF/0.5D0/
! LVLBI CONTROLS WHETHER CERTAIN DIFFERENCED MEASUREMENT TYPES WILL BE
! TREATED AS SAT VLBI
      DATA LVLBI/.FALSE./

      double precision,dimension( ndim_cgmass, MINTIM ) :: cg_par_array

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************

      !write(6,'(/A, 1x, I10/)') 'robs: MTYPE = ', MTYPE
      !write(6,'(A,1x,I6)')'robs: at entry  NOFFST = ', NOFFST
      !write(6,'(A,1x,I6)')'robs: at entry  MINTIM = ', MINTIM

      LSVLBI=LVLBI

      IF(MTYPE.NE.61.AND.MTYPE.NE.62.AND.MTYPE.NE.87)THEN
      LSVLBI=.FALSE.
      ENDIF
!     IF(MTYPE.EQ.36)  LSVLBI=.TRUE.
      IF(MTYPE.EQ.36)  LSVLBI=.FALSE.

      IF(ISEQ.LT.2) LSVLBI=.FALSE.
      IF(ISEQ.LT.3.AND.MTYPE.EQ.87) LSVLBI=.FALSE.
      IIISEQ=1
      IF(LSVLBI.AND.MTYPE.EQ.61) IIISEQ=ISEQ-1
      IF(LSVLBI.AND.MTYPE.EQ.62) IIISEQ=MSEQ+1
      IF(LSVLBI.AND.MTYPE.EQ.87) IIISEQ=ISEQ-2
      IF(LSVLBI.AND.MTYPE.EQ.36) IIISEQ=ISEQ-1
      kentry = kentry + 1
!
! RAMPS ***
      JNDRAM=INDRAM
      INDRAH=INDRAM
      INDRAM=0
      IF(MTYPE.EQ.42) JNDRAM=0
      IF(INDRAM.GT.0) THEN
      DO 101 N=1,NM
      AA(KSMCOR-1+N)=0.D0
  101 END DO
      IF(ILOOP.EQ.1) THEN
      DO 102 N=1,NM
      RDS1LK(N)=0.D0
      FREQ1(N)=0.D0
      FREQ3(N)=0.D0
      DFQP(N) =0.D0
      SAVED1(N)=0.D0
      SAVED2(N)=0.D0
      FT1AV(N)= 0.D0
  102 END DO
      ENDIF
      ENDIF
! RAMPS ***
! debug table information
!     write(6,*)' general block information'
!     write(6,*)' M1,M2,D ',rindm1,rindm2,rindd
!     do 91919 i=1,KKRAMP
!     write(6,*)'t1 t2 f fdot',ITMRA1(i),ITMRA2(i),FRSTRT(i),FRRATE(i),i
!91919 continue
!     IF(NSTAIN.GT.16) THEN
!      WRITE(6,*) 'STANFO DIMENSION INSUFFICIENT IN SUBROUTINE ROBS'
!      STOP
!     ENDIF
      IEXIT =MEXIT (ITYPE)
! THIS IS NOT ALWAYS TRUE AND HAS TO BE FIXED
      IENTER=MENTER(ITYPE)

      !write(6,'(A,3(1x,I3))')&
      !      'robs: ITYPE, IEXIT, IENTER ', ITYPE, IEXIT, IENTER

      LSKIP1=IENTER.GT.1
      NGOTO =MGOTO (ITYPE)
      !write(6,'(A,1x,I3)') 'robs: NGOTO ', NGOTO
      MTCALL(ISEQ)=NGOTO
      KGOTO =NGOTO * 10000
      LNRATE=LRANGE.AND..NOT.LTDRIV
! set flag indicating that this is one way tdrss doppler data
      LTDR1W=MTYPE.EQ.56
!
! SET DYNAMIC ARRAY POINTERS FOR TIMES, POSITIONS, AND VELOCITIES
! AND CHEBYCHEV POLYNOMIALS AND COEFFICIENTS
!
      JSEQ=ISEQ+MSEQ

      !write(6,'(A,1x,I3)') 'robs: JSEQ ', JSEQ


      DO 1000 I=1,6
      JFSEC (I)=KFSECS(I,JSEQ)
      JDELCT(I)=KDELCT(I,JSEQ)
 1000 END DO
!
! SET DYNAMIC ARRAY POINTERS FOR MEAN POLE STATION COORDINATES
      DO 2000 I=1,3
      JXM   (I)=KXMN  (I,ISEQ)
 2000 END DO
!
!     ....changed 4/10/90 -- hard-coded integers replaced by IORXX =
!     ....Chebychev polynomial orders for the tracking body (IORTB),
!     ....for the central body (IORCB), and for the Moon (IORMO)
!
      IFACT=0
      IF(JSEQ.EQ.1) IFACT=1
      JCHEMR=KCHEMR+(IORTB*NM)*IFACT
      JCHMOR=KCHMOR+(IORMO*NM)*IFACT
      JEPOSR=KEPOSR+(    3*NM)*IFACT
      JCHEMT=KCHEMT+(IORTB*NM)*IFACT
      JCHMOT=KCHMOT+(IORMO*NM)*IFACT
      JEPOST=KEPOST+(    3*NM)*IFACT
      JCHCBS=KCHCBS+(IORCB*NM)*IFACT
      JCPOSS=KCPOSS+(    3*NM)*IFACT
!CC   WRITE(6,*) 'ROBS: KCPOSS, JCPOSS ',KCPOSS, JCPOSS
!CC   WRITE(6,*) 'ROBS: NM, IFACT ', NM, IFACT
      JXSTT =KXSTT +(3*MINTIM)*IFACT
      JXSTR =KXSTR +(3*MINTIM)*IFACT
      JXSS  =KXSS  +(3*MINTIM)*IFACT
!*******************DEBUG*******************
      IF(JSEQ.EQ.1) THEN
         JFSCN2=JFSECN
         JFSCM2=JFSECM
         JFSCK2=JFSECK
      ELSE
         JFSCN1=JFSECN
         JFSCM1=JFSECM
         JFSCK1=JFSECK
      ENDIF
!********************************************
!
! TRANSFER TO CALL APPROPRIATE LINK MODELLING ROUTINE
      GO TO (10000,20000,30000),NGOTO
10000 CONTINUE
!
! WARNING ! COMPUTATIONS IN MTSTST, LITEUP, AND LITEDN DEPEND ON THE
! ORDER OF THE XTN THROUGH VTI ARRAYS IN THE CALL TO MTSTST.  THE
! INDEXING OF THE SAT ARRAYS ( X(MINTIM,3,6,2)) IN LITEUP AND LITEDN
! DEPENDS ON THE ORDER OF THE SAT AND STATION POSITION AND VELOCITY
! ARRAYS BEING PASSED INTO MTSTST. DO NOT CHANGE THE ORDER OF THESE
! ARRAYS IN THE CALL TO MTSTST.
!
!     write(6,*) 'robs: MJDSBL, AA(JFSECM), AA(JFSECN) ',
!    &                  MJDSBL, AA(JFSECM), AA(JFSECN)
!
      IF(LBPCLK) THEN
         IF(NM.GT.MOBS) THEN
             WRITE(6,60000) NM,MOBS
             STOP
         ENDIF
         DO IQP=1,NM
           FSECX(IQP)=AA(JFSECN-1+IQP)
         ENDDO
      ENDIF
      CALL  MTSTST (MJDSBL     ,AA(JFSECN),AA(JFSECM),AA(JFSECK),       &
     &   AA(JFSECJ),AA(JFSECI),AA(KXTN  ),AA(KXSM  ),AA(KXTK  ),        &
     &   AA(KXSJ  ),AA(KXTI  ),AA(KVTN  ),AA(KVSM  ),AA(KVTK  ),        &
     &   AA(KVSJ  ),AA(KVTI  ),AA(JXMN  ),AA(JXMK  ),AA(JXMI  ),        &
     &   AA(KXTNPC),AA(KXSMPC),AA(KXTKPC),AA(KXSJPC),AA(KXTIPC),        &
     &   AA(KRNM  ),AA(KRKM  ),AA(KRKJ  ),AA(KRIJ  ),AA(KRRNM ),        &
     &   AA(KRRKM ),AA(KRRKJ ),AA(KRRIJ ),AA(KURNM ),AA(KURKM ),        &
     &   AA(KURKJ ),AA(KURIJ ),AA(KPRRNM),AA(KPRRKM),AA(KPRRKJ),        &
     &   AA(KPRRIJ),II(KNMP  ),INDSTA    ,II(KINDP ),AA(KRPOLE),        &
     &   AA(KPXPOL),NINTPL    ,MINTPL    ,AA(KS0   ),AA(KS1   ),        &
     &   AA(KXTP  ),AA(KRELV ),AA(KPXSXP),AA(KPXSLV),AA(KCOSTH),        &
     &   AA(KSINTH),INDSAT    ,SIGNL     ,NM        ,LPOLAD    ,        &
     &   LIGHT     ,LNRATE    ,LSKIP1    ,IEXIT     ,.FALSE.   ,        &
     &   AA(KGRLT1),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),AA(JDLCTN),        &
     &   AA(JDLCTK),AA(JDLCTI),AA(KPSTAT),AA(KRSUNS),AA(JCHEMR),        &
     &   AA(JCHMOR),AA(JEPOSR),AA(JCHEMT),AA(JCHMOT),AA(JEPOST),        &
     &   AA(JCHCBS),AA(JCPOSS),AA(KSCRTM),AA(JXSTT) ,AA(JXSTR) ,        &
     &   AA(JXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,       &
     &   lyara     ,isnumb ,LADD  ,AA(KXDPOL),AA(KPXDXP),AA(KXUTDT),    &
     &   LSVLBI, AA(KFSECS(2,IIISEQ)),LNUTAD )
      GO TO 40000
20000 CONTINUE
      IF(LTDR1W) IEXIT=2
      CALL  MTSSST (MJDSBL     ,AA(JFSECN),AA(JFSECM),AA(JFSECK),       &
     &   AA(JFSECJ),AA(JFSECI),AA(KXTN  ),AA(KXSM  ),AA(KXSK  ),        &
     &   AA(KXSJ  ),AA(KXTI  ),AA(KVTN  ),AA(KVSM  ),AA(KVSK  ),        &
     &   AA(KVSJ  ),AA(KVTI  ),AA(JXMN  ),           AA(JXMK  ),        &
     &   AA(KXTNPC),AA(KXSMPC),AA(KXTKPC),AA(KXSJPC),AA(KXTIPC),        &
     &   AA(KRNM  ),AA(KRKM  ),AA(KRKJ  ),AA(KRIJ  ),AA(KRRNM ),        &
     &   AA(KRRKM ),AA(KRRKJ ),AA(KRRIJ ),AA(KURNM ),AA(KURKM ),        &
     &   AA(KURKJ ),AA(KURIJ ),AA(KPRRNM),AA(KPRRKM),AA(KPRRKJ),        &
     &   AA(KPRRIJ),II(KNMP  ),INDSTA    ,II(KINDP ),AA(KRPOLE),        &
     &   AA(KPXPOL),NINTPL    ,MINTPL    ,AA(KS0   ),AA(KS1   ),        &
     &   AA(KXTP  ),AA(KRELV ),AA(KPXSXP),AA(KPXSLV),AA(KCOSTH),        &
     &   AA(KSINTH),INDSAT    ,SIGNL     ,NM        ,LPOLAD    ,        &
     &   LIGHT     ,LNRATE    ,LSKIP1    ,IEXIT     ,                   &
     &   AA(KGRLT1),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),AA(JDLCTN),        &
     &   AA(JDLCTI),AA(KPSTAT),AA(KRSUNS),LL(KLPTS) ,LPSBL     ,        &
     &   LACC      ,AA(KXSMBF),AA(KXSKBF),AA(KXSJBF),AA(KRSSV1),        &
     &   AA(KRSSV2),AA(KRSSV3),MTYPE,INDSET,AA(KBRTS),AA(KBRTSV),       &
     &   AA,        II,        LL,   lyara, isnumb ,LADD ,              &
     &   AA(KXDPOL),AA(KPXDXP),AA(KXUTDT),                              &
     &   AA(JXSTR) ,AA(KSCRTM),AA(JCHEMR),AA(JCHMOR),                   &
     &   AA(JXSS)  ,AA(JCHCBS),AA(JCHEMT),AA(JCHMOT),LNUTAD)
      GO TO 40000
30000 CONTINUE
      CALL  MTPTT (MJDSBL     ,AA(JFSECN),AA(JFSECM),AA(JFSECK),        &
     &   AA(KXTN  ),AA(KXPTM ),AA(KXTK  ),                              &
     &   AA(KVTN  ),AA(KVPTM ),AA(KVTK  ),                              &
     &   AA(JXMN  ),AA(JXMK  ),XMM,                                     &
     &   AA(KRNM  ),AA(KRKM  ),                                         &
     &   AA(KRRKM ),AA(KRRKJ ),AA(KURNM ),AA(KURKM ),AA(KURKJ),         &
     &   AA(KURIJ),AA(KPRRNM),AA(KPRRKM),                               &
     &   II(KNMP  ),INDSTA    ,II(KINDP ),AA(KRPOLE),                   &
     &   AA(KPXPOL),NINTPL    ,MINTPL    ,AA(KS0   ),AA(KS1   ),        &
     &   AA(KXTP  ),AA(KRELV ),AA(KPXSXP),AA(KPXSLV),AA(KCOSTH),        &
     &   AA(KSINTH),INDSAT    ,SIGNL     ,NM        ,LPOLAD    ,        &
     &   LIGHT     ,LNRATE    ,LSKIP1    ,IEXIT     ,                   &
     &   AA(KGRLT1),AA(KGRLT2),AA(KGRLT3),AA(KGRLT4),AA(JDLCTN),        &
     &   AA(JDLCTK),AA(JDLCTI),AA(KPSTAT),AA(KRSUNS),AA(JCHEMR),        &
     &   AA(JCHMOR),AA(JEPOSR),AA(JCHEMT),AA(JCHMOT),AA(JEPOST),        &
     &   AA(JCHCBS),AA(JCPOSS),AA(KSCRTM),AA(JXSTT) ,AA(JXSTR) ,        &
     &   AA(JXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,       &
     &   LYARA     ,ISNUMB , LADD , KTARNO,UM1,UM2,AA(KXDPOL),          &
     &   AA(KPXDXP),AA(KXUTDT),LNUTAD)
!
40000 CONTINUE
!
      IF(LNTDRS) GO TO 40100
      JEXIT=2
      CALL  MTSTST (MJDSBL     ,AA(JFSECN),AA(JFSECM),AA(JFSCKP),       &
     &   AA(JFSECJ),AA(JFSECI),AA(KXTN  ),AA(KXSM  ),AA(KXTKP ),        &
     &   AA(KXSJ  ),AA(KXTI  ),AA(KVTN  ),AA(KVSM  ),AA(KVTKP ),        &
     &   AA(KVSJ  ),AA(KVTI  ),AA(JXMN  ),AA(JXMKP ),AA(JXMI  ),        &
     &   AA(KXTNPC),AA(KXSMPC),AA(KXTKPP),AA(KXSJPC),AA(KXTIPC),        &
     &   AA(KRNM  ),AA(KRKPM ),AA(KRKJ  ),AA(KRIJ  ),AA(KRRNM ),        &
     &   AA(KRRKPM),AA(KRRKJ ),AA(KRRIJ ),AA(KURNM ),AA(KURKPM),        &
     &   AA(KURKJ ),AA(KURIJ ),AA(KPRRNM),AA(KPRRKP),AA(KPRRKJ),        &
     &   AA(KPRRIJ),II(KNMP  ),INDSTA    ,II(KINDP ),AA(KRPOLE),        &
     &   AA(KPXPOL),NINTPL    ,MINTPL    ,AA(KS0   ),AA(KS1   ),        &
     &   AA(KXTP  ),AA(KRELV ),AA(KPXSXP),AA(KPXSLV),AA(KCOSTH),        &
     &   AA(KSINTH),INDSAT    ,SIGNL     ,NM        ,LPOLAD    ,        &
     &   LIGHT     ,LNRATE    ,.TRUE.    ,JEXIT     ,.TRUE.    ,        &
     &   AA(KGRLT1),AA(KGRLT5),AA(KGRLT3),AA(KGRLT4),AA(JDLCTN),        &
     &   AA(JDLCTP),AA(JDLCTI),AA(KPSTAT),AA(KRSUNS),AA(JCHEMR),        &
     &   AA(JCHMOR),AA(JEPOSR),AA(JCHEMT),AA(JCHMOT),AA(JEPOST),        &
     &   AA(JCHCBS),AA(JCPOSS),AA(KSCRTM),AA(JXSTT) ,AA(JXSTR) ,        &
     &   AA(JXSS)  ,MTYPE  ,INDSET,AA(KBRTS),AA(KBRTSV),AA,II,LL,       &
     &   lyara, isnumb, LADD,AA(KXDPOL),AA(KPXDXP),AA(KXUTDT),          &
     &   .FALSE., AA(KFSECS(2,IIISEQ)),LNUTAD )
!
      IEXIT=5
40100 CONTINUE
      LNEG=.NOT.LADD
      IF(INDRAM.GT.0) LNEG=LADD
!
! Loop thru each link of the observation tracing the signal path from
! the received end to the transmitted end
!
! COMPUTE FREQUENCIES FOR USE IN IONOSPHERIC MODEL FROM INFORMATION
! PROVIDED IN LOCATION, LENGTH AND DATA RECORDS IF FREQUENCIES ARE
! NOT PROVIDED ON THE OFFSET CARDS.
!
      IF(LIONC.AND.(.NOT.LFRQOP)) THEN
        CALL LENFO(AA,II)
        NIHDR=MAX(INT(ALEN(9)),INT(ALEN(10)))
        ISIZE=10*NIHDR+13
        NPART=0
        DO 40120 ICALC=1,80
          IF(LSWADT(ICALC)) NPART=NPART+1
40120   CONTINUE
        ADAT(1)=DBLE(NPART)
        ADAT(2)=DWORD(9)
        ADAT(3)=DWORD(10)
        K2PT=KOBBUF+INT(DWORD(5))-2
        DO 40130 IADAT=1,ISIZE-3
          ADAT(IADAT+3)=AA(K2PT+IADAT)
40130   CONTINUE
        CALL FRQGET(FRQALL)
      ENDIF

      DO 90000 ILINK=IENTER,IEXIT



      IF(LBPCLK.AND.ILINK.EQ.IENTER) GO TO 90000

      IFROM =ILINKF(ILINK)
      ITO   =ILINKT(ILINK)
      !write(6,'(A,3(1x,I4))')'robs: ilink, ifrom, ito ', &
      !                              ilink, ifrom, ito
      JLINK =ILINK
      IF(ILINK.EQ.5) JLINK=2
      IDELCT=JDELCT(IFROM)
      IF(ILINK.EQ.5) IDELCT=JDELCT(6)
      IF(LTDR1W) THEN
         ! skip links 3 & 4 for one way TDRSS doppler data
         IF(ILINK.EQ.3 .OR. ILINK.EQ.4) GO TO 90000
      ENDIF
      MADDC=3*NM
      MADDC2=12*MINTIM
      IFSEC =JFSEC(ITO)
      IFSEC2=JFSEC(IFROM)
      IF(ILINK.EQ.5) IFSEC2=JFSCKP

! Determine which satellite to set as the "from" or "to" satellite.
! ISATF and ISATT are always set based on a signal going from a station
! to a satellite or in multiple satellite links going from the tracked
! satellite to the relay satellite. ISATF and ISATT are internal satelli
! numbers that are related to the satellite numbers in the header record
! thru the INPSAT array

      ISATF =ISATFN(ILINK,NGOTO)
      ISATT =ISATTN(ILINK,NGOTO)

! Determine which station the signal moves "from" and "to"
! ISTAF and ISTAT are  set based on a signal going from a station
! to a satellite or in multiple station links going from the tracked
! station (ground transponder) to the relay satellite. ISTAF and ISTAT
! are internal station numbers that are related to the stations numbers
! in the header record thru the INPSTA array

      ISTAF =ISTAFN(ILINK,NGOTO)
      ISTAT =ISTATN(ILINK,NGOTO)
!
! Set flag indicating this link has two satellites
      L2SATL=ISATT.GT.0 .AND. ISATF.GT.0

      !write(6,'(A,3x,L1,1x,2(1x,I4))') &
      !      'robs: L2SATL, ISATT, ISATF ', &
      !             L2SATL, ISATT, ISATF

! Set flag indicating this link has two stations

      L2STAL=ISTAT.GT.0 .AND. ISTAF.GT.0
!
      IX    =KXP   (ITO)
      IX2   =KXP   (IFROM)
      IF(ILINK.EQ.5) IX2 = KXTKP
      IV    =KV    (ITO)
      IV2   =KV    (IFROM)
      IF(ILINK.EQ.5) IV2 = KVTKP
      IR    =JR    (ILINK)
      IRR   =JRR   (ILINK)
      IUR   =JUR   (ILINK)
      IPMPX =IUR
      IF(LRANGE) GO TO 40500
      IPRR  =JPRR(ILINK)
      IPMPX =IPRR
40500 CONTINUE

! Initialize pointer for track. point offset correction for "to"  sat

      JOCTPC=KSMCOR

! Initialize pointer for track. point offset correction for "from"  sat

      JOCTP2=KSMCOR
      JOCDRY=KSMCOR
      JOCWET=KSMCOR
      JOCANT=KSMCOR
      JOCION=KSMCOR
      JOCIOS=KSMCOR
      JOCIOS=KSMCOR
      JOCDLG=KSMCOR
      JOCDG2=KSMCOR
      JOCDLS=KSMCOR
      JOCDS2=KSMCOR
      ISAT  =1
      ISATP =1
      ISATP2=1
      ISATID=0
      IURSAT=0
      IF(ISATT.LE.0) GO TO 41000
      ISATP =INPSAT(ISATT)
      ISET1 =INDSET(ISATT)
      ISATID=JSATNO(ISATP)

      ! set location for offset correction
      JOCTPC=KOBCOR(2,ISATP)

      JOCIOS=KOBCOR(7,ISATP)
      JOCDLS=KOBCOR(9,ISATP)
      ISAT  =INDSAT(ISATT)

      !write(6,'(A,3(1x,I10))') 'robs: isatp,  isatid, JOCTPC ',&
      !                                isatp,  isatid, JOCTPC

      IF(ISATF.LE.0) GO TO 41000

! Set pointers for the "from" satellite for two satellite links

      ISATP2=INPSAT(ISATF)
      ISET2 =INDSET(ISATF)
      IURSAT=JSATNO(ISATP2)
      JOCTP2=KOBCOR(2,ISATP2)
      JOCDS2=KOBCOR(9,ISATP2)

      !write(6,'(A,2(1x,I10))') 'robs: isatp2, iursat, JOCTP2 ', &
      !                                isatp2, iursat, JOCTP2

41000 CONTINUE


      LNELEV=.TRUE.
      JENV  =KXLOCV
      NMSTA1=0
      ISTA  =1
      ISTAP =1
      ISTAP2=1
      ISTAID=0
      IELVPT=0
      WAVLEN=SBAND
      IF(ISTAF.LE.0) GO TO 42000
      ISTAP =INPSTA(ISTAF)
      ISTAID=JSTANO(ISTAP)
      JMET  =KOBCOR(1,ISTAP)
      JOCDRY=KOBCOR(3,ISTAP)
      JOCWET=KOBCOR(4,ISTAP)
      JOCANT=KOBCOR(5,ISTAP)
      JOCION=KOBCOR(6,ISTAP)
      JOCDLG=KOBCOR(8,ISTAP)
!
! Set pointers for the "to" station for two station links
!
      IF(ISTAT.LE.0) GO TO 41400
      ISTAP2=INPSTA(ISTAT)
      JOCDG2=KOBCOR(8,ISTAP2)
41400 CONTINUE
      LNELEV=.NOT.LELEVS(ISTAF)
      JENV  =KENVN(ISTAF,ISEQ)
      ISTA  =INDSTA(ISTAF)

      ! The current values of the STAINF array are loaded into the
      ! STANFO array.
      ! The STANFO array is equivalenced to the start of the
      ! common block CSTA and thus the common block values are set
      ! to the STANFO values.

      DO 41500 I=1,NSTAIN
      STANFO(I)=STAINF(I,ISTA)
41500 END DO

!      ISTAID=ISTA
! debug extatt
! commented the above setting of istaid
! debug extatt
!cc   WAVLEN=VLIGHT/BLKDAT(3,1)
!
!     ....fix for glrs problem 9/17/90   >>>>
!
      if( BLKDAT(3,1) .gt. zero ) wavlen = vlight / blkdat(3,1)
!
!     ....fix for glrs problem 9/17/90   <<<<
!
      MLINK =MOD(ILINK,2)
      IF(MLINK.EQ.0.AND.WAVXMT.GT.ZERO) WAVLEN=WAVXMT
      IF(MLINK.EQ.1.AND.WAVREC.GT.ZERO) WAVLEN=WAVREC
      IELVPT=(INDELV(ISTAF,ISEQ)-1)*NM
      IELVPT=MAX(IELVPT,0)
      NMSTA1=NM*(ISTAF-1)
!     WRITE(6,*) ' ROBS : ISTAF,NMSTA1  ',ISTAF,NMSTA1
42000 CONTINUE
!
! CHECK FOR APPLICABLE TRANSPONDER OFFSETS OR DELAYS
!
      JXYZOF(1)=KFRQOF
      JXYZOF(2)=KFRQOF
      JXYOF2(1)=KFRQOF
      JXYOF2(2)=KFRQOF
      JXYOF3(1)=KFRQOF
      JXYOF3(2)=KFRQOF
      JSADLY(1)=KFRQSA
      JSADLY(2)=KFRQSA
      !write(6,*)'robs: KFRQSA, JSADLY(1:2) ', KFRQSA, JSADLY(1:2)
      JSTDLY(1)=KFRQST
      JSTDLY(2)=KFRQST
      !write(6,*)'robs: KFRQST, JSTDLY(1:2) ', KFRQST, JSTDLY(1:2)
      JXYZCG(1,1)=KFRQOF
      JXYZCG(2,1)=KFRQOF
      JXYZCG(1,2)=KFRQOF
      JXYZCG(2,2)=KFRQOF
      !write(6,'(A,1x,I2,1x,I10)') &
      !      'robs: ilink, KFRQOF ', ilink, KFRQOF
      !write(6,'(A,1x,I2,4(1x,I10))') &
      !      'robs: ilink, JXYZCG ', ilink, JXYZCG
      MJDFCG(1,1)=0
      MJDFCG(2,1)=0
      MJDFCG(1,2)=0
      MJDFCG(2,2)=0
      !write(6,'(A,4(1x,I10))') 'robs: MJDFCG ', MJDFCG
      LOFEXT(1)=.FALSE.
      LOFEXT(2)=.FALSE.

      NOFFDL=NOFFST+NSATDL+NSTADL

      !!! NOFFDL=NOFFST+NSATDL+NSTADL + NCGMAS ! debug only

      !write(6,'(A,1x,I6)')'robs: NOFFDL ', NOFFDL
      !write(6,'(A,4(1x,I6))')&
      !      'robs: NOFFST, NSATDL, NSTADL, NCGMAS ', &
      !             NOFFST, NSATDL, NSTADL, NCGMAS

      IF(NOFFDL.LE.0) GO TO 42200
!
! BEFORE CALL TO OFFDEL
! CALL IDANT TO SEE IF NEW PREPRO ID SCHEME FOR OFFSET IS REQUESTED
!
      !write(6,'(A,1x,I2)')'robs: ILINK ', ILINK
!      write(6,*)'robs:bef IDANT sat AA(JSADLY(1))  ', AA(JSADLY(1))
!      write(6,*)'robs:bef IDANT sat AA(JSADLY(2))  ', AA(JSADLY(2))
!      write(6,*)'robs:bef IDANT STA AA(JSTDLY(1))  ', AA(JSTDLY(1))
!      write(6,*)'robs:bef IDANT STA AA(JSTDLY(2))  ', AA(JSTDLY(2))

      LOFFA1=.FALSE.
      LOFFA2=.FALSE.

      IOFFAP=0   ! for OFFADJ
      !write(6,'(A,1x,I10)')  'robs: IOFFAP = ', IOFFAP
      !write(6,'(A,2(3x,L1))')'robs: LOFFA1, LOFFA2 ',  LOFFA1, LOFFA2


      IF(NOFFST.GT.0) THEN
       IF(.NOT.L2SATL) THEN
        CALL IDANT(ISATID,ISTAID,.FALSE.,IANT)
       ELSE
        CALL IDANT(ISATID,IURSAT,.TRUE.,IANT)
       ENDIF
      ENDIF

!      write(6,*)'robs: aft IDANT call IANT = ', IANT

! Compute OFFSET and CENTER OF MASS pointers for the relay satellite

      !write(6,'(A,2(1x,I10))')'robs: ILINK, ISATID ', ILINK, ISATID
      !write(6,'(A,1x,I10)') 'robs:bef OFFDEL NOFFST = ', NOFFST
!      write(6,*)'robs:bef OFFDEL AA(JSADLY(1))  ', AA(JSADLY(1))
!      write(6,*)'robs:bef OFFDEL AA(JSADLY(2))  ', AA(JSADLY(2))
!      write(6,*)'robs:bef OFFDEL STA AA(JSTDLY(1))  ', AA(JSTDLY(1))
!      write(6,*)'robs:bef OFFDEL STA AA(JSTDLY(2))  ', AA(JSTDLY(2))

      !write(6,*)'robs:bef OFFDEL II(KISATC) ', II(KISATC)
      !write(6,*)'robs:bef OFFDEL II(KMJDCG) ', II(KMJDCG)
      !write(6,*)'robs:bef OFFDEL JXYZCG(1,1),MJDFCG(1,1) ', &
      !                           JXYZCG(1,1),MJDFCG(1,1)

      CALL OFFDEL(ISATID,ISTAID,BLKDAT(3,1),                            &
     &   II(KISATO),II(KISATD),II(KISATC),II(KISTAD),                   &
     &   AA(KFRQOF),AA(KFRQSA),AA(KFRQST),II(KMJDCG),JCOOR1,            &
     &   JXYZOF(1),JSADLY(1),JSTDLY(1),JXYZCG(1,1),MJDFCG(1,1),         &
     &   IANT,II(KANTOF), JXYOF2(1),LL(KNSCID),II(KIANTO),LL(KEXTOF),   &
     &   LOFEXT(1),.TRUE.,II,FRQLNK,ISEQ,AA,JXYOF3(1))
      ICOOR1=0
      IF(JCOOR1.GT.0) THEN
        ICOOR1=II(JCOOR1)
      ENDIF
      !write(6,*)'robs:aft OFFDEL II(KISATC) ', II(KISATC)
      !write(6,*)'robs:aft OFFDEL II(KMJDCG) ', II(KMJDCG)
      !write(6,*)'robs:aft OFFDEL JXYZCG(1,1),MJDFCG(1,1) ', &
      !                           JXYZCG(1,1),MJDFCG(1,1)

      !write(6,'(A,3x,L1)')'robs: L2SATL ', L2SATL

      IF(.NOT. L2SATL) GO TO 42200

! BEFORE CALL TO OFFDEL
! CALL IDANT TO SEE IF NEW PREPRO ID SCHEME FOR OFFSET IS REQUESTED

      IF(NOFFST.GT.0) CALL IDANT(IURSAT,ISATID,.TRUE.,IANT)

!      write(6,*)'robs:2 aft IDANT call NOFFST = ', NOFFST
!      write(6,*)'robs:2 aft IDANT call IANT = ', IANT
!
! Compute OFFSET and CENTER OF MASS pointers for the user satellite

      !write(6,'(A,2(1x,I10))')'robs:2 ILINK, IURSAT ', ILINK, IURSAT
      !write(6,'(A,2(1x,I10))')'robs:bef call 2 OFFDEL NOFFST = ', NOFFST
!      write(6,*)'robs:bef call 2 OFFDEL AA(JSADLY(1))  ', AA(JSADLY(1))
!      write(6,*)'robs:bef call 2 OFFDEL AA(JSADLY(2))  ', AA(JSADLY(2))
!      write(6,*)'robs:bef call 2 OFFDEL STA AA(JSTDLY(1))  ', &
!                                            AA(JSTDLY(1))
!      write(6,*)'robs:befcall 2  OFFDEL STA AA(JSTDLY(2))  ', &
!                                            AA(JSTDLY(2))

!      write(6,*)'robs:befcall 2  OFFDEL II(KISATC) ', II(KISATC)
      !write(6,*)'robs:befcall 2  OFFDEL II(KMJDCG) ', II(KMJDCG)
      !write(6,*)'robs:befcall 2  OFFDEL JXYZCG(1,2),MJDFCG(1,2) ', &
      !                                  JXYZCG(1,2),MJDFCG(1,2)


      CALL OFFDEL(IURSAT,ISTAID,BLKDAT(3,1),                            &
     &   II(KISATO),II(KISATD),II(KISATC),II(KISTAD),                   &
     &   AA(KFRQOF),AA(KFRQSA),AA(KFRQST),II(KMJDCG),JCOOR2,            &
     &   JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(1,2),MJDFCG(1,2),         &
     &   IANT,ii(KANTOF),JXYOF2(2),LL(KNSCID),II(KIANTO),LL(KEXTOF),    &
     &   LOFEXT(2),.FALSE.,II,FRQLNK,ISEQ,AA,JXYOF3(2))
      ICOOR2=0
      IF(JCOOR2.GT.0) THEN
        ICOOR2=II(JCOOR2)
      ENDIF
42200 CONTINUE

      ICOSTH=KCOSTH+NMSTA1
      ISINTH=KSINTH+NMSTA1
      JELEVS=KELEVS+IELVPT
      RSCALE=DOPSCL(ILINK,ISEQ)
      LSCALE=RSCALE.NE.ZERO .AND. RSCALE.NE.ONE

!     WRITE(6,*) 'ROBS: ILINK ', ILINK
!     WRITE(6,*) 'ROBS: KELEVS,NMSTA1,JELEVS ',KELEVS,NMSTA1,JELEVS
!     WRITE(6,*) 'ROBS: ISEQ,  MSEQ  ',ISEQ,MSEQ
!     WRITE(6,*) 'ROBS: BEFORE ROBSUM LNELEV ',LNELEV

! SUM OBSERVATIONS AND CORRECTIONS

      !WRITE(6,'(A,1x,E24.16)') &
      !  'ROBS: BEFORE ROBSUM aa(joctpc) ', aa(joctpc)
      !WRITE(6,'(A,1x,E24.16)')  &
      !  'ROBS: BEFORE ROBSUM aa(joctp2) ', aa(joctp2)
!      WRITE(6,*) 'ROBS: BEFORE ROBSUM LPRE  (,ISTAP) ',  &
!                                    ( LPRE(jjj,ISTAP) ,jjj=1,24)
!      WRITE(6,*) 'ROBS: BEFORE ROBSUM LPRE  (,ISATP) ',  &
!                                    ( LPRE(jjj,ISATP) ,jjj=1,24)
!
      JELEVS=KELEVS+NM*NELVC
      IF(MSEQ.EQ.0.AND.INDXEL(ILINK,ISEQ).EQ.1) THEN
         NELVC=NELVC+1
         LNELEV=.FALSE.
      ENDIF
      IF(LIONC.AND.(.NOT.LFRQOP))FRQLNK=FRQALL(ILINK)
      IISEQ=1
      IF(MTYPE.EQ.54.AND.MSEQ.EQ.0) IISEQ=2

!      WRITE(6,*) 'ROBS: JNDRAM = ', JNDRAM

      IF(JNDRAM.GT.0) THEN

          !WRITE(6,*) 'ROBS: call gettmi isatid = ', isatid

          CALL GETTMI(NM,AA(JFSECN),AA(KRNM),AA(KRKM),                  &
                      VLIGHT,IISEQ,TRINT,1000)
          CALL GETRFQ(MJDSBL,NM,TRINT(1,1,IISEQ),FRINT(1,1,IISEQ),      &
                      MJDRMP, TRAMP,FRAMP,FDRAMP,NRAMPS)
          CALL GETRFQ(MJDSBL,NM,TRINT(1,2,IISEQ),FRINT(1,2,IISEQ),      &
                      MJDRMP, TRAMP,FRAMP,FDRAMP,NRAMPS)

          LSCALE=.TRUE.
          CALL GETRCP(NM,FREQO,FRINT(1,1,IISEQ),SF2,VLIGHT,RSCALX)
          DO I=1,NM
             RSCALY(I)=RSCALX(I)
          ENDDO
      ELSE
          DO  I=1,NM
              RSCALX(I)=RSCALE
          ENDDO
      ENDIF !  JNDRAM.GT.0


      IF(JNDRAM.EQ.0.OR.MTYPE.EQ.54) THEN
          DO  I=1,NM
              RSCALY(I)=RSCALE
          ENDDO
      ENDIF

!      write(6,*) 'robs: krlcor, AA( krlcor ) ', krlcor, AA( krlcor )

      !write(6,'(A,1x,I2)')'robs:bef ROBSUM ILINK ', ILINK

!      write(6,*)'robs:bef ROBSUM KFRQSA, JSADLY(1:2) ', &
!                                 KFRQSA, JSADLY(1:2)
!      write(6,*)'robs:bef ROBSUM AA(JSADLY(1))  ', AA(JSADLY(1))
!      write(6,*)'robs:bef ROBSUM AA(JSADLY(2))  ', AA(JSADLY(2))
!      !write(6,*)'robs:bef ROBSUM STA AA(JSTDLY(1))  ', AA(JSTDLY(1))
!      !write(6,*)'robs:bef ROBSUM STA AA(JSTDLY(2))  ', AA(JSTDLY(2))
!      !write(6,*)'robs:bef ROBSUM STA AA(JOCANT)  ', AA(JOCANT)

      !write(6,*)'robs:bef ROBSUM AA(JXYZCG(1:2,1)) ', AA(JXYZCG(1:2,1))
      !write(6,*)'robs:bef ROBSUM AA(JXYZCG(1:2,2)) ', AA(JXYZCG(1:2,2))

       IF(JCOOR1.LE.0) THEN
       IC1=0
       ELSE
       IC1=II(JCOOR1)
       ENDIF
       IF(JCOOR2.LE.0) THEN
       IC2=0
       ELSE
       IC2=II(JCOOR2)
       ENDIF
      CALL ROBSUM(AA(IX),AA(IV),AA(IR),AA(IRR),AA(IUR),AA(ICOSTH),      &
     &   AA(ISINTH),AA(JENV),AA(JELEVS),AA(JSADLY(1)),AA(JSADLY(2)),    &
     &   AA(JSTDLY(1)),AA(JSTDLY(2)),AA(IFSEC),AA(JXYZCG(1,1)),         &
     &   AA(JXYZCG(2,1)),AA(JXYZCG(1,2)),AA(JXYZCG(2,2)),AA(JXYZOF(1)), &
     &   AA(JXYZOF(2)), AA(KWORK),AA(KU),                               &
     &   AA(KRA),AA(KUHAT),AA(KRELV),AA(JOCANT),AA(JOCDRY),AA(JOCWET),  &
     &   AA(JOCION),AA(JOCIOS),AA(JOCDLG),AA(JOCDG2),                   &
     &   AA(JOCDLS),AA(JOCDS2),AA(JOCTPC),AA(JOCTP2),                   &
     &   AA(KOBSC),AA(KOBSTR),TPARTL,NM,ITYPE,MTYPE,LNELEV,             &
     &   LPRE  (1,ISTAP),LPRE  (1,ISATP),BLKDAT(1,ISTAP),AA(JMET),      &
     &   RSCALY,LNEG,L2SATL,AA(IX2),AA(IV2),AA(IFSEC2),JLINK,           &
     &   AA(JGRLLT(ILINK)),AA(IDELCT),LL(KLPTS),AA,II,IC1,       &
     &   IC2,AA(KRLCOR),INDSTA,AA(KTPMES+MINTIM*2*(ITO-1)),      &
     &   LL(KLTPMS+MINTIM*3*(ITO-1)),AA(KTPMES+MINTIM*2*(IFROM-1)),     &
     &   LL(KLTPMS+MINTIM*3*(IFROM-1)),AA(JXYOF2(1)),AA(JXYOF2(2)),     &
     &   ISET1,ISET2,AA(IUR+MADDC),ILINK,ILCORR,II(KLLBIA),LL,ISATID,   &
     &   ISTAID, MJDSBL, AA(JFSECN), RL1(1,ILOOP),FRQLNK,AA(KSRTCH),    &
     &   AA(KATROT),AA(KANTBL),AA(KXHOLD),SCALX,ISEQ,MJDSBL,AA(KW1PU),  &
     &   AA(KW1PU+6*MINTIM),AA(JXYOF3(1)),AA(JXYOF3(2)) ,  &
         cg_par_array  )

      !WRITE(6,'(A,1x,E24.16)')  &
      !  'ROBS: aft ROBSUM aa(joctpc) ', aa(joctpc)
      !WRITE(6,'(A,1x,E24.16)')  &
      !  'ROBS: aft ROBSUM aa(joctp2) ', aa(joctp2)

      !write(6,'(A,1x,I3,3x,L1)') 'robs: JSEQ, LNEG ', JSEQ, LNEG

      !if( LPHNTM(3) )then
      !    !write(6,'(/a)') 'robs: aft call ROBSUM '
      !    !write(6,'(A,1x,I2)') 'robs: cg_par_array ILINK =  ', ILINK
      !    write(6,'(6(1x,E15.7))') cg_par_array(1:ndim_cgmass,1)
      !endif  ! LPHNTM(3)

      IF(LBPCLK) THEN
        DO IQP=1,NM
         AA(JELEVS-1+NM+IQP)=AA(JELEVS-1+IQP)
        ENDDO
      ENDIF



! RAMP ***
      IF(INDRAM.GT.0) THEN
! SAVE PARTIALS
      DO 42300 N=1,NM3
!     write(6,*)' dbg part b',prl1(n,ilink)
      PRL1(N,ILINK)=PRL1(N,1)+AA(IPMPX+MADDC-1+N)
      PRL2(N,ILINK)=PRL2(N,1)+AA(IPMPX-1+N)
!     write(6,*)' dbg part ',prl1(n,ilink),n,ilink,aa(ipmpx+maddc-1+n)
42300 END DO

!     write(6,*)' dbg part ',prl1(1,ilink),ilink,aa(ipmpx+maddc)
!     WRITE(6,*)' DBG ILINK ILOOP ',ILINK,ILOOP

      !write(6,'(A,2(1x,I10))') 'robs: aft 42300 ILINK, IEXIT ', &
      !                                          ILINK, IEXIT

      IF(ILINK.EQ.IEXIT) THEN


! ADD UP ALL MEASUREMENT CORRECTIONS FOR THIS END OF THE COUNTING
! INTERVAL BOTH LINKS SO THE TOTAL CORRECTION WOULD BE APPLIED TO
! THE TWO WAY RANGE

      N1=KSMCOR
      N2=N1+NM-1

      !write(6,'(A,3x,L1)') 'robs: lram51 ', lram51

      DO 7000 IHEADR=2,NHEADR

      JHEADR=IHEADR-1

      DO 6000 IWORD =2,8

      IF( lram51 ) go to 6100

      !write(6,'(A,2(1x,I5),3x,L1)') &
      !      'robs: IWORD, JHEADR, LPRE(IWORD ,JHEADR) ', &
      !             IWORD, JHEADR, LPRE(IWORD ,JHEADR)

      IF(LPRE  (IWORD ,JHEADR)) GO TO 6000
 6100 continue
      IF(.NOT.LSWTCH(IWORD ,IHEADR)) GO TO 6000
      IF(MDRY(MTYPE).EQ.1 .AND. IWORD.EQ. 3) GOTO 6000
      IF(MWET(MTYPE).EQ.1 .AND. IWORD.EQ. 4) GOTO 6000
      JOBCOR=KOBCOR(IWORD ,JHEADR)
      J=1
      DO 5000 NC=N1,N2
      AA(NC    )   =AA(NC    )   +AA(JOBCOR)
      IF(ILOOP.EQ.1) THEN
      RDS1LK(J)=AA(NC)
      J=J+1
      ELSE
      IF(IWORD.EQ.8) THEN
      AA(NC    )   =AA(NC    )-RDS1LK(J)
      J=J+1
      ENDIF
      ENDIF

      !write(6,'(A,2(1x,I10), 2(1x,E15.7))')&
      !'robs: IHEADR,IWORD,AA(JOBCOR),AA(NC) ',&
      !       IHEADR,IWORD,AA(JOBCOR),AA(NC)

      JOBCOR=JOBCOR+1
 5000 END DO
 6000 END DO
 7000 END DO

! APPLY  THE TOTAL CORRECTION OF THE 2 WAY RANGE DOWN WHERE INDICATED

!      write(6,*)'robs: ILINK ', ILINK

      K1=JFSECN
      IF(ILOOP.EQ.1)THEN
          K2=1
      ELSE
          K2=2
      ENDIF

!      write(6,*)'robs: rl1(1,iloop),1,iloop,aa(k1-1+1) ', &
!                       rl1(1,iloop),1,iloop,aa(k1-1+1)
      DO 42400 N=1,NM

! APPLY  THE TOTAL CORRECTION OF THE 2 WAY RANGE HERE

!      write(6,*)'robs: uncorr 2 way range '
!      write(6,*)'robs: n,iloop, rl1(n,iloop)  ',&
!                       n,iloop, rl1(n,iloop)

      IF(ILOOP.EQ.1)THEN

          CSCALE=-1.D0
          IF(LITER1) THEN
              savec1(n)=aa(ksmcor+n-1)
          ENDIF

!          write(6,*)'robs: n, cscale, savec1(n)  ',n, cscale,savec1(n)

          RL1(N,ILOOP)=RL1(N,ILOOP)+CSCALE*savec1(n)

      ELSE

          CSCALE=1.D0
          IF(LITER1) THEN
              savec2(n)=aa(ksmcor+n-1)
          ENDIF
          !write(6,*)' dbg corr2 ',cscale,savec2(n),aa(ksmcor+n-1)
          RL1(N,ILOOP)=RL1(N,ILOOP)+CSCALE*savec2(n)

      ENDIF
!     csr= cscale*aa(KSMCOR+n-1)

!     write(6,*)'robs: corr 2-way r '
!     write(6,*)'robs: rl1(n,iloop),csr ', rl1(n,iloop),csr

!  COMPUTE T1S, t1e

      RATIO(N,ILOOP)=RL1(N,ILOOP)/VLIGHT
      T1SE(N,K2)=AA(K1-1+N)-RATIO(N,ILOOP)

!     if(k2.eq.1)then
!     write(6,*)'t1s t3s rhos rhos/c obs# ',t1se(n,k2),aa(k1-1+n),
!    .rl1(n,iloop),ratio(n,iloop),n
!     else
!     write(6,*)'t1e t3e rhoe rhoe/c obs# ',t1se(n,k2),aa(k1-1+n),
!    .rl1(n,iloop),ratio(n,iloop),n
!     endif

! TIME START TRANSIMIT

      IF(ILOOP.EQ.1)THEN
      ITIME1=MJDSBL+INT(T1SE(N,ILOOP))
      ITIM1E=MJDSBL+INT(T1SE(N,ILOOP))+INT(AA(KDTIME+N-1))
      DO 43500 IN=1,KKRAMP
      FSECIN=0.0
      MJDV=ITMRA1(1)
      CALL UTCET(.TRUE.,1,MJDV,FSECIN,FSECOT,AA(KA1UT))
      IFS=INT(FSECOT)
      ITMA1=ITMRA1(IN)+IFS
      ITMA2=ITMRA2(IN)+IFS
      IF(ITIME1.GE.ITMA1.AND.ITIME1.LE.ITMA2) THEN
      IF(ITIM1E.GE.ITMA1.AND.ITIM1E.LE.ITMA2) THEN
      LRDEDT(N)=.FALSE.
      IND1(N)=IN
!     write(6,*)' dbg 1 ',N,IN,ITMA1,ITIME1,ITIM1E,ITMA2
      ELSE
      LRDEDT(N)=.TRUE.
      IND1(N)=IN
!     write(6,*)' edt 1 ',N,IN,ITMA1,ITIME1,ITIM1E,ITMA2
      ENDIF
      ENDIF
!     WRITE(6,*)' DBG IND1 ',IND1(N),ITMRA1(IND1(N)),ITIME1,ITIM1E,
!    .ITMRA2(IND1(N))

43500 END DO

! t1s-tr=t3s-rs/c-tR
! Below -58 and -0.184 is converting ramp times from UTC to ET

      FSECIN=0.0
      MJDV=ITMRA1(IND1(N))
      CALL UTCET(.TRUE.,1,MJDV,FSECIN,FSECOT,AA(KA1UT))
      IFS=INT(FSECOT)
      DIF=FSECOT-DBLE(IFS)
      IPART=MJDSBL-ITMRA1(IND1(N))-IFS
      T1SMTR(N)=DBLE(IPART)+AA(K1-1+N)-DIF-RATIO(N,1)
      TEMP=FRRATE(IND1(N))*T1SMTR(N)
      FREQ1(N)=FRSTRT(IND1(N))
      FQT1S(N)=FRSTRT(IND1(N))+TEMP
      SAVED1(N)=TEMP
!     FQT1S(N)=FRSTRT(IND1(N))+FRRATE(IND1(N))*T1SMTR(N)
!     write(6,*)'dbg',fqt1s(n),frstrt(ind1(n)),frrate(ind1(n)),ind1(n)
      FTT1SE(N,ILOOP)=RINDM1*FQT1S(N)+RINDD
!     write(6,*)' dbg ftt1s ',ftt1se(n,iloop),n,iloop,fqt1s(n)

      ENDIF ! ?? ILINK.EQ.IEXIT

      IF(ILOOP.NE.1) THEN
!     WRITE(6,*)' DBG T1S T1E',T1SE(N,1),T1SE(N,2)
      TCP(N)=AA(KDTIME+N-1)-(RL1(N,ILOOP)-RL1(N,1))/VLIGHT
!     WRITE(6,*)' DBG TC prime,TC,obs#  ',TCP(N),aa(kdtime+n-1),n
! time start ground received
      ITIM3=MJDSBL-INT(AA(KDTIME+N-1))+INT(AA(K1-1+N))
      ITIM3E=MJDSBL+INT(AA(K1-1+N))
!     write(6,*)' dbg time3 ',mjdsbl,INT(AA(KDTIME+N-1)),INT(AA(K1-1+N))
!     WRITE(6,*)' DBG T start gr rec T start trans',ITIM3,ITIME1,n
! At this point we should convert ITMRA1,2 to ET times
      DO 43600 IN=1,KKRAMP
      FSECIN=0.0
      MJDV=ITMRA1(1)
      CALL UTCET(.TRUE.,1,MJDV,FSECIN,FSECOT,AA(KA1UT))
      IFS=INT(FSECOT)
      ITMA1=ITMRA1(IN)+IFS
      ITMA2=ITMRA2(IN)+IFS
      IF(ITIM3.GE.ITMA1.AND.ITIM3.LE.ITMA2) THEN
      IF(ITIM3E.GE.ITMA1.AND.ITIM3E.LE.ITMA2) THEN
!     LRDEDT(N)=.FALSE.
      IND3=IN
!     write(6,*)' dbg 2 ',N,IN,ITMA1,ITIM3,ITIM3E,ITMA2
!     write(6,*)' dbg ',itma1,itim3,itim3e,itma2,n,ind3
      ELSE
      LRDEDT(N)=.TRUE.
      IND3=IN
!     write(6,*)' edt 2 ',N,IN,ITMA1,ITIM3,ITIM3E,ITMA2
      ENDIF
      ENDIF
43600 END DO
      IF(LRDEDT(N)) THEN
!     write(6,*)' dbg ',itma1,itim3,itim3e,itma2,n,ind3,lrdedt(n)
      ENDIF
!     WRITE(6,*)' ramp beg int ',IND1(N),ITMRA1(IND1(N)),ITIME1,ITIM1E,
!    .ITMRA2(IND1(N))
!     WRITE(6,*)' ramp end int ',IND3,ITMRA1(IND3),ITIM3,ITIM3E,
!    .ITMRA2(IND3)
!     WRITE(6,*)' ramp time beg int ',IND1(N),ITMRA1(IND1(N))
!     WRITE(6,*)' ramp time end int ',IND3,ITMRA1(IND3)
! TC=AA(KDTIME+N-1)
! TCP has been computed
! t1s=t1se(n,2)
! t3s=MJDSBL=AA(KDTIME+N-1)
! t3s-tR=ground received time at the beg of the counting int-tR
! Below -58 and -0.184 is converting ramp times from UTC to ET

      IF(RININD.EQ.0) THEN
      FSECIN=0.0
      MJDV=ITMRA1(IND1(N))
      CALL UTCET(.TRUE.,1,MJDV,FSECIN,FSECOT,AA(KA1UT))
      IFS=INT(FSECOT)
      DIF=FSECOT-DBLE(IFS)
      IPART=MJDSBL-ITMRA1(IND3)-IFS
      T3SMTR=DBLE(IPART)+AA(K1-1+N)-DIF-AA(KDTIME+N-1)
! Evaluate functions
      TEMP=FRRATE(IND3)*T3SMTR
      FQT3S=FRSTRT(IND3)+TEMP
      FREQ3(N)=FRSTRT(IND3)
      SAVED2(N)=TEMP
!     write(6,*)' dbg fqt3s ',fqt3s,frrate(ind3),aa(KDTIME+N-1)
      TEMP=HALF*FRRATE(IND3)*AA(KDTIME+N-1)
      FQT3AV=FQT3S+TEMP
      SAVED2(N)=SAVED2(N)+ TEMP
      ELSE
      FQT3AV=AVFREQ
      FREQ3(N)=FQT3AV
      ENDIF
      FTT3AV=RINDM1*FQT3AV+RINDD
      TEMP=HALF*FRRATE(IND1(N))*TCP(N)
      SAVED1(N)=SAVED1(N)+TEMP
      FQT1AV=FQT1S(N)+TEMP
!     write(6,*)' dbg FTT1AV,FTT3AV,TCP ',FTT1AV,FTT3AV,TCP
!     write(6,*)' dbg fqt1s frr tcp ',fqt1s(n),frrate(ind1(n)),ind1(n)
!     write(6,*)' dbg v ',fvt1av,fqt1av
      FQT1E(N)=FQT1AV+FRRATE(IND1(N))*HALF*AA(KDTIME+N-1)
      FTT1SE(N,ILOOP)=RINDM1*FQT1E(N)+RINDD
!     write(6,*)' dbg ftt1e ',ftt1se(n,iloop),n,iloop,fqt1e(n)
      FTT1AV=RINDM1*FQT1AV+RINDD
      FT1AV(N)=FTT1AV
!     WRITE(6,*)' DBG FQT1S  FQT1E ',FQT1S(N),FQT1E(N),N
!     WRITE(6,*)' DBG FQT3AV FQT1AV',FQT3AV,FQT1AV
!     AA(KOBSC-1+N)=RINDM2*(FTT3AV*AA(KDTIME+N-1)
!    .             -FTT1AV*TCP(N))/AA(KDTIME+N-1)
!     WRITE(6,*)' DBG OBSERVATION ',AA(KOBSC-1+N),n
      DFQ=FREQ3(N)-FREQ1(N)
      IF(RININD.EQ.0.D0) THEN
      DIFFS= SAVED2(N)-SAVED1(N)
      ELSE
      DIFFS= -SAVED1(N)
      ENDIF
      DFQP(N)=RINDM1*(DFQ+DIFFS)

      ENDIF

42400 END DO
! SUBSTITUTE PARTIALS
      IF(ILINK.EQ.IEXIT) THEN
      DO 42800 N1=1,NM3
      DO 42800 N=1,NM
      PSCALE=RINDM2/(VLIGHT*AA(KDTIME+N-1))
!     ccc=ftt1av*rindm2*2.d0/vlight
!     pp=pscale*ftt1se(n,iloop)
!     ppp=pp*aa(kdtime+n-1)
!     write(6,*)' dbg con ',ccc,pp,pscale,aa(kdtime+n-1),ppp
      AA(IPMPX+MADDC+N1-1)=PRL1(N1,ILINK)*FTT1SE(N,ILOOP) *PSCALE
      AA(IPMPX+N1-1)=PRL2(N1,ILINK)*FTT1SE(N,ILOOP) *PSCALE
!     WRITE(6,*)' DBG NEW PART ',AA(IPMPX+MADDC+N-1),PRL1(n,ilink),
!    .n,pscale,ftt1se(n,iloop),pp,ppp
!     WRITE(6,*)' DBG PSCALE,PRL1(N,ILINK) ',PSCALE,PRL1(N,ILINK),N
42800 CONTINUE
!     WRITE(6,*)' DBG NEW PART ',AA(IPMPX+MADDC),PRL1(1,2),
!    .prl1(1,1),pscale
      ENDIF
! RAMP
      ENDIF

      ENDIF ! ??     INDRAM.GT.0


      IF(INDRAM.GT.0.AND.ILINK.EQ.1)THEN
      DO 91966 N=1,NM3
      AA(IPMPX+MADDC+N-1)=0.D0
91966 END DO
      ENDIF

!     WRITE(6,*) 'ROBS: AFTER ROBSUM AA(JELEVS) ',AA(JELEVS)

!      DO 91967 N=1,NM3
!     write(6,*)'contr part ',(AA(IPMPX+MADDC+N-1)),n,ilink,iloop,lneg
!91967 END DO

      !write(6,*)'robs:AA(IPMPX+MADDC),n,ilink,iloop,lneg ', &
      !                AA(IPMPX+MADDC),n,ilink,iloop,lneg

      IF(LNADJ) GO TO 80000

      IF(ISATF.LE.0) GO TO 45000

      ISAT=INDSAT(ISATF)
      ISET=INDSET(ISATF)
      IPXPF=KPXPF(ISATF)
      NDIMX1=NEQN(ISET)
      NDIMX2=N3(ISET)
      NDIMX3=NM
      NDIMA1=NEQN(1)
      NDIMA2=N3(1)
      NDIMA3=NM

      IF(LNPNM) GO TO 43000

      NDIMX1=NM
      NDIMX2=NEQN(ISET)
      NDIMX3=N3(ISET)
      NDIMA1=NM
      NDIMA2=NEQN(1)
      NDIMA3=N3(1)

43000 CONTINUE

! CHAIN MEASUREMENT POSITION PARTIALS WITH FORCE MODEL PARTIALS

      CALL CHAINR(AA(IPMPX+MADDC),NM,3,1,AA(IPXPF),NDIMX1,NDIMX2,NDIMX3,&
     &   NEQN(ISET),N3(ISET),IPTFMG(1,ISAT),ILNFMG(1,ISAT),NFMG(ISAT),  &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LADD,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(LASTSN) THEN
      CALL CHAINR(AA(IPMPX+MADDC),NM,3,1,AA(KASTP),NDIMA1,NDIMA2,NDIMA3,&
     &   NEQN(1),N3(1),IPTFMG(1,1),ILNFMG(1,1),NFMG(1),                 &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LADD,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(LRANGE) GO TO 45000

! CHAIN MEASUREMENT VELOCITY PARTIALS WITH FORCE MODEL PARTIALS

      CALL CHAINR(AA(IUR+MADDC),NM,3,2,AA(IPXPF),NDIMX1,NDIMX2,NDIMX3,  &
     &   NEQN(ISET),N3(ISET),IPTFMG(1,ISAT),ILNFMG(1,ISAT),NFMG(ISAT),  &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LADD,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(LASTSN) THEN
      CALL CHAINR(AA(IUR+MADDC),NM,3,2,AA(KASTP),NDIMA1,NDIMA2,NDIMA3,  &
     &   NEQN(1),N3(1),IPTFMG(1,1),ILNFMG(1,1),NFMG(1),                 &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LADD,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

45000 CONTINUE
      IF(ISATT.LE.0) GO TO 50000
      ISAT=INDSAT(ISATT)
      ISET=INDSET(ISATT)
      IPXPF=KPXPF(ISATT)
      NDIMX1=NEQN(ISET)
      NDIMX2=N3(ISET)
      NDIMX3=NM
      NDIMA1=NEQN(1)
      NDIMA2=N3(1)
      NDIMA3=NM
      IF(LNPNM) GO TO 48000
      NDIMX1=NM
      NDIMX2=NEQN(ISET)
      NDIMX3=N3(ISET)
      NDIMA1=NM
      NDIMA2=NEQN(1)
      NDIMA3=N3(1)
48000 CONTINUE

! CHAIN MEASUREMENT POSITION PARTIALS WITH FORCE MODEL PARTIALS

      CALL CHAINR(AA(IPMPX+MADDC),NM,3,1,AA(IPXPF),NDIMX1,NDIMX2,NDIMX3,&
     &   NEQN(ISET),N3(ISET),IPTFMG(1,ISAT),ILNFMG(1,ISAT),NFMG(ISAT),  &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LNEG,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(LASTSN) THEN
      CALL CHAINR(AA(IPMPX+MADDC),NM,3,1,AA(KASTP),NDIMA1,NDIMA2,NDIMA3,&
     &   NEQN(1),N3(1),IPTFMG(1,1),ILNFMG(1,1),NFMG(1),                 &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LNEG,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      IF(LRANGE) GO TO 50000

! CHAIN MEASUREMENT VELOCITY PARTIALS WITH FORCE MODEL PARTIALS

      CALL CHAINR(AA(IUR+MADDC),NM,3,2,AA(IPXPF),NDIMX1,NDIMX2,NDIMX3,  &
     &   NEQN(ISET),N3(ISET),IPTFMG(1,ISAT),ILNFMG(1,ISAT),NFMG(ISAT),  &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LNEG,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(LASTSN) THEN
      CALL CHAINR(AA(IUR+MADDC),NM,3,2,AA(KASTP),NDIMX1,NDIMX2,NDIMX3,  &
     &   NEQN(1),N3(1),IPTFMG(1,1),ILNFMG(1,1),NFMG(1),                 &
     &   AA(KPMPA),NDIM1,NDIM2,LNPNM,LNEG,LSCALE,RSCALX,SCALX,          &
     &   LL(KLAVOI))
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

50000 CONTINUE

!      IF(.NOT.LSTINR) GO TO 80000
!      IF(ISTAF.LE.0) GO TO 55000
!
! CHAIN AND SUM STATION POSITION AND POLAR MOTION PARTIALS
!
      INTPOL=NINTPL(ISTAF )
      INMP  =(ISTAF-1)*MINTPL
      IINDP =KINDP +INMP
      INMP  =KNMP  +INMP
      IXM   =JXM   (ISTAF )
      NPXS  =(ISTAF-1)*NM*6
      NPXSX =(ISTAF-1)*NM*3*NSETDP
      IPXSXP=KPXSXP+NPXS
      IPXSXD=KPXDXP+NPXS
      IPXSLV=KPXSLV+NPXS
      IPXSLV=KPXSLV+NPXSX
      IPXSLV=KPXSLV+NPXSX
      IXUTDT=KXUTDT+NM
      JPXSPA=KPXSPA+(ISTAF-1)*NM*18*MXOLSA
      JPXSP1=KPXSPA+(ISTAF-1)*NM*18*MXOLSA+NM*6*MXOLSA
      JPXSP2=KPXSPA+(ISTAF-1)*NM*18*MXOLSA+NM*12*MXOLSA
      ISTA  =INDSTA(ISTAF )
      LESTA =ISTA.LE.NSTAE
      IF(ISTA.EQ.0) LESTA=.FALSE.

!      WRITE(6,*) 'ROBS: NPLTAR = ', NPLTAR
!      WRITE(6,*) 'ROBS: NM = ', NM

      if( npltar .gt. 0 ) then           ! jjm 9/98
       IF(ILINK.EQ.1) THEN
          ICTARG=KCOSTH+2*NM
          ISTARG=KSINTH+2*NM
          DO NN=1,NM
             DO J=1,3
                UMV(NN,J)=UM1(NN,J)
             ENDDO
          ENDDO
       ENDIF
       IF(ILINK.EQ.2) THEN
          ICTARG=KCOSTH+3*NM
          ISTARG=KSINTH+3*NM
          DO NN=1,NM
             DO J=1,3
                UMV(NN,J)=UM2(NN,J)
             ENDDO
          ENDDO
       ENDIF
      endif ! ( npltar .gt. 0 )             ! jjm 9/98

!      IF(ILINK.EQ.1) THEN
!      ICTARG=KCOSTH+2*NM
!      ISTARG=KSINTH+2*NM
!      DO 71722 NN=1,NM
!      DO 71722 J=1,3
!      UMV(NN,J)=UM1(NN,J)
!1722  CONTINUE
!      ENDIF
!      IF(ILINK.EQ.2) THEN
!      ICTARG=KCOSTH+3*NM
!      ISTARG=KSINTH+3*NM
!      DO 81722 NN=1,NM
!      DO 81722 J=1,3
!      UMV(NN,J)=UM2(NN,J)
!1722  CONTINUE
!      ENDIF


      !------------------------------------------------------------------

      !write(6,'(A,1x,I10, 4x, L1)') &
      !      'robs: ISATID, LPHNTM(3) ', ISATID, LPHNTM(3)

!     if( LPHNTM(3) )then

          !  add cg partials to pmpa here

          !write(6,'(A)') 'robs: call store_cg_fan_pars '
          !write(6,'(A,1x,I3,3x,L1)') 'robs: JSEQ, LNEG ', JSEQ, LNEG
          !write(6,'(A,1x,I2)') 'robs: cg_par_array ILINK =  ', ILINK
          !write(6,'(6(1x,E15.7))') cg_par_array(1:ndim_cgmass,1)

          !write(6,'(/A)' ) 'robs:bef  AA( KPMPA )'
          !do  i = 1, NADJST  ! 15
          !        write(6,'(I8,1x,E15.7)') i, AA( KPMPA -1 + i )
          !enddo

!         call store_cg_fan_pars(  LNPNM, NM, AA(KPMPA), &
!                                  NDIM1 ,NDIM2, LNEG, &
!                                  LL(KLAVOI), cg_par_array    )

          !write(6,'(/A)' ) 'robs:aft  AA( KPMPA )'
          !do  i = 1, NADJST  ! 15
          !        write(6,'(I8,1x,E15.7)') i, AA( KPMPA -1 + i )
          !enddo

!     endif ! LPHNTM(3)

      !------------------------------------------------------------------


      !write(6,'(A)') 'robs:1  call psum '
      !write(6,'(A,1x,I2)') 'robs:1  ILINK ', ILINK
      !write(6,'(A,1x,I3,3x,L1)') 'robs:1 JSEQ, LNEG ', JSEQ, LNEG

      ! OFFADJ
      ! aa(kofdrv) is the range unit vector used by OFFADJ

      CALL PSUM(II(KICON ),II(KICNL2),II(KICNH2),                       &
     & AA(KDXSDP),AA(ICOSTH),AA(ISINTH),                                &
     & AA(IPMPX ),AA(IXM   ),AA(IPXSXP),AA(IPXSLV),II(INMP  ),          &
     & II(IINDP ),NM,MTYPE  ,INTPOL    ,AA(KPMPXE),AA(KPMPA ),          &
     & NDIM1     ,NDIM2     ,ISTA      ,LESTA     ,LPOLAD    ,          &
     & ISTAF     ,AA(JPXSPA),LNPNM     ,LNEG      ,RSCALX    ,          &
     & MJDSBL,AA(IFSEC),LL(KLVELA) ,AA(KTIMVL),LL(KLSTL2),LL(KLSTH2),   &
     & AA(KPRCBD),AA(KPRTBD),                                           &
     & AA(IPMPX+MADDC),AA(IPMPX+MADDC2),AA(JPXSP1),AA(JPXSP2),II(KICNV),&
     & XMM,KTARNO,PARTRG,AA(ICTARG),AA(ISTARG),UMV,AA(KPPER),AA(KANGWT),&
     & AA(KWT),LL(KLAVOI),AA(KOFDRV),SCALX,.TRUE.,AA(IPXSXD),AA(IXUTDT),&
     & II(KINDPI),AA(KDXDNU),LNUTAD)
55000 CONTINUE

      !write(6,*) 'robs:1 ILINK, ISTAT ', ILINK, ISTAT

      IF(ISTAT.LE.0) GO TO 80000
!
! CHAIN AND SUM STATION POSITION AND POLAR MOTION PARTIALS
!
      INTPOL=NINTPL(ISTAT )
      INMP  =(ISTAT-1)*MINTPL
      IINDP =KINDP +INMP
      INMP  =KNMP  +INMP
      IXM   =JXM   (ISTAT )
      NPXS  =(ISTAT-1)*NM*6
      IPXSXP=KPXSXP+NPXS
      IPXSXD=KPXDXP+NPXS
      IPXSLV=KPXSLV+NPXS
      IXUTDT=KXUTDT+NM
      JPXSPA=KPXSPA+(ISTAT-1)*NM*18*MXOLSA
      JPXSP1=KPXSPA+(ISTAF-1)*NM*18*MXOLSA+NM*6*MXOLSA
      JPXSP2=KPXSPA+(ISTAF-1)*NM*18*MXOLSA+NM*12*MXOLSA
      ISTA  =INDSTA(ISTAT )
      LESTA =ISTA.LE.NSTAE
      IF(ISTA.EQ.0) LESTA=.FALSE.
      ICTARG=KCOSTH+3*NM
      ISTARG=KSINTH+3*NM

      !write(6,'(A)')       'robs:2  call PSUM '
      !write(6,'(A,1x,I3)') 'robs:2  ILINK ', ILINK
      !write(6,'(A,1x,I3,3x,L1)') 'robs:2 JSEQ, LNEG ', JSEQ, LNEG

      CALL PSUM(II(KICON ),II(KICNL2),II(KICNH2),                       &
     & AA(KDXSDP),AA(ICOSTH),AA(ISINTH),                                &
     & AA(IPMPX ),AA(IXM   ),AA(IPXSXP),AA(IPXSLV),II(INMP  ),          &
     & II(IINDP ),NM,MTYPE  ,INTPOL    ,AA(KPMPXE),AA(KPMPA ),          &
     & NDIM1     ,NDIM2     ,ISTA      ,LESTA     ,LPOLAD    ,          &
     & ISTAT     ,AA(JPXSPA),LNPNM     ,LADD      ,RSCALX    ,          &
     & MJDSBL,AA(IFSEC),LL(KLVELA) ,AA(KTIMVL),LL(KLSTL2),LL(KLSTH2),   &
     & AA(KPRCBD),AA(KPRTBD),                                           &
     & AA(IPMPX+MADDC),AA(IPMPX+MADDC2),AA(JPXSP1),AA(JPXSP2),II(KICNV),&
     & XMM,KTARNO,PARTRG,AA(ICTARG),AA(ISTARG),UMV,AA(KPPER),AA(KANGWT),&
     & AA(KWT),LL(KLAVOI),AA(KOFDRV),SCALX,.FALSE.,AA(IPXSXD),          &
     & AA(IXUTDT),II(KINDPI),AA(KDXDNU),LNUTAD)
80000 CONTINUE


90000 END DO


      IF(INDRAM.GT.0) THEN
      DO 91000 N=1,NM3
      DO 91000 M=1,2
      PRL1(N,M)=0.D0
      PRL2(N,M)=0.D0
91000 CONTINUE
      ENDIF

      INDRAM=INDRAH

      IF(LBPCLK) THEN
         DO IQP=1,NM
          AA(JFSECN-1+IQP)=FSECX(IQP)
         ENDDO
      ENDIF
      RETURN
60000 FORMAT(' EXECUTION TERMINATING IN ROBS NM, MOBS',2I10)
      END
