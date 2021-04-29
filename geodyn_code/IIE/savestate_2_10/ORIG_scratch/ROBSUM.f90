





!$ROBSUM
      SUBROUTINE ROBSUM(XSM  ,VSM   ,RNM   ,RRNM  ,URNM  ,COSTHG,      &
         SINTHG,ENV   ,ELEVSC,SATDLY,SATDL2,STADLY,STADL2,FSECSA,      &
         XYZCG1,XYZCG2,XY2CG1,XY2CG2,XYZOFF,XY2OFF,WORK  ,U     ,      &
         RA    ,UHAT  ,RELV  ,OCANT ,OCDRYT,OCWETT,OCION ,OCIONS,      &
         OCDLYG,OCDLG2,OCDLYS,OCDLS2,OCTPC ,OCTP2 ,OBSC  ,OBSTRL,      &
         OBRATE,NM    ,ITYPE ,MTYPE ,LNELEV,LPRE  ,LPRESC,BLKMET,      &
         OBSMET,RSCALE,LNEG  ,L2SATL,XSM2  ,VSM2  ,FSECS2,JLINK ,      &
         GRELLT,DELCET,LPTS  ,AA    ,II    ,ICOOR1,ICOOR2,RLRCOR,      &
         INDSTA,TPMES1,LTPMS1,TPMES2,LTPMS2,XYOF21,XYOF22,ISET1 ,      &
         ISET2, URNM2, ILINK ,ILCORR,KLBIA ,LL    ,ISATID,NSTA0 ,      &
         MJDSIN,FSECIN,RL1,FRQLNK,SCRTCH,ATROT,ANTTAB,HOLD,SCALE,      &
         ISEQ,MJDSBL,W1PU,W2PU,XYOF31,XYOF32, cg_par_array )
!********1*********2*********3*********4*********5*********6*********7**
! ROBSUM           00/00/00            0000.0    PGMR - T. MARTIN
!
! FUNCTION: ADDS RANGES AND CORRECTIONS TO SUMS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I    A    TRUE OF REF INERT. COORD. OF FIRST SAT IN LINK
!   VSM      I    A    TRUE OF REF INERT. VEL.   OF FIRST SAT IN LINK
!   RNM      I    A    RANGE VALUES FOR THIS LINK OF THE MEASUREMENT
!   RRNM     I    A    RANGE RATE   FOR THIS LINK OF THE MEASUREMENT
!   URNM     I    A    UNIT VECTOR FROM Tn TO Sm
!                      1st PART IS TRUE OF DATE EARTH EQUAT. EQUINOX
!   URNM2              2nd PART IS TRUE OF REFERENCE INTEGRATION SYSTEM
!   COSTHG   I    A    COSINES OF RIGHT ASCENSION OF GREENWICH
!   SINTHG   I    A    SINES   OF RIGHT ASCENSION OF GREENWICH
!   ENV      I    A    EAST,NORTH,VERTICAL UNIT VECTORS
!   ELEVSC  I/O   A    ELEVATION OF S/C WRT STATION
!   SATDLY   I    A    TRANSPONDER DELAY CURVE INFO. FOR FIRST  SAT
!   SATDL2   I    A    TRANSPONDER DELAY CURVE INFO. FOR SECOND SAT
!   STADLY   I    A    TRANSPONDER DELAY CURVE INFO. FOR FIRST  STA
!   STADL2   I    A    TRANSPONDER DELAY CURVE INFO. FOR SECOND STA
!   FSECSA   I    A
!   XYZCG1   I    A    XYZ FOR CNTR OF MASS COR. FOR SAT 1 AT TIME 1
!   XYZCG2   I    A    XYZ FOR CNTR OF MASS COR. FOR SAT 1 AT TIME 2
!                      XYZCG1 & XYZCG2 HAVE SAME VALUES
!   XY2CG1   I    A    XYZ FOR CNTR OF MASS COR. FOR SAT 2 AT TIME 1
!   XY2CG2   I    A    XYZ FOR CNTR OF MASS COR. FOR SAT 2 AT TIME 2
!                      XY2CG1 & XY2CG2 HAVE SAME VALUES
!   XYZOFF   I    A    XYZ FOR OFFSET CORRECTION FOR SAT 1
!   XY2OFF   I    A    XYZ FOR OFFSET CORRECTION FOR SAT 2
!   WORK     I    A    SCRATCH WORKING ARRAY
!   U
!   RA
!   UHAT     I    A    E.C.F UNIT VECTOR FROM STATION TO S/C
!   RELV     I    A    VELOCITY OF SATELLITE W.R.T. TRACKING STATION
!   OCANT   I/O   A    OBSERVATION CORRECTION FOR ANTENNAE OFFSET
!   OCDRYT  I/O   A    OBSERVATION CORRECTION FOR DRY TROPOSPHERIC TERM
!   OCWETT  I/O   A    OBSERVATION CORRECTION FOR WET TROPOSPHERIC TERM
!   OCION   I/O   A    OBSERVATION CORRECTION FOR IONOSPHERE(STA/SAT)
!   OCIONS  I/O   A    OBSERVATION CORRECTION FOR IONOSPHERE(SAT/SAT)
!   OCDLYG  I/O   A    OBSERVATION CORRECTION FOR FIRST  STA DELAY
!   OCDLG2  I/O   A    OBSERVATION CORRECTION FOR SECOND STA DELAY
!   OCDLYS  I/O   A    OBSERVATION CORRECTION FOR FIRST  SAT DELAY
!   OCDLS2  I/O   A    OBSERVATION CORRECTION FOR SECOND SAT DELAY
!   OCTPC   I/O   A    OBSERVATION CORRECTION FOR FIRST  SAT OFFSET
!   OCTP2   I/O   A    OBSERVATION CORRECTION FOR SECOND SAT OFFSET
!   OBSC    I/O   A    ARRAY OF COMPUTED OBSERVATIONS
!   OBSTRL  I/O   A    ARRAY OF COMPUTED TDRSS LONG LINK AVG. RANGE RATE
!   OBRATE  I/O   A    ARRAY OF OBSERVATION RATES
!   NM       I    S    NUMBER OF MEASUREMENTS IN THIS BLOCK
!   ITYPE    I    S    METRIC MEASUREMENT TYPE INDICATOR. ITYPE=(MEASURE
!                      TYPE - START OF METRIC MEASUREMENT TYPES+2)/2
!                      EG. MTYPES 57,58 = ITYPE 11
!   MTYPE    I    S    MEASUREMENT TYPE
!   LNELEV   I    S    FLAG SET TO .TRUE. WHEN ELEVATION IS NOT TO BE
!                      COMPUTED
!   LPRE     I    A    PREPROCESSING FLAGS FOR THIS STATION
!   LPRESC   I    A    PREPROCESSING FLAGS FOR THIS SATELLITE
!   BLKMET   I    S    METEOROLOGICAL DATA FROM THE BLOCK HEADER RECORDS
!   OBSMET  I/O   A    METEOROLOGICAL DATA FOR EACH OBS. IN THE BLOCK
!   RSCALE   I    S    DOPPLER SCALE FACTOR FOR SATELLITE-TO-SATELLITE
!                      MEASUREMENTS
!   LNEG     I    S    LOGICAL FLAG FOR ADDING OR SUBTRACTING PARTIALS
!   L2SATL   I    S    FLAG SET TO .TRUE. WHEN A LINK CONTAINS TWO SATS
!   XSM2     I    A    TRUE OF REF INERT. COORD. OF SECOND SAT IN LINK
!   VSM2     I    A    TRUE OF REF INERT. VEL. OF SECOND SAT IN LINK
!   FSECS2   I    A
!   JLINK    I    S    ODD ON DOWNLINK EVEN ON UPLINK
!   GRELLT   I    A    GENERAL RELATIVISTIC LIGHT TIME CORRECTION
!   DELCET   I    A    DIFFERENCE BETWEEN ET AND CET AT GROUND RECEIVE
!                      TIME
!   LPTS     I    A    LOGICAL FLAGS FOR ACCEPTED SIMULATED OBSERVATION
!                      IN A PSEUDO BLOCK
!   AA      I/O   A    DYNAMIC ALLOCATION FOR REAL    VARIABLES
!   II      I/O   A    DYNAMIC ALLOCATION FOR INTEGER VARIABLES
!   ICOOR1   I    S    FLAG FOR COORDINATE SYSTEM INDICATOR FOR OFFSET S
!   ICOOR2   I    S    FLAG FOR COORDINATE SYSTEM INDICATOR FOR OFFSET S
!   RLRCOR   I    A    RANGE CORRECTION DUE TO UNMODELED RELATIVISTIC
!                      PERIODIC AND LINEAR SPACE CRAFT CLOCK DRIFT
!   TPMES1   I    A    TOPEX ATT. BACK VALUES FOR ITO   SATELLITE (REAL)
!   TPMES2   I    A    TOPEX ATT. BACK VALUES FOR IFROM SATELLITE (REAL)
!   LTPMS1   I    A    TOPEX ATT. BACK VALUES FOR ITO   SATELLITE (LOG.)
!   LTPMS2   I    A    TOPEX ATT. BACK VALUES FOR IFROM SATELLITE (LOG.)
!   XYOF21   I    A    XYZ FOR SECOND LINK OFFSET CORRECTION FOR SAT 1
!   XYOF22   I    A    XYZ FOR SECOND LINK OFFSET CORRECTION FOR SAT 2
!   XYOF31   I    A    XYZ FOR THIRD  LINK OFFSET CORRECTION FOR SAT 1
!   XYOF32   I    A    XYZ FOR THIRD  LINK OFFSET CORRECTION FOR SAT 2
!   ISET1    I    S    SET NUMBER RELATED TO SAT 1
!   ISET2    I    S    SET NUMBER RELATED TO SAT 2
!   ILINK    I    S    NUMBER OF THIS LINK
!   ILCORR
!   KLBIA
!   LL      I/O   A    DYNAMIC ALLOCATION FOR LOGICAL VARIABLES
!   ISATID   I    S    satellite id
!   NSTA0    I    S    station   id
!   MJDSIN
!   FSECIN
!   RL1
!   FRQLNK
!   SCRTCH
!   ATROT
!   ANTTAB
!   HOLD
!   SCALE
!   ISEQ
!   MJDSBL
!   W1PU
!   W2PU
!   XYOF31
!   XYOF32
!   cg_par_array I/O A  array for cgmass partials
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      use cgmass_module
      use antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO = 0.0D0 )
      PARAMETER ( ONE  = 1.0D0 )
!
      COMMON/LDA/LDORANT
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/CALTOP/LADYNO,LAGEOO,LRGEOT,LRETDT,LROTDT,LRSSTT,LCOTRM,   &
     &              LFILOT,LATCOR,LFIRIT,LX1ALT,LX2ALT,LMSSWG,LATMOD,   &
     &              LG1BOT,LG1B,LONEG1,LXATTD,LXBEAM,NXALTO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CEGREL/LGRELE,LRLCLK,LGRELR,LXPCLK,LBPCLK,NXEGRL
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CMET/P0,T0,RELHUM,E0,WAVLEN
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
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
      COMMON/COFFST/JXYZOF(2),JSADLY(2),JSTDLY(2),JXYZCG(2,2),          &
     &              MJDFCG(2,2),JXYOF2(2),JEXTOF(2),JXYOF3(2)
      COMMON/COFSTL/LOFEXT(2)
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
      COMMON/CORL01/KLORBT,KLORBV,KLNDRG,KLBAKW,KLSETS,KLAFRC,KLSTSN,   &
     &              KLSNLT,KLAJDP,KLAJSP,KLAJGP,KLTPAT,KEALQT,KLRDGA,   &
     &              KLRDDR,KLRDSR,KFHIRT,KLSURF,KHRFON,KLTPXH,KSETDN,   &
     &              KTALTA,NXCL01
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/EXATGI/MEASAT,MEAFIL,MEAMEM,MEAPLA,MEAANT,NXEAGI

      COMMON/FANTOM/LFGLB,LFARC,LFTARC,LFTGLB,LENDGL,LPHNTM(4),LPHTIM(4)&
     &             ,LXTRAF,LSATFA(4,3),NXFANT
!
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
!                     (3)- ARC GEOMETRIC
!                     (4)- ARC FORCE
!            LPHTIM - SWITCHES FOR FANTOM EPOCH OR APPLICATION START TIM
!                     (1)- GLOBAL GEOMETRIC
!                     (2)- GLOBAL FORCE
!                     (3)- ARC GEOMETRIC
!                     (4)- ARC FORCE
!            LXTRAF - TRUE IF ONE NEEDS TO ALLOCATE FOR EXTRA REAL INFOR
!                     USING COLS 60-72 and 73-80 ON THE FANTOM CARD
!            LSATFA - LOGICAL FOR  UNADJUSTED/ADJUSTED FANTOM ARC FORCE
!                     PARAMETERS
!            NXFANT - NUMBER OF WORDS IN THIS COMMON BLOCK
!


      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
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
      COMMON/IONO2E/IONNDX(12),IONRZ(12),IONOYM(12),IONYMS,IONYME,IURSI,&
     &              INTERP,NXIONO
      COMMON/IRAMPS/INDRAM,KKRAMP
      COMMON/NLRA/NLRARC,NLRAIN,NLRAD1,NLRAD2,NLRAPT,NLRAST,NSARAL, &
     &            NXNLRA
      COMMON/OFFASL/LOFFA1,LOFFA2
      COMMON/OCLRA /LOCLRA, LOCLRS, LOCLRC(3), LRAM51
      COMMON/PLNTRR/JCHEMR,JCHMOR,JEPOSR,JCHEMT,JCHMOT,JEPOST,JCHCBS,   &
     &              JCPOSS,JXSTT,JXSTR,JXSS,JFSCN1,JFSCN2,JFSCK1,       &
     &              JFSCK2,JFSCM1,JFSCM2
      COMMON/PRTCTL/IPRINT
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      COMMON/SIMLM/OBSINT,VINT,ELCUTO,XSMLM

!
!     DIMENSION RDS1LC(1000)
      DIMENSION RL1(NM)
      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),RNM(NM),RRNM(NM),           &
     &   COSTHG(NM),SINTHG(NM),ENV(3,3),ELEVSC(NM),XYZOFF(3),           &
     &   WORK(NM),U(NM,9),RA(NM,3),UHAT(NM,3),RELV(NM,3),OCIONS(NM),    &
     &   OCANT(NM),OCDRYT(NM),OCION(NM),OCTPC(NM),OBSC(NM),OCWETT(NM),  &
     &   OCDLYG(NM),OCDLYS(NM),OBRATE(NM),OBSMET(NM),FSECSA(NM),        &
     &   GRELLT(NM),DELCET(NM),LPTS(NM),AA(*),II(*),RLRCOR(NM),         &
     &   URNM(NM,3),URNM2(NM,3),LL(*),OBSTRL(NM)
      DIMENSION XSM2(MINTIM,3),VSM2(MINTIM,3),FSECS2(NM)
      DIMENSION SATDLY(3),STADLY(3),XYZCG1(3),XYZCG2(3),OFFSET(3,2)
      DIMENSION SATDL2(3),STADL2(3),XY2CG1(3),XY2CG2(3)
      DIMENSION XY2OFF(3),OCTP2(NM),OCDLG2(NM),OCDLS2(NM)
      DIMENSION LPRE  (24),LPRESC(24)
      DIMENSION INDSTA(3)
      DIMENSION ILCORR(5)
      DIMENSION LTPMS1(MINTIM,3),LTPMS2(MINTIM,3),TPMES1(MINTIM,2),     &
     &          TPMES2(MINTIM,2)
      DIMENSION XYOF21(3),XYOF22(3)
      DIMENSION XYOF31(3),XYOF32(3)
      DIMENSION KLBIA(*)
      DIMENSION SCRTCH(NM,5)
      DIMENSION X2(3),X1(3)
      DIMENSION ATROT(3,3,MINTIM)
      DIMENSION ANTTAB(1)
      DIMENSION HOLD(MINTIM,18)
      DIMENSION SCALE(NM),RSCALE(NM)
      DIMENSION W1PU(NM,3,2),W2PU(NM,3,2)
      DIMENSION DWR(3),DWT(3),DTXDR(3),VRNM2(3)
      DIMENSION XSMV(1000,3),VSMV(1000,3)
      DATA C90/90.D0/
!
      DIMENSION fsecin(*)
!
      DIMENSION Z_MAG_TEST(1),V_MAG_TEST(1),COS_YAW_TEST(1)
      DIMENSION YAW_TEST(1),YDOTROTX_TEST(1)
      DIMENSION POS_TEST(3),VEL_TEST(3),ROT_X_TEST(3)
      DIMENSION Z_TEST(3),V_TEST(3),Y_TEST(3),X_TEST(3)
!
      data kentry/0/  !, iprint/10/

      INTEGER :: IWPR
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: RQ1,RQ2,RQ3,RQ4


      DOUBLE PRECISION, dimension( ndim_cgmass, MINTIM )  :: cg_par_array


!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
        LXAH=LXATTD
        LXATTD=.FALSE.

        kentry = kentry + 1
        iprint = 10

        LDORANT=.FALSE.

         !dum=xsm(1,1)**2+xsm(1,2)**2+xsm(1,3)**2
         !dum=dsqrt(dum)
         !print*,'ddd in robsum xsm',dum,xsm(1,1),xsm(1,2),xsm(1,3)
         !dum=xsm2(1,1)**2+xsm2(1,2)**2+xsm2(1,3)**2
         !dum=dsqrt(dum)
         !print*,'ddd in robsum xsm2',dum,xsm2(1,1),xsm2(1,2),xsm2(1,3)
         !dum=(xsm2(1,1)-xsm(1,1))**2
         !dum=dum+(xsm2(1,2)-xsm(1,2))**2
         !dum=dum+(xsm2(1,3)-xsm(1,3))**2
         !dum=dsqrt(dum)
         !print*,'ddd in robsum x1-x2',(xsm(1,1)-xsm2(1,1))/dum,&
         !(xsm(1,2)-xsm2(1,2))/dum,(xsm(1,3)-xsm2(1,3))/dum


!      write(6,'(A)')        'robsum: at entry '
!      write(6,'(A,1x,I10)') 'robsum: ISATID   ', ISATID
!      write(6,'(A,3x,L1)') 'robsum:1 LPHNTM(3)   ', LPHNTM(3)
!      write(6,*) 'robsum: kentry = ', kentry
!      write(6,'(A,1x,E15.7)') 'robsum: octpc(1) ', octpc(1)
!      write(6,'(A,1x,E15.7)') 'robsum: octp2(1) ', octp2(1)
!      write(6,*) 'robsum: lpre     ', lpre
!      write(6,*) 'robsum: lpresc   ', lpresc


! INITIALIZE XSMV VSMV

      DO 11 I=1,1000
      DO 11 J=1,3
      XSMV(I,J)=0.D0
      VSMV(I,J)=0.D0
   11 CONTINUE

      u(1:NM, 1:9) = 0.0D0

      ALLOCATE(RQ1(NM),RQ2(NM),RQ3(NM),RQ4(NM))

      L2PWND=.FALSE.
      LAPWND=(XGPSTB.GT..01D0)
      L1PWND=LAPWND
      IUPW=1
      IDNW=2

      ! FOR SAT-SAT LINKS, THE FIRST CALL TO TRKEXT IS THE USER (LOW)
      ! SATELLITE, AND SO SHOULD BE THE "DOWN" (IDNW) SATELLITE, I.E.
      ! THE LOWER ONE.  THE SECOND CALL TO TRKEXT IS FOR THE "UP" (IUPW)
      ! SATELLITE (THE GPS SATELLITE).
      ! IN OTHER CASES (SAT-STATION), ONLY THE FIRST TRKEXT IS CALLED,
      ! AND THE SATELLITE IS ALWAYS "UP" (IUPW). NOTE THAT SLR AND
      ! DORIS USE VLOW IN ANTSAC. THE SETTING OF THESE TO IUPW DOES
      ! NOT INTERFERE WITH THIS BECAUSE OF THE USE OF THE L2SATL
      ! ARGUMENT IN ANTSAC
      IF (L2SATL) THEN
          IUDW1 = IDNW
      ELSE
          IUDW1 = IUPW
      END IF

      FACTX=1.D0
      IF(LNEG) FACTX=-1.D0
      DO 30 I=1,NM
      SCALE(I)=FACTX*RSCALE(I)
   30 END DO

      !write(6,'(A,3x,L1,2x,E15.7)') 'robsum: LNEG,FACTX ',LNEG,FACTX

      IF( LOFFA1.AND.LOFFA2) THEN
          WRITE(6,34567)
          WRITE(6,34568)
          STOP
34567 FORMAT(' EXECUTION TERMINATING. SAT-SAT LINK WITH BOTH SATS ')
34568 FORMAT(' HAVING OFFSET ADJUSTMENT')

      ENDIF !   LOFFA1.AND.LOFFA2


      ! this is the array of saved range unit vectors for OFFADJ

      CALL CLEARA(AA(KOFDRV),NM)   ! OFFADJ


      !write(6,*) 'robsum:aft 30 lpresc   ', lpresc
      !write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ

!-------------------------------------------------------------------------------

! COMPUTE ANTENNA PHASE CORRECTION FOR STATION ANTENNA FROM TABLE IF AVAILABLE
! TRANSFORM UNIT VECTOR TO EARTH FIXED


      IF(.NOT.L2SATL) CALL ECFIXP(URNM,COSTHG,SINTHG,UHAT,NM,NM,NM)

!      IF( .NOT. L2SATL     .AND.               &
!          .NOT. LPRE(5)    .AND.               &
!          ANTTNO .GT. 0.d0 .AND.               &
!          NANTPT .GT. 0          ) THEN
!            ITAB=ANTTNO+.001d0
!            KPOFF=KANTPS(ITAB)
!            NPTBZ=ANTTAB(KPOFF)
!            NPTBA=ANTTAB(KPOFF+3)
!            CALL ANTPHC(ENV,UHAT,ANTTAB(KPOFF+6),NPTBA,NPTBZ,          &
!     &                  ANTTAB(KPOFF),WORK,NM,RA)
!            DO 300 N=1,NM
!                OCANT(N)=OCANT(N)+WORK(N)*SCALE(N)
!  300       CONTINUE
!      ENDIF ! ... ANTTNO .GT. 0.d0 .AND.  NANTPT .GT. 0

!      write(6,'(A,1x,I10,1x,E15.7, 2(4x,L1))') &
!            'robsum: NANTPT, anttno, L2SATL, LPRE(5) ',&
!                     NANTPT, anttno, L2SATL, LPRE(5)
!      write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ

      IF( .NOT. L2SATL     .AND.               &
          .NOT. LPRE(5)    .AND.               &
          ANTTNO .GT. 0.D0 .AND.               &
          NANTPT .GT. 0          ) THEN

          itab = NINT( anttno )

          !write(6,'(A,1x,I10,1x,E15.7)') &
          !   'robsum: itab, anttno ', itab, anttno

          if( nantpt > 0 ) then
              call get_index( itab,  &
                              II(KANTYP+ 2*(NANT_sat + NANT_sta) ), &
                              NANTPT,   index_ant, L_ant_phc )

!              write(6,'(A,2(1x,I10), 4x, L1)') &
!                 'robsum: aft get_index  itab, index_ant, L_ant_phc ',&
!                                         itab, index_ant, L_ant_phc
          endif ! nantpt > 0

          !write(6,'(A,2(1x,I10))') &
          !      'robsum: index_ant, kantps(index_ant) ', &
          !               index_ant, kantps(index_ant)
          !write(6,'(A,2(1x,I10))') 'robsum: isum_sat, isum_sta ', &
          !                                  isum_sat, isum_sta


          !IPOFF=KANTPS(index_ant) + isum_sat + isum_sta
          IPOFF=KANTPS( itab    ) + isum_sat + isum_sta

          !write(6,'(A,2(1x,I10))') &
          !      'robsum: index_ant, kantps(index_ant) ', &
          !               index_ant, kantps(index_ant)
          !write(6,'(A,2(1x,I10))') 'robsum: itab,      kantps( itab    )
          !                                  itab,      kantps( itab    )
          !write(6,'(A,3(1x,I10))') 'robsum: isum_sat, isum_sta, IPOFF ',
          !                                  isum_sat, isum_sta, IPOFF



          NDIMA=INT( ANTTAB(IPOFF+3) + 0.0001D0 )
          NDIMZ=INT( ANTTAB(IPOFF  ) + 0.0001D0 )

!          write(6,'(A,4(1x,I10))') &
!                'robsum: itab, IPOFF, NDIMA, NDIMZ ', &
!                         itab, IPOFF, NDIMA, NDIMZ
!          write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ

          !ITAB=ANTTNO+.001d0
          !KPOFF=KANTPS(ITAB)
          !NPTBZ=ANTTAB(KPOFF)
          !NPTBA=ANTTAB(KPOFF+3)

!          write(6,'(A,2(1x,I10))') &
!                'robsum:1 call antphc ISATID, NSTA0 ', &
!                                      ISATID, NSTA0
!          write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ

          CALL ANTPHC(ENV,UHAT,ANTTAB(IPOFF+6),NDIMA,NDIMZ,          &
     &                  ANTTAB(IPOFF),WORK,NM,RA)

          !write(6,'(A,1x,I10/(5(1x,E15.7)))') &
          !      'robsum: aft antphc for ant NM, WORK(1:NM) ', &
          !                                  NM, WORK(1:NM)

          do  N=1,NM
              OCANT(N)=OCANT(N)+WORK(N)*SCALE(N)
          enddo

!          write(6,'(A,2(1x,E15.7))')&
!                'robsum:1 aft antphc for ant WORK(1), OCANT(1) ', &
!                                             WORK(1), OCANT(1)

      ENDIF ! ... ANTTNO .GT. 0.d0 .AND.  NANTPT .GT. 0


!      write(6,'(A,2(3x,L1),2x,2(1x,I10))') &
!            'robsum: l2satl, lpre(5), nsta0, nant_sta ', &
!                     l2satl, lpre(5), nsta0, nant_sta
!      write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ


      IF( .NOT. L2SATL   .AND.  .NOT. LPRE(5)    .AND.            &
          NSTA0 > 0      .AND.  nant_sta > 0            ) THEN

            !ITAB=ANTTNO+.001d0
            !ITAB = NSTA0


!            write(6,'(A,1x,I10,1x,I6)') &
!              'robsum: nsta0, nint(anttno)   ', nsta0, NINT(anttno)

            itab = NINT( anttno )

            !write(6,'(/a)') 'robsum: call get_index2 for NSTA0  '
            !write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ

            call get_index2( nsta0, itab, II(KANTYP+ 2*NANT_sat ), &
                             NANT_sta, &
                             index_sta, L_sta_phc )

            !write(6,'(/A)') 'robsum: aft call get_index2 for NSTA0  '

            !write(6,'(A,1x,I10,1x,I6)') &
            !  'robsum: nsta0, index_sta      ', nsta0, index_sta

            if( index_sta > 0 )then

                ITAB = index_sta

                !write(6,'(A,1x,I10)') 'robsum: isum_sat   ',isum_sat
!                write(6,'(A,3(1x,I10))') &
!                      'robsum: nsta0, itab, isum_sat ', &
!                               nsta0, itab, isum_sat

                KPOFF= KANTPS_sta(ITAB)   + isum_sat

                NPTBZ=ANTTAB(KPOFF)
                NPTBA=ANTTAB(KPOFF+3)
                !write(6,'(A,3(1x,I10))') &
                !      'robsum: KPOFF, NPTBZ, NPTBA ', &
                !               KPOFF, NPTBZ, NPTBA
                !write(6,'(A,1x,I4,2(1x,I10))') &
                !      'robsum: itab, kantps_sta(itab), kpoff  ',  &
                !               itab, kantps_sta(itab), kpoff
                !write(6,'(A,2(1x,I10))') &
                !      'robsum: itab, nantps_sta(itab)  ',  &
                !               itab, nantps_sta(itab)

                !write(6,'(A,1x,I8,3x,F10.2,1x,I4)') &
                !      'robsum: kpoff, ANTTAB(KPOFF),   NPTBZ', &
                !               kpoff, ANTTAB(KPOFF),   NPTBZ
                !write(6,'(A,1x,I8,3x,F10.2,1x,I4)') &
                !      'robsum: kpoff, ANTTAB(KPOFF+3), NPTBA', &
                !               kpoff, ANTTAB(KPOFF+3), NPTBA
                !write(6,'(A,2(1x,E15.7))')&
                !      'robsum: bef antphc OCANT(1) ', OCANT(1)



                ! RA used as scratch array in ANTPHC

                !write(6,'(A,2(1x,I10))') &
                !'robsum:2 call antphc ISATID, NSTA0 ', &
                !                      ISATID, NSTA0

                CALL ANTPHC( ENV,UHAT,ANTTAB(KPOFF+6),NPTBA,NPTBZ,    &
                             ANTTAB(KPOFF),WORK,NM,RA)


                do  N=1,NM
                    OCANT(N)=OCANT(N)+WORK(N)*SCALE(N)
                enddo ! N


!                write(6,'(A,3(1x,E15.7))')&
!                  'robsum:2 aft antphc WORK(1), OCANT(1), SCALE(1) ',&
!                                       WORK(1), OCANT(1), SCALE(1)

            endif ! index_sta > 0


      ENDIF!.NOT.L2SATL.AND..NOT.LPRE(5).AND.NSTA0>0.d0.AND.nant_sta>0


!-----------------------------------------------------------------------------

      !write(6,*) 'robsum:679 octpc(1) ', octpc(1)
      !write(6,*) 'robsum: octp2(1) ', octp2(1)
      !write(6,*) 'robsum: lpre ', lpre
      !write(6,*) 'robsum: lpresc ', lpresc
      !write(6,'(A,1x,I10)')  'robsum: ISEQ = ', ISEQ

      do 99 N=1,nm
      scrtch(n,3)=0.D0
      scrtch(n,5)=0.D0
   99 continue

!     DO 100 N=1,1000
!     RDS1LC(N)=0.D0
!100  CONTINUE

      N1=KSMCOR
      N2=N1+NM-1

      !write(6,'(A,(2(1x,E15.7)))') &
      !      'robsum: sumcor  1 ', (AA(N),N= 1, 2 ) !N1,N1+2)

      JJLNK=MOD(JLINK,2)

!      write(6,*) 'robsum: scale,lneg:   ', scale, lneg
!      write(6,*) 'robsum: scale, rscale ', scale, rscale
!      write(6,*) 'robsum: l2satl ', l2satl

! For observation links that involve two satellites and no stations,
!     skip certain computations. Do so based on L2SATL.

!   L2SATL  = FLAG SET TO .TRUE. WHEN A LINK CONTAINS TWO SATS

      IF(L2SATL) THEN

         IF(LNELEV) GO TO 1000

! COMPUTE ELEVATION ANGLE FOR SAT TO SAT LINKS
! ANGLE IS FROM THE LOWER SATELLITE

         R1=XSM( 1,1)*XSM( 1,1)+XSM( 1,2)*XSM( 1,2)+XSM( 1,3)*XSM( 1,3)
         R2=XSM2(1,1)*XSM2(1,1)+XSM2(1,2)*XSM2(1,2)+XSM2(1,3)*XSM2(1,3)
         L2LOW=R1.GT.R2

         !write(6,'(A,3x,L1)') 'robsum: L2LOW ', L2LOW

         IF(L2LOW) GO TO 200

!  1ST SATELLITE IS THE LOW ONE
         DO 50 I=1,NM
         WORK(I)=SQRT(XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)               &
     &               +XSM(I,3)*XSM(I,3))
         U(I,1)=XSM2(I,1)-XSM(I,1)
         U(I,2)=XSM2(I,2)-XSM(I,2)
         U(I,3)=XSM2(I,3)-XSM(I,3)
         U(I,4)=SQRT(U(I,1)*U(I,1) + U(I,2)*U(I,2) + U(I,3)*U(I,3))
         U(I,5)=ACOS((XSM(I,1)*U(I,1)+XSM(I,2)*U(I,2)+XSM(I,3)*U(I,3))  &
     &         /(U(I,4)*WORK(I)))/DEGRAD
         ELEVSC(I)=C90-U(I,5)
   50    CONTINUE
         GO TO 1000
  200    CONTINUE


!  2ND SATELLITE IS THE LOW ONE

         DO 250 I=1,NM
         WORK(I)=SQRT(XSM2(I,1)*XSM2(I,1)+XSM2(I,2)*XSM2(I,2)           &
     &               +XSM2(I,3)*XSM2(I,3))
         U(I,1)=XSM(I,1)-XSM2(I,1)
         U(I,2)=XSM(I,2)-XSM2(I,2)
         U(I,3)=XSM(I,3)-XSM2(I,3)
         U(I,4)=SQRT(U(I,1)*U(I,1) + U(I,2)*U(I,2) + U(I,3)*U(I,3))
         U(I,5)=ACOS((XSM2(I,1)*U(I,1)+XSM2(I,2)*U(I,2)                 &
     &               +XSM2(I,3)*U(I,3))/(U(I,4)*WORK(I)))/DEGRAD
         ELEVSC(I)=C90-U(I,5)
  250    CONTINUE
         GO TO 1000


      ENDIF !  L2SATL

!      write(6,*) 'robsum: aft 250     '

! IF THIS BLOCK OF MEASURMENTS IS GPS, THEN DO SOME
! PRELIMINARY COMPUTATIONS FOR PHASE WINDUP

      IF(LAPWND) THEN

        DO JQP=1,2
        DO IQP=1,NM
         W1PU(IQP,1,JQP)=COSTHG(IQP)*VLOW(1,JQP,ISEQ)                   &
     &                  -SINTHG(IQP)*VLOW(2,JQP,ISEQ)
         W1PU(IQP,2,JQP)=SINTHG(IQP)*VLOW(1,JQP,ISEQ)                   &
     &                  +COSTHG(IQP)*VLOW(2,JQP,ISEQ)
         W1PU(IQP,3,JQP)=VLOW(3,JQP,ISEQ)
        ENDDO
        ENDDO
        CALL TDORTR(MJDSBL,FSECS2,W1PU(1,1,1),W2PU(1,1,1),              &
     &              AA,NM,NM,.TRUE.,.TRUE.,II)
        CALL TDORTR(MJDSBL,FSECS2,W1PU(1,1,2),W2PU(1,1,2),              &
     &              AA,NM,NM,.TRUE.,.TRUE.,II)
        L2PWND=.TRUE.

      ENDIF !  LAPWND



! CALCULATE S/C ELEVATION ABOVE HORIZON


      IF(.NOT.LACC.AND.LPSBL)LNELEV=.FALSE.
      CALL ELEV(UHAT,RNM,RRNM,VSM,COSTHG,SINTHG,ENV,U(1,1),U(1,2),      &
     &          U(1,3),RELV,ELEVSC,U(1,5),NM,.FALSE.,.FALSE.,LNELEV,    &
     &          LNELEV,LRANGE,LRANGE,.TRUE.)

      IF(.NOT.LACC)CALL SDELEV(LPTS,ELEVSC,NM,ELCUTO,.FALSE.)
!
!>>>>>>>>IF(LPRE(5)) GO TO 1000

! COMPUTE GROUND RECEIVER/TRANSMITTER ANTENNA AXIS CORRECTION

      MOUNT=TMOUNT

      !if( kentry .le. iprint ) then
      !    write(6,*) 'robsum: lpre( 5 ) ',  lpre(5)
      !    write(6,*) 'robsum: call antena mount, disp ', mount, disp
      !endif

      IF( mount .gt. 5 ) LPRE(5) = .false.
      IF(LPRE(5).and. mount .le. 5 ) GO TO 1000

      !if( kentry .le. iprint ) then
      !    write(6,*) 'robsum: call antena mount, disp ', mount, disp
      !endif

!>>>>>>>>>

      CALL ANTENA(ENV,UHAT,RA,WORK,NM,DISP,MOUNT,                       &
     &               nsta0, mjdsin, fsecin, aa, ii )

      !if( kentry .le. iprint ) then
      !    write(6,*) 'robsum: work(1) ', work(1)
      !endif

      DO 800 N=1,NM
         OCANT(N)=OCANT(N)+WORK(N)*SCALE(N)
  800 END DO

 1000 CONTINUE

      !write(6,*) 'robsum: sumcor  2 ', (AA(N),N=N1,N2)  ! N1+2)

      !write(6,*) 'robsum: below 1000  '
      !write(6,*) 'robsum: lpre(2)      ', lpre(2)
      !write(6,*) 'robsum: lpre(3 4 6 ) ', lpre(3), lpre(4), lpre(6)

      IF(LIONC.AND.LITER1) THEN
      LPRE(6)=.FALSE.
      LPRE(7)=.FALSE.
      ENDIF

      IF(LPRE(3).AND.LPRE(4).AND.LPRE(6)) GO TO 3000
!
! Skip tropospheric corrections if this is a satellite to satellite link
!
      IF(L2SATL) GO TO 1900
!
! COMPUTE GROUND-TO-S/C DRY & WET TROPO REFRACTION AND IONO REFRACTION
!
      MODELT=1
      IF(.NOT.LRANGE) MODELT=2

      !write(6,*) 'robsum: call refrac MJDSBL ', MJDSBL

! CALL DIFFERENT REFRACTION MODELS
! AVAILABLE MODELS ARE: 2=VLBI/GPS,1=MARINI-MURRAY, 0=HOPFIELD
!                3= GPS MODEL USING NEIL MAPPING FUNCTIONS
!                4= MODEL 3 FOR NON-LASER DATA/MARINI-MURRAY FOR LASER D
!                5= PORTO MODEL FOR LASER DATA
!                6= GLOBAL MAPPING FUNCTIONS (GMF)
!                7= GMF USING SAASTAMOINEN MODEL, 8=VMF1
! REFRACTION MODEL TYPES ARE: 0=ALTIMETER, 1=RANGE, 2=DOPPLER, 3=ELEV.

!!jtw IWPR is command for wether or not to print REFRAC results

!      WRITE(6,*)'jtw MTYPE', MTYPE
!      WRITE(6,*)'jtw MODEL(TYPE)', MODEL(MTYPE)

      IWPR=IRFPRT(MODEL(MTYPE))

      IF(MODEL(MTYPE).EQ.0) THEN

      CALL HOPFLD(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.1) THEN

           IF(LASER) THEN

                CALL MARMUR(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),   &
               &    RA(1,3),NM,MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,   &
               &    OBSMET,LPRE(1),.FALSE.,RNM,RNM,RNM,RNM,MTYPE,       &
               &    MJDSBL,FSECSA,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL HOPFLD(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),   &
               &    RA(1,3),NM,MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,   &
               &    OBSMET,LPRE(1),.FALSE.,RNM,RNM,RNM,RNM,MTYPE,       &
               &    MJDSBL,FSECSA,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.2) THEN

      CALL VLBGPS(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.3) THEN

      CALL GPSNIL(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.4) THEN

           IF(LASER) THEN

                CALL MARMUR(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),   &
               &    RA(1,3),NM,MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,   &
               &    OBSMET,LPRE(1),.FALSE.,RNM,RNM,RNM,RNM,MTYPE,       &
               &    MJDSBL,FSECSA,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL GPSNIL(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),   &
               &    RA(1,3),NM,MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,   &
               &    OBSMET,LPRE(1),.FALSE.,RNM,RNM,RNM,RNM,MTYPE,       &
               &    MJDSBL,FSECSA,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.5) THEN

           IF(LASER) THEN

                CALL MENDEZ(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),   &
               &    RA(1,3),NM,MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,   &
               &    OBSMET,LPRE(1),.FALSE.,RNM,RNM,RNM,RNM,MTYPE,       &
               &    MJDSBL,FSECSA,IWPR,NSTA0)

           ELSE IF(.NOT.LASER) THEN

                CALL HOPFLD(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),   &
               &    RA(1,3),NM,MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,   &
               &    OBSMET,LPRE(1),.FALSE.,RNM,RNM,RNM,RNM,MTYPE,       &
               &    MJDSBL,FSECSA,IWPR,NSTA0)

           END IF

      ELSE IF(MODEL(MTYPE).EQ.6) THEN

      CALL GMFHOP(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.7) THEN

      CALL GMFSAS(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,IWPR,NSTA0)

      ELSE IF(MODEL(MTYPE).EQ.8) THEN

      CALL VMF1(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,IWPR,NSTA0,AA(KA1UT))

!jtw edit above call to VMF 9/14/12
!      CALL VMF1(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
!     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
!     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,NSTA0)
!
!!      ELSE
!!
!!      WRITE(6,*)'jtw REFRACTION CORRECTION NOT CALLED'
!!
      END IF
!      CALL REFRAC(U(1,2),U(1,1),U(1,5),RNM,RA(1,1),RA(1,2),RA(1,3),NM,  &
!     &   MODELT,LPRE(3),LPRE(4),LPRE(6),BLKMET,OBSMET,LPRE(1),.FALSE.,  &
!     &   RNM,RNM,RNM,RNM,MTYPE,MJDSBL,FSECSA,NSTA0)
!
      IF(LPRE(3)) GO TO 1300
      DO 1200 N=1,NM
      OCDRYT(N)=OCDRYT(N)+RA(N,1)*SCALE(N)
!C     RDS1LC(N)=RDS1LC(N)+RA(N,1)*SCALE(N)
 1200 END DO
 1300 CONTINUE

      !write(6,*) 'robsum: sumcor  3 ', (AA(N),N=N1,N1+2)
      !write(6,*) 'robsum: ocdryt(1) ', ocdryt(1)

      IF(LPRE(4)) GO TO 1500
      DO 1400 N=1,NM
      OCWETT(N)=OCWETT(N)+RA(N,2)*SCALE(N)
!C    RDS1LC(N)=RDS1LC(N)+RA(N,2)*SCALE(N)
 1400 END DO
 1500 CONTINUE

      !write(6,*) 'robsum: sumcor  4 ', (AA(N),N=N1,N2)  ! N1+2)
      !write(6,*) 'robsum: ocwett(1) ', ocwett(1)
      !write(6,*) 'robsum: aft 1500  '


! DEP ION
! ADD HERE THE CODE FOR THE IR-90 IONOSPHERIC MODEL CORRECTION
!
!   DEFINE MJDSC: MODIFIED JULIAN DATE SECONDS FOR EACH OBSERVATION IN
!   BLOCK
!   DIMENSION AND DEFINE X1(3) POSITION VECTOR FOR STATION IN THE TRUE
!   DATE INERTIAL COORDINATE SYSTEM
!   X1(1)= The information is not readily available. It is contained in
!   AA(KX) figure this out in subroutine ROBS. We need to import these
!   Coordinates too in ROBSUM
!   DIMENSION AND DEFINE X2(3) POSITION VECTOR FOR SPACECRAFT IN THE
!   TRUE OF DATE INERTIAL COORDINATE SYSTEM.
!   X2(1)= XSM(N,1)
!   SET STEC TO ZERO
!   DEFINE THE LOGICAL LOGINT THROUGH THE INFORMATION
!   OF THE NEW COMMON BLOCK

 1900 CONTINUE
      IF(.NOT.LITER1) GOTO 2000
      IF(L2SATL)THEN
         IF(LPRE(7)) GO TO 2000
      ElSE
         IF(LPRE(6)) GO TO 2000
      ENDIF

      IF(LIONC)THEN
         LOGINT=.FALSE.
         IF(INTERP.EQ.1) LOGINT=.TRUE.


         DO 1550 N=1,NM


         DO 1560 JJ=1,3
            X1(JJ)=XSM(N,JJ)
            X2(JJ)= XSM2(N,JJ)
 1560 END DO

!      write(6,*) 'robsum: aft 1560  '

! CONVERT POSITIONS FROM TRUE OF REFERENCE TO TRUE OF DATE

      LCALCM=.TRUE.
      CALL TDORTR(MJDSIN,FSECIN,X1,X1,AA,1,1,LCALCM,.FALSE.,II)
      LCALCM=.FALSE.
      CALL TDORTR(MJDSIN,FSECIN,X2,X2,AA,1,1,LCALCM,.FALSE.,II)
      MJDSV=MJDSIN

      IF(ILINK.EQ.1.OR.ILINK.EQ.3.OR.ILINK.EQ.5)GO TO 1540

!     CALL ETUT(MJDSV,FSECSA(N),AA(KDPSR),AA(KXPUT),SCRTCH(n,3),
!    .         SCRTCH(n,5),AA(KA1UT),1)

      TMJDSC=DBLE(MJDSV)+FSECSA(N)

!     UTCTIM=DFLOAT(MJDSV)+SCRTCH(N,5)

      GOTO 1570

 1540 CONTINUE

!     CALL ETUT(MJDSV,FSECS2(N),AA(KDPSR),AA(KXPUT),SCRTCH(n,3),
!    .         SCRTCH(n,5),AA(KA1UT),1)

       TMJDSC=DBLE(MJDSV)+FSECS2(N)

!      UTCTIM=DFLOAT(MJDSV)+SCRTCH(N,5)

 1570 CONTINUE

       CALL IONRC(AA,TMJDSC,X1,X2,RA(N,3),LOGINT)


 1550  CONTINUE



       !write(6,*) 'robsum: aft 1550  '

! DEP
        CFACI=1.0D0
        HALF=0.5D0
! NEW DEP NOV 27
        LW=.FALSE.
        LW=MOD(MTYPE,2).EQ.0
        IF(MTYPE.GE.45.AND.MTYPE.LE.52) CFACI=HALF
        IF(MTYPE.GE.57.AND.MTYPE.LE.60) CFACI=HALF
! NEW DEP NOV 27
        CFACI=CFACI*40.3D0
        IF(LW) CFACI=-CFACI
        ELCTOM=CFACI/FRQLNK**2
!
! DEP
      IF(L2SATL)THEN
         DO 1600 N=1,NM
            OCIONS(N)=OCIONS(N)+RA(N,3)*ELCTOM*SCALE(N)
 1600   CONTINUE
      ELSE
         DO 1605 N=1,NM
            OCION(N)=OCION(N)+RA(N,3)*ELCTOM*SCALE(N)
 1605    CONTINUE
      ENDIF
      ENDIF

 2000 CONTINUE

      !write(6,'(A)') 'robsum: aft 2000 '
      !write(6,'(A,(2(1x,E15.7)))') &
      !      'robsum:2000 sumcor  5 ', (AA(N),N= 1, 2 ) !N1,N2) !N1+2)

!     IF(LPRESC(7)) GO TO 3000
! COMPUTE IONOSPHERIC REFRACTION BETWEEN TWO SPACECRAFT; OCIONS

!*****

 3000 CONTINUE

!      write(6,*) 'robsum: below 3000 '
!      write(6,*) 'robsum: l2satl, lrange ', l2satl, lrange


!***** the next line disables tracking point correction for
!      instantaneous intersatellite range rates.

      IF(L2SATL.AND..NOT.LRANGE) GO TO 4000


      ! LPRESC(2) = TRUE means data corrected for cgmass offset

      !write(6,'(A,3x,L1)') 'robsum: lpresc(2) ', lpresc(2)
      !write(6,'(A,3x,L1)') 'robsum: LOFFA1    ', LOFFA1

!     IF(LPRESC(2) .AND. .NOT. lram51 ) GO TO 4000
!     IF( LPRESC(2) ) GO TO 4000


      !orig IF( LPRESC(2).AND..NOT.LOFFA1) GO TO 3900   !!!!!!!!!!!!!!!!!
      IF( LPRESC(2).AND..NOT.LOFFA1) GO TO 3700


! Compute offset correction for the relay satellite (the "to" sat) if
! an OFFSET card was found that applies to this block.
! (As implied by JXYZOF being equal to KFRQOF)

!      write(6,*) 'robsum: above goto 3900  jxyzof(1)  kfrqof ', &
!                                           jxyzof(1), kfrqof

!      ! this is the array of saved range unit vectors for OFFADJ

      LGPSTP=.FALSE.
      IF(L2SATL) THEN
         LGPS1=.FALSE.
         LGPS2=.FALSE.
         LTP1=.FALSE.
         LTP2=.FALSE.
         ICR1=ICOOR1/100
         ICR2=ICOOR2/100
         IF(ICR1.EQ.1) LGPS1=.TRUE.
         IF(ICR2.EQ.1) LGPS2=.TRUE.
         IF(ICR1.EQ.2) LTP1=.TRUE.
         IF(ICR2.EQ.2) LTP2=.TRUE.
         LGPSTP=(LGPS1.AND.LTP2).OR.(LTP1.AND.LGPS2)
       ENDIF

!      write(6,*) 'robsum:   KFRQOF , JXYZOF(1) ', &
!                            KFRQOF , JXYZOF(1)

      IF(JXYZOF(1).LE.KFRQOF) GO TO 3900




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! COMPUTE S/C C. G. CORRECTION  CGMASS
! from CGMASS CARD
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !write(6,*) 'robsum: cgmass   KFRQOF, JXYZCG(1,1) ', &
      !                             KFRQOF, JXYZCG(1,1)
      OSCALE=ZERO
      IF(JXYZCG(1,1).GT.KFRQOF) OSCALE=ONE

      !write(6,'(A,3(1x,E15.7))') 'robsum:   OSCALE ', OSCALE

      !write(6,*) 'robsum:   XYZCG1 ', XYZCG1
      !write(6,*) 'robsum:   XYZCG2 ', XYZCG2
! XYZCG1 & XYZCG2 HAVE SAME VALUES
      DO 3600 I=1,3
      OFFSET(I,1)=XYZOFF(I)-XYZCG1(I)*OSCALE
      OFFSET(I,2)=XYZOFF(I)-XYZCG2(I)*OSCALE
 3600 END DO
!        print*,(xyzcg1(i),i=1,3)
!        do i=1,nm
!          print*,i,mjdsbl,MJDSIN,FSECIN(i)
!        enddo

      !write(6,'(A,3(1x,E15.7))') 'robsum: xyzcg1 ', xyzcg1(1:3)
      !write(6,'(A,3(1x,E15.7))') 'robsum: xyzcg2 ', xyzcg2(1:3)

      !write(6,'(A,3(1x,E15.7))') &
      !      'robsum:3600 OFFSET(1:3,1) ', OFFSET(1:3,1)
      !write(6,'(A,3(1x,E15.7))') &
      !      'robsum:3600 OFFSET(1:3,2) ', OFFSET(1:3,2)

3700  continue  ! jjm 20120419

!      write(6,*) 'robsum: after 3700 lrange ', lrange

      IF(LRANGE) THEN

         ICRSYS = ICOOR1/100

!         write(6,*) 'robsum: after 3600 icrsys ', icrsys

        DO 3750 N=1,NM
        DO 3750 I=1,3
        DO 3750 J=1,3
        ATROT(I,J,N)=0.D0
 3750   CONTINUE
        DO 3755 N=1,NM
        DO 3755 I=1,3
        ATROT(I,I,N)=1.D0
 3755   CONTINUE

!        print *,'robsum: before getrpy1'
!        print *,'atrot(1,1-3) ',atrot(1,1,1),atrot(1,2,1),atrot(1,3,1)
!        print *,'atrot(2,1-3) ',atrot(2,1,1),atrot(2,2,1),atrot(2,3,1)
!        print *,'atrot(3,1-3) ',atrot(3,1,1),atrot(3,2,1),atrot(3,3,1)
!       IF(NASAT.GT.0) THEN
!       CALL GETRPY(AA,II,MJDSIN,FSECSA,II(KATSAT),II(KKVLAS),
!    .              AA(KATIME),II(KATRSQ),II(KISATN-1+ISET1),0,ATROT,
!    .              AA(KATPER),ISET1)
!       ENDIF

! COMPUTE OFFSET CORRECTION USING EXTERNAL ATTITUDE ORIENTATION
! IF REQUESTED

!        write(6,'(A,3x,L1)') 'robsum: after 3600 lofext(1) ', lofext(1)

        IF(LOFEXT(1)) THEN

         IANTNM=ICOOR1-ICRSYS*100


!         write(6,*) 'robsum: ICOOR1, ICRSYS ', ICOOR1, ICRSYS
!         write(6,*) 'robsum: IANTNM ', IANTNM

         IF((IANTNM.LT.1).OR.(IANTNM.GT.4)) GOTO 9100

         ! debug extatt
         !write(6,*) 'robsum: iset1 is: ',iset1
         ! debug extatt
!         write(6,*) 'robsum: LL(KEALQT+ISET1-1) ', &
!                             LL(KEALQT+ISET1-1)

          IF(.NOT.LL(KEALQT+ISET1-1)) GOTO 9200

          ISBJ20 = II(KEASBJ+ISET1-1)
          NUMAEA = II(KEANAN+ISET1-1)
          IPANEA = II(KEAPAN+ISET1-1)
          MJDSEA = II(KEAMJS+ISET1-1)
          FSSCEA = AA(KEAFSS+ISET1-1)
          RINTEA = AA(KEAINS+ISET1-1)
          LINATO = .FALSE.

          DO I=1,NM
              RQ1(I)=0.D0
              RQ2(I)=0.D0
              RQ3(I)=0.D0
              RQ4(I)=0.D0
          ENDDO


         if( (.not. lpresc(2) .or. LOFFA1 )  .or. &
             LPHNTM(3)   )then

             !write(6,'(A)') 'robsum:1 call trkext '

             ! aa(kofdrv) used by OFFADJ



      CALL TRKEXT(AA(KEAQAT),II(KEAAAA),MEAMEM,MEAANT,        &
     &            URNM2,OFFSET,                               &
     &            XYOF21,FSECSA,RA,WORK,IANTNM,ISBJ20,NUMAEA, &
     &            IPANEA,MJDSEA,FSSCEA,RINTEA,LINATO,AA,II,   &
     &            ICRSYS,.FALSE.,NM,MJDSIN,ATROT,AA(KOFDRV),  &
     &            LOFFA1,RQ1,RQ2,RQ3,RQ4,.TRUE.,LL(KLANTC),   &
     &            II(KANCUT),LAPWND,L2PWND,W2PU,ISEQ,IUDW1,   &
     &            ANTTAB,AA(KPHC),XYOF31,ISATID,AA(KSTAIN),   &
     &            INDSTA,cg_par_array, lpresc,LDORANT,L2SATL)


             if( LPHNTM(3) )then

                 !write(6,'(/a)') 'robsum:1 aft call trkext '
                 !write(6,'(A,1x,E15.7)') &
                 !      'robsum:1 cg_par_array OSCALE =  ', OSCALE

                 cg_par_array(1:ndim_cgmass,1:nm ) = &
                        cg_par_array(1:ndim_cgmass,1:nm ) * OSCALE  ! ??

                 !write(6,'(6(1x,E15.7))') cg_par_array(1:ndim_cgmass,1)

             endif ! LPHNTM(3)

         endif ! (.not. lpresc(2) .or. LOFFA1 )  .or.  LPHNTM(3)

! debug extatt
!!       write(6,*)'robsum: linato: ',linato
! debug extatt

         IF( .NOT.LINATO) THEN

             ! COMPUTE LRA CORRECTION FOR T/P IF NECESSARY

       IF((ICRSYS.EQ.2).AND.(MTYPE.EQ.51))                      &
     &  CALL TRKTOP(XSM,VSM,URNM2,OFFSET,ELEVSC,FSECSA,         &
     &         RA,WORK,LPTS,AA,II,INDSTA,                       &
     &         TPMES1,LTPMS1,.TRUE.,2,ISATID,.FALSE.,NM,ATROT,  &
     &         AA(KPHC),LGPSTP,.TRUE.,LOFFA1,AA(KOFDRV),        &
     &         LL(KLANTC),II(KANCUT),LAPWND,L2PWND,W2PU,ISEQ,   &
     &         IDNW,AA(KSTAIN),ANTTAB,LDORANT)
! IDNW JUST ABOVE IS OK; IF SAT-SAT LINK TOPEX IS DOWN
! IF SAT STA LINK THEN DORIS OR SLR APPLIES AND THESE USE
! VLOW IN ANTSAC
             GOTO 3650
         ENDIF ! .not. LINATO


      ENDIF !  LOFEXT(1)

      if( (.not. lpresc(2) .or. LOFFA1 )   )then


         IF(ICRSYS.EQ.0.AND.ISATID.NE.7502701) THEN

           CALL TRAKPC(XSM,VSM,    URNM,          OFFSET,               &
     &                            FSECSA,U,RA,WORK,LASER,               &
     &                 .TRUE.,LOFFA1,AA(KOFDRV))
         ELSE IF (ICRSYS.EQ.1) THEN
           CALL TRKPC2(XSM,VSM,URNM2,OFFSET,FSECSA,WORK,AA,             &
     &                 .TRUE.,LOFFA1,II,ISET1,.FALSE.,NM,ATROT,         &
     &                  AA(KOFDRV),HOLD(1,1),HOLD(1,10),LL(KLANTC),     &
     &                  II(KANCUT),LAPWND,L1PWND,W1PU,ISEQ,IUPW,ANTTAB)
         ELSE IF (ICRSYS.EQ.2) THEN
          LAPLRA=(MTYPE.EQ.51)
!           print *,'robsum: call to trktop for relay sat'
!        write(6,*)'robsum call trktop2'
           CALL TRKTOP(XSM,VSM,URNM2,OFFSET,ELEVSC,FSECSA,              &
     &                 RA,WORK,LPTS,AA,II,INDSTA,                       &
     &                 TPMES1,LTPMS1,LAPLRA,1,ISATID,.FALSE.,NM,ATROT,  &
     &                 AA(KPHC),LGPSTP,.TRUE.,LOFFA1,AA(KOFDRV),        &
     &                 LL(KLANTC),II(KANCUT),LAPWND,L2PWND,W2PU,ISEQ,   &
     &                 IDNW,AA(KSTAIN),ANTTAB,LDORANT)
! IDNW JUST ABOVE IS OK; IF SAT-SAT LINK TOPEX IS DOWN
! IF SAT STA LINK THEN DORIS OR SLR APPLIES AND THESE USE
! VLOW IN ANTSAC
         ELSE IF (ICRSYS.EQ.3) THEN
       CALL TRKSPT(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &             .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),              &
     &             AA(KSTAIN),INDSTA,AA(KPHC),ANTTAB,AA(KOFDRV),LOFFA1)
         ELSE IF (ICRSYS.EQ.4) THEN
       CALL TRKGPS(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &             LL(KLANTC),II(KANCUT),NM,ATROT,AA(KOFDRV),LOFFA1,    &
                   ISEQ,LAPWND,IUPW,W1PU,.TRUE.)
         ELSE IF (ICRSYS.EQ.5 .OR. ICRSYS.EQ.14) THEN
       CALL TRKERS(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &             .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),              &
     &              .TRUE.,LOFFA1,AA(KOFDRV),AA(KSTAIN),INDSTA,         &
     &              AA(KPHC),ANTTAB,LDORANT)
         ELSE IF (ICRSYS.EQ.6) THEN

! Note that this model assumes that the HGA points at the Earth and
! gimbal angles are fixed.  Therefore, the position of the tracking
! point in the body-fixed system is constant.   Also, note that since
! the rotation about the s/c Y-axis is ignored, the location of the
! HGA in inertial space is necessarily correct. However, with Doppler
! measurements this approximation is suitable.

           CALL TRKMO(AA(JXSS),VSM,URNM2,OFFSET,FSECSA,RA,WORK,         &
     &                XYOF21,AA,II,.FALSE.,NM,ATROT,LL(KLANTC),         &
     &                II(KANCUT),ISATID,AA(KOFDRV),LOFFA1)

!        ELSE IF (ICRSYS.EQ.7) THEN
         ELSE IF (ICRSYS.EQ.7 .OR. ICRSYS.EQ.8 ) THEN
! The following TDRSS routine only works with single link offset

          CALL TRKTDS(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,        &
     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,    &
     &                 AA(KOFDRV),LOFFA1)
         ELSE IF (ICRSYS.EQ.9) THEN
!        ....magnetically stabilized satellite
!         if( kentry .le. iprint ) then
!            write(6,*) 'robsum: call trkmag '
!         endif
          CALL TRKMAG(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID, &
     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),           &
     &                AA(KOFDRV),LOFFA1)
         ELSE IF (ICRSYS.EQ.10) THEN
       CALL TRKGFO(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &             .TRUE.,LOFFA1,AA(KOFDRV),.FALSE.,NM,ATROT,LL(KLANTC),&
     &              II(KANCUT))
         ELSE IF (ICRSYS.EQ.11) THEN
       CALL TRKTRM(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &              LL(KLANTC),II(KANCUT),NM,AA(KOFDRV),LOFFA1)
         ELSE IF (ICRSYS.EQ.12) THEN
       CALL TRKEUV(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &             LL(KLANTC),II(KANCUT),NM,AA(KOFDRV),LOFFA1)
         ELSE IF (ICRSYS.EQ.13) THEN
! pd %%%%%% the unit x vector is in the direction of forward velocity
!          CALL TRKVCL(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,        &
!     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID)
           CALL TRKICE(XSM,VSM,URNM2,OFFSET,FSECRA,RA,WORK,AA,II,       &
     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,    &
     &                .TRUE.,AA(KPHC),LAPWND,L2PWND,W2PU,ISEQ,IDNW,     &
     &                ANTTAB,LOFFA1,AA(KOFDRV))
! IDNW JUST ABOVE IS OK; IF SAT-SAT LINK ICESAT IS DOWN
! IF SAT STA LINK THEN DORIS OR SLR APPLIES AND THESE USE
! VLOW IN ANTSAC
         ELSE IF (ICRSYS.EQ.15) THEN
       CALL TRKCR2(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,ISATID,    &
     &             .TRUE.,LOFFA1,AA(KOFDRV),.FALSE.,NM,ATROT,LL(KLANTC),&
     &              II(KANCUT),AA(KSTAIN),INDSTA,AA(KPHC),ANTTAB,       &
     &              LDORANT)

         ELSE IF (ISATID.EQ.7502701) THEN
! CONVERT XSM (SATELLITE) FROM TOR TO TOD
         DO 3651 I=1,NM
         DO 3651 J=1,3
         X1(J)=XSM(I,J)
         X2(J)=VSM(I,J)
         CALL TDORTR(MJDSIN,FSECSA(I),X1,X1,AA,1,1,.FALSE.,.FALSE.,II)
         CALL TDORTR(MJDSIN,FSECSA(I),X2,X2,AA,1,1,.FALSE.,.FALSE.,II)
         XSMV(I,J)=X1(J)
         VSMV(I,J)=X2(J)
 3651    CONTINUE
         CALL TRKGEOS3(MJDSIN,FSECSA,XSMV,VSMV,XSM2,WORK,NM,URNM2,      &
     &                 AA(KOFDRV),LOFFA1)


         ELSE IF (ICRSYS.EQ.18) THEN
      CALL TRKHY2(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,.FALSE.,    &
     &          NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,AA(KSTAIN),       &
     &          INDSTA,AA(KPHC),ANTTAB,LDORANT,AA(KOFDRV),LOFFA1)

         ELSE IF (ICRSYS.EQ.19) THEN
         IF(MTYPE.EQ.51)write(6,*)' dbg ROBSUM ICRSYS',ICRSYS,NSARAL
         LSRLLRA=(MTYPE.EQ.51.AND.NSARAL.EQ.1)
         write(6,*)'robsum call trksrl1, lsrllra',LSRLLRA
      CALL TRKSRL(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,.FALSE.,    &
     &        NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,AA(KSTAIN),         &
     &        INDSTA,AA(KPHC),ANTTAB,LDORANT,AA(KOFDRV),LOFFA1,LSRLLRA)

         ELSE

           GO TO 9000


         ENDIF !  ICRSYS.EQ.0.AND.ISATID.NE.7502701

      endif ! (.not. lpresc(2) .or. LOFFA1 )

 3650   CONTINUE

! debug extatt
!        write(6,'(A,3x,L1)') 'robsum: after 3650, lrange: ',lrange
! debug extatt

      ENDIF ! ?? LRANGE

      IF(.NOT.LRANGE) CALL TRAKRR(XSM,VSM,RNM,URNM,UHAT,RELV,OFFSET,    &
     &                     ELEVSC,FSECSA,U,RA,WORK,LASER,ICRSYS,AA,II,  &
     &                     LPTS,XYOF21,ISET1,URNM2,ICOOR1,INDSTA,       &
     &                     TPMES1,LTPMS1,ISATID,MJDSIN,FSECIN,ATROT,LL, &
     &                     XYOF31)

!!!!TEST
!        if( indsta(1).ne.0) then
!            print*, 'dbg STATION ----- SATELLITE',kentry
!        else
!            print*, 'dbg SATELLITE ----- SATELLITE: S1',kentry
!        endif
!        print*, 'dbg scale ', scale(1)
!!!!TEST

      IF( LPRESC(2).AND..NOT.LOFFA1) GO TO 3900   ! jjm 20120419

      !write(6,'(A,3(1x,E15.7))') 'robsum: bef 3800 octpc(1) ', &
      !                                             octpc(1)

      DO 3800 N=1,NM

      OCTPC(N)=OCTPC(N)+WORK(N)*SCALE(N)

      ! debug extatt
      !if( N == 1 )then
      !write(6,'(A,1x,I3,1x,E24.16)') &
      !      'robsum: 3800 N, octpc(N)   ', N, octpc(N)
      !write(6,'(A,1x,I3,2(1x,E24.16))') &
      !      'robsum: 3800 n,work,scale: ',n,work(n),scale(n)
      !endif
      ! debug extatt
 3800 END DO

 3900 CONTINUE

      !write(6,'(A,(2(1x,E15.7)))') &
      !      'robsum2: sumcor  6 ', (AA(N),N=1,2) !N1,N1+2)
      !write(6,'(A)') &
      !  'robsum: after 3900 TRACK POINT CORR. NOT COMPUTED '
      !write(6,'(A,3(1x,E24.16))') &
      !           'robsum: after 3900 octpc(1) ', octpc(1)

! Compute offset correction for the user satellite


      !!write(6,'(A)') 'robsum2: user above goto 4000 '
      !write(6,'(A,2(1x,I10))') 'robsum: jxyzof(2), kfrqof ', &
      !                                  jxyzof(2), kfrqof


      IF(JXYZOF(2).LE.KFRQOF) GO TO 4000

      !write(6,'(A,2(3x,L1))') 'robsum: CGMASS LPRESC(2), LOFFA2 ', &
      !                                        LPRESC(2), LOFFA2


      !orig IF( LPRESC(2).AND..NOT.LOFFA2) GO TO 4000
      IF( LPRESC(2).AND..NOT.LOFFA2) GO TO 3300


! Compute s/c c.g. correction  ( cgmass ) 2nd sat

      OSCALE=ZERO
      IF(JXYZCG(1,2).GT.KFRQOF) OSCALE=ONE

      !write(6,'(A,3(1x,E15.7))') 'robsum:2   OSCALE ', OSCALE
      !write(6,'(A,3(1x,E15.7))') 'robsum:2   XY2CG1 ', XY2CG1
      !write(6,'(A,3(1x,E15.7))') 'robsum:2   XY2CG2 ', XY2CG2
      !write(6,'(A,3(1x,E15.7))') 'robsum:2   XY2OFF ', XY2OFF
! XY2CG1 & XY2CG2 HAVE SAME VALUES
      DO 3920 I=1,3
          OFFSET(I,1)=XY2OFF(I)-XY2CG1(I)*OSCALE
          OFFSET(I,2)=XY2OFF(I)-XY2CG2(I)*OSCALE

          !write(6,'(A,1x,I3, 2(1x,E15.7))') &
          !          'robsum:2 i, xy2off(i) ', &
          !                    i, xy2off(i)
          !write(6,'(A,1x,I3, 2(1x,E15.7))') &
          !          'robsum:2 i, xy2cg1(i),xy2cg2(i)  ', &
          !                    i, xy2cg1(i),xy2cg2(i)
          !write(6,'(A,1x,I3, 2(1x,E15.7))') &
          !          'robsum:2 i, offset(i,1), offset(i,2) ', &
          !                    i, offset(i,1), offset(i,2)
 3920 ENDDO

      !write(6,'(A/(3(1x,E15.7)))') &
      !   'robsum: after 3920 offset ', offset
      !write(6,'(A,4x,L1)') 'robsum: after 3920 lrange ', lrange

 3300 continue

!       WRITE(6,*)'robsum2:LRANGE',LRANGE
      IF(LRANGE) THEN

         ICRSYS = ICOOR2/100

        DO 3760 N=1,NM
        DO 3761 I=1,3
        DO 3761 J=1,3
        ATROT(I,J,N)=0.D0
        ATROT(I,I,N)=1.D0
 3761   CONTINUE
 3760   CONTINUE

!        print *,'robsum: before getrpy1'
!        print *,'atrot(1,1-3) ',atrot(1,1,1),atrot(1,2,1),atrot(1,3,1)
!        print *,'atrot(2,1-3) ',atrot(2,1,1),atrot(2,2,1),atrot(2,3,1)
!        print *,'atrot(3,1-3) ',atrot(3,1,1),atrot(3,2,1),atrot(3,3,1)
!        IF(NASAT.GT.0) THEN
!        print *,'robsum: call to getrpy'
!        CALL GETRPY(AA,II,MJDSIN,FSECS2,II(KATSAT),II(KKVLAS),
!     .              AA(KATIME),II(KATRSQ),II(KISATN-1+ISET2),0,ATROT,
!     .              AA(KATPER),ISET2)
!        ENDIF


! COMPUTE OFFSET CORRECTION USING EXTERNAL ATTITUDE ORIENTATION
! IF REQUESTED

!        write(6,'(A,3x,L1)') 'robsum2 after 3760 lofext(2) ', lofext(2)

        IF(LOFEXT(2)) THEN

         IANTNM=ICOOR2-ICRSYS*100


!         write(6,'(A,1x,I10)') 'robsum: IANTNM ', IANTNM

         IF((IANTNM.LT.1).OR.(IANTNM.GT.4)) GOTO 9100

          ! debug extatt
          !write(6,*) 'robsum: iset2 is: ',iset2
          ! debug extatt

          IF(.NOT.LL(KEALQT+ISET2-1)) GOTO 9200
          ISBJ20 = II(KEASBJ+ISET2-1)
          NUMAEA = II(KEANAN+ISET2-1)
          IPANEA = II(KEAPAN+ISET2-1)
          MJDSEA = II(KEAMJS+ISET2-1)
          FSSCEA = AA(KEAFSS+ISET2-1)
          RINTEA = AA(KEAINS+ISET2-1)
          LINATO = .FALSE.
          DO I=1,NM
            RQ1(I)=0.D0
            RQ2(I)=0.D0
            RQ3(I)=0.D0
            RQ4(I)=0.D0
          ENDDO

!         write(6,'(A,3(3x,L1))') &
!               'robsum2 lpresc(2), LOFFA2, LPHNTM(3) ', &
!                         lpresc(2), LOFFA2, LPHNTM(3)

         if( (.not. lpresc(2) .or. LOFFA2 )    .or.    &
             LPHNTM(3)                                   )then

!             write(6,'(A)') 'robsum2 call trkext '
!             write(6,*)'KSTAIN,AA(KSTAIN)',KSTAIN,AA(KSTAIN)
!           write(6,*)'robsum bef trkext work(1)',WORK(1)


       CALL TRKEXT(AA(KEAQAT),II(KEAAAA),MEAMEM,MEAANT,        &
      &            URNM2,OFFSET,                               &
      &            XYOF22,FSECS2,RA,WORK,IANTNM,ISBJ20,NUMAEA, &
      &            IPANEA,MJDSEA,FSSCEA,RINTEA,LINATO,AA,II,   &
      &            ICRSYS,.FALSE.,NM,MJDSIN,ATROT,AA(KOFDRV),  &
      &            LOFFA2,RQ1,RQ2,RQ3,RQ4,.FALSE.,LL(KLANTC),  &
      &            II(KANCUT),.FALSE.,L2PWND,W2PU,ISEQ,IUPW,   &
      &            ANTTAB,AA(KPHC),XYOF32,ISATID,AA(KSTAIN),   &
      &            INDSTA,cg_par_array, lpresc,LDORANT,L2SATL)
!           write(6,*)'robsum aft trkext work(1)',WORK(1)


             if( LPHNTM(3) )then

                 !write(6,'(/a)') 'robsum:2 aft call trkext '

                 !write(6,'(A,1x,E15.7)') &
                 !      'robsum:2 cg_par_array OSCALE =  ', OSCALE

                 cg_par_array(1:ndim_cgmass,1:nm ) = &
                        cg_par_array(1:ndim_cgmass,1:nm ) * OSCALE  ! ??

                 !write(6,'(6(1x,E15.7))') cg_par_array(1:ndim_cgmass,1)

             endif ! LPHNTM(3)



         IF(.NOT.LINATO) THEN

! COMPUTE LRA CORRECTION FOR T/P IF NECESSARY
          IF((ICRSYS.EQ.2).AND.(MTYPE.EQ.51))                           &
     &     CALL TRKTOP(XSM2,VSM2,URNM2,OFFSET,ELEVSC,FSECS2,            &
     &                 RA,WORK,LPTS,AA,II,INDSTA,                       &
     &                 TPMES2,LTPMS2,.TRUE.,2,II(KISATN-1+ISET2),       &
     &                 .FALSE.,NM,ATROT,AA(KPHC),LGPSTP,.FALSE.,LOFFA2, &
     &                  AA(KOFDRV),LL(KLANTC),II(KANCUT),               &
     &                 .FALSE.,L2PWND,W2PU,ISEQ,IDNW,AA(KSTAIN),ANTTAB, &
     &                  LDORANT)
! IDN JUST ABOVE IS OK. THIS CALL IS FOR THE HIGH SAT OF A SAT SAT LINK
! TOPEX WOULD NOT BE IN THIS POSITION. ALL TOPEX TRACKING TYPES ARE
! LOW
          GOTO 3930
         ENDIF !  .NOT.LINATO



      endif ! ( .not. lpresc(2) .or. LOFFA2 )   .or.   LPHNTM(3)


      ENDIF !  LOFEXT(2)

       !write(6,'(A,1x,I6)') 'robsum: after 3920 icrsys ', icrsys


      if( .not. lpresc(2) .or. LOFFA2 )then

       IF(ICRSYS.EQ.0) THEN

           CALL TRAKPC(XSM2,VSM2,    URNM,  OFFSET,   &
                       FSECS2,U,RA,WORK,LASER,        &
                       .FALSE.,LOFFA2,AA(KOFDRV))

         ELSE IF (ICRSYS.EQ.1) THEN

           CALL TRKPC2(XSM2,VSM2,URNM2,OFFSET,FSECS2,WORK,AA,           &
     &                 .FALSE.,LOFFA2,II,ISET2,.FALSE.,NM,ATROT,        &
     &                  AA(KOFDRV),HOLD(1,1),HOLD(1,10),LL(KLANTC),     &
     &                  II(KANCUT),LAPWND,L1PWND,W1PU,ISEQ,IUPW,ANTTAB)

         ELSE IF (ICRSYS.EQ.2) THEN

           LAPLRA=(MTYPE.EQ.51)

!           write(6,*)'robsum: call to trktop for user sat'

!        write(6,*)'robsum call trktop4'
           CALL TRKTOP(XSM2,VSM2,URNM2,OFFSET,ELEVSC,FSECS2,            &
     &                 RA,WORK,LPTS,AA,II,INDSTA,                       &
     &                 TPMES2,LTPMS2,LAPLRA,1,                          &
     &                 II(KISATN-1+ISET2),                              &
     &                .FALSE.,NM,ATROT,AA(KPHC),LGPSTP,.FALSE.,LOFFA2,  &
     &                 AA(KOFDRV),LL(KLANTC),II(KANCUT),.FALSE.,L2PWND, &
     &                 W2PU,ISEQ,IDNW,AA(KSTAIN),ANTTAB,LDORANT)
! IDN JUST ABOVE IS OK. THIS CALL IS FOR THE HIGH SAT OF A SAT SAT LINK
! TOPEX WOULD NOT BE IN THIS POSITION. ALL TOPEX TRACKING TYPES ARE
! LOW

         ELSE IF (ICRSYS.EQ.3) THEN

      CALL TRKSPT(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,          &
     &           II(KISATN-1+ISET2), .FALSE.,NM,ATROT,LL(KLANTC),       &
     &           II(KANCUT),AA(KSTAIN),INDSTA,AA(KPHC),ANTTAB,          &
     &           AA(KOFDRV),LOFFA2)

         ELSE IF (ICRSYS.EQ.4) THEN
      CALL TRKGPS(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,          &
     &            II(KISATN-1+ISET2),LL(KLANTC),II(KANCUT),NM,          &
     &            ATROT,AA(KOFDRV),LOFFA2,ISEQ,LAPWND,IUPW,W1PU,.FALSE.)

         ELSE IF (ICRSYS.EQ.5) THEN

      CALL TRKERS(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,          &
     &            II(KISATN-1+ISET2), .FALSE.,NM,ATROT,LL(KLANTC),      &
     &          II(KANCUT),.FALSE.,LOFFA2,AA(KOFDRV),AA(KSTAIN),INDSTA, &
     &          AA(KPHC),ANTTAB,LDORANT)
         ELSE IF (ICRSYS.EQ.6) THEN

! Note that this model assumes that the HGA points at the Earth and gimb
! angles are fixed.  Therefore, the position of the tracking point in th
! body-fixed system is constant.   Also, note that since the rotation ab
! the s/c Yaxis is ignored, the location of the HGA in inertial space is
! necessarily correct. However, with Doppler measements this approximati
! is suitable.
           CALL TRKMO(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,            &
     &                XYOF22,AA,II,.FALSE.,NM,ATROT,LL(KLANTC),         &
     &                II(KANCUT),ISATID,AA(KOFDRV),LOFFA2)
! THERE WOULD NOT BE A SECOND CALL, BUT IT WOULD LOOK LIKE THIS
!          CALL TRKMO(AA(JXSS),VSM2,URNM2,OFFSET,FSECS2,RA,WORK,

!        ELSE IF (ICRSYS.EQ.7) THEN
         ELSE IF (ICRSYS.EQ.7 .OR. ICRSYS.EQ.8 ) THEN

! The following TDRSS routine only works with single link offset
          CALL TRKTDS(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,      &
     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,    &
     &                AA(KOFDRV),LOFFA2)

         ELSE IF (ICRSYS.EQ.9) THEN

!        ....magnetically stabilized satellite
!         if( kentry .le. iprint ) then
!            write(6,*) 'robsum: user sat call trkmag '
!         endif
          CALL TRKMAG(XSM2,VSM2,URNM2,OFFSET,FSECS2,                    &
     &                RA,WORK,AA,II,II(KISATN-1+ISET2),.FALSE.,NM,      &
     &                ATROT,LL(KLANTC),II(KANCUT),AA(KOFDRV),LOFFA2)

         ELSE IF (ICRSYS.EQ.10) THEN

       CALL TRKGFO(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,         &
     &             II(KISATN-I+ISET2),.FALSE.,LOFFA2,AA(KOFDRV),        &
     &             .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT))

         ELSE IF (ICRSYS.EQ.11) THEN

       CALL TRKTRM(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,ISATID,  &
     &             LL(KLANTC),II(KANCUT),NM,AA(KOFDRV),LOFFA2)

         ELSE IF (ICRSYS.EQ.12) THEN

       CALL TRKEUV(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,ISATID,  &
     &             LL(KLANTC),II(KANCUT),NM,AA(KOFDRV),LOFFA2)

         ELSE IF (ICRSYS.EQ.13) THEN
! pd %%%%%% the unit x vector is in the direction of forward velocity
!          CALL TRKVCL(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,      &
!     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID)
           CALL TRKICE(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,     &
     &                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,    &
     &                .FALSE.,AA(KPHC),.FALSE.,L2PWND,W2PU,ISEQ,IDNW,   &
     &                ANTTAB,LOFFA2,AA(KOFDRV))
! IDN JUST ABOVE IS OK. THIS CALL IS FOR THE HIGH SAT OF A SAT SAT LINK
! ICESAT WOULD NOT BE IN THIS POSITION. ALL TOPEX TRACKING TYPES ARE
! LOW
! CHANGED FOR GRACE
!          CALL TRKICE(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,
!     1                .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,
!     2                .FALSE.,AA(KPHC),.FALSE.,L2PWND,W2PU,ISEQ,IDNW,
!     3                ANTTAB,LOFFA2,AA(KOFDRV))

         ELSE IF (ICRSYS.EQ.15) THEN

       CALL TRKCR2(XSM2,VSM2,URNM2,OFFSET,FSECS2,RA,WORK,AA,II,         &
     &             II(KISATN-I+ISET2),.FALSE.,LOFFA2,AA(KOFDRV),        &
     &             .FALSE.,NM,ATROT,LL(KLANTC),II(KANCUT),AA(KSTAIN),   &
     &             INDSTA,AA(KPHC),ANTTAB,LDORANT)

         ELSE IF (ICRSYS.EQ.18) THEN
      CALL TRKHY2(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,.FALSE.,    &
     &             NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,AA(KSTAIN),    &
     &             INDSTA,AA(KPHC),ANTTAB,LDORANT,AA(KOFDRV),LOFFA2)

         ELSE IF (ICRSYS.EQ.19) THEN
         IF(MTYPE.EQ.51)write(6,*)' dbg2ROBSUM ICRSYS',ICRSYS,NSARAL
         LSRLLRA=(MTYPE.EQ.51.AND.NSARAL.GT.0) !.AND.OPTION SELECTED!
         write(6,*)'robsum call trksrl2, lsrllra',LSRLLRA
      CALL TRKSRL(XSM,VSM,URNM2,OFFSET,FSECSA,RA,WORK,AA,II,.FALSE.,    &
     &        NM,ATROT,LL(KLANTC),II(KANCUT),ISATID,AA(KSTAIN),         &
     &        INDSTA,AA(KPHC),ANTTAB,LDORANT,AA(KOFDRV),LOFFA2,LSRLLRA)

         ELSE
           GO TO 9000

         ENDIF ! ICRSYS

      endif !.not. lpresc(2) .or. LOFFA2

 3930   CONTINUE


      ENDIF  ! ?? LRANGE

      !write(6,'(A)') 'robsum: aft 3930 '

      IF(.NOT.LRANGE) CALL TRAKRR(XSM2,VSM2,RNM,URNM,UHAT,RELV,OFFSET,  &
     &                     ELEVSC,FSECS2,U,RA,WORK,LASER,ICRSYS,AA,II,  &
     &                     LPTS,XYOF22,ISET2,URNM2,ICOOR2,INDSTA,       &
     &                     TPMES1,LTPMS1,II(KISATN-1+ISET2),MJDSIN,     &
     &                     FSECIN,ATROT,LL,XYOF32)
!
!!!!TEST
!        if( indsta(1).ne.0) then
!            print*, 'dbg STATION ----- SATELLITE',kentry
!        else
!            print*, 'dbg SATELLITE ----- SATELLITE: S2',kentry
!        endif
!        print*, 'dbg sacle ', scale(1)
!!!!TEST

      !write(6,'(A,1x,E15.7)') &
      !      'robsum: before 3940 loop octp2(1) ', octp2(1)

      DO 3940 N=1,NM

! NOTE: THE MINUS SIGN USED IS TO ACCOUNT FOR THE FACT THAT THIS OFFSET
! IS BEING COMPUTED FOR THE SATELLITE FROM WHICH THE UNIT VECTOR URNM
! IS POINTING. IN THIS CASE WHEN THE DOT PRODUCT BETWEEN THE SATELLITE
! OFFSET AND URNM GIVES +1, THE COMPUTED RANGE SHOULD BE SHORTENED

!cc      OCTP2(N)=OCTP2(N)+WORK(N)*SCALE(N)

      OCTP2(N)=OCTP2(N) - WORK(N)*SCALE(N)

      !if( n == 1) then
      !write(6,'(A,1x,I6,3(1x,E24.16))') &
      !      'robsum: 3940: n,fsecsa(n), work,scale: ', &
      !                     n,fsecsa(n), work(n),scale(n)
      !endif

 3940 ENDDO

      !write(6,'(A,1x,E24.16)') &
      !  'robsum: aft 3940 loop octp2(1) ', octp2(1)

      !write(6,'(A, 3(1x,E24.16))') &
      !           'robsum: before 4000 octp2(1) ', octp2(1)

 4000 CONTINUE

      !write(6,'(A,(2(1x,E15.7)))') &
      !      'robsum: aft 4000 sumcor  6.1 ', (AA(N),N=1,2) !N1,N1+2)

!  CORRECT FOR PHASE WINDUP IF NECESSARY

      IF(L1PWND.AND.L2PWND) THEN

         DO IQP=1,NM
         IF(L2SATL.AND..NOT.L2LOW) THEN
            VRNM2(1)=-URNM2(IQP,1)
            VRNM2(2)=-URNM2(IQP,2)
            VRNM2(3)=-URNM2(IQP,3)
         ELSE
            VRNM2(1)=URNM2(IQP,1)
            VRNM2(2)=URNM2(IQP,2)
            VRNM2(3)=URNM2(IQP,3)
         ENDIF

         DOT=-VRNM2(1)*W2PU(IQP,1,1)-VRNM2(2)*W2PU(IQP,2,1)             &
     &       -VRNM2(3)*W2PU(IQP,3,1)
         DWR(1)=W2PU(IQP,1,1)+DOT*VRNM2(1)                              &
     &             -VRNM2(2)*W2PU(IQP,3,2)                              &
     &             +VRNM2(3)*W2PU(IQP,2,2)
         DWR(2)=W2PU(IQP,2,1)+DOT*VRNM2(2)                              &
     &             -VRNM2(3)*W2PU(IQP,1,2)                              &
     &             +VRNM2(1)*W2PU(IQP,3,2)
         DWR(3)=W2PU(IQP,3,1)+DOT*VRNM2(3)                              &
     &             -VRNM2(1)*W2PU(IQP,2,2)                              &
     &             +VRNM2(2)*W2PU(IQP,1,2)

!
         DOT=-VRNM2(1)*W1PU(IQP,1,1)-VRNM2(2)*W1PU(IQP,2,1)             &
     &       -VRNM2(3)*W1PU(IQP,3,1)
         DWT(1)=W1PU(IQP,1,1)+DOT*VRNM2(1)                              &
     &             +VRNM2(2)*W1PU(IQP,3,2)                              &
     &             -VRNM2(3)*W1PU(IQP,2,2)
         DWT(2)=W1PU(IQP,2,1)+DOT*VRNM2(2)                              &
     &             +VRNM2(3)*W1PU(IQP,1,2)                              &
     &             -VRNM2(1)*W1PU(IQP,3,2)
         DWT(3)=W1PU(IQP,3,1)+DOT*VRNM2(3)                              &
     &             +VRNM2(1)*W1PU(IQP,2,2)                              &
     &             -VRNM2(2)*W1PU(IQP,1,2)
         DTXDR(1)=DWT(2)*DWR(3)-DWT(3)*DWR(2)
         DTXDR(2)=DWT(3)*DWR(1)-DWT(1)*DWR(3)
         DTXDR(3)=DWT(1)*DWR(2)-DWT(2)*DWR(1)
         SIGNP=-VRNM2(1)*DTXDR(1)-VRNM2(2)*DTXDR(2)                     &
     &         -VRNM2(3)*DTXDR(3)
         IF(SIGNP.GT.0.D0) THEN
           SIGNP=1.D0
         ELSE
           SIGNP=-1.D0
         ENDIF
         RDR=SQRT(DWR(1)*DWR(1)+DWR(2)*DWR(2)+DWR(3)*DWR(3))
         RDT=SQRT(DWT(1)*DWT(1)+DWT(2)*DWT(2)+DWT(3)*DWT(3))
         DOT=DWR(1)*DWT(1)+DWR(2)*DWT(2)+DWR(3)*DWT(3)
         ARGQ=DOT/(RDR*RDT)
         IF(ARGQ.GT. 1.D0) ARGQ= 0.999999999999999D0
         IF(ARGQ.LT.-1.D0) ARGQ=-0.999999999999999D0
         DPHIS=SIGNP*ACOS(ARGQ)
         DPHILP=PYUSQ(ISEQ)
         XN=(DPHILP-DPHIS)/TWOPI
         JSIGN=1
         IF(XN.LT.0.D0) JSIGN=-1
         XN=ABS(XN)
         NN=XN
         XNNN=XN-DBLE(NN)
         IF(XNNN.GT..5D0) NN=NN+1
         NN=JSIGN*NN
         XN=DBLE(NN)
         DPHIL=DPHIS+XN*TWOPI
         OCTPC(IQP)=OCTPC(IQP)+SCALE(IQP)*DPHIL*WVL3/TWOPI
         PYUSQ(ISEQ)=DPHIL
         ENDDO

      ENDIF !  L1PWND.AND.L2PWND

!  END PHASE WINDUP CORRECTION


      !write(6,'(A,3x,L1)') 'robsum: after 4000 lrange ', lrange


      IF(.NOT.LRANGE) GO TO 5500

      !write(6,*) 'robsum: after 4000 lpre(8),lpresc(9) ',&
      !                               lpre(8),lpresc(9)
      !write(6,*) 'robsum: KFRQSA, JSADLY(1:2) ', &
      !                    KFRQSA, JSADLY(1:2)

      IF(LPRE(8).AND.LPRESC(9)) GO TO 4500
      IF(JSADLY(1).LE.KFRQSA) GO TO 4300

! Compute transponder delay corrections for first sat in a link

      CALL DELAY(SATDLY,1,RRNM,NM,OCDLYS)

      !write(6,*) 'robsum: aft call delay sat1 dly  OCDLYS(1) ', &
      !                                             OCDLYS(1)

 4300 IF(JSADLY(2).LE.KFRQSA) GO TO 4500

! Compute transponder delay correction for second sat in a link

      CALL DELAY(SATDL2,1,RRNM,NM,OCDLS2)
      !write(6,*) 'robsum: aft call delay sat2 dly  OCDLS2(1) ', &
      !                                             OCDLS2(1)

 4500 CONTINUE

      !write(6,'(A,(2(1x,E15.7)))') &
      !  'robsum: aft 4500 sumcor  8 ', (AA(N),N=1,2) !N1,N2)  ! N1+2)

      !write(6,*) 'robsum: after 4500 lpre(8),lpresc(8) ', &
      !                               lpre(8),lpresc(8)

      IF(LPRE(8).AND.LPRESC(8)) GO TO 5000
!
!********** STATION DELAY SECTION ************************************
! *** NOTE BY DDR 1/13/88  THE NEXT 4 EXECUTABLE LINES OF CODE
! WERE COMMENTED OUT.
!
! STATION DELAY IS BEING USED FOR RELATIVITY CORRECTIONS
!
!     IF(JSTDLY(1).LE.KFRQST) GO TO 4800
! Compute transponder delay correction for first station in a link
!     CALL DELAY(STADLY,1,RRNM,NM,OCDLYG)
!4800 IF(JSTDLY(2).LE.KFRQST) GO TO 5000
! Compute transponder delay correction for second station in a link
!     CALL DELAY(STADL2,1,RRNM,NM,OCDLG2)
!********** END OF STATION DELAY SECTION *****************************

      !write(6,*) 'robsum: after 4500 lgrele ', lgrele

      IF(ITBDGM.EQ.ICBDGM.AND..NOT.LGRELE) GO TO 4900

! Compute relativistic corrections to range measurement

!      write(6,*) 'robsum: compute relativistic correction '

!      write(6,*) 'robsum: L2SATL ', L2SATL
!      write(6,*) 'robsum: LBPCLK ', LBPCLK

      XSIGN=ONE
      IF(JJLNK.NE.0) XSIGN=-ONE
      IF(L2SATL) XSIGN=ZERO

!      write(6,*) 'robsum: XSIGN  ',  XSIGN

      GRLSN=VLIGHT
      CETSN=GRLSN*XSIGN

!      write(6,*) 'robsum: GRLSN = ', GRLSN
!      write(6,*) 'robsum: xsign,  CETSN = ', xsign, CETSN
!      write(6,*) 'robsum: GRELLT(1) ',  GRELLT(1)
!      write(6,*) 'robsum: GRELLT(1)*GRLSN*SCALE(1) ', &
!                          GRELLT(1)*GRLSN*SCALE(1)
!      write(6,*) 'robsum: DELCET(1) ',  DELCET(1)
!      write(6,*) 'robsum: DELCET(1)*CETSN*SCALE(1) ', &
!                          DELCET(1)*CETSN*SCALE(1)

      DO 4850 N=1,NM
      OCDLYG(N)=OCDLYG(N)+GRELLT(N)*GRLSN*SCALE(N)
      OCDLYG(N)=OCDLYG(N)+DELCET(N)*CETSN*SCALE(N)
      IF(LBPCLK) OCDLYG(N)=OCDLYG(N)-AA(KRLRNG-1+N)*CETSN*SCALE(N)
 4850 END DO
 4900 CONTINUE

!      write(6,*) 'robsum: sumcor  9 ', (AA(N),N=N1,N1+2)
!      write(6,*) 'robsum: after 4900 sta1 delay ocdlyg ', ocdlyg(1)
!
! APPLY RELATIVISTIC S/C PERIODIC AND SECULAR CLOCK DRIFT CORRECTION
! FOR NOW, THIS CORRECTION DOES NOT WORK FOR INTERPLANETARY RUNS.
! This correction is not link dependent and is made on the entire
! measurement.  Therefore, once it is applied LRLCLK is set to FALSE
! to not apply it to other links of this observation.

      !write(6,'(A,3x,L1)') 'robsum:bef 4950loop LRLCLK ', LRLCLK

      IF( (ITBDGM.EQ.ICBDGM).AND.LRLCLK.AND.MTYPE.LT.80 ) THEN

           XSIGN=ONE
           IF(LNEG) XSIGN=-ONE
           IF((MTYPE.EQ.39).OR.(MTYPE.EQ.40)) XSIGN=-XSIGN

           !write(6,*) 'robsum: XSIGN  ', XSIGN
           !write(6,*) 'robsum: RLRCOR(1) ', RLRCOR(1)

           DO  N=1,NM
               OCDLYG(N)=OCDLYG(N)+RLRCOR(N)*XSIGN
           ENDDO
           LRLCLK=.FALSE.

      ENDIF !  (ITBDGM.EQ.ICBDGM).AND.LRLCLK.AND.MTYPE.LT.80


!      write(6,*) 'robsum: sumcor  10 ', (AA(N),N=N1,N1+2)
!      write(6,*) 'robsum: after 4950 sta1 delay ocdlyg ', ocdlyg(1)
!      write(6,*) 'robsum:aft 4950loop LRLCLK ', LRLCLK

 5000 CONTINUE

       !write(6,'(A)') 'robsum: aft 5000   '

        DEALLOCATE(RQ1,RQ2,RQ3,RQ4)

!      write(6,*) 'robsum: sumcor  11 ', (AA(N),N=N1,N1+2)
!      write(6,*) 'robsum: lrange: ',lrange

      IF(LRANGE) GO TO 6000
 5500 CONTINUE
        DEALLOCATE(RQ1,RQ2,RQ3,RQ4)
!
! SUM COMPUTED RANGE RATES
!
      !write(6,*) &
      !'robsum:bef 5800 rrate obsc(1), rrnm(1), scale(1) ', &
      !                       obsc(1), rrnm(1), scale(1)

      DO 5800 N=1,NM
      OBSC(N)=OBSC(N)+RRNM(N)*SCALE(N)
 5800 END DO

      !write(6,*) &
      !'robsum:aft 5800 rrate obsc(1), rrnm(1), scale(1) ', &
      !                       obsc(1), rrnm(1), scale(1)

      LXATTD=LXAH

      RETURN
 6000 CONTINUE

! SUM COMPUTED RANGES

      FCT=1.D0
      IF(MTYPE.EQ.36)FCT=VLIGHT

!      write(6,*)'robsum, sum computed ranges'
!       write(6,*) &
!       'robsum: bef   6800 range obsc(1), rnm(1), scale(1)'!, &
!       write(6,*)   obsc(1), rnm(1), scale(1)

!       write(52,*)'mtype',MTYPE
!       write(52,*)'N,OBSC(N),RNM(N)'
!      write(6,*)'OBSC(N),RNM(N),SCALE(N)'
      DO 6800 N=1,NM
!       write(52,*)N,OBSC(N),RNM(N)
!      write(6,*)OBSC(N),RNM(N),SCALE(N)
      OBSC(N)=OBSC(N)+RNM(N)*SCALE(N)/FCT

!     write(6,*)' dbg robsum range ',rl1(n)
!     RL1(N)=RL1(N)+RDS1LC(N)
!     write(6,*)' dbg robsum corrected range ',rl1(n)

 6800 END DO

!      write(6,*) 'robsum: INDRAM ', INDRAM

      IF(INDRAM.GT.0) THEN
      DO 6810 N=1,NM
      RL1(N)=RL1(N)+RNM(N)
 6810 END DO
      ENDIF

!      write(6,*) &
!      'robsum: after 6800 range obsc(1), rnm(1), scale(1)   ', &
!                                obsc(1), rnm(1), scale(1)
!
! SUM COMPUTED TDRS LONG LINK ONLY IF 1-WAY TDRSS OBSERVATION
!
! make sure that link one is added in without scaling of 1+pilotratio
!
      IF(LAPP) THEN
      SN=ONE
      IF(LNEG) SN=-ONE
      SN=SN*ILCORR(ILINK)
!      write(6,*) &
!      'robsum: bef   6900 obstrl(1), rnm(1)  ', &
!                          obstrl(1), rnm(1)
      DO 6900 N=1,NM
      OBSTRL(N)=OBSTRL(N)+RNM(N)*SN
 6900 END DO
!      write(6,*) &
!      'robsum: after 6900 obstrl(1), rnm(1)  ', &
!                          obstrl(1), rnm(1)
      ENDIF
!
      LXATTD=LXAH
      IF(.NOT.LTDRIV) RETURN
      DO 7800 N=1,NM
      OBRATE(N)=OBRATE(N)+RRNM(N)*SCALE(N)
 7800 END DO
!      write(6,*) &
!      'robsum: after 7800 obrate(1), rrnm(1)  ', &
!                          obrate(1), rrnm(1)
      RETURN
 9000 CONTINUE
      WRITE(IOUT6,90000)
      STOP 16
 9100 CONTINUE
      WRITE(IOUT6,91000)
      STOP 16
 9200 CONTINUE
      WRITE(IOUT6,92000)
      STOP 16
90000 FORMAT(' ROBSUM: *** ERROR *** CHECK OFFSET CARD.'/               &
     &  10x,' THE COORDINATE SYSTEM INDICATED IS INCORRECT.')
91000 FORMAT(' ROBSUM: *** ERROR *** CHECK OFFSET CARD.'/               &
     &  'EXTERNAL ATTITUDE REQUESTED BUT NO ANTENNA NUMBER FOUND')
92000 FORMAT(' ROBSUM: *** ERROR *** CHECK OFFSET CARD.'/               &
     &  'EXTERNAL ATTITUDE REQUESTED BUT NO EXTATT SUPPLIED ')
      END
