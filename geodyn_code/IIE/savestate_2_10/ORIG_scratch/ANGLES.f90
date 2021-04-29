!$ANGLES
      SUBROUTINE ANGLES(AA    ,II    ,LL    ,STAINF,N3    ,NEQN  ,      &
     &    IPTFMG,ILNFMG,NFMG  ,INDSTA,INDSAT,INDSET,INPSTA,INPSAT,      &
     &    LELEVS )
!********1*********2*********3*********4*********5*********6*********7**
! ANGLES           00/00/00            0000.0    PGMR - ?
!                      3/90            90??.0    PGMR - J. MCCARTHY
!
! FUNCTION:        PROCESS ANGLE MEASUREMENTS
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES.
!   II      I/O   A    DYNAMIC ARRAY FOR INTEGER VARIABLES
!   LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL VARIABLES
!   STAINF   I    A    Station information array
!   N3            A
!   NEQN     I    A    NUMBER OF FORCE MODEL EQUATIONS IN EACH SET
!   IPTFMG   I    A    POINTER TO PMPF ARRAY FOR EACH FORCE MODEL GROUP
!                      FOR EACH SATELLITE
!   ILNFMG   I    A    LENGTH OF EACH FORCE MODEL GROUP
!   NFMG     I    A    NUMBER OF FORCE MODEL GROUPS FOR EACH SATELLITE
!   INDSTA   O    A    FOR A GIVEN INTERNAL STA NO (1,2,3) INDSTA
!                      GIVES THE ACTUAL LOCATION IN THE STATION
!                      RELATED ARRAYS FOR THIS STATION.
!                      (EG.   STA NO=ISTANO(INDSTA(1,2,OR 3))
!   INDSAT   O    A    FOR A GIVEN INTERNAL SAT NO (1,2,3) INDSAT
!                      GIVES THE ACTUAL LOCATION IN THE SATELLITE
!                      RELATED ARRAYS FOR THIS SATELLITE.
!                      (EG.   SAT ID=ISATNO(INDSAT(1,2,OR 3))
!   INDSET   O    A    FOR A GIVEN INTERNAL SATELLITE NUMBER INDSET TELL
!                      WHICH SAT SET THAT THE GIVEN SAT BELONGS TO.
!   INPSTA   O    A    POINTER ARRAY THAT RELATES THE INTERNAL STATION
!                      NUMBER (1,2,3) TO THE STATIONS DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISTAP=INPSTA(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE STATIONS DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL STATION NO. 1)
!   INPSAT   O    A    POINTER ARRAY THAT RELATES THE INTERNAL SATELLITE
!                      NUMBER (1,2,3) TO THE SATELLITES DEFINED IN THE
!                      DATA HEADER RECORDS (N=1,2,3). (EG.
!                      ISATP=INPSAT(1) WOULD TELL WHICH OF THE THREE
!                      POSSIBLE SATELLITES DEFINED IN THE DATA HEADER
!                      RECORDS PERTAINED TO INTERNAL SATELLITE NO. 1)
!   LELEVS  I/O   A    INDICATES WHICH ELEVATION ANGLES NEED TO BE
!                      COMPUTED FOR THE THREE POSSIBLE STATION SATELLITE
!                      CONFIGURATIONS DEFINED IN THE BLOCK HEADER
!                      RECORDS
!
! COMMENTS:
!   05/20/2011
!   UPDATED FOR PAIRED MEASUREMENT TYPEE BY: SARA CORBITT-SANDERS
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( ZERO = 0.D0 )
!
      COMMON/CANGLE/KDRCOS,KDOTEN,KEZ,KREZ,KCOSE,KELCOR
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
      COMMON/CBIASL/LEBIAS(4),LMBIAS(2),LBSCAL(1)  ,LTBIAS(1),          &
     &              LBTROP(3),LBIONO(3),LCLSTA(4,2),LCLSAT(4,2),        &
     &              LKLSTS(2,2),LTRPZE(2,2),LTRPGR(2,4),LBSBIA(1),      &
     &              LNBIAS
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
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
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
      COMMON/COBGLO/MABIAS,MCBIAS,MPVECT,MINTPL,MPARAM,MSTA,MOBBUF,     &
     &       MNPITR,MWIDTH,MSIZE ,MPART ,MBUF  ,NXCOBG
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/COBREL/KDELCT(6,8)
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
      COMMON/CORA05/KOBTYP,KDSCRP,KGMEAN,KGLRMS,KWGMEA,KWGRMS,KTYMEA,   &
     &       KTYRMS,KWTMTY,KWTYRM,KTMEAN,KTRMS ,KWMEAN,KWTRMS,KWTRND,   &
     &       KPRVRT,KEBSTT,KVLOPT,NXCA05
      COMMON/CORA06/KPRMV ,KPNAME,KPRMV0,KPRMVC,KPRMVP,KPRMSG,KPARVR,   &
     &              KPRML0,KPDLTA,KSATCV,KSTACV,KPOLCV,KTIDCV,KSUM1 ,   &
     &              KSUM2 ,KGPNRA,KGPNRM,KPRSG0,KCONDN,KVELCV,          &
     &              KSL2CV,KSH2CV,KTIEOU,KPRMDF,NXCA06
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
      COMMON/CORL03/KLVELA,KLSTL2,KLSTH2,NXCL03
      COMMON/CORL04/KLEDIT,KLBEDT,KLEDT1,KLEDT2,KNSCID,KEXTOF,KLSDAT,   &
     &              KLRDED,KLAVOI,KLFEDT,KLANTC,NXCL04
      COMMON/CORL07/KLPTS,NXCL07
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
      COMMON/CSTA/RLAT,COSLAT,SINLAT,RLON,COSLON,SINLON,HEIGHT,         &
     &   TMOUNT,DISP,WAVREC,WAVXMT,ELCUT,PLATNO,SITNOL,SDTLRA,ANTTNO
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/RAMPS/TRINT(1000,2,2),FRINT(1000,2,2),TRAMP(1000,2),       &
     &             FRAMP(1000),FDRAMP(1000),RSCALX(1000),SCALX(1000),   &
     &             XINTS(1000),XINTB(1000),XINTS2(1000),RSCALY(1000)
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/LPAIR/LSECOND
!
      DIMENSION AA(1),II(1),LL(1),INDSTA(3),INDSAT(3),INDSET(3),        &
     &   LELEVS(3),N3(MSETA),NEQN(MSETA),ILNFMG(MAXFMG,MSATA),          &
     &   NFMG(MSATA),IPTFMG(MAXFMG,MSATA),STAINF(NSTAIN,1),             &
     &   INPSTA(3),INPSAT(3)
      DIMENSION DUM(3),DUM2(1,3)
      DIMENSION NCALL(14),NINTPL(3),KPXPF(3),STANFO(25)
      LOGICAL, DIMENSION(:), ALLOCATABLE :: LEDIT_EXTRA
!
      EQUIVALENCE (KPXPF(1),KPXPFM),(STANFO(1),RLAT)
      EQUIVALENCE (KPXPF(1),KPXPFM)
! LABELS             1300 1000 1700 1900
      DATA NCALL/       2,   1,   3,   4,                               &
     &              5,   6,   6,   6,   6,                              &
     &              1,   1,   1,   1,   1/
! LABELS         2100 2300 2300 2300 2300
! LABELS         1000 1000 1000 1000 1000
      DATA SCALE/1.0D0/
      DATA kentry/0/
      DATA iprint/-1/
      DATA ONETEN/0.1D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      DO I=1,NM
        RSCALX(I)=SCALE
      ENDDO
!     IF(NSTAIN.GT.16) THEN
!      WRITE(6,*) 'STANFO DIMENSION INSUFFICIENT IN SUBROUTINE ANGLES'
!      STOP
!     ENDIF
      IF(LLOCRI) CALL ANGHD(INDSTA,INDSAT)
      JFSECN=KFSECS(1,1)
      JFSECM=KFSECS(2,1)
      JFSECK=KFSECS(3,1)
      JDLCTN=KDELCT(1,1)
      JDLCTM=KDELCT(2,1)
      JDLCTK=KDELCT(3,1)
      JXMN=KXMN(1,1)
      JXMK=KXMN(2,1)
      JENVN=KENVN(1,1)
      JENVK=KENVN(2,1)
      ICALL=(MTYPE-11)/2
      MCALL=NCALL(ICALL)
      IOBS=KOBS
      ISMCOR=KSMCOR
      IOBSIG=KOBSIG
      IEDSIG=KEDSIG
      ISTATS=JSTATS
      ISTAF =1
      ISTA  =INDSTA(ISTAF )
      ISTAP =INPSTA(ISTAF )
      DO 300 I=1,NSTAIN
      STANFO(I)=STAINF(I,ISTA)
  300 END DO
      WAVLEN=VLIGHT/BLKDAT(3,1)
      IF(WAVREC.GT.ZERO) WAVLEN=WAVREC
      NTYPE=MTYPE
      L2ND=.FALSE.
      LNRATE=.NOT.LTDRIV
      IEXIT=1
      NCLEAR=NM*NADJST
      DO 9000 ILM=1,2
      IF(ILM.eq.2) LSECOND=.TRUE.
! CLEAR TIME DERIVATIVE ARRAY
      CALL CLEARA(AA(KTPRTL),NM)
      IF(LNADJ) GO TO 500
! CLEAR PARTIAL DERIVATIVE ARRAY
      CALL CLEARA(AA(KPMPA),NCLEAR)
  500 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 91000,ILM,ICALL,MCALL,IEXIT
!1000 FORMAT(' ** ANGLES **  ILM,ICALL,MCALL =',3I12)
!***** END DEBUG *****
      GO TO (1000,1300,1700,1900,2100,2300),MCALL
 1000 CONTINUE
      JLINE6=ILINE6+3
      IF(JLINE6.LE.MLINE6) GO TO 1050
      IPAGE6=IPAGE6+1
      JLINE6=4
      WRITE(IOUT6,10000) IPAGE6
 1050 CONTINUE
      WRITE(IOUT6,20000) MTYPE
      ILINE6=JLINE6
      GO TO 8000
 1300 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 91300,IEXIT
!1300 FORMAT(' ** ANGLES **  CALL RADEC  IEXIT =',I12)
!***** END DEBUG *****
      CALL RADEC (MJDSBL,AA(JFSECN)   ,AA(JFSECM)   ,AA(JFSECK)   ,     &
     &     AA(KOBS  )   ,AA(KDTIME)   ,AA(KSMCOR)   ,AA(KSMCR2)   ,     &
     &     AA(KXTN  )   ,AA(KXSM  )   ,AA(KXTK  )   ,                   &
     &     AA(KVSM  )   ,AA(JXMN  )   ,AA(JXMK  )   ,                   &
     &     AA(KRNM  )   ,AA(KRKM  )   ,AA(KURNM )   ,AA(KURKM )   ,     &
     &     AA(KXY2  )   ,AA(KUHAT )   ,AA(JENVN )   ,AA(KZT   )   ,     &
     &     AA(KCOSE )   ,AA(KELEVS)   ,AA(KRELV )   ,AA(KRRNM )   ,     &
     &     AA(KRESID)   ,AA(KTPRTL)   ,AA(KPMPXI)   ,II(KNMP  )   ,     &
     &     II(KINDP )   ,AA(KRPOLE)   ,AA(KPXPOL)   ,NINTPL       ,     &
     &     MINTPL       ,AA(KS0   )   ,AA(KS1   )   ,AA(KXTP  )   ,     &
     &     AA(KPXSXP)   ,AA(KCOSTH)   ,AA(KSINTH)   ,AA(KTHETG)   ,     &
     &     AA(KWORK )   ,INDSAT       ,SIGNL        ,NM           ,     &
     &     LNRATE,LPOLAD,LIGHT ,L2ND  ,IEXIT        ,INDSTA       ,     &
     &     AA(JDLCTN)   ,AA(JDLCTM)   ,AA(JDLCTK)   ,LL(KLPTS)    ,     &
     &     LPSBL        ,LACC         ,                                 &
     &     MTYPE  ,INDSET,AA(KXDPOL),LNUTAD,AA   ,II    ,LL )
      GO TO 5000
 1700 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 91700
!1700 FORMAT(' ** ANGLES **  CALL AZELEV')
!***** END DEBUG *****
      nm1 = MAX( nm, 1 )
      CALL AZELEV(MJDSBL,AA(JFSECN)   ,AA(JFSECM)   ,AA(IOBS  )   ,     &
     &     AA(ISMCOR)   ,AA(JENVN )   ,AA(KXTN  )   ,AA(KXSM  )   ,     &
     &     AA(JXMN  )   ,AA(KVSM  )   ,                                 &
     &     AA(KRNM  )   ,AA(KURNM )   ,AA(KRRNM )   ,AA(KUHAT )   ,     &
     &     AA(KDRCOS)   ,AA(KEZ   )   ,AA(KRESID)   ,AA(KPMPXI)   ,     &
     &     AA(KPMPXE)   ,AA(KREZ  )   ,AA(KTPRTL)   ,II(KNMP  )   ,     &
     &     II(KINDP )   ,AA(KRPOLE)   ,AA(KPXPOL)   ,NINTPL       ,     &
     &     MINTPL       ,AA(KS0   )   ,AA(KS1   )   ,AA(KXTP  )   ,     &
     &     AA(KPXSXP)   ,AA(KCOSTH)   ,AA(KSINTH)   ,AA(KRELV )   ,     &
     &     AA(KDOTEN)   ,AA(KELEVS)   ,AA(KWORK )   ,                   &
     &     INDSAT,SIGNL ,NM    ,       nm1,                             &
     &     LPOLAD,LIGHT ,LNRATE,L2ND         ,                          &
     &     INDSTA       ,AA(JDLCTN)   ,AA(JDLCTM)   ,AA(JDLCTK)   ,     &
     &     LL(KLPTS)    ,LPSBL        ,LACC         ,                   &
     &     MTYPE  ,INDSET,AA(KXDPOL),LASER,LNUTAD,AA    ,II    ,LL    )
      GO TO 5000
!jtw add laser to call azelev
 1900 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 91900
!1900 FORMAT(' ** ANGLES **  CALL XYANGL')
!***** END DEBUG *****
      MOUNT = ( STANFO(8) + 0.1D0)
      IF(MOUNT.LT.1) MOUNT=2
      IY=MAX(MIN(MOUNT,2),1)

!
!     ....set the mount type indicator based on the flag in the
!     ....data -- in block header record word 4 for station 1
!
      iy2 = INT( blkdat(4,1) + oneten ) + 1
!
!
!
      if( iy .ne. iy2 ) then
         if( kentry .lt. iprint )write(6,*)                             &
     &   'angles: XY angle mount on data differs from mount on station '
!
!        ....The default mount type in the STAINF array is 3 (az-el).
!        ....If the mount type from STAINF is not 3, an INSTRMNT card
!        ....was used to specify the mount type for the station.
!        ....This mount type will override the mount type specified on
!        ....the data.
!
         if( mount .eq. 3 ) then
            if( kentry .lt. iprint )write(6,*)                          &
     &      'angles: XY angle mount type taken from data '
         else
            if( kentry .lt. iprint )write(6,*)                          &
     &      'angles: mount type on INSTRMNT card is ', mount
            if( kentry .lt. iprint )write(6,*)                          &
     &      'angles: INSTRMNT card mount type overrides',               &
     &      ' mount type on data '
!
            iy2 = iy
!
         endif
!
      endif
!
!***** BEGIN DEBUG *****
!      write(6,8881) stanfo(8), mount, iy, iy2, tmount
 8881 format(' angles: stanfo(8), mount, iy,iy2, tmount ',              &
     &       f12.2, 3i12,1x,f12.2)
!cc      write(6,93333) NM,MINTPL,MINTIM,MSETA,MAXFMG,MSATA
!cc93333 FORMAT(' angles: NM    MINTPL   MINTIM  MSETA  MAXFMG  MSATA '/
!cc     .   1X,6I8)
!***** END DEBUG *****
!
      nm1 = MAX( nm, 1 )
      CALL XYANGL(MJDSBL,AA(JFSECN)   ,AA(JFSECM)   ,AA(IOBS  )   ,     &
     &     AA(ISMCOR)   ,AA(JENVN )   ,AA(KXTN  )   ,AA(KXSM  )   ,     &
     &     AA(KVSM  )   ,AA(JXMN  )   ,AA(KRNM  )   ,AA(KURNM )   ,     &
     &     AA(KUHAT )   ,AA(KDRCOS)   ,AA(KDOTEN)   ,                   &
     &     AA(KCOSE )   ,AA(KELCOR)   ,AA(KEZ   )   ,AA(KRESID)   ,     &
     &     AA(KTPRTL)   ,AA(KPMPXI)   ,AA(KPMPXE)   ,II(KNMP  )   ,     &
     &     II(KINDP )   ,AA(KRPOLE)   ,AA(KPXPOL)   ,NINTPL       ,     &
     &     MINTPL       ,AA(KS0   )   ,AA(KS1   )   ,AA(KXTP  )   ,     &
     &     AA(KPXSXP)   ,AA(KCOSTH)   ,AA(KSINTH)   ,AA(KREZ  )   ,     &
     &     AA(KRELV )   ,AA(KRRNM )   ,AA(KWORK )   ,AA(KELEVS)   ,     &
     &     INDSAT       ,NM           ,nm1          ,LNRATE       ,     &
     &     LPOLAD       ,LIGHT        ,L2ND         ,                   &
     &     iy2    ,INDSTA,AA(JDLCTN)   ,AA(JDLCTM)   ,AA(JDLCTK)   ,    &
     &     LL(KLPTS)    ,LPSBL        ,LACC         ,                   &
     &     MTYPE  ,INDSET,AA(KXDPOL),LASER,LNUTAD,AA    ,II    ,LL    )
!>>>>>>>>>>>>>>
!>>>>>>>>>>>>>>
      GO TO 5000
 2100 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 92100
!2100 FORMAT(' ** ANGLES **  CALL LMDCOS')
!***** END DEBUG *****
!>>>>>>>>>>>>>>>>
      nm1 = MAX( nm, 1 )
      CALL LMDCOS(MJDSBL,AA(JFSECN)   ,AA(JFSECM)   ,AA(IOBS  )   ,     &
     &     AA(ISMCOR)   ,AA(JENVN )   ,AA(JXMN  )   ,AA(KXTN  )   ,     &
     &     AA(KXSM  )   ,AA(KVSM  )   ,                                 &
     &     AA(KRNM  )   ,AA(KURNM )   ,AA(KRRNM )   ,AA(KUHAT )   ,     &
     &     AA(KRESID)   ,AA(KTPRTL)   ,AA(KPMPXI)   ,AA(KPMPXE)   ,     &
     &     II(KNMP  )   ,II(KINDP )   ,AA(KRPOLE)   ,AA(KPXPOL)   ,     &
     &     NINTPL       ,MINTPL       ,                                 &
     &     AA(KS0   )   ,AA(KS1   )   ,AA(KXTP  )   ,                   &
     &     AA(KPXSXP)   ,AA(KCOSTH)   ,AA(KSINTH)   ,AA(KDRCOS)   ,     &
     &     AA(KXY2  )   ,AA(KRELV )   ,AA(KDOTEN)   ,AA(KELEVS)   ,     &
     &     INDSAT       ,NM           ,nm1          ,                   &
     &     LNRATE       ,LPOLAD       ,LIGHT        ,ILM          ,     &
     &     INDSTA,                                                      &
     &     AA(JDLCTN)   ,AA(JDLCTM)   ,AA(JDLCTK)   ,                   &
     &     LL(KLPTS)    ,LPSBL        ,LACC         ,                   &
     &     MTYPE  ,INDSET,AA(KXDPOL),LASER,LNUTAD,AA   ,II   ,LL    )
!>>>>>>>>>>>>>>
      GO TO 5000
 2300 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 92300
!2300 FORMAT(' ** ANGLES **  CALL LNDMRK')
!***** END DEBUG *****
      CALL LNDMRK(AA,II,LL)
 5000 CONTINUE
! MAKE SURE MEAS PARTIALS WRT SAT ARE TRUE OF REF
      CALL TDORTR(MJDSBL,AA(JFSECM),AA(KPMPXI),AA(KPMPXI),AA,NM,NM,     &
     &           .TRUE.,.TRUE.,II)
!***** BEGIN DEBUG *****
!     PRINT 95000
!5000 FORMAT(' ** ANGLES **  5000 CONTINUE')
!***** END DEBUG *****
      IF(LNADJ) GO TO 8000
      IF(LNBIAS) GO TO 6000
      CALL BIASP (AA,AA(KPMPA),NDIM1,NDIM2,AA(KOBS),AA(KTPRTL),         &
     &      AA(KOBTIM),AA(KDTIME),AA(KPRML0),ILM,LNPNM,LL(KLAVOI),      &
     &      AA(KOBSC))
 6000 CONTINUE
      ISTAF=1
      ISATT=1
      ICOSTH=KCOSTH
      ISINTH=KSINTH
!***** BEGIN DEBUG *****
!     PRINT 95001,ISTAF,ISATT,ICOSTH,ISINTH
!5001 FORMAT(' ** ANGLES **  ISTAF,ISATT,ICOSTH,ISINTH =',4I12)
!***** END DEBUG *****
! CHAIN MEASUREMENT POSITION PARTIALS WITH FORCE MODEL PARTIALS
      IF(ISATT.LE.0) GO TO 7000
      ISAT=INDSAT(ISATT)
      ISET=INDSET(ISATT)
      IPXPF=KPXPF(ISATT)
!***** BEGIN DEBUG *****
!     PRINT 95002,ISAT,ISET,IPXPF
!5002 FORMAT(' ** ANGLES ** BEFORE CHAINING ISAT,ISET,IPXPF =',3I12)
!***** END DEBUG *****
      NDIMX1=NEQN(ISET)
      NDIMX2=N3(ISET)
      NDIMX3=NM
      IF(LNPNM) GO TO 6500
      NDIMX1=NM
      NDIMX2=NEQN(ISET)
      NDIMX3=N3(ISET)
 6500 CONTINUE
      CALL CHAIN (AA(KPMPXI),NM,3,1,AA(IPXPF),NDIMX1,NDIMX2,NDIMX3,     &
     &    NEQN(ISET),N3(ISET),IPTFMG(1,ISAT),ILNFMG(1,ISAT),NFMG(ISAT), &
     &    AA(KPMPA),NDIM1,NDIM2,LNPNM,.FALSE.,.FALSE.,SCALE,LL(KLAVOI))
 7000 CONTINUE
!***** BEGIN DEBUG *****
!     PRINT 97000
!7000 FORMAT(' ** ANGLES **  7000 CONTINUE')
!***** END DEBUG *****
      IF(.NOT.LSTINR) GO TO 8000
      IF(ISTAF.LE.0) GO TO 8000
! CHAIN AND SUM STATION POSITION AND POLAR MOTION/U.T. PARTIALS
      INTPOL=NINTPL(ISTAF )
      INMP  =(ISTAF-1)*MINTPL
      IINDP =KINDP +INMP
      INMP  =KNMP  +INMP
      IXM   =KXMN  (ISTAF ,1)
      ISTA  =INDSTA(ISTAF )
      LESTA =ISTA.LE.NSTAE
!***** BEGIN DEBUG *****
!     PRINT 97001,ISTA,IXM,LESTA
!7001 FORMAT(' ** ANGLES ** BEFORE PSUM ISTA,IXM,LESTA =',2I12,1X,L1)
!***** END DEBUG *****
      MADDC=NM*3
      MADDC2=MINTIM*12
      JPXSP1=KPXSPA+(ISTAF-1)*NM*18*MXOLSA+NM*6*MXOLSA
      JPXSP2=KPXSPA+(ISTAF-1)*NM*18*MXOLSA+NM*12*MXOLSA
      ICTARG=KCOSTH+3*NM
      ISTARG=KSINTH+3*NM
      CALL PSUM  (II(KICON ),II(KICNL2),II(KICNH2),                     &
     & AA(KDXSDP),AA(ICOSTH),AA(ISINTH),                                &
     & AA(KPMPXI),AA(IXM   ),AA(KPXSXP),AA(KPXSLV),II(INMP  ),          &
     & II(IINDP ),NM,MTYPE  ,INTPOL    ,AA(KPMPXE),AA(KPMPA ),          &
     & NDIM1     ,NDIM2     ,ISTA      ,LESTA     ,LPOLAD    ,          &
     & ISTAF     ,AA(KPXSPA),LNPNM     ,.FALSE.   ,RSCALX    ,          &
     & MJDSBL,AA(JFSECM),LL(KLVELA),AA(KTIMVL),LL(KLSTL2),LL(KLSTH2),   &
     & AA(KPRCBD),AA(KPRTBD),                                           &
     & AA(KPMPXI+MADDC),AA(KPMPXI+MADDC2),AA(JPXSP1),AA(JPXSP2),        &
     & II(KICNV),DUM,1,DUM,AA(ICTARG),AA(ISTARG),DUM2,AA(KPPER),        &
     & AA(KANGWT),AA(KWT),LL(KLAVOI),AA(KOFDRV),SCALX,.FALSE.,          &
     & AA(KPXDXP), AA(KXUTDT),II(KINDPI),AA(KDXDNU) )
! MUST CHECK THE AA(JFSECM) ABOVE FOR STATION VELOCITIES
 8000 CONTINUE
      IF(.NOT.LACC.AND.LPSBL) GOTO 9100
!***** BEGIN DEBUG *****
!     PRINT 98000
!8000 FORMAT(' ** ANGLES **  8000 CONTINUE  BEFORE PROCES')
!***** END DEBUG *****
! EDIT, PRINT, SUM STATISTICS AND SUM INTO NORMAL EQUATIONS
      ALLOCATE(LEDIT_EXTRA(NM))
      LEDIT_EXTRA(:) = .FALSE.
      CALL PROCES(AA,II,LL,AA(KOBSC ),AA(KRESID),AA(KSIGMA),AA(KRATIO), &
     &          AA(KPMPA ),AA(IOBS  ),AA(ISMCOR),AA(KOBTIM),AA(IOBSIG), &
     &          AA(KIAUNO),AA(IEDSIG),II(KINDH ),II(KNMH  ),AA(KS    ), &
     &          AA(KS0   ),AA(KS1   ),LL(KLEDIT),AA(KELEVS),AA(KDSCRP), &
     &          ISTATS    ,NTYPE     ,INDSTA    ,INDSAT    ,LELEVS    , &
     &          II(KISTNO),AA(KSTNAM),AA(KXPMPA),LL(KLSDAT),1,1,        &
     &          LL(KLSDAT),LL(KLFEDT),LEDIT_EXTRA)
      DEALLOCATE(LEDIT_EXTRA)
 9100 CONTINUE
      IOBS=KDTIME
      ISMCOR=KSMCR2
      IOBSIG=KOBSG2
      IEDSIG=KEDSG2
      ISTATS=ISTATS+1
      NTYPE =NTYPE +1
      L2ND=.TRUE.
 9000 END DO
      RETURN
10000 FORMAT('1',109X,'UNIT  6 PAGE NO.',I6)
20000 FORMAT(1X/' ANGLES ** MEASUREMENT TYPE',I3,' NOT IMPLEMENTED.'/   &
     &   1X)
      END
