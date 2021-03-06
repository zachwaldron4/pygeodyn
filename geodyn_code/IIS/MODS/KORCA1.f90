!$KORCA1
      SUBROUTINE KORCA1(KORE,NCORE)
!********1*********2*********3*********4*********5*********6*********7**
! KORCA1           06/15/83            VVVV.V    PGMR - ?
!
! FUNCTION:         CALCULATE CORE FOR FIRST DYNAMIC ALLOCATION ARRAYS
!                   THE FOLLOWING GROUPS ARE ALLOCATED FOR:
!                       . A1TIME,POLAR MOTION AND FLUX
!                       . SELECT/DELETE ARRAYS FOR DATA HANDLING
!                       . MEASUREMENT BIASES
!                       . STATION POSITION ARRAYS
!                       . ARC FORCE MODEL PARAMETER COUNT
!                       . POLAR MOTION
!                       . OCEAN LOADING
!                       . TIME DEPENDENT GRAVITY ARRAYS
!                       . TIME DEPENDENT SEA SURFACE TOPOGRAPHY
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NCORE    I         NUMBER OF ALLOCATION GROUPS
!   KORE     O         ARRAY CONTAINING REAL,INTEGER AND LOGICAL
!                      CORE REQUIREMENTS FOR EACH ALLOCATION
!                      GROUP.DIM=(3,NCORE)
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/ACCELR/NSATAC,NACCB,NSATDY,NDYNAC,NXACC,                   &
     &       MXACC,NXACCX,MXACCX,NXISSM,MXISSM,NXS1,MXS1,NXS2,MXS2,     &
     &       NXPPN,MXPPN,NXSBS,MXSBS,NSATHR,NFACCB(2,200),NFATIT(2,200),&
     &       MRAT,NBAPPD(200),NDYNPD(200),IACSAT(200),NACB(200),        &
     &       IDNSAT(200),NDYN(200)
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/BIASNX/IEBIAS(4  ),IMBIAS(2)  ,IBSCAL(1),ITBIAS(1),        &
     &              IBREFR(3,2),KLKSTA(4,2),KLKSAT(4,2),KLKSTS(2,2),    &
     &              ITRPZE(2,2),ITRPGR(2,4),IBBIAS(1),NBKBIA,NDXBS1,  &
     &              NXBIAS
      COMMON/BINBUF/NBUFIN,NLENI ,MAXBUF,MAXOBS,NBUFOT,NLENO ,NMAXOT,   &
     &              KBUFOT,MXBLEN,MAXTDD,NLENDR,MOBSBK,MXTIMB,          &
     &              MXOBSB,IIOBDR,IPSMAX,ILIMBK,NPASS ,MVERSN,NXBINB
      COMMON/BWINTG/NMSLAB,NAREPT,NSPRPT,NDFRPT,NEMIPT,NTMAPT,NTMCPT,   &
     &      NTMDPT,NTMFPT,NTHXPT,MXSLAB,MXARPT,MXSRPT,MXDFPT,MXEMPT,    &
     &      MXTAPT,MXTCPT,MXTDPT,MXTFPT,MXTXPT,NXBWIN
      COMMON/CGVTMI/NCDT,NSDT,NCPD,NSPD,NCDTA,NSDTA,NCPDA,NSPDA,NGRVTM, &
     &       KCDT,KCPDA,KCPDB,KSDT,KSPDA,KSPDB,KOMGC,KOMGS,KCOMGC,      &
     &       KSOMGC,KCOMGS,KSOMGS,NXGVTI
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CORA  /KA1UTC,KA1UT1,KXP   ,KYP   ,KEOPDP,KEOPDS,KFLXS ,   &
     &              KFLXM ,KFLXAV,KBIN  ,KBOUT ,KOBDR ,KDAT1 ,KDAT2 ,   &
     &              KDAT3 ,KSCOB ,KB1   ,KB2   ,KB3   ,KBSIGM,KBBIAS,   &
     &              JDSCRP,JDPSR ,JPOLIN,JPUTBH,JXNMOL,JEAQFB,JEAQFE,   &
     &              JEAEFB,JEAEFE,JEAINT,JEASTP,JEAVER,JATPER,JTACCL,   &
     &              JTIMES,JTDYNS,JTIMED,JEXACT,JXACIN,JCTBTI,JCTBWE,   &
     &              JCTCTM,JRTMTB,JVLOPT,JSIGSP,JSSDST,JSSDSR,JSSDDG,   &
     &              JXCGST,JXCGDT,JTACFB,JTACFE,JTAVER,JTAEFB,JTAEFE,   &
     &              JTAINT,JTASTP,JNUTIN,JNUTMD,JDWNWT,NXCORA
      COMMON/CORBNF/MAXINF,NORBNF,INFSTA(15),INFNDX(15),INFSTR,INFSTP,  &
     &              INFRT ,NXCORB
      COMMON/CTIDES/NTIDE ,NSTADJ,NET   ,NETADJ,NETUN ,NOT   ,NOTADJ,   &
     &              NOTUN ,NETUNU,NETADU,NOTUNU,NOTADU,NBSTEP,KTIDA(3), &
     &              ILLMAX,NUNPLY,NNMPLY,NDOODN,MXTMRT,NRESP ,NXCTID
      COMMON/CORI  /KNAME ,KSTA  ,KCONFG,KTYPE ,KSATCD,KMOD  ,KDAY1 ,   &
     &              KTIM1 ,KDAY2 ,KTIM2 ,KVBAS ,KVSTA ,KSATID,KISETN,   &
     &              KVBSTA,KVBVEL,KQUAID,KBNQUA,KBSTA1,KBSAT1,KBTYPE,   &
     &              KBTIM1,KBTIM2,KBIDX, KKPTR ,KSATEQ,JIORFR,JIPDFR,   &
     &              JINDPI,KPSSTA,KPSSAT,KPSTYP,KPSBIA,KPSBLK,KPSTIM,   &
     &              KPSPTR,JEBSTA,JEBSAT,JEBTYP,JEBBEG,JEBEND,JBKBIA,   &
     &              JIPOLC,JIBASP,JIPDAD,JIPDRD,JISTOL,JGTSRT,JGTSRA,   &
     &              JSSTPT,JSSTPA,JSSTFQ,JNPANE,JIFACE,JIPARE,JIPSPR,   &
     &              JIPDIF,JIPEMI,JIPTPA,JIPTPC,JIPTMD,JIPTMF,JIPTHX,   &
     &              JNADAR,JNADSP,JNADDF,JNADEM,JNADTA,JNADTC,JNADTD,   &
     &              JNADTF,JNADTX,JNMOVE,JEASID,JEAQSB,JEAQSE,JEANPA,   &
     &              JEAESB,JEAESE,JEAPNT,JEANMQ,KVBSL2,KVBSH2,KBATYP,   &
     &              JNRDGA,JNRDDR,JNRDSR,JCSETA,JCSETD,JCSETS,JVSETA,   &
     &              JVSETD,JVSETC,JSTRTA,JSTRTD,JSTRTS,JNSTLV,JSLVID,   &
     &              JATSAT,JKVLAS,JYWSID,JLOV  ,JCBID ,JCBDEG,JSATAC,   &
     &              JSATDY,JSATHR,JACSID,JCTBST,JSRTTB,JTMPTB,JSTBST,   &
     &              JDN   ,JDN2  ,JDN3  ,JALSIT,JAPLST,KBDEGR,KBNODE,   &
     &              KBNPRM,KBSSTA,JBSPLN,JSSPLN,JNEXCG,JIDEXC,JNREXC,   &
     &              JWEMGA,JWEMDR,JTPATS,KSIGOV,JTACSB,JTACSE,JTASID,   &
     &              JTAESB,JTAESE,JTANMC,JTAPNT,JTANPA,JGPSID,JGPSBL,   &
     &              JGPSND,JPNALB,JTDSAT,JTDANT,NXCORI
      COMMON/CORL  /KLKEEP,JLAFRC,KPSOUT,KPSOPN,KNBCDT,KQUADJ,JLEBIS,   &
     &              JLAJOL,JEAFIL,JLRDGA,JLRDDR,JLRDSR,JACFIL,KQVADJ,   &
     &              JTAFIL,NXCORL
      COMMON/COREQ /NCHRLY,MAXUTC,MAXUT1,MAXFLX,MAXDSC,KORSUM(3),       &
     &              KORSM2(3),NXCORQ
      COMMON/DELAY /NTDL
      COMMON/DWNWGT/LDWNWT,LDNWMT(999),NXDWNW
      COMMON/EXTAGI/MTASAT,MTAFIL,MTAMEM,MTAPLA,NXTAGI
      COMMON/EXTAGL/LEXTHC,NXTAGL
      COMMON/EXACGI/MACSAT,MACFIL,NXACGI
      COMMON/EXATGI/MEASAT,MEAFIL,MEAMEM,MEAPLA,MEAANT,NXEAGI
      COMMON/GLBPAR/NARCS,NDPOLE,NDCSA,MXDEGG,MXORDG,NSRA,              &
     &              NDUMMY,MAXPAS,NXGLBP
      COMMON/GPSINT/IGPSBW,NGPSBW
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988),  &
     &              NXEPH2
      COMMON/KNTSTS/KNTACC(9,40),MKNTAC,KNTDRG,KNTSLR,MAXNGA,MAXNDR,    &
     &              MAXNSR,NXKNTS
      COMMON/MBIAS /NBIASK,NBIASA,NBIASG,NBCDP,NBCDT,                   &
     &              NUMPAS,NPSFIL,NARCBS,NEBKNT,NXMBIA
      COMMON/MBIAST/MBTRAD,MBTRUN,ICONBIA,IPRCTP,IPREND,NXMBST
      COMMON/MBSGLB/MBIASK,MBIASA,MBIASG,MBCDP,MBCDT,MARCBS,            &
     &              NBIST2,MEBKNT,NBIAST,NXMBGB
      COMMON/NALOAD/NSITES,NALFIL,NALSIT,MAXSIT,ITOTAL,MAXAPL,ITAPLB,   &
     &              ITAPLE,IAPLDT,NXLOAD
      COMMON/NUTINF/NNUT,NNUTAA,NXNUTA
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/POLINF/NPOLE,NPOLEA,NUTA,MXUTC,NMODEL,NXPOLI
      COMMON/SSLIST/MXBAS,MXSTA,MXSAT,MXBSTA,MXSTV,MXQUA,NXSSLI
      COMMON/SSTCOM/NCBAR, NSBAR ,NCDOT ,NSDOT ,NCAPRD,NCBPRD,NSAPRD,   &
     &              NSBPRD,NCBARA,NSBARA,NCDOTA,NSDOTA,NCAPDA,NCBPDA,   &
     &              NSAPDA,NSBPDA,ISSTEP,NXSST
      COMMON/SPBIAS/NCTSTA,NXSPB
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      COMMON/UNITS /IUNT01,IUNT02,IUNT03,IUNT05,IUNT06,IUNT07,IUNT11,   &
     &              IUNT12,IUNT15,IUNT16,                               &
     &              IUNT30,IUNT31,IUNT39,IUNT40,IUNT41,IUNT42,IUNT43,   &
     &              IUNT50,IUNT52,IUNT53,IUNT60,IUNT90,IUNT91,IUNT99,   &
     &              ILINE(2),IPAGE(2),MLINEP(2),MAXCOL(2),IUNT17,       &
     &              IUNT70,IUNT71,                                      &
     &              IUNT88,NXUNIT
      COMMON/VLBI/NQUA,NADJQ,NADJQV,NQUAB,IYMDV,NVOPT,NXVLBI
      COMMON/LSFSHD/LSLFSH, NXSLFS
      COMMON/SLFSHD/NSHSAT,IDSASH(200),NSPANL(200),IFLGSH(200),        &
     &              IATYSH(200),NSHRCD(200),MNSHPL,NXSFSD
      COMMON/EXCGCR/NEXCG,IDEXCG(200),IFTPCG(200),NRCDCG(200),NXEXCG
!
      DIMENSION KORE(3,NCORE)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! ZERO OUT KORE AND KORSUM ARRAYS
      CALL ZEROI(KORE,3*NCORE)
      CALL ZEROI(KORSUM,3)
      MAXCDS=MAXSEL+MAXDEL+MAXMET
!**********************************************************************
! GROUP 1 ALLOCATION:     A1TIME,POLAR MOTION AND FLUX ARRAYS
!**********************************************************************
! FLOATING POINT ARRAYS **
!*************************
! (A1UTC(3,MAXUTC)+1)
! (A1UT1,XP,YP,EOPDEP,EOPDPS;MAXUT1)
! (FLUXS,FLXAV;MAXFLX)
! (FLUXM;MAXFLX*IDYFLX)
      IDYFLX=1
      IF((IATDEN.EQ.41).OR.(IATDEN.EQ.42).OR.(IATDEN.EQ.43)) IDYFLX=8
      IF((IATDEN.EQ.51).OR.(IATDEN.EQ.52).OR.(IATDEN.EQ.53)) IDYFLX=8
      IF((IATDEN.EQ.61).OR.(IATDEN.EQ.62).OR.(IATDEN.EQ.63)) IDYFLX=8
!
      KORE(1,1)=3*MAXUTC+1+5*MAXUT1+2*MAXFLX+MAXFLX*IDYFLX
!******************
! INTEGER ARRAYS **
!******************
      KORE(2,1)=3*ICBODY
!******************
! LOGICAL ARRAYS **
!******************
! LEBIAS
      KORE(3,1)=MAXDSC
!**********************************************************************
! GROUP 2 ALLOCATION:  SELECT/DELETE ARRAYS FOR DATA HANDLING
!**********************************************************************
!
!   FLOATING POINT ARRAYS:
!     KORE(1,2)=MAXBUF*10*NLENI+MAXCDS*3+MAXOBS*6+NLENO*NMAXOT+NLENDR
      KORE(1,2)=MAXBUF*10*NLENI+MAXCDS*3+MAXOBS*3+9000+NLENO*NMAXOT+    &
     & NLENDR
      KORE(1,2)=KORE(1,2)+7*MAXDSC +3*NACCB +2*NACCB*NSATAC
      KORE(1,2)=KORE(1,2)+3*NDYNAC +2*NDYNAC*NSATDY
      KORE(1,2)=KORE(1,2)+3*MXSAT
!******************
! INTEGER ARRAYS **
!******************
      KORE(2,2)=MAXCDS*11+MAXOBS+10*IPSMAX+ILIMBK*IPSMAX + NSATAC +     &
     & NSATDY + NSATHR + NTIDE*2 + NTOLMD+ NTIDE+10 + NBKBIA*IPSMAX
!******************
! LOGICAL ARRAYS **
!******************
      KORE(3,2)=MXOBSB+2*IPSMAX
!**********************************************************************
! GROUP 3 ALLOCATION:   MEASUREMENT BIASES
!**********************************************************************
!   FLOATING POINT ARRAYS:
!*************************
      KORE(1,3)=NBIAST*2 + ICONBIA*5 + NVOPT*64 +6*NCTSTA
! EXTERNAL CGMASS COORDINATES
      KORE(1,3)=KORE(1,3)+NEXCG*2
! ELEVATION DEPENDENT DOWNWEIGHTING
      IF (LDWNWT) KORE(1,3)=KORE(1,3)+90*111
!
!******************
! INTEGER ARRAYS **
!******************
      KORE(2,3)=NBIAST*11+9*MEBKNT+NBKBIA + ICONBIA*3 + MBTRAD +        &
     &          2*  MAXSIT + NBIAST*11 + 7*NCTSTA
! EXTERNAL CGMASS COOR
      KORE(2,3)=KORE(2,3)+1+NEXCG*2
!
!******************
! LOGICAL ARRAYS **
!******************
      KORE(3,3)=NBIAST
!**********************************************************************
! GROUP 4 ALLOCATION: STATION POSITION ARRAYS
!**********************************************************************
!   FLOATING POINT ARRAYS:
!*************************
      KORE(1,4)=0
!******************
! INTEGER ARRAYS **
!******************
      KORE(2,4)=MXBAS+MXSTA+2*MXSAT+4*MXBSTA+2*MXQUA+NORBNF
!******************
! LOGICAL ARRAYS **
!******************
      KORE(3,4)=MXQUA*2
!**********************************************************************
! GROUP 5 ALLOCATION: FORCE MODEL ARRAYS
!**********************************************************************
!   FLOATING POINT ARRAYS:
!*************************
! IEAQFB(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE FSEC START TIME
! IEAQFE(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE FSEC STOP  TIME
! IEAEFB(MEASAT) - EXTERNAL ATTITUDE EPOCH CARD FSEC START TIME
! IEAEFE(MEASAT) - EXTERNAL ATTITUDE EPOCH CARD FSEC STOP  TIME
! IEAINT(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE INTERVAL TIME
! IEASTP(MEASAT) - EXTERNAL ATTITUDE INTEGRATION STEP TIME
! IEAVER(MEAFIL) - EXTERNAL ATTITUDE QUAT. FILE VERSION NUMBER
! ATPER(MXSAT)   - ATTITUDE PERIOD ARRAY
      KORE(1,5)=6*MEASAT + NARCS +MXSAT
! ALLOCATE FOR SELF SHADOWING MODEL
      IF(LSLFSH) THEN
! START TIME, STOP TIME, AND TIME INTERVAL NEED THREE SPACES
       KORE(1,5)=KORE(1,5)+3*NSHSAT+2*MNSHPL
       DO ISAT=1,NSHSAT
! GET THE NUMBER OF BLOCKS FOR THE SATELLITE
         IBLK=0
         IF(IFLGSH(ISAT).EQ.10.OR.IFLGSH(ISAT).EQ.1) THEN
           IBLK=1
         ELSE IF(IFLGSH(ISAT).EQ.11) THEN
           IBLK=2
         ENDIF
! ALLOCATE SPACE FOR DATA READ FROM EXTERNAL FILE
         KORE(1,5)=KORE(1,5)+IBLK*NSHRCD(ISAT)*NSPANL(ISAT)
       ENDDO
      ENDIF
! ITACFB(MTASAT) - EXTERNAL THERMAL ACCEL FILE FSEC START TIME
! ITACFE(MTASAT) - EXTERNAL THERMAL ACCEL FILE FSEC STOP  TIME
! ITAEFB(MTASAT) - EXTERNAL THERMAL ACCEL EPOCH CARD FSEC START TIME
! ITAEFE(MTASAT) - EXTERNAL THERMAL ACCEL EPOCH CARD FSEC STOP  TIME
! ITAINT(MTASAT) - EXTERNAL THERMAL ACCEL FILE INTERVAL TIME
! ITASTP(MTASAT) - EXTERNAL THERMAL ACCEL INTEGRATION STEP TIME
! ITAVER(MTAFIL) - EXTERNAL THERMAL ACCEL FILE VERSION NUMBER

      IEXTA=0
      IF(LEXTHC) IEXTA=1
       if(LEXTHC) KORE(1,5)=KORE(1,5) + (6*MTASAT+MTAFIL)*IEXTA
!******************
! INTEGER ARRAYS **
!******************
! NSATEQ(2,ISATEQ,MXSAT)
! IORFRC(3,MXSAT)
! IPDFRC(3,MXSAT)
! IBASPD(3,MXSAT)
! IPDADJ(3,MXSAT)
! IPDRD(3,2,MXSAT)
! NPANEL(MXSAT)-  NUMBER PANELS FOR EACH SPECIFIC SAT
! NARADJ(MXSAT)-  NUMBER ADJ AREAS FOR EACH SPECIFIC SAT
! NSPADJ(MXSAT)-  NUMBER ADJ SPEC REF FOR EACH SPECIFIC SAT
! NDFADJ(MXSAT)-  NUMBER ADJ DIFF REF FOR EACH SPECIFIC SAT
! NEMADJ(MXSAT)-  NUMBER ADJ EMISSIV  FOR EACH SPECIFIC SAT
! NTAADJ(MXSAT)-  NUMBER ADJ TEMP A   FOR EACH SPECIFIC SAT
! NTCNEL(MXSAT)-  NUMBER ADJ TEMP C FOR EACH SPECIFIC SAT
! NTDADJ(MXSAT)-  NUMBER ADJ TIME D FOR EACH SPECIFIC SAT
! NTFADJ(MXSAT)-  NUMBER ADJ TIME F FOR EACH SPECIFIC SAT
! NTXADJ(MXSAT)-  NUMBER ADJ THETAX FOR EACH SPECIFIC SAT
! NMOVE(MXSAT) -  ARRAY HOLDING NUMBER OF MOVEABLE PLATES FOR EACH SAT.
! IFACE(MXSLAB) - ARRAY FOR INDEX STORAGE
! IPAREA(MXARPT)
! IPSPRF(MXSRPT)
! IPDFRF(MXDFPT)
! IPEMIS(MXEMPT)
! IPTMPA(MXTAPT)
! IPTMPC(MXTCPT)
! IPTIMD(MXTDPT)
! IPTIMF(MXTFPT)
! IPTHTX(MXTXPT)
! JEASID(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE SAT. ID
! JEAQSB(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE MJDSEC START TIME
! JEAQSE(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE MJDSEC STOP TIME
! JEANPA(MEASAT) - EXTERNAL ATTITUDE QUAT. FILE NUMBER OF QATS+PLATES+AN
! JEAESB(MEASAT) - EXTERNAL ATTITUDE EPOCH CARD MJDSEC START TIME
! JEAESE(MEASAT) - EXTERNAL ATTITUDE EPOCH CARD MJDSEC STOP TIME
! JEAPNT(MEASAT) - EXTERNAL ATTITUDE QUATERNION POINTER
! JEANMQ(MEASAT) - EXTERNAL ATTITUDE QUATERNION NUMBER TO BE LOADED
! NREDGA(MKNTAC,3,3,MXSAT) - NO OF CONSTRAINED PERIODS FOR GEN. ACCELERA
! NREDDR(KNTDDR,MXSAT)     - NO OF CONSTRAINED PERIODS FOR  DRAG
! NREDSR(KNTSLR,MXSAT)     - NO OF CONSTRAINED PERIODS FOR  SOLAR RADIAT
! ISTRTA(MKNTAC,3,3,MXSAT) - STARTING LOC OF CONT PERIODS FOR GEN. ACCEL
! ISTRTD(KNTDDR,MXSAT)     - STARTING LOC OF CONT PERIODS FOR DRAG
! ISTRTS(KNTSLR,MXSAT)     - STARTING LOC OF CONT PERIODS FOR SOL RAD.
! NSTLOV(NSTLVS)    - NUMBER TSTLOV CARDS for EACH SATELLITE in MULTI SA
!                     JOB RUN
! ISLVID(NSTLVS) - SATELLITE IDS WHICH HAS TSTLOV CARDS
! II1(JATSAT) - ARRAY USED TO COUNT SATELLITES WITH ATTITUDE MODELS
! II1(JTPATS) - ARRAY USED TO STORE UNIQUE SAT IDS FOR TOPATT
!
      KORE(2,5)=2*ISATEQ*MXSAT+29*MXSAT+MXSLAB+MXARPT+MXSRPT+           &
     &          MXDFPT+MXEMPT+MXTAPT+MXTCPT+MXTDPT+MXTFPT+MXTXPT+       &
     &          8*MEASAT +2* 9*MXSAT*MKNTAC + 2*MXSAT*KNTDRG +          &
     &          2*MXSAT*KNTSLR                                          &
     &         +22*MXSAT                                                &
     &         +2*NSTLVS+11*MXSAT+NYWBIA + MACSAT
! INDICATOR OF WRITEING TO EMATRIX FOR CONSTRAINED GENERAL AND DRAG
! ACCELERATIONS
      KORE(2,5)=KORE(2,5)+9*MXSAT*MKNTAC+MXSAT*KNTDRG
      KORE(2,5)=KORE(2,5)+NSATTP
! JTASID(MTASAT) - EXTERNAL THERMAL ACCEL FILE SAT ID
! JTACSB(MTASAT) - EXTERNAL THERMAL ACCEL FILE MJDSEC START TIME
! JTACSE(MTASAT) - EXTERNAL THERMAL ACCEL FILE MJDSEC STOP TIME
! JTANPA(MTASAT) - EXTERNAL THERMAL ACCEL FILE NUMBER PLATES
! JTAESB(MTASAT) - EXTERNAL THERMAL ACCEL EPOCH CARD MJDSEC START TIME
! JTAESE(MTASAT) - EXTERNAL THERMAL ACCEL EPOCH CARD MJDSEC STOP TIME
! JTAPNT(MTASAT) - EXTERNAL THERMAL ACCEL TEMP?EMIS POINTER
! JTANMC(MTASAT) - EXTERNAL THERMAL ACCEL TEMP?EMIS NUM TO BE LOADED
      KORE(2,5)=KORE(2,5) + 8*MTASAT*IEXTA
!
      KORE(2,5)=KORE(2,5) + IGPSBW*11
      KORE(2,5)=KORE(2,5) + MXSLAB
!  ANTENA TRANSPONDER DELAY
      KORE(2,5)=KORE(2,5) + 2* NTDL
!******************
! LOGICAL ARRAYS **
!******************
! LAFRC(4,3,MXSAT)
! LEAFIL(MEASAT) - EXTERNAL ATTITUDE LOGICAL FLAG
! LREDGA(3,3,MXSAT) - NO OF CONSTRAINED PERIODS FOR GEN. ACCELERATION
! LREDDR(3)         - NO OF CONSTRAINED PERIODS FOR  DRAG
! LREDSR(3)         - NO OF CONSTRAINED PERIODS FOR  SOLAR RADIATION
! LTAFIL(MTASAT) - EXTERNAL THERMAL ACCEL LOGICAL FLAG
!
      KORE(3,5)=4*3*MXSAT+MEASAT + 11*MXSAT + MACSAT + MTASAT*IEXTA
!
!**********************************************************************
! GROUP 6 ALLOCATION: POLAR MOTION
!*********************************************************************
! FLOATING POINT ARRAYS **:
!*************************
! DPSR(NPOLE*2+1) -  POLE TIMES(INITIALLY MID POINT TIMES THEN CHANGED
!                    TO START TIMES IN POLEUT) - ALLOCATED LARGE ENOUGH
!                    FOR GENERATION OF POLE INTERVALS IF TIME GAPS EXIST
! POLINT(NPOLE*2+1)- POLE INTERVAL
! PUTBIH(3,NPOLE*2+1)- BIH VALUES FOR POLE AND UT AT POLEUT TIMES
! KNUTMD(NNUT) - NUTATION TIMES (MIDPOINT OF INTERVALS)
! KNUTIN(NNUT) - NUTATION INTERVAL LENGTH
      KORE(1,6)=(NPOLE*2+1)*5 + 2*NNUT
!******************
! INTEGER ARRAYS **
!******************
! INDPI(NPOLE*2+1)- ARRAY INDICATING WHICH POLE INTERVALS ARE ADJUSTED
! IPOLCD(NPOLE*2+1)-RELATES ORIGINAL POLE CARDS TO REORDERED POLE ARRAYS
!
      KORE(2,6)=(NPOLE*2+1)*2
!
!******************
! LOGICAL ARRAYS **
!******************
      KORE(3,6)=0
!**********************************************************************
! GROUP 7 ALLOCATION:     OCEAN LOADING
!**********************************************************************
! FLOATING POINT ARRAYS **
!*************************
! (XNUMOL(NTOLMD))
!
      KORE(1,7)=NTOLMD
!******************
! INTEGER ARRAYS **
!******************
! (ISITOL(NTOLMD))
      KORE(2,7)=NTOLMD
!******************
! LOGICAL ARRAYS **
!******************
! (LAJOL(NTOLMD))
      KORE(3,7)=NTOLMD
!
!**********************************************************************
! GROUP 8 ALLOCATION: TIME DEPENDENT GRAVITY ARRAYS
!*********************************************************************
!******************
! INTEGER ARRAYS **
!******************
! IGTSRT - SIZE OF IGTSRT = (NCDT+NSDT+3*(NCPD+NSPD))
! IGTSRA - SIZE OF IGTSRA = (NCDTA+NSDTA+2*(NCPDA+NSPDA))
!
      KORE(2,8)=NCDT+NSDT+3*(NCPD+NSPD)+NCDTA+NSDTA+2*(NCPDA+NSPDA)
!
!**********************************************************************
! GROUP 9 ALLOCATION: SEA SURFACE TOPOGRAPHY ARRAYS
!*********************************************************************
!******************
! FLOATING POINT ARRAYS **
!******************
! SSTFRQ - SIZE OF SSTFRQ = (NCAPRD+NCBPRD+NSAPRD+NSBPRD
!                           +NCDOT+NSDOT+NCBAR+NSBAR)
      KORE(1,9)=NCDOT+NSDOT+NCBAR+NSBAR+NCAPRD+NCBPRD+NSAPRD+NSBPRD
!
!******************
! INTEGER ARRAYS **
!******************
! ISSTPT - SIZE OF ISSTPT = (NCDOT+NSDOT+NCBAR+NSBAR+NCAPRD+NCBPRD
!                           +NSAPRD+NSBPRD)
! ISSTPA - SIZE OF ISSTPA = (NCDOTA+NSDOTA+NCBARA+NSBARA+
!                            NCAPDA+NSAPDA+NCBPDA+NSBPDA)
!
      KORE(2,9)=NCDOT+NSDOT+NCBAR+NSBAR+NCAPRD+NCBPRD+NSAPRD+NSBPRD     &
     &         +NCDOTA+NSDOTA+NCBARA+NSBARA+NCAPDA+NCBPDA+NSAPDA+NSBPDA
!
! SUM THE KORE ARRAY(ARRAY IS SCALED TO PROPER MACHINE UNITS IN MAIN)
!
      DO 500 I=1,NCORE
      KORSUM(1)=KORSUM(1)+KORE(1,I)
      KORSUM(2)=KORSUM(2)+KORE(2,I)
      KORSUM(3)=KORSUM(3)+KORE(3,I)
  500 END DO
      WRITE(IUNT90,80100) (KORE(1,INC),INC=1,NCORE)
      WRITE(IUNT90,80110) (KORE(2,INC),INC=1,NCORE)
      WRITE(IUNT90,80120) (KORE(3,INC),INC=1,NCORE)
      WRITE(IUNT90,80130) KORSUM
      WRITE(IUNT06,80140) KORSUM
      RETURN
80100 FORMAT(10X,'DYNAMIC ALLOCATION NUMBER 1'/(1X,'REAL    - ',10I11))
80110 FORMAT(1X,'INTEGER - ',10I11)
80120 FORMAT(1X,'LOGICAL - ',10I11)
80130 FORMAT(/1X,'CORE SUMS ',3I12)
80140 FORMAT(/4X,'MEMORY ALLOCATION NUMBER 1',/,                        &
     &     4X,I7,1X,'R*8 WORDS  ;  ',I7,' INTEGER WORDS  ;  ',I7,       &
     &     ' LOGICAL WORDS')
      END
