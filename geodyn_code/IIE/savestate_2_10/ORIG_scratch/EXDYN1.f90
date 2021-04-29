!$EXDYN1
      SUBROUTINE EXDYN1(MAXD,ND1,ISAT1,ISET1,AA,II,LL,                  &
     &                  XTD,XTP,XMP,PXEPXI,NEQN,N3,IPTFMG,ILNFMG,       &
     &                  NFMG,PXEPA,ACOEF,ACOEF2,ACOEF3,MJDSC1,          &
     &                  FSEC1,AUX1,                                     &
     &                  TIME1,NDM1,NDM2,TAT1,                           &
     &                  IPAT11,IPAT12,                                  &
     &                  X2VSPR,NDMX1,NDMX2,NDMX3,X2SPA,                 &
     &                  ATPER,NM,ND2AC,LCON,KPAIR,MAXNM)
!***********************************************************************
!
! PURPOSE: FORM THE CONSTRAINT EQUATION ASSOCIATED WITH A DYNAMIC CROSSO
!
!          ND1    -  DEGREE +1 OF POLYNOMIAL USED TO FIT ASCENDING &
!                    DESCENDING STREAMS (QUADRATIC ; ND1=3)
!          ISAT1  -  INTERNAL SATELLITE NUMBER ASSOCIATED WITH FIRST STR
!          ISET1  -  SATELLITE SET NUMBER ASSOCIATED WITH FIRST STREAM
!          ISAT2  -  INTERNAL SATELLITE NUMBER ASSOCIATED WITH SECOND ST
!          ISET2  -  SATELLITE SET NUMBER ASSOCIATED WITH SECOND STREAM
!          AA     -  DYNAMIC FLOATING POINT ARRAY
!          II     -  DYNAMIC INTEGER ARRAY
!          LL     -  DYNAMIC LOGICAL ARRAY
!          XTD    -  SCRATCH SPACE DIMENSIONED MINTIM*3
!          XTP    -  SCRATCH SPACE DIMENSIONED MINTIM*3
!          XMP    -  SCRATCH SPACE DIMENSIONED MINTIM*3
!          PXEPXI -  ARRAY OF PARTIALS OF EARTH FIXED COORDINATES OF STR
!                    WRT INERTIAL TRUE OF REFERENCE COORDINATES OF SATEL
!                    (SCRATCH SPACE)
!          NEQN   -  ARRAY WITH NUMBER OF ADJUSTING FORCE MODEL PARAMETE
!                    FOR EACH SATELLITE SET
!          N3     -  ARRAY WITH 3*NUMBER OF SATS FOR EACH SATELLITE SET
!          IPTFMG -  POINTER TO STARTING LOCATIONS OF ADJUSTING FORCE
!                    MODEL PARAMETER GROUPS IN THE SET OF ALL ADJUSTING
!                    PARAMETERS (FOR EACH SATELLITE)
!          ILNFMG -  NUMBER OF ADJUSTING FORCE MODEL PARAMETERS IN EACH
!                    FORCE MODEL GROUP (FOR EACH SATELLITE)
!          NFMG   -  NUMBER OF FORCE MODEL PARAMETERS GROUPS FOR (EACH
!                    SATELLITE)
!          PXEPA   - SRATCH ARRAY USED FOR EACH EARTH FIXED COORDINATE O
!                    STREAM TO FORM THE PARTIAL OF THE COORDINATE WRT EA
!                    ADJUSTING PARAMETER
!          ACOEF   - SCRATCH ARRAY USED TO HOLD COEFFICIENTS OF 3 POLYNO
!                    DESCRIBING THE STREAM OF EACH CEARTH FIXED COORDIAT
!                    ALSO HOLDS THE PARTIAL OF EACH COEFFICIENT WRT EACH
!                    COORDINATE IN THE STREAM
!          ACOEF2  - SCRATCH ARRAY USED TO HOLD COEFFICIENTS OF 3 POLYNO
!                    DESCRIBING THE STREAM OF EACH CEARTH FIXED COORDIAT
!                    THIS FOR THE QUADRATIC
!          MJDSC1  - TIME IN INTEGER SECONDS FROM (2430000.5) TO START O
!                    FIRST STREAM
!          FSEC1   - TIME TAGS OF FIRST STREAM (MJDSC1+FSEC1)
!          TIME1   - SCRATCH ARRAY TO HOLD TIME TAGS OF FIRST STREAM
!          DDDA    - SCRATCH ARRAY TO HOLD PARTIALS OF MINIMUM DISTANCE
!                    (BETWEEN STREAMS) WRT POLYNOMIAL COEFFICIENTS OF
!                    EACH STREAM
!          NDM1    - FIRST DIMENSION OF PXEPA ARRAY (ABOVE) ; NADJST IF
!                    LNPNM=TRUE ; NUCON OTHERWISE
!          NDM2    - SECOND DIMENSION OF PXEPA ARRAY (ABOVE) ; NUCON IF
!                    LNPNM=TRUE ; NADJST OTHERWISE
!          TAT1    - TIME TAG ASSOCIATED WITH TIME DEPENDENT ATTITUDE
!                    PARAMETERS OF FIRST STREAM
!          IPAT11  - STARTING LOCATION IN IN ADJUSTED PARAMETER ARRAY OF
!                    ATTITUDE PARAMTERS OF SATELLITE OF FIRST STREAM (0
!                    IF NO ADJUSTMENT)
!          IPAT12  - STARTING LOCATION IN IN ADJUSTED PARAMETER ARRAY OF
!                    ATTITUDE PARAMTERS OF INSTRUMENT OF FIRST STREAM (0
!                    IF NO ADJUSTMENT)
!          XV2SPR  - FORCE MODEL PARTIALS FOR POINTS IN THE STREAM
!          LCON    - IF TRUE CALLED FROM CONGEN
!                  - IF FALSE CALLED FROM SNAPF2
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
! WATCH OUT IF CBLOKI IS ADDED (NM IS RESET)
      COMMON/ATTCB /KPAT,MAXAT,MAXLAS,IBLAS,                            &
     &              NASAT,IATCNT,NXATT
      COMMON/AXIS/LINTAX
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CESTIM/MPARM,MAPARM,MAXDIM,MAXFMG,ICLINK,IPROCS,NXCEST
      COMMON/CESTML/LNPNM ,NXCSTL
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
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
      COMMON/CPMPA /KTMPAR(3)    ,KMBPAR(2)    ,KRFPAR(2)    ,          &
     &              KAEPAR,KFEPAR,KFPPAR,KVLPAR,KETPAR(2,2)  ,          &
     &              KPMPAR,KUTPAR,KSTPAR(3,4)  ,NADJST       ,          &
     &              NDIM1 ,NDIM2 ,NXDIM1,NXDIM2
      COMMON/LTOTCR/LEDCR(100000)
      COMMON/NEWCON/XOK0,XNOK0
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
      COMMON/NTOTCR/NTCR,NWCR
      COMMON/RMSCR/RMSTCR,RMSWCR,RMSRCR,RMSPCR
      COMMON/XOVERS/NRXTOT,NXBLK,NUSIDE,NDEGX2,NDEGXX,NUCON,ITERNU,     &
     &              IPEDIT,IDEDIT,NXXOVR
!
      DIMENSION AA(1),II(1),LL(1)
      DIMENSION FSEC1(NM),TIME1(NM)
      DIMENSION XTD(MINTIM,3),XTP(MINTIM,3),XMP(MINTIM,3)
      DIMENSION PXEPXI(NM,3,3)
      DIMENSION NEQN(MSETA),N3(MSETA),IPTFMG(MAXFMG,MSATA)
      DIMENSION ILNFMG(MAXFMG,MSATA),NFMG(MSATA)
      DIMENSION PXEPA(NDM1,NDM2,3)
      DIMENSION ACOEF(MAXD,ND2AC,3)
      DIMENSION ACOEF2(3,1,3)
      DIMENSION ACOEF3(MAXD,4)
      DIMENSION AUX1(MAXNM,3)
      DIMENSION X2VSPR(NDMX1,NDMX2,NDMX3)
      DIMENSION X2SPA(NM,3,20)
      DIMENSION ATPER(1)
      DIMENSION IPTTMB(2)
      DIMENSION PPOR(3,NM)
      DIMENSION TMPPOR(NM)
      DIMENSION DADA0(1),DADIC(1)
!
!
!
!
!  LOAD THE NU PERTINENT TIMES OF THE PASSES INTO FSEC1
!  MJDSCX + FSECX  WILL BE USED FOR ABSOLUTE TIME REFERENCE
!
!  ADJUST THE PERTINENT TIMES PUT INTO TIME1
!
!     write(6,*)' dbg enter exdyn1 '
!   T1EST IS ALSO REFERENCED FROM MJDSC1
      T1EST=(FSEC1(1)+FSEC1(NM))*.5D0
      DO 100 I=1,NM
      TIME1(I)=FSEC1(I)-T1EST
  100 END DO
!
      IF(LCON) GOTO 200
!
!
!  LOAD 1ST PASS OF ECF BOUNCE POINTS INTO XTD
      DO I=1,NM
      XTD(I,1)=AUX1(I,1)
      XTD(I,2)=AUX1(I,2)
      XTD(I,3)=AUX1(I,3)
      ENDDO
!
      CALL PFTPR2(NM,1,3,TIME1,XTD(1,1),XTD(1,2),                       &
     &            XTD(1,3),ACOEF2(1,1,1))
!     CALL PFTPAR(NM,ND2AC,ND1,TIME1,XTD(1,1),XTD(1,2),                 &
!     CALL PFTPR(MAXD,ND2AC,ND1,NM,TIME1,XTD(1,1),XTD(1,2),             &
!    &            XTD(1,3),ACOEF(1,1,1),RMS1,SLOPE1)
      CALL PFTPR3(MAXD,ND2AC,ND1,NM,TIME1,XTD(1,1),XTD(1,2),            &
     &            XTD(1,3),ACOEF(1,1,1),X2SPA(1,1,20),X2SPA(1,3,20),    &
     &            ACOEF3,XTP)

 200   CONTINUE
! check x2spa and x2vspr
!        write(6,*) '------X2SPA-----------------'
!        do i=1,20
!          write(6,*) i, x2spa(1,1,i),x2spa(1,2,i),x2spa(1,3,i)
!        enddo
!        write(54,*) '------X2VSPR-----------------'
!        write(54,*) ndmx1,ndmx2,ndmx3
!        write(6,*) ndmx1,ndmx2,ndmx3
!        do i=1,ndmx1
!          write(54,*) i,x2vspr(i,1,1),x2vspr(i,2,1),x2vspr(i,3,1)
!        enddo
!         write(6,*)' dbg fm partials '
!        do i=1,ndmx1
!          write(6,*) i,x2vspr(i,1,1),x2vspr(i,2,1),x2vspr(i,3,1)
!        enddo
!
!      DO 1000 KPASS=1,1
!
!  GET PXEPXI ARRAY OF FIRST PASS
!  XTD,XTP AND XMP ARE JUST SCRATCH ARRAYS
!
      KPASS=1
!     IF(KPASS.EQ.1) THEN
         JSAT=ISAT1
         JSET=ISET1
         IP0AT1=IPAT11
         IP0AT2=IPAT12
!!!!!    PER=ATPER(JSAT)
         DO 410 I=1,NM
         TIME1(I)=FSEC1(I)-(TAT1-DBLE(MJDSC1))
  410    CONTINUE
         CALL DXEDXI(AA,II,LL,MJDSC1,FSEC1,NM,XTD,XTP,XMP,PXEPXI)
!      ENDIF
!      write(6,*)' dbg after DXEDXI ',NM
!      write(6,*)PXEPXI(1,1,1),PXEPXI(1,1,2),PXEPXI(1,1,3)
!      write(6,*)PXEPXI(1,2,1),PXEPXI(1,2,2),PXEPXI(1,2,3)
!      write(6,*)PXEPXI(1,3,1),PXEPXI(1,3,2),PXEPXI(1,3,3)
!
!  FOR KPASS PASS OF DATA
!  LOAD PARTIALS OF XYZ TOR WRT FORCE MODELS INTO AA(KPXPFM)
!
!
!  CHAIN PXEPXI WITH PXIPFM TO GET PARTIALS OF XE1 WRT FORCE MODEL
!
      NCLEAR=NDM1*NDM2*3
      CALL CLEARA(PXEPA(1,1,1),NCLEAR)
      DO 900 IX=1,3
      CALL CHAIN (PXEPXI(1,1,IX),NM,3,1,X2VSPR(1,1,1),NDMX1,            &
     &    NDMX2,NDMX3,NEQN(JSET),                                       &
     &    N3(JSET),IPTFMG(1,JSAT),ILNFMG(1,JSAT),NFMG(JSAT),            &
     &    PXEPA(1,1,IX),NDM1,NDM2,LNPNM,.FALSE.,.FALSE.,1.D0,LL(KLAVOI))
!           write(54,*) (pxepa(k,1,ix),k=1,ndm1)
!           write(6,*)'pxepa 1 '
!           do k=1,ndm1
!           write(6,*) pxepa(k,1,ix),k,1,ix
!           enddo
      IF(LSTINR.AND.NPVAL0(IXXTRO).GT.0) THEN
        IF(LINTAX) THEN
           IPT=IPVAL0(IXXTRO)
           KPT=IPVAL0(IXXTRO)+NPVAL0(IXXTRO)-6
           IF(LNPNM) THEN
               DO I=1,NM
                 PXEPA(IPT,I,IX)=PXEPA(IPT,I,IX)+                       &
     &           X2SPA(I,IX,12)
                 PXEPA(IPT+1,I,IX)=PXEPA(IPT+1,I,IX)+                   &
     &           X2SPA(I,IX,13)
                 PXEPA(IPT+2,I,IX)=PXEPA(IPT+2,I,IX)+                   &
     &           X2SPA(I,IX,14)
                 PXEPA(IPT+3,I,IX)=PXEPA(IPT+3,I,IX)+                   &
     &           X2SPA(I,IX,15)
                 PXEPA(IPT+4,I,IX)=PXEPA(IPT+4,I,IX)+                   &
     &           X2SPA(I,IX,16)
                 PXEPA(IPT+5,I,IX)=PXEPA(IPT+5,I,IX)+                   &
     &           X2SPA(I,IX,17)
                 PXEPA(KPT,I,IX)=PXEPA(KPT,I,IX)+                       &
     &           X2SPA(I,IX,6)
                 PXEPA(KPT+1,I,IX)=PXEPA(KPT+1,I,IX)+                   &
     &           X2SPA(I,IX,7)
                 PXEPA(KPT+2,I,IX)=PXEPA(KPT+2,I,IX)+                   &
     &           X2SPA(I,IX,8)
                 PXEPA(KPT+3,I,IX)=PXEPA(KPT+3,I,IX)+                   &
     &           X2SPA(I,IX,9)
                 PXEPA(KPT+4,I,IX)=PXEPA(KPT+4,I,IX)+                   &
     &           X2SPA(I,IX,10)
                 PXEPA(KPT+5,I,IX)=PXEPA(KPT+5,I,IX)+                   &
     &           X2SPA(I,IX,11)
               ENDDO
           ELSE
               DO I=1,NM
                 PXEPA(I,IPT,IX)=PXEPA(I,IPT,IX)+                       &
     &           X2SPA(I,IX,12)
                 PXEPA(I,IPT+1,IX)=PXEPA(I,IPT+1,IX)+                   &
     &           X2SPA(I,IX,13)
                 PXEPA(I,IPT+2,IX)=PXEPA(I,IPT+2,IX)+                   &
     &           X2SPA(I,IX,14)
                 PXEPA(I,IPT+3,IX)=PXEPA(I,IPT+3,IX)+                   &
     &           X2SPA(I,IX,15)
                 PXEPA(I,IPT+4,IX)=PXEPA(I,IPT+4,IX)+                   &
     &           X2SPA(I,IX,16)
                 PXEPA(I,IPT+5,IX)=PXEPA(I,IPT+5,IX)+                   &
     &           X2SPA(I,IX,17)
                 PXEPA(I,KPT,IX)=PXEPA(I,KPT,IX)+                       &
     &           X2SPA(I,IX,6)
                 PXEPA(I,KPT+1,IX)=PXEPA(I,KPT+1,IX)+                   &
     &           X2SPA(I,IX,7)
                 PXEPA(I,KPT+2,IX)=PXEPA(I,KPT+2,IX)+                   &
     &           X2SPA(I,IX,8)
                 PXEPA(I,KPT+3,IX)=PXEPA(I,KPT+3,IX)+                   &
     &           X2SPA(I,IX,9)
                 PXEPA(I,KPT+4,IX)=PXEPA(I,KPT+4,IX)+                   &
     &           X2SPA(I,IX,10)
                 PXEPA(I,KPT+5,IX)=PXEPA(I,KPT+5,IX)+                   &
     &           X2SPA(I,IX,11)
               ENDDO
           ENDIF
        ELSE
           DO I=1,NM
             PPOR(1,I)=X2SPA(I,IX,12)
             PPOR(2,I)=X2SPA(I,IX,13)
             PPOR(3,I)=X2SPA(I,IX,14)
             TMPPOR(I)=FSEC1(I)+DBLE(MJDSC1)
           END DO
           CALL CHPOR(PPOR,PXEPA(1,1,IX),NM,NDM1,NDM2,LNPNM,            &
     &                TMPPOR,LL(KLAVOI),AA(KANGWT),AA(KWT),AA(KPPER),   &
     &                DADA0,DADIC,1)
!!!!!!!!ENDIF FOR:  IF(LINTAX) THEN
        ENDIF
!!!!!ENDIF FOR: IF(LSTINR.AND.NPVAL0(IXXTRO).GT.0) THEN
      ENDIF
!
!
!  INSERT THE PARTIALS OF XE WRT ATTITUDE INTO PXEPA ARRAY
!
!  FIRST INSERT PARTIALS OF COMPONENT IX WRT ROLL PITCH YAW OF SATELLITE
!  INTO XTD
      DO 125 I=1,NM
      DO 120 IK=1,3
      XTD(I,IK)=X2SPA(I,IX,IK)
!     write(6,*)' dbg ATT PART ',X2SPA(I,IX,IK),I,IX,IK
  120 END DO
  125 END DO
      IF(IP0AT1.GT.0) THEN
        IF(KPASS.EQ.1) ISATQ=ISAT1
        IF(KPASS.EQ.2) ISATQ=ISAT2
        CALL FNDNUM(II(KISATN-1+ISATQ),II(KATSAT),NASAT,IATSAT)
        IF(IATSAT.LE.0) THEN
          WRITE(6,6500) II(KISATN-1+ISATQ)
          WRITE(6,6501)
          WRITE(6,6502)
          STOP
        ENDIF
        PER=ATPER(IATSAT)
          CALL CHNAT(XTD,PXEPA(1,1,IX),NDM1,NDM2,MINTIM,LNPNM,NM,IP0AT1,&
     &               TIME1,PER,LL(KLAVOI))
      ENDIF
!
!  NOW INSERT PARTIALS OF COMPONENT IX WRT ROLL PITCH YAW OF LASER
!  INTO XTD
      DO 135 I=1,NM
      DO 130 IK=1,3
      XTD(I,IK)=X2SPA(I,IX,IK)
!     write(6,*)' dbg ATT PART LAS ',X2SPA(I,IX,IK),I,IX,IK
  130 END DO
  135 END DO
      IF(IP0AT2.GT.0) THEN
          CALL CHNAT(XTD,PXEPA(1,1,IX),NDM1,NDM2,MINTIM,LNPNM,NM,IP0AT2,&
     &               TIME1,PER,LL(KLAVOI))
      ENDIF
!           write(6,*)'pxepa 2 '
!           do k=1,ndm1
!           write(6,*) pxepa(k,1,ix),k,1,ix
!           enddo
!
!  NOW INSERT PARTIALS OF OBS AND ATTITUDE TIMING BIAS
!  INTO XTD
      DO 145 I=1,NM
      DO 140 IK=1,2
      XTD(I,IK)=X2SPA(I,IX,IK+3)
  140 END DO
  145 END DO
      IPTTMB(1)=X2SPA(1,1,18)+.001D0
      IPTTMB(2)=X2SPA(1,2,18)+.001D0
      NTTB=IPTTMB(1)+IPTTMB(2)
      IF(NTTB.GT.0) THEN
          CALL CHNTM(IPTTMB,XTD,PXEPA(1,1,IX),NDM1,NDM2,MINTIM,LNPNM,NM,&
     &                     LL(KLAVOI))
      ENDIF
!
! REGULAR BIAS AND INDIVIDUAL LASER BIAS
!
      DO ISBIA=1,2
        IBF=0
        IF(ISBIA.EQ.1) IBF=X2SPA(1,2,20)+.001D0
        IF(ISBIA.EQ.2) IBF=X2SPA(1,3,18)+.001D0
        IF(IBF.GT.0) THEN
         LL(KLAVOI-1+IBF)=.FALSE.
         IF(LNPNM) THEN
          DO I=1,NM
             PXEPA(IBF,I,IX)=-(X2SPA(I,1,19)*PXEPXI(I,1,IX)             &
     &                        +X2SPA(I,2,19)*PXEPXI(I,2,IX)             &
     &                        +X2SPA(I,3,19)*PXEPXI(I,3,IX))
           ENDDO
          ELSE
            DO I=1,NM
             PXEPA(I,IBF,IX)=-(X2SPA(I,1,19)*PXEPXI(I,1,IX)             &
     &                        +X2SPA(I,2,19)*PXEPXI(I,2,IX)             &
     &                        +X2SPA(I,3,19)*PXEPXI(I,3,IX))
            ENDDO
          ENDIF
        ENDIF
      ENDDO
!
! END REG AND INDIVIDUAL BIAS
!
!
!
  900 END DO
!     write(6,*)' dbg ndm1 ndm2',ndm1,ndm2
!     do k=1,ndm1
!       write(6,*) k,pxepa(k,1,1),pxepa(k,1,2),pxepa(k,1,3),k
!     enddo
!     do k=1,ndm1
!       write(6,*) k,pxepa(k,1,1),pxepa(k,1,2),pxepa(k,1,3),k
!     enddo
!     write(6,*) '-------------------------------------'
      RETURN
 6500 FORMAT(' EXECUTION TERMINATING IN EXDYNX FOR SATELLITE ',I12)
 6501 FORMAT(' ATTITUD IS ADJUSING BUT SATELLITE NOT FOUND IN ARRAY')
 6502 FORMAT(' OF SATELLITES WITH ATITUD PARAMETERS')
      END
