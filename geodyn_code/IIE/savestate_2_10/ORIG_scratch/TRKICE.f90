!$TRKICE
      SUBROUTINE TRKICE(XSM,VSM,UI,OFFSET,FSECRA,RA,DR,AA,II,LALT,      &
     &NDIMRA,ATROT,LANTCT,IANTSC,ISATID,L1ST,PHC,LWNDI,LWNDO,WPU,ISEQ,  &
     &IUPDN,ANTTAB,LOFFAJ,DWRKDO)
!********1*********2*********3*********4*********5*********6*********7**
! TRKICE            03/01/19           9212.4    PGMR - SCOTT B. LUTHCKE
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!            TRACKING POINT AND S/C C.G. NOT AT BODY-FIXED
!            ORIGIN FOR TDRS
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
!   LANTCT   I    A    LOGICAL ANTENNA CUTOFF
!   IANTSC   I    A    ANTENNA CUT SATELLITE IDS
!   ISATID   I    S    SATELLITE ID
!
!
!
! NOTES:
!            SEE THE ICEATT ROUTINE FOR NOTES ON ASSUMPTIONS MADE
!
!********1*********2*********3*********4*********5*********6*********7**

      use antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!      COMMON/TRKQP/IMQP
      !!!COMMON/APHASE/NANTPT,NANTPS,NXAPHA

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
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),OFFSET(3,2),                &
     &   RA(NDIMRA,3),DR(NM),FSECRA(NM),UI(NDIMRA,3)
      DIMENSION BDFOFF(3),UNTOFF(3),XPOS(3),VPOS(3)
      DIMENSION AA(1),II(1)
      DIMENSION ATROT(3,3,NM),ROT(9),ROT2(3,3),DUM(20)
      DIMENSION WPU(NM,3,2),VWND(3,2)
      !DIMENSION ANTTAB(NANTPS,1)
      DIMENSION ANTTAB( * )
      DIMENSION PHC(NM)
      DIMENSION LANTCT(1),IANTSC(1)
      DIMENSION DWRKDO(NM,3)
! PARAMETER STATEMENT NOT VALID FOR ARRAY IN SUBROUTINE ARG. LIST.
! IT REMAINS HERE TO SHOW THAT THIS VALUE IS SET IN THIS ROUTINE IN THIS
! RELEASE.  FUTURE VERSIONS WILL GET THIS INFO FROM 2S INPUT
      DATA ZERO/0.0D0/
      DATA ONE/1.0D0/
      EQUIVALENCE(ROT,ROT2)

      LOGICAL :: L_sat_phc
      LOGICAL :: L_ant_phc
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IANT=-9999
      LPHC=.FALSE.
      PHC(1:NM) = 0.0D0  ! jjm 20120601



      IF( XGPSTB .GT. 0.D0  .or.                &
          MTYPE  .EQ. 51    .or.                &
          MTYPE  .EQ. 39         )then

          IANT=(XANTSL(ISEQ)+.001D0)

      endif ! xgpstb ...

      !write(6,'(A,3(1x,I10))') 'trkice: MTYPE, ISEQ, IANT ', &
      !                                  MTYPE, ISEQ, IANT
      !write(6,'(A,2(1x,I10))') 'trkice: ISATID     ', ISATID

      if( nant_sat > 0 )then

          !write(6,'(A,2(1x,I10), 4x, L1)') &
          !      'trkice: call get_index2 nant_sat, iant ', &
          !                               nant_sat, iant

          call get_index2( isatid, iant, II(KANTYP),  NANT_sat, &
                           index_sat, L_sat_phc )
          !write(6,'(A,2(1x,I10), 4x, L1)') &
          !       'trkice: ISATID, index_sat, L_sat_phc   ', &
          !                ISATID, index_sat, L_sat_phc
      endif ! nant_sat > 0


      if( nantpt > 0 ) then
!          write(6,'(A,2(1x,I10), 4x, L1)') &
!                'trkice: call get_index nantpt, iant ', &
!                                        nantpt, iant

          call get_index( iant,  &
                          II(KANTYP+ 2*(NANT_sat + NANT_sta) ), &
                          NANTPT,   index_ant, L_ant_phc )

!          write(6,'(A,2(1x,I10), 4x, L1)') &
!                'trkice: IANT, index_ant, L_ant_phc   ', &
!                         IANT, index_ant, L_ant_phc
      endif ! nantpt > 0

      !write(6,'(A,2(1x,I10), 4x, L1)') &
      !      'trkice: ISATID, index_sat, L_sat_phc   ', &
      !               ISATID, index_sat, L_sat_phc

      !write(6,'(A,2(1x,I10), 2(2x, L1),1x, F10.3)') &
      !      'trkice: ISATID, iant, L_sat_phc, L_ant_phc, xgpstb   ', &
      !               ISATID, iant, L_sat_phc, L_ant_phc, xgpstb


      !IF(IANT.GT.0.AND.IANT.LE.99) LPHC=.TRUE.

      !orig IF( xgpstb > 0.0D0 .and. iant >= 0  .and. &
      !orig     ( L_sat_phc .or. L_ant_phc )           ) LPHC=.TRUE.
      IF( xgpstb > 0.0D0                 .and. &
          ( L_sat_phc .or. L_ant_phc )           ) LPHC=.TRUE.


!      write(6,'(A,3x,L1)')  'trkice: LPHC ', LPHC
!      write(6,'(A,1x,I10)') 'trkice: ISATID ', ISATID

      LWNDO=LWNDI

      !write(6,'(A,2(3x,L1))') 'trkice: LWNDI, LWNDO ', LWNDI, LWNDO

      !write(6,'(A,1x,I5)') 'trkice: IUPDN ', IUPDN

       IF( LWNDI ) THEN
           IF( IUPDN.EQ.1 ) THEN
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
           ENDIF !  IUPDN.EQ.1
       ENDIF !  LWNDI

       !write(6,'(A,3(1x,E15.7))') 'trkice: VWND(1,1:3) ', VWND(1,1:3)
       !write(6,'(A,3(1x,E15.7))') 'trkice: VWND(2,1:3) ', VWND(2,1:3)
       !write(6,'(A,3(1x,E15.7))') 'trkice: VWND(3,1:3) ', VWND(3,1:3)

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
      XPOS(1) = XSM(I,1)
      XPOS(2) = XSM(I,2)
      XPOS(3) = XSM(I,3)
      VPOS(1) = VSM(I,1)
      VPOS(2) = VSM(I,2)
      VPOS(3) = VSM(I,3)
! sbl
!      xxmag = sqrt(xpos(1)*xpos(1)+xpos(2)*xpos(2)+xpos(3)*xpos(3))
!      xx1 = xpos(1)/xxmag
!      xx2 = xpos(2)/xxmag
!      xx3 = xpos(3)/xxmag
!      print *,'trkice: xx1-3: ',xx1,xx2,xx3
! sbl
!
! ROTATE TOTAL OFFSET VECTOR FROM SBF TO INERTIAL FRAME
! COMPUTE INERTIAL OFFSET VECTOR
!
!
        CALL ICEATT(MJDSEC,FSEC,XPOS,VPOS,BDFOFF(1),BDFOFF(2),          &
     &  BDFOFF(3),UNTOFF(1),UNTOFF(2),UNTOFF(3),DUM,                    &
     &  1,0,.FALSE.,IDSATS,II(KIDATB),AA(KSABIA),                       &
     &  AA(KSBTM1),AA(KSBTM2),AA(KVLOUV),II(KNSTLV),II(KSLVID),         &
     &  AA(KTSLOV), AA,ISATID,ROT,ATROT(1,1,I))

       IF(LWNDI) THEN
          DO JJ=1,2
          DO III=1,3
            WPU(I,III,JJ)=ROT2(III,1)*VWND(1,JJ)                        &
     &                   +ROT2(III,2)*VWND(2,JJ)                        &
     &                   +ROT2(III,3)*VWND(3,JJ)
          ENDDO
          ENDDO

           !if( i == 1 ) then
           !    write(6,'(A,3(1x,E15.7))') &
           !     'trkice:  WPU(1,1,1:3) ', WPU(1,1,1:3)
           !    write(6,'(A,3(1x,E15.7))') &
           !     'trkice:  WPU(1,2,1:3) ', WPU(1,2,1:3)
           !    write(6,'(A,3(1x,E15.7))') &
           !     'trkice:  WPU(1,3, 1:3) ', WPU(1,3,1:3)
           !endif

       ENDIF !  LWNDI

      IF( LOFFAJ) THEN

          IF( L1ST) THEN
              DWRKDO(I,1)=UI(I,1)*ROT(1)                  &
                         +UI(I,2)*ROT(2)                  &
                         +UI(I,3)*ROT(3)
              DWRKDO(I,2)=UI(I,1)*ROT(4)                  &
                         +UI(I,2)*ROT(5)                  &
                         +UI(I,3)*ROT(6)
              DWRKDO(I,3)=UI(I,1)*ROT(7)                  &
                         +UI(I,2)*ROT(8)                  &
                         +UI(I,3)*ROT(9)
          ELSE
              DWRKDO(I,1)=-UI(I,1)*ROT(1)                 &
                          -UI(I,2)*ROT(2)                 &
                          -UI(I,3)*ROT(3)
              DWRKDO(I,2)=-UI(I,1)*ROT(4)                 &
                          -UI(I,2)*ROT(5)                 &
                          -UI(I,3)*ROT(6)
              DWRKDO(I,3)=-UI(I,1)*ROT(7)                 &
                          -UI(I,2)*ROT(8)                 &
                          -UI(I,3)*ROT(9)
          ENDIF !    L1ST
      ENDIF !   LOFFAJ

!  ANTENNA CUTOFF FOR GPS SATELLITES

! sbl
!      print *,'trkice: rot1-3: ',rot(1),rot(2),rot(3)
!      print *,'trkice: rot4-6: ',rot(4),rot(5),rot(6)
!      print *,'trkice: rot7-9: ',rot(7),rot(8),rot(9)
!      print *,'trkice: i,ui: ',i,ui(i,1),ui(i,2),ui(i,3)
       !if( i == 1 )then
       !    write(6,'(A,1x,I8,3(1x,E15.7))')  &
       !          'trkice: i,ui ',i,ui(i,1),ui(i,2),ui(i,3)
       !    write(6,'(A,1x,E15.7)')  'trkice: XGPSTB ', XGPSTB
       !endif ! i == 1
! sbl

      if( XGPSTB.GT.0.D0 ) then
          CALL ANTANG(ISEQ,ROT,UI(I,1),UI(I,2),UI(I,3),LANTCT(I))
      endif !   XGPSTB.GT.0.D0


      IF( LPHC) THEN

          XV=UI(I,1)
          YV=UI(I,2)
          ZV=UI(I,3)

          IF( L1ST ) THEN
              XV=-XV
              YV=-YV
              ZV=-ZV
          ENDIF !  L1ST

          !if( i == 1 )then
          ! write(6,'(A,3(1x,E15.7))') 'trkice: XV,YV,ZV', XV,YV,ZV
          !endif ! i == 1

          IMQP=I

          !if( i == 1 )then
          !    write(6,'(A,2(1x,I10))')  'trkice: I, IMQP ', i, imqp
          !endif ! i == 1

          ! old version of ANTSAC
          !CALL ANTSAC(ROT,XV,YV,ZV,ANTTAB(1,IANT),ISEQ,PHC(I))



          !IF( LPHC ) THEN
          !    IPOFF=KANTPS(IANT)
          !    NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
          !    NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)
          !    !if( i == 1 )then
          !    !    write(6,'(A,3(1x,I10))') &
          !    !          'trkice: iant, kantps(iant), ipoff ', &
          !    !                   iant, kantps(iant), ipoff
          !    !    write(6,'(A,1x,E15.7,1x,I10)') &
          !    !          'trkice: ANTTAB(IPOFF+3) , NDIMA   ', &
          !    !                   ANTTAB(IPOFF+3) , NDIMA
          !    !    write(6,'(A,1x,E15.7,1x,I10)') &
          !    !          'trkice: ANTTAB(IPOFF  ) , NDIMZ   ', &
          !    !                   ANTTAB(IPOFF  ) , NDIMZ
          !    !endif ! i == 1
          !ENDIF ! LPHC

          if( L_ant_phc )then

              !IPOFF=KANTPS(IANT) + isum_sat + isum_sta
              IPOFF=KANTPS(index_ant) + isum_sat + isum_sta

              NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
              NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

              !if( i == 1 )then
              !    write(6,'(A,4(1x,I10))') &
              !          'trkice: IANT, IPOFF, NDIMA, NDIMZ ', &
              !                   IANT, IPOFF, NDIMA, NDIMZ
              !endif ! i == 1

          endif ! L_ant_phc

          if( L_sat_phc )then

              IPOFF = KANTPS_sat(index_sat)

              NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
              NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

              !if( i == 1 )then
              !    write(6,'(A,4(1x,I10))') &
              !          'trkice: ISATID, IPOFF, NDIMA, NDIMZ ', &
              !                   ISATID, IPOFF, NDIMA, NDIMZ
              !endif ! i == 1

          endif ! L_sat_phc


          if( ipoff > 0 ) then

              !if( i == 1 )then
              !    write(6,'(A)') 'trkice: call antsac '
              !endif ! i == 1

              call ANTSAC(ROT,XV,YV,ZV,ANTTAB(ipoff+6), &
                          NDIMA,NDIMZ,ANTTAB(ipoff), ISEQ,PHC(I), 2, &
                          .FALSE.)

              !if( i == 1 )then
              !    write(6,'(A,1x,I10,1x,E15.7)') &
              !          'trkice: aft antsac i, PHC(I) ', i, PHC(I)
              !endif ! i == 1

          endif ! ipoff > 0

      ELSE

          PHC(I)=0.D0

      ENDIF ! LPHC

!
      IF(LALT) THEN
        UI(I,1)=-ROT(7)
        UI(I,2)=-ROT(8)
        UI(I,3)=-ROT(9)
      ENDIF

! COMPUTE INERTIAL OFFSET VECTOR

      RA(I,1) = UNTOFF(1)*VMAG
      RA(I,2) = UNTOFF(2)*VMAG
      RA(I,3) = UNTOFF(3)*VMAG


  100 ENDDO


!...RANGE CORRECTION IS DOT PRODUCT OF INERTIAL OFFSET AND
!...UNIT INERTIAL TOPOCENTRIC VECTORS

      CALL DOTPRD(RA,UI,DR,NM,NM,NM,3)

      IF( LPHC ) THEN
          IF( L1ST ) THEN
              DO  I=1,NM
                  DR(I)=DR(I)+PHC(I)
!                  if( i == 1 )then
!                      write(6,'(A,1x,I10,2(1x,E15.7),3x,L1)')  &
!                            'trkice: i, PHC(I), DR(I), L1ST ', &
!                                     i, PHC(I), DR(I), L1ST
!                  endif ! i == 1
              ENDDO
          ELSE
              DO  I=1,NM
                  DR(I)=DR(I)-PHC(I)
!                  if( i == 1 )then
!                      write(6,'(A,1x,I10,2(1x,E15.7),3x,L1)')  &
!                            'trkice: i, PHC(I), DR(I), L1ST ', &
!                                     i, PHC(I), DR(I), L1ST
!                  endif ! i == 1
              ENDDO
          ENDIF !  L1ST
      ENDIF !  LPHC

      RETURN

      END
