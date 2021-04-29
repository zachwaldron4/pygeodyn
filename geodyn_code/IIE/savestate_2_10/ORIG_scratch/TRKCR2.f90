!$TRKCR2
      SUBROUTINE TRKCR2(XSM,VSM,UI,OFFSET,FSECRA,RA,DR,AA,II,ISATID,    &
     &                  L1ST,LOFFAJ,DWRKDO,LALT,NDIMRA,ATROT,LANTCT,    &
     &                  IANTSC,STAINF,INDSTA,PHC,ANTTAB,LDORANT)
!********1*********2*********3*********4*********5*********6*********7**
! TRKTOP           97/01/24                      PGMR - ANDREW MARSHALL
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!            TRACKING POINT AND S/C C.G. NOT AT BODY-FIXED
!            ORIGIN FOR CRYOSAT-2
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSM      I         EARTH CENTERED INERTIAL POSITION VECTORS
!   VSM      I         EARTH CENTERED INERTIAL VELOCITY VECTORS
!   UI       I         UNIT INERTIAL TOPOCENTRIC S/C POSITION
!   OFFSET   I         BODY CENTERED FIXED TRACKING POINT LOCATION
!   FSECRA   I    A    ANTENNA CORR. TIME SINCE BLOCK START
!   RA            A    OFFSET LOCATION MAPPED INTO INERTIAL COORD.
!   DR       O         CORRECTIONS TO RANGES
!   LANTCT   I    A    LOGICAL ANTENNA CUTOFF
!   IANTSC   I    A    ANTENNA CUT SATELLITE IDS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      use antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      PARAMETER(MAXDIM_L=1000)
      SAVE
      INCLUDE 'COMMON_DECL.inc'
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/CANTEN/NCUT,NXANTE
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
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
      COMMON/TOPOVR/NMTPAT,MXTPAT,NOVRID,NGCATT,NTPBIA,NSTLVS,          &
     &              NISLV, NYWBIA,MSATYW,MAXYWB,NSATTP,NXTOPO
      DIMENSION XSM(MINTIM,3),VSM(MINTIM,3),OFFSET(3,2),                &
     &   UI(NDIMRA,3),RA(NDIMRA,3),DR(NM),FSECRA(NM)
      DIMENSION BDFOFF(3),UNTOFF(3)
      DIMENSION XPOS(3),VPOS(3),ROT(9)
      DIMENSION AA(1),II(1)
      DIMENSION DWRKDO(MINTIM,3)
      DIMENSION ATROT(3,3,NM),TOTROT(3,3),ALTPT(3)
      DIMENSION LANTCT(1),IANTSC(1)
      DIMENSION STAINF(NSTAIN,*)
      DIMENSION INDSTA(3)
      DIMENSION PHC(NM)
      DIMENSION ANTTAB(1)
      DATA ZERO/0.0D0/
      DATA L1STE/.TRUE./

      LOGICAL :: L_sat_phc
      LOGICAL :: L_ant_phc
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(L1STE.AND.XGPSTB.GT.0.D0) THEN
        L1STE=.FALSE.
        WRITE(6,60000)
        WRITE(6,60001)
        WRITE(6,60002)
        WRITE(6,60003)
        WRITE(6,60004)
        WRITE(6,60005)
        WRITE(6,60006)
      ENDIF
!
      IF(NM.GT.MAXDIM_L) THEN
         WRITE(6,78912)
78912    FORMAT(' SUBROUTINE TRKCR2. NUMBER MEAS EXCEEDS MAXDIM_L')
         STOP
      ENDIF

!--------------------------------------------------------------------

      IANT=-9999
      LPHC=.FALSE.

      L_sat_phc  = .FALSE.
      L_ant_phc  = .FALSE.

      PHC(1:NM) = 0.0D0  ! jjm 20120601

      !write(6,'(A,1x,I4,1x,F6.1)') &
      !      'trkcr2:1 MTYPE, XGPSTB      ', MTYPE, XGPSTB
      !write(6,'(A,1x,I4,1x,F6.1)') &
      !      'trkcr2:1 ISEQ, XANTSL(ISEQ) ', &
      !                ISEQ, XANTSL(ISEQ)

      if( XGPSTB .GT. 0.D0  .or.                     &
          MTYPE  .EQ. 51    .OR.                     &
          MTYPE  .EQ. 39         ) then

          !write(6,'(A,1x,I4,1x,F6.1)') &
          !      'trkcr2:2 MTYPE, XGPSTB            ', MTYPE, XGPSTB

          IANT=(XANTSL(ISEQ)+.001D0)

          !write(6,'(A,1x,I4,1x,F6.1, 1x, I6)') &
          !      'trkcr2:2 ISEQ, XANTSL(ISEQ), IANT ', &
          !                ISEQ, XANTSL(ISEQ), IANT

      endif ! xgpstb...

!      write(6,'(A,1x,I4,1x,I6, 4x, L1)') 'trkcr2: ISEQ, IANT, LPHC ', &
!                                                  ISEQ, IANT, LPHC
!      write(6,'(A,1x,I10)')  'trkcr2: ISATID ', ISATID


      if( nant_sat > 0 )then
          call get_index2( isatid, iant, II(KANTYP),  NANT_sat, &
                           index_sat, L_sat_phc )
!          write(6,'(A,1x,I10, 2(1x,I6), 4x, L1)') &
!                 'trkcr2: ISATID, iant, index_sat, L_sat_phc', &
!                          ISATID, iant, index_sat, L_sat_phc
      endif


      if( nantpt > 0 )then
          call get_index( iant,  &
                          II(KANTYP+ 2*(NANT_sat + NANT_sta) ),  &
                          NANTPT, index_ant, L_ant_phc )
!          write(6,'(A,1x,I4,1x,I6, 4x, L1)') &
!                 'trkcr2: IANT, index_ant, L_ant_phc', &
!                          IANT, index_ant, L_ant_phc
      endif  ! nantpt > 0


      IF( XGPSTB > 0.0D0  .and.                &
          ( L_sat_phc .or. L_ant_phc )              ) LPHC=.TRUE.

      !write(6,'(A,3x,L1)') 'trkcr2: LPHC ', LPHC

      IF( LPHC ) THEN

        if( NANTPT > 0 .and. L_ant_phc )then

            IPOFF=KANTPS(index_ant) + isum_sat + isum_sta
            NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
            NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

            !write(6,'(A,4(1x,I6))') &
            !      'trkcr2:a IANT, IPOFF, NDIMA, NDIMZ ', &
            !                IANT, IPOFF, NDIMA, NDIMZ

        endif ! L_ant_phc


        if( L_sat_phc )then

            IPOFF = KANTPS_sat(index_sat)

            NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
            NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

            !write(6,'(A,4(1x,I6))') &
            !      'trkcr2: sa index_sat, IPOFF, NDIMA, NDIMZ ', &
            !                  index_sat, IPOFF, NDIMA, NDIMZ

            !write(6,'(A,4(1x,I10))') &
            !      'trkcr2: satid ... &
            !      &index_sat, ii( KANTYP + (index_sat -1) * 2   ) ', &
            !       index_sat, ii( KANTYP + (index_sat -1) * 2   )
            !write(6,'(A,4(1x,I10))') &
            !      'trkcr2: ant   ... &
            !      &index_sat, ii( KANTYP + (index_sat -1) * 2 +1) ', &
            !       index_sat, ii( KANTYP + (index_sat -1) * 2 +1)


        endif ! L_sat_phc


      ENDIF ! LPHC


!      write(6,'(A,3(1x,I4))') 'trkcr2: MTYPE, ISEQ ', &
!                                       MTYPE, ISEQ
!      write(6,'(A,3x,L1)') 'trkcr2: LPHC ', LPHC


!--------------------------------------------------------------------

!
! COMPUTE BODY FIXED OFFSET UNIT VECTOR
!     write(79,*) 'enter trkgfo',(offset(i,2),i=1,3)
      VMAG = SQRT(OFFSET(1,2)**2+OFFSET(2,2)**2+OFFSET(3,2)**2)
      BDFOFF(1)=OFFSET(1,2)/VMAG
      BDFOFF(2)=OFFSET(2,2)/VMAG
      BDFOFF(3)=OFFSET(3,2)/VMAG
!
! COMPUTE INERTIAL OFFSET UNIT VECTOR
      DO 100 I=1,NM
        XPOS(1) = XSM(I,1)
        XPOS(2) = XSM(I,2)
        XPOS(3) = XSM(I,3)
        VPOS(1) = VSM(I,1)
        VPOS(2) = VSM(I,2)
        VPOS(3) = VSM(I,3)
        CALL CRYOATT(MJDSBL,FSECRA(I),XPOS,VPOS,BDFOFF(1),              &
     &           BDFOFF(2),BDFOFF(3),UNTOFF(1),UNTOFF(2),               &
     &           UNTOFF(3),DUM,1,0,.FALSE.,BDSTAT(1,8),                 &
     &           AA,ISATID,II(KYAWID),ROT,AA(KYAWBS),ALTPT)
!      RQ=XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)+XSM(I,3)*XSM(I,3)
!      RQ=DSQRT(RQ)
!      XQ1=XSM(I,1)/RQ
!      XQ2=XSM(I,2)/RQ
!      XQ3=XSM(I,3)/RQ
      TOTROT(1,1)=ROT(1)
      TOTROT(2,1)=ROT(2)
      TOTROT(3,1)=ROT(3)
      TOTROT(1,2)=ROT(4)
      TOTROT(2,2)=ROT(5)
      TOTROT(3,2)=ROT(6)
      TOTROT(1,3)=ROT(7)
      TOTROT(2,3)=ROT(8)
      TOTROT(3,3)=ROT(9)
      CALL MATPRD(TOTROT,ATROT(1,1,I),ROT,3,3,3)
! SINCE THE POINTING VECTOR IS LOADED INTO ALTPT
! AND NOT (ROT7-9) ATTITUDE ADJUSTMENT AN NOT
! BE DONE WITH CRYOSAT-2 ALTIMETRY
      IF(LALT) THEN
!      WRITE(6,78919)
         UI(I,1)=+ALTPT(1)
         UI(I,2)=+ALTPT(2)
         UI(I,3)=+ALTPT(3)
         GO TO 90
!     ELSE
!      WRITE(6,78920)
      ENDIF
!      WRITE(6,78921)
!      WRITE(6,78922) XQ1,XQ2,XQ3
!      WRITE(6,78923)
!      WRITE(6,78922) ROT(1),ROT(4),ROT(7)
!      WRITE(6,78922) ROT(2),ROT(5),ROT(8)
!      WRITE(6,78922) ROT(3),ROT(6),ROT(9)
78919   FORMAT(' TRKCR2 FOR ALTIMETRY')
78920   FORMAT(' TRKCR2 FOR GROUND TRACK')
78921   FORMAT(' NORMALIZED SAT STATE FOR GROUND TRACK')
78922   FORMAT(' ',F15.10,5X,F15.10,5X,F15.10)
78923   FORMAT(' CRYOSAT-2 ROTATION MATRIX FOR GROUND TRACK')
      IF(LOFFAJ) THEN
        IF(L1ST) THEN
         DWRKDO(I,1)=UI(I,1)*ROT(1)                                     &
     &              +UI(I,2)*ROT(2)                                     &
     &              +UI(I,3)*ROT(3)
         DWRKDO(I,2)=UI(I,1)*ROT(4)                                     &
     &              +UI(I,2)*ROT(5)                                     &
     &              +UI(I,3)*ROT(6)
         DWRKDO(I,3)=UI(I,1)*ROT(7)                                     &
     &              +UI(I,2)*ROT(8)                                     &
     &              +UI(I,3)*ROT(9)
        ELSE
         DWRKDO(I,1)=-UI(I,1)*ROT(1)                                    &
     &               -UI(I,2)*ROT(2)                                    &
     &               -UI(I,3)*ROT(3)
         DWRKDO(I,2)=-UI(I,1)*ROT(4)                                    &
     &               -UI(I,2)*ROT(5)                                    &
     &               -UI(I,3)*ROT(6)
         DWRKDO(I,3)=-UI(I,1)*ROT(7)                                    &
     &               -UI(I,2)*ROT(8)                                    &
     &               -UI(I,3)*ROT(9)
        ENDIF
      ENDIF
!!!ADD UNIT 22 STATION PHASE CORRECTION, jtw 20130416
         CALL FNDNUM(MTYPE,NANTMT,99,IDUM)
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


!-------------------------------------------------------------------------------
      IF( LPHC ) THEN

          if( i == 1 )then

              !write(6,'(A,3(1x,E15.7))') &
              !      'trkcr2: XV, YV, ZV ', XV, YV, ZV
              !write(6,'(A,2(1x,I10))')  'trkcr2: I, IMQP ', i, imqp

              !if( NANTPT > 0 .and. L_ant_phc )then
              !    write(6,'(A,3(1x,I10))') &
              !          'trkcr2: iant, kantps(iant), ipoff ', &
              !                   iant, kantps(iant), ipoff
              !endif ! L_ant_phc


              if( L_sat_phc )then

                  IPOFF = KANTPS_sat(index_sat)

                  NDIMA=INT(ANTTAB(IPOFF+3)+0.0001D0)
                  NDIMZ=INT(ANTTAB(IPOFF  )+0.0001D0)

                  !write(6,'(A,4(1x,I10))') &
                  !      'trkcr2: sa index_sat, IPOFF, NDIMA, NDIMZ ', &
                  !                  index_sat, IPOFF, NDIMA, NDIMZ

                  !write(6,'(A,1x,E15.7,1x,I10)') &
                  !      'trkcr2: sa ANTTAB(IPOFF+3) , NDIMA   ', &
                  !                  ANTTAB(IPOFF+3) , NDIMA
                  !write(6,'(A,1x,E15.7,1x,I10)') &
                  !      'trkcr2: sa ANTTAB(IPOFF  ) , NDIMZ   ', &
                  !                  ANTTAB(IPOFF  ) , NDIMZ

                  !write(6,'(A,4(1x,I10))') &
                  !      'trkcr2: satid &
                  !      &...index_sat, ii( KANTYP + (index_sat-1) * 2 )
                  !          index_sat, ii( KANTYP + (index_sat-1) * 2 )
                  !write(6,'(A,4(1x,I10))') &
                  !      'trkcr2: ant ...  &
                  !      &index_sat, ii( KANTYP + (index_sat-1) * 2 +1) '
                  !       index_sat, ii( KANTYP + (index_sat-1) * 2 +1)


              endif ! L_sat_phc

          endif ! i == 1


!          write(6,'(A)') 'trkcr2: call antsac '

          CALL ANTSAC(ROT,XV,YV,ZV,ANTTAB(IPOFF+6),NDIMA,NDIMZ,   &
     &                ANTTAB(IPOFF),ISEQ,PHC(I),2,.FALSE.)

      ENDIF  ! LPHC


!-------------------------------------------------------------------------------


! COMPUTE INERTIAL OFFSET VECTOR
   90 CONTINUE
      RA(I,1) = UNTOFF(1)*VMAG
      RA(I,2) = UNTOFF(2)*VMAG
      RA(I,3) = UNTOFF(3)*VMAG
  100 END DO
!
!...RANGE CORRECTION IS DOT PRODUCT OF INERTIAL OFFSET AND
!...UNIT INERTIAL TOPOCENTRIC VECTORS
      CALL DOTPRD(RA,UI,DR,NM,NM,NM,3)
      IF (IDORTAB.GT.0) THEN
           DO I=1,NM
              DR(I)=DR(I)+PHC(I)
           ENDDO
      END IF

!-------------------------------------------------------------------------------

      IF(LPHC) THEN
        IF(L1ST) THEN
           DO I=1,NM
              DR(I)=DR(I)+PHC(I)
           ENDDO
        ELSE
           DO I=1,NM
              DR(I)=DR(I)-PHC(I)
           ENDDO
        ENDIF ! L1ST
!        write(6,'(A,1x,E20.10)')'trkcr2: phc(1) ', phc(1)

      ENDIF ! LPHC

!-------------------------------------------------------------------------------

      RETURN
60000 FORMAT(' WARNING !!!!!!!!!!!!!!!!')
60001 FORMAT(' TRKCR2 CALLED FOR GPS MEASUREMENTS:')
60002 FORMAT(' THIS ROUTINE DOES NOT COMPUTE:')
60003 FORMAT('   (1) PHASE WIND UP CORRECTION')
60004 FORMAT('   (2) ANTENNA MAP CORRECTIOINS')
60005 FORMAT('   (3) ANTENNA FRAME EDITING (ANTCUT)')
60006 FORMAT(' TO ADD THESE FEATURES SEE TRKEXT OR TRKTOP')
      END
