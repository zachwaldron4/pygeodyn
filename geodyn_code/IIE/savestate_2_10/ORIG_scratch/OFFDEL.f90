!$OFFDEL
      SUBROUTINE OFFDEL(ISATID,ISTAID,FREQ  ,                           &
     &                  ISATOF,ISATDL,ISATCG,ISTADL,                    &
     &                  FRQOFF,FRQSAT,FRQSTA,MJDSCG,JCOORD,             &
     &                  JXYZOF,JSADLY,JSTDLY,JXYZCG,MJDFCG,             &
                        IANT,iantof2, JXYOF2,LNSCID,IANTOF,LEXTOF,      &
     &                  LOFEXT,L1ST,II,FRQLNK,ISEQ,AA,JXYOF3)
!********1*********2*********3*********4*********5*********6*********7**
! OFFDEL           00/00/00            0000.0    PGMR - T. MARTIN
!
! FUNCTION:  TAKES CARE OF ANTENNA OFFSETS OR TRANSPONDER DELAYS
!            IN MEASUREMENT MODELLING.
!
! I/O PARAMETERS:
!
! NAME   I/O A/S   DESCRIPTION OF PARAMETERS
! ------ --  --   ------------------------------------------------
! ISATID  I   S   SATELLITE ID
! ISTAID  I   S   STATION ID
! FREQ    I   S   REFERENCE FREQUENCY FROM THE BLOCK HEADER RECORD
! ISATOF  I   A   ARRAY OF SAT ID'S FOR WHICH OFFSET CARDS WERE INPUT
! ISATDL  I   A   ARRAY OF SAT ID'S FOR WHICH DELAY  CARDS WERE INPUT
! ISATCG  I   A   ARRAY OF SAT ID'S FOR WHICH CGMASS CARDS WERE INPUT
! ISTADL  I   A   ARRAY OF STA NO.S FOR WHICH DELAY  CARDS WERE INPUT
! FRQOFF  I   A   FOR OFFSET CORRECTIONS THIS FREQUENCY IS USED
!                 TO DISCRIMINATE BETWEEN ANTENNAE WHEN MORE THAN
!                 ONE ANTENNA IS ON A SATELLITE
! FRQSAT  I   A   FREQUENCY USED TO DISCRIMINATE BETWEEN TRANSPONDER
!                 DELAYS WHEN MORE THAN ONE SATELLITE IS IN THE RUN
! FRQSTA  I   A   FREQUENCY USED TO DISCRIMINATE BETWEEN TRANSPONDER
!                 DELAYS WHEN MORE THAN ONE STATION HAS DELAYS
! MJDSCG  I   A   MJD SECONDS FOR CENTER OF MASS CORRECTION AS GIVEN
!                 ON CGMASS INPUT CARDS

! JCOORD  O   S   POINTER IN II TO CONTENT OF COLS 14-17 OF THE OFFSET
!                 CARD OF THE ANTENNA THAT IS IN PLAY

! JXYZOF  O   S   POINTER TO WHERE THE XYZ FOR THE OFFSET CORRECTION
!                 IS STORED
! JSADLY  O   S   POINTER TO WHERE THE SATELLITE DELAY CURVE
!                 INFORMATION IS STORED
! JSTDLY  O   S   POINTER TO WHERE THE STATION DELAY CURVE
!                 INFORMATION IS STORED
! JXYZCG  O   S   POINTER TO WHERE THE XYZ FOR THE CENTER OF MASS
!                 CORRECTION IS STORED
! MJDFCG  I/O  A   MJD SECONDS FOR CENTER OF MASS CORRECTION THAT
!                 PERTAIN TO THIS BLOCK OF DATA
! IANT    I   S   ANTENNA NUMBER FOR NEW PREPRO ID SCHEME FOR OFFSET
!                 WHEN = 0 THEN USE OLD FREQ. SCHEME
!                 IF THE MEASUREMENT TYPE IS LASER ALTIMETRY IANT
!                 IS THE LASER ID CARRIED ON BLKDAT(4,1)
! IANTOF2 I   A   array of (sat,ant no.) pairs from OFFSET cards for PHC
! JXYOF2  I   S   POINTER TO WHERE THE XYZ OF THE SECOND LINK OF THE
!                 OFFSET IS STORED
! LNSCID  I   A   =.TRUE. THEN NEW PREPRO ANT. ID SCHEME IS REQUESTED
!                 OFFSET IS STORED
! IANTOF  I   A   ANTENNA ID FROM OFFSET CARD
! LEXTOF
! LOFEXT
! L1ST
! II
! FRQLNK
! ISEQ
! AA
! JXYOF3
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      use antphc_module

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/APHASE/NANT_sat,NANTPS_sat(999),KANTPS_sat(999),           &
     &              nant_sta,NANTPS_sta(999),KANTPS_sta(999),           &
     &              NANTPT,  NANTPS(999),    KANTPS(999), NANTMT(99),   &
     &              NNUMMT, NXAPHA
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
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
      COMMON/CORI06/KPTRAU,KPTRUA,KNFMG ,KIPTFM,KILNFM,KIGPC ,          &
     &              KIGPS ,KIGPCA,KIGPSA,KIELMT,KMFMG ,KTPGPC,          &
     &              KTPGPS,KTPUC ,KTPUS,                                &
     &              KLINK,KPTFAU,KPTFUA,NXCI06
      COMMON/DELOFF/NOFFST,NSATDL,NSTADL,NCGMAS,NXDELO
      COMMON/ION2EL/LIONC,LFRQOP,LFRQOF,NXIONL
      COMMON/GPSBLK/LGPS,LNOARC,LSPL85,NXGPS
      COMMON/LANTID/LANTBD,LASALT
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
      COMMON/OFFASI/IOFFAP
      COMMON/OFFASL/LOFFA1,LOFFA2
      COMMON/PYUA  /XGPSTB,WVL3,VHIGH(3,3,4),VLOW(3,3,4),PYUSQ(5),      &
     &              VISATH(4),VISATL(4),XANTSL(4),XCUTSL(4),XANTSH(4)
!
      DIMENSION ISATOF(1),ISATDL(1),ISATCG(1),ISTADL(1),                &
     &          FRQOFF(1),FRQSAT(1),FRQSTA(1),MJDSCG(1),                &
     &          JXYZCG(2),MJDFCG(2),LNSCID(1),IANTOF(1),                &
     &          LEXTOF(1)
      DIMENSION ANTOR(3,3)
      DIMENSION IANTOF2(2, * )
      DIMENSION AA(1),II(1)
!
      DATA ZERO/0.0D0/,TOL/0.50D0/

      LOGICAL ::  L_sat_phc
      LOGICAL ::  L_sta_phc
      LOGICAL ::  L_ant_phc

!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************

       JCOORD=-99
!      write(6,'(/A,2(1x,I10))') &
!            'offdel: at entry NSATDL, IANT', NSATDL, IANT
!      write(6,'(A,2(1x,I10))')  &
!            'offdel: at entry ISATID,ISTAID', ISATID,ISTAID
!      write(6,'(A,1x,I3)')      &
!            'offdel: at entry MTYPE ', MTYPE
!      write(6,'(A,1x,I3)')      &
!            'offdel: at entry ISEQ ', ISEQ
!      write(6,'(A,3(1x,I6)/)')  &
!            'offdel: NANT_sat, NANT_STA, NANTPT ', &
!                     NANT_sat, NANT_STA, NANTPT
!      write(6,'(A,1x,I6)')  &
!            'offdel: at entry IANTOF ', IANTOF

      !XANTSH(ISEQ)=-99.D0
      !XANTSL(ISEQ)=-99.D0
!      write(6,'(A,1x,I10,2x,F10.2)') &
!            'offdel:1 ISEQ, XANTSH(ISEQ) ', &
!                      ISEQ, XANTSH(ISEQ)
!      write(6,'(A,1x,I10,2x,F10.2)') &
!            'offdel:1 ISEQ, XANTSL(ISEQ) ', &
!                      ISEQ, XANTSL(ISEQ)

! II(KANTYP+...) has the satellite and station id's and
!                        antenna numbers (if any)



      IF( LGPS) THEN
          !IF(MTYPE.EQ.40.OR.MTYPE.EQ.51) THEN
          FRQTOL=.12D0*FREQ
          IF(FREQ.GT.3.D10) FRQTOL=TOL*FREQ
          !ENDIF
      ELSE
          FRQTOL=TOL*FREQ
      ENDIF

      LIDOF=.FALSE.

!      write(6,*) 'offdel: FREQ, FRQTOL ',  FREQ, FRQTOL

!     JXYZOF=KFRQOF
!     JXYZCG(1)=KFRQOF
!     JXYZCG(2)=KFRQOF
!     MJDFCG(1)=0
!     MJDFCG(2)=0

      !write(6,*) 'offdel: NOFFST ',  NOFFST
!      write(6,'(A,2(1x,I10))') 'offdel: lasalt,noffst: ', &
!                                        lasalt,noffst

      IF(NOFFST.LE.0) GO TO 5000
!
! IF IANT .GT. ZERO USE NEW PREPRO SCHEME TO FIND OFFSET
! TEST ONLY OFFSETS THAT HAVE REQUESTED THE NEW SCHEME ON THE 2S CARD
!
! .... LASER ALTIMETER
!      write(6,*) 'offdel: lasalt,noffst: ',lasalt,noffst

      IF(LASALT)  THEN

!         write(6,*) 'offdel: IANT ',IANT

         IF(IANT.EQ.0) GOTO 2000

         DO 300 I=1,NOFFST
          !      print *,'offdel: isatid: ',isatid
          IF(ISATID.NE.ISATOF(I)) GOTO 300
          ITMP=IANTOF(I)/100
          ITMP=IANTOF(I)-ITMP*100
!          write(6,*) 'offdel: I, IANTOF(I), ITMP: ',I, IANTOF(I), ITMP

          IF(ITMP.LT.50) GOTO 300

          IANTCR=ITMP-50

!          write(6,*) 'offdel: IANT, IANTCR ', IANT, IANTCR
!          write(6,*) 'offdel: I, LEXTOF(I) ', I,LEXTOF(I)

          IF(IANT.NE.IANTCR) GOTO 300
          JXYZOF=KXYZOF+(I-1)*3
          LIDOF=.TRUE.
          JXYOF2=KXYOF2+(I-1)*3
          JXYOF3=KXYOF3+(I-1)*3
          JCOORD=KIANTO+I-1
          IF(LIONC.AND.LFRQOP)FRQLNK=FRQOFF(I)

          IF(LEXTOF(I)) LOFEXT=.TRUE.

          GOTO 2000

300       CONTINUE

          !write(6,*)'ABNORMAL TERMINATION IN OFFDEL'
          !write(6,*)'LASER ALTIMETER ID SCHEME SELECTED'
          !write(6,*)'NO MATCH FOUND ON OFFSET CARDS FOR: ',IANT
          STOP 'offdel:bad laser alt offset'

       ENDIF !  LASALT


!-------------------------------------------------------------------------

      ! IF IANT .GT. ZERO -- USE NEW PREPRO SCHEME TO FIND OFFSET

      ! TEST ONLY OFFSETS THAT HAVE REQUESTED
      !  THE NEW SCHEME ON THE 2S CARD
      ! this means LNSCID = true

      !write(6,'(A,1x,I6)') 'offdel:2 IANT ',IANT
      !write(6,'(A,1x,I3)') 'offdel:2 ISEQ ', ISEQ

      IF( IANT.GT.0 ) THEN

         DO 400 I=1,NOFFST
          IF(ISATID.NE.ISATOF(I)) GOTO 400
          IF(.NOT.LNSCID(I)) GOTO 400
          ITMP=IANTOF(I)/100
          ITMP=IANTOF(I)-ITMP*100
          IF(IANT.NE.ITMP) GOTO 400
          JXYZOF=KXYZOF+(I-1)*3
          LIDOF=.TRUE.
          JXYOF2=KXYOF2+(I-1)*3
          JXYOF3=KXYOF3+(I-1)*3
          JCOORD=KIANTO+I-1

!          write(6,*) 'offdel: I, JXYZOF   ', I,JXYZOF
!          write(6,*) 'offdel: I, JXYZOF2  ', I,JXYZOF2
!          write(6,*) 'offdel: I, JXYZOF3  ', I,JXYZOF3
!          write(6,*) 'offdel: I, FRQOFF(I)', I,FRQOFF(I)

          IF(LIONC.AND.LFRQOP)FRQLNK=FRQOFF(I)
          IF(LEXTOF(I)) LOFEXT=.TRUE.
          GO TO 2000
  400     CONTINUE

      ENDIF !  IANT.GT.0



!----------------------------------------------------------------------

!      write(6,'(A)') 'offdel: 3 check iantof2'
!      write(6,'(A,1x,I3)') 'offdel: ISEQ ', ISEQ

      do  I=1,NOFFST


          IF(ISATID.NE.IANTOF2(1,I)) cycle

!          write(6,'(A,1x,I6,2(1x,I10))')&
!                'offdel:3 I, ISATID, IANTOF2(1,I)', &
!                          I, ISATID, IANTOF2(1,I)

          iant = iantof2(2,I)

          !write(6,'(/A,1x,I6,2(1x,I10)/)')&
          !      'offdel:3 I, IANTOF2(2,I), IANT',&
          !                I, IANTOF2(2,I), IANT

          xantsh(ISEQ) = iant    ! jjm 20130204

          !write(6,'(A,1x,I6,1x,E15.7)')&
          !      'offdel:3 ISEQ, xantsh(ISEQ) ',&
          !                ISEQ, xantsh(ISEQ)
          exit

      enddo


      if( nant_sat > 0 )then

!          write(6,'(a,2(1x,I10))') &
!              'offdel:4 call get_index2 for isatid, iant =', &
!                                            isatid, iant
!          write(6,'(A,1x,I3)') 'offdel: ISEQ ', ISEQ

          call get_index2( isatid, iant, II(KANTYP),  NANT_sat, &
                           index_sat, L_sat_phc )

!          write(6,'(A,1x,I10,2(1x,I4), 4x, L1)') &
!                 'offdel:4 ISATID, iant, index_sat, L_sat_phc   ', &
!                           ISATID, iant, index_sat, L_sat_phc

      endif !   nant_sat > 0

      !!!!!if( iant > 0 ) go to 2000  ! jjm 20130621

!----------------------------------------------------------------------

! IF NEW SCHEME ANT ID NOT REQUESTED  OR  NO OFFSET MATCH FOUND
! THEN USE OLD FREQ. DISCRIMINATION SCHEME

!      write(6,'(A)') 'offdel:1000 old freq scheme  '

      DO 1000 I=1,NOFFST

       IF(ISATID.NE.ISATOF(I)) GO TO 1000

       IF(LNSCID(I)) GOTO 1000
       IF(FRQOFF(I).LE.ZERO) GO TO 500
       FRQDIF=ABS(FREQ-FRQOFF(I))
       IF(FRQDIF.GT.FRQTOL) GO TO 1000
  500 CONTINUE

       IF(LIONC.AND.LFRQOP)FRQLNK=FRQOFF(I)
       JXYZOF=KXYZOF+(I-1)*3
       LIDOF=.TRUE.
       JXYOF2=KXYOF2+(I-1)*3
       JXYOF3=KXYOF3+(I-1)*3
!        write(6,*) 'offdel:old I, JXYZOF   ', I,JXYZOF
!        write(6,*) 'offdel:old I, JXYZOF2  ', I,JXYZOF2
!        write(6,*) 'offdel:old I, JXYZOF3  ', I,JXYZOF3
!        write(6,*) 'offdel:old I, FRQOFF(I)', I,FRQOFF(I)

       JCOORD=KIANTO+I-1
       IF(LEXTOF(I)) LOFEXT=.TRUE.
       GO TO 2000
 1000 END DO
!
      GO TO 5000
!
 2000 CONTINUE

      ! CGMASS offset
      !write(6,'(A,2(1x,I10))')'offdel: aft 2000  MJDFCG ', MJDFCG
      !write(6,'(A,2(1x,I10))')'offdel: aft 2000  JXYZCG ', JXYZCG
      !write(6,*) &
      !  'offdel:2000 number of cgmass values  NCGMAS ', NCGMAS
      !write(6,*)'offdel:2000 NCGMAS, ISATID ', &
      !                       NCGMAS, ISATID

      ! FIND THE LAST CGMASS CARD WITH A START TIME BEFORE THE CURRENT
      ! MEASUREMENT TIME
      L_CGMASS_FOUND = .FALSE. ! WILL BE SET TO .TRUE. IF A MATCHING
                               ! CGMASS CARD HAS BEEN FOUND
      DO I = 1, NCGMAS
          ! MAKE SURE THE SATELLITE ID MATCHES AND THE CGMASS CARD START
          ! TIME IS BEFORE THE CURRENT MEASUREMENT TIME
          IF (ISATID == ISATCG(I) .AND. MJDSBL >= MJDSCG(I)) THEN
              ! MAKE SURE THE CGMASS CARDS FOR THIS SATELLITE ARE IN
              ! TIME ORDER
              IF (L_CGMASS_FOUND) THEN
                  IF (MJDSCG(I) < MJDFCG(1)) THEN
                      WRITE(*,'(A)') 'ERROR: CGMASS CARDS MUST BE IN &
                                     &TIME ORDER'
                      STOP 1
                  END IF
              END IF
              L_CGMASS_FOUND = .TRUE.

              ! OVERWRITE THE VALUES FROM PREVIOUS CGMASS CARDS.
              ! FOR HISTORICAL REASONS, THERE ARE TWO VALUES FOR MJDFCG
              ! AND JXYZCG, BUT THEY ARE ALWAYS THE SAME (SOME ROUTINES
              ! USE ONE AND SOME USE THE OTHER)
              MJDFCG(1)=MJDSCG(I)
              MJDFCG(2)=MJDFCG(1)
              ! ... SET THE POINTER (IN THE AA ARRAY) TO THE CURRENT
              ! ... CGMASS OFFSET
              JXYZCG(1)=KXYZCG+(I-1)*3
              JXYZCG(2)=JXYZCG(1)
          END IF
      END DO

 5000 CONTINUE

      !write(6,'(A,2(1x,I10))')'offdel: aft 5000  MJDFCG ', MJDFCG
      !write(6,'(A,2(1x,I10))')'offdel: aft 5000  JXYZCG ', JXYZCG

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!--------------------------------------------------------------------------

!      satellite delay

!     JSADLY=KFRQSA

!      write(6,'(A,1x,I10)')'offdel: aft 5000  NSATDL ', NSATDL

      IF(NSATDL.LE.0) GO TO 7000
      DO 6000 I=1,NSATDL
      IF(ISATID.NE.ISATDL(I)) GO TO 6000
      IF(FRQSAT(I).LE.ZERO) GO TO 5500
      FRQDIF=ABS(FREQ-FRQSAT(I))
      IF(FRQDIF.GT.FRQTOL) GO TO 6000
 5500 CONTINUE
      JSADLY=KSADLY+(I-1)
      GO TO 7000
 6000 END DO


 7000 CONTINUE

!--------------------------------------------------------------------------

!     station delay

!     JSTDLY=KFRQST

      IF(NSTADL.LE.0) GO TO 9000
      DO 8000 I=1,NSTADL
      IF(ISTAID.NE.ISTADL(I)) GO TO 8000
      IF(FRQSTA(I).LE.ZERO) GO TO 7500
      FRQDIF=ABS(FREQ-FRQSTA(I))
      IF(FRQDIF.GT.FRQTOL) GO TO 8000
 7500 CONTINUE
      JSTDLY=KSTDLY+(I-1)
      GO TO 9000
 8000 END DO

 9000 CONTINUE

!--------------------------------------------------------------------------

!   FILL IN ANTENNA ORIENTATION INFORMATION (IF POSSIBLE)

!   XGPSTB - GT 0 IF THE CURRENT BLOCK IS A GPS MEASUREMENT BLOCK
!                 BEING CORRECTED FOR PHASE WINDUP

!      write(6,'(A,3x,L1)')'offdel: aft 9000  LIDOF ', LIDOF
!      write(6,'(A,1x,F10.2,1x,I10)')'offdel: XGPSTB, MTYPE   ', &
!                                             XGPSTB, MTYPE
!      write(6,'(A,1x,I3)') 'offdel:aft 9000  ISEQ ', ISEQ

      LAOR=.FALSE.
      IF( XGPSTB  <=  0.D0  .AND.   &
          MTYPE   /=   39   .AND.   &
          MTYPE   /=   51             ) GO TO 9500

      IF(.NOT.LIDOF) GO TO 9500

      IQPP=1+(JXYZOF-KXYZOF)/3

      ! call fishor to:
      ! "FISH THE GPS ANTENNA ORIENTATION UNIT VECTORS OUT OF
      !  THE BIG ANTENNA ORIENTATION ARRAY PASSED FROM IIS"

!      write(6,'(A,1x,I10)')'offdel: call FISHOR  IQPP = ', IQPP

      CALL FISHOR(AA(KANTOR),NOFFST,IQPP,ANTOR)


      ! ISATH is sat id for high satellite
      ! ISATL is sat id for low  satellite

      ISATH=(VISATH(ISEQ)+.05D0)
      ISATL=(VISATL(ISEQ)+.05D0)

!      write(6,*)'offdel: VISATH ', VISATH
!      write(6,*)'offdel: VISATL ', VISATL

!      write(6,'(A,3(1x,I10))')'offdel: ISATID, ISATH, ISATL', &
!                                       ISATID, ISATH, ISATL
!      write(6,'(A,1x,I3)') 'offdel: ISEQ ', ISEQ


      IF( ISATID .EQ. ISATH ) THEN

          TEST=ANTOR(1,1)*ANTOR(1,1) +  &
               ANTOR(2,1)*ANTOR(2,1) +  &
               ANTOR(3,1)*ANTOR(3,1)
           IF( TEST.GT.0.5D0) THEN
               VHIGH(1,1,ISEQ)=ANTOR(1,1)
               VHIGH(2,1,ISEQ)=ANTOR(2,1)
               VHIGH(3,1,ISEQ)=ANTOR(3,1)
           ENDIF
           TEST=ANTOR(1,2)*ANTOR(1,2) + &
                ANTOR(2,2)*ANTOR(2,2) + &
                ANTOR(3,2)*ANTOR(3,2)
           IF( TEST.GT.0.5D0) THEN
               VHIGH(1,2,ISEQ)=ANTOR(1,2)
               VHIGH(2,2,ISEQ)=ANTOR(2,2)
               VHIGH(3,2,ISEQ)=ANTOR(3,2)
           ENDIF
           TEST=ANTOR(1,3)*ANTOR(1,3) + &
                ANTOR(2,3)*ANTOR(2,3) + &
                ANTOR(3,3)*ANTOR(3,3)
           IF(TEST.GT.0.5D0) THEN
              VHIGH(1,3,ISEQ)=ANTOR(1,3)
              VHIGH(2,3,ISEQ)=ANTOR(2,3)
              VHIGH(3,3,ISEQ)=ANTOR(3,3)
           ENDIF

          !--------------------------------------------------------------

          ! FIND THE GPS ANTENNA NUMBER FOR PCV CORRECTIONS

        IQP=KANGPS+(JXYZOF-KXYZOF)/3

        !write(6,*) 'offdel: kangps, JXYZOF, KXYZOF, IQP ', &
        !                    kangps, JXYZOF, KXYZOF, IQP

        XANTSH(ISEQ)=-99.D0

        !write(6,*) 'offdel: ISEQ, XANTSH(ISEQ) ', &
        !                    ISEQ, XANTSH(ISEQ)
        IANTP=II(IQP)

        !write(6,*) 'offdel: IQP, IANTP ', IQP, IANTP
        !write(6,*) 'offdel: NANTPT ', NANTPT

          IF( NANTPT.GT.0 ) THEN


              !IANTP=II(IQP)

              !write(6,'(A,3(1x,I10))') &
              !      'offdel:a IQP, II(IQP), IANTP ', IQP, II(IQP), IANTP


                XANTSH(ISEQ)= IANTP

!              write(6,'(A,2(1x,I10),2x,F10.2)') &
!                    'offdel:a ISEQ, IANTP, XANTSH(ISEQ) ', &
!                              ISEQ, IANTP, XANTSH(ISEQ)

          ENDIF ! NANTPT.GT.0


          !--------------------------------------------------------------


!          write(6,'(A,1x,I10,2x,I6,1x,I6)') &
!                'offdel: high  ISATID, nant_sat, index_sat ', &
!                               ISATID, nant_sat, index_sat
!          write(6,'(A,1x,I3)') 'offdel: ISEQ ', ISEQ

          IF( nant_sat > 0 .and. L_sat_phc ) THEN

              !XANTSH(ISEQ)= index_sat
              XANTSH(ISEQ)= ii( KANTYP + (index_sat-1)*2 + 1) ! ant. no.

              !write(6,'(A,2(1x,I10),2x,F10.2)') &
              !      'offdel:2 high index_sat, ISEQ, XANTSH(ISEQ) ', &
              !                     index_sat, ISEQ, XANTSH(ISEQ)

          ENDIF ! nant_sat > 0

          !--------------------------------------------------------------

      ENDIF !  ISATID.EQ.ISATH


      IF(ISATID.EQ.ISATL) THEN

        TEST=ANTOR(1,1)*ANTOR(1,1) + &
             ANTOR(2,1)*ANTOR(2,1) + &
             ANTOR(3,1)*ANTOR(3,1)
        IF(TEST.GT.0.5D0) THEN
           VLOW(1,1,ISEQ)=ANTOR(1,1)
           VLOW(2,1,ISEQ)=ANTOR(2,1)
           VLOW(3,1,ISEQ)=ANTOR(3,1)
           LAOR=.TRUE.
        ENDIF
        TEST=ANTOR(1,2)*ANTOR(1,2) + &
             ANTOR(2,2)*ANTOR(2,2) + &
             ANTOR(3,2)*ANTOR(3,2)
        IF(TEST.GT.0.5D0) THEN
           VLOW(1,2,ISEQ)=ANTOR(1,2)
           VLOW(2,2,ISEQ)=ANTOR(2,2)
           VLOW(3,2,ISEQ)=ANTOR(3,2)
           LAOR=.TRUE.
        ENDIF
        TEST=ANTOR(1,3)*ANTOR(1,3) + &
             ANTOR(2,3)*ANTOR(2,3) + &
             ANTOR(3,3)*ANTOR(3,3)
        IF(TEST.GT.0.5D0) THEN
           VLOW(1,3,ISEQ)=ANTOR(1,3)
           VLOW(2,3,ISEQ)=ANTOR(2,3)
           VLOW(3,3,ISEQ)=ANTOR(3,3)
           LAOR=.TRUE.
        ENDIF
        IF( .NOT.LAOR    .AND. &
            MTYPE.EQ.39  .OR.  &
            MTYPE.EQ.51            ) THEN
           VLOW(1,1,ISEQ)=1.D0
           VLOW(2,1,ISEQ)=0.D0
           VLOW(3,1,ISEQ)=0.D0
           VLOW(1,2,ISEQ)=0.D0
           VLOW(2,2,ISEQ)=-1.D0
           VLOW(3,2,ISEQ)=0.D0
           VLOW(1,3,ISEQ)=0.D0
           VLOW(2,3,ISEQ)=0.D0
           VLOW(3,3,ISEQ)=-1.D0
        ENDIF
          !--------------------------------------------------------------

          IQP=KANGPS+(JXYZOF-KXYZOF)/3

          XANTSL(ISEQ)=-99.D0

          !IF( NANTPT.GT.0 ) THEN

              IANTP=II(IQP)

!              write(6,'(A,3(1x,I10))') &
!                    'offdel: low sat  iqp, ii(IQP) , iantp ', &
!                                      iqp, ii(IQP) , iantp

!              write(6,'(A,1x,I3)') 'offdel: ISEQ ', ISEQ

              XANTSL(ISEQ)= IANTP

              !write(6,'(A,2(1x,I10),2x,F10.2)') &
              !      'offdel: ISEQ, IANTP, XANTSL(ISEQ) ', &
              !               ISEQ, IANTP, XANTSL(ISEQ)

          !ENDIF !  NANTPT.GT.0

          !--------------------------------------------------------------

          !write(6,'(A,1x,I10,2x,I6,1x,I6)') &
          !      'offdel: low  ISATID, nant_sat, index_sat ', &
          !                    ISATID, nant_sat, index_sat

          !IF( nant_sat > 0 ) THEN

              !XANTSL(ISEQ)= index_sat

          !    XANTSL(ISEQ)= ii( KANTYP + (index_sat -1) * 2 + 1 )

          !    write(6,'(A,2(1x,I10),2x,F10.2)') &
          !          'offdel:2 low  index_sat, ISEQ, XANTSL(ISEQ) ', &
          !                         index_sat, ISEQ, XANTSL(ISEQ)


          !ENDIF ! nant_sat > 0

          !--------------------------------------------------------------

          IQP=KANTUV+(JXYZOF-KXYZOF)/3

          XCUTSL(ISEQ)=AA(IQP)


      ENDIF ! ISATID.EQ.ISATL



 9500 CONTINUE

      !write(6,'(A,1x,I3)') 'offdel: aft 9500 ISEQ ', ISEQ
!-------------------------------------------------------------------------------

      ! do non-GPS meas here for mt = 39 or 51

!      write(6,'(A,1x,I10)') 'offdel:aft 9500 MTYPE ',  MTYPE
!      write(6,'(A,2(1x,I10),2x,F10.2)') &
!            'offdel:9500 index_sat, ISEQ, XANTSH(ISEQ) ', &
!                         index_sat, ISEQ, XANTSH(ISEQ)
!      write(6,'(A,2(1x,I10),2x,F10.2)') &
!            'offdel:9500 index_sat, ISEQ, XANTSL(ISEQ) ', &
!                         index_sat, ISEQ, XANTSL(ISEQ)


      IF( MTYPE .EQ. 51 .OR. MTYPE .EQ. 39 ) THEN

          IQP=KANGPS+(JXYZOF-KXYZOF)/3
          XANTSL(ISEQ)=-99.D0

          !--------------------------------------------------------------

          !IF( NANTPT .GT. 0 ) THEN

              IANTP=II(IQP)

              !write(6,'(A,3(1x,I10))')&
              !'offdel:non_gps IQP, II(IQP),IANTP ', IQP, II(IQP), IANTP

              XANTSL(ISEQ)= IANTP

              !write(6,'(A,1x,I10,2x,F10.2)') &
              !      'offdel:non-gps  ISEQ, XANTSL(ISEQ) ', &
              !                       ISEQ, XANTSL(ISEQ)

          !ENDIF !  NANTPT.GT.0

          !--------------------------------------------------------------

          ! put an antphc code section for non-gps?

!          write(6,'(A,1x,I10,2x,I6,1x,I6)') &
!                'offdel: non-gps ISATID, nant_sat, index_sat ', &
!                                 ISATID, nant_sat, index_sat

          !IF( nant_sat > 0 ) THEN

          !    !XANTSH(ISEQ)= index_sat
          !    !XANTSH(ISEQ)= ii( KANTYP + (index_sat -1) * 2 + 1)
          ! ?? set for non-gps?

          !    write(6,'(A,2(1x,I10),2x,F10.2)') &
          !          'offdel:2 non-gps index_sat, ISEQ, XANTSH(ISEQ) ', &
          !                            index_sat, ISEQ, XANTSH(ISEQ)


          !    ! jjm 20121206 >>>

          !    !XANTSL(ISEQ)= index_sat
          !    XANTSL(ISEQ)= ii( KANTYP + (index_sat -1) * 2 + 1)

          !    write(6,'(A,2(1x,I10),2x,F10.2)') &
          !          'offdel:2 non-gps index_sat, ISEQ, XANTSL(ISEQ) ', &
          !                            index_sat, ISEQ, XANTSL(ISEQ)
          !    ! jjm 20121206 <<<



          !ENDIF ! nant_sat > 0

          !--------------------------------------------------------------


      ENDIF !  MTYPE.EQ.51.OR.MTYPE.EQ.39


!      write(6,'(A,1x,I10)')                                             &
!            'offdel: aft 9800  NPVAL0(IXOFFS) ', NPVAL0(IXOFFS)
!      write(6,'(A,3x,L1)' )'offdel: aft 9800  L1ST ', L1ST
!
!      write(6,'(A,1x,I3)') 'offdel:977  ISEQ ', ISEQ

!-----------------------------------------------------------------------

      ! this section for OFFADJ - adjustment of offset bias

      if( NPVAL0(IXOFFS) .LE. 0 ) then

          !write(6,*)'offdel: at return NPVAL0(IXOFFS) ', NPVAL0(IXOFFS)
          !write(6,*)'offdel: at return LOFFA1, LOFFA2 ', LOFFA1, LOFFA2
          !write(6,'(A,1x,I10,2x,F10.2)') &
          !      'offdel: at return1  ISEQ, XANTSH(ISEQ) ', &
          !                           ISEQ, XANTSH(ISEQ)
          !write(6,'(A,1x,I10,2x,F10.2)') &
          !      'offdel: at return1  ISEQ, XANTSL(ISEQ) ', &
          !                           ISEQ, XANTSL(ISEQ)

          RETURN

      endif

      IPT=II(2*(JXYZOF-KXYZOF)+IPVAL(IXOFFS)+KPTRUA-1)

      !write(6,*)'offdel: JXYZOF, KXYZOF ',  JXYZOF, KXYZOF
      !write(6,*)'offdel: IPVAL(IXOFFS)  ',  IPVAL(IXOFFS)
      !!write(6,*)'offdel: KPTRUA         ',  KPTRUA
      !write(6,*)'offdel: 2*(JXYZOF-KXYZOF)+IPVAL(IXOFFS)+KPTRUA-1 ', &
      !                   2*(JXYZOF-KXYZOF)+IPVAL(IXOFFS)+KPTRUA-1

!      write(6,*)'offdel: IPT ', IPT


      IF( IPT.GT.0 ) THEN
          IOFFAP=IPT
!          write(6,*)'offdel: IOFFAP ', IOFFAP
          IF( L1ST ) THEN
              LOFFA1=.TRUE.
          ELSE
              LOFFA2=.TRUE.
          ENDIF !  L1ST
      ENDIF ! IPT.GT.0

       !write(6,'(A,2(3x,L1))')&
       !      'offdel: at return LOFFA1, LOFFA2 ', LOFFA1, LOFFA2
       !write(6,'(A,1x,I10,2x,F10.2)') &
       !      'offdel: at return2  ISEQ, XANTSH(ISEQ) ', &
       !                           ISEQ, XANTSH(ISEQ)
       !write(6,'(A,1x,I10,2x,F10.2)') &
       !      'offdel: at return2  ISEQ, XANTSL(ISEQ) ', &
       !                           ISEQ, XANTSL(ISEQ)

!-----------------------------------------------------------------------

      RETURN
      END
