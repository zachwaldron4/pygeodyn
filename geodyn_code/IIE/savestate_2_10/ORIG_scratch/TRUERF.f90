!$TRUERF
      SUBROUTINE TRUERF(IELMTP,IELMCS,IELMCI,MJDRF,FSECRF,MJDEP,FSECEP, &
     &                  ELEMS,PARMV,ISET,ISAT,AA,II,UTDT)
!***********************************************************************
!    DATE      06/12/86                           PGMR D. ROWLANDS
!    MODS:
!    DATE      10/09/90                           PGMR S. B. LUTHCKE
!
!    PURPOSE:  CONVERT ELEMENTS FROM VARIOUS COORDINATE SYSTEMS INTO
!              TRUE OF REFERENCE
!
!    INPUT     IELMTP=TYPE OF ELEMENTS (SAME NUMBER CONVENTION AS ON
!                     ELEMS1 CARD
!              IELMCS=INPUT COORDINATE SYSTEM (SAME NUMBER CONVENTION
!                     AS ON ELEMS1 CARD)
!                     Coordinate System of Input Elements
!                        0 True of Reference Date earth equator
!                        1 Mean of Reference Date earth equator
!                        2 True of Date earth equator
!                        3 Mean of Date earth equator
!                        4 Mean of 1950 earth equator
!                        5 Mean of 1950 ecliptic
!                        6 J2000 (= mean of Jan. 1.5, 2000)
!              IELMCI=BODY THAT INPUT ELEMENTS ARE CENTERED TO
!                     (SAME NUMBER CONVENTION AS ON ELEMS1 CARD)
!              MJDRF =INTEGER SECONDS OF REFERENCE TIME
!              FSECRF=FRACTIONAL SECONDS OF REFERENCE TIME
!              MJDEP =INTEGER SECONDS ASSOCIATED WITH COORDINATE SYSTEM
!                     OF INPUT
!              FSECEP=FRACTIONAL SECONDS ASSOCIATED WITH COORDINATE
!                     SYSTEM OF INPUT
!              ELEMS =INPUT ELEMENTS
!              PARMV =ARRAY OF CURRENT PARAMETER VALUES
!              ISET  =SET NO OF CURRENT SATELLITE
!              ISAT  =SAT NO WITHIN CURRENT SET
!              AA    =REAL DYNAMIC SPACE
!              II    =INTEGER DYNAMIC SPACE
!    OUTPUT    ELEMS =INPUT ELEMNTS IN TRUE OF REF
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/CBDSTA/BDSTAT(7,999),XBDSTA
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
      COMMON/CORI07/KSMSA1,KSMSA2,KSMSA3,KSMST1,KSMST2,KSMST3,KSMTYP,   &
     &              KSMT1,KSMT2,KMSDM,KIOUT,KISUPE,NXCI07
      COMMON/CREFMT/REFMT(9)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/EPHMPT/NTOT,NTOTS,NNPDPR,NUTORD,IBODDG(8),IGRPDG(4),       &
     &              NENPD,INRCD(2),KPLAN(11),NINEP,NEPHMR,NEPHEM,       &
     &              IBHINT,NINTBH,NXEPHP
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/LDUAL/LASTSN,LSTSNX,LSTSNY
      COMMON/LEPHM2/LXEPHM
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
      COMMON/PLNETI/IPLNET(999),IPLNIN(999),MPLNGD(999),MPLNGO(999),   &
     &              IPLNZ(999),NXPLNI
!
      DIMENSION PARMV(1),AA(1),II(1)
      DIMENSION FSEC(1),EF(1),ZF(1),TF(1),ROT(9),ELEMS(6),X1(6),X2(6)
      DIMENSION ELEMP(6)
      DIMENSION DPSI(1),EPST(1),EPSM(1)
      DIMENSION ROTEC(9,2),R50200(9)
      DIMENSION UTDT(1)
      DIMENSION DUM(3),DUM3(3,2,1)
!
      DATA ROTEC/1.0D0,0.0D0,0.0D0,0.0D0,.9174369522511642D0,           &
     &           -.3978811865923022D0,0.D0,                             &
     &           +.3978811865923022D0,.9174369522511642D0,              &
     &           1.0D0,0.0D0,0.0D0,0.0D0,.9174820620691819D0,           &
     &           -.3977771559319137D0,0.D0,                             &
     &           +.3977771559319137D0,.9174820620691819D0/
!
! (APPROX) ROTATION MEAN OF 50 TO MEAN OF 2000 GIVEN BY BIH CIRULAR 163
! "ERRORS OF UP TO 1 ARC SECOND"
      DATA R50200/+.9999257079523629D0,+.0111789381264276D0,            &
     &+.0048590038414544D0,-.0111789381377700D0,+.9999375133499888D0,   &
     &-.0000271579262585D0,-.0048590038153592D0,-.0000271625947142D0,   &
     &+.9999881946023742D0/
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      write(6,*) 'truerf: at entry '
      write(6,*) 'truerf: IELMTP,IELMCS,IELMCI ',                       &
     &                    IELMTP,IELMCS,IELMCI
      write(6,*) 'truerf: MJDRF,FSECRF,MJDEP,FSECEP ',                  &
     &                    MJDRF,FSECRF,MJDEP,FSECEP
      write(6,*) 'truerf: ELEMS,ISET,ISAT ',                            &
     &            ELEMS,ISET,ISAT
!
!     ....define internal planet number corr. to ielmci
!
      do 5 jj=1,11
         if( ielmci .eq. iplnet(jj) ) then
            ielmc2 = iplnin(jj)
            go to 6
         endif
    5 continue
    6 continue
!
!  DETERMINE IF ELEMENTS ARE IN CARTESIAN
      DO 10 JJ=1,6
         X1(JJ)=ELEMS(JJ)
   10 END DO
      write(6,*) 'truerf: 10 x1 ', x1
!
      IF(IELMTP.EQ.1) GO TO 80
!  IF ELEMENTS ARE NOT CARTESIAN THEN THE BODY OF INPUT IS EITHER
!  THE EARTH OR THE SUN
!CCC      GM=PARMV(IPVAL(IXGM))
!CCC      GM =PARMV(IPVAL(IXGM)+IBDGM(ICBDGM)-1)
!     ....central body GM
      GMCB =PARMV(IPVAL(IXGM)+IBDGM(ICBDGM)-1)
      GMCI = bdstat(7, ielmc2 )
!
      write(6,*) 'truerf: GMCB, GMCI ', GMCB, GMCI
!      write(6,*) 'truerf: parmv(ipval(ixgm)) ',
!     &         (parmv(ipval(ixgm)+jjj),jjj=0,12)
      write(6,*) 'truerf: icbdgm , ibdgm(icbdgm) ',                     &
     &                    icbdgm , ibdgm(icbdgm)
      write(6,*) 'truerf: gm ', gm
      IF(IELMCI.EQ.9999) GM=BDSTAT(7,8)
!
      IF(LASTSN.AND.ISET.EQ.1) GMCI=BDSTAT(7,8)
      IF(IELMTP.EQ.2) then
         write(6,*) 'truerf: ielmtp = ', ielmtp, 'call posvel'
         CALL POSVEL(X1,ELEMS,1,GMCI)
      endif
!
      IF(IELMTP.EQ.3) then
!         write(6,*) 'truerf: ielmtp = ', ielmtp, 'call pvnsk'
         CALL PVNSK(X1,ELEMS,1,GMCI)
      endif
!
!C      IF(IELMTP.EQ.2) CALL POSVEL(X1,ELEMS,1,GM)
!C      IF(IELMTP.EQ.3) CALL PVNSK(X1,ELEMS,1,GM)
   80 CONTINUE
!      write(6,*) 'truerf: after 80 x1 ', x1
!C      IF(IELMCS.EQ.0.AND.IELMCI.EQ.300) GO TO 600
      write(6,*) 'truerf: ielmcs, ielmci, iplnet(icbdgm) ',             &
     &                    ielmcs, ielmci, iplnet(icbdgm)

      IF( IELMCS.EQ.0 .AND. IELMCI.EQ.iplnet(icbdgm) ) GO TO 600
      IF(IELMCS.NE.0) GO TO 95
!
! INPUT IS TRUE OF REF AND MAY NOT BE EARTH CENTERD
! CONVERT TO MEAN OF 50/J2000 FOR TRANSLATION
      X2(1) = REFMT(1)*X1(1)+REFMT(4)*X1(2)+REFMT(7)*X1(3)
      X2(2) = REFMT(2)*X1(1)+REFMT(5)*X1(2)+REFMT(8)*X1(3)
      X2(3) = REFMT(3)*X1(1)+REFMT(6)*X1(2)+REFMT(9)*X1(3)
      X2(4) = REFMT(1)*X1(4)+REFMT(4)*X1(5)+REFMT(7)*X1(6)
      X2(5) = REFMT(2)*X1(4)+REFMT(5)*X1(5)+REFMT(8)*X1(6)
      X2(6) = REFMT(3)*X1(4)+REFMT(6)*X1(5)+REFMT(9)*X1(6)
!      write(6,*) 'truerf: after 80 x2 ', x2
      GO TO 130
   95 CONTINUE
! CONVERT TO TRUE OF REFERENCE SYSTEM
      GO TO ( 100,200,300,400,500,450),IELMCS
!
! INPUT MEAN OF REFERENCE
  100 CONTINUE
      write(6,*)'TRUERF: 100 MEAN OF REFERENCE EARTH EQUATOR'
      MJD=MJDRF
      FSEC(1)=FSECRF
  110 CONTINUE
! GET MATRIX TO GO BACK TO MEAN OF 50/J2000
!      CALL BUFPOS(MJD,FSEC(1),MJD,FSEC(1),1,IDUM1,DUM1,LDUM1,AA)
!
      if( ielmci .eq. 300 ) then
!         write(6,*) 'truerf: call bufear '
         CALL BUFEAR(MJD,FSEC(1),MJD,FSEC(1),1,IDUM1,DUM1,LDUM1,AA,II)
!         write(6,*) 'truerf: after call bufear '
      else
!         write(6,*) 'truerf: call bufxtr '
         CALL BUFXTR(MJD,FSEC(1),MJD,FSEC(1),1,IDUM1,DUM1,LDUM1,AA)
!
      IF(LXEPHM) THEN
      DO IJ=1,ICBODY
      CALL BUFSUP(AA,MJD,FSEC(1),MJD,FSEC(1),IDUM,                      &
     &AA(KEPHMS),IJ,1)
      ENDDO
      ENDIF
!
      endif
!
!      write(6,*) 'truerf: call precss '
      CALL PRECSS(MJD,FSEC,EF,ZF,TF,AA(KSRTCH),1)
!      write(6,*) 'truerf: call fmrot '
      CALL FMROT(EF,ZF,TF,ROT,1)
  120 CONTINUE
      write(6,*) 'truerf: 120 rot ', rot
! ROTATE BACK TO MEAN OF 50/J2000
      X2(1)=ROT(1)*X1(1)+ROT(2)*X1(2)+ROT(3)*X1(3)
      X2(2)=ROT(4)*X1(1)+ROT(5)*X1(2)+ROT(6)*X1(3)
      X2(3)=ROT(7)*X1(1)+ROT(8)*X1(2)+ROT(9)*X1(3)
      X2(4)=ROT(1)*X1(4)+ROT(2)*X1(5)+ROT(3)*X1(6)
      X2(5)=ROT(4)*X1(4)+ROT(5)*X1(5)+ROT(6)*X1(6)
      X2(6)=ROT(7)*X1(4)+ROT(8)*X1(5)+ROT(9)*X1(6)
      write(6,*) 'truerf: 120 x2 ', x2
! ROTATE FROM MEAN OF 50/J2000 TO TRUE OF REFERENCE
  130 CONTINUE
! FIRST CONVERT TO EARTH CENTERED IF NECESSARY
!      IF( IELMCI.EQ.iplnet(icbdgm) ) GO TO 150
!      IPT=8
!      IF(IELMCI.EQ.100) IPT=10
!      IF(IELMCI.EQ.200) IPT=1
!      IF(IELMCI.EQ.301) IPT=11
!      IF(IELMCI.EQ.400) IPT=2
!      IF(IELMCI.EQ.500) IPT=3
!      IF(IELMCI.EQ.600) IPT=4
!      IF(IELMCI.EQ.700) IPT=5
!      IF(IELMCI.EQ.800) IPT=6
!      IF(IELMCI.EQ.900) IPT=7
!C      CALL BUFPOS(MJDEP,FSECEP,MJDEP,FSECEP,1,IDUM1,DUM1,LDUM1,AA)
!      write(6,*) 'truerf: 130 ielmci, ipt ', ielmci, ipt
!
!      if( ielmci .eq. 300 ) then
!         CALL BUFEAR(MJDEP,FSECEP,MJDEP,FSECEP,1,IDUM1,DUM1,LDUM1,AA,
!     1   II)
!      else
!         CALL BUFXTR(MJDEP,FSECEP,MJDEP,FSECEP,1,IDUM1,DUM1,LDUM1,AA)
!      endif
!
!     ....last arg is LBARYC - if true, output in SS barycentric
!      CALL PLANPO(MJDEP,FSECEP,.TRUE.,.false.,aa,ii)
!CCC      CALL PLANPO( MJDEP,FSECEP,.TRUE., .true.,aa,ii )
!
!     ....correct???
!      DO 140 IJ=1,6
!         X2(IJ)=X2(IJ)+BDSTAT(IJ,IPT)
! 140  CONTINUE
!     ....correct???
      write(6,*) 'truerf: 140 x2 ', x2
  150 CONTINUE
      X1(1) = REFMT(1)*X2(1)+REFMT(2)*X2(2)+REFMT(3)*X2(3)
      X1(2) = REFMT(4)*X2(1)+REFMT(5)*X2(2)+REFMT(6)*X2(3)
      X1(3) = REFMT(7)*X2(1)+REFMT(8)*X2(2)+REFMT(9)*X2(3)
      X1(4) = REFMT(1)*X2(4)+REFMT(2)*X2(5)+REFMT(3)*X2(6)
      X1(5) = REFMT(4)*X2(4)+REFMT(5)*X2(5)+REFMT(6)*X2(6)
      X1(6) = REFMT(7)*X2(4)+REFMT(8)*X2(5)+REFMT(9)*X2(6)
      write(6,*) 'truerf: 150 x1 ', x1
      GO TO 600
!
! INPUT TRUE OF DATE
  200 CONTINUE
      WRITE(6,*)'TRUERF: TRUE OF DATE EARTH EQUATOR'
      MJD=MJDEP
      FSEC(1)=FSECEP
      write(6,*)'TRUERF: mjd, fsec(1) ', mjd, fsec(1)
! GET MATRIX TO GO BACK TO MEAN OF 50/J2000
!      CALL BUFPOS(MJD,FSEC(1),MJD,FSEC(1),1,IDUM1,DUM1,LDUM1,AA)
!
      if (ielmci .eq. 300 ) then
         write(6,*) 'truerf:200 call bufear '
         CALL BUFEAR(MJD,FSEC(1),MJD,FSEC(1),1,IDUM1,DUM1,LDUM1,AA,II)
         write(6,*) 'truerf:200 call precss '
         CALL PRECSS(MJD,FSEC,EF,ZF,TF,AA(KSRTCH),1)
         write(6,*) 'truerf:200 call nuvect '
         CALL NUVECT(MJD,FSEC,DPSI,EPST,EPSM,1,AA(KSRTCH))
         write(6,*) 'truerf:200 call npvect '
         CALL NPVECT(MJD,fsec, DPSI,EPST,EPSM,                          &
     &               EF, TF ,ZF ,1,AA(KSRTCH),                          &
     &               ROT,EQN,1,.FALSE.,                                 &
     &         AA(KPDPSI),AA(KPEPST),AA(KNUTIN),AA(KNUTMD),.FALSE. ,    &
     &            AA(KDXDNU),AA(KDPSIE),AA(KEPSTE),DUM,DUM3)
         CALL GRHRAN(MJD,fsec,.FALSE.,.FALSE.,EQN,AA(KSRTCH),           &
     &         AA(KTHG),AA(KCOSTG),AA(KSINTG),1,AA,II,UTDT,.FALSE.,     &
     &         DUM,DUM3)
      else
!         write(6,*) 'truerf:200 call bufxtr '
         CALL BUFXTR(MJD,FSEC(1),MJD,FSEC(1),1,IDUM1,DUM1,LDUM1,AA)
!
      IF(LXEPHM) THEN
      DO IJ=1,ICBODY
      CALL BUFSUP(AA,MJD,FSEC(1),MJD,FSEC(1),IDUM,                      &
     &AA(KEPHMS),IJ,1)
      ENDDO
      ENDIF
!
!  NEW MARS ONLY NUTATION SUBROUTINE
          IF( ielmci .EQ.5 .AND. IMRNUT .EQ. 1 ) THEN
             CALL PRMCSS(MJD,FSEC,EF,ZF,TF,AA(KSRTCH),1)
             CALL NUVMCT(MJD,FSEC,DPSI,EPST,EPSM,1,AA(KSRTCH))
             CALL NPVMCT(MJD,fsec, DPSI,EPST,EPSM,                      &
     &                EF, TF ,ZF ,1,AA(KSRTCH),                         &
     &                ROT,EQN, 1,.FALSE.)
             CALL MM2EJ2(MJD,fsec, rot, 1,AA(KSRTCH),                   &
     &                   .FALSE.)
!
             CALL ROTMTR(MJD,fsec,.FALSE.,EQN,AA(KSRTCH),               &
     &                   AA(KTHG),AA(KCOSTG),AA(KSINTG),1,AA,IDUM)
          ELSE
!            write(6,*) 'truerf:200 call prxcss '
             CALL PRXCSS(MJD,FSEC,EF,ZF,TF,AA(KSRTCH),1,AA(KDPSI),      &
     &       AA(KEPST),AA(KEPSM),AA,1)
!            write(6,*) 'truerf:200 call nuvxct '
             CALL NUVXCT(MJD,FSEC,DPSI,EPST,EPSM,1,AA(KSRTCH))
!            write(6,*) 'truerf:200 call npvxct '
             CALL NPVXCT(MJD,fsec, DPSI,EPST,EPSM,                      &
     &                   EF, TF ,ZF ,1,AA(KSRTCH),                      &
     &                   ROT,EQN,1,.FALSE.)
!             write(6,*) 'truerf:200 call rotxtr '
!             CALL ROTXTR(MJD,fsec,.FALSE.,EQN,AA(KSRTCH),
!     .                   AA(KTHG),AA(KCOSTG),AA(KSINTG),1,AA)
          ENDIF
      endif
!
      GO TO 120
!
! INPUT MEAN OF DATE
  300 CONTINUE
!      write(6,*) 'TRUERF: 300 MEAN OF DATE EARTH EQUATOR'
      MJD=MJDEP
      FSEC(1)=FSECEP
      GO TO 110
!
! INPUT MEAN OF 50
  400 CONTINUE
!      write(6,*) 'TRUERF: 400 MEAN OF 50 EARTH EQUATOR'
      IF(NEPHEM.NE.200) THEN
         DO 410 IJ=1,6
            X2(IJ) = X1(IJ)
  410    CONTINUE
!         write(6,*) 'truerf: 410 x2 ', x2
      ELSE
! EPHEMERIS IS IN J2000
! ROTATE FROM MEAN OF 50 TO J2000
         X2(1) = R50200(1)*X1(1)+R50200(4)*X1(2)+R50200(7)*X1(3)
         X2(2) = R50200(2)*X1(1)+R50200(5)*X1(2)+R50200(8)*X1(3)
         X2(3) = R50200(3)*X1(1)+R50200(6)*X1(2)+R50200(9)*X1(3)
         X2(4) = R50200(1)*X1(4)+R50200(4)*X1(5)+R50200(7)*X1(6)
         X2(5) = R50200(2)*X1(4)+R50200(5)*X1(5)+R50200(8)*X1(6)
         X2(6) = R50200(3)*X1(4)+R50200(6)*X1(5)+R50200(9)*X1(6)
!         write(6,*) 'truerf: else410 x2 ', x2
      ENDIF
      GO TO 130
!
! INPUT J2000
  450 CONTINUE
!      write(6,*)'TRUERF: 450 MEAN OF J2000 EARTH EQUATOR'
      IF(NEPHEM.EQ.200) THEN
         DO 455 IJ=1,6
            X2(IJ) = X1(IJ)
  455    CONTINUE
!         write(6,*) 'truerf: 455 x2 ', x2
      ELSE
! EPHEMERIS IS IN MEAN OF 50
! ROTATE FROM J2000 TO MEAN OF 50
         X2(1) = R50200(1)*X1(1)+R50200(2)*X1(2)+R50200(3)*X1(3)
         X2(2) = R50200(4)*X1(1)+R50200(5)*X1(2)+R50200(6)*X1(3)
         X2(3) = R50200(7)*X1(1)+R50200(8)*X1(2)+R50200(9)*X1(3)
         X2(4) = R50200(1)*X1(4)+R50200(2)*X1(5)+R50200(3)*X1(6)
         X2(5) = R50200(4)*X1(4)+R50200(5)*X1(5)+R50200(6)*X1(6)
         X2(6) = R50200(7)*X1(4)+R50200(8)*X1(5)+R50200(9)*X1(6)
!         write(6,*) 'truerf: else455 x2 ', x2
      ENDIF
      GO TO 130
! INPUT MEAN OF 50 ECLIPTIC
  500 CONTINUE
!      write(6,*) 'TRUERF: 500 MEAN OF 50 ECLIPTIC'
! THE COMMENTED CODE IS FOR MEAN OF 2000 ECLIPTIC
!     IPT=2
!     IF(NEPHEM.NE.200) IPT = 1
! ROTATE FROM MEAN OF 50 ECLIPTIC TO MEAN OF 50 EARTH EQUATOR
      DO 510 JJ=1,9
!        ROT(JJ)=ROTEC(JJ,IPT)
         ROT(JJ)=ROTEC(JJ,1)
  510 END DO
!      write(6,*) 'truerf: 510 rot ', rot
!      write(6,*) 'truerf: 510 rotec(jj,2)  ',
!     &             (rotec(jj,2),jj=1,9)
      X2(1)=ROT(1)*X1(1)+ROT(2)*X1(2)+ROT(3)*X1(3)
      X2(2)=ROT(4)*X1(1)+ROT(5)*X1(2)+ROT(6)*X1(3)
      X2(3)=ROT(7)*X1(1)+ROT(8)*X1(2)+ROT(9)*X1(3)
      X2(4)=ROT(1)*X1(4)+ROT(2)*X1(5)+ROT(3)*X1(6)
      X2(5)=ROT(4)*X1(4)+ROT(5)*X1(5)+ROT(6)*X1(6)
      X2(6)=ROT(7)*X1(4)+ROT(8)*X1(5)+ROT(9)*X1(6)
!      write(6,*) 'truerf: 510 x2 ', x2
! IF THE EPHEMERIS IS DE200 THEN ROTATE FROM MEAN OF 50 EARTH EQUATOR TO
! MEAN OF 2000 EARTH EQUATOR
      IF(NEPHEM.EQ.200) THEN
! ROTATE FROM MEAN OF 50 TO J2000
         X1(1) = R50200(1)*X2(1)+R50200(4)*X2(2)+R50200(7)*X2(3)
         X1(2) = R50200(2)*X2(1)+R50200(5)*X2(2)+R50200(8)*X2(3)
         X1(3) = R50200(3)*X2(1)+R50200(6)*X2(2)+R50200(9)*X2(3)
         X1(4) = R50200(1)*X2(4)+R50200(4)*X2(5)+R50200(7)*X2(6)
         X1(5) = R50200(2)*X2(4)+R50200(5)*X2(5)+R50200(8)*X2(6)
         X1(6) = R50200(3)*X2(4)+R50200(6)*X2(5)+R50200(9)*X2(6)
         DO 520 JJ=1,6
            X2(JJ)=X1(JJ)
  520    CONTINUE
!         write(6,*) 'truerf: 520 x1 ', x1
      ENDIF
      GO TO 130
  600 CONTINUE
!
!    AT THIS POINT X1 CONTAINS TRUE OF REF coord center
!        FIXED CARTESIAN COORDINATES
!
!  DETERMINE IF ELEMENTS ARE TO BE PUT BACK INTO KEPLER
      IF(IELMTP.EQ.1) GO TO 800
!  CONVERT TO EARTH FIXED KEPLER
!CC      GM = PARMV(IPVAL(IXGM))
      GMCB =PARMV(IPVAL(IXGM)+IBDGM(ICBDGM)-1)
      gmci = bdstat(7,ielmc2)
      IF(LASTSN.AND.ISET.EQ.1) GMCI=BDSTAT(7,8)
      IDRAD=1
      IF(IELMTP.EQ.3) IDRAD=9
      IDRADP=IDRAD-1
      CALL ELEM(X1(1),X1(2),X1(3),X1(4),X1(5),X1(6),                    &
     &          ELEMS,IDRAD,ROT,gmci)
      CALL ELEM(X1(1),X1(2),X1(3),X1(4),X1(5),X1(6),                    &
     &          ELEMP,IDRADP,ROT,gmci)
      GO TO 820
  800 CONTINUE
      DO 810 IJ=1,6
         ELEMS(IJ) = X1(IJ)
  810 END DO
  820 CONTINUE
      IF(IELMCS.EQ.0.AND.IELMCI.EQ.iplnet(icbdgm) ) RETURN
!C      WRITE(IOUT6,20000)
      WRITE(IOUT6,20001) ISAT,ISET
      IF(IELMTP.EQ.1) WRITE(IOUT6,20101) (ELEMS(IP),IP=1,6)
      IF(IELMTP.EQ.2) WRITE(IOUT6,20102) (ELEMP(IP),IP=1,6)
      IF(IELMTP.EQ.3) WRITE(IOUT6,20103) (ELEMP(IP),IP=1,6)
      write(6,*) 'truerf: exit '
      RETURN
!
20000 FORMAT(///'  EPOCH ELEMENTS HAVE BEEN RESET TO EARTH CENTER')
20001 FORMAT(///'  TRUE OF REF FOR SAT NO ',I3,' IN SET NO ',I3)
20101 FORMAT(//6X,'INERTIAL RECTANGULAR COORDINATES'/ 6X, 32(1H-) /     &
     & 1H0,16X,1HX,26X,1HY,26X,1HZ/ 16X,3H(M),24X,3H(M),24X,3H(M)// 2X, &
     & 7P3D28.10// 15X, 4HXDOT,23X,4HYDOT,23X,4HZDOT/ 15X,5H(M/S) ,     &
     & 2(22X,5H(M/S)) // 2X, 4P3D28.13/)
20102 FORMAT(//6X,'KEPLERIAN ELEMENTS'/ 6X,18(1H-)/ 1H0,12X,1HA,13X,    &
     & 1HE,13X,1HI,9X,'RA ASC NODE     ARG PERIGEE    MEAN ANOMALY',    &
     &/10X,8H(METERS),14X,5X,9H(DEGREES),6X,9H(DEGREES),7X,             &
     & 9H(DEGREES),6X,9H(DEGREES) /1H0,                                 &
     &   F18.3,F14.10,4F15.8)
20103 FORMAT(//6X, 'NON SINGULAR KEPLERIAN ELEMENTS' /                  &
     & 6X,31(1H-) //                                                    &
     &  T30,'NON-SINGULAR KEPLERIAN ELEMENTS EXPRESSED IN KEPLERIAN ',  &
     &  ' A, E, I, NODE, PERG, MNANOM '/                                &
     &  T30, 'A    = A',                                                &
     &  t65, 'ECWN = E * COS( PERG + NODE)'/                            &
     &  T30, 'ESWN = E * SIN( PERG + NODE)',                            &
     &  t65, 'SISN = SIN( I/2 ) * SIN( NODE )'/                         &
     &  T30, 'SICN = SIN( I/2 ) * COS( NODE )',                         &
     &  t65, 'MWN  = NODE + PERG + MNANOM'//                            &
     & 1H0,12X,1HA,8X,'E*COS(W+N)', 6X,'E*SIN(W+N)',5X, 'SIN(I/2)*SIN(N)&
     &' , 2X,'SIN(I/2)*COS(N)',4X , 'M+W+N' /                           &
     &  10X,'(METERS) ', 69X,'(DEGREES)'/                               &
     &  1H0,F18.3,3F16.12,1X ,F16.12,F15.8)
      END
