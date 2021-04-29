!$EBPRNT
      SUBROUTINE EBPRNT(EBVAL ,EBSIG ,EBNAME,DSCRPT,JSTATE,      &
     &                  ISATID,MJDSEB,FSECEB1,FSECEB2)
!********1*********2*********3*********4*********5*********6*********7**
! EBPRNT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   EBVAL
!   EBSIG
!   EBNAME
!   DSRPT
!   JSTATE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CEBARC/NEBIAS,NBIASE,NXCEBA
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/CITER /NINNER,NARC,NGLOBL
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
      COMMON/CUNIT9/L9SET ,L9RES ,L9RSST,L9RSTY,L9EBS ,L9APAR,L9SUMP,   &
     &              L9GPAR,L9UPDT,L9LAST,L9OUT ,NXUNT9
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      CHARACTER(LEN=8) :: EBNAME
      DIMENSION EBVAL (NBIASE),EBSIG (NBIASE),EBNAME(2,NBIASE),         &
     &          DSCRPT(7,1),JSTATE(NBIASE),FSECEB2(NBIASE)
      DIMENSION MJDSEB(NBIASE),FSECEB1(NBIASE),JHM(1),JYMD(1),FSEC(1)

      PARAMETER( HALF = 0.5D0 )
      PARAMETER( C1D2 = 1.0D2 )
      PARAMETER( C1D6 = 1.0D6 )
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      write(6,*) "EBPRNT: MODESC(MTYPE) :", MODESC(MTYPE), MTYPE

      INCR=((MLINE6-3)/7)*6
      DO 3000 I11=1,NBIASE,INCR
      I22=MIN(I11+INCR-1,NBIASE)
      IPAGE6=IPAGE6+1
      WRITE(IOUT6,10000) NARC,NINNER,NGLOBL,IPAGE6
      DO 2000 I1=I11,I22,6
      WRITE(IOUT6,20000)
      I2=MIN(I1+5,NBIASE)
      DO 1000 I=I1,I2
      K=JSTATE(I)
      WRITE(IOUT6,30000) (EBNAME(J,I),J=1,2),EBVAL(I),EBSIG(I),         &
     &                   (DSCRPT(J,K),J=1,7)

      !!jtw edit for refrac print
      IF (LSTINR .AND. IRFPRT(MODEL(MTYPE))==1) THEN

      IF (EBNAME(2,I) .EQ. "TROP" ) THEN

      CALL YMDHMS(MJDSEB(I),FSECEB1(I),JYMD,JHM,FSEC,1)
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
      STRTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)

      CALL YMDHMS(MJDSEB(I),FSECEB2(I),JYMD,JHM,FSEC,1)
      IF(JYMD(1).GE.1000000)JYMD(1)=JYMD(1)-1000000
      STPTIM=DBLE(JYMD(1))*C1D6 + DBLE(JHM(1))*C1D2+ FSEC(1)


!      write(6,*) "EBPRNT: STRTIM is :", STRTIM

      WRITE(400,30010) (EBNAME(J,I),J=1,2),STRTIM,STPTIM,EBVAL(I),      &
     &                  EBSIG(I), (DSCRPT(J,K),J=1,3),MODESC(MTYPE)
30010 FORMAT(1X,2A8,2G24.16,G24.16,G14.6,3(1X,A8),1x,I3)
      ENDIF
      ENDIF

 1000 END DO
 2000 END DO
 3000 END DO

      ILINE6=MLINE6
      IF(.NOT.(L9EBS.AND.L9OUT)) RETURN
      INCR=((MLINE9-3)/7)*6
      DO 6000 I11=1,NBIASE,INCR
      I22=MIN(I11+INCR-1,NBIASE)
      IPAGE9=IPAGE9+1
      WRITE(IOUT9,10009) NARC,IPAGE9,NINNER,NGLOBL
      DO 5000 I1=I11,I22,6
      WRITE(IOUT9,20000)
      I2=MIN(I1+5,NBIASE)
      DO 4000 I=I1,I2
      K=JSTATE(I)
      WRITE(IOUT9,30009) (EBNAME(J,I),J=1,2),EBVAL(I),EBSIG(I),         &
     &                   (DSCRPT(J,K),J=1,7)
 4000 END DO
 5000 END DO
 6000 END DO
      ILINE9=MLINE9
      RETURN
10000 FORMAT('1',15X,'SUMMARY OF ELECTRONIC PASS BIASES FOR ARC',       &
     &   I3,' INNER ITERATION',I3,' OF GLOBAL ITERATION',I2,8X,         &
     &   'UNIT  6 PAGE NO.',I6/1X /2X,'PARAMETER NAME', 6X,             &
     &   'PARAMETER VALUE', 6X,'STD. DEV.', 4X,'OBS TYPE',              &
     &   3(1X,'STATION SATELLITE'))
10009 FORMAT('1SUMMARY OF ELECTRONIC PASS BIASES FOR ARC',I3,           &
     &   13X,'UNIT  9 PAGE NO.',I4/                                     &
     &   ' INNER ITERATION',I3,' OF GLOBAL ITERATION',I2/1X /           &
     &   ' PARAMETER NAME  PARAMETER VALUE',                            &
     &   2X,'STD.DEV. OBS-TYPE STATION SATELLITE STATION'/              &
     &   50X        ,        'SATELLITE STATION SATELLITE')
20000 FORMAT(1X)
30000 FORMAT(1X,2A8,G24.16,G14.6,7(1X,A8))
30009 FORMAT(1X,2A8,D15.8 ,D10.3,4(1X,A8)/51X,3(1X,A8))
      END
