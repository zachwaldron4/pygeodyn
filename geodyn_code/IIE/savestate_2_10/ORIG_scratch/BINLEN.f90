!$BINLEN
      SUBROUTINE BINLEN(AA,II)
!********1*********2*********3*********4*********5*********6*********7**
! BINLEN           85/03/25            8504.0    PGMR - D. ROWLANDS
! BINLEN           92/03/09            9201.1    PGMR - J. MCCARTHY
!
! FUNCTION:  WRITE OUT LENGTHS RECORD FOR BINARY RESIDUAL FILE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/ANTID/ISATAN(3),ISTAN1(3),ISATN1(3),ISTAN2(3),ISATN2(3),   &
     &             IANNB1(3),IANNB2(3)
      COMMON/CBINRI/NSATB,NSTAB,INMSAT(3),INMSTA(3)
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CNELVS/KNELEV(999)
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
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/SAVEIM/RIM(3,3),SBF1(3,3),SBF2(3,3),SBF3(3,3),SBF4(3,3),   &
     &       SCF2(3,3),SCF3(3,3),SCF4(3,3),RKEY
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION W(20),FSCI(1),FSECO(1),IYMD(1),IHM(1),SEC(1)
      DIMENSION AA(1),II(1)
!
      DATA ZERO/0.0D0/
      DATA ONE/1.D0/,TWO/2.D0/,FILL/-9999.D0/,THREE/3.0D0/
      DATA T6/1000000.D0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
      DO 50 I=11,16
      W(I)=0.0
   50 END DO
!
!     mjds from default reference time
      W(1)=DBLE(MJDSBL) + REPDIF
      W(2)=FSECBL
      FSCI(1)=ZERO
!     CALL UTCET(.FALSE.,1,MJDSBL,FSCI,FSECO,AA(KA1UT))
      FSECO(1)=FSCI(1)
      CALL YMDHMS(MJDSBL,FSECO,IYMD,IHM,SEC,1)
!
!
      W(3)=SEC(1)+DBLE(100*IHM(1))+T6*DBLE(IYMD(1))
!
!CCC  I3=W(3)
!CCC  W(3)=DFLOAT(I3)
!     ....REPLACED ABOVE 2 LINES WITH LINE BELOW BECAUSE
!     ....THIS CAUSED ERRORS ON 32-BIT WORD COMPUTERS. THE
!     ....INTEGER IS NOT LARGE ENOUGH TO HOLD W(3)  JJM 3/92
!....Note: a regular NELEVS come from KNELEV for each MTYPE. PL check
!.. the subroutine of CKNELEV.f which loads these numbers in if the
!.. number NELEVS is not correct in the head of resid file
!..
      JELEVS=KNELEV(MTYPE)
      IF(NELEVS.NE.JELEVS)NELEVS=JELEVS
!
      W(3) = AINT( W(3) )
!
!
      W(4)=DBLE(MTYPE)
      XIDI=ONE
      IF(MTYPE.GE.61.AND.MTYPE.LE.82) XIDI=TWO
      IF(MTYPE.GE.63.AND.MTYPE.LE.92) XIDI=THREE
      IF(MTYPE.EQ.36) XIDI=TWO
      W(5)=XIDI
      XAVGRR=ONE
      IF(LAVGRR) XAVGRR=TWO
      W(6)=XAVGRR
!     ....NOW NELEVS IS VARIABLE, AND COULD VARY FROM 1 TO 12
!     ....W(7) indicates the actual number of elevations, but
!     ....the residual record has a minimum of 3 elevation words
!     ....written out so that the record is not shorter than the
!     ....old format.   --  jjm 3/3/93
!C      W(7)= MAX(NELEVS,3)
      W(7)= NELEVS
!
      W(8)=DBLE(NM)
      W(9)=DBLE(NSTAB)
      W(10)=DBLE(NSATB)
      IF(INMSTA(1).NE.0)W(11)=DBLE(II(KISTNO-1+INMSTA(1)))
      IF(INMSAT(1).NE.0)W(12)=DBLE(II(KISATN-1+INMSAT(1)))
      IF(INMSTA(2).NE.0)W(13)=DBLE(II(KISTNO-1+INMSTA(2)))
      IF(INMSAT(2).NE.0)W(14)=DBLE(II(KISATN-1+INMSAT(2)))
      IF(INMSTA(3).NE.0)W(15)=DBLE(II(KISTNO-1+INMSTA(3)))
      IF(INMSAT(3).NE.0)W(16)=DBLE(II(KISATN-1+INMSAT(3)))
!
! FILL ANTENNA IDS INTO WORD 17
!
      IMULT=1
      ITMP=0
      DO 100 INDX=1,3
      ITMP=ITMP+IMULT*IANNB1(INDX)+IMULT*10*IANNB2(INDX)
      IMULT=IMULT*100
  100 END DO
      W(17)=DBLE(ITMP)
!
      W(18)=RKEY
      W(19)=FILL
      W(20)=FILL
! ADD LASER ALTIMETER ID IF PRESENT
      IF(MTYPE.GE.99.AND.MTYPE.LE.101.AND.BLKDAT(4,1).GT.0.D0)          &
     &     W(18)=BLKDAT(4,1)
! ADD LASER ALTIMETER WAVELENGTH IF PRESENT
      IF(MTYPE.GE.99.AND.MTYPE.LE.101.AND.BLKDAT(3,1).GT.0.D0)          &
     &     W(19)=BLKDAT(3,1)
      WRITE(IUNT19) W
      RETURN
      END
