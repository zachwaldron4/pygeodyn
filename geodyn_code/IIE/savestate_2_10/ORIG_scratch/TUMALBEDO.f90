!$TUMALBEDO.f
      SUBROUTINE TUMALBEDO(MJDSEC,FSEC,ALBACCEL,ELEM,ISATID,AA,II,LL)
!********1*********2*********3*********4*********5*********6*********7**
! TUMALBEDO
!
! FUNCTION:     COMPUTE ACCELERATION DUE TO SOLAR RADIATION
!               FOR GPS SATELLITES USING THE ROCK4 MODEL
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC     I         FRACTIONAL REMAINING SECONDS
!   AA      I/O        REAL DYNAMIC ARRAY
!   II      I/O        INTEGER DYNAMIC ARRAY
!   LL      I/O        LOGICAL DYNAMIC ARRAY
!   ISATID             SAT ID ARRAY
!   ALBACCEL O         ALBEDO ACCELERATIONS
!
!
! NOTE:  ALL FLAGS NEEDED IN THIS SUBROUTINE DEALING WITH THE
!        SATELLITE ARE SET IN SUBROUTINE ORBIT OR COWLIN
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
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
      COMMON/CORI03/KICRD ,KICON ,KISTNO,KINDPI,KMJDPL,KIPOLC,          &
     &              KNDOLA,KIOLPA,KKIOLA,KIP0OL,KNDOLM,KIOLPM,          &
     &              KKIOLM,KIPVOL,KICNL2,KICNH2,                        &
     &              KSTMJD, KNUMST, KITAST, KSTNRD, KICNV,              &
     &              KTIDES,KFODEG,KFOORD,KRDEGR,KRORDR,                 &
     &              KPHINC,KDOODS,KOTFLG,KJDN  ,KPTDN ,                 &
     &              KATIND,KPRESP,KMP2RS,KSITE,KPTOLS,NXCI03
      COMMON/CSBSAT/KR,KRT,KZT,KXY2,KUZ,KUZSQ,KTEMP,KC3,KTHETG
      DIMENSION AA(1),II(1),LL(1)
      DIMENSION ALBACCEL(3),ELEM(6)
      DIMENSION HELIOS(3),EFTOI(3,3)
      DIMENSION DUM(4,1)
      DIMENSION ISATID(1)
      DIMENSION DUMN(3),DUM1(3,2,1)

      DATA IANT/0/
!     = 0, no antenna thrust;
!     = 1, ant. thrust on.
      DATA IERM/2/
!     = 1, analytical albedo+IR model;
!     = 2, CERES model (it should be set to "2" for this test).
      DATA IGRD/1/
!     =1, 2.5x2.5 grid;
!     =2, 5x5 grid;
!     =3, 10x10 grid (degrees).
      DATA IREFF/0/
!     = 0, results given in inertial space coords.

! START OF EXECUTABLE CODE
!**********************************************************************

!  EXTRACT IPRN IBLK
       CALL GPSBLOCK(ISATID,ISVN,IPRN,IBLK)
!     CONVERT MJDSEC,FSEC TO YY MM DD HH MM SEC

        CALL MJDYMD(MJDSEC,IYMD,IHMS,4)
        CALL EXTIME(IYMD,IHMS,IY,MONTH,ID,IH,IMIN,ISEC)
        SEC=ISEC+DBLE(FSEC)

!  THE OLC APPROACH
! Converting the epoch from YMDHmS to MJD.
      CALL YMD_TO_MJD(iy,month,id,ih,imin,sec,mjd,fmjd)
!
      amjd = mjd+fmjd

        HELIOS(1)=BDTRUE(1,8)
        HELIOS(2)=BDTRUE(2,8)
        HELIOS(3)=BDTRUE(3,8)

!  COMPUTE ROTATION PATRIX EFTOI (Earth Fixed TO Inertial)

         CALL GRHRAN(MJDSEC,FSEC,.TRUE.,.TRUE.,                     &
     &      AA(KEQN),AA(KSRTCH),                                    &
     &      AA(KTHETG),AA(KCOSTH),AA(KSINTH),1,AA,II,AA(KXUTDT),    &
     &      .FALSE.,DUMN,DUM1)

!     write(6,*)' dbg GEODYN CALL ICRSITRF ',AA(KROTMT),AA(KROTMT+1)
!     write(6,*)' dbg GEODYN CALL ICRSITRF ',AA(KCOSTH),AA(KSINTH)
      CALL ICRSITRF(AA(KROTMT),AA(KDPSR),AA(KXPUT),AA(KYPUT), &
     &              AA(KA1UT),II(KINDPI),MJDSEC,FSEC, &
     &              AA(KCOSTH),AA(KSINTH),1,MINTIM,DUM,2, &
     &              AA(KXDPOL),AA(KXDOTP),EFTOI,.FALSE.)
!     write(6,*)' dbg GEODYN BACK FROM ICRSITRF '
!     write(6,*)EFTOI(1,1),EFTOI(1,2),EFTOI(1,3)
!     write(6,*)EFTOI(2,1),EFTOI(2,2),EFTOI(2,3)
!     write(6,*)EFTOI(3,1),EFTOI(3,2),EFTOI(3,3)

      CALL ERPFBOXW(iERM,IANT,IGRD,IREFF,ELEM,HELIOS,EFTOI,MONTH, &
     &                    IBLK,ISVN,AMJD,ALBACCEL)
!     write(6,*)'ALBEDO ACCELS',ALBACCEL



      RETURN
      END
