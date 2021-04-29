!$PFTIDE
      SUBROUTINE PFTIDE( DLAT,DLON,TIME,TIDE,ISDATA,AA )
!********1*********2*********3*********4*********5*********6*********7**
! PFTIDE           97/08/27            9708.0    PGMR -
!
! FUNCTION:    to compute the ocean tidal height at a given time
!              and location from grids of harmonic constants.
!              This routine is a driver to select the proper tide model.
!              (Long period tides are NOT computed by this routine.)
!              Name derivation - Pathfinder tide routine
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   DLAT     I         north latitude (in degrees) for desired
!                      location.
!
!   DLON     I         east longitude (in degrees).
!
!   TIME     I         desired UTC time, in (decimal) MJD
!                      e.g., 1 Jan 1990 noon = 47892.5
!
!   TIDE     O         computed tidal height.  The units will be
!                      the same as the 'amplitude' units on the
!                      input tidal grids (usually cm).
!
!   ISDATA   O         logical denoting whether tide data exist at
!                      desired location.  If FALSE, then TIDE is
!                      not modified.
!
!  Usage notes -
!    On first call, this routine reads (from std input) the filenames
!   and the area limits for each tide model to be used.  These files
!   are subsequently read by DPERTH and TLOAD. Unit 31 is used for reads
!    The tide height predictions are based on the model appropriate
!   for the given lat/lon.  If more than one model covers a given area,
!   the model selected is according to the order read in; therefore,
!   put the default model as the last model on std input.
!
!  Revised 5 Feb 1996 - added entry pftdst (mostly for debugging)
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
      LOGICAL           isdata
!
      INTEGER           lu(5)
                                 ! maximum number of tide models
      parameter        (mf=10)
                                 ! maximum number subareas delineated
      parameter        (ma=30)
                                  ! number of tide models
      parameter        (nfil=5)
                                  !
      parameter        (ndat=7)
      character*26      namfil(5)
      character         ccl
      intrinsic         char, ichar
! %%%%%%%%%% CHANGED maxind(ma) => maxind(ndat)
      dimension         knt(2,ma), maxind(ndat)
      dimension          rltln(4,7)
      DOUBLE PRECISION              latmin,latmax,lonmin,lonmax
      LOGICAL           init
      dimension aa(1)
!      save              init,nfil,nna,ndat
!     save              init,nfil,rltln,knt,nna,maxind,ndat
      data  init/.true./
      data  lu/21,22,23,24,25/
      data namfil/                                         &
     &     'rdrtides.Medn.data        ',                   &
     &     'rdrtides.Persian_Gulf.data',                   &
     &     'rdrtides.Maine.data       ',                   &
     &     'rdrtides.StLawrence.data  ',                   &
     &     'rdrtides.Schrama_Ray.data '/
      data rltln/                                          &
     &           30.0,45.0,  0.0, 37.0,                    &
     &           35.0,40.0,- 5.5,  0.0,                    &
     &                 23.0,30.5, 47.0, 56.5,                    &
     &           42.2,45.5,289.0,294.3,                    &
     &           45.9,51.0,294.0,300.4,                    &
     &           47.5,51.5,300.4,303.0,                    &
     &          -90.0,90.0,  0.0,360.0/
      data maxind /1,1,2,3,4,4,5/
      INCLUDE 'COMMON_DECL.inc'
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
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
      if (init) then
         init = .false.
         nna = 0
         do 6 i=1,ma
            knt(1,i) = 0
            knt(2,i) = 0
    6    continue
!
!        Read in tide models
!        -------------------
         call prthip(aa(ktmg),lu,namfil, nfil )
      endif
!
!
!     Determine which model to use for this lat/lon
!     ---------------------------------------------
      xlat = dlat
      do 80 i=1,ndat
         if (xlat.lt.rltln(1,i) .or. xlat.gt.rltln(2,i)) go to 80
         xlon = dlon
         if (xlon.lt.rltln(3,i)) xlon = xlon + 360.
         if (xlon.gt.rltln(4,i)) xlon = xlon - 360.
         if (xlon.lt.rltln(3,i) .or. xlon.gt.rltln(4,i)) go to 80
!
!        Model found for this area; call tide predictor
!        ----------------------------------------------
         model = maxind(i)
         call dperth( xlat,xlon,time,xtide,isdata,model,aa(ktmg))
         if (isdata) tide = DBLE(xtide)
         knt(1,i) = knt(1,i) + 1
         if (.not.isdata) knt(2,i) = knt(2,i) + 1
         return
   80 continue
      isdata = .false.
      nna = nna + 1
      return
!
!
!     print counts, for debugging
!     ---------------------------
      entry pftdst
      write(6,*) 'Number evaluations for each internal model:'
      ii = 0
      do 110 i=1,ndat
         model = maxind(i)
         if (model.ne.ii) then
            ccl = 'a'
            write(6,111) model, knt(1,i), knt(2,i)
         else
            ccl = char( ichar(ccl) + 1 )
            write(6,112) model,ccl, knt(1,i), knt(2,i)
         endif
         ii = model
  110 continue
  111 format(' Model',I4,I12,' counts;',I9,' nulls.')
  112 format(' Model',I4,A1,I11,' counts;',I9,' nulls.')
      if (nna.gt.0) write(6,114) nna
  114 format(' Number with no available model:',I9)
      return
      END
