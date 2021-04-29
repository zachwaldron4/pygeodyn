!$ANTENA
      SUBROUTINE ANTENA(ENV,UHAT,WORK,DR,NM,DISP,ITYPE,                 &
     &           nsta0, mjdsec, fsec, aa, ii )
!********1*********2*********3*********4*********5*********6*********7**
! ANTENA           83/06/01            8306.0    PGMR - TOM MARTIN
!
! FUNCTION: COMPUTE ANTENNA AXIS DISPLACEMENT CORRECTIONS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ENV      I    A    STATION EAST,NORTH,VERTICAL VECTORS IN E.C.F.
!   UHAT     I    A    E.C.F. UNIT VECTOR FROM STATION TO S/C
!   WORK          A    WORKING ARRAY OF LENGTH "NM"
!   DR       O    A    RANGE CORRECTIONS DUE TO ANTENNA AXIS
!                      DISPLACEMENT
!   NM       I    S    NUMBER OF MEASUREMENTS
!   DISP     I    S    ANTENNA AXIS DISPLACEMENT
!   ITYPE    I    S    INDICATES ANTENNA MOUNT TYPE;
!                      1=X/Y ANGLE; Y MEAS. POSITIVE EAST (E-W MOUNT)
!                      2=X/Y ANGLE; Y MEAS. POSITIVE NORTH (N-S MOUNT)
!                      3=AZIMUTH/ELEVATION
!                      4=HOUR ANGLE/DECLINATION
!                      5=VLBI only !
!                      6=pjd station height displacement
!                      7=time-varying pjd station height displacement
!                      8=time-varying pjd station pressure changes
!    nsta0    I   S    Requested station number
!    mjdsec   I   S    Time in integer MJD seconds
!    fsec     I   A    Fractional MJD seconds
!    aa       I   A    DYNAMIC ARRAY OF REALS
!
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION UHAT(NM,3),DR(NM),ENV(3,3),WORK(NM)
      DIMENSION fsec(*)
      DIMENSION aa(*)
      DIMENSION ii(*)
!
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
!
      DATA kentry /0/, iprint/10/
!
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
       kentry = kentry + 1
!          if( kentry .le. iprint ) then
!             write(6,*) 'antena: itype ', itype
!          endif
!
      IF(ITYPE.LT.1.OR.ITYPE.GT.8) GO TO 3000
!  ITYPE=      1    2    3   4     5     6     7      8
      GO TO (1000,1000,3000,4000, 3000,15000,15000,15000 ),ITYPE
 1000 CONTINUE
! COMPUTE DIRECTION COSINES
      CALL MATPRD(UHAT,ENV(1,ITYPE),WORK,NM,3,1)
! COMPUTE Y ANGLE COSINES SQUARED
      DO 2800 N=1,NM
      DR(N)=ONE-WORK(N)*WORK(N)
 2800 END DO
      GO TO 5000
 3000 CONTINUE
! AZIMUTH/ELEVATION MOUNTS HAVE ZERO AXIS DISPLACEMENT
      DO 3800 N=1,NM
      DR(N)=ZERO
 3800 END DO
      RETURN
! COMPUTE COSINE SQUARED OF DECLINATION
 4000 CONTINUE
      CALL DOTPRD(UHAT,UHAT,DR,NM,NM,NM,2)
 5000 CONTINUE
! COSINE IS SQUARE ROOT
      DO 5800 N=1,NM
      WORK(N)=SQRT(DR(N))
 5800 END DO
! RANGE CORRECTION IS ANTENNA AXIS DISPLACEMENT TIMES COSINE
      DO 6800 N=1,NM
      DR(N)=-DISP*WORK(N)
 6800 END DO
      RETURN
!
!     ....new options for PJD
!
15000 continue
!
! COMPUTE DIRECTION COSINES
!
      CALL MATPRD(UHAT,ENV(1,3),WORK,NM,3,1)
!
!          if( kentry .le. iprint ) then
!             write(6,*) 'antena: disp, uhat(1,3) ', disp, uhat(1,3)
!             write(6,*) 'antena:  work(1) ',  work(1)
!             write(6,*) 'antena: env(1,123) ', env(1,1), env(1,2), env(
!             write(6,*) 'antena: env(2,123) ', env(2,1), env(2,2), env(
!             write(6,*) 'antena: env(3,123) ', env(3,1), env(3,2), env(
!             write(6,*) 'antena: uhat(1,123) ',
!     &               uhat(1,1), uhat(1,2), uhat(1,3)
!          endif
!
      if( itype .eq.7 ) then
!
!        ....compute antenna offsets vs time using arrays read
!        ....from file in 2s
!
         call RDOFFL( nsta0, mjdsec, fsec, nm, AA(KANTOU),aa, ii )
!
         DO 16000 N=1,NM
            DR(N)=AA(KANTOU+N-1)*work(N)
!           DR(N)=+antout(N)*work(N)
16000    CONTINUE
!
!             if( kentry .le. iprint ) then
!               write(6,*) 'antena: antout(1), uhat(1,3) ',
!     &                              antout(1), uhat(1,3)
!                write(6,*) 'antena:  work(1),dr(1) ',  work(1),dr(1)
!             endif
!
      else if( itype .eq. 8 ) then
!
!        ....use  mean pressure and scale factor
!        ....read from file in 2s
!        ....and compute height change
!
         call RDOFFP( nsta0, mjdsec, fsec, nm,AA(KANTOU) , aa, ii )
!
         DO 16010 N=1,NM
            DR(N)=AA(KANTOU+N-1)*work(N)
16010    CONTINUE
!             if( kentry .le. iprint ) then
!                write(6,*) 'antena: antout(1), uhat(1,3) ',
!     &                              antout(1), uhat(1,3)
!                write(6,*) 'antena:  work(1),dr(1) ',  work(1),dr(1)
!             endif
!
      else
!
!        ....itype .eq.6
!        ....add height increment (DISP) from INSTRMNT card to station h
!
         DO 16100 N=1,NM
            DR(N)=+DISP*work(N)
16100    CONTINUE
!
      endif
!
!          if( kentry .le. iprint ) then
!             write(6,*) 'antena: dr(1) ', dr(1)
!          endif
      RETURN
      END
