!$METHD
      SUBROUTINE METHD(IMTYPE,MTYPE,ICALL,INDSAT,INDSTA)
!********1*********2*********3*********4*********5*********6*********7**
! METHD            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  PUTS OUT THE LOCATION DATA HEADER RECORD OF THE BINARY
!            RESIDUAL FILE FOR METRIC MEASUREMENTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IMTYPE   I    S    LINK TYPE INDICATOR
!   MTYPE    I    S    MEASUREMENT TYPE
!   ICALL    I    S    INDICATOR OF APPROPRIATE SUBROUTINE TO CALL
!   INDSAT   I    A    INTERNAL SATELLITE NUMBER
!   INDSTA   I    A    INTERNAL STATION NUMBER
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/BRESDAT/ALEN(20),ALOC(20),ADAT(80)
      COMMON/CBINRI/NSATB,NSTAB,INMSAT(3),INMSTA(3)
      COMMON/CBINRL/LBINR,LLOCR,LODR,LBNCR,LBINRI,LLOCRI,LODRI,LBNCRI,  &
     &              NXCBIN
      COMMON/CBLOKL/LIGHT ,LNRATE,LNREFR,LPOLAD,LTDRIV,LNANT ,LNTRAK,   &
     &       LASER ,LGPART,LGEOID,LETIDE,LOTIDE,LNTIME,LAVGRR,LRANGE,   &
     &       LSSTMD,LSSTAJ,LNTDRS,LPSBL,LACC,LAPP,LTARG,LTIEOU,LEOTRM,  &
     &       LSURFM,LGEOI2,LSSTM2,LOTID2,LOTRM2,LETID2,LNUTAD
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/IOBTYP/NDXST(2,35),NDXSAT(2,35),                           &
     &      NDRSTA(50),NDRSAT(50),NDRST2(50),NDRSA2(50),                &
     &      ITMBIT(999),IOETR(999),MTYPED(11),NTYPES,                   &
     &      NM0112(4),NM1314(4),NM1522(4),NM2330(4),                    &
     &      NM3138(4),NM3997(4),NM4098(4),NM9900(4),NMT199(4),NMT203(4),&
     &      NS0112(4),NS1314(4),NS1522(4),NS2330(4),                    &
     &      NS3138(4),NS3997(4),NS4098(4),NS9900(4),NST199(4),NST203(4),&
     &      ITYPD(12,2),ITYPDD(7,2),ILINKF(5),ISTAFN(5,3),              &
     &      ISATFN(5,3),ILINKT(5),ISTATN(5,3),ISATTN(5,3),              &
     &      MGOTO(12),MENTER(12),MEXIT(12),                             &
     &      NDST60(3,24),NDST82(6,12),NDST92(12,7),NTDRSS,              &
     &      NDSA60(3,24),NDSA82(6,12),NDSA92(12,7),                     &
     &      MTDRSS(7),KTDRSS(7),ITDRSS(4,7),JTDRSS(4,7),                &
     &      NLLC60(5,24),NLLC82(10,12),NLLC98(20,7),NNBIA,              &
     &      NXMTYP
      COMMON/ROBTYP/CONVRT(999),OBSCAL(999),RESCAL(999),                &
     &              XMTYP
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION  INDSAT(3,4),INDSTA(3,4),XPSEQ(20),                     &
     &           ILWSTA(3),ILWSAT(3),SUB(3)
!
      CHARACTER(8)      :: SUB
      DATA SUB/'MTSTST','MTSSST','MTPTT '/
      DATA MSATB/3/,MSTAB/3/
      DATA ILWSTA /11,13,15/,ILWSAT/12,14,16/
      DATA ZERO/0.D0/
      DATA kentry/0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      kentry = kentry + 1
!      if( kentry .gt. 1000 ) stop
!
!--------------------------------------------------------------
! set flag indicating that this is one way tdrss doppler data
!     LTDR1W=MTYPE.EQ.58 .AND. INDSAT(3,1).EQ.0
      LTDR1W=MTYPE.EQ.56
      LOKAY =  (.not.LNTDRS) .and. (.NOT. LTDR1W)
      LOKAY=.FALSE.
!
      if(lokay)write(6,*) 'METHD: lntdrs ', lntdrs
      if(lokay)write(6,*) 'METHD: indsat(3,1), ltdr1w ',                &
     &                   indsat(3,1), ltdr1w
!--------------------------------------------------------------
!
      if(lokay)write(6,*) 'METHD:IMTYPE,MTYPE,ICALL,INDSAT,INDSTA ',    &
     &                  IMTYPE,MTYPE,ICALL,INDSAT,INDSTA
      if(lokay)write(6,*) 'METHD:INMSAT ',INMSAT
      if(lokay)write(6,*) 'METHD:INMSTA ',INMSTA
      if(lokay)write(6,*) 'METHD:ILINKF ', ILINKF
      if(lokay)write(6,*) 'METHD:ILINKT ', ILINKT
!
      NTIMES=2**ICALL
      NTIMES=NTIMES/2
      if(lokay)write(6,*) 'METHD:ntimes ', ntimes
!
!
      DO 100 I=1,20
      XPSEQ(I)=ZERO
  100 END DO
!
      KPSEQ=1
      ISEQ=0
  400 CONTINUE
      GO TO (500,600,700),ICALL
  500 CONTINUE
      if(lokay)write(6,*) 'METHD:below 500 '
! STRAIGHT TO ROBS
      ISEQ=ISEQ+1
!--------------------------------------------------------------
      IF(ISEQ.GT.1) then
         IF( (.not.LNTDRS) .and. (.NOT. LTDR1W) ) GO TO 2600
         GO TO 5000
      endif
!--------------------------------------------------------------
!
      ITYPE=IMTYPE
      GO TO 900
  600 CONTINUE
      if(lokay)write(6,*) 'METHD:below 600 '
! ROBS THROUGH ROBSD
      ISEQ=ISEQ+1
      IF(ISEQ.GT.2) GO TO 5000
      JTYPE=(MTYPE-59)/2
      ITYPE=ITYPD(JTYPE,ISEQ)
      GO TO 900
  700 CONTINUE
      if(lokay)write(6,*) 'METHD:below 700 '
! ROBS THROUGH ROBSDD
      ISEQ=ISEQ+1
      IF(ISEQ.GT.4) GO TO 5000
      JTYPE=(MTYPE-83)/2
      IISEQ=(ISEQ+1)/2
      JJTYPE=ITYPDD(JTYPE,IISEQ)
      IIISEQ=MOD(ISEQ-1,2)+1
      JJJTYP=(JJTYPE-59)/2
      ITYPE=ITYPD(JJJTYP,IIISEQ)
!
! DOWN TO THE ROBS LEVEL (1 CALL TO A MT ROUTINE;ROBS ITYPE DETERMINED)
  900 CONTINUE
      if(lokay)write(6,*) 'METHD:below 900 '
      NRGOTO=MGOTO(ITYPE)
      IENTER=MENTER(ITYPE)
      IEXIT=MEXIT(ITYPE)
      if(lokay)write(6,*) 'METHD:itype, nrgoto,ienter, iexit ',         &
     &                  itype, nrgoto,ienter, iexit
      GO TO (1000,2000,3000),NRGOTO
! MTSTST TYPE MEASUREMENT (START STATION ODD ;START SATELLITE)
 1000 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1000 '
      IFROM=ILINKF(IENTER)
      ITO=ILINKT(IENTER)
      IX=MOD(IENTER,2)
      if(lokay)write(6,*)                                               &
     &   'METHD:ienter, ifrom, ito, ix ',ienter, ifrom, ito, ix
      IF(IX.EQ.0) GO TO 1010
! FIRST ELEMENT IS A STATION
      KSTA=MAX(ISTAFN(IFROM,NRGOTO),ISTATN(ITO,NRGOTO))
      JSTA=INDSTA(KSTA,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,32348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      DO 1005 I=1,3
      IF(JSTA.NE.INMSTA(I)) GO TO 1005
      GO TO 1007
 1005 END DO
      if(lokay)write(6,*) 'METHD:below 1005 '
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,12348)                                       &
     &     ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                   &
     &                   KSTA,JSTA,INMSTA
      STOP 16
!
 1007 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1007 '
      XPSEQ(KPSEQ)=DBLE(ILWSTA(I))
      if(lokay)write(6,*)                                               &
     &  'METHD:1007 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      LSAT=.TRUE.
      GO TO 1020
 1010 CONTINUE
! FIRST ELEMENT IS A SATELLITE
      KSAT=MAX(ISATFN(IFROM,NRGOTO),ISATTN(ITO,NRGOTO))
      JSAT=INDSAT(KSAT,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,32349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      DO 1015 I=1,3
      IF(JSAT.NE.INMSAT(I)) GO TO 1015
      GO TO 1017
 1015 END DO
! PRINT OUT AND STOP
      if(lokay)write(6,*) 'METHD:below 1010 '
      if(lokay)write(IOUT6,12347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,12349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      STOP 16
!
 1017 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1017 '
      XPSEQ(KPSEQ)=DBLE(ILWSAT(I))
      if(lokay)write(6,*)                                               &
     &   'METHD:1017 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      LSAT=.FALSE.
 1020 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1020 '
      DO 1300 ILINK=IENTER,IEXIT
! GET LAST ELEMENT IN EACH LINK (FIRST ELEMENT OF NEXT LINK)
      IFROM=ILINKF(ILINK)
      ITO=ILINKT(ILINK)
      IF(LSAT) GO TO 1190
      KSTA=MAX(ISTAFN(IFROM,NRGOTO),ISTATN(ITO,NRGOTO))
      JSTA=INDSTA(KSTA,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,32348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      DO 1105 I=1,3
      IF(JSTA.NE.INMSTA(I)) GO TO 1105
      GO TO 1107
 1105 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,12348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      STOP 16
!
 1107 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1107 '
      XPSEQ(KPSEQ)=DBLE(ILWSTA(I))
      if(lokay)write(6,*)                                               &
     &    'METHD:1107 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      LSAT=.NOT.LSAT
      GO TO 1300
 1190 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1190 '
      KSAT=MAX(ISATFN(IFROM,NRGOTO),ISATTN(ITO,NRGOTO))
      JSAT=INDSAT(KSAT,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,32349)                                       &
     &        ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                &
     &                   KSAT,JSAT,INMSAT
      DO 1215 I=1,3
      IF(JSAT.NE.INMSAT(I)) GO TO 1215
      GO TO 1217
 1215 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,12349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      STOP 16
!
 1217 CONTINUE
      if(lokay)write(6,*) 'METHD:below 1217 '
      XPSEQ(KPSEQ)=DBLE(ILWSAT(I))
      if(lokay)write(6,*)                                               &
     &   'METHD:1217 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      LSAT=.NOT.LSAT
 1300 END DO
      if(lokay)write(6,*) 'METHD:below 1300 '
      GO TO 400
!
!
!   MTSSST TYPE MEAS
 2000 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2000 '
      IFROM=ILINKF(IENTER)
      ITO=ILINKT(IENTER)
      IF(IENTER.GT.1) GO TO 2010
! FIRST ELEMENT IS A STATION
      KSTA=MAX(ISTAFN(IFROM,NRGOTO),ISTATN(ITO,NRGOTO))
      JSTA=INDSTA(KSTA,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,32348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      DO 2005 I=1,3
      IF(JSTA.NE.INMSTA(I)) GO TO 2005
      GO TO 2007
 2005 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,12348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      STOP 16
!
 2007 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2007 '
      XPSEQ(KPSEQ)=DBLE(ILWSTA(I))
      if(lokay)write(6,*)                                               &
     &    'METHD:2007 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      GO TO 2020
 2010 CONTINUE
! FIRST ELEMENT IS A SATELLITE
      KSAT=MAX(ISATFN(IFROM,NRGOTO),ISATTN(ITO,NRGOTO))
      JSAT=INDSAT(KSAT,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,32349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      DO 2015 I=1,3
      IF(JSAT.NE.INMSAT(I)) GO TO 2015
      GO TO 2017
 2015 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),IENTER,IFROM,ITO
      if(lokay)write(IOUT6,12349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      STOP 16
!
 2017 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2017 '
      XPSEQ(KPSEQ)=DBLE(ILWSAT(I))
      if(lokay)write(6,*)                                               &
     &    'METHD:2017 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
 2020 CONTINUE
      DO 2300 ILINK=IENTER,IEXIT
! GET LAST ELEMENT IN EACH LINK (FIRST ELEMENT OF NEXT LINK)
      IFROM=ILINKF(ILINK)
      ITO=ILINKT(ILINK)
      IF(ILINK.EQ.4) GO TO 2500
      KSAT=MAX(ISATFN(IFROM,NRGOTO),ISATTN(ITO,NRGOTO))
      JSAT=INDSAT(KSAT,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,32349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      DO 2215 I=1,3
      IF(JSAT.NE.INMSAT(I)) GO TO 2215
      GO TO 2217
 2215 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,12349)                                       &
     &    ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),                    &
     &                   KSAT,JSAT,INMSAT
      STOP 16
!
 2217 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2217 '
      XPSEQ(KPSEQ)=DBLE(ILWSAT(I))
      if(lokay)write(6,*)                                               &
     &   'METHD:2217 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
 2300 END DO
      GO TO 400
 2500 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2500 '
! GET LAST ELEMENT IN LINK 4 (A STATION)
      KSTA=MAX(ISTAFN(IFROM,NRGOTO),ISTATN(ITO,NRGOTO))
      JSTA=INDSTA(KSTA,ISEQ)
      if(lokay)write(IOUT6,32347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,32348)                                       &
     &   ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                     &
     &                   KSTA,JSTA,INMSTA
      DO 2505 I=1,3
      IF(JSTA.NE.INMSTA(I)) GO TO 2505
      GO TO 2507
 2505 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,12348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      STOP 16
!
 2507 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2507 '
      XPSEQ(KPSEQ)=DBLE(ILWSTA(I))
      if(lokay)write(6,*)                                               &
     &    'METHD:2507 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      GO TO 400
!
!
!--------------------------------------------------------------
 2600 continue
      if(lokay)write(6,*) 'METHD:below 2600 '
!
!     ....section for 5th leg
!
! GET LAST ELEMENT IN LINK 4 (A STATION)
      KSTA=MAX(ISTAFN(IFROM,NRGOTO),ISTATN(ITO,NRGOTO))
      JSTA=INDSTA(KSTA, 1  )
      if(lokay)write(IOUT6,32347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,32348)                                       &
     &   ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                     &
     &                   KSTA,JSTA,INMSTA
      DO 2605 I=1,3
      IF(JSTA.NE.INMSTA(I)) GO TO 2605
      GO TO 2607
 2605 END DO
! PRINT OUT AND STOP
      if(lokay)write(IOUT6,12347) SUB(ICALL),ILINK,IFROM,ITO
      if(lokay)write(IOUT6,12348)                                       &
     &    ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRGOTO),                    &
     &                   KSTA,JSTA,INMSTA
      STOP 16
!
 2607 CONTINUE
      if(lokay)write(6,*) 'METHD:below 2607 '
      XPSEQ(KPSEQ)=DBLE(ILWSTA(I))
      if(lokay)write(6,*)                                               &
     &    'METHD:2607 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
      KPSEQ=KPSEQ+1
      GO TO 5000
!----------------------------------------------------------
!C FIRST ELEMENT IS A SATELLITE
!      KSAT=MAX0(ISATFN(IFROM,NRGOTO),ISATTN(ITO,NRGOTO))
!      JSAT=INDSAT(KSAT, 1  )
!      if(lokay)write(IOUT6,32347) SUB(ICALL),IENTER,IFROM,ITO
!      if(lokay)write(IOUT6,32349)
!     1   ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),
!     .                   KSAT,JSAT,INMSAT
!C
!      DO 2615 I=1,3
!      IF(JSAT.NE.INMSAT(I)) GO TO 2615
!      GO TO 2617
! 2615 CONTINUE
!C PRINT OUT AND STOP
!      if(lokay)write(6,*) 'METHD:below 2610 '
!      if(lokay)write(IOUT6,12347) SUB(ICALL),IENTER,IFROM,ITO
!      if(lokay)write(IOUT6,12349)
!     1   ISATFN(IFROM,NRGOTO),ISATTN(IFROM,NRGOTO),
!     .                   KSAT,JSAT,INMSAT
!      STOP 16
!CRAY\      call tracebk
!C
! 2617 CONTINUE
!      if(lokay)write(6,*) 'METHD:below 2617 '
!      XPSEQ(KPSEQ)=DFLOAT(ILWSAT(I))
!      if(lokay)write(6,*)
!     1   'METHD:2617 kpseq,xpseq(kpseq) ',kpseq,xpseq(kpseq)
!      KPSEQ=KPSEQ+1
!      LSAT=.FALSE.
! 2620 CONTINUE
!      if(lokay)write(6,*) 'METHD:below 2620 '
!C
!C
!CCC      DO 2613 ILINK=IENTER,IEXIT
!      ILINK=IEXIT
!C
!C
!C GET LAST ELEMENT IN EACH LINK (FIRST ELEMENT OF NEXT LINK)
!      IFROM=ILINKF(ILINK)
!      ITO=ILINKT(ILINK)
!      IF(LSAT) GO TO 1190
!      KSTA=MAX0(ISTAFN(IFROM,NRGOTO),ISTATN(ITO,NRGOTO))
!      JSTA=INDSTA(KSTA,ISEQ)
!      if(lokay)write(IOUT6,32347) SUB(ICALL),ILINK,IFROM,ITO
!      if(lokay)write(IOUT6,32348) ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRG
!     .                   KSTA,JSTA,INMSTA
!      DO 26005 I=1,3
!      IF(JSTA.NE.INMSTA(I)) GO TO 26005
!      GO TO 26007
!26005 CONTINUE
!C PRINT OUT AND STOP
!      if(lokay)write(IOUT6,12347) SUB(ICALL),ILINK,IFROM,ITO
!      if(lokay)write(IOUT6,12348) ISTAFN(IFROM,NRGOTO),ISTATN(IFROM,NRG
!     .                   KSTA,JSTA,INMSTA
!      STOP 16
!CRAY\      call tracebk
!C
!26007 CONTINUE
!      if(lokay)write(6,*) 'METHD:below 26007 '
!      XPSEQ(KPSEQ)=DFLOAT(ILWSTA(I))
!      if(lokay)write(6,*) 'METHD:26007 kpseq,xpseq(kpseq) ',kpseq,xpseq
!      KPSEQ=KPSEQ+1
!      LSAT=.NOT.LSAT
!2613  continue
!     go to 5000
!--------------------------------------------------------------
!
!
 3000 CONTINUE
      if(lokay)write(6,*) 'METHD:below 3000 '
!
!
! MTPTT TYPE MEASUREMENT
! NOT YET IMPLEMENTED
      if(lokay)write(IOUT6,12344)
12344 FORMAT(' SUBROUTINE METHD: MTPTT MEAS TYPE NOT YET IMPLEMENTED ')
!
!     STOP 2344
!
 5000 CONTINUE
      if(lokay)write(6,*) 'METHD:below 5000 '
!
      KPSEQ=KPSEQ-1
      CALL RTNELV(NTIMES,KPSEQ,XPSEQ)
      if(lokay)write(6,22345) NSATB,NSTAB,KPSEQ
22345 FORMAT(' methd:NSATB,NSTAB,KPSEQ ',3I10)
      if(lokay)write(6,22346) (XPSEQ(K),K=1,KPSEQ)
22346 FORMAT(' methd:xpseq'/(1x,5D15.6)   )
!
!
! WRITE OUT RECORD
      DO 6000 I=1,20
      ALOC(I)=XPSEQ(I)
 6000 END DO
      IF(LLOCRI) WRITE(IUNT19) XPSEQ
!
      RETURN
12347 FORMAT(' METHD: PROBLEM IN ',A6,' SECTION'/                       &
     &      ' LINK,IFROM,ITO ',3I5)
12348 FORMAT(' ISTAFN,ISTATN,KSTA,JSTA,INMSTA(3) ',7I5)
12349 FORMAT(' ISATFN,ISATTN,KSAT,JSAT,INMSAT(3) ',7I5)
32347 FORMAT(' METHD: IN ',A6,' SECTION'/                               &
     &      ' LINK,IFROM,ITO ',3I5)
32348 FORMAT(' METHD:ISTAFN,ISTATN,KSTA,JSTA,INMSTA(3) ',7I5)
32349 FORMAT(' METHD:ISATFN,ISATTN,KSAT,JSAT,INMSAT(3) ',7I5)
      END
