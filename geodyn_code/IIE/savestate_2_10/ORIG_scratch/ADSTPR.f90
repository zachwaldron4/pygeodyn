!$ADSTPR
      SUBROUTINE ADSTPR(APRIOR,SXYZ,STAINF,SPSIG,STNAME,ISTANO,SAVSTA)
!********1*********2*********3*********4*********5*********6*********7**
! ADSTPR           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: ADJUSTS STATION PARAMETERS AND OUTPUTS TO UNIT 6
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   APRIOR   I    A    A PRIORI COORDINATES OF ADJUSTED STATIONS
!   SXYZ     I    A    EARTH CENTERED CARTESIAN XYZ COORDINATES
!   STAINF   I    A    STATION INFORMATION ARRAY
!   SPSIG    I    A    MASTER STATION VARIANCE-COVARIANCE
!   STNAME   I    A    STATION NAME, ALPHANUMERIC
!   ISTANO   I    A    EXTERNAL STATION NUMBER
!   SAVSTA   I    A    CONTAINS STATION RECTANGULAR COORDINATES
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION APRIOR(25,1),SXYZ(3,1),STAINF(NSTAIN,1),SPSIG(6,3,1),   &
     &          SPPSIG(6),STNAME(1),ISTANO(1)
      DIMENSION SAVSTA(NSTA,3)
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      CHARACTER(8)      :: DELTAP, PRIORI, ADJUST
      DATA DELTAP/' DELTA'/,PRIORI/' APRIORI'/,ADJUST/'ADJUSTED'/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(NSTAE.LE.0) RETURN
      ISTACT=0
      JLINE6=MLINE6
!  PRINT OUT RECTANGULAR
   10 CONTINUE
      ISTACT=ISTACT+1
      IF(ISTACT.GT.NSTAE) GO TO 100
      DO JJ=1,3
      SAVSTA(ISTACT,JJ)=SXYZ(JJ,ISTACT)
!     WRITE(6,*)' DBG LOADING SAVSTA ',SAVSTA(ISTACT,JJ),ISTACT,JJ
      ENDDO
!
!   SET PRINT OUT OPTIONS
      JLINE6=JLINE6+4
      IF(JLINE6.LE.MLINE6) GO TO 50
      IPAGE6=IPAGE6+1
      JLINE6=7
      WRITE(IOUT6,3000) NGLOBL
      WRITE(IOUT6,3010)
   50 CONTINUE
      ILINE6=JLINE6
      WRITE(IOUT6,2999)
      WRITE(IOUT6,3030) PRIORI,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  APRIOR(4,ISTACT),APRIOR(5,ISTACT),              &
     &                  APRIOR(6,ISTACT),APRIOR(14,ISTACT),             &
     &                  APRIOR(17,ISTACT),APRIOR(19,ISTACT),            &
     &                  APRIOR(15,ISTACT),APRIOR(16,ISTACT),            &
     &                  APRIOR(18,ISTACT)
!
      SPPSIG(1)=SQRT(SPSIG(1,2,ISTACT))
      SPPSIG(4)=SQRT(SPSIG(4,2,ISTACT))
      SPPSIG(6)=SQRT(SPSIG(6,2,ISTACT))
      SPPSIG(2)=SPSIG(2,2,ISTACT)/(SPPSIG(1)*SPPSIG(4))
      SPPSIG(3)=SPSIG(3,2,ISTACT)/(SPPSIG(1)*SPPSIG(6))
      SPPSIG(5)=SPSIG(5,2,ISTACT)/(SPPSIG(4)*SPPSIG(6))
      WRITE(IOUT6,3030) ADJUST,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  SXYZ(1,ISTACT),SXYZ(2,ISTACT),SXYZ(3,ISTACT),   &
     &                  SPPSIG(1),SPPSIG(4),SPPSIG(6),SPPSIG(2),        &
     &                  SPPSIG(3),SPPSIG(5)
!
      DEL1=SXYZ(1,ISTACT)-APRIOR(4,ISTACT)
      DEL2=SXYZ(2,ISTACT)-APRIOR(5,ISTACT)
      DEL3=SXYZ(3,ISTACT)-APRIOR(6,ISTACT)
      WRITE(IOUT6,3030) DELTAP,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  DEL1,DEL2,DEL3
      GO TO 10
!  GET GEODETIC PRINT OUT
  100 CONTINUE
      ISTACT=0
!
!   SET PRINT OUT OPTIONS
      JLINE6=JLINE6+4
      IF(JLINE6.LE.MLINE6) GO TO 105
      IPAGE6=IPAGE6+1
      JLINE6=7
      WRITE(IOUT6,3000) NGLOBL
      WRITE(IOUT6,3040)
      ILINE6=JLINE6
      GO TO 110
  105 CONTINUE
      ILINE6=JLINE6
      WRITE(IOUT6,3040)
  110 CONTINUE
      ISTACT=ISTACT+1
      IF(ISTACT.GT.NSTAE) GO TO 200
!
!   SET PRINT OUT OPTIONS
      JLINE6=JLINE6+4
      IF(JLINE6.LE.MLINE6) GO TO 150
      IPAGE6=IPAGE6+1
      JLINE6=7
      WRITE(IOUT6,3000) NGLOBL
      WRITE(IOUT6,3040)
  150 CONTINUE
      ILINE6=JLINE6
      CALL OUTRAD(APRIOR(1,ISTACT),ILATD,ILATM,SLAT,.FALSE.)
      CALL OUTRAD(APRIOR(2,ISTACT),ILOND,ILONM,SLON,.TRUE.)
      WRITE(IOUT6,2999)
      WRITE(IOUT6,3050) PRIORI,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  ILATD,ILATM,SLAT,ILOND,ILONM,SLON,              &
     &                  APRIOR(3,ISTACT),APRIOR(8,ISTACT),              &
     &                  APRIOR(11,ISTACT),APRIOR(13,ISTACT),            &
     &                  APRIOR(9,ISTACT),APRIOR(10,ISTACT),             &
     &                  APRIOR(12,ISTACT)
!
      SPPSIG(1)=SQRT(SPSIG(1,1,ISTACT))
      SPPSIG(4)=SQRT(SPSIG(4,1,ISTACT))
      SPPSIG(6)=SQRT(SPSIG(6,1,ISTACT))
      SPPSIG(2)=SPSIG(2,1,ISTACT)/(SPPSIG(1)*SPPSIG(4))
      SPPSIG(3)=SPSIG(3,1,ISTACT)/(SPPSIG(1)*SPPSIG(6))
      SPPSIG(5)=SPSIG(5,1,ISTACT)/(SPPSIG(4)*SPPSIG(6))
      SPPSIG(1)=SPPSIG(1)/SECRAD
      SPPSIG(4)=SPPSIG(4)/SECRAD
      CALL OUTRAD(STAINF(1,ISTACT),ILATD,ILATM,SLAT,.FALSE.)
      CALL OUTRAD(STAINF(4,ISTACT),ILOND,ILONM,SLON,.TRUE.)
      WRITE(IOUT6,3050) ADJUST,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  ILATD,ILATM,SLAT,ILOND,ILONM,SLON,              &
     &                  STAINF(7,ISTACT),SPPSIG(1),SPPSIG(4),           &
     &                  SPPSIG(6),SPPSIG(2),SPPSIG(3),SPPSIG(5)
!
      DEL1=STAINF(1,ISTACT)-APRIOR(1,ISTACT)
      CALL OUTRAD(DEL1,ILATD,ILATM,SLAT,.FALSE.)
      DEL2=STAINF(4,ISTACT)-APRIOR(2,ISTACT)
      CALL OUTRAD(DEL2,ILOND,ILONM,SLON,.FALSE.)
      DEL3=STAINF(7,ISTACT)-APRIOR(3,ISTACT)
      WRITE(IOUT6,3050) DELTAP,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  ILATD,ILATM,SLAT,ILOND,ILONM,SLON,DEL3
      GO TO 110
!  GET CYLINDRICAL PRINT OUT
  200 CONTINUE
      ISTACT=0
!
!   SET PRINT OUT OPTIONS
      JLINE6=JLINE6+4
      IF(JLINE6.LE.MLINE6) GO TO 205
      IPAGE6=IPAGE6+1
      JLINE6=7
      WRITE(IOUT6,3000) NGLOBL
      WRITE(IOUT6,8010)
      ILINE6=JLINE6
      GO TO 210
  205 CONTINUE
      ILINE6=JLINE6
      WRITE(IOUT6,8010)
  210 CONTINUE
      ISTACT=ISTACT+1
      IF(ISTACT.GT.NSTAE) RETURN
!
!   SET PRINT OUT OPTIONS
      JLINE6=JLINE6+4
      IF(JLINE6.LE.MLINE6) GO TO 250
      IPAGE6=IPAGE6+1
      JLINE6=7
      WRITE(IOUT6,3000) NGLOBL
      WRITE(IOUT6,8010)
  250 CONTINUE
      ILINE6=JLINE6
      CALL OUTRAD(APRIOR(2,ISTACT),ILOND,ILONM,SLON,.TRUE.)
      WRITE(IOUT6,2999)
      WRITE(IOUT6,8030) PRIORI,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  APRIOR(7,ISTACT),ILOND,ILONM,SLON,              &
     &                  APRIOR(6,ISTACT),APRIOR(20,ISTACT),             &
     &                  APRIOR(23,ISTACT),APRIOR(25,ISTACT),            &
     &                  APRIOR(21,ISTACT),APRIOR(22,ISTACT),            &
     &                  APRIOR(24,ISTACT)
!
      SPPSIG(1)=SQRT(SPSIG(1,3,ISTACT))
      SPPSIG(4)=SQRT(SPSIG(4,3,ISTACT))
      SPPSIG(6)=SQRT(SPSIG(6,3,ISTACT))
      SPPSIG(2)=SPSIG(2,3,ISTACT)/(SPPSIG(1)*SPPSIG(4))
      SPPSIG(3)=SPSIG(3,3,ISTACT)/(SPPSIG(1)*SPPSIG(6))
      SPPSIG(5)=SPSIG(5,3,ISTACT)/(SPPSIG(4)*SPPSIG(6))
      SPPSIG(4)=SPPSIG(4)/SECRAD
      CALL OUTRAD(STAINF(4,ISTACT),ILOND,ILONM,SLON,.TRUE.)
      SAD=SQRT(SXYZ(1,ISTACT)*SXYZ(1,ISTACT)+SXYZ(2,ISTACT)            &
     &         *SXYZ(2,ISTACT))
      WRITE(IOUT6,8030) ADJUST,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  SAD,ILOND,ILONM,SLON,SXYZ(3,ISTACT),            &
     &                  SPPSIG(1),SPPSIG(4),SPPSIG(6),SPPSIG(2),        &
     &                  SPPSIG(3),SPPSIG(5)
!
      DEL1=SAD-APRIOR(7,ISTACT)
      DEL2=STAINF(4,ISTACT)-APRIOR(2,ISTACT)
      CALL OUTRAD(DEL2,ILOND,ILONM,SLON,.FALSE.)
      DEL3=SXYZ(3,ISTACT)-APRIOR(6,ISTACT)
      WRITE(IOUT6,8030) DELTAP,STNAME(ISTACT),ISTANO(ISTACT),           &
     &                  DEL1,ILOND,ILONM,SLON,DEL3
      GO TO 210
 2999 FORMAT(' ')
 3000 FORMAT('1','C',34X,'STATION POSITION ADJUSTMENT SUMMARY FOR ',    &
     &       'ITERATION NUMBER',I3/' ')
!
 3010  FORMAT('0'/5X,'EARTH FIXED RECTANGULAR COORDINATES',             &
     & 37X,'STANDARD DEVIATION',15X,'CORRELATION'/' '/                  &
     & 16X,'STATION',11X,'X',13X,                                       &
     & 'Y',13X,'Z',13X,'X',9X,'Y',9X,'Z',                               &
     &  9X,'X-Y',6X,'X-Z',6X,'Y-Z'/14X,                                 &
     & 'NAME NUMBER',8X,'(M)',3(11X,'(M)'),2(7X,'(M)'))
 3030 FORMAT(' ',A8,1X,A7,I9,1X,3F14.3,2X,3F10.3,2X,3F9.4)
!
 3040 FORMAT('0'/5X, 'GEODETIC COORDINATES',53X,'STANDARD DEVIATION',   &
     &  11X,'CORRELATION'/1X /                                          &
     &  16X,'STATION',3X,'GEODETIC LATITUDE',3X,                        &
     & 'EAST LONGITUDE',4X,'HEIGHT',                                    &
     &  6X,'LAT',6X,'LON',7X,'HT',3X,'LAT-LON',                         &
     & 3X,'LAT-HT',4X,'LON-HT'/14X,'NAME NUMBER',                       &
     & 2(3X,'DEG MN SECONDS',1X),                                       &
     & '  (METERS)',1X,2(3X,'(SEC)',1X),4X,'(M)')

 3050 FORMAT(' ',A8,1X,A7,I9,2(I6,I3,F8.4,1X),F10.3,2F9.4,F9.3,1X,      &
     & F8.4,2(2X,F8.4))
!
 8010 FORMAT('0'/5X,'EARTH FIXED CYLINDRICAL COORDINATES',37X,          &
     &   'STANDARD DEVIATION',14X,'CORRELATION'/' '/16X,'STATION',3X,   &
     &   'SPIN AXIS DIST',2X,'EAST LONGITUDE',8X,'Z',10X,'SAD',7X,      &
     &   'LON',8X,'Z',6X,                                               &
     & 'SAD-LON',3X,'SAD-Z',4X,'LON-Z'/14X,'NAME NUMBER',7X,'(M)',7X,   &
     & 'DEG MN SECONDS',7X,'(M)',9X,'(M)',6X,'(SEC)',6X,'(M)')
 8030 FORMAT(' ',A8,1X,A7,I9,1X,F13.3,I6,I3,F8.4,F13.3,1X,3F10.3,1X,    &
     & 3F9.4)

      END
