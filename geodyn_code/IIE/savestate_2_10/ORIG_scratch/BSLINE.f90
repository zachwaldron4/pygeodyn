!$BSLINE
      SUBROUTINE BSLINE(ITIME,ICRD,ICON,ISTANO,STNAME,ISSTA,NRMTOT,     &
     &                  SPSIG,SXYZ,DXSDPM,ATPA)
!********1*********2*********3*********4*********5*********6*********7**
! BSLINE           84/05/23            0000.0    PGMR - D. ROWLANDS
!
! FUNCTION:  (1) COMPUTE BASELINES BETWEEN ADJUSTED STATIONS AND ALL
!                STATIONS
!            (2) COMPUTE SIGMAS FOR ALL BASELINES COMING FROM ADJUSTING
!                STATIONS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ITIME    I    S    1 IF FIRST CALL;2 IF SECOND
!   ICRD     I    A    POINTER TO SYSTEM OF ADJUSTMENT
!   ICON     I    A    POINTER TO (INTERNAL) MASTER STATION NUMBER
!                      FOR EACH STATION
!   ISTANO   I    A    EXTERNAL STATION NUMBER
!   STNAME   I    A    STATION NAME
!   ISSTA    I    S    STARTING STATION PARAMETER NUMBER
!   NRMTOT   I    S    NUMBER OF PARAMETERS IN THE NORMAL MATRIX
!   SPSIG    I    A    ADJUSTED MASTER STATION VARIANCE/COVARIANCES IN
!                      ALL SYSTEMS
!   SXYZ     I    A    ADJUSTED STATION COORDIATES IN XYZ
!   DXSDPM   I    A    MATRIX OF PARTIALS OF SLAVE STAION XYZ COORD-
!                      INATES WRT MASTER STATION COORDINATES (SYSTEM OF
!                      ADJUSTMENT)
!   ATPA     I    A    INVERTED NORMAL MATRIX
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION  ICON(1),SPSIG(6,3,1),SXYZ(3,1),DXSDPM(3,3,1),          &
     &           ICRD(1),SPPSIG(6,2),COVAR(3,3),ATPA(1),                &
     &           ISTANO(1),STNAME(1),WORD(2)
! /CEARTH/ GEOMETRICAL EARTH CONSTANTS USED FOR MEAS. PROCESSING
      COMMON/CEARTH/AEG,AEGSQ,FGINV,FG,EGSQ,EGSQP1,C1,C2,FOURC1,TWOC2,  &
     &              XH2,XL2,WREF,RMEAN,REFC20,REFC40,REFC60,BEG,CBLTC,  &
     &              WAE2,GAMMAA,FSTAR,F4
      COMMON/CITER /NINNER,NARC,NGLOBL
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
      DATA ZERO /0.D0/,TWO/2.D0/
      CHARACTER(8)      :: WORD
      DATA WORD/' APRIORI','ADJUSTED'/
      IDEXNO(I)=(I-1)*NRMTOT-(I-1)*(I-2)/2+1
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      ILINE6=MLINE6
      NUP=MIN(NSTA-1,NSTAE)
      IF(NUP.LE.0) RETURN
      DO 100 I=1,NUP
      INSTA=ICON(I)
      INADJ=MOD(ICRD(I),10)
      I1=I+1
      SPPSIG(1,1)=SPSIG(1,2,I)
      SPPSIG(2,1)=SPSIG(2,2,I)
      SPPSIG(3,1)=SPSIG(3,2,I)
      SPPSIG(4,1)=SPSIG(4,2,I)
      SPPSIG(5,1)=SPSIG(5,2,I)
      SPPSIG(6,1)=SPSIG(6,2,I)
      DO 100 J=I1,NSTA
      JNSTA=ICON(J)
      IF(JNSTA.EQ.INSTA) GO TO 100
      ILSTA=MIN(INSTA,JNSTA)
      IBSTA=MAX(INSTA,JNSTA)
!   BASELINE
      PART3=ZERO
      DX=SXYZ(1,I)-SXYZ(1,J)
      DY=SXYZ(2,I)-SXYZ(2,J)
      DZ=SXYZ(3,I)-SXYZ(3,J)
      BSLN2=DX*DX+DY*DY+DZ*DZ
      BSLN=SQRT(BSLN2)
      IF(J.GT.NSTAE) GO TO 90
      JNADJ=MOD(ICRD(J),10)
      ILADJ=INADJ
      IBADJ=JNADJ
      II=I
      JJ=J
      IF(INSTA.LT.JNSTA) GO TO 31
      ILADJ=JNADJ
      IBADJ=INADJ
      II=J
      JJ=I
   31 CONTINUE
! GATHER THE FULL 3X3 INTERSTATION CROSS COVARIANCE
      IPT1=IDEXNO(ISSTA+(ILSTA-1)*3)-1
      IPT2=IDEXNO(ISSTA+(ILSTA-1)*3+1)-2
      IPT3=IDEXNO(ISSTA+(ILSTA-1)*3+2)-3
      IPT11=IPT1+(IBSTA-ILSTA)*3
      IPT21=IPT2+(IBSTA-ILSTA)*3
      IPT31=IPT3+(IBSTA-ILSTA)*3
      DO 35 JJJ=1,3
      COVAR(1,JJJ)=ATPA(IPT11+JJJ)
      COVAR(2,JJJ)=ATPA(IPT21+JJJ)
      COVAR(3,JJJ)=ATPA(IPT31+JJJ)
   35 END DO
      IF(ILADJ.EQ.2) GO TO 38
! GET INTERSTA COVARIANCE CONTRIBUTION FROM FIRST STATION INTO XYZ
      DO 37 JJJ=1,3
      TMP1=DXSDPM(1,1,II)*COVAR(1,JJJ)+DXSDPM(2,1,II)*COVAR(2,JJJ)      &
     &    +DXSDPM(3,1,II)*COVAR(3,JJJ)
      TMP2=DXSDPM(1,2,II)*COVAR(1,JJJ)+DXSDPM(2,2,II)*COVAR(2,JJJ)      &
     &    +DXSDPM(3,2,II)*COVAR(3,JJJ)
      TMP3=DXSDPM(1,3,II)*COVAR(1,JJJ)+DXSDPM(2,3,II)*COVAR(2,JJJ)      &
     &    +DXSDPM(3,3,II)*COVAR(3,JJJ)
      COVAR(1,JJJ)=TMP1
      COVAR(2,JJJ)=TMP2
      COVAR(3,JJJ)=TMP3
   37 END DO
   38 IF(IBADJ.EQ.2) GO TO 40
! GET INTERSTA COVARIANCE CONTRIBUTION FROM SECOND STATION INTO XYZ
      DO 39 JJJ=1,3
      TMP1=COVAR(JJJ,1)*DXSDPM(1,1,JJ)+COVAR(JJJ,2)*DXSDPM(2,1,JJ)      &
     &    +COVAR(JJJ,3)*DXSDPM(3,1,JJ)
      TMP2=COVAR(JJJ,1)*DXSDPM(1,2,JJ)+COVAR(JJJ,2)*DXSDPM(2,2,JJ)      &
     &    +COVAR(JJJ,3)*DXSDPM(3,2,JJ)
      TMP3=COVAR(JJJ,1)*DXSDPM(1,3,JJ)+COVAR(JJJ,2)*DXSDPM(2,3,JJ)      &
     &    +COVAR(JJJ,3)*DXSDPM(3,3,JJ)
      COVAR(JJJ,1)=TMP1
      COVAR(JJJ,2)=TMP2
      COVAR(JJJ,3)=TMP3
   39 END DO
! GET CONTRIBUTION FROM COVARIANCE TO BASELINE SIGMA
   40 CONTINUE
      PART3=DX*DX*COVAR(1,1)+DY*DY*COVAR(2,2)+DZ*DZ*COVAR(3,3)          &
     &     +DX*DY*(COVAR(1,2)+COVAR(2,1))                               &
     &     +DX*DZ*(COVAR(1,3)+COVAR(3,1))                               &
     &     +DY*DZ*(COVAR(2,3)+COVAR(3,2))
      SPPSIG(1,2)=SPSIG(1,2,J)
      SPPSIG(2,2)=SPSIG(2,2,J)
      SPPSIG(3,2)=SPSIG(3,2,J)
      SPPSIG(4,2)=SPSIG(4,2,J)
      SPPSIG(5,2)=SPSIG(5,2,J)
      SPPSIG(6,2)=SPSIG(6,2,J)
      PART1=DX*DX*(SPPSIG(1,1)+SPPSIG(1,2))                             &
     &     +DY*DY*(SPPSIG(4,1)+SPPSIG(4,2))                             &
     &     +DZ*DZ*(SPPSIG(6,1)+SPPSIG(6,2))                             &
     &+TWO*(DX*DY*(SPPSIG(2,1)+SPPSIG(2,2))                             &
     &     +DX*DZ*(SPPSIG(3,1)+SPPSIG(3,2))                             &
     &     +DY*DZ*(SPPSIG(5,1)+SPPSIG(5,2)))
      GO TO 95
   90 CONTINUE
      PART1=DX*DX*SPPSIG(1,1)+DY*DY*SPPSIG(4,1)+DZ*DZ*SPPSIG(6,1)       &
     & +TWO*(DX*DY*SPPSIG(2,1)+DX*DZ*SPPSIG(3,1)+DY*DZ*SPPSIG(5,1))
   95 CONTINUE
      BSSG=(PART1-TWO*PART3)/BSLN2
!   PRINT OUT BASELINES
      JLINE6=ILINE6+1
      IF(JLINE6.LE.MLINE6) GO TO 96
      IPAGE6=IPAGE6+1
      JLINE6=5
      WRITE(IOUT6,10000) WORD(ITIME),NGLOBL,IPAGE6
   96 CONTINUE
      ILINE6=JLINE6
      IF(BSSG.GT.ZERO) GO TO 97
      ILINE6=ILINE6+3
      WRITE(IOUT6,90000) BSSG
      BSSG=ABS(BSSG)
   97 CONTINUE
      BSSG=SQRT(BSSG)
      WRITE(IOUT6,10100) STNAME(I),ISTANO(I),(SXYZ(JIJ,I),JIJ=1,3),     &
     &                   STNAME(J),ISTANO(J),(SXYZ(IJI,J),IJI=1,3),     &
     &                   BSLN,BSSG
  100 CONTINUE
      RETURN
10000 FORMAT('1',42X,A8,' STATION BASELINES FOR OUTER ITERATION',       &
     & I3,18X,'UNIT  6 PAGE NO.',I6/' ' /                               &
     & 2(6X,'STATION',11X,'X',11X,'Y',11X,'Z',6X),2(2X,'BASELINE')/     &
     & 2(3X,'NAME',4X,'NUMBER',6X,'(M)',2(9X,'(M)'),5X),5X,             &
     & '(M)',4X,'SIGMA(M)')
10100 FORMAT(2(1X,A8,I9,1X,3F12.3,1X),F12.3,F8.3)
90000 FORMAT('0***** NEGATIVE ARGUMENT (' ,D13.6,                       &
     &   ') TO DSQRT IN SUBROUTINE BSLINE'/' ')
      END
