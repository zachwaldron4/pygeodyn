!$PREWRT
      SUBROUTINE PREWRT(IYMD  ,IHM   ,SEC   ,KOUNTO,KNTBLK,SEGMNT,      &
     &    NTYPE ,SUMCOR,AA    ,NUMOB ,RESCAL)
!********1*********2*********3*********4*********5*********6*********7**
! PREWRT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CONTROLS THE PRINTING OF MEASUREMENT CORRECTION INFORMATION
!            ON UNIT 16
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IYMD     I    S    DATE IN YYMMDD FOR THE PARTICULAR MEASUREMENT
!                      CORRECTION
!   IHM      I    S    HH MM OF THE SAME DATE
!   SEC      I    S    SECONDS OF THE SAME DATE
!   KOUNTO   I    S    OBSERVATION COUNT
!   KNTBLK   I    S    OBSERVATION BLOCK COUNT
!   SEGMNT   I    A    OBSERVATION BLOCK SEGMENT LABEL
!   NTYPE    I    S    MEASUREMENT TYPE
!   SUMCOR   I    S    SUM OF OBSERVATION CORRECTIONS
!   AA       I    A    REAL DYNAMIC ARRAY
!   NUMOB    I    S    OBSERVATION COUNT WITHIN THE BLOCK
!   RESCAL   I    S    SCALE FACTOR FOR RESIDUAL OUTPUT UNITS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER ( METRIX = 37 )
      PARAMETER ( ZERO   = 0.0D0 )
!
! /CBLOKI/ INTEGER INFORMATION ABOUT DATA BLOCK
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      CHARACTER(8)      :: G
      DIMENSION AA(1),IPART(5),G(5),VALUE(5)
      CHARACTER(1)      :: BLANK
      CHARACTER(1)      :: GEODYN
      DATA BLANK/' '/,GEODYN/'G'/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      IF(MTYPE.LT.13) RETURN
      SCALE=RESCAL
      SUMPRT=SUMCOR*SCALE
      DO 1000 I=1,5
         IPART(I)=0
         G(I)=BLANK
         VALUE(I)=ZERO
 1000 END DO
      I1=2
      I2=9
      INCR=1
!
!CCC  IF(MTYPE.GT.40) GO TO 2000      9/19/91 JJM
      IF( MTYPE .GT. METRIX ) GO TO 2000
!
      I1=NTYPE-MTYPE+1
      IF(MTYPE.GT.14) I1=I1+2
      I2=8
      INCR=2
 2000 CONTINUE
      NCORR=0
      DO 3000 NH=1,NHEADB
         NHP1=NH+1
         DO 3000 I=I1,I2,INCR
            IF(.NOT.LSWTCH(I,NHP1)) GO TO 3000
            NCORR=NCORR+1
 3000 CONTINUE
      IF(NCORR.LE.0) RETURN
      NLINES=(NCORR-1)/5+1
      NLIN16=ILIN16+NLINES+1
      IF(NLIN16.LE.MLIN16) GO TO 4000
! PAGE HEADER
      IPAG16=IPAG16+1
      WRITE(IOUT16,10000) NARC,IPAG16
      ILIN16=3
 4000 CONTINUE
      WRITE(IOUT16,10300)
      ILIN16=ILIN16+1
      LFIRST=.TRUE.
      NC=0
      IF(MTYPE.GT.14) GO TO 5000
      IF(.NOT.LSWTCH(9,2)) GO TO 5000
      INDEX=KOBCOR(9,1)+NUMOB-1
      IF(AA(INDEX).EQ.ZERO) GO TO 5000
      NC=1
      IPART(NC)=19
      IF(.NOT.LPRE(9,1)) G(NC)=GEODYN
      VALUE(NC)=AA(INDEX)
 5000 CONTINUE
         DO 6000 NH=1,NHEADB
         NHP1=NH+1
         DO 6000 I=I1,I2,INCR
            IF(.NOT.LSWTCH(I,NHP1)) GO TO 6000
            NC=NC+1
            IPART(NC)=NH*10+I
            G(NC)=BLANK
            IF(.NOT.LPRE(I,NH)) G(NC)=GEODYN
            INDEX=KOBCOR(I,NH)+NUMOB-1
            VALUE(NC)=AA(INDEX)*SCALE
            IF(NC.LT.5) GO TO 6000
            ILIN16=ILIN16+1
! FIRST LINE OF CORRECTIONS PRINTOUT
            IF(LFIRST) WRITE(IOUT16,10400)                              &
     &                       IYMD,IHM,SEC,NTYPE,SUMPRT,                 &
     &                       (IPART(J),G(J),VALUE(J),J=1,5),            &
     &                       KOUNTO,KNTBLK,SEGMNT
! SUBSEQUENT LINES OF CORRECTIONS PRINTOUT
            IF(.NOT.LFIRST)WRITE(IOUT16,10500)                          &
     &                       (IPART(J),G(J),VALUE(J),J=1,NC)
            LFIRST=.FALSE.
            NC=0
 6000 CONTINUE
      IF(NC.LE.0) RETURN
! FIRST LINE OF CORRECTIONS PRINTOUT
      IF(LFIRST) WRITE(IOUT16,10400) IYMD,IHM,SEC,NTYPE,SUMPRT,         &
     &                               (IPART(J),G(J),VALUE(J),J=1,5),    &
     &                               KOUNTO,KNTBLK,SEGMNT
! SUBSEQUENT LINES OF CORRECTIONS PRINTOUT
      IF(.NOT.LFIRST)WRITE(IOUT16,10500)(IPART(J),G(J),VALUE(J),J=1,NC)
      ILIN16=ILIN16+1
      RETURN
10000 FORMAT('1',37X,'OBSERVATION CORRECTIONS FOR ARC',I3,38X,          &
     &   'UNIT 16 PAGE NO.',I6/                                         &
     &   '  DATE  GREENWICH TIME MEAS CORRECTION',                      &
     &     5(2X,'CORRECTION',4X), 2X,'OBSERVATION'/                     &
     &   ' YYMMDD HHMM SS.SSSSSS TYPE SUMMATION ',                      &
     &   5(1X,'NO.    VALUE',3X),2X,'NUMBER BLOCK')
10300 FORMAT(1X)
10400 FORMAT(1X,I6,I5,F10.6,2X,I2,1X,G11.4,5(1X,I2,A1,1X,G11.4),        &
     &   I8,I5,A1)
10500 FORMAT(38X,5(1X,I2,A1,1X,G11.4))
      END
