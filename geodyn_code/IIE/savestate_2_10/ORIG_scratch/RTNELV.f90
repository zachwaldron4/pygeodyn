!$RTNELV
      SUBROUTINE RTNELV(NSEQ,NPT,XPT)
!SDOC*******************************************************************
!
!   PURPOSE:    OBTAIN THE INFORMATION NECESSARY TO RETAIN CRITCAL
!               ELEVATION ANGLES. THESE ELEVATION ANGLES WILL BE USED
!               FOR PRINT OUT AND ELEVATION CUTOFF CRITERIA
!   ARGUMENTS:  NSEQ   - FOR ANGLE MEASUREMENTS NSEQ=1.
!                        FOR METRIC MEASUREMENTS NSEQ IS THE NUMBER
!                        OF TIMES THAT SUBROUTINE ROBS IS CALLED
!                        (NOT COUNTING BOTH ENDS OF A COUNTING INTERVAL
!                        FOR AVERAGE RANGE RATE).
!               NPT    - NUMBER OF EVENT TIMES IN THE MEASUREMENT
!                        (OR AT ONE END OF THE COUNTING INTERVAL
!                        OF AN AVERAGE RANGE RATE MEASUREMENT)
!               XPT    - AN ARRAY WITH NPT ELEMENTS. EACH ELEMENT
!                        IS A NUMBER FROM 11 - 16 INCLUSIVE.
!                        11 SIGNIFIES STATION #1
!                        12 SIGNIFIES SATELLITE #1
!                        13 SIGNIFIES STATION #2
!                        14 SIGNIFIES SATELLITE #2
!                        15 SIGNIFIES STATION #3
!                        16 SIGNIFIES SATELLITE #3
!                        XPT(K) GIVES THE LOCATION OF THE KTH EVENT
!                        IN THE MEASUREMENT.
!
!EDOC*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBINRI/NSATB,NSTAB,INMSAT(3),INMSTA(3)
      COMMON/CBLOKI/MJDSBL,MTYPE ,NM    ,JSTATS,NPSEG ,JSATNO(3),ITSYS ,&
     &       NHEADB,NELEVS,ISTAEL(12),INDELV(3,4),JSTANO(3),ITARNO,     &
     &       KTARNO
      COMMON/ELRTNX/NELVS,NELVC,IELSTA(6),IELSAT(6),IELSA2(6),          &
     &       INDXEL(5,4)
!
      DIMENSION XPT(24)
      DIMENSION ICONFG(3,6)
      DATA SUB/9.99D0/
!
!  NUMBER OF EVENTS IN EACH SEQUENCE
!
      NEES=NPT/NSEQ
      NLINK=NEES-1
!
!  ZERO OUT THE ELEVATION INDEX ARRAYS FOR THE BLOCK
!
      NELVS=0
      NELVC=0
      DO 100 I=1,6
      ICONFG(1,I)=0
      ICONFG(2,I)=0
      ICONFG(3,I)=0
      IELSTA(I)=0
      IELSAT(I)=0
      IELSA2(I)=0
  100 END DO
      DO 200 I=1,5
      INDXEL(I,1)=0
      INDXEL(I,2)=0
      INDXEL(I,3)=0
      INDXEL(I,4)=0
  200 END DO
!
!  MEASURMENT SPECIFIC TESTS
      IF(MTYPE.EQ.47.OR.MTYPE.EQ.48) THEN
         NELVS=2
         INDXEL(1,1)=1
         IELSTA(1)=INMSTA(1)
         RETURN
      ENDIF
      IF(MTYPE.EQ.51.OR.MTYPE.EQ.52.OR.MTYPE.EQ.53.OR.MTYPE.EQ.54) THEN
         NELVS=2
         INDXEL(1,1)=1
         INDXEL(2,1)=1
         IELSAT(1)=INMSAT(1)
         IELSTA(1)=INMSTA(1)
         IELSAT(2)=INMSAT(1)
         IELSTA(2)=INMSTA(1)
        RETURN
      ENDIF
      IF(MTYPE.EQ.40) THEN
         NELVS=1
         INDXEL(2,1)=1
         IELSAT(1)=INMSAT(1)
         IELSTA(1)=INMSTA(1)
        RETURN
      ENDIF
      IF(MTYPE.EQ.43.OR.MTYPE.EQ.44) THEN
         NELVS=1
         INDXEL(2,1)=1
         IELSAT(1)=INMSAT(2)
         IELSA2(1)=INMSAT(1)
        RETURN
      ENDIF
      IF(MTYPE.EQ.57.OR.MTYPE.EQ.58) THEN
         NELVS=4
         INDXEL(1,1)=1
         INDXEL(2,1)=1
         INDXEL(3,1)=1
         INDXEL(4,1)=1
         IELSAT(4)=INMSAT(3)
         IELSTA(4)=INMSTA(2)
         IELSAT(3)=INMSAT(3)
         IELSA2(3)=INMSAT(1)
         IELSAT(2)=INMSAT(2)
         IELSA2(2)=INMSAT(1)
         IELSAT(1)=INMSAT(2)
         IELSTA(1)=INMSTA(1)
        RETURN
       ENDIF
      IF(MTYPE.EQ.55.OR.MTYPE.EQ.56) THEN
! TDRSS 1 WAY ALIASED AS 58
         NELVS=2
         INDXEL(1,1)=1
         INDXEL(2,1)=1
         IELSAT(2)=INMSAT(2)
         IELSA2(2)=INMSAT(1)
         IELSAT(1)=INMSAT(2)
         IELSTA(1)=INMSTA(1)
        RETURN
      ENDIF
        IF(MTYPE.EQ.59) THEN
         NELVS=4
         INDXEL(1,1)=1
         INDXEL(2,1)=1
         INDXEL(3,1)=1
         INDXEL(4,1)=1
         IELSAT(4)=INMSAT(1)
         IELSTA(4)=INMSTA(2)
         IELSAT(3)=INMSAT(1)
         IELSTA(3)=INMSTA(1)
         IELSAT(2)=INMSAT(2)
         IELSTA(2)=INMSTA(1)
         IELSAT(1)=INMSAT(2)
         IELSTA(1)=INMSTA(3)
        RETURN
      ENDIF
      IF(MTYPE.EQ.65.OR.MTYPE.EQ.66) THEN
         NELVS=2
         INDXEL(2,1)=1
         INDXEL(2,2)=1
         IELSAT(1)=INMSAT(2)
         IELSA2(1)=INMSAT(1)
         IELSAT(2)=INMSAT(3)
         IELSA2(2)=INMSAT(1)
        RETURN
      ENDIF
! code included by P. Visser for single differences
      IF(MTYPE.EQ.67.OR.MTYPE.EQ.68) THEN
         NELVS=2
         INDXEL(1,1)=1
         INDXEL(1,2)=1
         IELSAT(1)=INMSAT(1)
         IELSAT(2)=INMSAT(2)
         IELSTA(1)=INMSTA(1)
         IELSTA(2)=INMSTA(2)
         RETURN
      ENDIF
! end inclusion
      IF(MTYPE.EQ.85.OR.MTYPE.EQ.86) THEN
         NELVS=4
         INDXEL(1,1)=1
         INDXEL(1,2)=1
         INDXEL(2,3)=1
         INDXEL(2,4)=1
!
         IELSAT(1)=INMSAT(2)
         IELSTA(1)=INMSTA(1)
!
         IELSAT(2)=INMSAT(3)
         IELSTA(2)=INMSTA(1)
!
         IELSAT(3)=INMSAT(2)
         IELSA2(3)=INMSAT(1)
!
         IELSAT(4)=INMSAT(3)
         IELSA2(4)=INMSAT(1)
        RETURN
      ENDIF
      IF(MTYPE.EQ.87.OR.MTYPE.EQ.88) THEN
         NELVS=4
         INDXEL(1,1)=1
         INDXEL(1,2)=1
         INDXEL(1,3)=1
         INDXEL(1,4)=1
!
         IELSAT(1)=INMSAT(1)
         IELSTA(1)=INMSTA(1)
!
         IELSAT(2)=INMSAT(2)
         IELSTA(2)=INMSTA(1)
!
         IELSAT(3)=INMSAT(1)
         IELSTA(3)=INMSTA(2)
!
         IELSAT(4)=INMSAT(2)
         IELSTA(4)=INMSTA(2)
        RETURN
      ENDIF
!
!
!
      DO 2000 ISEQ=1,NSEQ
      KPT=(ISEQ-1)*NEES
      DO 1000 ILINK=1,NLINK
!
      IE1=XPT(KPT+ILINK)-SUB
      IF(MOD(IE1,2).EQ.0) THEN
         IE1=IE1/2
         LSAT1=.TRUE.
      ELSE
         LSAT1=.FALSE.
         IE1=IE1/2+1
      ENDIF
      IE2=XPT(KPT+ILINK+1)-SUB
      IF(MOD(IE2,2).EQ.0) THEN
         IE2=IE2/2
         LSAT2=.TRUE.
      ELSE
         LSAT2=.FALSE.
         IE2=IE2/2+1
      ENDIF
!
!     IF((LSAT1.AND.LSAT2).OR.(.NOT.LSAT1.AND..NOT.LSAT2)) GO TO 1000
      IF(.NOT.LSAT1.AND..NOT.LSAT2) GO TO 1000
      L2SAT=LSAT1.AND.LSAT2
      IF(L2SAT) GO TO 700
!
      IF(LSAT1) THEN
        ISTA=IE2
        ISAT=IE1
      ELSE
        ISTA=IE1
        ISAT=IE2
      ENDIF
      IF(NELVS.EQ.0) THEN
         NELVS=NELVS+1
         INDXEL(ILINK,ISEQ)=1
         IELSTA(NELVS)=INMSTA(ISTA)
         IELSAT(NELVS)=INMSAT(ISAT)
!        IF(LSAT1) IELSAT(NELVS)=-INMSAT(ISAT)
         ICONFG(1,NELVS)=ISTA
         ICONFG(2,NELVS)=ISAT
         GO TO 1000
      ELSE
         DO 500 I=1,NELVS
         IF(ICONFG(1,I).EQ.ISTA.AND.ICONFG(2,I).EQ.ISAT) GO TO 1000
  500    CONTINUE
         NELVS=NELVS+1
         INDXEL(ILINK,ISEQ)=1
         IELSTA(NELVS)=INMSTA(ISTA)
         IELSAT(NELVS)=INMSAT(ISAT)
!        IF(LSAT1) IELSAT(NELVS)=-INMSAT(ISAT)
         ICONFG(1,NELVS)=ISTA
         ICONFG(2,NELVS)=ISAT
         GO TO 1000
      ENDIF
!
! TWO SAT CASE
  700 CONTINUE
      ISAT=IE1
      ISAT2=IE2
      IF(NELVS.EQ.0) THEN
         NELVS=NELVS+1
         INDXEL(ILINK,ISEQ)=1
         IELSAT(NELVS)=INMSAT(ISAT)
         IELSA2(NELVS)=INMSAT(ISAT2)
         ICONFG(2,NELVS)=ISAT
         ICONFG(3,NELVS)=ISAT2
         GO TO 1000
      ELSE
         DO 900 I=1,NELVS
         IF(ICONFG(2,I).EQ.ISAT.AND.ICONFG(3,I).EQ.ISAT2) GO TO 1000
         IF(ICONFG(2,I).EQ.ISAT2.AND.ICONFG(3,I).EQ.ISAT) GO TO 1000
  900    CONTINUE
         NELVS=NELVS+1
         INDXEL(ILINK,ISEQ)=1
         IELSAT(NELVS)=INMSAT(ISAT)
         IELSA2(NELVS)=INMSAT(ISAT2)
         ICONFG(2,NELVS)=ISAT
         ICONFG(3,NELVS)=ISAT2
         GO TO 1000
      ENDIF
 1000 END DO
 2000 END DO
      RETURN
      END
