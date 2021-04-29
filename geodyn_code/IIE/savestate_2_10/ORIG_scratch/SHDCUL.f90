      SUBROUTINE SHDCUL(ISAT,NTIMES,AA,IGAP,NP,LOK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      CHARACTER*96 CARD,CARD2
      CHARACTER*1 ENEX
      DIMENSION AA(12,1),A(12),B(12),IGAP(3,1)
      EQUIVALENCE (A(1),CARD)
      EQUIVALENCE (B(1),CARD2)
!
      NP=0
      ITMP=-99
      LAST=.FALSE.
!
      DO 500 I=1,NTIMES
      IF(I.EQ.NTIMES) LAST=.TRUE.
      DO 10 J=1,12
      A(J)=AA(J,I)
   10 END DO
      IF(I.EQ.1.AND.LAST) THEN
        NP=1
        IGAP(1,NP)=1
        IGAP(2,NP)=1
        IGAP(3,NP)=1
        CARD(9:9)='?'
        DO 15 J=1,12
        AA(J,I)=A(J)
   15   CONTINUE
        GO TO 500
      ENDIF
      READ(CARD(10:19),7000) ITM
      IDIF=ITM-ITMP
      IF(IDIF.GT.11.OR.LAST) THEN
        IF(NP.GT.0) THEN
          IGAP(3,NP)=I-1
          IF(LAST.AND.IDIF.LE.11) IGAP(3,NP)=I
          NTX=IGAP(3,NP)-IGAP(1,NP)+1
          IF(NTX.GT.1) THEN
             DO 20 J=1,12
             B(J)=AA(J,IGAP(1,NP))
   20        CONTINUE
             READ(CARD2(33:41),7001) RATIO1
             DO 25 J=1,12
             B(J)=AA(J,IGAP(1,NP)+1)
   25        CONTINUE
             READ(CARD2(33:41),7001) RATIO2
             ENEX='E'
             LENT=.TRUE.
             IF(RATIO2.GT.RATIO1) LENT=.FALSE.
             IF(RATIO2.GT.RATIO1) ENEX='X'
             RATIO5=RATIO1
             D5=ABS(RATIO5-.05D0)
             I5=IGAP(1,NP)
             II1=IGAP(1,NP)
             II2=IGAP(3,NP)
             RATIOP=RATIO1
             LPROB=.FALSE.
!
             DO 50 II=II1,II2
             DO 30 J=1,12
             B(J)=AA(J,II)
   30        CONTINUE
             READ(CARD2(33:41),7001) RATIOX
             IF(.NOT.LPROB) THEN
             IF((LENT.AND.RATIOX.GT.RATIOP).OR.                         &
     &                   (.NOT.LENT.AND.RATIOX.LT.RATIOP)) THEN
                  LPROB=.TRUE.
                  ENEX='I'
              ENDIF
             ENDIF
             DX=ABS(RATIOX-.05D0)
             CARD2(9:9)=ENEX
             DO 35 J=1,12
             AA(J,II)=B(J)
   35        CONTINUE
               IF(DX.LT.D5) THEN
               I5=II
               D5=DX
               RATIO5=RATIOX
             ENDIF
             RATIOP=RATIOX
   50        CONTINUE
             IGAP(2,NP)=I5
             DO 55 J=1,12
             B(J)=AA(J,IGAP(2,NP))
   55        CONTINUE
             CARD2(9:9)=ENEX
             DO 60 J=1,12
             AA(J,IGAP(2,NP))=B(J)
   60        CONTINUE
           ELSE
             IGAP(2,NP)=IGAP(1,NP)
             DO 65 J=1,12
             B(J)=AA(J,IGAP(2,NP))
   65        CONTINUE
             CARD2(9:9)='?'
             DO 70 J=1,12
             AA(J,IGAP(2,NP))=B(J)
   70        CONTINUE
          ENDIF
        ENDIF
        IF(LAST.AND.IDIF.GT.11) THEN
          NP=NP+1
          IGAP(1,NP)=1
          IGAP(2,NP)=1
          IGAP(3,NP)=1
          CARD(9:9)='?'
          DO 75 J=1,12
          AA(J,I)=A(J)
   75     CONTINUE
        GO TO 500
        ENDIF
        IF(.NOT.LAST) THEN
           NP=NP+1
           IGAP(1,NP)=I
        ENDIF
      ENDIF
      ITMP=ITM
  500 END DO
      LFIND=.FALSE.
      LSORT=.FALSE.
!
!
!
      DO 600 I=1,NP
      ITST=MOD(I,2)
      LODD=.FALSE.
      IF(ITST.EQ.1) LODD=.TRUE.
!
      II=IGAP(2,I)
      DO 510 J=1,12
      A(J)=AA(J,II)
  510 END DO
!
      IF(CARD(9:9).EQ.'E'.OR.CARD(9:9).EQ.'X') THEN
        LFIND=.TRUE.
        IF(LODD.AND.CARD(9:9).EQ.'E') LE1=.TRUE.
        IF(.NOT.LODD.AND.CARD(9:9).EQ.'E') LE1=.FALSE.
        IF(LODD.AND.CARD(9:9).EQ.'X') LE1=.FALSE.
        IF(.NOT.LODD.AND.CARD(9:9).EQ.'E') LE1=.TRUE.
        GO TO 610
      ENDIF
  600 END DO
  610 CONTINUE
      IF(.NOT.LFIND) THEN
       WRITE(6,6000)
       WRITE(6,6000)
       WRITE(6,6001) ISAT
       WRITE(6,6002)
       WRITE(6,6003)
       WRITE(6,6000)
       WRITE(6,6000)
        GO TO 800
      ENDIF
!
      DO 700 I=1,NP
      ITST=MOD(I,2)
      LODD=.FALSE.
      IF(ITST.EQ.1) LODD=.TRUE.
!
      II=IGAP(2,I)
      DO 620 J=1,12
      A(J)=AA(J,II)
  620 END DO
      ENEX=CARD(9:9)
      IF(ENEX.EQ.'I') GO TO 710
      IF(LODD.AND.LE1.AND.ENEX.EQ.'X') GO TO 710
      IF(LODD.AND..NOT.LE1.AND.ENEX.EQ.'E') GO TO 710
      IF(.NOT.LODD.AND.LE1.AND.ENEX.EQ.'E') GO TO 710
      IF(.NOT.LODD.AND..NOT.LE1.AND.ENEX.EQ.'X') GO TO 710
      IF(ENEX.EQ.'?') THEN
        IF(LODD.AND.LE1) CARD(9:9)='E'
        IF(LODD.AND..NOT.LE1) CARD(9:9)='X'
        IF(.NOT.LODD.AND.LE1) CARD(9:9)='X'
        IF(.NOT.LODD.AND..NOT.LE1) CARD(9:9)='E'
        DO 630 J=1,12
        AA(J,II)=A(J)
  630   CONTINUE
      ENDIF
  700 END DO
      LSORT=.TRUE.
  710 CONTINUE
      IF(.NOT.LSORT) THEN
       WRITE(6,6000)
       WRITE(6,6000)
       WRITE(6,6001) ISAT
       WRITE(6,6002)
       WRITE(6,6000)
       WRITE(6,6000)
        GO TO 800
      ENDIF
  800 CONTINUE
      LOK=.TRUE.
      IF(.NOT.LFIND) LOK=.FALSE.
      IF(.NOT.LSORT) LOK=.FALSE.
      DO 900 I=1,NP
      II=IGAP(2,I)
      DO 820 J=1,12
      A(J)=AA(J,II)
  820 END DO
      IF(LOK) THEN
        WRITE(44,7002) CARD
      ELSE
        WRITE(45,7002) CARD
      ENDIF
  900 END DO
      RETURN
 6000 FORMAT(' ')
 6001 FORMAT(' WARNING CHECK UNIT 44 FOR SATELLITE ',I10)
 6002 FORMAT(' ENTRY EXIT TIMES CAN NOT BE SORTED OUT PROPERLY.')
 6003 FORMAT(' BECAUSE CAN NOT IDENTIFY ENTRY OR EXIT')
 7000 FORMAT(I10)
 7001 FORMAT(F9.5)
 7002 FORMAT(A96)
      END
