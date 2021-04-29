!$CRAAGG
      SUBROUTINE CRAAGG(SUM1,NPARAM,NDIM,INNER)
!********1*********2*********3*********4*********5*********6*********7**
! CRAAGG           00/00/00            0000.0    PGMR - CORREL
!
! FUNCTION:  TO COMPUTE AND PRINT CORRELATION COEFFICIENTS FROM
!            THE DIAGONAL AND ABOVE THE DIAGONAL OF A NORMAL
!            MATRIX IN VECTOR FORM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SUM1     I         INVERTED LEAST SQUARES MATRIX IN VECTOR
!   (1)                FORM
!   NPARAM   I         ORDER OF LEAST SQUARES MATRIX
!   NDIM     I         FIRST DIMENSION OF SQUARE ARRAY EQUIVALENT
!                      TO 'SUM1' VECTOR
!   INNER    I         ITERATION NUMBER
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      CHARACTER*1 NUM, FMT, FMT2
      DIMENSION ITL(20)
      DIMENSION SUM1(1),FMT1(3),FMT21(3),XNUMB(2)
      DIMENSION NUM(11), FMT(23), FMT2(18)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      EQUIVALENCE (FMT1(1),FMT(1)), (FMT21(1),FMT2(1)),                 &
     &   (XNUMB(1),NUM(1))
                                                ! jjm 9/98
      CHARACTER(8)      :: FMT1, FMT21, XNUMB

      DATA FMT1/'(''0'',   ', 'X,  F6.3', ',1X,I6)'/
      !!DATA FMT1/8H(0,   , 8HX,  F6.3, 7H,1X,I6)/ ! original


      DATA FMT21/'(''0'',  F', '6.3,1X,I', '6)'/
      !!DATA FMT21/8H(0,  F, 8H6.3,1X,I, 2H6)/ ! original

      DATA XNUMB/'12345678', '90' /
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE CORRELATION COEFFICIENTS
      IF(NPARAM-1)20,10,30
   10 SUM1(1)=SQRT(ABS(SUM1(1)))
   20 RETURN
   30 DO 2534 I=1,NPARAM
      II=NDIM*(I-1)-((I-1)*I)/2+I
      SUM1(II)=SQRT(ABS(SUM1(II)))
      IF(I.EQ.NPARAM) GO TO 2532
      I1=I+1
      DO 2531 J=I1,NPARAM
      IJ=II+J-I
 2531 SUM1(IJ)=SUM1(IJ)/SUM1(II)
! DIVIDE OFF-DIAGONAL TERMS BY SQUARE ROOT OF ROW AND COLUMN DIAGONAL
! TERM
      IF(I.EQ.1) GO TO 2534
 2532 I1=I-1
      JI=I
      DO 2533 J=1,I1
      SUM1(JI)=SUM1(JI)/SUM1(II)
 2533 JI=JI+NDIM-J
 2534 END DO
! PRINT CORRELATION COEFFICIENTS
      K=1
      JSTRT=2
      ISTOP=NPARAM-1
      IF(NPARAM.GT.21)ISTOP=20
      ISTRT=1
      IS=ISTRT
      WRITE(IOUT6,10112) INNER
      LFRSTM=.TRUE.
 2535 IF(.NOT.LFRSTM) WRITE(IOUT6,10113)
      JSEND=JSTRT+19
      IF(JSEND.GT.NPARAM) JSEND=NPARAM
      NPRN=JSEND-JSTRT+1
      DO 35 I=1,NPRN
      ITL(I)=JSTRT+I-1
   35 END DO
      WRITE(IOUT6,10214) (ITL(I),I=1,NPRN)
      DO 25 I=1,ISTOP
      I1=NDIM*(I-1)-(I*(I-1))/2+I+IS
      ICOL1=ISTRT+I
      IF(K.GT.1)ICOL1=IS+I
      ICOL2=MIN(ISTRT+20,NPARAM)
      I2=I1+ICOL2-ICOL1
      IF(K.GT.1.AND.IS.GT.1)IS=IS-1
      IF(I.LE.20*(K-1)+1)GO TO 2538
      IF(I.EQ.1.AND.ISTOP.LT.20) GO TO 2538
      NX=(I-(20*(K-1)+1))*6
      M=0
      FMT(6)=NUM(11)
      FMT(7)=NUM(11)
      FMT(11)=NUM(11)
      IR100=NX/100
      IR10=(NX-(IR100*100))/10
      IR1=NX-(IR10*10+IR100*100)
      IF(IR100.NE.0)FMT(6)=NUM(IR100)
   40 IF(IR10.NE.0)FMT(7+M)=NUM(IR10)
      IF(IR10.EQ.0.AND.IR100.NE.0)FMT(7)=NUM(10)
      IF(IR1.EQ.0)GO TO 50
      FMT(8+M)=NUM(IR1)
      GO TO 60
   50 FMT(8+M)=NUM(10)
   60 IF(M.EQ.4)GO TO 2537
      M=4
      NF=I2-I1+1
      IR100=0
      IR10=NF/10
      IR1=NF-IR10*10
      GO TO 40
 2537 WRITE(IOUT6,FMT) (SUM1(IL),IL=I1,I2),I
      GO TO 25
 2538 NF=I2-I1+1
      FMT2(6)=NUM(11)
      IR10=NF/10
      IR1=NF-IR10*10
      IF(IR10.EQ.0) GO TO 70
      FMT2(6)=NUM(IR10)
   70 FMT2(7)=NUM(IR1)
      IF(IR1.EQ.0) FMT2(7)=NUM(10)
      WRITE(IOUT6,FMT2) (SUM1(J),J=I1,I2),I
   25 END DO
      JSTRT=JSTRT+20
      IF(JSTRT.GT.NPARAM) RETURN
      K=K+1
      ISTOP=20*K
      IF(NPARAM-20*(K-1).LT.21)ISTOP=NPARAM-1
      ISTRT=ISTRT+20
      IS=ISTRT
      LFRSTM=.FALSE.
      GO TO 2535
10112 FORMAT('0',6X,'CORRELATION COEFFICIENTS FOR ADJUSTED PARAMETERS', &
     &'AFTER ITERATION NUMBER',I3/)
10113 FORMAT('0',6X,'CORRELATION COEFFICIENTS FOR ADJUSTED ARC',  &
     &'PARAMETERS - CONTINUED'/)
10214 FORMAT('0',20I6)
      END
