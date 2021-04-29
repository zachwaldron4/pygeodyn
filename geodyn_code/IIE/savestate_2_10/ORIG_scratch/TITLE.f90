!$TITLE
      SUBROUTINE TITLE(IUNIT)
!********1*********2*********3*********4*********5*********6*********7**
! TITLE            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: WRITES OUT HEADER INFORMATION AND PAGE NUMBERS FOR THE
!           GIVEN INPUT UNIT.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNIT              INPUT UNIT NUMBER TO WRITE HEADER INFO TO
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      CHARACTER(1) NAME,LINE,BLANK,DIGITS,CHAR

      DIMENSION NAME(6,3),NSTRCT(7,6,3),LINE(126),DIGITS(10),           &
     &   NUMBRS(7,10),IND(6)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DATA BLANK/' '/
      DATA NAME/'G','E','O','D','Y','N',                                &
     &          ' ','I','I','-','E',' ',                                &
     &          'U','N','I','T','0','0'/
      DATA NSTRCT/                                                      &
     &   0, 62, 65, 65, 81,113,  0,                                     &
     &   0,127, 73, 73, 73, 65,  0,                                     &
     &   0, 62, 65, 65, 65, 62,  0,                                     &
     &   0,127, 65, 65, 65, 62,  0,                                     &
     &   0,  3,  4,120,  4,  3,  0,                                     &
     &   0,127,  4,  8, 16,127,  0,                                     &
     &   0,  0,  0,  0,  0,  0,  0,                                     &
     &   0,  0, 65,127, 65,  0,  0,                                     &
     &   0,  0, 65,127, 65,  0,  0,                                     &
     &   0,  8,  8,  8,  8,  8,  0,                                     &
     &   0,127, 73, 73, 73, 73,  0,                                     &
     &   0,  0,  0,  0,  0,  0,  0,                                     &
     &   0, 63, 64, 64, 64, 63,  0,                                     &
     &   0,127,  4,  8, 16,127,  0,                                     &
     &   0,  0, 65,127, 65,  0,  0,                                     &
     &   0,  1,  1,127,  1,  1,  0,                                     &
     &   0, 62, 81, 73, 69, 62,  0,                                     &
     &   0, 62, 81, 73, 69, 62,  0/
! G
! E
! O
! D
! Y
! N
! BLANK
! I
! I
! -
! S
!    B   0, 38, 73, 73, 73, 50,  0,
! E
! BLANK
! U
! N
! I
! T
! 0
! 0
!
      DATA NUMBRS/                                                      &
     &   0, 62, 81, 73, 69, 62,  0,                                     &
     &   0,  0, 66,127, 64,  0,  0,                                     &
     &   0, 67, 97, 81, 73, 71,  0,                                     &
     &   0, 34, 65, 73, 73, 54,  0,                                     &
     &   0, 16, 24, 20, 18,127,  0,                                     &
     &   0, 47, 73, 73, 73, 49,  0,                                     &
     &   0, 60, 74, 73, 73, 49,  0,                                     &
     &   0, 97, 17,  9,  5,  3,  0,                                     &
     &   0, 54, 73, 73, 73, 54,  0,                                     &
     &   0, 70, 73, 73, 41, 30,  0/
! 0
! 1
! 2
! 3
! 4
! 5
! 6
! 7
! 8
! 9
      DATA NLINE /126/,MWIDE /  3/,MHIGH /  3/,MLINES/  3/,MCHAR /  6/, &
     &     MATRIX/  7/,MBEGIN/  1/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      WRITE(IUNIT,10000)
      IND(5)=MOD(IUNIT/10,10)+1
      IND(6)=MOD(IUNIT,10)+1
      NWIDE=MWIDE
      NHIGH=MHIGH
      NBEGIN=MBEGIN
      DO 8000 N=1,MLINES
      IF(N.NE.2) GO TO 500
      NWIDE=2
      NHIGH=2
      NBEGIN=22
  500 CONTINUE
      IDIV=1
      DO 7000 M=1,MATRIX
      DO 1000 J=1,NLINE
      LINE(J)=BLANK
 1000 END DO
      I1=NBEGIN
      DO 5000 K=1,MCHAR
      CHAR=NAME(K,N)
      IF(N.EQ.3.AND.K.GT.4) CHAR=DIGITS(IND(K))
      DO 4000 J=1,MATRIX
      JMOD=NSTRCT(J,K,N)
      IF(N.EQ.3.AND.K.GT.4) JMOD=NUMBRS(J,IND(K))
      JMOD=JMOD/IDIV
      LOG=MOD(JMOD,2).EQ.1
      IF(.NOT.LOG) GO TO 3000
      I2=I1+NWIDE-1
      DO 2000 I=I1,I2
      LINE(I)=CHAR
 2000 END DO
 3000 CONTINUE
      I1=I1+NWIDE
 4000 END DO
 5000 END DO
      DO 6000 I=1,NHIGH
      WRITE(IUNIT,20000) LINE
 6000 END DO
      IDIV=IDIV*2
 7000 END DO
      WRITE(IUNIT,30000)
 8000 END DO
      RETURN
10000 FORMAT('1'/1X )
20000 FORMAT(6X,126A1)
30000 FORMAT('0'/1X )
      END
