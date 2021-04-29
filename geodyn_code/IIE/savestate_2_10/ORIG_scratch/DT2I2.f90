!$DT2I2
      SUBROUTINE DT2I2(STR,I1,I2,LPROB)
!********1*********2*********3*********4*********5*********6*********7**
!  DT2I2           00/00/00         0000.0      PGMR - ?
!
! FUNCTION: CONVERTS INPUT DATE/TIME CHARATER STRING TO TWO INTERGER
!           VALUES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  STR                 INPUT CHARACTER STRING
!  I1                  CONVERTED INTERGER DATE
!  I2                  CONVERTED INTEGER TIME
!  LPROB               FLAG IF THERE WAS A PROBLEM CONVERTING THE STRING
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      CHARACTER*15 STR
      CHARACTER*9 DGT
      DATA DGT/'123456789'/
!****************************************************
! START OF EXECUTABLE CODE
!****************************************************
!
      LPROB=.FALSE.
!
! FIND the DECIMAL POINT IN THE TIME FIELD
!
      DO I=1,15
       IF(STR(I:I).EQ.'.') THEN
        ID=I
        GOTO 100
       ENDIF
      ENDDO

      LPROB=.TRUE.
      RETURN
!
! FIND YY START IN THE TIME FIELD
!
  100 CONTINUE

!...TEST TO SEE ALL DIGITS ARE AVAILABLE
      IF(ID.LT.13.OR.ID.GT.15) THEN
       LPROB=.TRUE.
       RETURN
      ENDIF
!...YY START
      IS=ID-12

!
! COMPUTE INTEGER REPRESENTATION OF YYMMDD
!
      IE=IS+5
      MULT=100000
      I1=0
      DO 300 I=IS,IE
      IF(STR(I:I).EQ.' ') GO TO 290
      IF(STR(I:I).EQ.'0') GO TO 290
      DO 200 J=1,9
      IF(STR(I:I).EQ.DGT(J:J)) GO TO 210
  200 END DO
      LPROB=.TRUE.
      RETURN
  210 CONTINUE
      I1=I1+MULT*J
  290 CONTINUE
      MULT=MULT/10
  300 END DO

!
! COMPUTE INTEGER REPRESENTATION OF HHMMSS
!
      IS=IE+1
      IE=IS+5
      MULT=100000
      I2=0
      DO 500 I=IS,IE
      IF(STR(I:I).EQ.' ') GO TO 490
      IF(STR(I:I).EQ.'0') GO TO 490
      DO 400 J=1,9
      IF(STR(I:I).EQ.DGT(J:J)) GO TO 410
  400 END DO
      LPROB=.TRUE.
      RETURN
  410 CONTINUE
      I2=I2+MULT*J
  490 CONTINUE
      MULT=MULT/10
  500 END DO
      RETURN
      END
