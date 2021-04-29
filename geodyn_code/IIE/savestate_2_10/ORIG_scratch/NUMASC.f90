!$NUMASC
      SUBROUTINE NUMASC(INTNUM,OUTSTR,ILOC,LZERO)
!********1*********2*********3*********4*********5*********6*********7**
! NUMASC           03/26/85            0000.0    PGMR - BILL EDDY
!
!
! FUNCTION          CONVERTS INTEGER NUMBERS TO ALPHANUMERIC AND LOADS
!                   THEM IN THE DESIGNATED BYTES IN THE OUTPUT STRING
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INTNUM   I         INTEGER NUMBER TO BE CONVERTED TO
!                      ALPHANUMERIC REPRESENTATION
!   OUTSTR  I/O        OUTPUT STRING TO WHICH THE CONVERTED INTNUM
!                      WILL BE ADDED IN THE LOCATIONS INDICATED BY
!                      ILOC. THE NUMBER WILL BE RIGHT JUSTIFIED.(INPUT)
!                      /OUTPUT STRING (OUTPUT)
!   ILOC     I         TWO DIGIT INTEGER NUMBER USED TO INDICATE
!                      BEGINNING AND ENDING LOCATIONS IN OUTPUT
!                      STRING AT WHICH THE ALPHANUMERIC REPRESENT-
!                      ATION FOR INTNUM WILL BE PLACED. IF THE
!                      NUMBER PASSED IN REQUIRES MORE LOCATIONS
!                      THAN SPECIFIED BY ILOC AN ERROR OCCURS. IF
!                      THE NUMBER PASSED IN REQUIRES FEWER
!                      LOCATIONS THAN SPECIFIED THE REMAINING
!                      SPACES ARE 0 OR BLANK FILLED DEPENDING ON
!                      HOW LZERO IS SET.
!   LZERO    I         LOGICAL FLAG THAT CONTROLS WHETHER BLANKS
!                      OR ZEROES ARE USED WHEN ILOC SPECIFIES
!                      MORE LOCATIONS THAN INTNUM REQUIRES
!                      T - FILL WITH ZEROS; F - FILL WITH BLANKS
!
! EXAMPLE:
!
!        GIVEN OUTSTR AS    BILL01XX
!          AND INTNUM AS     30
!          AND ILOC   AS     57
!             LZERO   T                        LZERO  F
!                           12345678           12345678
!   THEN ON OUTPUT OUTSTR = BILL030X         = BILL 30X
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE

                                        ! jjm 9/98
        PARAMETER( IUNT90 = 90 )

      CHARACTER*1 DIGIT,OUTSTR,CHAR
!
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
      DIMENSION DIGIT(11),OUTSTR(1)
      DIMENSION ICH(16)
!
      DATA DIGIT/'0','1','2','3','4','5','6','7','8','9',' '/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
      NUMM  =INTNUM
! DETERMINE NUMBER OF OUTPUT CHARACTER FIELDS INDICATED BY ILOC
      IF1   =ILOC/10
      IF2   =ILOC-IF1*10
      NCHAR =IF2-IF1+1
! DETERMINE NUMBER OF DIGITS IN INTNUM
      NDIGIT=1
      ITEN  =10
      DO 100 I=1,9
      IF(INTNUM.LT.ITEN) GO TO 150
      NDIGIT=NDIGIT+1
      ITEN=ITEN*10
  100 END DO
! TEST IF THE NO. OF DIGITS WILL FIT IN THE NO. OF CHARACTERS SPECIFIED
  150 IF(NDIGIT.GT.NCHAR) GO TO 9999
! INITIALIZE FIELDS WITH 0 OR BLANK (ICH SET = 0 OR 10)
      ICHAR =10
      IF(LZERO) ICHAR  =0
      DO 200 I=1,NCHAR
      ICH(I) = ICHAR
  200 END DO
! BREAK INTEGER NUMBER INTO SEPARATE DIGITS
      DO 300 I=1,NDIGIT
      ICH(I)=MOD(NUMM,10)
      NUMM  =NUMM/10
  300 END DO
! LOAD OUTPUT STRING
      IF2   =IF2+1
      DO 400 K=1,NCHAR
      CHAR  =DIGIT(ICH(K)+1)
      OUTSTR(IF2-K)=CHAR
  400 END DO
      RETURN
 9999 WRITE(IUNT90,80000) INTNUM,ILOC,NDIGIT,NCHAR
      RETURN
!********1*********2*********3*********4*********5*********6*********7**
! FORMATS
!********1*********2*********3*********4*********5*********6*********7**
80000 FORMAT(1X,'SUBROUTINE NUMASC - INPUT FIELD SIZE TO SMALL FOR',&
     &'NUMBER ',2I10,2I5 )
      END
