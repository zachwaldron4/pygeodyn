!$A3TOI6
      SUBROUTINE A3TOI6(IA3,I6,I3)
!********1*********2*********3*********4*********5*********6*********7**
! A3TOI6           86/03/21            8603.0    PGMR - TOM MARTIN
!
!
! FUNCTION: CONVERT A 3A1 INPUT FIELD INTO A BASE 100 INTEGER
!           AND A BASE 10 INTEGER.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IA3      I         AN INTEGER ARRAY DIMENSIONED BY THREE AND
!                      FILLED BY READING WITH A 3A1 FORMAT.
!   I6       O         A BASE 100 INTEGER CONSTRUCTED FROM THE
!                      3A1 FIELDS ASSUMING THAT THE LETTERS OF THE
!                      ALPHABET ARE AN EXTENSION OF THE DECIMAL
!                      NUMERALS BEYOND 9.
!   I3       O         A BASE 10 INTEGER CONSTRUCTED FROM THE
!                      3A1 FIELDS ASSUMING THAT ALL CHARACTERS
!                      EXCEPT DECIMAL NUMERALS AND MINUS ARE ZERO.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      CHARACTER(1)        :: IA3, IALPHA, KCHAR
      DIMENSION IA3(3),IALPHA(35)
      DATA IALPHA/'1','2','3','4','5','6','7','8','9',                  &
     &            'A','B','C','D','E','F','G','H','I',                  &
     &            'J','K','L','M','N','O','P','Q','R',                  &
     &            'S','T','U','V','W','X','Y','Z'/
      CHARACTER(1)        :: MINUS
      DATA MINUS/'-'/
      DATA NDIGIT/9/,NALPHA/10/,MALPHA/35/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      LNEG=.FALSE.
      I3=0
      I6=0
      DO 4000 J=1,3
      I3=I3*10
      I6=I6*100
      KCHAR=IA3(J)
      IF(KCHAR.NE.MINUS) GO TO 1000
      LNEG=.TRUE.
      GO TO 4000
 1000 CONTINUE
      DO 2000 I=1,NDIGIT
      IF(KCHAR.NE.IALPHA(I)) GO TO 2000
      I3=I3+I
      I6=I6+I
      GO TO 4000
 2000 END DO
      DO 3000 I=NALPHA,MALPHA
      IF(KCHAR.NE.IALPHA(I)) GO TO 3000
      I6=I6+I
 3000 END DO
 4000 END DO
      IF(.NOT.LNEG) RETURN
      I3=-I3
      I6=-I6
      RETURN
      END
