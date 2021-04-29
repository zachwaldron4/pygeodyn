!$BBSORT
      SUBROUTINE BBSORT(INDEX,ISORT,NLEN)
!********1*********2*********3*********4*********5*********6*********7**
! BBSOR2           83/10/28            0000.0    PGMR - D.SEVITSKI
!                  85/12/09            0000.0    PGMR - TOM MARTIN
!
! FUNCTION:  SORT ASCENDING, MAINTAINING INITIAL ORDER
!            WHENEVER POSSIBLE.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   INDEX    O         INDEX TO SORTED ORDER OF ISORT
!   ISORT    I         INTEGER ARRAY CONTAINING
!                      INFO TO BE SORTED
!   NLEN     I         LENGTH OF ARRAY TO BE SORTED
!
! COMMENTS:       INDEX(1) POINTS TO FIRST
!                 INDEX(2) POINTS TO SECOND
!                 NO LONGER PERFORMS BUBBLE TYPE SORT.  TVM
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      DIMENSION ISORT(1),INDEX(1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      IF(NLEN.GT.1) GOTO 350
      IF(NLEN.EQ.0) RETURN
! NULL SORT
      INDEX(1)=1
      RETURN
  350 CONTINUE
! INITIALIZE SORT INDICES
      DO 400 I=1,NLEN
      INDEX(I)=I
  400 END DO
! LOOP TO LOCATE SMALLEST ELEMENT FIRST, THEN NEXT SMALLEST, ETC.
      NS1=NLEN-1
      DO 600 I=1,NS1
! FIRST GUESS IS THAT CURRENT ELEMENT IS SMALLEST
      IX=INDEX(I)
      JJ=I
! LOOP THRU REMAINING ELEMENTS TO FIND SMALLEST
      I1=I+1
      DO 500 J=I1,NLEN
      JX=INDEX(J)
      IF(ISORT(JX).GE.ISORT(IX)) GOTO 500
! SMALLER ELEMENT IS FOUND. REDEFINE INDEX.
      IX=JX
      JJ=J
  500 END DO
! JUMP TO 600 IF FIRST GUESS WAS CORRECT
      IF(JJ.EQ.I) GO TO 600
! SHIFT INDICES TO CLOSE HOLE WHERE SMALLEST ELEMENT LOCATED
      J2=JJ
      DO 550 J=I1,J2
      J1=JJ-1
      INDEX(JJ)=INDEX(J1)
      JJ=J1
  550 END DO
! LOAD SMALLEST ELEMENT INDEX
      INDEX(I)=IX
  600 END DO
      RETURN
      END
