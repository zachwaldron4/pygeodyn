!$SOLVE
      SUBROUTINE SOLVE(ATPA,ATPL,JDIM,NRMTOT,DX)
!********1*********2*********3*********4*********5*********6*********7**
! SOLVE            00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  GIVEN AN INVERTED PORTION OF THE ENTIRE  TRIANGULAR
!            NORMAL MATRX AND A RHS OF THE NORMAL EQ CORRESPONDING
!            TO THAT PORTION OF THE NORMAL MATRIX,GET THE
!            VECTOR OF PARAMETER CORRECTIONS.SEE RESTRICTION
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ATPA     I    A    INVERTED PORTION OF THE NORMAL MATRX
!   ATPL     I    A    RHS OF NORMAL EQUATION CORRESPONDING
!                      TO ATPA
!   JDIM     I    S    ORDER OF THE SUBBLOCK
!   NRMTOT   I    S    LENGTH OF THE SUBBLOCK'S FIRST ROW IN THE ENTIR
!                      NORMAL MATRIX
!   DX       O    A    SOLVED PARAMETER CORRECTIONS
!
! COMMENTS:
!
!
! RESTRICTIONS:   ANY PORTION OF THE NORMAL MATRIX (AND CORRESPONDING
!                 RHS) NOT STARTING AT THE TOP MUST HAVE BEEN MODIFIED
!                 TO ACCOUNT FOR OFF DIAGONAL BLOCKS.THE PARAMETERS
!                 CORRESPONDING TO THE TOP PART ARE MODIFIED AT THE
!                 END OF AN OUTER ITERATION
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION ATPA(1),ATPL(1),DX(1)
      DATA ZERO/0.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
! ZERO OUT DX
      DO 10 I=1,JDIM
      DX(I)=ZERO
   10 END DO
! MULTIPLY THE PORTION OF THE MATRIX FROM THE 'MISSING' PART OF THE ROWS
      IF(JDIM.EQ.1) GO TO 110
      IPTL=1
      IL=JDIM-1
      DO 100 I=2,JDIM
      IPTR=I-1
      DO 50 J=1,IL
      DX(IPTR+J)=DX(IPTR+J)+ATPA(IPTL+J)*ATPL(IPTR)
   50 END DO
      IPTL=IPTL+NRMTOT-I+2
      IL=IL-1
  100 END DO
  110 CONTINUE
!
! MULTILY THE PORTION OF THE MATRIX FROM THE TRIANGULAR ROWS
      IPTL=0
      IL=JDIM
      DO 200 I=1,JDIM
      IPTR=I-1
      DO 150 J=1,IL
      DX(I)=DX(I)+ATPA(IPTL+J)*ATPL(IPTR+J)
  150 END DO
      IPTL=IPTL+NRMTOT-I+1
      IL=IL-1
  200 END DO
      RETURN
      END
