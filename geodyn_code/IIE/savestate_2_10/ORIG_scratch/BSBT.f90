!$BSBT
      SUBROUTINE BSBT(B,S,R,N,M)
!********1*********2*********3*********4*********5*********6*********7**
! BSBT             00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   B
!   S
!   R
!   N
!   M
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION B(N,M),R(N,N),S(M)
      DATA ZERO/0.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 10 I=1,N
      DO 10 J=I,N
      R(I,J)=ZERO
      DO 5 K=1,M
      IF((B(I,K).EQ.ZERO).OR.(B(J,K).EQ.ZERO)) GO TO 5
      R(I,J)=R(I,J)+B(I,K)*B(J,K)*S(K)
    5 END DO
   10 R(J,I)=R(I,J)
      RETURN
      END
