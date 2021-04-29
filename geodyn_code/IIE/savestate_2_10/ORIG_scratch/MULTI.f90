!$MULTI
      SUBROUTINE MULTI(A,B,C,N,M,R)
!********1*********2*********3*********4*********5*********6*********7**
! MULTI            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: PERFORM MATRIX MULTIPLICATION:  A*B=C
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I    A    LEFT HAND MATRIX OF THE PRODUCT OF DIM NXM
!   B        I    A    RIGHT HAND MATRIX OF THE PRODUCT OF DIM MXR
!   C        O    A    THE PRODUCT OF A*B, OF DIM NXR
!   N        I    S    ROW DIMENSION OF MATRICES A AND C
!   M        I    S    COLUMN DIMENSION OF A AND ROW DIMENSION OF B
!   R        I    S    COLUMN DIMENSION OF MATRICES B AND C
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
          IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      INTEGER R
      DIMENSION A(N,M), B(M,R), C(N,R)
      DATA ZERO/0.D0/
      DO 10 I=1,N
      DO 10 J=1,R
      C(I,J)=ZERO
      DO 10 K=1,M
      C(I,J)=C(I,J)+A(I,K)*B(K,J)
   10 CONTINUE
! ... DEBUG OUTPUT...
!CC   WRITE(6,100) A,B,C
  100 FORMAT(1X, 'A =', 9F7.3/                                          &
     &       1X, 'B =', 9F7.3/                                          &
     &       1X, 'C =', 9F7.3)
      RETURN
      END
