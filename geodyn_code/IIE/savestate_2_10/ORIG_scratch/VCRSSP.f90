!$VCRSSP
      SUBROUTINE VCRSSP(A,B,C)
!********1*********2*********3*********4*********5*********6*********7**
! VCRSSP            12/01/92            0000.0    PGMR - S.LUO
!
!
! FUNCTION: PERFORM VECTOR CROSS PRODUCT:  AXB=C
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I    A    A VECTOR IN 3 DIMENSION
!   B        I    A    B VECTOR IN 3 DIMENTION
!   C        O    A    C VECTOR , THE PRODUCT OF AXB
!                      THE DIRECTION OF C IS .........
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
          IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION A(3), B(3), C(3)
      DATA ZERO/0.D0/
      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = A(3)*B(1) - A(1)*B(3)
      C(3) = A(1)*B(2) - A(2)*B(3)
      SQC = C(1)*C(1) + C(2)*C(2) +C(3)*C(3)
      ABC = SQRT(SQC)
      DO 10 I = 1,3
      C(I) = C(I)/ABC
   10 END DO
!
! ... DEBUG OUTPUT...
!     WRITE(6,100) A,B,C
  100 FORMAT(1X, 'VCRSSP : A =', 3F7.3/                                 &
     &       9X, 'B =', 3F7.3/                                          &
     &       9X, 'C =', 3F7.3)
      RETURN
      END
