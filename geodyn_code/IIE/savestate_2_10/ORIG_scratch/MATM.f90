!$MATM
      SUBROUTINE MATM(X,Y,Z)
!********1*********2*********3*********4*********5*********6*********7**
! MATM               93/06/22            9305.0    PGMR - D. ROWLANDS
!
!
! FUNCTION:    MULTIPLY 2 3X3 MATRICES TOGETHER
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X        I         MATRIX  NUMBER 1
!   Y        I         MATRIX  NUMBER 2
!   Z        O         PRODUCT MATRIX  X*Y
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION X(9),Y(9),Z(9)
!
      Z(1)=X(1)*Y(1)+X(4)*Y(2)+X(7)*Y(3)
      Z(2)=X(2)*Y(1)+X(5)*Y(2)+X(8)*Y(3)
      Z(3)=X(3)*Y(1)+X(6)*Y(2)+X(9)*Y(3)
!
      Z(4)=X(1)*Y(4)+X(4)*Y(5)+X(7)*Y(6)
      Z(5)=X(2)*Y(4)+X(5)*Y(5)+X(8)*Y(6)
      Z(6)=X(3)*Y(4)+X(6)*Y(5)+X(9)*Y(6)
!
      Z(7)=X(1)*Y(7)+X(4)*Y(8)+X(7)*Y(9)
      Z(8)=X(2)*Y(7)+X(5)*Y(8)+X(8)*Y(9)
      Z(9)=X(3)*Y(7)+X(6)*Y(8)+X(9)*Y(9)
!
      RETURN
      END
