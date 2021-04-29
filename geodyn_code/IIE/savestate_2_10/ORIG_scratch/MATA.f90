!$MATA
      SUBROUTINE MATA(X,Y,Z)
!********1*********2*********3*********4*********5*********6*********7**
! MATA               93/06/22            9305.0    PGMR - D. ROWLANDS
!
!
! FUNCTION:    ADD 2 3X3 MATRICES TOGETHER
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X        I         MATRIX  NUMBER 1
!   Y        I         MATRIX  NUMBER 2
!   Z        O         SUM MATRIX  X+Y
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION X(9),Y(9),Z(9)
!
      DO 10 I=1,9
      Z(I)=X(I)+Y(I)
   10 END DO
!
      RETURN
      END
