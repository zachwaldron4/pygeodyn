!$THVMAT
      SUBROUTINE THVMAT( A, X, ROTMAT, VMAT )
!********1*********2*********3*********4*********5*********6*********7**
! THVMAT           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A
!   X
!   ROTMAT
!   VMAT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DATA LDEBUG/.FALSE./
      DIMENSION A(3), X(6), VMAT(3,6), ROTPAR(3,3,6), ROTMAT(3,3)
      DATA ZERO /0.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!-----------------------------------------------------------------------
!
!     ....COMPUTE ROTPAR = PARTIAL DERIVATIVES OF THE HCL
!     ....TO ECI XYZ ROTATION MATRIX WRT X
!     ....ROTPAR(I,K,J) = D( ROTMAT(I,K) ) / DX(J)
!
      CALL HCLPAR( X, ROTMAT, ROTPAR )
!
      DO 50 J=1,6
      IF(LDEBUG)   WRITE(6,4001) ((ROTPAR(I,K,J),I=1,3),K=1,3)
 4001 FORMAT(' THVMAT: ROTPAR '/(1X,3D24.16))
   50 END DO
!
!-----------------------------------------------------------------------
!
!     ....MULTIPLY ROTPAR BY THE HCL ACCELERATION TO GET
!     ....THE PARTIALS OF THE THRUST ACCELERATION IN THE
!     ....TRUE OF REFERENCE ECI FRAME
!
      DO 10 J=1,6
      DO 10 I=1,3
!     ....TAKE OUT 2/19/88 ==========================
!CC   VMAT(I,J) = ZERO
!     ....TAKE OUT 2/19/88 ==========================
      DO 20 K=1,3
      VMAT(I,J) = VMAT(I,J) + ROTPAR(I,K,J) * A(K)
   20 END DO
   10 CONTINUE
!
      IF(LDEBUG)   WRITE(6,4000) ((VMAT(I,J),I=1,3),J=1,6)
 4000 FORMAT(' THVMAT: VMAT '/(1X,3D24.16))
      RETURN
      END
