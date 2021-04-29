!$HCLPAR
      SUBROUTINE HCLPAR( X, ROTMAT, ROTPAR )
!********1*********2*********3*********4*********5*********6*********7**
! HCLPAR           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: COMPUTES THE PARTIALS OF THE HCL TO XYZ ROTATION MATRIX WITH
!           RESPECT TO X AND V.  THE PARTIALS ARE STORED IN ROTPAR AND
!           ARE USED IN THE CALCULATION OF THE VARIATIONAL PARTIALS OF
!           THE THRUST.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   X        I    A    SATELLITE POSITION AND VELOCITY VECTOR
!   ROTMAT   I    A    XYZ ROTATION MATRIX
!   ROTPAR   O    A    PARTIALS
!
! COMMENTS:  THE CALCULATIONS HAVE BEEN MODIFIED TO USE THE GEODYN
!            DEFINITION OF HCL (SEE UVCOMP)
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!-------------------------------------------------------------
!
!     ....THIS SUBROUTINE COMPUTES THE PARTIALS OF
!     ....THE HCL TO XYZ ROTATION MATRIX WITH RESPECT
!     ....TO X AND V.
!
!     ....THE CALCULATIONS HAVE BEEN MODIFIED TO USE THE
!     ....GEODYN DEFINITION OF HCL (SEE UVCOMP)
!
!     ....THE PARTIALS ARE STORED IN ROTPAR AND ARE USED
!     ....IN THE CALCULATION OF THE VARIATIONAL PARTIALS
!     ....OF THE THRUST
!
!     ....X = SATELLITE POSITION AND VELOCITY VECTOR
!     ....ROTPAR(I,J,K) = D( ROTMAT(I,J) ) / DX(K)
!
!-------------------------------------------------------------
!
      DIMENSION X(6), ROTPAR(3,3,6), ROTMAT(3,3),                       &
     &   TEMP(3)
      DATA ZERO/0.D0/, ONE/1.D0/, TWO/2.D0/
!
!-------------------------------------------------------------
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!CC      IF(IDBG .GT.0)WRITE(6,5000)  X
!CC5000  FORMAT(' HCLPAR: X'/1X,6D20.10)
!
      DO 10 K=1,6
      DO 10 J=1,3
      DO 10 I=1,3
      ROTPAR(I,J,K) = ZERO
   10 CONTINUE
!
      R2 = X(1)**2 + X(2)**2 + X(3)**2
      R = SQRT(R2)
!
      V2 = X(4)**2 + X(5)**2 + X(6)**2
      V = SQRT(V2)
!CC      IF(IDBG.GT.0)WRITE(6,5001)  R, R2, V,V2
!CC5001  FORMAT(' HCLPAR: R,R2,V,V2 ',4D20.10)
!
!--------------------------------------------------------------
!
!      ....COLUMN 1
!
!      ....X PARTIALS ONLY  --  XDOT PARTIALS ARE ZERO
!
      DO 20 I=1,3
      DO 21 J=1,3
      ROTPAR(I,1,J) = ( -X(I) * X(J) ) / R / R2
   21  CONTINUE
      ROTPAR(I,1,I) = ROTPAR(I,1,I) + ONE / R
   20  CONTINUE
!
!CC      IF(IDBG.GT.0)WRITE(6,5002) (( ROTPAR(I,1,J),I=1,3),J=1,3)
!CC5002  FORMAT(' HCLPAR ROTPAR(1-3,1,1-3) '/(1X,3D24.16))
!
!---------------------------------------------------------------
!
!      ....COLUMN 3
!
!
!      ....XDOT PARTIALS ONLY  --  X PARTIALS ARE ZERO
!
      DO 40 I=1,3
      DO 41 J=1,3
      ROTPAR(I,3,J+3) = ( -X(I+3) * X(J+3) ) / V / V2
   41 END DO
      ROTPAR(I,3,I+3) = ROTPAR(I,3,I+3) + ONE / V
   40 END DO
!
!
!CC      IF(IDBG.GT.0)WRITE(6,5005)  (( ROTPAR(I,3,J),I=1,3),J=1,6)
!CC5005  FORMAT(' HCLPAR ROTPAR(1-3,3,1-6) '/(1X,3D24.16))
!
!
!-------------------------------------------------------------
!
!      .... DON'T COMPUTE THE ROTATION MATRIX
!      ....ROTMAT IS PASSED IN FROM GENACC
!
!CC       CALL UVCOMP( ROTMAT, X, .FALSE. )
!
!CC      IF(IDBG.GT.0)WRITE(6,5006)  (( ROTMAT(I,J),J=1,3),I=1,3)
!CC5006  FORMAT(' HCLPAR ROTMAT(1-3,1-3) '/(1X,3D24.16))
!
!
!------------------------------------------------------------
!
!
!      ....COLUMN 2
!
!      ....ROTPAR(I,2,J) = ( D(R(,3)/DX(J) /\ R(,1) )SUB I
!      ....              + ( R(,3) /\ D(R(,1))/DX(J) ) SUB I
!      ....   ( /\ IS THE CROSS PRODUCT )
!
!      XNN = ONE
!      DXNNXJ = ZERO
!      DXNXJD = ZERO
!
      DO 50 J=1,6
!
      CALL CROSPR( ROTMAT(1,3), ROTPAR(1,1,J), TEMP)
!
      DO 52 I=1,3
      ROTPAR(I,2,J) = TEMP(I)
!    1    / XNN  + ROTMAT(I,2) * DXNNXJ
   52 END DO
!
!CC      IF(IDBG.GT.0)WRITE(6, 5007) ( ROTPAR(I,2,J),I=1,3)
!
      CALL CROSPR( ROTPAR(1,3,J), ROTMAT(1,1), TEMP)
!
      DO 53 I=1,3
      ROTPAR(I,2,J) = ROTPAR(I,2,J) +                                   &
     &                TEMP(I)
!    1    / XNN  + ROTMAT(I,2) * DXNXJD
   53 END DO
!
!CC      IF(IDBG.GT.0)WRITE(6, 5007) ( ROTPAR(I,2,J),I=1,3)
   50 END DO
!
!CC      IF(IDBG.GT.0)WRITE(6, 5007) (( ROTPAR(I,2,J),I=1,3),J=1,6)
!CC5007  FORMAT(' HCLPAR ROTPAR(1-3,2,1-3) '/(1X,3D24.16))
!
!
!
      RETURN
      END
