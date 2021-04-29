      SUBROUTINE MULTIINV (N,M,S,X)
!-------------------------------------------------------------------------------
! Left multiply a 3 x M matrix X by the inverse of I_c, the full moment of inert
! tensor
!
! N = input leading dimension of X
! M = input column dimension of X
! S = input six independent elements of 3 x 3 moment of inertia tensor I_c such
!
!                         / S(1) S(4) S(6) \
!                   I_c = | S(4) S(2) S(5) |
!                         \ S(6) S(5) S(3) /
!
! X = input matrix to be left multiplied by I which is overwritten with result o
!     output
!
! Original version date:  4 November, 2013
! Programmer:             T.J. Sabaka
!-------------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      DIMENSION S(6),X(N,M)
! Compute upper Cholesy factor U of I_c = U^T U and multiply X = U^{-T} X
      U11=SQRT(S(1))
      U12=S(4)/U11
      U13=S(6)/U11
      DO J=1,M
         X(1,J)=X(1,J)/U11
      END DO
      U22=SQRT(S(2)-U12**2)
      U23=(S(5)-U12*U13)/U22
      DO J=1,M
         X(2,J)=(X(2,J)-U12*X(1,J))/U22
      END DO
      U33=SQRT(S(3)-U13**2-U23**2)
      DO J=1,M
         X(3,J)=(X(3,J)-U13*X(1,J)-U23*X(2,J))/U33
      END DO
! Multiply X = U^{-1} X
      DO J=1,M
         X(3,J)=X(3,J)/U33
      END DO
      DO J=1,M
         X(2,J)=(X(2,J)-U23*X(3,J))/U22
      END DO
      DO J=1,M
         X(1,J)=(X(1,J)-U12*X(2,J)-U13*X(3,J))/U11
      END DO
      RETURN
      END
