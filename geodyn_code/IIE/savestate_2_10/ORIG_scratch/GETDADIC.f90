      SUBROUTINE GETDADIC (R,DODIC,DADIC)
!-------------------------------------------------------------------------------
! Compute partial derivatives of RA,DEC,W with respect to the six independent el
! of I_c, the moment of inertia tensor in the body-fixed system
!
! R      = input current 3 x 3 reference frame axis matrix
! DODIC  = input current 12 x 6 d{O}/d{I_c} partial derivative matrix
! DADIC  = output 3 x 6 partial derivative matrix structured as
!          d{RA,DEC,W}/d{s(1),s(2),s(3),s(4),s(5),s(6)}
!          where:
!
!                         / s(1) s(4) s(6) \
!                   I_c = | s(4) s(2) s(5) |
!                         \ s(6) s(5) s(3) /
!
! Original version date:  6 November, 2013
! Programmer:             T.J. Sabaka
!-------------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      DIMENSION R(3,3),DODIC(12,6),DADIC(3,6)
      DIMENSION DADO(3,12)
! get partials of RA, DEC, and W with respect to omega and R
      CALL GETDADO (R,DADO)
! compure partials of RA, DEC, and W with respect to six independent elements of
      DO J=1,6
         DO I=1,3
            DADIC(I,J)=DADO(I, 1)*DODIC( 1,J)+                          &
     &                 DADO(I, 2)*DODIC( 2,J)+                          &
     &                 DADO(I, 3)*DODIC( 3,J)+                          &
     &                 DADO(I, 4)*DODIC( 4,J)+                          &
     &                 DADO(I, 5)*DODIC( 5,J)+                          &
     &                 DADO(I, 6)*DODIC( 6,J)+                          &
     &                 DADO(I, 7)*DODIC( 7,J)+                          &
     &                 DADO(I, 8)*DODIC( 8,J)+                          &
     &                 DADO(I, 9)*DODIC( 9,J)+                          &
     &                 DADO(I,10)*DODIC(10,J)+                          &
     &                 DADO(I,11)*DODIC(11,J)+                          &
     &                 DADO(I,12)*DODIC(12,J)
         END DO
      END DO
      RETURN
      END
