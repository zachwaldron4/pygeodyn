      SUBROUTINE GETDADA0 (R,DODO0,DO0DA0,DADA0)
!-------------------------------------------------------------------------------
! Compute partial derivatives of RA,DEC,W with respect to RA_0, DEC_0, W_0, RA_t
! DEC_t_0, and W_t_0
!
! R      = input current 3 x 3 reference frame axis matrix
! DODO0  = input current 12 x 12 d{O}/d{O_0} partial derivative matrix
! DO0DA0 = input current 12 x 6 d{O_0}/d{A_0} partial derivative matrix
! DADA0  = output 3 x 6 partial derivative matrix structured as
!          d{RA,DEC,W}/d{RA_0,DEC_0,W__0,RA_t_0,DEC_t_0,W_t_0}
!
! Original version date:  10 September, 2013
! Programmer:             T.J. Sabaka
!-------------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      DIMENSION R(3,3),DODO0(12,12),DO0DA0(12,6),DADA0(3,6)
      DIMENSION DADO(3,12),DADO0(3,12)
! get partials of RA, DEC, and W with respect to omega and R
      CALL GETDADO (R,DADO)
! compure partials of RA, DEC, and W with respect to omega_0 and R_0
      DO J=1,12
         DO I=1,3
            DADO0(I,J)=DADO(I, 1)*DODO0( 1,J)+                          &
     &                 DADO(I, 2)*DODO0( 2,J)+                          &
     &                 DADO(I, 3)*DODO0( 3,J)+                          &
     &                 DADO(I, 4)*DODO0( 4,J)+                          &
     &                 DADO(I, 5)*DODO0( 5,J)+                          &
     &                 DADO(I, 6)*DODO0( 6,J)+                          &
     &                 DADO(I, 7)*DODO0( 7,J)+                          &
     &                 DADO(I, 8)*DODO0( 8,J)+                          &
     &                 DADO(I, 9)*DODO0( 9,J)+                          &
     &                 DADO(I,10)*DODO0(10,J)+                          &
     &                 DADO(I,11)*DODO0(11,J)+                          &
     &                 DADO(I,12)*DODO0(12,J)
         END DO
      END DO
! compure partials of RA, DEC, and W with respect to RA_0, DEC_0, W_0, RA_t_0, D
      DO J=1,6
         DO I=1,3
            DADA0(I,J)=DADO0(I, 1)*DO0DA0( 1,J)+                        &
     &                 DADO0(I, 2)*DO0DA0( 2,J)+                        &
     &                 DADO0(I, 3)*DO0DA0( 3,J)+                        &
     &                 DADO0(I, 4)*DO0DA0( 4,J)+                        &
     &                 DADO0(I, 5)*DO0DA0( 5,J)+                        &
     &                 DADO0(I, 6)*DO0DA0( 6,J)+                        &
     &                 DADO0(I, 7)*DO0DA0( 7,J)+                        &
     &                 DADO0(I, 8)*DO0DA0( 8,J)+                        &
     &                 DADO0(I, 9)*DO0DA0( 9,J)+                        &
     &                 DADO0(I,10)*DO0DA0(10,J)+                        &
     &                 DADO0(I,11)*DO0DA0(11,J)+                        &
     &                 DADO0(I,12)*DO0DA0(12,J)
         END DO
      END DO
      RETURN
      END
