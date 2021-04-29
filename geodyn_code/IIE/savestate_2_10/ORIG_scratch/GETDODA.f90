      SUBROUTINE GETDODA (OMEGA,R,DORDRDW)
!-------------------------------------------------------------------------------
! Compute partial derivatives of omega and R with respect to RA,DEC,W,RA_t,DEC_t
!
! OMEGA   = input current 3 x 1 omega vector value
! R       = input current 3 x 3 reference frame axis matrix
! DORDRDW = output 12 x 6 partial derivative matrix structured as
!           / d{omega}/d{RA,DEC,W} d{omega}/d{RA_t,DEC_t,W_t} \
!           \ d{R}/d{RA,DEC,W}     d{R}/d{RA_t,DEC_t,W_t}     /
!
! Original version date:  19 August, 2013
! Programmer:             T.J. Sabaka
!
! Modification 1:         Changed units of RA,DEC,W,RA_t,DEC_t,W_t from
!                         radians to degrees.
! Modification 1 date:    5 December, 2014
! Mod 1 programmer:       J.B. Nicholas
!
! Modification 2:         Corrected code bug in computation of PHI_t
! Modification 2 date:    16 December, 2014
! Mod 1 programmer:       T.J. Sabaka
!-------------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION OMEGA(3),R(3,3),DORDRDW(12,6)
      DIMENSION AXIS(3,3),ANGLET(3)
! compute PHI,THETA,W from current R matrix
      PHI=ATAN2(R(1,3),-R(2,3))
      THETA=ACOS(R(3,3))
      W=ATAN2(R(3,1),R(3,2))
      CPHI=COS(PHI)
      SPHI=SIN(PHI)
      CTHETA=COS(THETA)
      STHETA=SIN(THETA)
      CW=COS(W)
      SW=SIN(W)
! compute axis matrix from PHI,THETA,W
      AXIS(1,1)=STHETA*SW
      AXIS(2,1)=STHETA*CW
      AXIS(3,1)=CTHETA
      AXIS(1,2)= CW
      AXIS(2,2)=-SW
      AXIS(3,2)=0.D0
      AXIS(1,3)=0.D0
      AXIS(2,3)=0.D0
      AXIS(3,3)=1.D0
! d{omega}/d{RA_t} = d{omega}/d{PHI_t}
      DORDRDW(1,4)=AXIS(1,1)
      DORDRDW(2,4)=AXIS(2,1)
      DORDRDW(3,4)=AXIS(3,1)
! d{omega}/d{DEC_t} = -d{omega}/d{THETA_t}
      DORDRDW(1,5)=-AXIS(1,2)
      DORDRDW(2,5)=-AXIS(2,2)
      DORDRDW(3,5)=-AXIS(3,2)
! d{omega}/d{W_t}
      DORDRDW(1,6)=AXIS(1,3)
      DORDRDW(2,6)=AXIS(2,3)
      DORDRDW(3,6)=AXIS(3,3)
! solve for PHI_t,THETA_t,W_t = {AXIS}^{-1} omega via Gaussian elimination
!!!      ANGLET(1)=OMEGA(1)
!!!      ANGLET(2)=OMEGA(2)
!!!      ANGLET(3)=OMEGA(3)
!!!      AXIS(2,2)=AXIS(2,2)-AXIS(2,1)*AXIS(1,2)/AXIS(1,1)
!!!      AXIS(3,2)=AXIS(3,2)-AXIS(3,1)*AXIS(1,2)/AXIS(1,1)
!!!      ANGLET(2)=ANGLET(2)-AXIS(2,1)*ANGLET(1)/AXIS(1,1)
!!!      ANGLET(3)=ANGLET(3)-AXIS(3,1)*ANGLET(1)/AXIS(1,1)
!!!      ANGLET(3)=ANGLET(3)-AXIS(3,2)*ANGLET(2)/AXIS(2,2)
!!!      ANGLET(3)=ANGLET(3)/AXIS(3,3)
!!!      ANGLET(2)=(ANGLET(2)-AXIS(2,3)*ANGLET(3))/AXIS(2,2)
!!!      ANGLET(1)=(ANGLET(1)-AXIS(1,2)*ANGLET(2)-AXIS(1,3)*ANGLET(3))/    &
!!!     &          AXIS(1,1)
!!!
!!! THE ABOVE COMMENTED OUT CODE IS SINGULAR WHEN W0=0
!!! THE FOLLOWING 4 LINES REPLACE THE ABOVE ANF WORK FOR ALL W0
!!!
      CALL DNVERT(3,AXIS,3,ISCR)
      DO I=1,3
       ANGLET(I)=AXIS(I,1)*OMEGA(1)+AXIS(I,2)*OMEGA(2)+AXIS(I,3)*OMEGA(3)
      ENDDO
! d{omega}/d{RA} = d{omega}/d{PHI}
      DORDRDW( 1,1)=0.D0
      DORDRDW( 2,1)=0.D0
      DORDRDW( 3,1)=0.D0
! d{omega}/d{DEC} = -d{omega}/d{THETA}
      DORDRDW( 1,2)=-CTHETA*SW*ANGLET(1)
      DORDRDW( 2,2)=-CTHETA*CW*ANGLET(1)
      DORDRDW( 3,2)= STHETA   *ANGLET(1)
! d{omega}/d{W}
      DORDRDW( 1,3)= STHETA*CW*ANGLET(1)-SW*ANGLET(2)
      DORDRDW( 2,3)=-STHETA*SW*ANGLET(1)-CW*ANGLET(2)
      DORDRDW( 3,3)=0.D0
! d{R}/d{RA} = d{R}/d{PHI}
      DORDRDW( 4,1)=-SPHI*CW-CPHI*CTHETA*SW
      DORDRDW( 5,1)= CPHI*CW-SPHI*CTHETA*SW
      DORDRDW( 6,1)=0.D0
      DORDRDW( 7,1)= SPHI*SW-CPHI*CTHETA*CW
      DORDRDW( 8,1)=-CPHI*SW-SPHI*CTHETA*CW
      DORDRDW( 9,1)=0.D0
      DORDRDW(10,1)= CPHI*STHETA
      DORDRDW(11,1)= SPHI*STHETA
      DORDRDW(12,1)=0.D0
! d{R}/d{DEC} = -d{R}/d{THETA}
      DORDRDW( 4,2)=-SPHI*STHETA*SW
      DORDRDW( 5,2)= CPHI*STHETA*SW
      DORDRDW( 6,2)=-CTHETA*SW
      DORDRDW( 7,2)=-SPHI*STHETA*CW
      DORDRDW( 8,2)= CPHI*STHETA*CW
      DORDRDW( 9,2)=-CTHETA*CW
      DORDRDW(10,2)=-SPHI*CTHETA
      DORDRDW(11,2)= CPHI*CTHETA
      DORDRDW(12,2)= STHETA
! d{R}/d{W}
      DORDRDW( 4,3)=-CPHI*SW-SPHI*CTHETA*CW
      DORDRDW( 5,3)=-SPHI*SW+CPHI*CTHETA*CW
      DORDRDW( 6,3)= STHETA*CW
      DORDRDW( 7,3)=-CPHI*CW+SPHI*CTHETA*SW
      DORDRDW( 8,3)=-SPHI*CW-CPHI*CTHETA*SW
      DORDRDW( 9,3)=-STHETA*SW
      DORDRDW(10,3)=0.D0
      DORDRDW(11,3)=0.D0
      DORDRDW(12,3)=0.D0
! d{R}/d{RA_t,DEC_t,W_t} = 0
      DO J=4,6
         DO I=4,12
            DORDRDW(I,J)=0.D0
         END DO
      END DO
      DORDRDW(:,:) = DORDRDW(:,:) * DEGRAD
      RETURN
      END
