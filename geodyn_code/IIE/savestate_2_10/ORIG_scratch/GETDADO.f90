      SUBROUTINE GETDADO (R,DRDWDOR)
!-------------------------------------------------------------------------------
! Compute partial derivatives of RA,DEC,W, in degrees, with respect to omega and
!
! R       = input current 3 x 3 reference frame axis matrix
! DRDWDOR = output 3 x 12 partial derivative matrix structured as
!           [ d{RA,DEC,W}/d{omega} d{RA,DEC,W}/d{R} ]
!
! Original version date:  19 August, 2013
! Programmer:             T.J. Sabaka
!-------------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      DIMENSION R(3,3),DRDWDOR(3,12)
      DATA DDDR /57.2957795130823D0/
! d{RA,DEC,W}/d{omega} = 0
      DO I=1,3
         DO J=1,12
            DRDWDOR(I,J)=0.D0
         END DO
      END DO
! d{PHI}/d{R} = d{RA}/d{R}
      DRDWDOR(1,10)=-DDDR*R(2,3)/(R(1,3)**2+R(2,3)**2)
      DRDWDOR(1,11)= DDDR*R(1,3)/(R(1,3)**2+R(2,3)**2)
! d{DEC}/d{R} = -d{THETA}/d{R}
      DRDWDOR(2,12)= DDDR/SQRT(1.D0-R(3,3)**2)
! d{W}/d{R}
      DRDWDOR(3, 6)= DDDR*R(3,2)/(R(3,1)**2+R(3,2)**2)
      DRDWDOR(3, 9)=-DDDR*R(3,1)/(R(3,1)**2+R(3,2)**2)
      RETURN
      END
