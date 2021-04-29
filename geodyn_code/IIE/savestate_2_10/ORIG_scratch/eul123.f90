!$eul123
      subroutine eul123(q,phi,theta,psi)
!***********************************************************************
! Function: computes the associated euler angles for a 1-2-3 representat
!           from the given quaternion q.
!
! Programmer: S.B. Luthcke                               Date: 11/06/95
!
!
! Note: angles are in radians
! Reference: Kane, T.R., Likins, P.W., Levinson, D.A., 'Spacecraft Dynam
!            McGraw-Hill, 1983
!
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      dimension q(4),r(3,3)
!
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/,tol/1.0D-03/
!
!***********************************************************************
! start of executable code
!***********************************************************************
      pi=4.0D0*ATAN(1.0D0)
!
! compute rotation matrix from quaternion
!
      call qatrot(q,r)
!
! compute 1-2-3 set of euler angles
!
        diff=one - r(3,1)*r(3,1)
        if (diff .gt. tol) then
          theta = ASIN(r(3,1))
          phi = ATAN2(-r(3,2),r(3,3))
          if(phi.lt.zero) phi=phi+two*pi
          psi = ATAN2(-r(2,1),r(1,1))
          if(psi.lt.zero) psi=psi+two*pi
        else
          if (r(3,1) .lt. zero) then
             theta = -pi/two
          else
           theta = pi/two
          endif
          psi=pi/two
          phi=ASIN(r(1,3))
         endif
!
        return
      END
