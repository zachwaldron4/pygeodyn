      subroutine grotate(alpha,x,xr,rotv,ierr,ikind)
!
!********1*********2*********3*********4*********5*********6*********7**
! GROTATE           02/20/13            1304.0    Oscar Colombo
!
! FUNCTION         CALCULATE ACCELERATIONS ON SATELLITE AND
!                  VARIATIONAL PARTIALS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
! This subroutine rotates a vector x => xr by an angle alpha around an oriented
! through the origin, and aligned with a vector called 'rotv'.
! If ikind = 1, alpha is in degrees;
! if ikind = 0, in radians.
! This subroutine calls subroutine "sprod", "vprod", and "vnorm".
! ierr = 0 if no problems are found. Otherwise, ierr = 1 means wrong "ikind" arg
!
!********1*********2*********3*********4*********5*********6*********7**
!
      implicit DOUBLE PRECISION (a-h,o-z)
      save
      dimension x(3),xr(3),r(3),xp(3),yp(3),work(3),rotv(3),xpr(3), &
     &dotxr(3)
      data istart/1/
      if(istart.eq.1) then
      pi = 4.D0*ATAN(1.D0)
      radeg = pi/180.D0
      istart = 0
      endif
!
      if(ikind.ne.1.and.ikind.ne.0) then
      print*,' ERROR: Last argument in a call to "grotate" '
      print*,' should be an integer valued 1 or 0, but it is not. '
      ierr = 1
      return
      endif
!
      angle = alpha
      if(ikind.eq.1) angle = alpha*radeg
      cosangle = COS(angle)
      sinangle = SIN(angle)
!
! Normalize rotv, in case it is not a unit vector already, obtaining r :
!
      call vnorm(rotv,r,rnorm)
!
! Project x on the plane perpendicular to r and normalize the result,
! to obtain vector xp:
!
      call sprod(x,r,dotr)
!
      do i = 1,3
      dotxr(i) = dotr*r(i)
      work(i) = x(i)-dotxr(i)
      enddo
      call vnorm(work,xp,xpmod)
!
! Find yp, a unit vector perpendicular to r and xp,
! forming a right-handed triad with them:
!
      call vprod(r,xp,work)
      call vnorm(work,yp,www)
!
! Rotate vector xp about r by the desired angle, to obtain xpr:
!
      do i = 1,3
      xpr(i) = xp(i)*cosangle+ yp(i)*sinangle
      enddo
!
! Finally, go back from xpr, in the plane normal to r, to the rotated x, or xr:
!
      do i = 1,3
      xr(i) = xpr(i)*xpmod+dotxr(i)
      enddo
!
      return
      end
