!$XMDED
      FUNCTION XMDED(COSCHI,R,YW)
!********1*********2*********3*********4*********5*********6*********7**
! XMDED                                          PGMR - R.WILLIAMSON
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
!
!     function xmded(xhi,r,yw)
! d. bilitza, 1978, calculates electron density of d maximum.
! xhi/deg. is solar zenith angle, r smoothed zurich sunspot number
! and yw/m-3 the assumed constant night value.
! [ref.: d.bilitza, world data center a report uag-82,7,
! r.williamson, 1995, computational efficiency changes
!       boulder,1981]
      data rold /-999./
      if (r.eq.rold) go to 10
      rold=r
      y=6.05D8+0.088D8*r
      z=(-0.1/(LOG(yw/y)))**0.3704
! use constant for over 60 degree zenith angle
      zlim=0.5
      if(z.gt.zlim) z=zlim
   10 x=coschi
      xmded=yw
      if(x.gt.z) xmded=y*EXP(-0.1/x**2.7)
      return
      END
