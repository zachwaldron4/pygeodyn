!$ZENITH
      SUBROUTINE ZENITH (RSTA,RSUN,CHICOS,CHNCOS,RNIGHT,EPSTDF)
!********1*********2*********3*********4*********5*********6*********7**
! ZENITH                                         PGMR - R.WILLIAMSON
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
      dimension rsun(3),rsta(3)
!
      one=1.
! rtoh is 12./pi
      rtoh=3.81971863421
!
      chicos=rsta(1)*rsun(1)+rsta(2)*rsun(2)+rsta(3)*rsun(3)
!
! sunrise at height h metres is at...............................
!       chi(h) = 90.83 + 0.0347 * sqrt(h)........................
! this includes corrections for horizontal refraction and........
! semi-diameter of the solar disk................................
! see explanatory supplement to the ephemeris (1961) pg 401......
!     ch = cos(90.83 * dtr)
!
      ch=-.01448573
!
      aa0=rsta(3)*rsun(3)
      bb=SQRT((one-rsta(3)**2)*(one-rsun(3)**2))
! cos(zenith) at high noon:
      chncos= bb+aa0
! hour-noon
      cosdif=rsun(2)*rsta(2)+rsun(1)*rsta(1)
      sindif=rsun(1)*rsta(2)-rsun(2)*rsta(1)
      phihnc=ATAN2(sindif,cosdif)
      phihnc = phihnc*rtoh
!
! calculate sunrise and sunset wrt high noon --  at the ground
!
      cosphi = (ch -aa0) / bb
      if(ABS(cosphi).gt.one) then
!          sun does not rise/set
        epstdf=0.
        if(chicos.lt.ch) then
          rnight=1.0
        else
          rnight=0.0
        endif
      else
! phi=ra(sun) - ra(sta) at max zenith angle
        phi = ACOS(cosphi)
        phi = phi*rtoh
!
        hrmrse=phihnc+phi
        hrmset=phihnc-phi
!
!       epstdf=epst(hour,1.,sunrse) - epst(hour,1.,sunset)
!
        epstdf=one/(one+EXP(-hrmrse))-                                  &
     &         one/(one+EXP(-hrmset))
!
        if(hrmrse.lt.0.0.or.hrmset.gt.0.0) then
           rnight=1.0
        else
           rnight=0.0
        endif
!
      endif
!
      return
      END
