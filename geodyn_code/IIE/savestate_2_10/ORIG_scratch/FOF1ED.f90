!$FOF1ED
      FUNCTION FOF1ED(YLATI,R,CHI,COSCHI)
!********1*********2*********3*********4*********5*********6*********7**
! FOF1ED                                         PGMR - R.WILLIAMSON
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
!--------------------------------------------------------------
!     function fof1ed(ylati,r,chi)
! calculates the f1 peak plasma frequency (fof1/mhz)
! for   dip-latitude (ylati/degree)
!       smoothed zurich sunspot number (r)
!       solar zenith angle (chi/degree)
! reference:
!       e.d.ducharme et al., radio science 6, 369-378, 1971
!                                      and 8, 837-839, 1973
!       however with magnetic dip latitude instead of geomagnetic
!       dipole latitude, eyfrig, 1979
!--------------------------------------------- d. bilitza, 1988.
! use available cosines ---------------------r.williamson, 1995.
!
      dla =  ABS(ylati)
      chi0 = 49.84733 + 0.349504 * dla
      chi100 = 38.96113 + 0.509932 * dla
      chim = ( chi0 + ( chi100 - chi0 ) * r / 100. )
      if(chi.gt.chim) then
        fof1 = 0.0
      else
        f0 = 4.35 + dla * ( 0.0058 - 1.2D-4 * dla )
        f100 = 5.348 + dla * ( 0.011 - 2.3D-4 * dla )
        fs = f0 + ( f100 - f0 ) * r / 100.0
        xmue = 0.093 + dla * ( 0.0046 - 5.4D-5 * dla ) + 3.0D-4 * r
        fof1 = fs * coschi**xmue
      endif
      fof1ed = fof1
      return
      END
