!$HMF2ED
      FUNCTION HMF2ED(XMAGBR,R,X,XM3)
!********1*********2*********3*********4*********5*********6*********7**
! HMF2ED                                         PGMR - R.WILLIAMSON
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
! calculates the peak height hmf2/km for the magnetic
! latitude xmagbr/deg. and the smoothed zuerich sunspot
! number r using ccir-m3000 xm3 and the ratio x=fof2/foe.
! [ref. d.bilitza et al., telecomm.j., 46, 549-553, 1979]
! d.bilitza,1980.
      f1=(2.32D-3)*r+0.222
      f2=1.2-(1.16D-2)*EXP((2.39D-2)*r)
      f3=0.096*(r-25.0)/150.0
      delm=f1*(1.0-r/150.0*EXP(-xmagbr*xmagbr/1600.0))/(x-f2)+f3
      hmf2ed=1490.0/(xm3+delm)-176.0
      return
      END
