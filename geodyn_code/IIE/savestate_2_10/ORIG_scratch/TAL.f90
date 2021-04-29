!$TAL
      SUBROUTINE TAL(SHABR,SDELTA,SHBR,SDTDH0,AUS6,SPT)
!********1*********2*********3*********4*********5*********6*********7**
! xxxxxx                                         PGMR - R.WILLIAMSON
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
! calculates the coefficients spt for the polynomial
! y(x)=1+spt(1)*x**2+spt(2)*x**3+spt(3)*x**4+spt(4)*x**5
! to fit the valley in y, represented by:
! y(x=0)=1, the x value of the deepest valley point (shabr),
! the precentage depth (sdelta), the width (shbr) and the
! derivative dy/dx at the upper valley boundry (sdtdh0).
! if there is an unwanted additional extremum in the valley
! region, then aus6=.true., else aus6=.false..
! for -sdelta the coeff. are calculated for the function
! y(x)=exp(spt(1)*x**2+...+spt(4)*x**5).
      LOGICAL aus6
      dimension spt(4)
!
      z1=-sdelta/100.0
      if(sdelta.le.0.) z1=LOG(1.+z1)
      z1=z1/(shabr*shabr)
      z3=sdtdh0/(2.*shbr)
      z4=shabr-shbr
      spt(4)=2.0*(z1*(shbr-2.0*shabr)*shbr+z3*z4*shabr)/                &
     &  (shabr*shbr*z4*z4*z4)
      spt(3)=z1*(2.0*shbr-3.0*shabr)/(shabr*z4*z4)-                     &
     &  (2.*shabr+shbr)*spt(4)
      spt(2)=-2.0*z1/shabr-2.0*shabr*spt(3)-3.0*shabr*shabr*spt(4)
      spt(1)=z1-shabr*(spt(2)+shabr*(spt(3)+shabr*spt(4)))
      aus6=.false.
      b=4.*spt(3)/(5.*spt(4))+shabr
      c=-2.*spt(1)/(5*spt(4)*shabr)
      z2=b*b/4.-c
      if(z2.lt.0.0) goto 300
      z3=SQRT(z2)
      z1=b/2.
      z2=-z1+z3
      if(z2.gt.0.0.and.z2.lt.shbr) aus6=.true.
      if (ABS(z3).gt.1.D-15) goto 400
      z2=c/z2
      if(z2.gt.0.0.and.z2.lt.shbr) aus6=.true.
      return
  400 z2=-z1-z3
      if(z2.gt.0.0.and.z2.lt.shbr) aus6=.true.
  300 return
      END
