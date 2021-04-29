!$IONLIM
      subroutine ionlim (rd,pt,htlim,rlim1,rlim2,*)
!********1*********2*********3*********4*********5*********6*********7**
! IONLIM                                          PGMR - R.WILLIAMSON
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
      dimension rd(3),pt(3)
!
!     common /celips/ ae,esq,flat
!
      zero=0.
      two=2.
      four=4.
!
      one=1.
      ae=6378136.3
      flat=one/298.257
!
      asq=(ae+htlim)**2
      bsq=(ae-ae*flat+htlim)**2
      abrat=asq/bsq
!
      a=rd(1)**2+rd(2)**2+abrat*rd(3)**2
      b=two*(rd(1)*pt(1)+rd(2)*pt(2)+abrat*rd(3)*pt(3))
      c=pt(1)**2+pt(2)**2+abrat*pt(3)**2-asq
!
      func=b**2-four*a*c
      if (func.lt.zero) return 1
!
      rt=SQRT(func)
!
      rat=(-b-rt)/(a+a)
      x=rat*rd(1)+pt(1)
      y=rat*rd(2)+pt(2)
      z=rat*rd(3)+pt(3)
      rlim1=rd(1)*x+rd(2)*y+rd(3)*z
!
      rat=(-b+rt)/(a+a)
      x=rat*rd(1)+pt(1)
      y=rat*rd(2)+pt(2)
      z=rat*rd(3)+pt(3)
      rlim2=rd(1)*x+rd(2)*y+rd(3)*z
!
      if (rlim1.le.rlim2) return
!
      x=rlim1
      rlim1=rlim2
      rlim2=x
!
      return
      END
