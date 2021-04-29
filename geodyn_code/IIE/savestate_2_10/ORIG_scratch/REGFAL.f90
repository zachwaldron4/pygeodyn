!$REGFAL
      SUBROUTINE REGFAL(X11,X22,FX11,FX22,EPS,FW,SCHALT,X,lxe3)
!********1*********2*********3*********4*********5*********6*********7**
! REGFAL                                         PGMR - R.WILLIAMSON
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
! regula-falsi-procedure to find x with g(x)-fw=0. x1,x2 are the
! starting values. the comutation ends when the x-interval
! has become less than eps . if sign(f(x1)-fw)= sign(f(x2)-fw)
! then schalt=.true.
      LOGICAL l1,links,k,schalt
      external xe2e3
      fxe2e3(x)=xe2e3(x,lxe3)
      data lfirst/.true./
      data lfirst2/.true./
! begin executable code
      schalt=.false.
      ep=eps
      x1=x11
      x2=x22
      f1=fx11-fw
      f2=fx22-fw
      k=.false.
      ng=2
      nlfd=0
      links=(f1*f2.gt.0.0)
      if(links) then
        x=0.0
        schalt=.true.
        lfirst=.false.
        lfirst2=.false.
        return
      endif
      l1=.false.
      x=(x1*f2-x2*f1)/(f2-f1)
!
  400 fx=fxe2e3(x)-fw
      nlfd=nlfd+1
      if(nlfd.gt.20) then
        ep=ep*10.
        nlfd=0
      endif
      links=(f1*fx.gt.0.0)
      if(links) then
        x1=x
        f1=fx
      else
        x2=x
        f2=fx
      endif
      if(ABS(x2-x1).le.ep) return
      k=.not.k
      if(k) then
        l1=links
        dx=(x2-x1)/ng
        if(.not.links) dx=dx*(ng-1)
        x=x1+dx
      else
        if((links.and.(.not.l1)).or.(.not.links.and.l1)) ng=2*ng
        x=(x1*f2-x2*f1)/(f2-f1)
      endif
      go to 400
!c    return
!
      END
