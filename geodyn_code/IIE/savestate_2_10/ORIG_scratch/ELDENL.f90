!$ELDENL
      FUNCTION ELDENL(HHM)
!********1*********2*********3*********4*********5*********6*********7**
! ELDENL                                         PGMR - R.WILLIAMSON
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
! electron density at height h (m) according to IRI electron profile -
! see electp
!
!
      COMMON/EPROF/HMF2,RNMF2,HMF1,RNMF1,B0P,B1,C1ION,HZ,T,HST,EION(4), &
     &        HME,RNME,HEF,HMD,RNMD,HDX,D1,XKK,FP30,FP3U,FP1,FP2,       &
     &        BETA,ETAION,DELTAI,ZETAIO,X0,EPTRC1,EPTRC2,               &
     &        ALNMF2,ALNME,ALNMD,RNIGHT
      dimension ary(37)
      equivalence (ary(1),hmf2)
!
      data argmax/87.3/
!
      h=hhm/1000.
!
      if(h.lt.hmf2) go to 100
!
!     x0 = 300. - deltai
!     eptrc1 = eptr(x0,beta,394.5)
!     eptrc2 = eptr(x0,100.,300.0)
!
      dxdh = (1000.-hmf2)/700.
      xmx0 = (h-hmf2)/dxdh
      x = xmx0 + x0
!
!     eptr1 = eptr(x,beta,394.5) - eptrc1
!     eptr2 = eptr(x,100.,300.0) - eptrc2
!
        ddx=(x-394.5)/beta
        if(ABS(ddx).lt.argmax) then
          eptrx=LOG(1.+EXP(ddx))
        else
          eptrx=0.
          if(ddx.gt.0.) eptrx=ddx
        endif
        eptr1=eptrx-eptrc1
!
        ddx=(x-300.0)/100.
        if(ABS(ddx).lt.argmax) then
          eptrx=LOG(1.+EXP(ddx))
        else
          eptrx=0.
          if(ddx.gt.0.) eptrx=ddx
        endif
        eptr2=eptrx-eptrc2
!
      y=beta*etaion*eptr1+zetaio*(100.*eptr2-xmx0)
      y=y*dxdh
      if(ABS(y).gt.argmax)y=SIGN(argmax,y)
!
      eldenl=alnmf2-y
      return
  100 hh=h
      hpar=0.
! electron density for the bottomside f-region (hmf1...hmf2).
      if(h.ge.hmf1) go to 150
! electron density for the f1-layer (hz.....hmf1).
      if(h.ge.hz) go to 140
! electron density for the intermediate region (hef..hz).
      if(h.lt.hef) go to 200
      if(hst.lt.0.) then
        elden=rnme+t*(h-hef)
        eldenl=LOG(elden)
        return
      endif
      one=1.
      hh=hz+t/2.0-SIGN(one,t)*SQRT(t*(hz-h+t/4.0))
!
  140 hpar=c1ion*SQRT(ABS(hmf1-hh)/b0p)
  150 x=(hmf2-hh)/b0p
!       z=x**b1
      goober=x
      goober=goober**b1
        if (ABS(goober).gt.3.4D38) then
          goober=SIGN(3.4D38,goober)
        elseif (ABS(goober).lt.1.176D-38) then
          goober=SIGN(1.176D-38,goober)
        endif
      z=goober
      if(z.gt.argmax) z=argmax
      goober=EXP(-z)
      goober=goober/COSH(x)+hpar
      if (ABS(goober).gt.3.4D38) then
        goober=SIGN(3.4D38,goober)
      elseif (ABS(goober).lt.1.176D-38) then
        goober=SIGN(1.176D-38,goober)
      endif
      tt=goober
!     tt=exp(-z)/cosh(x) + hpar
!     elden=rnmf2*(exp(-z)/cosh(x)+hpar)
      eldenl=alnmf2+LOG(tt)
      return
!
  200 if(h.lt.hme) go to 300
! electron density for the e and valley region (hme..hef).
      z=h-hme
      tt=z*z*(eion(1)+z*(eion(2)+z*(eion(3)+z*eion(4))))
      if (rnight.le.0.0) tt=LOG(1.+tt)
      eldenl=alnme+tt
      return
! electron density for the d region (ha...hme).
  300 if(h.lt.hdx) go to 310
      z=hme-h
!     exparg=-d1*z**xkk
      goober=z
      goober=alnme-d1*goober**xkk
        if (ABS(goober).gt.3.4D38) then
          goober=SIGN(3.4D38,goober)
        elseif (ABS(goober).lt.1.176D-38) then
          goober=SIGN(1.176D-38,goober)
        endif
      exparg=goober
      if(ABS(exparg).gt.argmax) exparg=SIGN(argmax,exparg)
      eldenl=exparg
!     eldenl=alnme-d1*z**xkk
      return
  310 z=h-hmd
      fp3=fp3u
      if(z.gt.0.0) fp3=fp30
!     exparg=z*(fp1+z*(fp2+z*fp3))
      goober=z
      goober=alnmd+goober*(fp1+goober*(fp2+goober*fp3))
        if (ABS(goober).gt.3.4D38) then
          goober=SIGN(3.4D38,goober)
        elseif (ABS(goober).lt.1.176D-38) then
          goober=SIGN(1.176D-38,goober)
        endif
      exparg=goober
      if(ABS(exparg).gt.argmax) exparg=SIGN(argmax,exparg)
      eldenl=exparg
!     eldenl=alnmd+z*(fp1+z*(fp2+z*fp3))
      return
      END
