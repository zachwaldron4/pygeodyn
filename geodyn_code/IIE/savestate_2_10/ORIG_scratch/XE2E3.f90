!$XE2E3
      FUNCTION XE2E3(H,lxe3)
!********1*********2*********3*********4*********5*********6*********7**
! XE2E3                                          PGMR - R.WILLIAMSON
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
      COMMON/EPROF/HMF2,RNMF2,HMF1,RNMF1,B0P,B1,C1ION,HZ,T,HST,EION(4), &
     &        HME,RNME,HEF,HMD,RNMD,HDX,D1,XKK,FP30,FP3U,FP1,FP2,       &
     &        BETA,ETAION,DELTAI,ZETAIO,X0,EPTRC1,EPTRC2,               &
     &        ALNMF2,ALNME,ALNMD,RNIGHT
!
      data argmax/87.3/
!
      hh=h
      hpar=0.
      if (lxe3) hpar=c1ion*SQRT(ABS(hmf1-hh)/b0p)
      x=(hmf2-hh)/b0p
!     z=x**b1
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
        goober=rnmf2*(goober/COSH(x)+hpar)
        if (ABS(goober).gt.3.4D38) then
          goober=SIGN(3.4D38,goober)
        elseif (ABS(goober).lt.1.176D-38) then
          goober=SIGN(1.176D-38,goober)
        endif
      xe2e3=goober
!     xe2e3=rnmf2*(exp(-z)/cosh(x)+hpar)
      return
      END
