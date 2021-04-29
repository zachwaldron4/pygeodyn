!$GAMMAX
      FUNCTION GAMMAX (INIT,IHARM,NQ,KNQ,M,MM,M3,SFE)
!********1*********2*********3*********4*********5*********6*********7**
! GAMMAX                                         PGMR - R.WILLIAMSON
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
! calculates gamma1=fof2 or m3000 using ccir numerical map
! coefficients sfe(m3) for modified dip latitude (smodip/deg)
! geographic latitude (slat/deg) and longitude (slong/deg)
! and universial time (hour/decimal hours).
! nq(k1) is an integer array giving the highest degrees in
! modip latitude for each longitude harmonic.
! m=1+nq1+2(nq2+1)+2(nq3+1)+... .
! sheikh,4.3.77.
      COMMON/CTRIG/CHA,SHA,CBG,SBG,CLG,SLG,SMODIP
      dimension nq(knq),sfe(m3)
      dimension sc(18),scl(22),xsinx(11),coef(100)
!$$   real sc(18),scl(22),xsinx(11),coef(100)
!
!     real ff0(988),ff0n(988),xm0(441)
!     integer qm(7),qf(9)
!     data qf/11,11,8,4,1,0,0,0,0/,qm/6,7,5,2,1,0,0/
!
!
!
      if(init.ne.0) go to 50
!
      imaxt=6
      imaxsm=11
      imaxl=8
!
      sc(1)=0.
      sc(2)=1.
      sc(3)=sha
      sc(4)=cha
!     hou=(15.0*hour-180.0)*dtr
!     sc(3)=sin(hou)
!     sc(4)=cos(hou)
      c2=sc(4)+sc(4)
!
      imt=imaxt+imaxt+2
!
      do 10 i=1,imt
   10 sc(i+4)=c2*sc(i+2) - sc(i)
!
      xpow=1.0
      do 20 i=1,imaxsm
      xpow=xpow*smodip
   20 xsinx(i)=xpow
!
      scl(1)=0.
      scl(2)=1.
      scl(3)=slg
      scl(4)=clg
      c2=scl(4)+scl(4)
!
      iml=imaxl+imaxl+2
!
      do 30 i=1,iml
   30 scl(i+4)=c2*scl(i+2) - scl(i)
!
      xpow=1.0
      do 40 i=1,imaxl
      xpow=xpow*cbg
      scl(i+i+1)=xpow*scl(i+i+1)
   40 scl(i+i+2)=xpow*scl(i+i+2)
!
   50 mi=0
      iht=iharm+iharm
!
      do 70 i=1,m
      coefi=sfe(mi+1)
      do 60 j=1,iht
   60 coefi=coefi+sfe(mi+j+1)*sc(j+2)
      coef(i)=coefi
!
   70 mi=mi+mm
!
      indx=nq(1)
      sum=coef(1)
      do 80 j=1,indx
   80 sum=sum+coef(j+1)*xsinx(j)
!
      np=nq(1)+1
      kk=1
!
      do 100 k=2,knq
!
      s1=coef(np+1)
      s2=coef(np+2)
      np=np+2
      indx=nq(k)
      if(indx.gt.0) then
        do 90 il=1,indx
        s1=s1+coef(np+1)*xsinx(il)
        s2=s2+coef(np+2)*xsinx(il)
   90   np=np+2
      endif
! note input does not consistently use sine, cosine order
      sum=sum+s2*scl(kk+2)+s1*scl(kk+3)
  100 kk=kk+2
!
      gammax=sum
      return
      END
