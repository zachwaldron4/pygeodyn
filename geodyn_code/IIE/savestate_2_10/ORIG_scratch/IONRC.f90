!$IONRC
      SUBROUTINE IONRC (AA,TSEC,POS1,POS2,STEC,LOGINT)
!********1*********2*********3*********4*********5*********6*********7**
! IONRC                                          PGMR - R.WILLIAMSON
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
! integrate through electron profiles to get tec in
!
!  Calculate vertical electron profiles using electp at equiangular
!  spacing delta. No stepsize checking/error control implemented.
!
! WARNING: working units are km for interfacing with IRI-90.
! NOTE:    geodetic conversions are reduced accuracy version.
!
!          index of refraction = 1.-40.3*N/f**2
!          ( 0.5*e**2/(4*pi*m*eps0 = 40.3 )
!
!          e.g.: elctom=40.3/(2.400e9)**2
!                stec=stec*elctom   in m-2
!
      LOGICAL logint,begin,vert
!     common/const/
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CTRIG/CHA,SHA,CBG,SBG,CLG,SLG,SMODIP
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      dimension AA(1)
      dimension rsun(3),ru(3)
      dimension pos1(3),pos2(3),rd(3),u1(3),u2(3),pt(3)
!
!  define arrays for flipping common block data --
!  geodyn conventions preclude alternative approaches
!  until structures available or conventions change
!
      dimension aryc(37),aryp(37)
!
!
!
      dimension htrng(6),step(5),stepd2(5)
      data htrng /40.0,100.,330.,550.,700.,1000./
      data step  /     2.,    1., 2.5, 10.,  30./
      data one/0./
      data lfirst/.true./
      data lfirst2/.true./
      data lfirst3/.true./
!
!
!  heibeg should be ha ? = hpole(65.,80.,epstdf) ?
!     heirng(1) = heibeg
!     heirng(6) = heiend
!     heimax = heiend
!     heirng(2) = hmf2 - 50.0
!     if ( heirng(2) .lt. 110.0 ) heirng(2) = 120.0
!     heirng(3) = hmf2 + 50.0
!     heirng(4) = hmf2 + 100.0
!     heirng(5) =  700.0
!
      if(one.ne.1.) then
        one=1.
        zero=0.
!
!       elctom=40.3/(2.400e9)**2
!
        ae=6378136.3
        flat=one/298.257
        ome2=(one-flat)**2
        cht1=ae
        cht3=1.5*ae*flat**2
        cht2=-ae*flat-cht3
!       ht=rht-(cht1+cht2*sinphi**2+cht3*sinphi**4)
!          where sinphi=z/r
!
        htrng(1)=htrng(1)*1000.
        do 10 i=1,5
        htrng(i+1)=htrng(i+1)*1000.
        step(i)=step(i)*1000.
   10   stepd2(i)=step(i)/2.
!
!
!       delta=2.*umr
        delta=2.*degrad
        cosdel=COS(delta)
        sindel=SIN(delta)
!
!
      endif
!
      stec=zero
!
      rd(1)=pos2(1)-pos1(1)
      rd(2)=pos2(2)-pos1(2)
      rd(3)=pos2(3)-pos1(3)
!
      rdmag=rd(1)**2+rd(2)**2+rd(3)**2
      if (rdmag.le.zero) return
      rdmag=SQRT(rdmag)
      rd(1)=rd(1)/rdmag
      rd(2)=rd(2)/rdmag
      rd(3)=rd(3)/rdmag
!
      rr1=pos1(1)*rd(1)+pos1(2)*rd(2)+pos1(3)*rd(3)
      rr2=pos2(1)*rd(1)+pos2(2)*rd(2)+pos2(3)*rd(3)
!
      pt(1)=pos1(1)-rr1*rd(1)
      pt(2)=pos1(2)-rr1*rd(2)
      pt(3)=pos1(3)-rr1*rd(3)
!
      rp2=pt(1)**2+pt(2)**2+pt(3)**2
      rp=SQRT(rp2)
!
!
      if (rp2.gt.zero) then
        sinph2=pt(3)**2/rp2
      else
        sinph2=one-rd(3)**2
      endif
!
      htp=rp-(cht1+cht2*sinph2+cht3*sinph2**2)
!
      if (htp.ge.htrng(6)) return
      if (htp.le.htrng(1).and.rr1*rr2.lt.zero) then
        stec=-one
        return
      endif
!
      call ionlim (rd,pt,htrng(6),rlim1,rlim2,*200)
!
      if(rr1.gt.rlim2.or.rr2.lt.rlim1) return
!
!
!
      rstrt=rr1
      if (rstrt.lt.rlim1) rstrt=rlim1
      rend=rr2
      if (rend.gt.rlim2) rend=rlim2
      rstrti=rend
      rendi=rend
!
      if (htp.lt.htrng(2)) then
!
        call ionlim (rd,pt,htrng(2),rlim1,rlim2,*20)
!
        if (rstrt.gt.rlim1) then
          if (rstrt.lt.rlim2) rstrt=rlim2
        elseif (rend.lt.rlim2) then
          if (rend.gt.rlim1) then
            rend=rlim1
            rstrti=rend
            rendi=rend
          endif
        elseif (rstrt.le.rlim1.and.rend.ge.rlim2) then
          rendi=rlim1
          rstrti=rlim2
        endif
        if (rstrt.gt.rend) then
           write(*,*)                                                   &
     &      ' Signal path entirely below ionosphere - anomalous!'
          stec=-2.
          return
        endif
      endif
!
   20 rstrtx=rstrt
      rendx=rendi
!
      time=tsec/86400.
      mjd=time+TMGDN2
      fday=MOD(time,one)
!
!     houra=(360.*fday-180.)*umr
      houra=(360.*fday-180.)*degrad
      cha=COS(houra)
      sha=SIN(houra)
!
      call sunvec (mjd,fday,rsun,rasun)
!
      thg=rasun*degrad+houra
      costh=COS(thg)
      sinth=SIN(thg)
!
      p1=SQRT(rr1**2+rp2)
      p2=SQRT(rr2**2+rp2)
!
      rp=SQRT(rp2)
      sing=rp/p1
      cosg=-rr1/p1
!
      u1(1)=pos1(1)/p1
      u1(2)=pos1(2)/p1
      u1(3)=pos1(3)/p1
!
      u2(1)=pos2(1)/p2
      u2(2)=pos2(2)/p2
      u2(3)=pos2(3)/p2
!
      cosb=(rr1*rr2+rp2)/p1/p2
      sinb=SQRT(MAX(one-cosb**2,zero))
!
      begin=.true.
!
      vert=ABS(sinb).lt.0.001
!
      if (.not.vert) go to 30
!
      ru(1)=u1(1)
      ru(2)=u1(2)
      ru(3)=u1(3)
!
      rht=MIN(p1,p2)
!
      go to 90
!
! slant calculation:
!
   30 cosa=(rr1*rstrtx+rp2)/p1/SQRT(rstrtx**2+rp2)
      sina=SQRT(MAX(one-cosa**2,zero))
      go to 80
!
   40 continue
      call electp (aa,mjd,fday,ru,rsun)
!
      do 50 i=1,6
      if(htrng(i).lt.ht) go to 60
   50 continue
      i=5
   60 ihts=i
!
      if(vert) go to 160
!
      begin=.false.
!
   70 csa=cosa*cosdel-sina*sindel
      sina=cosa*sindel+sina*cosdel
      cosa=csa
!
      oldht=ht
      oldrt=rendt
!
   80 rht=rp/(sing*cosa+cosg*sina)
      rendt=sina*rht/sing+rr1
!
      aabb=cosa-sina*cosb/sinb
      bb=sina/sinb
!
      ru(1)=aabb*u1(1)+bb*u2(1)
      ru(2)=aabb*u1(2)+bb*u2(2)
      ru(3)=aabb*u1(3)+bb*u2(3)
!
   90 sinph2=ru(3)**2
      ht=rht-(cht1+cht2*sinph2+cht3*sinph2**2)
!
      rtxy=SQRT(ru(1)**2+ru(2)**2)
      rtxym=ome2*rtxy
      denom=SQRT(sinph2+rtxym**2)
      cbg=rtxym/denom
      sbg=ru(3)/denom
      if (rtxy.ne.zero) then
        clon=ru(1)/rtxy
        slon=ru(2)/rtxy
        clg=clon*costh+slon*sinth
        slg=slon*costh-clon*sinth
      else
        clg=costh
        slg=-sinth
      endif
!
      if (begin) go to 40
!
      htrate=(ht-oldht)/(rendt-oldrt)
      rtest=MIN(rendt,rendx)
!
      psum=zero
!
      if (logint) go to 120
!
!** integrate along ray path using
!** linear interpolation for profile:
!
      psum=zero
      rr=oldrt
      iht=ihts
  100 rx=rr+stepd2(iht)
      height=htrate*(rx-oldrt)+oldht
      xe=zero
      if (height.ge.htrng(1).and.height.le.htrng(6)) xe=elden(height)
      psum=psum+xe*step(iht)*(rendt-rx)
      if (height.lt.htrng(iht)) then
        if (iht.gt.1) iht=iht-1
      elseif (height.gt.htrng(iht+1)) then
        if (iht.lt.5) iht=iht+1
      endif
      rr=rr+step(iht)
      if (rr.lt.rtest) go to 100
      if (rr.gt.rtest) then
         psum=psum-xe*(rr-rtest)*(rendt-rx)
      endif
!
      call electp (aa,mjd,fday,ru,rsun)
!
      rr=oldrt
      iht=ihts
  110 rx=rr+stepd2(iht)
      height=htrate*(rx-oldrt)+oldht
      xe=zero
      if (height.ge.htrng(1).and.height.le.htrng(6)) xe=elden(height)
      psum=psum+xe*step(iht)*(rx-oldrt)
      if (height.lt.htrng(iht)) then
        if (iht.gt.1) iht=iht-1
      elseif (height.gt.htrng(iht+1)) then
        if (iht.lt.5) iht=iht+1
      endif
      rr=rr+step(iht)
      if (rr.lt.rtest) go to 110
      if (rr.gt.rtest) then
        psum=psum-xe*(rr-rtest)*(rx-oldrt)
      endif
      stec=stec+psum/(rendt-oldrt)
!
      go to 150
!
!** integrate along ray path using
!** logarithmic interpolation for profile:
!
  120 call eprget (aryp)
!
      call electp (aa,mjd,fday,ru,rsun)
!
      call eprget (aryc)
!
      rr=oldrt
      iht=ihts
      xntvl=rendt-oldrt
  130 rx=rr+stepd2(iht)
      height=htrate*(rx-oldrt)+oldht
      xe=zero
      if (height.ge.htrng(1).and.height.le.htrng(6)) then
        call eprput (aryp)
        denlnp=eldenl(height)
        call eprput (aryc)
        denlnc=eldenl(height)
        xe=EXP( denlnp+(denlnc-denlnp)*(rx-oldrt)/xntvl )
        psum=psum+xe*step(iht)
        if (height.lt.htrng(iht)) then
          if (iht.gt.1) iht=iht-1
        elseif (height.gt.htrng(iht+1)) then
          if (iht.lt.5) iht=iht+1
        endif
      endif
      rr=rr+step(iht)
      if (rr.lt.rtest) go to 130
      if (rr.gt.rtest) then
        psum=psum-xe*(rr-rtest)
      endif
      stec=stec+psum
!
  150 ihts=iht
      if (rendt.lt.rendx) go to 70
!
      if (rend.gt.rendx) then
        begin=.true.
        rstrtx=rstrti
        rendx=rend
        go to 30
      endif
!
      go to 200
!
!** integrate along vertical ray path
!
  160 htmax=MAX(p1,p2)+ht-rht
      htmax=MIN(htmax,htrng(6))
!
      iht=ihts
      psum=zero
  170 height=ht+stepd2(iht)
      xe=zero
      if (height.ge.htrng(1).and.height.le.htrng(6)) xe=elden(height)
      psum=psum+xe*step(iht)
      if (height.lt.htrng(iht)) then
        if (iht.gt.1) iht=iht-1
      elseif (height.gt.htrng(iht+1)) then
        if (iht.lt.5) iht=iht+1
      endif
      ht=ht+step(iht)
      if (ht.lt.htmax) go to 170
      if (ht.gt.htmax) then
        psum=psum-xe*(ht-htmax)
      endif
      stec=stec+psum
!
!200  stec=stec*elctom
  200 continue
      return
      END
