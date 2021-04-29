!$ELECTP
      SUBROUTINE ELECTP (AA,MJD,FDAY,RSTA,RSUN)
!********1*********2*********3*********4*********5*********6*********7**
! ELECTP                                         PGMR - R.WILLIAMSON
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
! electp computes parametrs describing the vertical electron profile
! according to the International Reference Ionosphere (IRI) 90 Model
! warning: IRI working units of length are km.
      INTEGER season
      DOUBLE PRECISION modip,magbr
      LOGICAL ext,schalt,f1reg
!     logical gulb0,old79
!     common/const/
! debug
      COMMON/CKOUNT/KNTBLK,KSEGMN,KNTOBS,KZROBS
! debug
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/EPROF/HMF2,RNMF2,HMF1,RNMF1,B0P,B1,C1ION,HZ,T,HST,EION(4), &
     &        HME,RNME,HEF,HMD,RNMD,HDX,D1,XKK,FP30,FP3U,FP1,FP2,       &
     &        BETA,ETAION,DELTAI,ZETAIO,X0,EPTRC1,EPTRC2,               &
     &        ALNMF2,ALNME,ALNMD,RNIGHT
      COMMON/CORA04/KABIAS,KABSTR,KABSTP,KCBIAS,KCBSTR,KCBSTP,          &
     &       KRNM  ,KRKM  ,KRKJ  ,KRIJ  ,KRKPM ,                        &
     &       KRRNM ,KRRKM ,KRRKJ ,KRRIJ ,KRRKPM,                        &
     &       KURNM ,KURKM ,KURKJ ,KURIJ ,KURKPM,                        &
     &       KPRRNM,KPRRKM,KPRRKJ,KPRRIJ,KPRRKP,                        &
     &       KPMPXI,KPMPXE,KRA   ,KRELV ,KUHAT ,KWORK ,KTMPCR,KU    ,   &
     &       KS0   ,KS1   ,KXTP  ,KPXSXP,KPXDXP,KXUTDT,KRPOLE,KPXPOL,   &
     &       KXDPOL,KOBBUF,KOBSC ,KRESID,KSIGMA,KRATIO,KELEVS,          &
     &       KPMPA ,KPXSLV,KTPRTL,                                      &
     &       KFRQOF,KXYZOF,KFRQST,KFRQSA,KSTDLY,KSADLY,KXYZCG,KSCMRA,   &
     &       KEBVAL,KEBSIG,KEBNAM,KBTWB ,KTBTWA,KBTWD ,KPMPE ,          &
     &       KFSEB1,KFSEB2,KSTAMT,KPXSPA,KDIAGN,KOBOUT,KGRLT1,KGRLT2,   &
     &       KGRLT3,KGRLT4,KGRLT5,KPSTAT,KRSUNS,KCHEMR,KCHMOR,KEPOSR,   &
     &       KCHEMT,KCHMOT,KEPOST,KCHCBS,KCPOSS,KSCRTM,KXSTT ,KXSTR ,   &
     &       KXSS  ,KRANGT,KRANGR,KCHB1 ,KCHB2 ,KCHBV1,KCHBV2,KCHEB ,   &
     &       KSSTFQ,KSSTCC,KSSTSS,KSSTWT,KRLCOR,KWVLBI,KQUINF,KBRTS ,   &
     &       KBRTSV,KLRARC,KXEPBF,KXEPBR,KPRCBD,KPRTBD,KXPMPA,KXL,      &
     &       KXSIG ,KXEDTS,KXALO1,KXALO2,KXYOF2,KDLATF,KDLATS,KDLONF,   &
     &       KDLONS,KPF   ,KPS   ,                                      &
     &       KXOBSV,KXOBSW,KXEDSW,                                      &
     &       KXSIGW,KPSF  ,KDNUM ,KCNUM ,KFCOR ,KCOSAR,KSINAR,KSABIA,   &
     &       KSBTM1,KSBTM2,KYAWBS,KVLOUV,KACMAG,KOBSTR,                 &
     &       KPRL1 ,KPRL2, KRL1  ,KT1SE ,KTCP  ,                        &
     &       KRATDR,KFQT1S,KFQT1E,KT1STR,KFTTSE,KFRSTR,KFRRAT,KSV1  ,   &
     &       KSV2  ,KTSLOV,                                             &
     &       KGLGR1,KGLGR2,KGLFR1,KGLFR2,                               &
     &       KARGR1,KARGR2,KARFR1,KARFR2,                               &
     &       KRDS1L,KFT1AV,KDFQP ,KFREQ1,KFREQ3,KSAVD1,KSAVD2,          &
     &       KANTOU,KFM3CF,KF2CF,KTMG,KLTMG,KX2TIM,KX2OBS,KXRNDX,KX2SCR,&
     &       KALTWV,KXXBM ,KX2PAR,KATIME,KPMPAT,KPMATT,KX2PAT,          &
     &       KPXEXI,KPXEPA,KPV,KPXEP2,KX2COF,KACOF2,KACOF3,KBCOF,KBCOF2,&
     &       KDDDA ,KX2AUX,KX2VPR,KX2VPA,KEXTRA,KVARAY,KATROT,KATPER,   &
     &       KLTAR ,KXHOLD,KANTBL,KPHC  ,KOFDRV,KGNAME,KGRSIZ,KGRCNT,   &
     &       KGRDAT,KACCDT,KCTBTI,KCTBWE,KCTCTM,KANTUV,KANTOR,KW1PU ,   &
     &       KPYSQH,KSIGSP,KXYOF3,KXVTM1,KXDIST,KXDST0,KTMSE ,KDSDP ,   &
     &       KEXCST,KEXCDT,KEXCGX,KEXCGY,KEXCGZ,KIMNDX,KIMOBS,KIMTIM,   &
     &       KIMSAT,KM2VPA,KDCDD ,KDWNWT,KDPOR ,KC2PAR,KBOUNC,KBPART,   &
     &       NXCA04
      COMMON/CTRIG/CHA,SHA,CBG,SBG,CLG,SLG,SMODIP
      dimension AA(1)
      dimension  f(3),xdels(4),dnds(4),rsta(3),rsun(3)
!     dimension  b0b1(5)
      dimension ary(37)
      equivalence (ary(1),hmf2)
! debug
      dimension mo(13,2)
      data      mo     /                                                &
     &  0,31,59,90,120,151,181,212,243,273,304,334,365,                 &
     &  0,31,60,91,121,152,182,213,244,274,305,335,366   /
      data lfirst/.true./
      data lfstreg/.true./
      data lfsthz/.true./
! debug
!  set smodip for gammax:
!
      data      xdels   /3*5.,10./,      dnds   /.016,.01,2*.016/
!    &      b0b1  /.755566,.778596,.797332,.812928,.826146/
!
      hpole(vday,vnite,epstdf) = vnite + (vday-vnite)*epstdf
!
! calculate magnetic field values
!
      alt=300.
      call fieldg (alt,bx,by,bz,coslat)
!
! dip latitude (magbr) and modified dip (modip) in degrees
!
      bxy=SQRT(bx**2+by**2)
!     f=sqrt(bx**2+by**2+bz**2)

      dip=ATAN2(bz,bxy)
      magbr=ATAN2(0.5*bz,bxy)
!
      dipdiv=dip/SQRT(dip*dip+coslat)
      smodip=dipdiv
      modip=ASIN(dipdiv)
!
!     magbr=magbr/umr
!     modip=modip/umr
      magbr=magbr/degrad
      modip=modip/degrad
!
! initialize variables for debug printout - may not always be set
!     rrrr=0.
!     rathh=0.
! **** cheat !
!     rssn=100
!     rig=100
!     xm3000=2.65
!     fof2=10.2
! debug
!     15384 = 45384-30000
!      IDAY0= 45384-INT(TMGDN2+0.001)
!
      IDAY0= 45384-INT(TMGDN2+0.001)
      idaynr=mjd-IDAY0
!     idayt=mjd-15384
      iy=idayt*100/36525
      idayt=idayt-36525*iy/100
      iy=iy+1
      if (idayt.le.0) then
        iy=iy-1
        idayt=366
      endif
      iyind=1
      if (MOD(iy,4).eq.0.and.MOD(iy,400).ne.0) iyind=2
      do 11 ix=2,13
      if(mo(ix,iyind).ge.idayt) go to 22
   11 continue
      i=13
   22 month=i-1
      iym=iy*100+month
! debug
! **** end cheat - cheat replaces call to fof2c below
!
      call fof2c(mjd,fday,idaynr,rssn,rig,fof2,xm3000,aa(kfm3cf),       &
     &aa(kf2cf))
!
! calculation of season (summer=2, winter=4)
!
      season=(idaynr+45)/92
      if(season.lt.1) season=4
      nseson=season
! check latitude:
      if (rsta(3).le.0.0) then
        season=season-2
        if(season.lt.1) season=season+4
      endif
!
! calculation of solar zenith angle cosines - actual and at high noon;
! set rnight and quantities dependant on sun rise/set angles
!
      call zenith (rsta,rsun,chicos,chicmx,rnight,epstdf)
!
!     chi=acos(chicos)/umr
      chi=ACOS(chicos)/degrad
!
! calculation of electron density parameters
!
!   e-region peak
!
      hme=105.0
      cov=63.75+rssn*(0.728+rssn*0.00089)
!
      foe=foeedi(cov,chi,chicmx,coslat)
      rnme=1.24D10*foe*foe
      alnme=LOG(rnme)
!
      dela=4.32
      absmdp=ABS(modip)
      if(absmdp.ge.18.) dela=1.0+EXP(-(absmdp-30.0)/10.0)
!
!  parameters below e
!
!  d-region parameter
!
      zero=0.
      rnmd=xmded(chicos,rssn,4.0D8+zero)
      alnmd=LOG(rnmd)
!
      hmd=hpole(81.0+zero,88.0+zero,epstdf)
      f(1)=hpole(0.02+0.03/dela,0.05+zero,epstdf)
      f(2)=hpole(4.6+zero,4.5+zero,epstdf)
      f(3)=hpole(-11.5+zero,-4.0+zero,epstdf)
      fp1=f(1)
      fp2=-fp1*fp1/2.0
      fp30=(-f(2)*fp2-fp1+1.0/f(2))/(f(2)*f(2))
      fp3u=(-f(3)*fp2-fp1-1.0/f(3))/(f(3)*f(3))
      hdx=hmd+f(2)
      x=hdx-hmd
      xdx=rnmd*EXP(x*(fp1+x*(fp2+x*fp30)))
      dxdx=fp1+x*(2.0*fp2+x*3.0*fp30)
      x=hme-hdx
      xlogr=LOG(xdx/rnme)
      xkk=-dxdx*x/xlogr
      d1=-xlogr/x**xkk
!
!  e and valley-region parameters
!
      xdel=xdels(season)/dela
      dndhbr=dnds(season)/dela
      hdeep=hpole(10.5/dela,28.+zero,epstdf)
      width=hpole(17.8/dela,45.+22./dela,epstdf)
      depth=hpole(xdel,81.+zero,epstdf)
      dlndh=hpole(dndhbr,.06+zero,epstdf)
!
      hef=hme + width
      if(depth.lt.1.0) then
        hef=hme
      else
        if(rnight.ge.1.0) depth=-depth
        call tal(hdeep,depth,width,dlndh,ext,eion)
        if (ext) then
          hef=hme
        endif
      endif
!
!  f1-region parameters
!
      f1reg=.false.
      hmf1=0.
      rnmf1=0.
      c1ion=0.
      if(rnight.le.0.01) then
        fof1=fof1ed(magbr,rssn,chi,chicos)
        if(fof1.ge.1.D-3) then
          f1reg=.true.
          c1ion=.09+.11/dela
          rnmf1=1.24D10*fof1*fof1
        endif
      endif
!
! f2-region parameters
!
      hmf2=hmf2ed(magbr,rssn,fof2/foe,xm3000)
      rnmf2=1.24D10*fof2*fof2
      alnmf2=LOG(rnmf2)
!
!  interpolation for b0p out of array b0f
!
      b0p = b0tab(epstdf,nseson,rssn,modip)
!
!  define b0cnew to make compiler happy: is wrong for option.
!
!       b0cnew=b0p*b0b1(1)
!
! match bottomside and e-region valley profiles:
! search for hmf1 - possibly adjust b1
!
      hz=hmf2
      b1=3.0
!
  100 if(.not.f1reg) goto 130
      lxe3=.false.
      xe2h=xe2e3(hef,lxe3)
      eps=0.001
      call regfal(hef,hmf2,xe2h,rnmf2,eps,rnmf1,schalt,hmf1,lxe3)
      if(.not.schalt) goto 130
!
! change b1 and try again
!
      if(b1.gt.4.5) goto 120
!
  110 b1=b1+0.5
!     if(gulb0) then
!       ib1=int(b1*2.-5.)
!       b0p=b0cnew/b0b1(ib1)
!     endif
      goto 100
!
! omit f1 feature
!
  120 if(lxe3) go to 170
      hmf1=0.
      rnmf1=0.
      c1ion=0.0
      b1=3.
      f1reg=.false.
!
! search for hst [ne3(hst)=rnme]
!
  130 lxe3=.true.
      if(f1reg) then
        hf1=hmf1
        xf1=rnmf1
      else
        rathh=0.5
  140   hf1=hef+(hmf2-hef)*rathh
        xf1=xe2e3(hf1,lxe3)
        if(xf1.lt.rnme) then
          rathh=rathh+.1
          go to 140
        endif
      endif
!
      h=hf1
      deh=10.
      xxmin=xf1
      hhmin=hf1
  150 h=h-deh
      if(h.lt.hef) then
        h=h+2*deh
        deh=deh/10.
        if(deh.lt.1.) go to 160
      endif
      xe3h=xe2e3(h,lxe3)
      if(xe3h.lt.xxmin) then
        xxmin=xe3h
        hhmin=h
      endif
      if(xe3h.gt.rnme) goto 150
!
      eps=0.001
      call regfal(h,hf1,xe3h,xf1,eps,rnme,schalt,hst,lxe3)
!
      str=hst
      if(.not.schalt) goto 190
  160 continue
!
      if(xxmin/rnme.ge.1.3) goto 170
!
      if (f1reg.and.b1.lt.4.5) go to 110
!
! assume linear interpolation between hz and hef
!
  170 rrrr=0.5
  180 hz=hhmin+(hf1-hhmin)*rrrr
      xnehz=xe2e3(hz,lxe3)
      if(xnehz-rnme.lt.0.001) then
        rrrr=rrrr+.1
        goto 180
      endif
      t=(xnehz-rnme)/(hz-hef)
      hst=-333.
      goto 200
!
! calculate hz, d and t
!
  190 hz=(hst+hf1)/2.0
      d=hz-hst
      t=d*d/(hz-hef-d)
  200 if(.not.f1reg) hmf1=hz
!
! topside profile parameters
!
! harmonized Bent model admitting
! variability of global parameter etaion,zetaio,beta,deltai with
! geom. latitude, smoothed solar flux and critical frequency

      call mlat(cmlat,smlat)
!
      cos2=cmlat**2
!     if(old79) then
!       eta1=-0.0070305*cos2
!     else
!       dmlat=asin(smlat)/umr
        dmlat=ASIN(smlat)/degrad
        ex=EXP(-dmlat/15.)
        eta1=-.08*ex/(ex+1.)**2
!     endif
!
!     covr=63.75+rig*(0.728+rig*0.00089)
      covr=63.75+rssn*(0.728+rssn*0.00089)
      flu=(covr-40.0)/30.0
!
      etaion=0.058798+eta1+flu*(-0.014065+0.0069724*cos2)+              &
     &(0.0024287+0.0042810*cos2-0.00015280*fof2)*fof2
      zetaio=0.078922-0.0046702*cos2+flu*(-0.019132+0.0076545*cos2)+    &
     &(0.0032513+0.0060290*cos2-0.00020872*fof2)*fof2
      beta=-128.03+20.253*cos2+flu*(-8.0755-0.65896*cos2)+(0.44041      &
     &+0.71458*cos2-0.042966*fof2)*fof2
      z=EXP(94.45/beta)
      z1=z+1
      z2=z/(beta*z1*z1)
      deltai=(etaion/z1-zetaio/2.0)/(etaion*z2+zetaio/400.0)
!
!debug
!      write(6,6900) idaynr,dmlat,modip,magbr,chi,chicmx,season,rnight
!6900 format(' idaynr,dmlat,modip,magbr = ',i4,3f10.5/
!    . ' chi,chicmx,season,rnight = ',2f10.5,i3,1x,i1)
!      write(6,6901)
!    . rssn,fof2,xm3000,foe,rnme,hme,rnmf2,hmf2,fof1,rnmf1,hmf1,
!    . etaion,zetaio,beta,deltai,hdeep,width,depth,dlndh,eion,
!    . hef,hdx,rnmd,hmd,f,d1,xkk,fp30,fp3u,fp1,fp2,
!    . b0p,b1,ca1ion
!6901 format(' rssn,fof2,xm3000,foe,rnme,hme = ',4f10.5,e15.8,f10.5/
!    . ' rnmf2,hmf2,fof1,rnmf1,hmf1 = ', e15.8,2f10.5,e15.8,f10.5/
!    . ' etaion,zetaio,beta,deltai,hdeep,width,depth,dlndh = ',8f10.5/
!    . ' eion(1),eion(2),eion(3),eion(4) = ', 4e15.8/
!    . ' hef,hdx,rnmd,hmd = ', 2f10.5,e15.8,f10.5/
!    . ' f(1),f(2),f(3),d1,xkk,fp30,fp3u,fp1,fp2 =',9f10.5/
!    . ' b0p,b1,c1ion = ',3f10.5)
!      write(6,6902)
!    . b0p,b1,c1ion,hz,t,hst,rathh,rrrr
!6902 format(' b0p,b1,c1ion,hz,t,hst,rathh,rrrr = ',8f10.5)
!debug
!
!  set topside constant functions
!
      x0 = 300. - deltai
!     eptrc1 = eptr(x0,beta,394.5)
!     eptrc2 = eptr(x0,100.,300.0)
!
      eptrc1=LOG(1.+EXP((x0-394.5)/beta))
      eptrc2=LOG(1.+EXP((x0-300.0)/100.))
!
      hmf2c=hmf2
      return
      END
