!$FOF2C
      SUBROUTINE FOF2C(MJD,FDAY,IDAYNR,RSSN,RIG,FOF2,XM3000,FM3CF,F2CF)
!********1*********2*********3*********4*********5*********6*********7**
! FOF2C                                          PGMR - R.WILLIAMSON
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
      COMMON/IONO2E/IONNDX(12),IONRZ(12),IONOYM(12),IONYMS,IONYME,IURSI,&
     &              INTERP,NXIONO
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      dimension  fm3cf(441,2,1),f2cf(988,2,1)
      dimension  rgz(3),rsn(3)
!                988==(13,76); 441==(9,49)
      dimension  ff0(988),ff0n(988),ff0t(988),xm0(441),xm0n(441),       &
     &           xm0t(441)
      dimension  f2(988,2),f2n(988,2),fm3(441,2),fm3n(441,2)
      dimension mo(13,2)
      data      mo     /                                                &
     &  0,31,59,90,120,151,181,212,243,273,304,334,365,                 &
     &  0,31,60,91,121,152,182,213,244,274,305,335,366   /
!
      data  montho,nmono,icurd,icurym /4*0/
!
      INTEGER qm(7),qf(9)
      data qf/11,11,8,4,1,0,0,0,0/,qm/6,7,5,2,1,0,0/
!
!
!
!     if(mjd.eq.icurd) go to 140
!     icurd=mjd
!     fdayp=-1.
!
!     15384 = 45384-30000
!      IDAY0= 45384-INT(TMGDN2+0.001)
!
      IDAY0= 45384-INT(TMGDN2+0.001)
      idaynr=mjd-IDAY0
!     idaynr=mjd-15384
      iy=idaynr*100/36525
      idaynr=idaynr-36525*iy/100
      iy=iy+1
      if (idaynr.le.0) then
        iy=iy-1
        idaynr=366
      endif
! DEPDEP NEW
      if(mjd.eq.icurd) go to 140
      icurd=mjd
      fdayp=-1.
!
      iyind=1
      if (MOD(iy,4).eq.0.and.MOD(iy,400).ne.0) iyind=2
      do 10 i=2,13
      if(mo(i,iyind).ge.idaynr) go to 20
   10 continue
      i=13
   20 month=i-1
!
      iym=iy*100+month
      if (iym.eq.icurym) go to 40
      icurym=iym
!
      idayc=(mo(month,iyind) + mo(month+1,iyind))/2
!
      do 21 j=1,12
         if(ionoym(j).eq.iym) then
            do 31 jj=1,3
               rsn(jj)=DBLE(ionrz(j+jj-2))/10.
               rgz(jj)=DBLE(ionndx(j+jj-2))/10.
   31 continue
         endif
   21 continue
!
   40 rr = rsn(2)
      rg=rgz(2)
! COMPUTE NUMBER OF MONTHS  SPANNED BY IONOSPHERIC COEFFICIENTS
      NUMYRS=IONYME/100-IONYMS/100
      NMNTHS=12*NUMYRS - MOD(IONYMS,100) + MOD(IONYME,100) + 1
! COMPUTE ACTUAL NUMBER OF MONTHS OF IONOSPHERIC COEFFICIENT DATA
! STORED IN DYNAMIC ARRAYS WHERE COEFFICIENT DATA FOR THE SAME
! MONTH IN DIFFERENT YEARS IS IDENTICAL
      NMDATA=NMNTHS
      IF(NMNTHS.GT.12) NMDATA=12
      mnth=MOD(ionyms,100)
      imnth=1
      do 22 i=1,nmdata
      if(mnth.ne.month) then
         mnth=mnth+1
         imnth=imnth+1
         if(mnth.gt.12) mnth=1
      endif
   22 continue
      if(idaynr.ge.idayc) then
        rrn=rsn(3)
        rgn=rgz(3)
        nmonth=month+1
        nimnth=imnth+1
        if (nmonth.gt.12) then
          nmonth=1
          idayn=mo(13,iyind) + 15
        else
          idayn=(mo(nmonth,iyind) + mo(nmonth+1,iyind))/2
        endif
      else
        rrn=rsn(1)
        rgn=rgz(1)
        nmonth=month-1
        nimnth=imnth-1
        if (nmonth.lt.1) then
          nmonth=12
          idayn=-16
        else
          idayn=(mo(nmonth,iyind) + mo(nmonth+1,iyind))/2
        endif
      endif
!
      if((month.eq.montho).and.(nmonth.eq.nmono)) go to 140
      nmono = nmonth
!
      if(month.eq.montho) go to 50
      montho=month
      do 23 j=1,2
      do 24 i=1,441
         fm3(i,j)=fm3cf(i,j,imnth)
         fm3n(i,j)=fm3cf(i,j,nimnth)
   24 continue
   23 continue
      do 25 j=1,2
      do 26 i=1,988
         f2(i,j)=f2cf(i,j,imnth)
         f2n(i,j)=f2cf(i,j,nimnth)
   26 continue
   25 continue
!
! linear interpolation in solar activity
!
   50 continue
  110 rr2=rg/100.
      rr2n=rgn/100.
!
!                988==(13,76); 441==(9,49)
!
      do 120 k=1,988
      ff0n(k)=f2n(k,1) + (f2n(k,2)-f2n(k,1))*rr2n
  120 ff0(k)=f2(k,1) + (f2(k,2)-f2(k,1))*rr2
!
! check following statement - r is uninterpolated ionosphere index
      rr2=rr/100.
      rr2n=rrn/100.
      do 130 k=1,441
      xm0n(k)=fm3n(k,1) + (fm3n(k,2)-fm3n(k,1))*rr2n
  130 xm0(k)=fm3(k,1) + (fm3(k,2)-fm3(k,1))*rr2
!
  140 continue
      if(fday.eq.fdayp) go to 160
      fdayp=fday
      dspan=idayn-idayc
      fracm=(idaynr-idayc)/dspan
      do 150 k=1,988
  150 ff0t(k)=ff0(k)+(ff0n(k)-ff0(k))*fracm
      do 155 k=1,441
  155 xm0t(k)=xm0(k)+(xm0n(k)-xm0(k))*fracm
!
      rssn=rr+(rrn-rr)*fracm
      rig =rg+(rgn-rg)*fracm
!
! note: gammax replaces gamma1, first argument controls initialization
!       /ctrig/ contains input for sine,cosine of time,lat,long,modip
!
  160 fof2  =gammax(0,6,qf,9,76,13,988,ff0t)
      xm3000=gammax(1,4,qm,7,49,9,441,xm0t)
      return
!
!     fof2=0.
!     xm3000=0.
!     return
!
      END
