!$SUNVEC
      SUBROUTINE SUNVEC (MJD,FDAY,RSUN,RASUN)
!********1*********2*********3*********4*********5*********6*********7**
! SUNVEC                                         PGMR - R.WILLIAMSON
!
! FUNCTION:
!
!       s/r to calculate the unit vector pointing towards the sun and
!       to provide the right ascension of the mean sun for thetag calc.
!       short period solar terms are based on Newbern Smith's algorithm(
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
!     common/const/
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      dimension rsun(3)
!
! amplitudes of fourier coefficients  --  1955 epoch.................
      data    p1,p2,p3,p4,p6 /                                          &
     &  0.017203534,0.034407068,0.051610602,0.068814136,0.103221204 /
!
!    15019 = 45019.5-30000.d0
!
!     dt=mjd+fday-15019.5d0
      dt=mjd+fday-(45019.5D0-TMGDN2)
!
!  drasun is Ru (thetag0-pi)
      drasun=4.881528547307D0+0.017202791266007D0*dt+5.064090D-15*dt**2
      drasun=MOD(drasun/degrad,360.D0)
      if(drasun.lt.0.D0) drasun=drasun+360.D0
      rasun=drasun
!
! 15384 = 45384 (1901.0.0)-30000
!
!     iday=mjd-15384
      iday=mjd-(45384-INT(TMGDN2+0.001))
      iy=iday*100/36525
      iday=iday-36525*iy/100
      iy=iy+1
      if (iday.le.0) then
        iy=iy-1
        iday=366
      endif
!
!  compute short period sun from 1980
!
        td = iday+fday
!       write(6,*)'td,iday',td,iday
! time of equinox for 1980...........................................
        te = td + 0.9369
!
! declination of the sun..............................................
        dcl = 23.256 * SIN(p1*(te-82.242)) + 0.381 * SIN(p2*(te-44.855))&
     &      + 0.167 * SIN(p3*(te-23.355)) - 0.013 * SIN(p4*(te+11.97))  &
     &      + 0.011 * SIN(p6*(te-10.41)) + 0.339137
!       dc = dcl * umr
        dc = dcl * degrad
!       write(6,*)'dc,dcl,degrad',dc,dcl,degrad
!
! the equation of time................................................
        tf = te - 0.5
        eqt = -7.38*SIN(p1*(tf-4.)) - 9.87*SIN(p2*(tf+9.))              &
     &      + 0.27*SIN(p3*(tf-53.)) - 0.2*COS(p4*(tf-17.))
        et = eqt/4.
!
!     phi=(rasun-et)*umr
      phi=(rasun-et)*degrad
!
      cdec=COS(dc)
      sdec=SIN(dc)
      cra=COS(phi)
      sra=SIN(phi)
!
      rsun(1)=cdec*cra
      rsun(2)=cdec*sra
      rsun(3)=sdec
!
      return
      END
