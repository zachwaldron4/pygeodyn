!$FOEEDI
      FUNCTION FOEEDI(COV,XHI,COSZMX,CLAT)
!********1*********2*********3*********4*********5*********6*********7**
! FOEEDI                                         PGMR - R.WILLIAMSON
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
!-------------------------------------------------------
! calculates foe/mhz by the edinburgh-method.
! input: mean 10.7cm solar radio flux (cov), geographic
! latitude (xlati/deg), solar zenith angle (xhi/deg and
! xhim/deg at noon).
! reference:
!       kouris-muggeleton, ccir doc. 6/3/07, 1973
!       trost, j. geophys. res. 84, 2736, 1979 (was used
!               to improve the nighttime varition)
! d.bilitza--------------------------------- august 1986.
! compute using available cosines
! r.williamson---------------------------- november 1995.
!     common/const/
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
! variation with solar activity (factor a)
      a=1.0+0.0094*(cov-66.0)
! variation with noon solar zenith angle (b) and with latitude (c)
! sl is cos(lat)
        sl=ABS(clat)
!  check latitude lt 32 degrees:
        if(sl.gt.0.84804810) then
                sm=-1.93+1.92*sl
                c=23.0+116.0*sl
        else
                sm=0.11-0.49*sl
                c=92.0+35.0*sl
        endif
! chi at high noon restricted to 89.999 --
! coszmx always gt 1.7453e-5
        b=1.7453D-5
        if(coszmx.gt.b) b=coszmx
        b=b**sm
! variation with solar zenith angle (d)
!  check latitude gt 12 degrees:
        if(sl.lt.0.97814760) then
                sp=1.2
        else
                sp=1.31
        endif
! adjusted solar zenith angle during nighttime (xhic)
      xhic=xhi-3.*LOG(1.+EXP((xhi-89.98)/3.))
!     d=cos(xhic*umr)**sp
      d=COS(xhic*degrad)**sp
! determine foe**4
      r4foe=a*b*c*d
! minimum allowable foe (sqrt[smin])
      smin=0.121+0.0015*(cov-60.)
      smin=smin*smin
      if(r4foe.lt.smin) r4foe=smin
!     foeedi=r4foe**0.25
      r2foe=SQRT(r4foe)
      foeedi=SQRT(r2foe)
      return
      END
