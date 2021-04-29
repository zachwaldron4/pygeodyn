!$B0TAB
        FUNCTION B0TAB (EPSTDF, ISEASON, R, ZMODIP)
!********1*********2*********3*********4*********5*********6*********7**
! B0TAB                                          PGMR - R. WILLIAMSON
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
!-----------------------------------------------------------------
! interpolation procedure for bottomside thickness parameter b0.
! array b0f(ilt,iseason,ir,ilati) distinguishes between day and
! night (ilt=1,2), four seasons (iseason is northern season with
! iseason=1 northern spring), low and high
! solar activity (ir=1,2), and low and middle modified dip
! latitudes (ilati=1,2). in the data statement the first value
! corresponds to b0f(1,1,1,1), the second to b0f(2,1,1,1), the
! third to b0f(1,2,1,1) and so on.
! june 1989 --------------------------------------- dieter bilitza
!
! corrected to include a smooth transition at the modip equator
! and no discontinuity at the equatorial change in season.
! jan 1993 ---------------------------------------- dieter bilitza
!
! computational efficiency changes, etc
! nov 1995 -------------------------------------------r williamson
      dimension bfr(2,2,2),bfd(2,2),zx(4),g(5),eptrdf(4),eptrc(4)
      dimension b0f(2,4,2,2)
      data      b0f/114.,64.0,134.,77.0,128.,66.0,75.,73.0,             &
     &              113.,115.,150.,116.,138.,123.,94.,132.,             &
     &              72.0,84.0,83.0,89.0,75.0,85.0,57.,76.0,             &
     &              102.,100.,120.,110.,107.,103.,76.,86.0/
      data    zx/-45.,-18.,18.,45./
      data    one/0./
!
!
      hpole(vday,vnite,epstdf) = vnite + (vday-vnite)*epstdf
      rintrp(vlo,vhi,dr) = vlo + (vhi-vlo)*dr
!
      if(one.ne.1.) then
        one=1.
        dd=5.
        eptrc(1)=LOG(1.+EXP((-zx(1)/dd)))
        eptrc(2)=LOG(1.+EXP((-zx(2)/dd)))
        eptrc(3)=LOG(1.+EXP((-zx(3)/dd)))
        eptrc(4)=LOG(1.+EXP((-zx(4)/dd)))
      endif
!
      jseason=iseason+2
      if(jseason.gt.4) jseason=jseason-4
      dr=(r-10.)/90.
!
      do 7035 isl=1,2
!
! interpolation low/high rz12: linear from 10 to 100
!
      bfr(1,1,isl)=rintrp(b0f(1,iseason,1,isl),b0f(1,iseason,2,isl),dr)
      bfr(2,1,isl)=rintrp(b0f(2,iseason,1,isl),b0f(2,iseason,2,isl),dr)
      bfr(1,2,isl)=rintrp(b0f(1,jseason,1,isl),b0f(1,jseason,2,isl),dr)
      bfr(2,2,isl)=rintrp(b0f(2,jseason,1,isl),b0f(2,jseason,2,isl),dr)
!
! interpolation day/night with transitions at sax (sunrise) and sux (sun
!
      bfd(1,isl) = hpole(bfr(1,1,isl),bfr(2,1,isl),epstdf)
      bfd(2,isl) = hpole(bfr(1,2,isl),bfr(2,2,isl),epstdf)
 7035 continue
!
      g(1) = 0.
      g(2) = ( bfd(2,1) - bfd(2,2) ) / 27.
      g(3) = ( bfd(1,1) - bfd(2,1) ) / 36.
      g(4) = ( bfd(1,2) - bfd(1,1) ) / 27.
      g(5) = 0.
!
! interpolation low/middle modip with transition at 30 degrees modip
!
      eptrdf(1)=LOG(1.+EXP((zmodip-zx(1))/dd))-eptrc(1)
      eptrdf(2)=LOG(1.+EXP((zmodip-zx(2))/dd))-eptrc(2)
      eptrdf(3)=LOG(1.+EXP((zmodip-zx(3))/dd))-eptrc(3)
      eptrdf(4)=LOG(1.+EXP((zmodip-zx(4))/dd))-eptrc(4)
!
      bb0 = ( bfd(1,1) + bfd(2,1) ) / 2.
!
      sum= 0.
      do 10 i=1,4
   10 sum=sum+(g(i+1)-g(i))*eptrdf(i)
!
      b0tab = sum*dd + bb0

      return
      END
