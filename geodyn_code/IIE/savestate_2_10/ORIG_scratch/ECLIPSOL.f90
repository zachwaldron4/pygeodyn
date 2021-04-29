!$ECLIPSOL
      SUBROUTINE ECLIPSOL(rsat,dsun,epsilonr,area,areafrac)
!
! This subroutine computes the fraction "areafrac" of the solar disk
! that is visible at any time.
! It is usually 1, but during eclipses it is less than 1.
! When the sun is complete eclipsed, "areafrac" is 0.
! The actual solar irradiance at 1 AU is assumed to be proportional
! to areafrac: actual irradiance = s0*areafrac, where s0
! is the full sun's irradiance at 1 AU.
! The apparent radius of the Earth, as seen from the satellite,
! is much larger than that of the Sun, so the arc of the Earth's
! limb cutting across the sun during a partial eclipse is approximated
! with a straight line segment to simplify calculations.
!
! Input arguments:
!
! Rsat = distance satellite to geocenter.
! Dsun = distance satellite to geocenter.
! epsilonr = gocenter-satellite-heliocenter angle (radians).
!
! Output arguments:
!
! area = area of visible portion of the sun disk, normalized
! so its radius is one and its total area is equal to pi.
! areafrac = ratio of visible area of sun disk to total area of sun disk.
!
! Oscar L. Colombo, December 2012.
!
      implicit DOUBLE PRECISION (a-h,o-z)
      save
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      data rsun/695950000.D0/,re/6378137.D0/
!
      sundisk = rsun/dsun    ! Angle span by the sun radius at actual dis
!                              from the satellite to the sun, in radians.
!
      earthdisk = ASIN(re/rsat) ! " " " " Earth, as seen from satellite,

!!!!!!
      epsil = ABS(epsilonr/sundisk)
      earth = earthdisk/sundisk
      eme = epsil-earth
      h = 1.D0-eme
      if(h.gt.1.D0) h = 2.D0-h
      area = pi
      if(ABS(eme).lt.1.D0) then ! Sun is partially eclipsed.
!
! Calculate the area of a segment of a disk of unit radius
! given its angular height h < 1:
!
      d = 1.D0-h
      halftheta = ACOS(d)
      slice = halftheta ! Area of the wedge of the unit radius sun disk
!                         with a central angle of theta (= 2*halftheta) radians.
      atriang = d*SIN(halftheta)
      area = slice-atriang
      if(eme.gt.0) area = pi-area
      elseif(eme.lt.0.D0) then ! Sun is totally eclipsed.
      area = 0.D0
      endif
      areafrac = area/pi
!
      return
      end
