!$TDIURNAL
      FUNCTION TDIURNAL(DLO,TMIN,TMAX)
!********1*********2*********3*********4*********5*********6*********7**
! TDIURNAL         00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!...  function to compute surface temperature from site-sun longitude
!...  difference and daily maximum and minimum surface temperature
!
!   ------  ---  ---   ------------------------------------------------
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  function to compute surface temperature from site-sun longitude
!...  difference and daily maximum and minimum surface temperature
      pi180 = ATAN(1.D0)/45.D0
!...  factors for site-sun longitude range -180 to -102.19
      cosfac = -1.D0
      period = 0.615132D0
      offset = 102.19D0
!...  factors for range -102.19 to -48.6
      If (dlo .gt. -102.19D0)period = 1.679418D0
!...  factors for range -48.6 to +43.8
      If (dlo .gt. -48.6D0)then
        cosfac = 1.D0
        period = 0.974026D0
        offset = -43.8D0
      Endif
!...  factors for range +43.8 to +111.5
      If (dlo .gt. 43.8D0)period = 1.329394D0
!...  factors for range +111.5 to +180
      If (dlo .gt. 111.5D0) then
        cosfac = -1.D0
        period = 0.615132D0
        offset = -257.81D0
      Endif
!...  diurnal shape factor
      dT = .5D0*(1.D0 + cosfac*COS(pi180*period*(dlo + offset)))
      dT = (-0.3455D0*dT + 1.3455D0)*dT
!...  surface temperature based on Tmax, Tmin and diurnal shape
      Tdiurnal = Tmin + (Tmax - Tmin)*dT
      Return
      END
