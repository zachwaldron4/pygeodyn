!$ALBMS
      Function ALBMS(als,alat)
!********1*********2*********3*********4*********5*********6*********7**
! ALBMS                00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: CALCULATE SURFACE ALBEDO AS THE FUNCTION OF AREOCENTRIC LONG
!           ALS AND LATITUDE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   als      I    S
!   alat     I    S
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  albedo of surface as a function of areocentric lon. of sun Ls
!...  and latitude (both in degrees)
      pi180 = ATAN(1.)/45.
!...  albedo of non-polar surface
      al = 0.32 - 0.12*COS(pi180*alat)
!...  factors for North polar hood cloud
      alsc = 280.
      radc = 22.
!...  factors for South polar hood cloud
      If (alat .lt. 0.)then
        alsc = 60.
        radc = 27.
      Endif
!...  Polar hood cloud semi-diameter
      cldrad = radc*(1. + COS(pi180*(als - alsc)))
!...  albedo of polar hood cloud
      If (ABS(alat) .ge. 90. - cldrad)al = 1.15*al
!...  Polar cap semi-diameter, degrees
      caprad = polecap(alat,als)
!...  albedo of polar cap
      If (ABS(alat) .ge. 90. - caprad)al = 0.6
      ALBMS = al
      Return
      END
