!$POLECAP
      DOUBLE PRECISION Function polecap(alat,als)
!********1*********2*********3*********4*********5*********6*********7**
! POLECAP          00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
! Obtain polar cap radius (degrees) at areocentric longitude of sun = al
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  Polar cap radius (degrees) at areocentric longitude of sun = als
!     If alat < 0 gives South polar cap; otherwise North polar cap
      pi180 = ATAN(1.D0)/45.D0
!...  als0 = Ls phase of polar cap variation
      If (alat.lt.0.0D0)Then
        als0 = 230.D0
      Else
        als0 = 50.D0
      Endif
      polecap = 19.D0 - 16.D0* SIN(pi180*(als-als0))
      Return
      END
