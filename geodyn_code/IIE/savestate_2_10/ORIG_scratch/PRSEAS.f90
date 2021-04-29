!$PRSEAS
      Subroutine PRSEAS (ALS, CLat, PR)
!********1*********2*********3*********4*********5*********6*********7**
! PRSEAS           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: CALCULATE SEASONAL PRESSURE RAVIATION ON THE SURFACE
!           OF ELLIPSOID
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    CLAT     I    S     SPACECRAFT LATITUDE (DEGREES)
!    Lsun    I    S     AREOCENTRIC LONGITUDE OF SUN ORBIT
!    PR      O    S     Relative seasonal pressure on ellipsoid surface
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!.... Relative seasonal pressure variation on reference ellipsoid
      Data a11,a12/0.0847194,-0.570405D-5/
      Data a21,a22/0.0690599,-0.132689D-5/
      Data phi11,phi12/304.041,0.0080602/
      Data phi21,phi22/61.362,0.0016533/
      pi180 = ATAN(1.)/45.
      CLAT2 = CLAT**2
!...  a1, a2 = amplitudes of cos(Ls) and cos(2*Ls) terms
      a1 = a11 + a12*CLAT2
      a2 = a21 + a22*CLAT2
!...  phi1, phi2 = phases of cos(Ls) and cos(2*Ls) terms
      phi1 = phi11 + phi12*CLAT2
      phi2 = phi21 + phi22*CLAT2
!...  relative variation in pressure on reference ellipsoid, due to
!     latitude and time (ALs) variations
      PR = 1. + a1*COS(pi180*(ALS - phi1))  +                          &
     & a2*COS(2.*pi180*(ALS - phi2))
      Return
      END
