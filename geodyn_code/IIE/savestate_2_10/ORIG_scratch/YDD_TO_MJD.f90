      SUBROUTINE YDD_TO_MJD (YY,DDD,HH,MM,SS,MJD,FMJD)
!
! --- CONVERT YY, DDD, HH, MM, SS --> MJD, FMJD
!
      IMPLICIT NONE
!
      INTEGER YY,DDD,HH,MM
      INTEGER*4 MJD
      DOUBLE PRECISION FMJD,SS
!
      INTEGER Y,C
      save
!
      Y = YY + 1899
!
! TAKING CARE OF THE "MILLENIUM BUG"
      if(yy.lt.80) y = y+100
!
      C = Y / 100
!
      MJD = -678576 + 365 * Y + Y / 4 - C + C / 4 + DDD
!
      FMJD = DBLE(HH) / 24.0D0 + DBLE(MM) / 1440.0D0 + SS / 86400.0D0
      RETURN
!
      END
