!$DZDUST
      SUBROUTINE DZDUST ( LSUN, als0, INTENS, DZ )
!********1*********2*********3*********4*********5*********6*********7**
! DZDUST           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION: CALCULATE PERTUBATION TO ZF DUE TO DUST STORM
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    LSUN    I    S    AREOCENTRIC LONGITUDE OF SUN ORBIT
!    ALS0    I    S    START LSUN VALUE FOR DUST STORM ( 0=NONE)
!    INTENS  I    S    DUST STORM INTENSITY ( 0.0 - 3.0)
!    DZ      O    S    INCREASE IN THERMOSPHERIC REF. SURFACE DURING
!                      DUST STORMS
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DOUBLE PRECISION LSUN,DZ,INTENS
!.... CALCULATES PERTURBATION TO ZF DUE TO DUST STORMS
      DOUBLE PRECISION DLS, DZA, DLSA, QA
!
!.... Modified from original Stewart subroutine (which assumed storms
!     starting every year at Ls = 205 and Ls = 275
!
!      SUBROUTINE DZDUST (FLAG, LSUN, DFAO, DZ )
!      INTEGER FLAG
!      LSA = 205.0D0
!      LSB = 275.0D0
!      DLS = 25.0D0
!      DZA = 10.0D0
!      DZB = 14.0D0
!      DLSA = AMOD(((LSUN - LSA) + 720.0D0), 360.0D0)
!      DLSB = AMOD(((LSUN - LSB) + 720.0D0), 360.0D0)
!      QA = DEXP(-DLSA / DLS)
!      QB = DEXP(-DLSB / DLS)
!      DZ = DZA*QA*(1.0D0 - QA**4) + DZB*QB*(1.0D0 - QB**4)
!      DZ = DFAO * DZ * 1.869D0
!
!
      DLS = 25.0D0
!...  DLSA = Ls-Ls0 put into range 0-25 degrees
      DLSA = MOD(((LSUN - als0) + 720.0D0), 360.0D0)
!.... QA = exponential time decay factor for dust effect
      QA = EXP(-DLSA / DLS)
      DZA = 5.0D0*DBLE(INTENS)
      DZ = DZA*QA*(1.0D0 - QA**4)*1.869D0
!.... DZ=INCREASE IN THERMOSPHERIC REF. SURFACE DURING DUST STORMS
      RETURN
      END
