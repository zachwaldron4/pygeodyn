!$CP
      Function Cp(T)
!********1*********2*********3*********4*********5*********6*********7**
! CP               00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!...  Specific heat at constant pressure, as function of temperature
!...  T in kelvins; Cp in joules kg**-1 K**-1
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    T       I    S     TEMPERATURE (K)
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      Cp = 639.5 + 0.123687*T + 0.00200225*T*T
      Return
      END
