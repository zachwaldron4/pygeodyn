!$DEGOUT
      SUBROUTINE DEGOUT(RAD,ID,IM,S,K)
!********1*********2*********3*********4*********5*********6*********7**
! DEGOUT           84/03/20            0000.0    PGMR - BILL EDDY
!
! FUNCTION:  CONVERT RADIANS TO DEG/MIN/SEC OR HOUR/MIN/SEC
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   RAD      I    S    ANGLE IN RADIANS
!   ID       O    S    DEGREES OR HOURS (DEPENDING ON K)
!   IM       O    S    MINUTES
!   S        O    S    SECONDS
!   K        I    S    FLAG CONTROLLING OUTPUT AS FOLLOWS
!                       = 1  DEG /MIN/SEC
!                       = 2  HOUR/MIN/SEC
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
      DATA C60D0/60.D0/,C15D0/15.D0/
      DATA ROUND/.5D-12/
!**********************************************************************
! START OF EXECUTABLE CODE
!**********************************************************************
!
! CONVERT RADIANS TO DEG/MIN/SEC OR HOUR/MIN/SEC
!
      RADIAN=RAD/DEGRAD
! IF OUTPUT IS TO BE IN HOURS DIVIDE BY 15
      IF(K.EQ.2) RADIAN=RADIAN/C15D0
! EXTRACT HOURS OR DEGREES
      ID=RADIAN+ROUND
      H=ID
! EXTRACT MINUTES
      SMIN=(RADIAN-H)*C60D0
      IM=SMIN+ROUND
! EXTRACT SECONDS
      S=(SMIN-IM)*C60D0
      RETURN
      END
