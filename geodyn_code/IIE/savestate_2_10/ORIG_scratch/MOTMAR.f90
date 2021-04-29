!$MOTMAR
      SUBROUTINE MOTMAR(T,SL)
!********1*********2*********3*********4*********5*********6*********7
! MOTMAR           05/30/92            0000.0    PGMR - S.LUO
!
! FUNCTION:  CALCULATE MARS MEAN ANOMLY
!            IN MARS ORBIT PLANE REFRENCE FRAME FOR MARS NUTATION
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T        I         INTERVAL TIME IN DAY FROM J2000.0(JD2451545)
!   SL       O         MARS MEAN ANOMLY AT EPOCH T
!                      SL = DL - W
!                      DL IS MARS MEAN LONGITUDE
!                      W  IS THE LONGITUDE OF MARS PERIHELION
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/ELEMTM/W(6),OM(6),DL(4),CI(3),DPERIM
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      D=T
!
! CONVERT THE TIME INTERVAL IN DAY TO IN JULIAN CENTURY
!
      T1=T/36525.D0
!
! CALCULATES THE MEAN ANOMALY OF MARS : SL = DL-W
!   WHERE DL IS MARS MEAN LONGITUDE
!          W IS THE LONGITUDE OF MARS PERIHELION
!
      SL=DL(1)-W(1)
      SL=SL+(DL(2)-W(2))*T1+(DL(3)-W(3))*T1**2                          &
     &   +(DL(4)-W(4))*T1**3
!
! KEEP SL LESS THAN 360 DEGREE
!
      SL=MOD(SL,360.D0)
!
! CONVERT SL'S UNIT FROM DEGREE TO RADIAN
!
      SL=SL*DEGRAD
!
!CC   SL=SL*DEGRAN
  100 FORMAT(1X,'SL=',F10.3)
      RETURN
      END
