!$SAREA
      SUBROUTINE SAREA(AX,AS,ASX,AX2,AS2,AREA,LTOT)
!**********************************************************************
!
!   PURPOSE CALCULATE THE PERCENT OF SUNLIGHT SHADOWED
!           BY A BODY
!
!   INPUT   AX    APPARENT RADIUS OF THE BODY AS SEEN BY SATELLITE
!           AS    APPARENT RADIUS OF THE SUN AS SEEN BY SATELLITE
!           ASX   ANGLE BETWEEN THE BODY AND THE SUN AS SEEN BY SATELLIT
!           AX2   AX*AX
!           AS2   AS*AS
!
!   OUTPUT  AREA  PERCENT OF THE SUN SHADOWED BY THE BODY
!           LTOT  TRUE IF TOTAL APPARENT AREA OF SUN BLOCKED OUT
!
! CALCULATE FRACTION OF SUNLIGHT
!
!**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DATA HALF/.5D0/,ONE/1.D0/,TWO/2.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!
      LTOT=.FALSE.
      IF(AX.GT.AS) GO TO 500
! SUN LOOKS LARGER
      EASY=ASX+AX
      IF(EASY.GT.AS) GO TO 700
! BODY IS A DOT IN THE SUN
      AREA=AX2/AS2
      RETURN
  500 CONTINUE
! BODY LOOKS LARGER
      EASY=ASX+AS
      IF(EASY.GT.AX) GO TO 700
! BODY TOTALLY BLOCKS OUT THE SUN
      AREA=ONE
      LTOT=.TRUE.
      RETURN
! PARTIAL SHADOWING BY BODY
  700 CONTINUE
      ASX2=ASX*ASX
      ANG1=AS2-AX2-ASX2
      ANG1=ANG1/(TWO*ASX*AX)
!CC      ANG1=DARSIN(ANG1)
      ANG1=ASIN(ANG1)
      ANG2=AS2-AX2+ASX2
      ANG2=ANG2/(TWO*ASX*AS)
!C      ANG2=DARSIN(ANG2)
      ANG2=ASIN(ANG2)
      PART1=AX2*(ANG1+(SIN(TWO*ANG1)+PI)*HALF)
      PART2=AS2*((PI-SIN(TWO*ANG2))*HALF-ANG2)
      AREA=(PART1+PART2)/(PI*AS2)
      RETURN
      END
